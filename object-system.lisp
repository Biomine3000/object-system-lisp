;;;; Copyright (c) 2012, Atte Hinkka <atte.hinkka@iki.fi>
;;;; 
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
(defpackage #:object-system
  (:use #:common-lisp #:flexi-streams)
  (:export :make-plaintext-business-object
	   :make-message-id
	   :serialize
	   :read-object

	   :payload
	   :id
	   :textual-p
	   :type-definition-as-string
	   :read-type-definition-from-string
	   :object-type
	   :metadata
	   :get-field

	   :serialize-alist-metadata

	   :business-object
	   :type-definition

	   :make-subscribe-object
	   :wait-for-reply-to))

(in-package :object-system)

;;;
;;; Object system mechanics
;;;
(defgeneric serialize (object stream))

;; Type definition
(defclass type-definition ()
  ((content-type :initarg :content-type :accessor content-type)
   (content-subtype :initarg :content-subtype :accessor content-subtype)
   (metadata :initarg :metadata :accessor metadata :initform nil)))

(defmethod print-object ((object type-definition) stream)
  (print-unreadable-object (object stream :type t)
    (serialize object stream)))

(defun serialize-alist-metadata (stream metadata)
  "((:foo . \"bar\") (:baz 123)) => foo=\"bar\"; baz=123"
  (format stream "~{~a~^; ~}"
	  (mapcar #'(lambda (pair)
		      (format nil "~(~A~)=~S" (car pair) (cdr pair)))
		  metadata)))

(defmethod serialize ((object type-definition) stream)
  (with-slots (content-type content-subtype charset) object
    (format stream "~a/~a" content-type content-subtype)
    (when (metadata object)
      (format stream "; ")
      (serialize-alist-metadata stream object))))

(defun textual-p (type-definition)
  (if (string-equal "text" (content-type type-definition)) t nil))

(defun type-definition-as-string (type-definition)
  (let
      ((serializing-stream (make-string-output-stream :element-type 'character)))
    (serialize type-definition serializing-stream)
    (get-output-stream-string serializing-stream)))

(defun read-type-definition-from-string (string)
  (when (eq string nil)
    (return-from read-type-definition-from-string nil))
  (cl-ppcre:register-groups-bind
   (content-type content-subtype delimiter charset)
   ("(\\w+)/([-\\w]+)(;\\s+charset=)?([\\w-\\d]+)?" string)
    (declare (ignore delimiter))
    (if charset
	(make-instance 'type-definition
		       :content-type content-type
		       :content-subtype content-subtype
		       :metadata (acons :charset charset nil))
	(make-instance 'type-definition
		       :content-type content-type
		       :content-subtype content-subtype))))

;; Business objects
(defclass business-object ()
  ((object-type :initarg :object-type :accessor object-type :initform nil)
   (id :initarg :id :accessor id :initform (make-message-id))
   (metadata :initarg :metadata :accessor metadata :initform nil)
   (payload :initarg :payload :accessor payload :initform nil)
   (payload-size :initarg :payload-size :accessor payload-size :initform 0)))

(defmethod print-object ((object business-object) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (object-type metadata) object
      (serialize-alist-metadata stream (metadata object))
      (when object-type
	(if (textual-p object-type)
	    (progn
	      (serialize object-type stream)
	      (format stream " ~s" (babel:octets-to-string (payload object))))
	    (serialize object-type stream))))))


(defmethod serialize ((object business-object) stream)
  (with-slots (metadata object-type payload payload-size id) object
    (let
	((character-stream (make-flexi-stream stream
			    :element-type 'character
			    :external-format '(:utf-8 :eol-style :lf)))
	 (metadata (acons :id id metadata)))

      (when payload
	(setf metadata (acons :size payload-size metadata)))

      (when object-type
	(setf metadata (acons :type (type-definition-as-string object-type) metadata)))

      ;; (let
      ;; 	  ((json:*lisp-identifier-name-to-json*
      ;; 	   #'(lambda (identifier) (string-downcase (string identifier)))))
      ;; 	(json:encode-json-alist metadata character-stream))
      (json:encode-json-alist metadata character-stream)
      (format character-stream "~C" #\Nul)
      (force-output character-stream))

    (when (and payload-size (> payload-size 0))
      (write-sequence payload stream :end payload-size)
      (force-output stream))))

(defun read-object (stream)
  "Read an object from stream. First consume until nul, after that read metadata
defined count of bytes, the payload."
  (let
      ((buffer (make-array 128 :element-type 'character :adjustable t :fill-pointer 0))
       (character-stream (make-flexi-stream stream
					    :element-type 'character
					    :external-format '(:utf-8 :eol-style :lf)))
       (last-char nil)
       (metadata nil)
       (payload nil))
    (loop
       until (equal #\Nul last-char) do
	 (setf last-char (read-char character-stream))
	 (vector-push-extend last-char buffer))

    (decf (fill-pointer buffer)) ; we don't need the nul for anything

    (handler-case
	(setf metadata (json:decode-json-from-string buffer))
      (json:json-syntax-error (jse)
	(format *error-output* "Raw input as string: ~s~%" buffer)
	(format *error-output* "Raw input as octets: ~a~%"
		(babel:string-to-octets buffer :external-format :utf8))
	(error jse)))

    (handler-case
	(progn
	  (let
	      ((size (cdr (assoc :size metadata))))
	    (unless (or (not size) (= size 0))
	      (progn
		(setf payload (make-array size :element-type 'octet))
		(read-sequence payload stream :end size))))

	  (make-instance 'business-object
			 :object-type (read-type-definition-from-string
				       (cdr (assoc :type metadata)))
			 :id (cdr (assoc :id metadata))
			 :metadata (remove-if #'(lambda (pair)
						    (if (or (eq (car pair) :type)
							    (eq (car pair) :size)
							    (eq (car pair) :id))
							t)) metadata)
			 :payload payload
			 :payload-size (cdr (assoc :size metadata))))
      (type-error (te)
	(format *error-output* "Raw input as string: ~s~%" buffer)
	(format *error-output* "Raw input as octets: ~a~%"
		(babel:string-to-octets buffer  :external-format :utf8))
	(error te)))))

(defun get-field (obj key &optional default)
  (cdr (assoc key (metadata obj) :test #'string-equal)))

;; Message identification generation
(defun make-message-id ()
  (format nil "<~a.~a@~a>"
	  (write-to-string (get-internal-real-time) :base 36)
	  (write-to-string (random most-positive-fixnum) :base 36)
	  (machine-instance)))

;; SHA1 from string or stream
(defun make-sha1-digest-from-string (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha1
			     (babel:string-to-octets string))))

(defun make-sha1-digest-from-stream (stream)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-stream :sha1 stream)))

;; Convenience functions
(defun make-plaintext-business-object (text)
  (let*
      ((payload (babel:string-to-octets text))
       (size (length payload)))
    (make-instance 'business-object
		   :payload payload
		   :object-type (read-type-definition-from-string "text/plain")
		   :payload-size size)))
