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
(defpackage #:object-system-tests
  (:use #:common-lisp #:object-system #:lift #:flexi-streams)
  (:export #:run-suites))

(in-package :object-system-tests)

(deftestsuite object-serialization ()
  ((original-object (make-plaintext-business-object "aoeu")))
  (:equality-test #'equalp))

(addtest (object-serialization)
  serialize
  (ensure-same
   (let*
       ((output-stream (make-in-memory-output-stream)))
     (serialize original-object output-stream)
     (let*
	 ((serialized-sequence (get-output-stream-sequence output-stream))
	  (input-stream (make-in-memory-input-stream serialized-sequence))
	  (read-object (read-object input-stream)))
       (payload read-object)))
   (payload original-object)))

(defun run-suites ()
  (dolist (suite (list
		  'object-serialization))
    (lift:run-tests :suite suite
		    :break-on-errors? t
		    :break-on-failures? t)))
