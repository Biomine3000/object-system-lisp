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
(in-package :object-system)

(defun make-subscribe-object (&key (receive-mode "all") (types "all"))
  (make-instance 'business-object
		 :metadata `((:event . "routing/subscribe")
			     ("receive-mode" . ,receive-mode)
			     (:types . ,types))))

(defun wait-for-reply-to (socket stream request-object &key (timeout-seconds 5))
  (let ((started (/ (get-internal-real-time) internal-time-units-per-second)))
    (loop
       (when
	   (< (+ started timeout-seconds) (/ (get-internal-real-time) internal-time-units-per-second))
	 (return (values nil nil)))
       (let
	   ((ready-socket (usocket:wait-for-input socket :timeout 0.01 :ready-only t)))
	 (when ready-socket
	   (let*
	       ((read-object (read-object stream))
		(in-reply-to (cdr (assoc :in-reply-to (metadata read-object)))))
	     (when (string-equal (id request-object) in-reply-to)
	       (return (values read-object (-
					    (/ (get-internal-real-time) internal-time-units-per-second)
					    started))))))))))
