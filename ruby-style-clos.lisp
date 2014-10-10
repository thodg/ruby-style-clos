;;
;;  Ruby style CLOS
;;
;;  Copyright 2014 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package #:cl-user)

(defpackage #:ruby-style-clos
  (:use #:cl)
  (:export
   #:class
   #:def
   #:initialize
   #:new))

(in-package #:ruby-style-clos)

(#+sbcl sb-ext:without-package-locks #-sbcl progn
  (defmacro class (name &optional parent-classes &body body)
    (let (slots
	  methods)
      (labels ((transform (x)
		 (cond ((consp x)
			(cons (transform (car x))
			      (transform (cdr x))))
		       ((and (symbolp x)
			     (char= #\@ (char (symbol-name x) 0)))
			(let ((slot-name (intern (subseq (symbol-name x) 1)
						 (symbol-package x))))
			  (pushnew slot-name slots)
			  `(slot-value self ',slot-name)))
		     (t
		      x))))
	(setq body (transform body)))

      (setq slots (sort slots #'string< :key #'symbol-name))

      (dolist (statement body)
	(case (car statement)
	  ((def) (push (cdr statement) methods))))

      `(progn

	 (defclass ,name ,parent-classes
	   ,(mapcar (lambda (slot-name)
		      `(,slot-name :initform nil))
		    slots))

	 (defmethod print-object ((object ,name) stream)
	   (print-unreadable-object (object stream :identity t)
	     (write-string (string-capitalize (symbol-name ',name)) stream)
	     ,@(loop for slot-name in slots
		  collect '(write-char #\Space stream)
		  collect '(write-char #\@ stream)
		  collect `(write-string (string-downcase
					  (symbol-name ',slot-name))
					 stream)
		  collect '(write-char #\= stream)
		  collect `(prin1 (slot-value object ',slot-name) stream))))

	 ,@(mapcan (lambda (method)
		     (destructuring-bind (method-name args &body method-body)
			 method
		       `((unless (fboundp ',method-name)
			   (defgeneric ,method-name (object &rest arguments)))
			 (defmethod ,method-name ((self ,name) &rest arguments)
			   (destructuring-bind ,args arguments
			     ,@method-body)))))
		   (nreverse methods))

	 (find-class ',name)))))

(defgeneric initialize (object &rest args))

(defmethod initialize (object &rest args)
  (declare (ignore object args)))

(defun new (klass &rest args)
  (let ((obj (make-instance klass)))
    (apply #'initialize obj args)
    obj))
