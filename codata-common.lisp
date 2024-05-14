;;; codata-common.lisp --- common definitions

;; Copyright (C) 2012 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage :codata-recommended-values-common
  (:use :common-lisp)
  (:documentation "Common definitions.

Use at your own risk."))

(in-package :codata-recommended-values-common)

(export '*string-value*)
(defvar *string-value* (make-hash-table :test 'eq)
  "Cache for string representations.")

(export 'defconst)
(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(export 'defsubst)
(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(export 'define-constant)
(defmacro define-constant (name (value abs-tol rel-tol) &optional doc)
  "Define a physical constant."
  (let ((val (gensym "VAL"))
        (str (gensym "STR"))
        (num (gensym "NUM"))
        (abs (gensym "ABS"))
        (rel (gensym "REL")))
    `(let* ((*read-default-float-format* 'long-float)
            ;; The value itself.
            (,val ,value)
            ;; String value.
            (,str (if (stringp ,val)
                      ,val
                    (nstring-upcase (write-to-string ,val))))
            ;; Numeric value.
            (,num (if (stringp ,val)
                      (read-from-string ,val)
                    ,val))
            ;; Standard uncertainty.
            (,abs (read-from-string ,abs-tol))
            ;; Relative standard uncertainty.
            (,rel (read-from-string ,rel-tol)))
       (export (quote ,name))
       (defconst ,name ,num
         ,@(when doc (list doc)))
       (defsubst ,name ()
         ,@(when doc (list (concatenate 'string doc "

Primary value is the value of the constant, secondary value is the
standard uncertainty, and tertiary value is the relative standard
uncertainty.")))
         (values ,name ,abs ,rel))
       (setf (gethash (quote ,name) *string-value*) ,str)
       (quote ,name))))

(export 'newton)
(defun newton (f/df xo)
  "Newton's method."
  (let ((x 0))
    (loop (setf x (- xo (funcall f/df xo)))
          (when (= x xo)
            (return x))
          (setf xo x))))

;;; codata-common.lisp ends here
