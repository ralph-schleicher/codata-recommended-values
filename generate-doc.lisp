;; generate-doc.lisp --- generate documentation.

;; Copyright (C) 2013 Ralph Schleicher

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
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(ql:quickload "cldoc")

(in-package :cludg)

(defun codata-format-doc (symbol-descriptor driver strings)
  (with-tag (:pre ())
    (html-write "~{~A~^~%~}" strings)))

(define-descriptor-handler DEFINE-CONSTANT (form)
    "constant"
  (make-instance 'defconstant-descriptor
    :type (format nil "~S" (first form))
    :name (format nil "~S" (second form))
    :value (list)
    :doc (fourth form)))

(cldoc:extract-documentation
 'cldoc:html "doc/html"
 (asdf:find-system :codata-recommended-values)
 :table-of-contents-title "CODATA Recommended Values of Physical Constants"
 :css-pathname (make-pathname :name "cldoc" :type "css" :directory '(:relative "doc"))
 :copy-css-into-output-dir t
 :charset "UTF-8"
 :doc-formater #'codata-format-doc
 :filter #'default-filter
 :sort-predicate (constantly nil))

;; generate-doc.lisp ends here