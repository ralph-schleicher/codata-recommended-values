;;; generate-doc.lisp --- generate documentation

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

(ql:quickload "codata-recommended-values")
(ql:quickload "rs-doc") ;private

(in-package :rs-doc-user)

(let* ((system-name "codata-recommended-values")
       (doc-directory (append (pathname-directory
                               (asdf:system-source-directory system-name))
                              (list "doc"))))
  (flet ((generate (package-name &rest arguments)
           (let ((data (apply #'gather-doc :package (string-upcase package-name) arguments))
                 (path (make-pathname :directory doc-directory :name package-name)))
             (generate-doc :data data
                           :output-format :html
                           :output (make-pathname :type "html" :defaults path))
             (generate-doc :data data
                           :output-format :text
                           :output (make-pathname :type "txt" :defaults path)))))
    (generate system-name :symbols '(codata-recommended-values:string-value))
    (dolist (release '(2010 2014 2018 2022))
      (generate (format nil "~A-~A" system-name release)))))

;;; generate-doc.lisp ends here
