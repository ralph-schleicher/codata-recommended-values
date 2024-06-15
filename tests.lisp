;;; tests.lisp --- test procedure

;; Copyright (C) 2024 Ralph Schleicher

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

(defpackage :codata-recommended-values-tests
  (:use :common-lisp
        :lisp-unit
        :iterate)
  (:import-from
   #:rs-cll
   #:string-match
   #:replace-match)
  (:export
   #:main))

(in-package :codata-recommended-values-tests)

(defparameter *source-directory* (asdf:system-source-directory "codata-recommended-values"))

(defun source-file (file-name)
  (uiop:merge-pathnames* file-name *source-directory*))

(defun float-equal* (a b)
  (if (and (integerp a) (integerp b))
      (= a b)
    (float-equal (coerce a 'long-float) (coerce b 'long-float))))

(defun %codata (release)
  (let ((package (find-package (format nil "CODATA-RECOMMENDED-VALUES-~A" release))))
    (with-open-file (stream (source-file (format nil "lib/codata-~A/VALUES" release)))
      ;; Skip header lines.
      (iter (for line = (read-line stream))
            (when (and (plusp (length line)) (char= (char line 0) #\-))
              (finish)))
      ;; File format is the same for all releases.
      ;;
      ;;           1         2         3         4         5         6         7         8         9         0         1
      ;; 01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012
      ;; -----------------------------------------------------------------------------------------------------------------
      ;; alpha particle mass                                         6.644 656 75 e-27        0.000 000 29 e-27        kg
      ;; alpha particle mass                                         6.644 657 230 e-27       0.000 000 082 e-27       kg
      ;; alpha particle mass                                         6.644 657 3357 e-27      0.000 000 0020 e-27      kg
      ;; alpha particle mass                                         6.644 657 3450 e-27      0.000 000 0021 e-27      kg
      (iter (with quantity-end = 60)
            (with value-end = 85)
            (for line = (read-line stream nil))
            (while line)
            (unless (plusp (length line))
              (next-iteration))
            (for quantity = (let ((string (subseq line 0 quantity-end)))
                              (setf string (string-trim " " string))
                              ;; See comment in file ‘CONSTANTS’.
                              (when (rs:string-match "1st" string)
                                (setf string (replace-match "first")))
                              (when (string-match "2nd" string)
                                (setf string (replace-match "second")))
                              (when (string-match "100 kPa" string)
                                (setf string (replace-match "100000 Pa")))
                              (when (string-match "101.325 kPa" string)
                                (setf string (replace-match "101325 Pa")))
                              (when (string-match "\\bCs\\b" string)
                                (setf string (replace-match "caesium")))
                              (when (string-match "\\bSi\\b" string)
                                (setf string (replace-match "silicon")))
                              (when (string-match "mag\\." string)
                                (setf string (replace-match "magnetic")))
                              (when (string-match "mom\\." string)
                                (setf string (replace-match "moment")))
                              ;; See the ‘wash-name’ function in
                              ;; file  ‘generate-code.lisp’.
                              (when (string-match "\\d(\\s+)K\\b" string)
                                (setf string (replace-match "" 1)))
                              (when (string-match "\\d(\\s+)Pa\\b" string)
                                (setf string (replace-match "" 1)))
                              (when (string-match " \\(" string)
                                (setf string (replace-match " at ")))
                              (when (string-match ", " string)
                                (setf string (replace-match " and ")))
                              (when (string-match "\\)" string)
                                (setf string (replace-match "")))
                              (when (string-match "\\{" string)
                                (setf string (replace-match "")))
                              (when (string-match "\\}" string)
                                (setf string (replace-match "")))
                              ;; Return the symbol name.
                              (nstring-upcase
                               (substitute #\- #\Space string))))
            (for value = (let ((string (subseq line quantity-end value-end)))
                           (setf string (delete #\Space string))
                           (when (string-match "\\.\\.\\." string)
                             (setf string (replace-match "")))
                           (let ((*read-default-float-format* 'long-float))
                             (read-from-string string))))
            (for symbol = (find-symbol quantity package))
            (if symbol
                (assert-true (float-equal* (symbol-value symbol) value) quantity)
              (assert-true (not (null symbol)) quantity))
            ()))))

(define-test codata-2010
  (%codata 2010))

(define-test codata-2014
  (%codata 2014))

(define-test codata-2018
  (%codata 2018))

(define-test codata-2022
  (%codata 2022))

(define-test string-values
  (let ((*read-default-float-format* 'long-float))
    (maphash (lambda (key val)
               (assert-true (= (symbol-value key) (read-from-string val)) key))
	     codata-recommended-values-internal:*string-value*)))

;; Entry point.
(defun main (&optional (tests :all))
  (let ((lisp-unit:*print-errors* t)
        (lisp-unit:*print-failures* t)
        (lisp-unit:*print-summary* t)
        (lisp-unit:*epsilon* single-float-epsilon))
    (run-tests tests :codata-recommended-values-tests)))

;;; tests.lisp ends here
