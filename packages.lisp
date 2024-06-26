;;; packages.lisp --- package definitions

;; Copyright (C) 2023 Ralph Schleicher

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

(defpackage :codata-recommended-values-internal
  (:use :common-lisp)
  (:export #:*string-value*
           #:defconst
           #:defsubst
           #:define-constant
           #:newton)
  (:documentation "Internal definitions; use at your own risk."))

(defpackage :codata-recommended-values
  (:use :common-lisp :codata-recommended-values-internal)
  (:documentation "CODATA recommended values of physical constants.

See <http://physics.nist.gov/cuu/index.html>.

All CODATA recommended values are defined as a constant and as a
function.  The function returns three values, the value of the
constant, the standard uncertainty, and the relative standard
uncertainty.  Floating-point numbers are of the type ‘long-float’.
Please note that some of the constants have values which can not be
represented as IEEE 754 single precision floating-point numbers.

This package also exports the symbols of the 2022 CODATA recommended
values."))

(defpackage :codata-recommended-values-2022
  (:use :common-lisp :codata-recommended-values-internal)
  (:documentation "2022 CODATA recommended values."))

(defpackage :codata-recommended-values-2018
  (:use :common-lisp :codata-recommended-values-internal)
  (:documentation "2018 CODATA recommended values."))

(defpackage :codata-recommended-values-2014
  (:use :common-lisp :codata-recommended-values-internal)
  (:documentation "2014 CODATA recommended values."))

(defpackage :codata-recommended-values-2010
  (:use :common-lisp :codata-recommended-values-internal)
  (:documentation "2010 CODATA recommended values."))

;;; packages.lisp ends here
