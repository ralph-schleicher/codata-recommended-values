;;; codata.lisp --- CODATA recommended values of physical constants.

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

(defpackage :codata-recommended-values
  (:use :common-lisp :codata-recommended-values-common)
  (:documentation "CODATA recommended values of physical constants.

See <http://physics.nist.gov/cuu/index.html>.

All CODATA recommended values are defined as a constant and as a
function.  The function returns three values, the value of the
constant, the standard uncertainty, and the relative standard
uncertainty.  Floating-point numbers are of the type ‘long-float’.
Please note that some of the constants have values which can not be
represented as IEEE 754 single precision floating-point numbers.

This package also exports the symbols of the 2014 CODATA recommended
values."))

(in-package :codata-recommended-values)

(export 'string-value)
(defun string-value (symbol)
  "Return the string representation of a recommended value.

Argument SYMBOL is the symbol of a recommended value.

Value is the string representation of the recommended value, or nil
if SYMBOL is not a valid symbol of a recommended value.

The string representation uses the letter E as the exponent char."
  (values (gethash symbol *string-value*)))

;; Export latest values.
(do-external-symbols (symbol (find-package :codata-recommended-values-2014))
  (import symbol)
  (export symbol))

;;; codata.lisp ends here
