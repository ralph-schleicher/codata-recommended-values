;;; generate-codata-2018.lisp --- create codata-2018.lisp source file.

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

;; https://en.wikipedia.org/wiki/Conventional_electrical_unit
(symbol-macrolet ((Kj-90 '(read-from-string "4.835979L+14"))
		  (Kj '(/ (* 2 e) h))
		  (Rk-90 '(read-from-string "2.5812807L+4"))
		  (Rk '(/ h (expt e 2))))
  (let (;; Exact definitions.
	(*exact* `(("Ahbar" . (/ h 2 pi)) ;atomic unit of action
		   ("kev" . (/ k e)) ;Boltzmann constant in eV/K
		   ("kshhz" . (/ k h)) ;Boltzmann constant in Hz/K
		   ("kshcminv" . (/ k h c)) ;Boltzmann constant in inverse meter per kelvin
		   ("conqu2e2sh" . (/ (* 2 (expt e 2)) h)) ;conductance quantum
		   ("ampere90" . (* (/ ,Kj-90 ,Kj) (/ ,Rk-90 ,Rk))) ;conventional value of ampere-90
		   ("charge90" . (* (/ ,Kj-90 ,Kj) (/ ,Rk-90 ,Rk))) ;conventional value of coulomb-90
		   ("capacitance90" . (/ ,Rk-90 ,Rk)) ;conventional value of farad-90
		   ("inductance90" . (/ ,Rk ,Rk-90));conventional value of henry-90
		   ("ohm90" . (/ ,Rk ,Rk-90)) ;conventional value of ohm-90
		   ("volt90" . (/ ,Kj-90 ,Kj)) ;conventional value of volt-90
		   ("power90" .  (* (expt (/ ,Kj-90 ,Kj) 2) (/ ,Rk-90 ,Rk))) ;conventional value of watt-90
		   ("evhz" . (/ e h)) ;electron volt-hertz relationship
		   ("evminv" . (/ e h c)) ;electron volt-inverse meter relationship
		   ("evk" . (/ e k)) ;electron volt-kelvin relationship
		   ("evkg" . (/ e (expt c 2))) ;electron volt-kilogram relationship
		   ("eshbar" . (* (/ e h) 2 pi)) ;elementary charge over h-bar
		   ("f" . (* na e)) ;Faraday constant
		   ("c11strc" . (* 2 pi h (expt c 2))) ;first radiation constant
		   ("c1l" . (* 2 h (expt c 2))) ;first radiation constant for spectral radiance
		   ("hzev" . (/ h e)) ;hertz-electron volt relationship
		   ("hzminv" . (/ c)) ;hertz-inverse meter relationship
		   ("hzk" . (/ h k)) ;hertz-kelvin relationship
		   ("hzkg" . (/ h (expt c 2))) ;hertz-kilogram relationship
		   ("minvev" . (/ (* h c) e)) ;inverse meter-electron volt relationship
		   ("minvj" . (* h c)) ;inverse meter-joule relationship
		   ("minvk" . (/ (* h c) k)) ;inverse meter-kelvin relationship
		   ("minvkg" . (/ h c)) ;inverse meter-kilogram relationship
		   ("invconqu" . (/ h 2 (expt e 2))) ;inverse of conductance quantum
		   ("kjos" . (/ (* 2 e) h)) ;Josephson constant
		   ("jev" . (/ e)) ;joule-electron volt relationship
		   ("jhz" . (/ h)) ;joule-hertz relationship
		   ("jminv" . (/ (* h c))) ;joule-inverse meter relationship
		   ("jk" . (/ k)) ;joule-kelvin relationship
		   ("jkg" . (/ (expt c 2))) ;joule-kilogram relationship
		   ("Rkev" . (/ k e)) ;kelvin-electron volt relationship
		   ("khz" . (/ k h)) ;kelvin-hertz relationship
		   ("kminv" . (/ k h c)) ;kelvin-inverse meter relationship
		   ("kkg" . (/ k (expt c 2))) ;kelvin-kilogram relationship
		   ("kgev" . (/ (expt c 2) e)) ;kilogram-electron volt relationship
		   ("kghz" . (/ (expt c 2) h)) ;kilogram-hertz relationship
		   ("kgminv" . (/ c h)) ;kilogram-inverse meter relationship
		   ("kgj" . (expt c 2)) ;kilogram-joule relationship
		   ("kgk" . (/ (expt c 2) k)) ;kilogram-kelvin relationship
		   ("n0" . (/ 100000 k 27315/100)) ;Loschmidt constant (273.15 K, 100000 Pa)
		   ("n0std" . (/ 101325 k 27315/100));Loschmidt constant (273.15 K, 101325 Pa)
		   ("flxquhs2e" . (/ h 2 e)) ;magnetic flux quantum
		   ("r" . (* na k)) ;molar gas constant
		   ("nah" . (* na h)) ;molar Planck constant
		   ("mvol" . (/ (* na k 27315/100) 100000)) ;molar volume of ideal gas (273.15 K, 100000 Pa)
		   ("mvolstd" . (/ (* na k 27315/100) 101325)) ;molar volume of ideal gas (273.15 K, 101325 Pa)
		   ("Nhbar" . (/ h 2 pi)) ;natural unit of action
		   ("Nhbarev" . (/ h 2 pi e)) ;natural unit of action in eV s
		   ("hev" . (/ h e)) ;Planck constant in eV/Hz
		   ("hbar" . (/ h 2 pi)) ;reduced Planck constant
		   ("hbarev" . (/ h 2 pi e)) ;reduced Planck constant in eV s
		   ("hbcmevf" . (* c (/ h 2 pi e) 1000000000)) ;reduced Planck constant times c in MeV fm
		   ("c22ndrc" . (/ (* h c) k)) ;second radiation constant
		   ("sigma" . (/ (* 8 (expt pi 5) (expt k 4)) 60 (expt h 3) (expt c 2))) ;Stefan-Boltzmann constant
		   ("rk" . (/ h (expt e 2))) ;von Klitzing constant
		   ("bpwien" . (let ((x (newton (lambda (x)
						  ;; (x - 3) exp(x) + 3 = 0
						  (let ((a (- x 3))
							(b (- x 2)))
						    (+ (/ a b) (/ 3 (* b (exp x))))))
						(read-from-string "2.8214393721220787L0"))))
				 (/ (* x k) h))) ;Wien frequency displacement law constant
		   ("bwien" . (let ((x (newton (lambda (x)
						 ;; (x - 5) exp(x) + 5 = 0
						 (let ((a (- x 5))
						       (b (- x 4)))
						   (+ (/ a b) (/ 5 (* b (exp x))))))
					       (read-from-string "4.965114231744276L0"))))
				(/ (* h c) x k))) ;Wien wavelength displacement law constant
		   )))
    (generate-code 2018)))

;;; generate-codata-2018.lisp ends here"
