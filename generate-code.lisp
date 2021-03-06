;;; generate-code.lisp --- create codata-RELEASE.lisp source file.

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

(ql:quickload "iterate")
(ql:quickload "rs-cll") ; private
(ql:quickload "drakma")

(use-package :iterate)
(use-package :rs-cll)

#-(and)
(get-working-directory)

(defparameter *release* nil
  "CODATA version number.")

(defparameter *cache-directory* nil
  "Where to cache the HTML pages.")

(defparameter *lib-directory* nil
  "Where to save the final data files.")

(defparameter *constants* ()
  "Alist of constants of the form (KEY . QUANTITY).")

(defun initialize-constants ()
  "Initialize ???*constants*??? parameter."
  (let ((constants (make-pathname :name "CONSTANTS" :defaults *lib-directory*)))
    (when (probe-file constants)
      (setf *constants* (with-open-file (stream constants :direction :input)
			  (iter (for line = (read-line stream nil))
				(while line)
				(when (string-match "^\\s*(\\w+)\\s+(.+)$" line)
				  (collect (cons (match-string 1) (match-string 2))))))))))

(defun get-page (key)
  "Fetch HTML page for constant KEY."
  (cond ((null *cache-directory*)
	 (let ((file-name (merge-pathnames (make-pathname :name key) *lib-directory*)))
	   (with-open-file (stream file-name :direction :input :if-does-not-exist :error)
	     (read-file stream))))
	(t
	 (let ((file-name (merge-pathnames (make-pathname :name key) *cache-directory*)))
	   (or (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
		 (when (not (null stream))
		   (read-file stream)))
	       (let* ((base "http://physics.nist.gov/cgi-bin/cuu/Value")
		      (page (drakma:http-request (concatenate 'string base "?" key))))
		 (when (null page)
		   (setf page ""))
		 (with-open-file (stream file-name :direction :output :if-exists :supersede)
		   (write-sequence page stream)))
	       page)))))

(defparameter *exponent-char* #\L)
(defparameter *force-exponent* t)

(defun get-values (page &optional relax)
  "Extract values from HTML page.
Return value is a list of strings."
  (let (point name value abs-tol rel-tol)
    (when (and (string-match "bgcolor=\"#d0f0c8\"" page)
	       (string-match "<b>\\s*(.*?)\\s*</b>" page :start (match-end)))
      (setf name (match-string 1)
	    point (match-end)))
    ;; Value.
    (when (and (string-match "bgcolor=\"#cce2f3\"" page :start point)
	       (string-match "<b>\\s*(.*?)\\s*</b>" page :start (match-end)))
      (setf value (match-string 1)
	    point (match-end)))
    ;; Standard uncertainty.
    (when (and (string-match "bgcolor=\"#cce2f3\"" page :start point)
	       (string-match "<b>\\s*(.*?)\\s*</b>" page :start (match-end)))
      (setf abs-tol (match-string 1)
	    point (match-end)))
    ;; Relative standard uncertainty.
    (when (and (string-match "bgcolor=\"#cce2f3\"" page :start point)
	       (string-match "<b>\\s*(.*?)\\s*</b>" page :start (match-end)))
      (setf rel-tol (match-string 1)
	    point (match-end)))
    (when (and name value abs-tol rel-tol)
      (let ((*exponent-char* (if relax #\E #\L))
	    (*force-exponent* (not relax)))
	(mapcar #'wash-number (list value abs-tol rel-tol))))))

(defun wash-number (string)
  (if (string-match "\\(exact\\)" string)
      "0"
    (progn
      (iter (while (string-match "\\s*\\&nbsp;\\s*" string))
	    (setf string (replace-match "")))
      (when (string-match "x10" string)
	(setf string (replace-match (list *exponent-char*))))
      (when (string-match "<sup>(.*?)</sup>" string)
	(setf string (replace-match (match-string 1))))
      (when (string-match " " string)
	(setf string (subseq string 0 (match-start))))
      (when (and *force-exponent* (position #\. string) (not (position *exponent-char* string)))
	(setf string (concatenate 'string string (list *exponent-char* #\0))))
      string)))

(defun wash-name (string)
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
  (substitute #\- #\Space string))

(defun with-early-bindings ()
  (if (< *release* 2018)
      (format nil "~
\(defmacro with-early-bindings (&body body)
  `(let (;; Speed of light in vacuum.
	 (c ~A)
	 ;; Magnetic constant.
	 (mu (* 4 pi 1L-7))
	 ;; Elementary charge.
	 (e ~A)
	 ;; Atomic unit of length.
	 (a ~A)
	 ;; Hartree energy.
	 (Eh ~A))
     ,@body))"
	      (first (get-values (get-page "c")))
	      (first (get-values (get-page "e")))
	      (first (get-values (get-page "tbohrrada0")))
	      (first (get-values (get-page "hr"))))
    (format nil "~
\(defmacro with-early-bindings (&body body)
  `(let (;; Speed of light in vacuum.
	 (c 299792458)
         ;; Planck constant.
	 (h 6.62607015L-34)
	 ;; Elementary charge.
	 (e 1.602176634L-19)
         ;; Boltzmann constant.
	 (k 1.380649L-23)
         ;; Avogadro constant.
	 (na 6.02214076L+23))
     ,@body))")))

(defparameter *exact*
  '(;; atomic unit of permittivity
    ("auperm" . "(/ (expt e 2) a Eh)")
    ;; characteristic impedance of vacuum
    ("z0" . "(* mu c)")
    ;; electric constant
    ("ep0" . "(/ (* mu (expt c 2)))")
    ;; hertz-inverse meter relationship
    ("hzminv" . "(/ c)")
    ;; joule-kilogram relationship
    ("jkg" . "(/ (expt c 2))")
    ;; kilogram-joule relationship
    ("kgj" . "(expt c 2)")
    ;; magnetic constant
    ("mu0" . "mu"))
  "Alist of exact values of the form (KEY . FORM).")

(defparameter *correct* ()
  "Alist of correct values of the form (KEY . VALUES).
Use this to overwrite erroneous values from the HTML page.")

(defun doc-string (key name values)
  (format nil "~
~A~A.

~A CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?~A>."
	  (char-upcase (aref name 0)) (subseq name 1) *release* key))

;; Program entry point.
(defun generate-code (release &key use-cache)
  (let ((*release* release)
	(*cache-directory* nil)
	(*lib-directory* nil)
	(*constants* nil))
    (let ((subdir (format nil "codata-~A" *release*)))
      (when use-cache
	(setf *cache-directory* (make-pathname :directory (list :relative "cache" subdir)))
	(ensure-directories-exist *cache-directory*))
      (setf *lib-directory* (make-pathname :directory (list :relative "lib" subdir)))
      (ensure-directories-exist *lib-directory*))
    (initialize-constants)
    (generate-code-1)))

(defun generate-code-1 ()
  (let (templ body)
    ;; Fetch template file.
    (with-open-file (stream (format nil "codata-~A.lisp.in" *release*) :direction :input)
      (setf templ (make-string (file-length stream)))
      (read-sequence templ stream))
    ;; Generate file contents.
    (setf body (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
    (with-output-to-string (stream body)
      (format stream "~A~2%" (with-early-bindings))
      (iter (for (key . name) :in *constants*)
	    (for page = (get-page key))
	    (when (null page)
	      (next-iteration))
	    (for values = (get-values page t))
	    (when (null values)
	      (error "Should not happen!"))
	    (for correct = (assoc key *correct* :test #'string=))
	    (when (not (null correct))
	      (setf values (rest correct)))
	    (for symbol = (wash-name name))
	    (format stream "(define-constant ~A~%    " symbol)
	    (for exact = (assoc key *exact* :test #'string=))
	    (if (null exact)
		(format stream "~S" values)
	      (let ((*print-case* :downcase)
		    (*print-right-margin* 132))
		(format stream "((with-early-bindings ~S) ~S ~S)"
			(cdr exact) (second values) (third values))))
	    (format stream "~%  ~S)~2%" (doc-string key name values))))
    ;; Write output file.
    (when (string-match "#-\\(and\\) BODY\\s+" templ)
      (setf templ (replace-match body)))
    (with-open-file (stream (format nil "codata-~A.lisp" *release*) :direction :output :if-exists :supersede)
      (princ templ stream))
    t))

;;; generate-code.lisp ends here
