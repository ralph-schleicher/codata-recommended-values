;; generate-codata-2010.lisp --- create codata-2010.lisp source file.

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

(ql:quickload "iterate")
(ql:quickload "rs-cll") ; private
(ql:quickload "drakma")

(use-package :iterate)
(use-package :rs-cll)

#-(and)
(get-working-directory)

(defparameter *constants*
  (with-open-file (stream "CONSTANTS" :direction :input)
    (iter (for line = (read-line stream nil))
	  (while line)
	  (when (string-match "^\\s*(\\w+)\\s+(.+)$" line)
	    (collect (cons (match-string 1) (match-string 2))))))
  "Alist of constants of the form (KEY . QUANTITY).")

(defparameter *cache-directory*
  (ensure-directories-exist
   (make-pathname :directory '(:relative "cache" "codata-2010")))
  "Where to cache the HTML pages.")

(defun get-page (key)
  "Fetch HTML page for constant KEY."
  (let ((file-name (merge-pathnames (make-pathname :name key) *cache-directory*)))
    (or (with-open-file (stream file-name :direction :input :if-does-not-exist nil)
	  (when (not (null stream))
	    (let ((page (make-string (file-length stream))))
	      (read-sequence page stream)
	      page)))
	(let* ((base "http://physics.nist.gov/cgi-bin/cuu/Value")
	       (page (drakma:http-request (concatenate 'string base "?" key))))
	  (when (null page)
	    (setf page ""))
	  (with-open-file (stream file-name :direction :output :if-exists :supersede)
	    (write-sequence page stream))
	  page))))

(defun get-values (page)
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
      (mapcar #'wash-number (list value abs-tol rel-tol)))))

(defun wash-number (string)
  (if (string-match "\\(exact\\)" string)
      "0"
    (progn
      (iter (while (string-match "\\s*\\&nbsp;\\s*" string))
	    (setf string (replace-match "")))
      (when (string-match "x10" string)
	(setf string (replace-match "L")))
      (when (string-match "<sup>(.*?)</sup>" string)
	(setf string (replace-match (match-string 1))))
      (when (string-match " " string)
	(setf string (subseq string 0 (match-start))))
      (when (and (position #\. string) (not (position #\L string)))
	(setf string (concatenate 'string string "L0")))
      string)))

(defun wash-name (string)
  (when (string-match "(\\s+)K\\b" string)
    (setf string (replace-match "" 1)))
  (when (string-match "(\\s+)Pa\\b" string)
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

(defparameter *with-early-bindings*
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
	  (first (get-values (get-page "hr")))))

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

(defun doc-string (key name values)
  (format nil "~
~A~A.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?~A>."
	  (char-upcase (aref name 0)) (subseq name 1) key))

;; Program entry point.
(let (templ body)
  ;; Fetch template file.
  (with-open-file (stream "codata-2010.lisp.in" :direction :input)
    (setf templ (make-string (file-length stream)))
    (read-sequence templ stream))
  ;; Generate file contents.
  (setf body (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
  (with-output-to-string (stream body)
    (format stream "~A~2%" *with-early-bindings*)
    (iter (for (key . name) :in *constants*)
	  (for page = (get-page key))
	  (when (null page)
	    (next-iteration))
	  (for values = (get-values page))
	  (when (null values)
	    (error "Should not happen!"))
	  (for exact = (assoc key *exact* :test #'string=))
	  (when (not (null exact))
	    (setf (first values)
		  (format nil "(with-early-bindings ~A)" (cdr exact))))
	  (for symbol = (wash-name name))
	  (format stream "(define-constant ~A~%    ~A~%  ~S)~2%"
		  symbol values (doc-string key name values))))
  ;; Write output file.
  (when (string-match "#-\\(and\\) BODY\\s+" templ)
    (setf templ (replace-match body)))
  (with-open-file (stream "codata-2010.lisp" :direction :output :if-exists :supersede)
    (princ templ stream))
  t)

;; generate-codata-2010.lisp ends here
