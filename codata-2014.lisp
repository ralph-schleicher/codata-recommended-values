;; codata-2014.lisp --- 2014 CODATA recommended values.

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

;;; Commentary:

;; Automatically generated, do not edit!

;;; Code:

(in-package :common-lisp-user)

(defpackage :codata-recommended-values-2014
  (:use :common-lisp :codata-recommended-values-common)
  (:documentation "2014 CODATA recommended values."))

(in-package :codata-recommended-values-2014)

(defmacro with-early-bindings (&body body)
  `(let (;; Speed of light in vacuum.
	 (c 299792458)
	 ;; Magnetic constant.
	 (mu (* 4 pi 1L-7))
	 ;; Elementary charge.
	 (e 1.6021766208L-19)
	 ;; Atomic unit of length.
	 (a 0.52917721067L-10)
	 ;; Hartree energy.
	 (Eh 4.359744650L-18))
     ,@body))

(define-constant alpha-particle-mass
    ("6.644657230E-27" "0.000000082E-27" "1.2E-8")
  "Alpha particle mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mal>.")

(define-constant alpha-particle-mass-energy-equivalent
    ("5.971920097E-10" "0.000000073E-10" "1.2E-8")
  "Alpha particle mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2>.")

(define-constant alpha-particle-mass-energy-equivalent-in-MeV
    ("3727.379378" "0.000023" "6.2E-9")
  "Alpha particle mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2mev>.")

(define-constant alpha-particle-mass-in-u
    ("4.001506179127" "0.000000000063" "1.6E-11")
  "Alpha particle mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malu>.")

(define-constant alpha-particle-molar-mass
    ("4.001506179127E-3" "0.000000000063E-3" "1.6E-11")
  "Alpha particle molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmal>.")

(define-constant alpha-particle-electron-mass-ratio
    ("7294.29954136" "0.00000024" "3.3E-11")
  "Alpha particle-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsme>.")

(define-constant alpha-particle-proton-mass-ratio
    ("3.97259968907" "0.00000000036" "9.2E-11")
  "Alpha particle-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsmp>.")

(define-constant Angstrom-star
    ("1.00001495E-10" "0.00000090E-10" "9.0E-7")
  "Angstrom star.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?angstar>.")

(define-constant atomic-mass-constant
    ("1.660539040E-27" "0.000000020E-27" "1.2E-8")
  "Atomic mass constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?u>.")

(define-constant atomic-mass-constant-energy-equivalent
    ("1.492418062E-10" "0.000000018E-10" "1.2E-8")
  "Atomic mass constant energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tuj>.")

(define-constant atomic-mass-constant-energy-equivalent-in-MeV
    ("931.4940954" "0.0000057" "6.2E-9")
  "Atomic mass constant energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muc2mev>.")

(define-constant atomic-mass-unit-electron-volt-relationship
    ("931.4940954E6" "0.0000057E6" "6.2E-9")
  "Atomic mass unit-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uev>.")

(define-constant atomic-mass-unit-hartree-relationship
    ("3.4231776902E7" "0.0000000016E7" "4.5E-10")
  "Atomic mass unit-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhr>.")

(define-constant atomic-mass-unit-hertz-relationship
    ("2.2523427206E23" "0.0000000010E23" "4.5E-10")
  "Atomic mass unit-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhz>.")

(define-constant atomic-mass-unit-inverse-meter-relationship
    ("7.5130066166E14" "0.0000000034E14" "4.5E-10")
  "Atomic mass unit-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uminv>.")

(define-constant atomic-mass-unit-joule-relationship
    ("1.492418062E-10" "0.000000018E-10" "1.2E-8")
  "Atomic mass unit-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uj>.")

(define-constant atomic-mass-unit-kelvin-relationship
    ("1.08095438E13" "0.00000062E13" "5.7E-7")
  "Atomic mass unit-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uk>.")

(define-constant atomic-mass-unit-kilogram-relationship
    ("1.660539040E-27" "0.000000020E-27" "1.2E-8")
  "Atomic mass unit-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ukg>.")

(define-constant atomic-unit-of-first-hyperpolarizability
    ("3.206361329E-53" "0.000000020E-53" "6.2E-9")
  "Atomic unit of first hyperpolarizability.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auhypol>.")

(define-constant atomic-unit-of-second-hyperpolarizability
    ("6.235380085E-65" "0.000000077E-65" "1.2E-8")
  "Atomic unit of second hyperpolarizability.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?au2hypol>.")

(define-constant atomic-unit-of-action
    ("1.054571800E-34" "0.000000013E-34" "1.2E-8")
  "Atomic unit of action.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tthbar>.")

(define-constant atomic-unit-of-charge
    ("1.6021766208E-19" "0.0000000098E-19" "6.1E-9")
  "Atomic unit of charge.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?te>.")

(define-constant atomic-unit-of-charge-density
    ("1.0812023770E12" "0.0000000067E12" "6.2E-9")
  "Atomic unit of charge density.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucd>.")

(define-constant atomic-unit-of-current
    ("6.623618183E-3" "0.000000041E-3" "6.1E-9")
  "Atomic unit of current.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucur>.")

(define-constant atomic-unit-of-electric-dipole-moment
    ("8.478353552E-30" "0.000000052E-30" "6.2E-9")
  "Atomic unit of electric dipole moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auedm>.")

(define-constant atomic-unit-of-electric-field
    ("5.142206707E11" "0.000000032E11" "6.1E-9")
  "Atomic unit of electric field.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefld>.")

(define-constant atomic-unit-of-electric-field-gradient
    ("9.717362356E21" "0.000000060E21" "6.2E-9")
  "Atomic unit of electric field gradient.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefg>.")

(define-constant atomic-unit-of-electric-polarizability
    ("1.6487772731E-41" "0.0000000011E-41" "6.8E-10")
  "Atomic unit of electric polarizability.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auepol>.")

(define-constant atomic-unit-of-electric-potential
    ("27.21138602" "0.00000017" "6.1E-9")
  "Atomic unit of electric potential.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auep>.")

(define-constant atomic-unit-of-electric-quadrupole-moment
    ("4.486551484E-40" "0.000000028E-40" "6.2E-9")
  "Atomic unit of electric quadrupole moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aueqm>.")

(define-constant atomic-unit-of-energy
    ("4.359744650E-18" "0.000000054E-18" "1.2E-8")
  "Atomic unit of energy.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thr>.")

(define-constant atomic-unit-of-force
    ("8.23872336E-8" "0.00000010E-8" "1.2E-8")
  "Atomic unit of force.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auforce>.")

(define-constant atomic-unit-of-length
    ("0.52917721067E-10" "0.00000000012E-10" "2.3E-10")
  "Atomic unit of length.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tbohrrada0>.")

(define-constant atomic-unit-of-magnetic-dipole-moment
    ("1.854801999E-23" "0.000000011E-23" "6.2E-9")
  "Atomic unit of magnetic dipole moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumdm>.")

(define-constant atomic-unit-of-magnetic-flux-density
    ("2.350517550E5" "0.000000014E5" "6.2E-9")
  "Atomic unit of magnetic flux density.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumfd>.")

(define-constant atomic-unit-of-magnetizability
    ("7.8910365886E-29" "0.0000000090E-29" "1.1E-9")
  "Atomic unit of magnetizability.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumag>.")

(define-constant atomic-unit-of-mass
    ("9.10938356E-31" "0.00000011E-31" "1.2E-8")
  "Atomic unit of mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ttme>.")

(define-constant atomic-unit-of-momentum
    ("1.992851882E-24" "0.000000024E-24" "1.2E-8")
  "Atomic unit of momentum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumom>.")

(define-constant atomic-unit-of-permittivity
    ((with-early-bindings (/ (expt e 2) a Eh)) "0" "0")
  "Atomic unit of permittivity.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auperm>.")

(define-constant atomic-unit-of-time
    ("2.418884326509E-17" "0.000000000014E-17" "5.9E-12")
  "Atomic unit of time.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aut>.")

(define-constant atomic-unit-of-velocity
    ("2.18769126277E6" "0.00000000050E6" "2.3E-10")
  "Atomic unit of velocity.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auvel>.")

(define-constant Avogadro-constant
    ("6.022140857E23" "0.000000074E23" "1.2E-8")
  "Avogadro constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?na>.")

(define-constant Bohr-magneton
    ("927.4009994E-26" "0.0000057E-26" "6.2E-9")
  "Bohr magneton.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mub>.")

(define-constant Bohr-magneton-in-eV/T
    ("5.7883818012E-5" "0.0000000026E-5" "4.5E-10")
  "Bohr magneton in eV/T.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubev>.")

(define-constant Bohr-magneton-in-Hz/T
    ("13.996245042E9" "0.000000086E9" "6.2E-9")
  "Bohr magneton in Hz/T.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshhz>.")

(define-constant Bohr-magneton-in-inverse-meters-per-tesla
    ("46.68644814" "0.00000029" "6.2E-9")
  "Bohr magneton in inverse meters per tesla.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshcminv>.")

(define-constant Bohr-magneton-in-K/T
    ("0.67171405" "0.00000039" "5.7E-7")
  "Bohr magneton in K/T.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubskk>.")

(define-constant Bohr-radius
    ("0.52917721067E-10" "0.00000000012E-10" "2.3E-10")
  "Bohr radius.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bohrrada0>.")

(define-constant Boltzmann-constant
    ("1.38064852E-23" "0.00000079E-23" "5.7E-7")
  "Boltzmann constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?k>.")

(define-constant Boltzmann-constant-in-eV/K
    ("8.6173303E-5" "0.0000050E-5" "5.7E-7")
  "Boltzmann constant in eV/K.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tkev>.")

(define-constant Boltzmann-constant-in-Hz/K
    ("2.0836612E10" "0.0000012E10" "5.7E-7")
  "Boltzmann constant in Hz/K.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshhz>.")

(define-constant Boltzmann-constant-in-inverse-meters-per-kelvin
    ("69.503457" "0.000040" "5.7E-7")
  "Boltzmann constant in inverse meters per kelvin.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshcminv>.")

(define-constant characteristic-impedance-of-vacuum
    ((with-early-bindings (* mu c)) "0" "0")
  "Characteristic impedance of vacuum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?z0>.")

(define-constant classical-electron-radius
    ("2.8179403227E-15" "0.0000000019E-15" "6.8E-10")
  "Classical electron radius.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?re>.")

(define-constant Compton-wavelength
    ("2.4263102367E-12" "0.0000000011E-12" "4.5E-10")
  "Compton wavelength.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwl>.")

(define-constant Compton-wavelength-over-2-pi
    ("386.15926764E-15" "0.00000018E-15" "4.5E-10")
  "Compton wavelength over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwlbar>.")

(define-constant conductance-quantum
    ("7.7480917310E-5" "0.0000000018E-5" "2.3E-10")
  "Conductance quantum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?conqu2e2sh>.")

(define-constant conventional-value-of-Josephson-constant
    ("483597.9E9" "0" "0")
  "Conventional value of Josephson constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj90>.")

(define-constant conventional-value-of-von-Klitzing-constant
    ("25812.807" "0" "0")
  "Conventional value of von Klitzing constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk90>.")

(define-constant Cu-x-unit
    ("1.00207697E-13" "0.00000028E-13" "2.8E-7")
  "Cu x unit.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xucukalph1>.")

(define-constant deuteron-g-factor
    ("0.8574382311" "0.0000000048" "5.5E-9")
  "Deuteron g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gdn>.")

(define-constant deuteron-magnetic-moment
    ("0.4330735040E-26" "0.0000000036E-26" "8.3E-9")
  "Deuteron magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mud>.")

(define-constant deuteron-magnetic-moment-to-Bohr-magneton-ratio
    ("0.4669754554E-3" "0.0000000026E-3" "5.5E-9")
  "Deuteron magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmub>.")

(define-constant deuteron-magnetic-moment-to-nuclear-magneton-ratio
    ("0.8574382311" "0.0000000048" "5.5E-9")
  "Deuteron magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmun>.")

(define-constant deuteron-mass
    ("3.343583719E-27" "0.000000041E-27" "1.2E-8")
  "Deuteron mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?md>.")

(define-constant deuteron-mass-energy-equivalent
    ("3.005063183E-10" "0.000000037E-10" "1.2E-8")
  "Deuteron mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2>.")

(define-constant deuteron-mass-energy-equivalent-in-MeV
    ("1875.612928" "0.000012" "6.2E-9")
  "Deuteron mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2mev>.")

(define-constant deuteron-mass-in-u
    ("2.013553212745" "0.000000000040" "2.0E-11")
  "Deuteron mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdu>.")

(define-constant deuteron-molar-mass
    ("2.013553212745E-3" "0.000000000040E-3" "2.0E-11")
  "Deuteron molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmd>.")

(define-constant deuteron-rms-charge-radius
    ("2.1413E-15" "0.0025E-15" "1.2E-3")
  "Deuteron rms charge radius.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rd>.")

(define-constant deuteron-electron-magnetic-moment-ratio
    ("-4.664345535E-4" "0.000000026E-4" "5.5E-9")
  "Deuteron-electron magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmuem>.")

(define-constant deuteron-electron-mass-ratio
    ("3670.48296785" "0.00000013" "3.5E-11")
  "Deuteron-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsme>.")

(define-constant deuteron-neutron-magnetic-moment-ratio
    ("-0.44820652" "0.00000011" "2.4E-7")
  "Deuteron-neutron magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmunn>.")

(define-constant deuteron-proton-magnetic-moment-ratio
    ("0.3070122077" "0.0000000015" "5.0E-9")
  "Deuteron-proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmup>.")

(define-constant deuteron-proton-mass-ratio
    ("1.99900750087" "0.00000000019" "9.3E-11")
  "Deuteron-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsmp>.")

(define-constant electric-constant
    ((with-early-bindings (/ (* mu (expt c 2)))) "0" "0")
  "Electric constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ep0>.")

(define-constant electron-charge-to-mass-quotient
    ("-1.758820024E11" "0.000000011E11" "6.2E-9")
  "Electron charge to mass quotient.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esme>.")

(define-constant electron-g-factor
    ("-2.00231930436182" "0.00000000000052" "2.6E-13")
  "Electron g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gem>.")

(define-constant electron-gyromagnetic-ratio
    ("1.760859644E11" "0.000000011E11" "6.2E-9")
  "Electron gyromagnetic ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammae>.")

(define-constant electron-gyromagnetic-ratio-over-2-pi
    ("28024.95164" "0.00017" "6.2E-9")
  "Electron gyromagnetic ratio over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammaebar>.")

(define-constant electron-magnetic-moment
    ("-928.4764620E-26" "0.0000057E-26" "6.2E-9")
  "Electron magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muem>.")

(define-constant electron-magnetic-moment-anomaly
    ("1.15965218091E-3" "0.00000000026E-3" "2.3E-10")
  "Electron magnetic moment anomaly.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ae>.")

(define-constant electron-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.00115965218091" "0.00000000000026" "2.6E-13")
  "Electron magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmub>.")

(define-constant electron-magnetic-moment-to-nuclear-magneton-ratio
    ("-1838.28197234" "0.00000017" "9.5E-11")
  "Electron magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmun>.")

(define-constant electron-mass
    ("9.10938356E-31" "0.00000011E-31" "1.2E-8")
  "Electron mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?me>.")

(define-constant electron-mass-energy-equivalent
    ("8.18710565E-14" "0.00000010E-14" "1.2E-8")
  "Electron mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2>.")

(define-constant electron-mass-energy-equivalent-in-MeV
    ("0.5109989461" "0.0000000031" "6.2E-9")
  "Electron mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2mev>.")

(define-constant electron-mass-in-u
    ("5.48579909070E-4" "0.00000000016E-4" "2.9E-11")
  "Electron mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?meu>.")

(define-constant electron-molar-mass
    ("5.48579909070E-7" "0.00000000016E-7" "2.9E-11")
  "Electron molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mme>.")

(define-constant electron-to-alpha-particle-mass-ratio
    ("1.370933554798E-4" "0.000000000045E-4" "3.3E-11")
  "Electron to alpha particle mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmalpha>.")

(define-constant electron-to-shielded-helion-magnetic-moment-ratio
    ("864.058257" "0.000010" "1.2E-8")
  "Electron to shielded helion magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmuhp>.")

(define-constant electron-to-shielded-proton-magnetic-moment-ratio
    ("-658.2275971" "0.0000072" "1.1E-8")
  "Electron to shielded proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmupp>.")

(define-constant electron-volt
    ("1.6021766208E-19" "0.0000000098E-19" "6.1E-9")
  "Electron volt.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tevj>.")

(define-constant electron-volt-atomic-mass-unit-relationship
    ("1.0735441105E-9" "0.0000000066E-9" "6.2E-9")
  "Electron volt-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evu>.")

(define-constant electron-volt-hartree-relationship
    ("3.674932248E-2" "0.000000023E-2" "6.1E-9")
  "Electron volt-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhr>.")

(define-constant electron-volt-hertz-relationship
    ("2.417989262E14" "0.000000015E14" "6.1E-9")
  "Electron volt-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhz>.")

(define-constant electron-volt-inverse-meter-relationship
    ("8.065544005E5" "0.000000050E5" "6.1E-9")
  "Electron volt-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evminv>.")

(define-constant electron-volt-joule-relationship
    ("1.6021766208E-19" "0.0000000098E-19" "6.1E-9")
  "Electron volt-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evj>.")

(define-constant electron-volt-kelvin-relationship
    ("1.16045221E4" "0.00000067E4" "5.7E-7")
  "Electron volt-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evk>.")

(define-constant electron-volt-kilogram-relationship
    ("1.782661907E-36" "0.000000011E-36" "6.1E-9")
  "Electron volt-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evkg>.")

(define-constant electron-deuteron-magnetic-moment-ratio
    ("-2143.923499" "0.000012" "5.5E-9")
  "Electron-deuteron magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmud>.")

(define-constant electron-deuteron-mass-ratio
    ("2.724437107484E-4" "0.000000000096E-4" "3.5E-11")
  "Electron-deuteron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmd>.")

(define-constant electron-helion-mass-ratio
    ("1.819543074854E-4" "0.000000000088E-4" "4.9E-11")
  "Electron-helion mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmh>.")

(define-constant electron-muon-magnetic-moment-ratio
    ("206.7669880" "0.0000046" "2.2E-8")
  "Electron-muon magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmumum>.")

(define-constant electron-muon-mass-ratio
    ("4.83633170E-3" "0.00000011E-3" "2.2E-8")
  "Electron-muon mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmmu>.")

(define-constant electron-neutron-magnetic-moment-ratio
    ("960.92050" "0.00023" "2.4E-7")
  "Electron-neutron magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmunn>.")

(define-constant electron-neutron-mass-ratio
    ("5.4386734428E-4" "0.0000000027E-4" "4.9E-10")
  "Electron-neutron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmn>.")

(define-constant electron-proton-magnetic-moment-ratio
    ("-658.2106866" "0.0000020" "3.0E-9")
  "Electron-proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmup>.")

(define-constant electron-proton-mass-ratio
    ("5.44617021352E-4" "0.00000000052E-4" "9.5E-11")
  "Electron-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmp>.")

(define-constant electron-tau-mass-ratio
    ("2.87592E-4" "0.00026E-4" "9.0E-5")
  "Electron-tau mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmtau>.")

(define-constant electron-triton-mass-ratio
    ("1.819200062203E-4" "0.000000000084E-4" "4.6E-11")
  "Electron-triton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmt>.")

(define-constant elementary-charge
    ("1.6021766208E-19" "0.0000000098E-19" "6.1E-9")
  "Elementary charge.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?e>.")

(define-constant elementary-charge-over-h
    ("2.417989262E14" "0.000000015E14" "6.1E-9")
  "Elementary charge over h.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esh>.")

(define-constant Faraday-constant
    ("96485.33289" "0.00059" "6.2E-9")
  "Faraday constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f>.")

(define-constant Faraday-constant-for-conventional-electric-current
    ("96485.3251" "0.0012" "1.2E-8")
  "Faraday constant for conventional electric current.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f90>.")

(define-constant Fermi-coupling-constant
    ("1.1663787E-5" "0.0000006E-5" "5.1E-7")
  "Fermi coupling constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gf>.")

(define-constant fine-structure-constant
    ("7.2973525664E-3" "0.0000000017E-3" "2.3E-10")
  "Fine-structure constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alph>.")

(define-constant first-radiation-constant
    ("3.741771790E-16" "0.000000046E-16" "1.2E-8")
  "First radiation constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c11strc>.")

(define-constant first-radiation-constant-for-spectral-radiance
    ("1.191042953E-16" "0.000000015E-16" "1.2E-8")
  "First radiation constant for spectral radiance.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c1l>.")

(define-constant Hartree-energy
    ("4.359744650E-18" "0.000000054E-18" "1.2E-8")
  "Hartree energy.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hr>.")

(define-constant Hartree-energy-in-eV
    ("27.21138602" "0.00000017" "6.1E-9")
  "Hartree energy in eV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?threv>.")

(define-constant hartree-atomic-mass-unit-relationship
    ("2.9212623197E-8" "0.0000000013E-8" "4.5E-10")
  "Hartree-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hru>.")

(define-constant hartree-electron-volt-relationship
    ("27.21138602" "0.00000017" "6.1E-9")
  "Hartree-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrev>.")

(define-constant hartree-hertz-relationship
    ("6.579683920711E15" "0.000000000039E15" "5.9E-12")
  "Hartree-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrhz>.")

(define-constant hartree-inverse-meter-relationship
    ("2.194746313702E7" "0.000000000013E7" "5.9E-12")
  "Hartree-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrminv>.")

(define-constant hartree-joule-relationship
    ("4.359744650E-18" "0.000000054E-18" "1.2E-8")
  "Hartree-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrj>.")

(define-constant hartree-kelvin-relationship
    ("3.1577513E5" "0.0000018E5" "5.7E-7")
  "Hartree-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrk>.")

(define-constant hartree-kilogram-relationship
    ("4.850870129E-35" "0.000000060E-35" "1.2E-8")
  "Hartree-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrkg>.")

(define-constant helion-g-factor
    ("-4.255250616" "0.000000050" "1.2E-8")
  "Helion g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ghn>.")

(define-constant helion-magnetic-moment
    ("-1.074617522E-26" "0.000000014E-26" "1.3E-8")
  "Helion magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muh>.")

(define-constant helion-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.158740958E-3" "0.000000014E-3" "1.2E-8")
  "Helion magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmub>.")

(define-constant helion-magnetic-moment-to-nuclear-magneton-ratio
    ("-2.127625308" "0.000000025" "1.2E-8")
  "Helion magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmun>.")

(define-constant helion-mass
    ("5.006412700E-27" "0.000000062E-27" "1.2E-8")
  "Helion mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mh>.")

(define-constant helion-mass-energy-equivalent
    ("4.499539341E-10" "0.000000055E-10" "1.2E-8")
  "Helion mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2>.")

(define-constant helion-mass-energy-equivalent-in-MeV
    ("2808.391586" "0.000017" "6.2E-9")
  "Helion mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2mev>.")

(define-constant helion-mass-in-u
    ("3.01493224673" "0.00000000012" "3.9E-11")
  "Helion mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhu>.")

(define-constant helion-molar-mass
    ("3.01493224673E-3" "0.00000000012E-3" "3.9E-11")
  "Helion molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmh>.")

(define-constant helion-electron-mass-ratio
    ("5495.88527922" "0.00000027" "4.9E-11")
  "Helion-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsme>.")

(define-constant helion-proton-mass-ratio
    ("2.99315267046" "0.00000000029" "9.6E-11")
  "Helion-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsmp>.")

(define-constant hertz-atomic-mass-unit-relationship
    ("4.4398216616E-24" "0.0000000020E-24" "4.5E-10")
  "Hertz-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzu>.")

(define-constant hertz-electron-volt-relationship
    ("4.135667662E-15" "0.000000025E-15" "6.1E-9")
  "Hertz-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzev>.")

(define-constant hertz-hartree-relationship
    ("1.5198298460088E-16" "0.0000000000090E-16" "5.9E-12")
  "Hertz-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzhr>.")

(define-constant hertz-inverse-meter-relationship
    ((with-early-bindings (/ c)) "0" "0")
  "Hertz-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzminv>.")

(define-constant hertz-joule-relationship
    ("6.626070040E-34" "0.000000081E-34" "1.2E-8")
  "Hertz-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzj>.")

(define-constant hertz-kelvin-relationship
    ("4.7992447E-11" "0.0000028E-11" "5.7E-7")
  "Hertz-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzk>.")

(define-constant hertz-kilogram-relationship
    ("7.372497201E-51" "0.000000091E-51" "1.2E-8")
  "Hertz-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzkg>.")

(define-constant inverse-fine-structure-constant
    ("137.035999139" "0.000000031" "2.3E-10")
  "Inverse fine-structure constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alphinv>.")

(define-constant inverse-meter-atomic-mass-unit-relationship
    ("1.33102504900E-15" "0.00000000061E-15" "4.5E-10")
  "Inverse meter-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvu>.")

(define-constant inverse-meter-electron-volt-relationship
    ("1.2398419739E-6" "0.0000000076E-6" "6.1E-9")
  "Inverse meter-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvev>.")

(define-constant inverse-meter-hartree-relationship
    ("4.556335252767E-8" "0.000000000027E-8" "5.9E-12")
  "Inverse meter-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhr>.")

(define-constant inverse-meter-hertz-relationship
    ("299792458" "0" "0")
  "Inverse meter-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhz>.")

(define-constant inverse-meter-joule-relationship
    ("1.986445824E-25" "0.000000024E-25" "1.2E-8")
  "Inverse meter-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvj>.")

(define-constant inverse-meter-kelvin-relationship
    ("1.43877736E-2" "0.00000083E-2" "5.7E-7")
  "Inverse meter-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvk>.")

(define-constant inverse-meter-kilogram-relationship
    ("2.210219057E-42" "0.000000027E-42" "1.2E-8")
  "Inverse meter-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvkg>.")

(define-constant inverse-of-conductance-quantum
    ("12906.4037278" "0.0000029" "2.3E-10")
  "Inverse of conductance quantum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?invconqu>.")

(define-constant Josephson-constant
    ("483597.8525E9" "0.0030E9" "6.1E-9")
  "Josephson constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kjos>.")

(define-constant joule-atomic-mass-unit-relationship
    ("6.700535363E9" "0.000000082E9" "1.2E-8")
  "Joule-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ju>.")

(define-constant joule-electron-volt-relationship
    ("6.241509126E18" "0.000000038E18" "6.1E-9")
  "Joule-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jev>.")

(define-constant joule-hartree-relationship
    ("2.293712317E17" "0.000000028E17" "1.2E-8")
  "Joule-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhr>.")

(define-constant joule-hertz-relationship
    ("1.509190205E33" "0.000000019E33" "1.2E-8")
  "Joule-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhz>.")

(define-constant joule-inverse-meter-relationship
    ("5.034116651E24" "0.000000062E24" "1.2E-8")
  "Joule-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jminv>.")

(define-constant joule-kelvin-relationship
    ("7.2429731E22" "0.0000042E22" "5.7E-7")
  "Joule-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jk>.")

(define-constant joule-kilogram-relationship
    ((with-early-bindings (/ (expt c 2))) "0" "0")
  "Joule-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jkg>.")

(define-constant kelvin-atomic-mass-unit-relationship
    ("9.2510842E-14" "0.0000053E-14" "5.7E-7")
  "Kelvin-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ku>.")

(define-constant kelvin-electron-volt-relationship
    ("8.6173303E-5" "0.0000050E-5" "5.7E-7")
  "Kelvin-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kev>.")

(define-constant kelvin-hartree-relationship
    ("3.1668105E-6" "0.0000018E-6" "5.7E-7")
  "Kelvin-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khr>.")

(define-constant kelvin-hertz-relationship
    ("2.0836612E10" "0.0000012E10" "5.7E-7")
  "Kelvin-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khz>.")

(define-constant kelvin-inverse-meter-relationship
    ("69.503457" "0.000040" "5.7E-7")
  "Kelvin-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kminv>.")

(define-constant kelvin-joule-relationship
    ("1.38064852E-23" "0.00000079E-23" "5.7E-7")
  "Kelvin-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj>.")

(define-constant kelvin-kilogram-relationship
    ("1.53617865E-40" "0.00000088E-40" "5.7E-7")
  "Kelvin-kilogram relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kkg>.")

(define-constant kilogram-atomic-mass-unit-relationship
    ("6.022140857E26" "0.000000074E26" "1.2E-8")
  "Kilogram-atomic mass unit relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgu>.")

(define-constant kilogram-electron-volt-relationship
    ("5.609588650E35" "0.000000034E35" "6.1E-9")
  "Kilogram-electron volt relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgev>.")

(define-constant kilogram-hartree-relationship
    ("2.061485823E34" "0.000000025E34" "1.2E-8")
  "Kilogram-hartree relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghr>.")

(define-constant kilogram-hertz-relationship
    ("1.356392512E50" "0.000000017E50" "1.2E-8")
  "Kilogram-hertz relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghz>.")

(define-constant kilogram-inverse-meter-relationship
    ("4.524438411E41" "0.000000056E41" "1.2E-8")
  "Kilogram-inverse meter relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgminv>.")

(define-constant kilogram-joule-relationship
    ((with-early-bindings (expt c 2)) "0" "0")
  "Kilogram-joule relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgj>.")

(define-constant kilogram-kelvin-relationship
    ("6.5096595E39" "0.0000037E39" "5.7E-7")
  "Kilogram-kelvin relationship.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgk>.")

(define-constant lattice-parameter-of-silicon
    ("543.1020504E-12" "0.0000089E-12" "1.6E-8")
  "Lattice parameter of silicon.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?asil>.")

(define-constant Loschmidt-constant-at-273.15K-and-100000Pa
    ("2.6516467E25" "0.0000015E25" "5.7E-7")
  "Loschmidt constant (273.15 K, 100000 Pa).

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0>.")

(define-constant Loschmidt-constant-at-273.15K-and-101325Pa
    ("2.6867811E25" "0.0000015E25" "5.7E-7")
  "Loschmidt constant (273.15 K, 101325 Pa).

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0std>.")

(define-constant magnetic-constant
    ((with-early-bindings mu) "0" "0")
  "Magnetic constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu0>.")

(define-constant magnetic-flux-quantum
    ("2.067833831E-15" "0.000000013E-15" "6.1E-9")
  "Magnetic flux quantum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?flxquhs2e>.")

(define-constant Mo-x-unit
    ("1.00209952E-13" "0.00000053E-13" "5.3E-7")
  "Mo x unit.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xumokalph1>.")

(define-constant molar-gas-constant
    ("8.3144598" "0.0000048" "5.7E-7")
  "Molar gas constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?r>.")

(define-constant molar-mass-constant
    ("1E-3" "0" "0")
  "Molar mass constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu>.")

(define-constant molar-mass-of-carbon-12
    ("12E-3" "0" "0")
  "Molar mass of carbon-12.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mm12c>.")

(define-constant molar-Planck-constant
    ("3.9903127110E-10" "0.0000000018E-10" "4.5E-10")
  "Molar Planck constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nah>.")

(define-constant molar-Planck-constant-times-c
    ("0.119626565582" "0.000000000054" "4.5E-10")
  "Molar Planck constant times c.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nahc>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-100000Pa
    ("22.710947E-3" "0.000013E-3" "5.7E-7")
  "Molar volume of ideal gas (273.15 K, 100000 Pa).

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvol>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-101325Pa
    ("22.413962E-3" "0.000013E-3" "5.7E-7")
  "Molar volume of ideal gas (273.15 K, 101325 Pa).

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolstd>.")

(define-constant molar-volume-of-silicon
    ("12.05883214E-6" "0.00000061E-6" "5.1E-8")
  "Molar volume of silicon.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolsil>.")

(define-constant muon-Compton-wavelength
    ("11.73444111E-15" "0.00000026E-15" "2.2E-8")
  "Muon Compton wavelength.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwl>.")

(define-constant muon-Compton-wavelength-over-2-pi
    ("1.867594308E-15" "0.000000042E-15" "2.2E-8")
  "Muon Compton wavelength over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwlbar>.")

(define-constant muon-g-factor
    ("-2.0023318418" "0.0000000013" "6.3E-10")
  "Muon g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gmum>.")

(define-constant muon-magnetic-moment
    ("-4.49044826E-26" "0.00000010E-26" "2.3E-8")
  "Muon magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumum>.")

(define-constant muon-magnetic-moment-anomaly
    ("1.16592089E-3" "0.00000063E-3" "5.4E-7")
  "Muon magnetic moment anomaly.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?amu>.")

(define-constant muon-magnetic-moment-to-Bohr-magneton-ratio
    ("-4.84197048E-3" "0.00000011E-3" "2.2E-8")
  "Muon magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmub>.")

(define-constant muon-magnetic-moment-to-nuclear-magneton-ratio
    ("-8.89059705" "0.00000020" "2.2E-8")
  "Muon magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmun>.")

(define-constant muon-mass
    ("1.883531594E-28" "0.000000048E-28" "2.5E-8")
  "Muon mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmu>.")

(define-constant muon-mass-energy-equivalent
    ("1.692833774E-11" "0.000000043E-11" "2.5E-8")
  "Muon mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2>.")

(define-constant muon-mass-energy-equivalent-in-MeV
    ("105.6583745" "0.0000024" "2.3E-8")
  "Muon mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2mev>.")

(define-constant muon-mass-in-u
    ("0.1134289257" "0.0000000025" "2.2E-8")
  "Muon mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuu>.")

(define-constant muon-molar-mass
    ("0.1134289257E-3" "0.0000000025E-3" "2.2E-8")
  "Muon molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmmu>.")

(define-constant muon-electron-mass-ratio
    ("206.7682826" "0.0000046" "2.2E-8")
  "Muon-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusme>.")

(define-constant muon-neutron-mass-ratio
    ("0.1124545167" "0.0000000025" "2.2E-8")
  "Muon-neutron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmn>.")

(define-constant muon-proton-magnetic-moment-ratio
    ("-3.183345142" "0.000000071" "2.2E-8")
  "Muon-proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmup>.")

(define-constant muon-proton-mass-ratio
    ("0.1126095262" "0.0000000025" "2.2E-8")
  "Muon-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmp>.")

(define-constant muon-tau-mass-ratio
    ("5.94649E-2" "0.00054E-2" "9.0E-5")
  "Muon-tau mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmtau>.")

(define-constant natural-unit-of-action
    ("1.054571800E-34" "0.000000013E-34" "1.2E-8")
  "Natural unit of action.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thbar>.")

(define-constant natural-unit-of-action-in-eV-s
    ("6.582119514E-16" "0.000000040E-16" "6.1E-9")
  "Natural unit of action in eV s.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thbarev>.")

(define-constant natural-unit-of-energy
    ("8.18710565E-14" "0.00000010E-14" "1.2E-8")
  "Natural unit of energy.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tmec2>.")

(define-constant natural-unit-of-energy-in-MeV
    ("0.5109989461" "0.0000000031" "6.2E-9")
  "Natural unit of energy in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tmec2mev>.")

(define-constant natural-unit-of-length
    ("386.15926764E-15" "0.00000018E-15" "4.5E-10")
  "Natural unit of length.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tecomwlbar>.")

(define-constant natural-unit-of-mass
    ("9.10938356E-31" "0.00000011E-31" "1.2E-8")
  "Natural unit of mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tme>.")

(define-constant natural-unit-of-momentum
    ("2.730924488E-22" "0.000000034E-22" "1.2E-8")
  "Natural unit of momentum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec>.")

(define-constant natural-unit-of-momentum-in-MeV/c
    ("0.5109989461" "0.0000000031" "6.2E-9")
  "Natural unit of momentum in MeV/c.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mecmevsc>.")

(define-constant natural-unit-of-time
    ("1.28808866712E-21" "0.00000000058E-21" "4.5E-10")
  "Natural unit of time.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nut>.")

(define-constant natural-unit-of-velocity
    ("299792458" "0" "0")
  "Natural unit of velocity.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tc>.")

(define-constant neutron-Compton-wavelength
    ("1.31959090481E-15" "0.00000000088E-15" "6.7E-10")
  "Neutron Compton wavelength.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwl>.")

(define-constant neutron-Compton-wavelength-over-2-pi
    ("0.21001941536E-15" "0.00000000014E-15" "6.7E-10")
  "Neutron Compton wavelength over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwlbar>.")

(define-constant neutron-g-factor
    ("-3.82608545" "0.00000090" "2.4E-7")
  "Neutron g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gnn>.")

(define-constant neutron-gyromagnetic-ratio
    ("1.83247172E8" "0.00000043E8" "2.4E-7")
  "Neutron gyromagnetic ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gamman>.")

(define-constant neutron-gyromagnetic-ratio-over-2-pi
    ("29.1646933" "0.0000069" "2.4E-7")
  "Neutron gyromagnetic ratio over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammanbar>.")

(define-constant neutron-magnetic-moment
    ("-0.96623650E-26" "0.00000023E-26" "2.4E-7")
  "Neutron magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munn>.")

(define-constant neutron-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.04187563E-3" "0.00000025E-3" "2.4E-7")
  "Neutron magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmub>.")

(define-constant neutron-magnetic-moment-to-nuclear-magneton-ratio
    ("-1.91304273" "0.00000045" "2.4E-7")
  "Neutron magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmun>.")

(define-constant neutron-mass
    ("1.674927471E-27" "0.000000021E-27" "1.2E-8")
  "Neutron mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mn>.")

(define-constant neutron-mass-energy-equivalent
    ("1.505349739E-10" "0.000000019E-10" "1.2E-8")
  "Neutron mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2>.")

(define-constant neutron-mass-energy-equivalent-in-MeV
    ("939.5654133" "0.0000058" "6.2E-9")
  "Neutron mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2mev>.")

(define-constant neutron-mass-in-u
    ("1.00866491588" "0.00000000049" "4.9E-10")
  "Neutron mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnu>.")

(define-constant neutron-molar-mass
    ("1.00866491588E-3" "0.00000000049E-3" "4.9E-10")
  "Neutron molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmn>.")

(define-constant neutron-to-shielded-proton-magnetic-moment-ratio
    ("-0.68499694" "0.00000016" "2.4E-7")
  "Neutron to shielded proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmupp>.")

(define-constant neutron-electron-magnetic-moment-ratio
    ("1.04066882E-3" "0.00000025E-3" "2.4E-7")
  "Neutron-electron magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmue>.")

(define-constant neutron-electron-mass-ratio
    ("1838.68366158" "0.00000090" "4.9E-10")
  "Neutron-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsme>.")

(define-constant neutron-muon-mass-ratio
    ("8.89248408" "0.00000020" "2.2E-8")
  "Neutron-muon mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmmu>.")

(define-constant neutron-proton-magnetic-moment-ratio
    ("-0.68497934" "0.00000016" "2.4E-7")
  "Neutron-proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmup>.")

(define-constant neutron-proton-mass-difference
    ("2.30557377E-30" "0.00000085E-30" "3.7E-7")
  "Neutron-proton mass difference.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmp>.")

(define-constant neutron-proton-mass-difference-energy-equivalent
    ("2.07214637E-13" "0.00000076E-13" "3.7E-7")
  "Neutron-proton mass difference energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2>.")

(define-constant neutron-proton-mass-difference-energy-equivalent-in-MeV
    ("1.29333205" "0.00000048" "3.7E-7")
  "Neutron-proton mass difference energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2mev>.")

(define-constant neutron-proton-mass-difference-in-u
    ("0.00138844900" "0.00000000051" "3.7E-7")
  "Neutron-proton mass difference in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpu>.")

(define-constant neutron-proton-mass-ratio
    ("1.00137841898" "0.00000000051" "5.1E-10")
  "Neutron-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmp>.")

(define-constant neutron-tau-mass-ratio
    ("0.528790" "0.000048" "9.0E-5")
  "Neutron-tau mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmtau>.")

(define-constant Newtonian-constant-of-gravitation
    ("6.67408E-11" "0.00031E-11" "4.7E-5")
  "Newtonian constant of gravitation.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bg>.")

(define-constant Newtonian-constant-of-gravitation-over-h-bar-c
    ("6.70861E-39" "0.00031E-39" "4.7E-5")
  "Newtonian constant of gravitation over h-bar c.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bgspu>.")

(define-constant nuclear-magneton
    ("5.050783699E-27" "0.000000031E-27" "6.2E-9")
  "Nuclear magneton.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mun>.")

(define-constant nuclear-magneton-in-eV/T
    ("3.1524512550E-8" "0.0000000015E-8" "4.6E-10")
  "Nuclear magneton in eV/T.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munev>.")

(define-constant nuclear-magneton-in-inverse-meters-per-tesla
    ("2.542623432E-2" "0.000000016E-2" "6.2E-9")
  "Nuclear magneton in inverse meters per tesla.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshcminv>.")

(define-constant nuclear-magneton-in-K/T
    ("3.6582690E-4" "0.0000021E-4" "5.7E-7")
  "Nuclear magneton in K/T.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munskk>.")

(define-constant nuclear-magneton-in-MHz/T
    ("7.622593285" "0.000000047" "6.2E-9")
  "Nuclear magneton in MHz/T.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshhz>.")

(define-constant Planck-constant
    ("6.626070040E-34" "0.000000081E-34" "1.2E-8")
  "Planck constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?h>.")

(define-constant Planck-constant-in-eV-s
    ("4.135667662E-15" "0.000000025E-15" "6.1E-9")
  "Planck constant in eV s.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hev>.")

(define-constant Planck-constant-over-2-pi
    ("1.054571800E-34" "0.000000013E-34" "1.2E-8")
  "Planck constant over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbar>.")

(define-constant Planck-constant-over-2-pi-in-eV-s
    ("6.582119514E-16" "0.000000040E-16" "6.1E-9")
  "Planck constant over 2 pi in eV s.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbarev>.")

(define-constant Planck-constant-over-2-pi-times-c-in-MeV-fm
    ("197.3269788" "0.0000012" "6.1E-9")
  "Planck constant over 2 pi times c in MeV fm.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbcmevf>.")

(define-constant Planck-length
    ("1.616229E-35" "0.000038E-35" "2.3E-5")
  "Planck length.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkl>.")

(define-constant Planck-mass
    ("2.176470E-8" "0.000051E-8" "2.3E-5")
  "Planck mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkm>.")

(define-constant Planck-mass-energy-equivalent-in-GeV
    ("1.220910E19" "0.000029E19" "2.3E-5")
  "Planck mass energy equivalent in GeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkmc2gev>.")

(define-constant Planck-temperature
    ("1.416808E32" "0.000033E32" "2.3E-5")
  "Planck temperature.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plktmp>.")

(define-constant Planck-time
    ("5.39116E-44" "0.00013E-44" "2.3E-5")
  "Planck time.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkt>.")

(define-constant proton-charge-to-mass-quotient
    ("9.578833226E7" "0.000000059E7" "6.2E-9")
  "Proton charge to mass quotient.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esmp>.")

(define-constant proton-Compton-wavelength
    ("1.32140985396E-15" "0.00000000061E-15" "4.6E-10")
  "Proton Compton wavelength.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwl>.")

(define-constant proton-Compton-wavelength-over-2-pi
    ("0.210308910109E-15" "0.000000000097E-15" "4.6E-10")
  "Proton Compton wavelength over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwlbar>.")

(define-constant proton-g-factor
    ("5.585694702" "0.000000017" "3.0E-9")
  "Proton g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gp>.")

(define-constant proton-gyromagnetic-ratio
    ("2.675221900E8" "0.000000018E8" "6.9E-9")
  "Proton gyromagnetic ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammap>.")

(define-constant proton-gyromagnetic-ratio-over-2-pi
    ("42.57747892" "0.00000029" "6.9E-9")
  "Proton gyromagnetic ratio over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapbar>.")

(define-constant proton-magnetic-moment
    ("1.4106067873E-26" "0.0000000097E-26" "6.9E-9")
  "Proton magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mup>.")

(define-constant proton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.5210322053E-3" "0.0000000046E-3" "3.0E-9")
  "Proton magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmub>.")

(define-constant proton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.7928473508" "0.0000000085" "3.0E-9")
  "Proton magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmun>.")

(define-constant proton-magnetic-shielding-correction
    ("25.691E-6" "0.011E-6" "4.4E-4")
  "Proton magnetic shielding correction.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmapp>.")

(define-constant proton-mass
    ("1.672621898E-27" "0.000000021E-27" "1.2E-8")
  "Proton mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mp>.")

(define-constant proton-mass-energy-equivalent
    ("1.503277593E-10" "0.000000018E-10" "1.2E-8")
  "Proton mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2>.")

(define-constant proton-mass-energy-equivalent-in-MeV
    ("938.2720813" "0.0000058" "6.2E-9")
  "Proton mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2mev>.")

(define-constant proton-mass-in-u
    ("1.007276466879" "0.000000000091" "9.0E-11")
  "Proton mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpu>.")

(define-constant proton-molar-mass
    ("1.007276466879E-3" "0.000000000091E-3" "9.0E-11")
  "Proton molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmp>.")

(define-constant proton-rms-charge-radius
    ("0.8751E-15" "0.0061E-15" "7.0E-3")
  "Proton rms charge radius.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rp>.")

(define-constant proton-electron-mass-ratio
    ("1836.15267389" "0.00000017" "9.5E-11")
  "Proton-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsme>.")

(define-constant proton-muon-mass-ratio
    ("8.88024338" "0.00000020" "2.2E-8")
  "Proton-muon mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmmu>.")

(define-constant proton-neutron-magnetic-moment-ratio
    ("-1.45989805" "0.00000034" "2.4E-7")
  "Proton-neutron magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmunn>.")

(define-constant proton-neutron-mass-ratio
    ("0.99862347844" "0.00000000051" "5.1E-10")
  "Proton-neutron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmn>.")

(define-constant proton-tau-mass-ratio
    ("0.528063" "0.000048" "9.0E-5")
  "Proton-tau mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmtau>.")

(define-constant quantum-of-circulation
    ("3.6369475486E-4" "0.0000000017E-4" "4.5E-10")
  "Quantum of circulation.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?qucirchs2me>.")

(define-constant quantum-of-circulation-times-2
    ("7.2738950972E-4" "0.0000000033E-4" "4.5E-10")
  "Quantum of circulation times 2.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hsme>.")

(define-constant Rydberg-constant
    ("10973731.568508" "0.000065" "5.9E-12")
  "Rydberg constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ryd>.")

(define-constant Rydberg-constant-times-c-in-Hz
    ("3.289841960355E15" "0.000000000019E15" "5.9E-12")
  "Rydberg constant times c in Hz.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydchz>.")

(define-constant Rydberg-constant-times-hc-in-eV
    ("13.605693009" "0.000000084" "6.1E-9")
  "Rydberg constant times hc in eV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcev>.")

(define-constant Rydberg-constant-times-hc-in-J
    ("2.179872325E-18" "0.000000027E-18" "1.2E-8")
  "Rydberg constant times hc in J.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcj>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-100000Pa
    ("-1.1517084" "0.0000014" "1.2E-6")
  "Sackur-Tetrode constant (1 K, 100000 Pa).

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0sr>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-101325Pa
    ("-1.1648714" "0.0000014" "1.2E-6")
  "Sackur-Tetrode constant (1 K, 101325 Pa).

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0srstd>.")

(define-constant second-radiation-constant
    ("1.43877736E-2" "0.00000083E-2" "5.7E-7")
  "Second radiation constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c22ndrc>.")

(define-constant shielded-helion-gyromagnetic-ratio
    ("2.037894585E8" "0.000000027E8" "1.3E-8")
  "Shielded helion gyromagnetic ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahp>.")

(define-constant shielded-helion-gyromagnetic-ratio-over-2-pi
    ("32.43409966" "0.00000043" "1.3E-8")
  "Shielded helion gyromagnetic ratio over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahpbar>.")

(define-constant shielded-helion-magnetic-moment
    ("-1.074553080E-26" "0.000000014E-26" "1.3E-8")
  "Shielded helion magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhp>.")

(define-constant shielded-helion-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.158671471E-3" "0.000000014E-3" "1.2E-8")
  "Shielded helion magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmub>.")

(define-constant shielded-helion-magnetic-moment-to-nuclear-magneton-ratio
    ("-2.127497720" "0.000000025" "1.2E-8")
  "Shielded helion magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmun>.")

(define-constant shielded-helion-to-proton-magnetic-moment-ratio
    ("-0.7617665603" "0.0000000092" "1.2E-8")
  "Shielded helion to proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmup>.")

(define-constant shielded-helion-to-shielded-proton-magnetic-moment-ratio
    ("-0.7617861313" "0.0000000033" "4.3E-9")
  "Shielded helion to shielded proton magnetic moment ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmupp>.")

(define-constant shielded-proton-gyromagnetic-ratio
    ("2.675153171E8" "0.000000033E8" "1.3E-8")
  "Shielded proton gyromagnetic ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapp>.")

(define-constant shielded-proton-gyromagnetic-ratio-over-2-pi
    ("42.57638507" "0.00000053" "1.3E-8")
  "Shielded proton gyromagnetic ratio over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammappbar>.")

(define-constant shielded-proton-magnetic-moment
    ("1.410570547E-26" "0.000000018E-26" "1.3E-8")
  "Shielded proton magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupp>.")

(define-constant shielded-proton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.520993128E-3" "0.000000017E-3" "1.1E-8")
  "Shielded proton magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmub>.")

(define-constant shielded-proton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.792775600" "0.000000030" "1.1E-8")
  "Shielded proton magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmun>.")

(define-constant speed-of-light-in-vacuum
    ("299792458" "0" "0")
  "Speed of light in vacuum.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c>.")

(define-constant standard-acceleration-of-gravity
    ("9.80665" "0" "0")
  "Standard acceleration of gravity.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gn>.")

(define-constant standard-atmosphere
    ("101325" "0" "0")
  "Standard atmosphere.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdatm>.")

(define-constant standard-state-pressure
    ("100000" "0" "0")
  "Standard-state pressure.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdspr>.")

(define-constant Stefan-Boltzmann-constant
    ("5.670367E-8" "0.000013E-8" "2.3E-6")
  "Stefan-Boltzmann constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigma>.")

(define-constant tau-Compton-wavelength
    ("0.697787E-15" "0.000063E-15" "9.0E-5")
  "Tau Compton wavelength.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwl>.")

(define-constant tau-Compton-wavelength-over-2-pi
    ("0.111056E-15" "0.000010E-15" "9.0E-5")
  "Tau Compton wavelength over 2 pi.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwlbar>.")

(define-constant tau-mass
    ("3.16747E-27" "0.00029E-27" "9.0E-5")
  "Tau mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtau>.")

(define-constant tau-mass-energy-equivalent
    ("2.84678E-10" "0.00026E-10" "9.0E-5")
  "Tau mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2>.")

(define-constant tau-mass-energy-equivalent-in-MeV
    ("1776.82" "0.16" "9.0E-5")
  "Tau mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2mev>.")

(define-constant tau-mass-in-u
    ("1.90749" "0.00017" "9.0E-5")
  "Tau mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauu>.")

(define-constant tau-molar-mass
    ("1.90749E-3" "0.00017E-3" "9.0E-5")
  "Tau molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmtau>.")

(define-constant tau-electron-mass-ratio
    ("3477.15" "0.31" "9.0E-5")
  "Tau-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausme>.")

(define-constant tau-muon-mass-ratio
    ("16.8167" "0.0015" "9.0E-5")
  "Tau-muon mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmmu>.")

(define-constant tau-neutron-mass-ratio
    ("1.89111" "0.00017" "9.0E-5")
  "Tau-neutron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmn>.")

(define-constant tau-proton-mass-ratio
    ("1.89372" "0.00017" "9.0E-5")
  "Tau-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmp>.")

(define-constant Thomson-cross-section
    ("0.66524587158E-28" "0.00000000091E-28" "1.4E-9")
  "Thomson cross section.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmae>.")

(define-constant triton-g-factor
    ("5.957924920" "0.000000028" "4.7E-9")
  "Triton g factor.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gtn>.")

(define-constant triton-magnetic-moment
    ("1.504609503E-26" "0.000000012E-26" "7.8E-9")
  "Triton magnetic moment.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mut>.")

(define-constant triton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.6223936616E-3" "0.0000000076E-3" "4.7E-9")
  "Triton magnetic moment to Bohr magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmub>.")

(define-constant triton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.978962460" "0.000000014" "4.7E-9")
  "Triton magnetic moment to nuclear magneton ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmun>.")

(define-constant triton-mass
    ("5.007356665E-27" "0.000000062E-27" "1.2E-8")
  "Triton mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mt>.")

(define-constant triton-mass-energy-equivalent
    ("4.500387735E-10" "0.000000055E-10" "1.2E-8")
  "Triton mass energy equivalent.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2>.")

(define-constant triton-mass-energy-equivalent-in-MeV
    ("2808.921112" "0.000017" "6.2E-9")
  "Triton mass energy equivalent in MeV.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2mev>.")

(define-constant triton-mass-in-u
    ("3.01550071632" "0.00000000011" "3.6E-11")
  "Triton mass in u.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtu>.")

(define-constant triton-molar-mass
    ("3.01550071632E-3" "0.00000000011E-3" "3.6E-11")
  "Triton molar mass.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmt>.")

(define-constant triton-electron-mass-ratio
    ("5496.92153588" "0.00000026" "4.6E-11")
  "Triton-electron mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsme>.")

(define-constant triton-proton-mass-ratio
    ("2.99371703348" "0.00000000022" "7.5E-11")
  "Triton-proton mass ratio.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsmp>.")

(define-constant unified-atomic-mass-unit
    ("1.660539040E-27" "0.000000020E-27" "1.2E-8")
  "Unified atomic mass unit.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tukg>.")

(define-constant von-Klitzing-constant
    ("25812.8074555" "0.0000059" "2.3E-10")
  "Von Klitzing constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk>.")

(define-constant weak-mixing-angle
    ("0.2223" "0.0021" "9.5E-3")
  "Weak mixing angle.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sin2th>.")

(define-constant Wien-frequency-displacement-law-constant
    ("5.8789238E10" "0.0000034E10" "5.7E-7")
  "Wien frequency displacement law constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bpwien>.")

(define-constant Wien-wavelength-displacement-law-constant
    ("2.8977729E-3" "0.0000017E-3" "5.7E-7")
  "Wien wavelength displacement law constant.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bwien>.")

(define-constant 220-lattice-spacing-of-silicon
    ("192.0155714E-12" "0.0000032E-12" "1.6E-8")
  "{220} lattice spacing of silicon.

2014 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?d220sil>.")

;; codata-2014.lisp ends here
