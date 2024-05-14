;;; codata-2010.lisp --- 2010 CODATA recommended values

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

(defpackage :codata-recommended-values-2010
  (:use :common-lisp :codata-recommended-values-common)
  (:documentation "2010 CODATA recommended values."))

(in-package :codata-recommended-values-2010)

(defmacro with-early-bindings (&body body)
  `(let (;; Speed of light in vacuum.
         (c 299792458)
         ;; Magnetic constant.
         (mu (* 4 pi 1L-7))
         ;; Elementary charge.
         (e 1.602176565L-19)
         ;; Atomic unit of length.
         (a 0.52917721092L-10)
         ;; Hartree energy.
         (Eh 4.35974434L-18))
     ,@body))

(define-constant alpha-particle-mass
    ("6.64465675E-27" "0.00000029E-27" "4.4E-8")
  "Alpha particle mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mal>.")

(define-constant alpha-particle-mass-energy-equivalent
    ("5.97191967E-10" "0.00000026E-10" "4.4E-8")
  "Alpha particle mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2>.")

(define-constant alpha-particle-mass-energy-equivalent-in-MeV
    ("3727.379240" "0.000082" "2.2E-8")
  "Alpha particle mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2mev>.")

(define-constant alpha-particle-mass-in-u
    ("4.001506179125" "0.000000000062" "1.5E-11")
  "Alpha particle mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malu>.")

(define-constant alpha-particle-molar-mass
    ("4.001506179125E-3" "0.000000000062E-3" "1.5E-11")
  "Alpha particle molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmal>.")

(define-constant alpha-particle-electron-mass-ratio
    ("7294.2995361" "0.0000029" "4.0E-10")
  "Alpha particle-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsme>.")

(define-constant alpha-particle-proton-mass-ratio
    ("3.97259968933" "0.00000000036" "9.0E-11")
  "Alpha particle-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsmp>.")

(define-constant Angstrom-star
    ("1.00001495E-10" "0.00000090E-10" "9.0E-7")
  "Angstrom star.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?angstar>.")

(define-constant atomic-mass-constant
    ("1.660538921E-27" "0.000000073E-27" "4.4E-8")
  "Atomic mass constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?u>.")

(define-constant atomic-mass-constant-energy-equivalent
    ("1.492417954E-10" "0.000000066E-10" "4.4E-8")
  "Atomic mass constant energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tuj>.")

(define-constant atomic-mass-constant-energy-equivalent-in-MeV
    ("931.494061" "0.000021" "2.2E-8")
  "Atomic mass constant energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muc2mev>.")

(define-constant atomic-mass-unit-electron-volt-relationship
    ("931.494061E6" "0.000021E6" "2.2E-8")
  "Atomic mass unit-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uev>.")

(define-constant atomic-mass-unit-hartree-relationship
    ("3.4231776845E7" "0.0000000024E7" "7.0E-10")
  "Atomic mass unit-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhr>.")

(define-constant atomic-mass-unit-hertz-relationship
    ("2.2523427168E23" "0.0000000016E23" "7.0E-10")
  "Atomic mass unit-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhz>.")

(define-constant atomic-mass-unit-inverse-meter-relationship
    ("7.5130066042E14" "0.0000000053E14" "7.0E-10")
  "Atomic mass unit-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uminv>.")

(define-constant atomic-mass-unit-joule-relationship
    ("1.492417954E-10" "0.000000066E-10" "4.4E-8")
  "Atomic mass unit-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uj>.")

(define-constant atomic-mass-unit-kelvin-relationship
    ("1.08095408E13" "0.00000098E13" "9.1E-7")
  "Atomic mass unit-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uk>.")

(define-constant atomic-mass-unit-kilogram-relationship
    ("1.660538921E-27" "0.000000073E-27" "4.4E-8")
  "Atomic mass unit-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ukg>.")

(define-constant atomic-unit-of-first-hyperpolarizability
    ("3.206361449E-53" "0.000000071E-53" "2.2E-8")
  "Atomic unit of first hyperpolarizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auhypol>.")

(define-constant atomic-unit-of-second-hyperpolarizability
    ("6.23538054E-65" "0.00000028E-65" "4.4E-8")
  "Atomic unit of second hyperpolarizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?au2hypol>.")

(define-constant atomic-unit-of-action
    ("1.054571726E-34" "0.000000047E-34" "4.4E-8")
  "Atomic unit of action.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tthbar>.")

(define-constant atomic-unit-of-charge
    ("1.602176565E-19" "0.000000035E-19" "2.2E-8")
  "Atomic unit of charge.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?te>.")

(define-constant atomic-unit-of-charge-density
    ("1.081202338E12" "0.000000024E12" "2.2E-8")
  "Atomic unit of charge density.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucd>.")

(define-constant atomic-unit-of-current
    ("6.62361795E-3" "0.00000015E-3" "2.2E-8")
  "Atomic unit of current.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucur>.")

(define-constant atomic-unit-of-electric-dipole-moment
    ("8.47835326E-30" "0.00000019E-30" "2.2E-8")
  "Atomic unit of electric dipole moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auedm>.")

(define-constant atomic-unit-of-electric-field
    ("5.14220652E11" "0.00000011E11" "2.2E-8")
  "Atomic unit of electric field.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefld>.")

(define-constant atomic-unit-of-electric-field-gradient
    ("9.71736200E21" "0.00000021E21" "2.2E-8")
  "Atomic unit of electric field gradient.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefg>.")

(define-constant atomic-unit-of-electric-polarizability
    ("1.6487772754E-41" "0.0000000016E-41" "9.7E-10")
  "Atomic unit of electric polarizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auepol>.")

(define-constant atomic-unit-of-electric-potential
    ("27.21138505" "0.00000060" "2.2E-8")
  "Atomic unit of electric potential.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auep>.")

(define-constant atomic-unit-of-electric-quadrupole-moment
    ("4.486551331E-40" "0.000000099E-40" "2.2E-8")
  "Atomic unit of electric quadrupole moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aueqm>.")

(define-constant atomic-unit-of-energy
    ("4.35974434E-18" "0.00000019E-18" "4.4E-8")
  "Atomic unit of energy.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thr>.")

(define-constant atomic-unit-of-force
    ("8.23872278E-8" "0.00000036E-8" "4.4E-8")
  "Atomic unit of force.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auforce>.")

(define-constant atomic-unit-of-length
    ("0.52917721092E-10" "0.00000000017E-10" "3.2E-10")
  "Atomic unit of length.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tbohrrada0>.")

(define-constant atomic-unit-of-magnetic-dipole-moment
    ("1.854801936E-23" "0.000000041E-23" "2.2E-8")
  "Atomic unit of magnetic dipole moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumdm>.")

(define-constant atomic-unit-of-magnetic-flux-density
    ("2.350517464E5" "0.000000052E5" "2.2E-8")
  "Atomic unit of magnetic flux density.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumfd>.")

(define-constant atomic-unit-of-magnetizability
    ("7.891036607E-29" "0.000000013E-29" "1.6E-9")
  "Atomic unit of magnetizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumag>.")

(define-constant atomic-unit-of-mass
    ("9.10938291E-31" "0.00000040E-31" "4.4E-8")
  "Atomic unit of mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ttme>.")

(define-constant atomic-unit-of-momentum
    ("1.992851740E-24" "0.000000088E-24" "4.4E-8")
  "Atomic unit of momentum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumom>.")

(define-constant atomic-unit-of-permittivity
    ((with-early-bindings (/ (expt e 2) a Eh)) "0" "0")
  "Atomic unit of permittivity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auperm>.")

(define-constant atomic-unit-of-time
    ("2.418884326502E-17" "0.000000000012E-17" "5.0E-12")
  "Atomic unit of time.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aut>.")

(define-constant atomic-unit-of-velocity
    ("2.18769126379E6" "0.00000000071E6" "3.2E-10")
  "Atomic unit of velocity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auvel>.")

(define-constant Avogadro-constant
    ("6.02214129E23" "0.00000027E23" "4.4E-8")
  "Avogadro constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?na>.")

(define-constant Bohr-magneton
    ("927.400968E-26" "0.000020E-26" "2.2E-8")
  "Bohr magneton.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mub>.")

(define-constant Bohr-magneton-in-eV/T
    ("5.7883818066E-5" "0.0000000038E-5" "6.5E-10")
  "Bohr magneton in eV/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubev>.")

(define-constant Bohr-magneton-in-Hz/T
    ("13.99624555E9" "0.00000031E9" "2.2E-8")
  "Bohr magneton in Hz/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshhz>.")

(define-constant Bohr-magneton-in-inverse-meters-per-tesla
    ("46.6864498" "0.0000010" "2.2E-8")
  "Bohr magneton in inverse meters per tesla.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshcminv>.")

(define-constant Bohr-magneton-in-K/T
    ("0.67171388" "0.00000061" "9.1E-7")
  "Bohr magneton in K/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubskk>.")

(define-constant Bohr-radius
    ("0.52917721092E-10" "0.00000000017E-10" "3.2E-10")
  "Bohr radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bohrrada0>.")

(define-constant Boltzmann-constant
    ("1.3806488E-23" "0.0000013E-23" "9.1E-7")
  "Boltzmann constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?k>.")

(define-constant Boltzmann-constant-in-eV/K
    ("8.6173324E-5" "0.0000078E-5" "9.1E-7")
  "Boltzmann constant in eV/K.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tkev>.")

(define-constant Boltzmann-constant-in-Hz/K
    ("2.0836618E10" "0.0000019E10" "9.1E-7")
  "Boltzmann constant in Hz/K.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshhz>.")

(define-constant Boltzmann-constant-in-inverse-meters-per-kelvin
    ("69.503476" "0.000063" "9.1E-7")
  "Boltzmann constant in inverse meters per kelvin.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshcminv>.")

(define-constant characteristic-impedance-of-vacuum
    ((with-early-bindings (* mu c)) "0" "0")
  "Characteristic impedance of vacuum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?z0>.")

(define-constant classical-electron-radius
    ("2.8179403267E-15" "0.0000000027E-15" "9.7E-10")
  "Classical electron radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?re>.")

(define-constant Compton-wavelength
    ("2.4263102389E-12" "0.0000000016E-12" "6.5E-10")
  "Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwl>.")

(define-constant Compton-wavelength-over-2-pi
    ("386.15926800E-15" "0.00000025E-15" "6.5E-10")
  "Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwlbar>.")

(define-constant conductance-quantum
    ("7.7480917346E-5" "0.0000000025E-5" "3.2E-10")
  "Conductance quantum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?conqu2e2sh>.")

(define-constant conventional-value-of-Josephson-constant
    ("483597.9E9" "0" "0")
  "Conventional value of Josephson constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj90>.")

(define-constant conventional-value-of-von-Klitzing-constant
    ("25812.807" "0" "0")
  "Conventional value of von Klitzing constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk90>.")

(define-constant Cu-x-unit
    ("1.00207697E-13" "0.00000028E-13" "2.8E-7")
  "Cu x unit.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xucukalph1>.")

(define-constant deuteron-g-factor
    ("0.8574382308" "0.0000000072" "8.4E-9")
  "Deuteron g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gdn>.")

(define-constant deuteron-magnetic-moment
    ("0.433073489E-26" "0.000000010E-26" "2.4E-8")
  "Deuteron magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mud>.")

(define-constant deuteron-magnetic-moment-to-Bohr-magneton-ratio
    ("0.4669754556E-3" "0.0000000039E-3" "8.4E-9")
  "Deuteron magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmub>.")

(define-constant deuteron-magnetic-moment-to-nuclear-magneton-ratio
    ("0.8574382308" "0.0000000072" "8.4E-9")
  "Deuteron magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmun>.")

(define-constant deuteron-mass
    ("3.34358348E-27" "0.00000015E-27" "4.4E-8")
  "Deuteron mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?md>.")

(define-constant deuteron-mass-energy-equivalent
    ("3.00506297E-10" "0.00000013E-10" "4.4E-8")
  "Deuteron mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2>.")

(define-constant deuteron-mass-energy-equivalent-in-MeV
    ("1875.612859" "0.000041" "2.2E-8")
  "Deuteron mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2mev>.")

(define-constant deuteron-mass-in-u
    ("2.013553212712" "0.000000000077" "3.8E-11")
  "Deuteron mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdu>.")

(define-constant deuteron-molar-mass
    ("2.013553212712E-3" "0.000000000077E-3" "3.8E-11")
  "Deuteron molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmd>.")

(define-constant deuteron-rms-charge-radius
    ("2.1424E-15" "0.0021E-15" "9.8E-4")
  "Deuteron rms charge radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rd>.")

(define-constant deuteron-electron-magnetic-moment-ratio
    ("-4.664345537E-4" "0.000000039E-4" "8.4E-9")
  "Deuteron-electron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmuem>.")

(define-constant deuteron-electron-mass-ratio
    ("3670.4829652" "0.0000015" "4.0E-10")
  "Deuteron-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsme>.")

(define-constant deuteron-neutron-magnetic-moment-ratio
    ("-0.44820652" "0.00000011" "2.4E-7")
  "Deuteron-neutron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmunn>.")

(define-constant deuteron-proton-magnetic-moment-ratio
    ("0.3070122070" "0.0000000024" "7.7E-9")
  "Deuteron-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmup>.")

(define-constant deuteron-proton-mass-ratio
    ("1.99900750097" "0.00000000018" "9.2E-11")
  "Deuteron-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsmp>.")

(define-constant electric-constant
    ((with-early-bindings (/ (* mu (expt c 2)))) "0" "0")
  "Electric constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ep0>.")

(define-constant electron-charge-to-mass-quotient
    ("-1.758820088E11" "0.000000039E11" "2.2E-8")
  "Electron charge to mass quotient.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esme>.")

(define-constant electron-g-factor
    ("-2.00231930436153" "0.00000000000053" "2.6E-13")
  "Electron g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gem>.")

(define-constant electron-gyromagnetic-ratio
    ("1.760859708E11" "0.000000039E11" "2.2E-8")
  "Electron gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammae>.")

(define-constant electron-gyromagnetic-ratio-over-2-pi
    ("28024.95266" "0.00062" "2.2E-8")
  "Electron gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammaebar>.")

(define-constant electron-magnetic-moment
    ("-928.476430E-26" "0.000021E-26" "2.2E-8")
  "Electron magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muem>.")

(define-constant electron-magnetic-moment-anomaly
    ("1.15965218076E-3" "0.00000000027E-3" "2.3E-10")
  "Electron magnetic moment anomaly.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ae>.")

(define-constant electron-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.00115965218076" "0.00000000000027" "2.6E-13")
  "Electron magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmub>.")

(define-constant electron-magnetic-moment-to-nuclear-magneton-ratio
    ("-1838.28197090" "0.00000075" "4.1E-10")
  "Electron magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmun>.")

(define-constant electron-mass
    ("9.10938291E-31" "0.00000040E-31" "4.4E-8")
  "Electron mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?me>.")

(define-constant electron-mass-energy-equivalent
    ("8.18710506E-14" "0.00000036E-14" "4.4E-8")
  "Electron mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2>.")

(define-constant electron-mass-energy-equivalent-in-MeV
    ("0.510998928" "0.000000011" "2.2E-8")
  "Electron mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2mev>.")

(define-constant electron-mass-in-u
    ("5.4857990946E-4" "0.0000000022E-4" "4.0E-10")
  "Electron mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?meu>.")

(define-constant electron-molar-mass
    ("5.4857990946E-7" "0.0000000022E-7" "4.0E-10")
  "Electron molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mme>.")

(define-constant electron-to-alpha-particle-mass-ratio
    ("1.37093355578E-4" "0.00000000055E-4" "4.0E-10")
  "Electron to alpha particle mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmalpha>.")

(define-constant electron-to-shielded-helion-magnetic-moment-ratio
    ("864.058257" "0.000010" "1.2E-8")
  "Electron to shielded helion magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmuhp>.")

(define-constant electron-to-shielded-proton-magnetic-moment-ratio
    ("-658.2275971" "0.0000072" "1.1E-8")
  "Electron to shielded proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmupp>.")

(define-constant electron-volt
    ("1.602176565E-19" "0.000000035E-19" "2.2E-8")
  "Electron volt.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tevj>.")

(define-constant electron-volt-atomic-mass-unit-relationship
    ("1.073544150E-9" "0.000000024E-9" "2.2E-8")
  "Electron volt-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evu>.")

(define-constant electron-volt-hartree-relationship
    ("3.674932379E-2" "0.000000081E-2" "2.2E-8")
  "Electron volt-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhr>.")

(define-constant electron-volt-hertz-relationship
    ("2.417989348E14" "0.000000053E14" "2.2E-8")
  "Electron volt-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhz>.")

(define-constant electron-volt-inverse-meter-relationship
    ("8.06554429E5" "0.00000018E5" "2.2E-8")
  "Electron volt-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evminv>.")

(define-constant electron-volt-joule-relationship
    ("1.602176565E-19" "0.000000035E-19" "2.2E-8")
  "Electron volt-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evj>.")

(define-constant electron-volt-kelvin-relationship
    ("1.1604519E4" "0.0000011E4" "9.1E-7")
  "Electron volt-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evk>.")

(define-constant electron-volt-kilogram-relationship
    ("1.782661845E-36" "0.000000039E-36" "2.2E-8")
  "Electron volt-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evkg>.")

(define-constant electron-deuteron-magnetic-moment-ratio
    ("-2143.923498" "0.000018" "8.4E-9")
  "Electron-deuteron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmud>.")

(define-constant electron-deuteron-mass-ratio
    ("2.7244371095E-4" "0.0000000011E-4" "4.0E-10")
  "Electron-deuteron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmd>.")

(define-constant electron-helion-mass-ratio
    ("1.8195430761E-4" "0.0000000017E-4" "9.2E-10")
  "Electron-helion mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmh>.")

(define-constant electron-muon-magnetic-moment-ratio
    ("206.7669896" "0.0000052" "2.5E-8")
  "Electron-muon magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmumum>.")

(define-constant electron-muon-mass-ratio
    ("4.83633166E-3" "0.00000012E-3" "2.5E-8")
  "Electron-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmmu>.")

(define-constant electron-neutron-magnetic-moment-ratio
    ("960.92050" "0.00023" "2.4E-7")
  "Electron-neutron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmunn>.")

(define-constant electron-neutron-mass-ratio
    ("5.4386734461E-4" "0.0000000032E-4" "5.8E-10")
  "Electron-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmn>.")

(define-constant electron-proton-magnetic-moment-ratio
    ("-658.2106848" "0.0000054" "8.1E-9")
  "Electron-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmup>.")

(define-constant electron-proton-mass-ratio
    ("5.4461702178E-4" "0.0000000022E-4" "4.1E-10")
  "Electron-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmp>.")

(define-constant electron-tau-mass-ratio
    ("2.87592E-4" "0.00026E-4" "9.0E-5")
  "Electron-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmtau>.")

(define-constant electron-triton-mass-ratio
    ("1.8192000653E-4" "0.0000000017E-4" "9.1E-10")
  "Electron-triton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmt>.")

(define-constant elementary-charge
    ("1.602176565E-19" "0.000000035E-19" "2.2E-8")
  "Elementary charge.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?e>.")

(define-constant elementary-charge-over-h
    ("2.417989348E14" "0.000000053E14" "2.2E-8")
  "Elementary charge over h.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esh>.")

(define-constant Faraday-constant
    ("96485.3365" "0.0021" "2.2E-8")
  "Faraday constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f>.")

(define-constant Faraday-constant-for-conventional-electric-current
    ("96485.3321" "0.0043" "4.4E-8")
  "Faraday constant for conventional electric current.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f90>.")

(define-constant Fermi-coupling-constant
    ("1.166364E-5" "0.000005E-5" "4.3E-6")
  "Fermi coupling constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gf>.")

(define-constant fine-structure-constant
    ("7.2973525698E-3" "0.0000000024E-3" "3.2E-10")
  "Fine-structure constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alph>.")

(define-constant first-radiation-constant
    ("3.74177153E-16" "0.00000017E-16" "4.4E-8")
  "First radiation constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c11strc>.")

(define-constant first-radiation-constant-for-spectral-radiance
    ("1.191042869E-16" "0.000000053E-16" "4.4E-8")
  "First radiation constant for spectral radiance.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c1l>.")

(define-constant Hartree-energy
    ("4.35974434E-18" "0.00000019E-18" "4.4E-8")
  "Hartree energy.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hr>.")

(define-constant Hartree-energy-in-eV
    ("27.21138505" "0.00000060" "2.2E-8")
  "Hartree energy in eV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?threv>.")

(define-constant hartree-atomic-mass-unit-relationship
    ("2.9212623246E-8" "0.0000000021E-8" "7.0E-10")
  "Hartree-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hru>.")

(define-constant hartree-electron-volt-relationship
    ("27.21138505" "0.00000060" "2.2E-8")
  "Hartree-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrev>.")

(define-constant hartree-hertz-relationship
    ("6.579683920729E15" "0.000000000033E15" "5.0E-12")
  "Hartree-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrhz>.")

(define-constant hartree-inverse-meter-relationship
    ("2.194746313708E7" "0.000000000011E7" "5.0E-12")
  "Hartree-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrminv>.")

(define-constant hartree-joule-relationship
    ("4.35974434E-18" "0.00000019E-18" "4.4E-8")
  "Hartree-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrj>.")

(define-constant hartree-kelvin-relationship
    ("3.1577504E5" "0.0000029E5" "9.1E-7")
  "Hartree-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrk>.")

(define-constant hartree-kilogram-relationship
    ("4.85086979E-35" "0.00000021E-35" "4.4E-8")
  "Hartree-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrkg>.")

(define-constant helion-g-factor
    ("-4.255250613" "0.000000050" "1.2E-8")
  "Helion g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ghn>.")

(define-constant helion-magnetic-moment
    ("-1.074617486E-26" "0.000000027E-26" "2.5E-8")
  "Helion magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muh>.")

(define-constant helion-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.158740958E-3" "0.000000014E-3" "1.2E-8")
  "Helion magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmub>.")

(define-constant helion-magnetic-moment-to-nuclear-magneton-ratio
    ("-2.127625306" "0.000000025" "1.2E-8")
  "Helion magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmun>.")

(define-constant helion-mass
    ("5.00641234E-27" "0.00000022E-27" "4.4E-8")
  "Helion mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mh>.")

(define-constant helion-mass-energy-equivalent
    ("4.49953902E-10" "0.00000020E-10" "4.4E-8")
  "Helion mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2>.")

(define-constant helion-mass-energy-equivalent-in-MeV
    ("2808.391482" "0.000062" "2.2E-8")
  "Helion mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2mev>.")

(define-constant helion-mass-in-u
    ("3.0149322468" "0.0000000025" "8.3E-10")
  "Helion mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhu>.")

(define-constant helion-molar-mass
    ("3.0149322468E-3" "0.0000000025E-3" "8.3E-10")
  "Helion molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmh>.")

(define-constant helion-electron-mass-ratio
    ("5495.8852754" "0.0000050" "9.2E-10")
  "Helion-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsme>.")

(define-constant helion-proton-mass-ratio
    ("2.9931526707" "0.0000000025" "8.2E-10")
  "Helion-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsmp>.")

(define-constant hertz-atomic-mass-unit-relationship
    ("4.4398216689E-24" "0.0000000031E-24" "7.0E-10")
  "Hertz-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzu>.")

(define-constant hertz-electron-volt-relationship
    ("4.135667516E-15" "0.000000091E-15" "2.2E-8")
  "Hertz-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzev>.")

(define-constant hertz-hartree-relationship
    ("1.5198298460045E-16" "0.0000000000076E-16" "5.0E-12")
  "Hertz-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzhr>.")

(define-constant hertz-inverse-meter-relationship
    ((with-early-bindings (/ c)) "0" "0")
  "Hertz-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzminv>.")

(define-constant hertz-joule-relationship
    ("6.62606957E-34" "0.00000029E-34" "4.4E-8")
  "Hertz-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzj>.")

(define-constant hertz-kelvin-relationship
    ("4.7992434E-11" "0.0000044E-11" "9.1E-7")
  "Hertz-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzk>.")

(define-constant hertz-kilogram-relationship
    ("7.37249668E-51" "0.00000033E-51" "4.4E-8")
  "Hertz-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzkg>.")

(define-constant inverse-fine-structure-constant
    ("137.035999074" "0.000000044" "3.2E-10")
  "Inverse fine-structure constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alphinv>.")

(define-constant inverse-meter-atomic-mass-unit-relationship
    ("1.33102505120E-15" "0.00000000094E-15" "7.0E-10")
  "Inverse meter-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvu>.")

(define-constant inverse-meter-electron-volt-relationship
    ("1.239841930E-6" "0.000000027E-6" "2.2E-8")
  "Inverse meter-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvev>.")

(define-constant inverse-meter-hartree-relationship
    ("4.556335252755E-8" "0.000000000023E-8" "5.0E-12")
  "Inverse meter-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhr>.")

(define-constant inverse-meter-hertz-relationship
    ("299792458" "0" "0")
  "Inverse meter-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhz>.")

(define-constant inverse-meter-joule-relationship
    ("1.986445684E-25" "0.000000088E-25" "4.4E-8")
  "Inverse meter-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvj>.")

(define-constant inverse-meter-kelvin-relationship
    ("1.4387770E-2" "0.0000013E-2" "9.1E-7")
  "Inverse meter-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvk>.")

(define-constant inverse-meter-kilogram-relationship
    ("2.210218902E-42" "0.000000098E-42" "4.4E-8")
  "Inverse meter-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvkg>.")

(define-constant inverse-of-conductance-quantum
    ("12906.4037217" "0.0000042" "3.2E-10")
  "Inverse of conductance quantum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?invconqu>.")

(define-constant Josephson-constant
    ("483597.870E9" "0.011E9" "2.2E-8")
  "Josephson constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kjos>.")

(define-constant joule-atomic-mass-unit-relationship
    ("6.70053585E9" "0.00000030E9" "4.4E-8")
  "Joule-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ju>.")

(define-constant joule-electron-volt-relationship
    ("6.24150934E18" "0.00000014E18" "2.2E-8")
  "Joule-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jev>.")

(define-constant joule-hartree-relationship
    ("2.29371248E17" "0.00000010E17" "4.4E-8")
  "Joule-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhr>.")

(define-constant joule-hertz-relationship
    ("1.509190311E33" "0.000000067E33" "4.4E-8")
  "Joule-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhz>.")

(define-constant joule-inverse-meter-relationship
    ("5.03411701E24" "0.00000022E24" "4.4E-8")
  "Joule-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jminv>.")

(define-constant joule-kelvin-relationship
    ("7.2429716E22" "0.0000066E22" "9.1E-7")
  "Joule-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jk>.")

(define-constant joule-kilogram-relationship
    ((with-early-bindings (/ (expt c 2))) "0" "0")
  "Joule-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jkg>.")

(define-constant kelvin-atomic-mass-unit-relationship
    ("9.2510868E-14" "0.0000084E-14" "9.1E-7")
  "Kelvin-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ku>.")

(define-constant kelvin-electron-volt-relationship
    ("8.6173324E-5" "0.0000078E-5" "9.1E-7")
  "Kelvin-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kev>.")

(define-constant kelvin-hartree-relationship
    ("3.1668114E-6" "0.0000029E-6" "9.1E-7")
  "Kelvin-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khr>.")

(define-constant kelvin-hertz-relationship
    ("2.0836618E10" "0.0000019E10" "9.1E-7")
  "Kelvin-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khz>.")

(define-constant kelvin-inverse-meter-relationship
    ("69.503476" "0.000063" "9.1E-7")
  "Kelvin-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kminv>.")

(define-constant kelvin-joule-relationship
    ("1.3806488E-23" "0.0000013E-23" "9.1E-7")
  "Kelvin-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj>.")

(define-constant kelvin-kilogram-relationship
    ("1.5361790E-40" "0.0000014E-40" "9.1E-7")
  "Kelvin-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kkg>.")

(define-constant kilogram-atomic-mass-unit-relationship
    ("6.02214129E26" "0.00000027E26" "4.4E-8")
  "Kilogram-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgu>.")

(define-constant kilogram-electron-volt-relationship
    ("5.60958885E35" "0.00000012E35" "2.2E-8")
  "Kilogram-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgev>.")

(define-constant kilogram-hartree-relationship
    ("2.061485968E34" "0.000000091E34" "4.4E-8")
  "Kilogram-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghr>.")

(define-constant kilogram-hertz-relationship
    ("1.356392608E50" "0.000000060E50" "4.4E-8")
  "Kilogram-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghz>.")

(define-constant kilogram-inverse-meter-relationship
    ("4.52443873E41" "0.00000020E41" "4.4E-8")
  "Kilogram-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgminv>.")

(define-constant kilogram-joule-relationship
    ((with-early-bindings (expt c 2)) "0" "0")
  "Kilogram-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgj>.")

(define-constant kilogram-kelvin-relationship
    ("6.5096582E39" "0.0000059E39" "9.1E-7")
  "Kilogram-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgk>.")

(define-constant lattice-parameter-of-silicon
    ("543.1020504E-12" "0.0000089E-12" "1.6E-8")
  "Lattice parameter of silicon.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?asil>.")

(define-constant Loschmidt-constant-at-273.15K-and-100000Pa
    ("2.6516462E25" "0.0000024E25" "9.1E-7")
  "Loschmidt constant (273.15 K, 100000 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0>.")

(define-constant Loschmidt-constant-at-273.15K-and-101325Pa
    ("2.6867805E25" "0.0000024E25" "9.1E-7")
  "Loschmidt constant (273.15 K, 101325 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0std>.")

(define-constant magnetic-constant
    ((with-early-bindings mu) "0" "0")
  "Magnetic constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu0>.")

(define-constant magnetic-flux-quantum
    ("2.067833758E-15" "0.000000046E-15" "2.2E-8")
  "Magnetic flux quantum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?flxquhs2e>.")

(define-constant Mo-x-unit
    ("1.00209952E-13" "0.00000053E-13" "5.3E-7")
  "Mo x unit.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xumokalph1>.")

(define-constant molar-gas-constant
    ("8.3144621" "0.0000075" "9.1E-7")
  "Molar gas constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?r>.")

(define-constant molar-mass-constant
    ("1E-3" "0" "0")
  "Molar mass constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu>.")

(define-constant molar-mass-of-carbon-12
    ("12E-3" "0" "0")
  "Molar mass of carbon-12.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mm12c>.")

(define-constant molar-Planck-constant
    ("3.9903127176E-10" "0.0000000028E-10" "7.0E-10")
  "Molar Planck constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nah>.")

(define-constant molar-Planck-constant-times-c
    ("0.119626565779" "0.000000000084" "7.0E-10")
  "Molar Planck constant times c.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nahc>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-100000Pa
    ("22.710953E-3" "0.000021E-3" "9.1E-7")
  "Molar volume of ideal gas (273.15 K, 100000 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvol>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-101325Pa
    ("22.413968E-3" "0.000020E-3" "9.1E-7")
  "Molar volume of ideal gas (273.15 K, 101325 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolstd>.")

(define-constant molar-volume-of-silicon
    ("12.05883301E-6" "0.00000080E-6" "6.6E-8")
  "Molar volume of silicon.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolsil>.")

(define-constant muon-Compton-wavelength
    ("11.73444103E-15" "0.00000030E-15" "2.5E-8")
  "Muon Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwl>.")

(define-constant muon-Compton-wavelength-over-2-pi
    ("1.867594294E-15" "0.000000047E-15" "2.5E-8")
  "Muon Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwlbar>.")

(define-constant muon-g-factor
    ("-2.0023318418" "0.0000000013" "6.3E-10")
  "Muon g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gmum>.")

(define-constant muon-magnetic-moment
    ("-4.49044807E-26" "0.00000015E-26" "3.4E-8")
  "Muon magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumum>.")

(define-constant muon-magnetic-moment-anomaly
    ("1.16592091E-3" "0.00000063E-3" "5.4E-7")
  "Muon magnetic moment anomaly.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?amu>.")

(define-constant muon-magnetic-moment-to-Bohr-magneton-ratio
    ("-4.84197044E-3" "0.00000012E-3" "2.5E-8")
  "Muon magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmub>.")

(define-constant muon-magnetic-moment-to-nuclear-magneton-ratio
    ("-8.89059697" "0.00000022" "2.5E-8")
  "Muon magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmun>.")

(define-constant muon-mass
    ("1.883531475E-28" "0.000000096E-28" "5.1E-8")
  "Muon mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmu>.")

(define-constant muon-mass-energy-equivalent
    ("1.692833667E-11" "0.000000086E-11" "5.1E-8")
  "Muon mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2>.")

(define-constant muon-mass-energy-equivalent-in-MeV
    ("105.6583715" "0.0000035" "3.4E-8")
  "Muon mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2mev>.")

(define-constant muon-mass-in-u
    ("0.1134289267" "0.0000000029" "2.5E-8")
  "Muon mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuu>.")

(define-constant muon-molar-mass
    ("0.1134289267E-3" "0.0000000029E-3" "2.5E-8")
  "Muon molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmmu>.")

(define-constant muon-electron-mass-ratio
    ("206.7682843" "0.0000052" "2.5E-8")
  "Muon-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusme>.")

(define-constant muon-neutron-mass-ratio
    ("0.1124545177" "0.0000000028" "2.5E-8")
  "Muon-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmn>.")

(define-constant muon-proton-magnetic-moment-ratio
    ("-3.183345107" "0.000000084" "2.6E-8")
  "Muon-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmup>.")

(define-constant muon-proton-mass-ratio
    ("0.1126095272" "0.0000000028" "2.5E-8")
  "Muon-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmp>.")

(define-constant muon-tau-mass-ratio
    ("5.94649E-2" "0.00054E-2" "9.0E-5")
  "Muon-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmtau>.")

(define-constant natural-unit-of-action
    ("1.054571726E-34" "0.000000047E-34" "4.4E-8")
  "Natural unit of action.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thbar>.")

(define-constant natural-unit-of-action-in-eV-s
    ("6.58211928E-16" "0.00000015E-16" "2.2E-8")
  "Natural unit of action in eV s.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thbarev>.")

(define-constant natural-unit-of-energy
    ("8.18710506E-14" "0.00000036E-14" "4.4E-8")
  "Natural unit of energy.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tmec2>.")

(define-constant natural-unit-of-energy-in-MeV
    ("0.510998928" "0.000000011" "2.2E-8")
  "Natural unit of energy in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tmec2mev>.")

(define-constant natural-unit-of-length
    ("386.15926800E-15" "0.00000025E-15" "6.5E-10")
  "Natural unit of length.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tecomwlbar>.")

(define-constant natural-unit-of-mass
    ("9.10938291E-31" "0.00000040E-31" "4.4E-8")
  "Natural unit of mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tme>.")

(define-constant natural-unit-of-momentum
    ("2.73092429E-22" "0.00000012E-22" "4.4E-8")
  "Natural unit of momentum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec>.")

(define-constant natural-unit-of-momentum-in-MeV/c
    ("0.510998928" "0.000000011" "2.2E-8")
  "Natural unit of momentum in MeV/c.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mecmevsc>.")

(define-constant natural-unit-of-time
    ("1.28808866833E-21" "0.00000000083E-21" "6.5E-10")
  "Natural unit of time.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nut>.")

(define-constant natural-unit-of-velocity
    ("299792458" "0" "0")
  "Natural unit of velocity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tc>.")

(define-constant neutron-Compton-wavelength
    ("1.3195909068E-15" "0.0000000011E-15" "8.2E-10")
  "Neutron Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwl>.")

(define-constant neutron-Compton-wavelength-over-2-pi
    ("0.21001941568E-15" "0.00000000017E-15" "8.2E-10")
  "Neutron Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwlbar>.")

(define-constant neutron-g-factor
    ("-3.82608545" "0.00000090" "2.4E-7")
  "Neutron g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gnn>.")

(define-constant neutron-gyromagnetic-ratio
    ("1.83247179E8" "0.00000043E8" "2.4E-7")
  "Neutron gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gamman>.")

(define-constant neutron-gyromagnetic-ratio-over-2-pi
    ("29.1646943" "0.0000069" "2.4E-7")
  "Neutron gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammanbar>.")

(define-constant neutron-magnetic-moment
    ("-0.96623647E-26" "0.00000023E-26" "2.4E-7")
  "Neutron magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munn>.")

(define-constant neutron-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.04187563E-3" "0.00000025E-3" "2.4E-7")
  "Neutron magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmub>.")

(define-constant neutron-magnetic-moment-to-nuclear-magneton-ratio
    ("-1.91304272" "0.00000045" "2.4E-7")
  "Neutron magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmun>.")

(define-constant neutron-mass
    ("1.674927351E-27" "0.000000074E-27" "4.4E-8")
  "Neutron mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mn>.")

(define-constant neutron-mass-energy-equivalent
    ("1.505349631E-10" "0.000000066E-10" "4.4E-8")
  "Neutron mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2>.")

(define-constant neutron-mass-energy-equivalent-in-MeV
    ("939.565379" "0.000021" "2.2E-8")
  "Neutron mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2mev>.")

(define-constant neutron-mass-in-u
    ("1.00866491600" "0.00000000043" "4.2E-10")
  "Neutron mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnu>.")

(define-constant neutron-molar-mass
    ("1.00866491600E-3" "0.00000000043E-3" "4.2E-10")
  "Neutron molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmn>.")

(define-constant neutron-to-shielded-proton-magnetic-moment-ratio
    ("-0.68499694" "0.00000016" "2.4E-7")
  "Neutron to shielded proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmupp>.")

(define-constant neutron-electron-magnetic-moment-ratio
    ("1.04066882E-3" "0.00000025E-3" "2.4E-7")
  "Neutron-electron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmue>.")

(define-constant neutron-electron-mass-ratio
    ("1838.6836605" "0.0000011" "5.8E-10")
  "Neutron-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsme>.")

(define-constant neutron-muon-mass-ratio
    ("8.89248400" "0.00000022" "2.5E-8")
  "Neutron-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmmu>.")

(define-constant neutron-proton-magnetic-moment-ratio
    ("-0.68497934" "0.00000016" "2.4E-7")
  "Neutron-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmup>.")

(define-constant neutron-proton-mass-difference
    ("2.30557392E-30" "0.00000076E-30" "3.3E-7")
  "Neutron-proton mass difference.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmp>.")

(define-constant neutron-proton-mass-difference-energy-equivalent
    ("2.07214650E-13" "0.00000068E-13" "3.3E-7")
  "Neutron-proton mass difference energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2>.")

(define-constant neutron-proton-mass-difference-energy-equivalent-in-MeV
    ("1.29333217" "0.00000042" "3.3E-7")
  "Neutron-proton mass difference energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2mev>.")

(define-constant neutron-proton-mass-difference-in-u
    ("0.00138844919" "0.00000000045" "3.3E-7")
  "Neutron-proton mass difference in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpu>.")

(define-constant neutron-proton-mass-ratio
    ("1.00137841917" "0.00000000045" "4.5E-10")
  "Neutron-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmp>.")

(define-constant neutron-tau-mass-ratio
    ("0.528790" "0.000048" "9.0E-5")
  "Neutron-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmtau>.")

(define-constant Newtonian-constant-of-gravitation
    ("6.67384E-11" "0.00080E-11" "1.2E-4")
  "Newtonian constant of gravitation.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bg>.")

(define-constant Newtonian-constant-of-gravitation-over-h-bar-c
    ("6.70837E-39" "0.00080E-39" "1.2E-4")
  "Newtonian constant of gravitation over h-bar c.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bgspu>.")

(define-constant nuclear-magneton
    ("5.05078353E-27" "0.00000011E-27" "2.2E-8")
  "Nuclear magneton.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mun>.")

(define-constant nuclear-magneton-in-eV/T
    ("3.1524512605E-8" "0.0000000022E-8" "7.1E-10")
  "Nuclear magneton in eV/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munev>.")

(define-constant nuclear-magneton-in-inverse-meters-per-tesla
    ("2.542623527E-2" "0.000000056E-2" "2.2E-8")
  "Nuclear magneton in inverse meters per tesla.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshcminv>.")

(define-constant nuclear-magneton-in-K/T
    ("3.6582682E-4" "0.0000033E-4" "9.1E-7")
  "Nuclear magneton in K/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munskk>.")

(define-constant nuclear-magneton-in-MHz/T
    ("7.62259357" "0.00000017" "2.2E-8")
  "Nuclear magneton in MHz/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshhz>.")

(define-constant Planck-constant
    ("6.62606957E-34" "0.00000029E-34" "4.4E-8")
  "Planck constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?h>.")

(define-constant Planck-constant-in-eV-s
    ("4.135667516E-15" "0.000000091E-15" "2.2E-8")
  "Planck constant in eV s.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hev>.")

(define-constant Planck-constant-over-2-pi
    ("1.054571726E-34" "0.000000047E-34" "4.4E-8")
  "Planck constant over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbar>.")

(define-constant Planck-constant-over-2-pi-in-eV-s
    ("6.58211928E-16" "0.00000015E-16" "2.2E-8")
  "Planck constant over 2 pi in eV s.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbarev>.")

(define-constant Planck-constant-over-2-pi-times-c-in-MeV-fm
    ("197.3269718" "0.0000044" "2.2E-8")
  "Planck constant over 2 pi times c in MeV fm.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbcmevf>.")

(define-constant Planck-length
    ("1.616199E-35" "0.000097E-35" "6.0E-5")
  "Planck length.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkl>.")

(define-constant Planck-mass
    ("2.17651E-8" "0.00013E-8" "6.0E-5")
  "Planck mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkm>.")

(define-constant Planck-mass-energy-equivalent-in-GeV
    ("1.220932E19" "0.000073E19" "6.0E-5")
  "Planck mass energy equivalent in GeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkmc2gev>.")

(define-constant Planck-temperature
    ("1.416833E32" "0.000085E32" "6.0E-5")
  "Planck temperature.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plktmp>.")

(define-constant Planck-time
    ("5.39106E-44" "0.00032E-44" "6.0E-5")
  "Planck time.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkt>.")

(define-constant proton-charge-to-mass-quotient
    ("9.57883358E7" "0.00000021E7" "2.2E-8")
  "Proton charge to mass quotient.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esmp>.")

(define-constant proton-Compton-wavelength
    ("1.32140985623E-15" "0.00000000094E-15" "7.1E-10")
  "Proton Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwl>.")

(define-constant proton-Compton-wavelength-over-2-pi
    ("0.21030891047E-15" "0.00000000015E-15" "7.1E-10")
  "Proton Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwlbar>.")

(define-constant proton-g-factor
    ("5.585694713" "0.000000046" "8.2E-9")
  "Proton g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gp>.")

(define-constant proton-gyromagnetic-ratio
    ("2.675222005E8" "0.000000063E8" "2.4E-8")
  "Proton gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammap>.")

(define-constant proton-gyromagnetic-ratio-over-2-pi
    ("42.5774806" "0.0000010" "2.4E-8")
  "Proton gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapbar>.")

(define-constant proton-magnetic-moment
    ("1.410606743E-26" "0.000000033E-26" "2.4E-8")
  "Proton magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mup>.")

(define-constant proton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.521032210E-3" "0.000000012E-3" "8.1E-9")
  "Proton magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmub>.")

(define-constant proton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.792847356" "0.000000023" "8.2E-9")
  "Proton magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmun>.")

(define-constant proton-magnetic-shielding-correction
    ("25.694E-6" "0.014E-6" "5.3E-4")
  "Proton magnetic shielding correction.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmapp>.")

(define-constant proton-mass
    ("1.672621777E-27" "0.000000074E-27" "4.4E-8")
  "Proton mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mp>.")

(define-constant proton-mass-energy-equivalent
    ("1.503277484E-10" "0.000000066E-10" "4.4E-8")
  "Proton mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2>.")

(define-constant proton-mass-energy-equivalent-in-MeV
    ("938.272046" "0.000021" "2.2E-8")
  "Proton mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2mev>.")

(define-constant proton-mass-in-u
    ("1.007276466812" "0.000000000090" "8.9E-11")
  "Proton mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpu>.")

(define-constant proton-molar-mass
    ("1.007276466812E-3" "0.000000000090E-3" "8.9E-11")
  "Proton molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmp>.")

(define-constant proton-rms-charge-radius
    ("0.8775E-15" "0.0051E-15" "5.9E-3")
  "Proton rms charge radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rp>.")

(define-constant proton-electron-mass-ratio
    ("1836.15267245" "0.00000075" "4.1E-10")
  "Proton-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsme>.")

(define-constant proton-muon-mass-ratio
    ("8.88024331" "0.00000022" "2.5E-8")
  "Proton-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmmu>.")

(define-constant proton-neutron-magnetic-moment-ratio
    ("-1.45989806" "0.00000034" "2.4E-7")
  "Proton-neutron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmunn>.")

(define-constant proton-neutron-mass-ratio
    ("0.99862347826" "0.00000000045" "4.5E-10")
  "Proton-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmn>.")

(define-constant proton-tau-mass-ratio
    ("0.528063" "0.000048" "9.0E-5")
  "Proton-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmtau>.")

(define-constant quantum-of-circulation
    ("3.6369475520E-4" "0.0000000024E-4" "6.5E-10")
  "Quantum of circulation.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?qucirchs2me>.")

(define-constant quantum-of-circulation-times-2
    ("7.2738951040E-4" "0.0000000047E-4" "6.5E-10")
  "Quantum of circulation times 2.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hsme>.")

(define-constant Rydberg-constant
    ("10973731.568539" "0.000055" "5.0E-12")
  "Rydberg constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ryd>.")

(define-constant Rydberg-constant-times-c-in-Hz
    ("3.289841960364E15" "0.000000000017E15" "5.0E-12")
  "Rydberg constant times c in Hz.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydchz>.")

(define-constant Rydberg-constant-times-hc-in-eV
    ("13.60569253" "0.00000030" "2.2E-8")
  "Rydberg constant times hc in eV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcev>.")

(define-constant Rydberg-constant-times-hc-in-J
    ("2.179872171E-18" "0.000000096E-18" "4.4E-8")
  "Rydberg constant times hc in J.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcj>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-100000Pa
    ("-1.1517078" "0.0000023" "2.0E-6")
  "Sackur-Tetrode constant (1 K, 100000 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0sr>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-101325Pa
    ("-1.1648708" "0.0000023" "1.9E-6")
  "Sackur-Tetrode constant (1 K, 101325 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0srstd>.")

(define-constant second-radiation-constant
    ("1.4387770E-2" "0.0000013E-2" "9.1E-7")
  "Second radiation constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c22ndrc>.")

(define-constant shielded-helion-gyromagnetic-ratio
    ("2.037894659E8" "0.000000051E8" "2.5E-8")
  "Shielded helion gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahp>.")

(define-constant shielded-helion-gyromagnetic-ratio-over-2-pi
    ("32.43410084" "0.00000081" "2.5E-8")
  "Shielded helion gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahpbar>.")

(define-constant shielded-helion-magnetic-moment
    ("-1.074553044E-26" "0.000000027E-26" "2.5E-8")
  "Shielded helion magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhp>.")

(define-constant shielded-helion-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.158671471E-3" "0.000000014E-3" "1.2E-8")
  "Shielded helion magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmub>.")

(define-constant shielded-helion-magnetic-moment-to-nuclear-magneton-ratio
    ("-2.127497718" "0.000000025" "1.2E-8")
  "Shielded helion magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmun>.")

(define-constant shielded-helion-to-proton-magnetic-moment-ratio
    ("-0.761766558" "0.000000011" "1.4E-8")
  "Shielded helion to proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmup>.")

(define-constant shielded-helion-to-shielded-proton-magnetic-moment-ratio
    ("-0.7617861313" "0.0000000033" "4.3E-9")
  "Shielded helion to shielded proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmupp>.")

(define-constant shielded-proton-gyromagnetic-ratio
    ("2.675153268E8" "0.000000066E8" "2.5E-8")
  "Shielded proton gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapp>.")

(define-constant shielded-proton-gyromagnetic-ratio-over-2-pi
    ("42.5763866" "0.0000010" "2.5E-8")
  "Shielded proton gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammappbar>.")

(define-constant shielded-proton-magnetic-moment
    ("1.410570499E-26" "0.000000035E-26" "2.5E-8")
  "Shielded proton magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupp>.")

(define-constant shielded-proton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.520993128E-3" "0.000000017E-3" "1.1E-8")
  "Shielded proton magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmub>.")

(define-constant shielded-proton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.792775598" "0.000000030" "1.1E-8")
  "Shielded proton magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmun>.")

(define-constant speed-of-light-in-vacuum
    ("299792458" "0" "0")
  "Speed of light in vacuum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c>.")

(define-constant standard-acceleration-of-gravity
    ("9.80665" "0" "0")
  "Standard acceleration of gravity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gn>.")

(define-constant standard-atmosphere
    ("101325" "0" "0")
  "Standard atmosphere.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdatm>.")

(define-constant standard-state-pressure
    ("100000" "0" "0")
  "Standard-state pressure.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdspr>.")

(define-constant Stefan-Boltzmann-constant
    ("5.670373E-8" "0.000021E-8" "3.6E-6")
  "Stefan-Boltzmann constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigma>.")

(define-constant tau-Compton-wavelength
    ("0.697787E-15" "0.000063E-15" "9.0E-5")
  "Tau Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwl>.")

(define-constant tau-Compton-wavelength-over-2-pi
    ("0.111056E-15" "0.000010E-15" "9.0E-5")
  "Tau Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwlbar>.")

(define-constant tau-mass
    ("3.16747E-27" "0.00029E-27" "9.0E-5")
  "Tau mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtau>.")

(define-constant tau-mass-energy-equivalent
    ("2.84678E-10" "0.00026E-10" "9.0E-5")
  "Tau mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2>.")

(define-constant tau-mass-energy-equivalent-in-MeV
    ("1776.82" "0.16" "9.0E-5")
  "Tau mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2mev>.")

(define-constant tau-mass-in-u
    ("1.90749" "0.00017" "9.0E-5")
  "Tau mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauu>.")

(define-constant tau-molar-mass
    ("1.90749E-3" "0.00017E-3" "9.0E-5")
  "Tau molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmtau>.")

(define-constant tau-electron-mass-ratio
    ("3477.15" "0.31" "9.0E-5")
  "Tau-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausme>.")

(define-constant tau-muon-mass-ratio
    ("16.8167" "0.0015" "9.0E-5")
  "Tau-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmmu>.")

(define-constant tau-neutron-mass-ratio
    ("1.89111" "0.00017" "9.0E-5")
  "Tau-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmn>.")

(define-constant tau-proton-mass-ratio
    ("1.89372" "0.00017" "9.0E-5")
  "Tau-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmp>.")

(define-constant Thomson-cross-section
    ("0.6652458734E-28" "0.0000000013E-28" "1.9E-9")
  "Thomson cross section.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmae>.")

(define-constant triton-g-factor
    ("5.957924896" "0.000000076" "1.3E-8")
  "Triton g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gtn>.")

(define-constant triton-magnetic-moment
    ("1.504609447E-26" "0.000000038E-26" "2.6E-8")
  "Triton magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mut>.")

(define-constant triton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.622393657E-3" "0.000000021E-3" "1.3E-8")
  "Triton magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmub>.")

(define-constant triton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.978962448" "0.000000038" "1.3E-8")
  "Triton magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmun>.")

(define-constant triton-mass
    ("5.00735630E-27" "0.00000022E-27" "4.4E-8")
  "Triton mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mt>.")

(define-constant triton-mass-energy-equivalent
    ("4.50038741E-10" "0.00000020E-10" "4.4E-8")
  "Triton mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2>.")

(define-constant triton-mass-energy-equivalent-in-MeV
    ("2808.921005" "0.000062" "2.2E-8")
  "Triton mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2mev>.")

(define-constant triton-mass-in-u
    ("3.0155007134" "0.0000000025" "8.2E-10")
  "Triton mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtu>.")

(define-constant triton-molar-mass
    ("3.0155007134E-3" "0.0000000025E-3" "8.2E-10")
  "Triton molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmt>.")

(define-constant triton-electron-mass-ratio
    ("5496.9215267" "0.0000050" "9.1E-10")
  "Triton-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsme>.")

(define-constant triton-proton-mass-ratio
    ("2.9937170308" "0.0000000025" "8.2E-10")
  "Triton-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsmp>.")

(define-constant unified-atomic-mass-unit
    ("1.660538921E-27" "0.000000073E-27" "4.4E-8")
  "Unified atomic mass unit.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tukg>.")

(define-constant von-Klitzing-constant
    ("25812.8074434" "0.0000084" "3.2E-10")
  "Von Klitzing constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk>.")

(define-constant weak-mixing-angle
    ("0.2223" "0.0021" "9.5E-3")
  "Weak mixing angle.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sin2th>.")

(define-constant Wien-frequency-displacement-law-constant
    ("5.8789254E10" "0.0000053E10" "9.1E-7")
  "Wien frequency displacement law constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bpwien>.")

(define-constant Wien-wavelength-displacement-law-constant
    ("2.8977721E-3" "0.0000026E-3" "9.1E-7")
  "Wien wavelength displacement law constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bwien>.")

(define-constant 220-lattice-spacing-of-silicon
    ("192.0155714E-12" "0.0000032E-12" "1.6E-8")
  "{220} lattice spacing of silicon.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?d220sil>.")

;;; codata-2010.lisp ends here
