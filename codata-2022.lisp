;;; codata-2022.lisp --- 2022 CODATA recommended values

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

(in-package :codata-recommended-values-2022)

(defmacro with-early-bindings (&body body)
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
     (declare (ignorable c h e k na))
     ,@body))

(define-constant alpha-particle-mass
    ("6.6446573450E-27" "0.0000000021E-27" "3.1E-10")
  "Alpha particle mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mal>.")

(define-constant alpha-particle-mass-energy-equivalent
    ("5.9719201997E-10" "0.0000000019E-10" "3.1E-10")
  "Alpha particle mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2>.")

(define-constant alpha-particle-mass-energy-equivalent-in-MeV
    ("3727.3794118" "0.0000012" "3.1E-10")
  "Alpha particle mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2mev>.")

(define-constant alpha-particle-mass-in-u
    ("4.001506179129" "0.000000000062" "1.6E-11")
  "Alpha particle mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malu>.")

(define-constant alpha-particle-molar-mass
    ("4.0015061833E-3" "0.0000000012E-3" "3.1E-10")
  "Alpha particle molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmal>.")

(define-constant alpha-particle-relative-atomic-mass
    ("4.001506179129" "0.000000000062" "1.6E-11")
  "Alpha particle relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aral>.")

(define-constant alpha-particle-electron-mass-ratio
    ("7294.29954171" "0.00000017" "2.4E-11")
  "Alpha particle-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsme>.")

(define-constant alpha-particle-proton-mass-ratio
    ("3.972599690252" "0.000000000070" "1.8E-11")
  "Alpha particle-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsmp>.")

(define-constant Angstrom-star
    ("1.00001495E-10" "0.00000090E-10" "9.0E-7")
  "Angstrom star.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?angstar>.")

(define-constant atomic-mass-constant
    ("1.66053906892E-27" "0.00000000052E-27" "3.1E-10")
  "Atomic mass constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?u>.")

(define-constant atomic-mass-constant-energy-equivalent
    ("1.49241808768E-10" "0.00000000046E-10" "3.1E-10")
  "Atomic mass constant energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uj>.")

(define-constant atomic-mass-constant-energy-equivalent-in-MeV
    ("931.49410372" "0.00000029" "3.1E-10")
  "Atomic mass constant energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muc2mev>.")

(define-constant atomic-mass-unit-electron-volt-relationship
    ("9.3149410372E8" "0.0000000029E8" "3.1E-10")
  "Atomic mass unit-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uev>.")

(define-constant atomic-mass-unit-hartree-relationship
    ("3.4231776922E7" "0.0000000011E7" "3.1E-10")
  "Atomic mass unit-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhr>.")

(define-constant atomic-mass-unit-hertz-relationship
    ("2.25234272185E23" "0.00000000070E23" "3.1E-10")
  "Atomic mass unit-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhz>.")

(define-constant atomic-mass-unit-inverse-meter-relationship
    ("7.5130066209E14" "0.0000000023E14" "3.1E-10")
  "Atomic mass unit-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uminv>.")

(define-constant atomic-mass-unit-joule-relationship
    ("1.49241808768E-10" "0.00000000046E-10" "3.1E-10")
  "Atomic mass unit-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Ruj>.")

(define-constant atomic-mass-unit-kelvin-relationship
    ("1.08095402067E13" "0.00000000034E13" "3.1E-10")
  "Atomic mass unit-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uk>.")

(define-constant atomic-mass-unit-kilogram-relationship
    ("1.66053906892E-27" "0.00000000052E-27" "3.1E-10")
  "Atomic mass unit-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Rukg>.")

(define-constant atomic-unit-of-first-hyperpolarizability
    ("3.2063612996E-53" "0.0000000015E-53" "4.7E-10")
  "Atomic unit of first hyperpolarizability.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auhypol>.")

(define-constant atomic-unit-of-second-hyperpolarizability
    ("6.2353799735E-65" "0.0000000039E-65" "6.2E-10")
  "Atomic unit of second hyperpolarizability.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?au2hypol>.")

(define-constant atomic-unit-of-action
    ((with-early-bindings (/ h 2 pi)) "0" "0")
  "Atomic unit of action.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Ahbar>.")

(define-constant atomic-unit-of-charge
    ("1.602176634E-19" "0" "0")
  "Atomic unit of charge.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Ae>.")

(define-constant atomic-unit-of-charge-density
    ("1.08120238677E12" "0.00000000051E12" "4.7E-10")
  "Atomic unit of charge density.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucd>.")

(define-constant atomic-unit-of-current
    ("6.6236182375082E-3" "0.0000000000072E-3" "1.1E-12")
  "Atomic unit of current.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucur>.")

(define-constant atomic-unit-of-electric-dipole-moment
    ("8.4783536198E-30" "0.0000000013E-30" "1.6E-10")
  "Atomic unit of electric dipole moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auedm>.")

(define-constant atomic-unit-of-electric-field
    ("5.14220675112E11" "0.00000000080E11" "1.6E-10")
  "Atomic unit of electric field.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefld>.")

(define-constant atomic-unit-of-electric-field-gradient
    ("9.7173624424E21" "0.0000000030E21" "3.1E-10")
  "Atomic unit of electric field gradient.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefg>.")

(define-constant atomic-unit-of-electric-polarizability
    ("1.64877727212E-41" "0.00000000051E-41" "3.1E-10")
  "Atomic unit of electric polarizability.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auepol>.")

(define-constant atomic-unit-of-electric-potential
    ("27.211386245981" "0.000000000030" "1.1E-12")
  "Atomic unit of electric potential.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auep>.")

(define-constant atomic-unit-of-electric-quadrupole-moment
    ("4.4865515185E-40" "0.0000000014E-40" "3.1E-10")
  "Atomic unit of electric quadrupole moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aueqm>.")

(define-constant atomic-unit-of-energy
    ("4.3597447222060E-18" "0.0000000000048E-18" "1.1E-12")
  "Atomic unit of energy.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Ahr>.")

(define-constant atomic-unit-of-force
    ("8.2387235038E-8" "0.0000000013E-8" "1.6E-10")
  "Atomic unit of force.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auforce>.")

(define-constant atomic-unit-of-length
    ("5.29177210544E-11" "0.00000000082E-11" "1.6E-10")
  "Atomic unit of length.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Abohrrada0>.")

(define-constant atomic-unit-of-magnetic-dipole-moment
    ("1.85480201315E-23" "0.00000000058E-23" "3.1E-10")
  "Atomic unit of magnetic dipole moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumdm>.")

(define-constant atomic-unit-of-magnetic-flux-density
    ("2.35051757077E5" "0.00000000073E5" "3.1E-10")
  "Atomic unit of magnetic flux density.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumfd>.")

(define-constant atomic-unit-of-magnetizability
    ("7.8910365794E-29" "0.0000000049E-29" "6.2E-10")
  "Atomic unit of magnetizability.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumag>.")

(define-constant atomic-unit-of-mass
    ("9.1093837139E-31" "0.0000000028E-31" "3.1E-10")
  "Atomic unit of mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Ame>.")

(define-constant atomic-unit-of-momentum
    ("1.99285191545E-24" "0.00000000031E-24" "1.6E-10")
  "Atomic unit of momentum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumom>.")

(define-constant atomic-unit-of-permittivity
    ("1.11265005620E-10" "0.00000000017E-10" "1.6E-10")
  "Atomic unit of permittivity.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auperm>.")

(define-constant atomic-unit-of-time
    ("2.4188843265864E-17" "0.0000000000026E-17" "1.1E-12")
  "Atomic unit of time.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aut>.")

(define-constant atomic-unit-of-velocity
    ("2.18769126216E6" "0.00000000034E6" "1.6E-10")
  "Atomic unit of velocity.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auvel>.")

(define-constant Avogadro-constant
    ("6.02214076E23" "0" "0")
  "Avogadro constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?na>.")

(define-constant Bohr-magneton
    ("9.2740100657E-24" "0.0000000029E-24" "3.1E-10")
  "Bohr magneton.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mub>.")

(define-constant Bohr-magneton-in-eV/T
    ("5.7883817982E-5" "0.0000000018E-5" "3.1E-10")
  "Bohr magneton in eV/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubev>.")

(define-constant Bohr-magneton-in-Hz/T
    ("1.39962449171E10" "0.00000000044E10" "3.1E-10")
  "Bohr magneton in Hz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshhz>.")

(define-constant Bohr-magneton-in-inverse-meter-per-tesla
    ("46.686447719" "0.000000015" "3.1E-10")
  "Bohr magneton in inverse meter per tesla.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshcminv>.")

(define-constant Bohr-magneton-in-K/T
    ("0.67171381472" "0.00000000021" "3.1E-10")
  "Bohr magneton in K/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubskk>.")

(define-constant Bohr-radius
    ("5.29177210544E-11" "0.00000000082E-11" "1.6E-10")
  "Bohr radius.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bohrrada0>.")

(define-constant Boltzmann-constant
    ("1.380649E-23" "0" "0")
  "Boltzmann constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?k>.")

(define-constant Boltzmann-constant-in-eV/K
    ((with-early-bindings (/ k e)) "0" "0")
  "Boltzmann constant in eV/K.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kev>.")

(define-constant Boltzmann-constant-in-Hz/K
    ((with-early-bindings (/ k h)) "0" "0")
  "Boltzmann constant in Hz/K.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshhz>.")

(define-constant Boltzmann-constant-in-inverse-meter-per-kelvin
    ((with-early-bindings (/ k h c)) "0" "0")
  "Boltzmann constant in inverse meter per kelvin.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshcminv>.")

(define-constant characteristic-impedance-of-vacuum
    ("376.730313412" "0.000000059" "1.6E-10")
  "Characteristic impedance of vacuum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?z0>.")

(define-constant classical-electron-radius
    ("2.8179403205E-15" "0.0000000013E-15" "4.7E-10")
  "Classical electron radius.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?re>.")

(define-constant Compton-wavelength
    ("2.42631023538E-12" "0.00000000076E-12" "3.1E-10")
  "Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwl>.")

(define-constant conductance-quantum
    ((with-early-bindings (/ (* 2 (expt e 2)) h)) "0" "0")
  "Conductance quantum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?conqu2e2sh>.")

(define-constant conventional-value-of-ampere-90
    ((with-early-bindings (* (/ (read-from-string "4.835979L+14") (/ (* 2 e) h))
                             (/ (read-from-string "2.5812807L+4") (/ h (expt e 2))))) "0" "0")
  "Conventional value of ampere-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ampere90>.")

(define-constant conventional-value-of-coulomb-90
    ((with-early-bindings (* (/ (read-from-string "4.835979L+14") (/ (* 2 e) h))
                             (/ (read-from-string "2.5812807L+4") (/ h (expt e 2))))) "0" "0")
  "Conventional value of coulomb-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?charge90>.")

(define-constant conventional-value-of-farad-90
    ((with-early-bindings (/ (read-from-string "2.5812807L+4") (/ h (expt e 2)))) "0" "0")
  "Conventional value of farad-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?capacitance90>.")

(define-constant conventional-value-of-henry-90
    ((with-early-bindings (/ (/ h (expt e 2)) (read-from-string "2.5812807L+4"))) "0" "0")
  "Conventional value of henry-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?inductance90>.")

(define-constant conventional-value-of-Josephson-constant
    ("483597.9E9" "0" "0")
  "Conventional value of Josephson constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj90>.")

(define-constant conventional-value-of-ohm-90
    ((with-early-bindings (/ (/ h (expt e 2)) (read-from-string "2.5812807L+4"))) "0" "0")
  "Conventional value of ohm-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ohm90>.")

(define-constant conventional-value-of-volt-90
    ((with-early-bindings (/ (read-from-string "4.835979L+14") (/ (* 2 e) h))) "0" "0")
  "Conventional value of volt-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?volt90>.")

(define-constant conventional-value-of-von-Klitzing-constant
    ("25812.807" "0" "0")
  "Conventional value of von Klitzing constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk90>.")

(define-constant conventional-value-of-watt-90
    ((with-early-bindings (* (expt (/ (read-from-string "4.835979L+14") (/ (* 2 e) h)) 2)
                             (/ (read-from-string "2.5812807L+4") (/ h (expt e 2))))) "0" "0")
  "Conventional value of watt-90.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?power90>.")

(define-constant copper-x-unit
    ("1.00207697E-13" "0.00000028E-13" "2.8E-7")
  "Copper x unit.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xucukalph1>.")

(define-constant deuteron-g-factor
    ("0.8574382335" "0.0000000022" "2.6E-9")
  "Deuteron g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gdn>.")

(define-constant deuteron-magnetic-moment
    ("4.330735087E-27" "0.000000011E-27" "2.6E-9")
  "Deuteron magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mud>.")

(define-constant deuteron-magnetic-moment-to-Bohr-magneton-ratio
    ("4.669754568E-4" "0.000000012E-4" "2.6E-9")
  "Deuteron magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmub>.")

(define-constant deuteron-magnetic-moment-to-nuclear-magneton-ratio
    ("0.8574382335" "0.0000000022" "2.6E-9")
  "Deuteron magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmun>.")

(define-constant deuteron-mass
    ("3.3435837768E-27" "0.0000000010E-27" "3.1E-10")
  "Deuteron mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?md>.")

(define-constant deuteron-mass-energy-equivalent
    ("3.00506323491E-10" "0.00000000094E-10" "3.1E-10")
  "Deuteron mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2>.")

(define-constant deuteron-mass-energy-equivalent-in-MeV
    ("1875.61294500" "0.00000058" "3.1E-10")
  "Deuteron mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2mev>.")

(define-constant deuteron-mass-in-u
    ("2.013553212544" "0.000000000015" "7.4E-12")
  "Deuteron mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdu>.")

(define-constant deuteron-molar-mass
    ("2.01355321466E-3" "0.00000000063E-3" "3.1E-10")
  "Deuteron molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmd>.")

(define-constant deuteron-relative-atomic-mass
    ("2.013553212544" "0.000000000015" "7.4E-12")
  "Deuteron relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ard>.")

(define-constant deuteron-rms-charge-radius
    ("2.12778E-15" "0.00027E-15" "1.3E-4")
  "Deuteron rms charge radius.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rd>.")

(define-constant deuteron-electron-magnetic-moment-ratio
    ("-4.664345550E-4" "0.000000012E-4" "2.6E-9")
  "Deuteron-electron magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmuem>.")

(define-constant deuteron-electron-mass-ratio
    ("3670.482967655" "0.000000063" "1.7E-11")
  "Deuteron-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsme>.")

(define-constant deuteron-neutron-magnetic-moment-ratio
    ("-0.44820652" "0.00000011" "2.4E-7")
  "Deuteron-neutron magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmunn>.")

(define-constant deuteron-proton-magnetic-moment-ratio
    ("0.30701220930" "0.00000000079" "2.6E-9")
  "Deuteron-proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmup>.")

(define-constant deuteron-proton-mass-ratio
    ("1.9990075012699" "0.0000000000084" "4.2E-12")
  "Deuteron-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsmp>.")

(define-constant electron-charge-to-mass-quotient
    ("-1.75882000838E11" "0.00000000055E11" "3.1E-10")
  "Electron charge to mass quotient.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esme>.")

(define-constant electron-g-factor
    ("-2.00231930436092" "0.00000000000036" "1.8E-13")
  "Electron g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gem>.")

(define-constant electron-gyromagnetic-ratio
    ("1.76085962784E11" "0.00000000055E11" "3.1E-10")
  "Electron gyromagnetic ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammae>.")

(define-constant electron-gyromagnetic-ratio-in-MHz/T
    ("28024.9513861" "0.0000087" "3.1E-10")
  "Electron gyromagnetic ratio in MHz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammaebar>.")

(define-constant electron-magnetic-moment
    ("-9.2847646917E-24" "0.0000000029E-24" "3.1E-10")
  "Electron magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muem>.")

(define-constant electron-magnetic-moment-anomaly
    ("1.15965218046E-3" "0.00000000018E-3" "1.6E-10")
  "Electron magnetic moment anomaly.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ae>.")

(define-constant electron-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.00115965218046" "0.00000000000018" "1.8E-13")
  "Electron magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmub>.")

(define-constant electron-magnetic-moment-to-nuclear-magneton-ratio
    ("-1838.281971877" "0.000000032" "1.7E-11")
  "Electron magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmun>.")

(define-constant electron-mass
    ("9.1093837139E-31" "0.0000000028E-31" "3.1E-10")
  "Electron mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?me>.")

(define-constant electron-mass-energy-equivalent
    ("8.1871057880E-14" "0.0000000026E-14" "3.1E-10")
  "Electron mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2>.")

(define-constant electron-mass-energy-equivalent-in-MeV
    ("0.51099895069" "0.00000000016" "3.1E-10")
  "Electron mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2mev>.")

(define-constant electron-mass-in-u
    ("5.485799090441E-4" "0.000000000097E-4" "1.8E-11")
  "Electron mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?meu>.")

(define-constant electron-molar-mass
    ("5.4857990962E-7" "0.0000000017E-7" "3.1E-10")
  "Electron molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mme>.")

(define-constant electron-relative-atomic-mass
    ("5.485799090441E-4" "0.000000000097E-4" "1.8E-11")
  "Electron relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?are>.")

(define-constant electron-to-alpha-particle-mass-ratio
    ("1.370933554733E-4" "0.000000000032E-4" "2.4E-11")
  "Electron to alpha particle mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmalpha>.")

(define-constant electron-to-shielded-helion-magnetic-moment-ratio
    ("864.05823986" "0.00000070" "8.1E-10")
  "Electron to shielded helion magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmuhp>.")

(define-constant electron-to-shielded-proton-magnetic-moment-ratio
    ("-658.2275856" "0.0000027" "4.1E-9")
  "Electron to shielded proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmupp>.")

(define-constant electron-volt
    ("1.602176634E-19" "0" "0")
  "Electron volt.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evj>.")

(define-constant electron-volt-atomic-mass-unit-relationship
    ("1.07354410083E-9" "0.00000000033E-9" "3.1E-10")
  "Electron volt-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evu>.")

(define-constant electron-volt-hartree-relationship
    ("3.6749322175665E-2" "0.0000000000040E-2" "1.1E-12")
  "Electron volt-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhr>.")

(define-constant electron-volt-hertz-relationship
    ((with-early-bindings (/ e h)) "0" "0")
  "Electron volt-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhz>.")

(define-constant electron-volt-inverse-meter-relationship
    ((with-early-bindings (/ e h c)) "0" "0")
  "Electron volt-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evminv>.")

(define-constant electron-volt-joule-relationship
    ("1.602176634E-19" "0" "0")
  "Electron volt-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Revj>.")

(define-constant electron-volt-kelvin-relationship
    ((with-early-bindings (/ e k)) "0" "0")
  "Electron volt-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evk>.")

(define-constant electron-volt-kilogram-relationship
    ((with-early-bindings (/ e (expt c 2))) "0" "0")
  "Electron volt-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evkg>.")

(define-constant electron-deuteron-magnetic-moment-ratio
    ("-2143.9234921" "0.0000056" "2.6E-9")
  "Electron-deuteron magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmud>.")

(define-constant electron-deuteron-mass-ratio
    ("2.724437107629E-4" "0.000000000047E-4" "1.7E-11")
  "Electron-deuteron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmd>.")

(define-constant electron-helion-mass-ratio
    ("1.819543074649E-4" "0.000000000053E-4" "2.9E-11")
  "Electron-helion mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmh>.")

(define-constant electron-muon-magnetic-moment-ratio
    ("206.7669881" "0.0000046" "2.2E-8")
  "Electron-muon magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmumum>.")

(define-constant electron-muon-mass-ratio
    ("4.83633170E-3" "0.00000011E-3" "2.2E-8")
  "Electron-muon mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmmu>.")

(define-constant electron-neutron-magnetic-moment-ratio
    ("960.92048" "0.00023" "2.4E-7")
  "Electron-neutron magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmunn>.")

(define-constant electron-neutron-mass-ratio
    ("5.4386734416E-4" "0.0000000022E-4" "4.0E-10")
  "Electron-neutron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmn>.")

(define-constant electron-proton-magnetic-moment-ratio
    ("-658.21068789" "0.00000019" "3.0E-10")
  "Electron-proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmup>.")

(define-constant electron-proton-mass-ratio
    ("5.446170214889E-4" "0.000000000094E-4" "1.7E-11")
  "Electron-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmp>.")

(define-constant electron-tau-mass-ratio
    ("2.87585E-4" "0.00019E-4" "6.8E-5")
  "Electron-tau mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmtau>.")

(define-constant electron-triton-mass-ratio
    ("1.819200062327E-4" "0.000000000068E-4" "3.8E-11")
  "Electron-triton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmt>.")

(define-constant elementary-charge
    ("1.602176634E-19" "0" "0")
  "Elementary charge.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?e>.")

(define-constant elementary-charge-over-h-bar
    ((with-early-bindings (* (/ e h) 2 pi)) "0" "0")
  "Elementary charge over h-bar.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?eshbar>.")

(define-constant Faraday-constant
    ((with-early-bindings (* na e)) "0" "0")
  "Faraday constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f>.")

(define-constant Fermi-coupling-constant
    ("1.1663787E-5" "0.0000006E-5" "5.1E-7")
  "Fermi coupling constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gf>.")

(define-constant fine-structure-constant
    ("7.2973525643E-3" "0.0000000011E-3" "1.6E-10")
  "Fine-structure constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alph>.")

(define-constant first-radiation-constant
    ((with-early-bindings (* 2 pi h (expt c 2))) "0" "0")
  "First radiation constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c11strc>.")

(define-constant first-radiation-constant-for-spectral-radiance
    ((with-early-bindings (* 2 h (expt c 2))) "0" "0")
  "First radiation constant for spectral radiance.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c1l>.")

(define-constant Hartree-energy
    ("4.3597447222060E-18" "0.0000000000048E-18" "1.1E-12")
  "Hartree energy.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hr>.")

(define-constant Hartree-energy-in-eV
    ("27.211386245981" "0.000000000030" "1.1E-12")
  "Hartree energy in eV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrev>.")

(define-constant hartree-atomic-mass-unit-relationship
    ("2.92126231797E-8" "0.00000000091E-8" "3.1E-10")
  "Hartree-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hru>.")

(define-constant hartree-electron-volt-relationship
    ("27.211386245981" "0.000000000030" "1.1E-12")
  "Hartree-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Rhrev>.")

(define-constant hartree-hertz-relationship
    ("6.5796839204999E15" "0.0000000000072E15" "1.1E-12")
  "Hartree-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrhz>.")

(define-constant hartree-inverse-meter-relationship
    ("2.1947463136314E7" "0.0000000000024E7" "1.1E-12")
  "Hartree-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrminv>.")

(define-constant hartree-joule-relationship
    ("4.3597447222060E-18" "0.0000000000048E-18" "1.1E-12")
  "Hartree-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrj>.")

(define-constant hartree-kelvin-relationship
    ("3.1577502480398E5" "0.0000000000034E5" "1.1E-12")
  "Hartree-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrk>.")

(define-constant hartree-kilogram-relationship
    ("4.8508702095419E-35" "0.0000000000053E-35" "1.1E-12")
  "Hartree-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrkg>.")

(define-constant helion-g-factor
    ("-4.2552506995" "0.0000000034" "8.1E-10")
  "Helion g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ghn>.")

(define-constant helion-magnetic-moment
    ("-1.07461755198E-26" "0.00000000093E-26" "8.7E-10")
  "Helion magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muh>.")

(define-constant helion-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.15874098083E-3" "0.00000000094E-3" "8.1E-10")
  "Helion magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmub>.")

(define-constant helion-magnetic-moment-to-nuclear-magneton-ratio
    ("-2.1276253498" "0.0000000017" "8.1E-10")
  "Helion magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmun>.")

(define-constant helion-mass
    ("5.0064127862E-27" "0.0000000016E-27" "3.1E-10")
  "Helion mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mh>.")

(define-constant helion-mass-energy-equivalent
    ("4.4995394185E-10" "0.0000000014E-10" "3.1E-10")
  "Helion mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2>.")

(define-constant helion-mass-energy-equivalent-in-MeV
    ("2808.39161112" "0.00000088" "3.1E-10")
  "Helion mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2mev>.")

(define-constant helion-mass-in-u
    ("3.014932246932" "0.000000000074" "2.5E-11")
  "Helion mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhu>.")

(define-constant helion-molar-mass
    ("3.01493225010E-3" "0.00000000094E-3" "3.1E-10")
  "Helion molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmh>.")

(define-constant helion-relative-atomic-mass
    ("3.014932246932" "0.000000000074" "2.5E-11")
  "Helion relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?arh>.")

(define-constant helion-shielding-shift
    ("5.9967029E-5" "0.0000023E-5" "3.8E-7")
  "Helion shielding shift.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmah>.")

(define-constant helion-electron-mass-ratio
    ("5495.88527984" "0.00000016" "2.9E-11")
  "Helion-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsme>.")

(define-constant helion-proton-mass-ratio
    ("2.993152671552" "0.000000000070" "2.4E-11")
  "Helion-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsmp>.")

(define-constant hertz-atomic-mass-unit-relationship
    ("4.4398216590E-24" "0.0000000014E-24" "3.1E-10")
  "Hertz-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzu>.")

(define-constant hertz-electron-volt-relationship
    ((with-early-bindings (/ h e)) "0" "0")
  "Hertz-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzev>.")

(define-constant hertz-hartree-relationship
    ("1.5198298460574E-16" "0.0000000000017E-16" "1.1E-12")
  "Hertz-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzhr>.")

(define-constant hertz-inverse-meter-relationship
    ((with-early-bindings (/ c)) "0" "0")
  "Hertz-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzminv>.")

(define-constant hertz-joule-relationship
    ("6.62607015E-34" "0" "0")
  "Hertz-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzj>.")

(define-constant hertz-kelvin-relationship
    ((with-early-bindings (/ h k)) "0" "0")
  "Hertz-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzk>.")

(define-constant hertz-kilogram-relationship
    ((with-early-bindings (/ h (expt c 2))) "0" "0")
  "Hertz-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzkg>.")

(define-constant hyperfine-transition-frequency-of-caesium-133
    ("9192631770" "0" "0")
  "Hyperfine transition frequency of caesium-133.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nucs>.")

(define-constant inverse-fine-structure-constant
    ("137.035999177" "0.000000021" "1.6E-10")
  "Inverse fine-structure constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alphinv>.")

(define-constant inverse-meter-atomic-mass-unit-relationship
    ("1.33102504824E-15" "0.00000000041E-15" "3.1E-10")
  "Inverse meter-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvu>.")

(define-constant inverse-meter-electron-volt-relationship
    ((with-early-bindings (/ (* h c) e)) "0" "0")
  "Inverse meter-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvev>.")

(define-constant inverse-meter-hartree-relationship
    ("4.5563352529132E-8" "0.0000000000050E-8" "1.1E-12")
  "Inverse meter-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhr>.")

(define-constant inverse-meter-hertz-relationship
    ("299792458" "0" "0")
  "Inverse meter-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhz>.")

(define-constant inverse-meter-joule-relationship
    ((with-early-bindings (* h c)) "0" "0")
  "Inverse meter-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvj>.")

(define-constant inverse-meter-kelvin-relationship
    ((with-early-bindings (/ (* h c) k)) "0" "0")
  "Inverse meter-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvk>.")

(define-constant inverse-meter-kilogram-relationship
    ((with-early-bindings (/ h c)) "0" "0")
  "Inverse meter-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvkg>.")

(define-constant inverse-of-conductance-quantum
    ((with-early-bindings (/ h 2 (expt e 2))) "0" "0")
  "Inverse of conductance quantum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?invconqu>.")

(define-constant Josephson-constant
    ((with-early-bindings (/ (* 2 e) h)) "0" "0")
  "Josephson constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kjos>.")

(define-constant joule-atomic-mass-unit-relationship
    ("6.7005352471E9" "0.0000000021E9" "3.1E-10")
  "Joule-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ju>.")

(define-constant joule-electron-volt-relationship
    ((with-early-bindings (/ e)) "0" "0")
  "Joule-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jev>.")

(define-constant joule-hartree-relationship
    ("2.2937122783969E17" "0.0000000000025E17" "1.1E-12")
  "Joule-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhr>.")

(define-constant joule-hertz-relationship
    ((with-early-bindings (/ h)) "0" "0")
  "Joule-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhz>.")

(define-constant joule-inverse-meter-relationship
    ((with-early-bindings (/ (* h c))) "0" "0")
  "Joule-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jminv>.")

(define-constant joule-kelvin-relationship
    ((with-early-bindings (/ k)) "0" "0")
  "Joule-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jk>.")

(define-constant joule-kilogram-relationship
    ((with-early-bindings (/ (expt c 2))) "0" "0")
  "Joule-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jkg>.")

(define-constant kelvin-atomic-mass-unit-relationship
    ("9.2510872884E-14" "0.0000000029E-14" "3.1E-10")
  "Kelvin-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ku>.")

(define-constant kelvin-electron-volt-relationship
    ((with-early-bindings (/ k e)) "0" "0")
  "Kelvin-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Rkev>.")

(define-constant kelvin-hartree-relationship
    ("3.1668115634564E-6" "0.0000000000035E-6" "1.1E-12")
  "Kelvin-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khr>.")

(define-constant kelvin-hertz-relationship
    ((with-early-bindings (/ k h)) "0" "0")
  "Kelvin-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khz>.")

(define-constant kelvin-inverse-meter-relationship
    ((with-early-bindings (/ k h c)) "0" "0")
  "Kelvin-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kminv>.")

(define-constant kelvin-joule-relationship
    ("1.380649E-23" "0" "0")
  "Kelvin-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj>.")

(define-constant kelvin-kilogram-relationship
    ((with-early-bindings (/ k (expt c 2))) "0" "0")
  "Kelvin-kilogram relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kkg>.")

(define-constant kilogram-atomic-mass-unit-relationship
    ("6.0221407537E26" "0.0000000019E26" "3.1E-10")
  "Kilogram-atomic mass unit relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgu>.")

(define-constant kilogram-electron-volt-relationship
    ((with-early-bindings (/ (expt c 2) e)) "0" "0")
  "Kilogram-electron volt relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgev>.")

(define-constant kilogram-hartree-relationship
    ("2.0614857887415E34" "0.0000000000022E34" "1.1E-12")
  "Kilogram-hartree relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghr>.")

(define-constant kilogram-hertz-relationship
    ((with-early-bindings (/ (expt c 2) h)) "0" "0")
  "Kilogram-hertz relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghz>.")

(define-constant kilogram-inverse-meter-relationship
    ((with-early-bindings (/ c h)) "0" "0")
  "Kilogram-inverse meter relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgminv>.")

(define-constant kilogram-joule-relationship
    ((with-early-bindings (expt c 2)) "0" "0")
  "Kilogram-joule relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgj>.")

(define-constant kilogram-kelvin-relationship
    ((with-early-bindings (/ (expt c 2) k)) "0" "0")
  "Kilogram-kelvin relationship.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgk>.")

(define-constant lattice-parameter-of-silicon
    ("5.431020511E-10" "0.000000089E-10" "1.6E-8")
  "Lattice parameter of silicon.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?asil>.")

(define-constant lattice-spacing-of-ideal-silicon-at-220
    ("1.920155716E-10" "0.000000032E-10" "1.6E-8")
  "Lattice spacing of ideal silicon (220).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?d220sil>.")

(define-constant Loschmidt-constant-at-273.15K-and-100000Pa
    ((with-early-bindings (/ 100000 k 5463/20)) "0" "0")
  "Loschmidt constant (273.15 K, 100000 Pa).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0>.")

(define-constant Loschmidt-constant-at-273.15K-and-101325Pa
    ((with-early-bindings (/ 101325 k 5463/20)) "0" "0")
  "Loschmidt constant (273.15 K, 101325 Pa).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0std>.")

(define-constant luminous-efficacy
    ("683" "0" "0")
  "Luminous efficacy.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kcd>.")

(define-constant magnetic-flux-quantum
    ((with-early-bindings (/ h 2 e)) "0" "0")
  "Magnetic flux quantum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?flxquhs2e>.")

(define-constant molar-gas-constant
    ((with-early-bindings (* na k)) "0" "0")
  "Molar gas constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?r>.")

(define-constant molar-mass-constant
    ("1.00000000105E-3" "0.00000000031E-3" "3.1E-10")
  "Molar mass constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu>.")

(define-constant molar-mass-of-carbon-12
    ("12.0000000126E-3" "0.0000000037E-3" "3.1E-10")
  "Molar mass of carbon-12.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mm12c>.")

(define-constant molar-Planck-constant
    ((with-early-bindings (* na h)) "0" "0")
  "Molar Planck constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nah>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-100000Pa
    ((with-early-bindings (/ (* na k 5463/20) 100000)) "0" "0")
  "Molar volume of ideal gas (273.15 K, 100000 Pa).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvol>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-101325Pa
    ((with-early-bindings (/ (* na k 5463/20) 101325)) "0" "0")
  "Molar volume of ideal gas (273.15 K, 101325 Pa).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolstd>.")

(define-constant molar-volume-of-silicon
    ("1.205883199E-5" "0.000000060E-5" "4.9E-8")
  "Molar volume of silicon.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolsil>.")

(define-constant molybdenum-x-unit
    ("1.00209952E-13" "0.00000053E-13" "5.3E-7")
  "Molybdenum x unit.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xumokalph1>.")

(define-constant muon-Compton-wavelength
    ("1.173444110E-14" "0.000000026E-14" "2.2E-8")
  "Muon Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwl>.")

(define-constant muon-g-factor
    ("-2.00233184123" "0.00000000082" "4.1E-10")
  "Muon g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gmum>.")

(define-constant muon-magnetic-moment
    ("-4.49044830E-26" "0.00000010E-26" "2.2E-8")
  "Muon magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumum>.")

(define-constant muon-magnetic-moment-anomaly
    ("1.16592062E-3" "0.00000041E-3" "3.5E-7")
  "Muon magnetic moment anomaly.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?amu>.")

(define-constant muon-magnetic-moment-to-Bohr-magneton-ratio
    ("-4.84197048E-3" "0.00000011E-3" "2.2E-8")
  "Muon magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmub>.")

(define-constant muon-magnetic-moment-to-nuclear-magneton-ratio
    ("-8.89059704" "0.00000020" "2.2E-8")
  "Muon magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmun>.")

(define-constant muon-mass
    ("1.883531627E-28" "0.000000042E-28" "2.2E-8")
  "Muon mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmu>.")

(define-constant muon-mass-energy-equivalent
    ("1.692833804E-11" "0.000000038E-11" "2.2E-8")
  "Muon mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2>.")

(define-constant muon-mass-energy-equivalent-in-MeV
    ("105.6583755" "0.0000023" "2.2E-8")
  "Muon mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2mev>.")

(define-constant muon-mass-in-u
    ("0.1134289257" "0.0000000025" "2.2E-8")
  "Muon mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuu>.")

(define-constant muon-molar-mass
    ("1.134289258E-4" "0.000000025E-4" "2.2E-8")
  "Muon molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmmu>.")

(define-constant muon-electron-mass-ratio
    ("206.7682827" "0.0000046" "2.2E-8")
  "Muon-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusme>.")

(define-constant muon-neutron-mass-ratio
    ("0.1124545168" "0.0000000025" "2.2E-8")
  "Muon-neutron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmn>.")

(define-constant muon-proton-magnetic-moment-ratio
    ("-3.183345146" "0.000000071" "2.2E-8")
  "Muon-proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmup>.")

(define-constant muon-proton-mass-ratio
    ("0.1126095262" "0.0000000025" "2.2E-8")
  "Muon-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmp>.")

(define-constant muon-tau-mass-ratio
    ("5.94635E-2" "0.00040E-2" "6.8E-5")
  "Muon-tau mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmtau>.")

(define-constant natural-unit-of-action
    ((with-early-bindings (/ h 2 pi)) "0" "0")
  "Natural unit of action.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Nhbar>.")

(define-constant natural-unit-of-action-in-eV-s
    ((with-early-bindings (/ h 2 pi e)) "0" "0")
  "Natural unit of action in eV s.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Nhbarev>.")

(define-constant natural-unit-of-energy
    ("8.1871057880E-14" "0.0000000026E-14" "3.1E-10")
  "Natural unit of energy.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Nmec2>.")

(define-constant natural-unit-of-energy-in-MeV
    ("0.51099895069" "0.00000000016" "3.1E-10")
  "Natural unit of energy in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Nmec2mev>.")

(define-constant natural-unit-of-length
    ("3.8615926744E-13" "0.0000000012E-13" "3.1E-10")
  "Natural unit of length.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Necomwlbar>.")

(define-constant natural-unit-of-mass
    ("9.1093837139E-31" "0.0000000028E-31" "3.1E-10")
  "Natural unit of mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Nme>.")

(define-constant natural-unit-of-momentum
    ("2.73092453446E-22" "0.00000000085E-22" "3.1E-10")
  "Natural unit of momentum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec>.")

(define-constant natural-unit-of-momentum-in-MeV/c
    ("0.51099895069" "0.00000000016" "3.1E-10")
  "Natural unit of momentum in MeV/c.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mecmevsc>.")

(define-constant natural-unit-of-time
    ("1.28808866644E-21" "0.00000000040E-21" "3.1E-10")
  "Natural unit of time.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nut>.")

(define-constant natural-unit-of-velocity
    ("299792458" "0" "0")
  "Natural unit of velocity.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?Nc>.")

(define-constant neutron-Compton-wavelength
    ("1.31959090382E-15" "0.00000000067E-15" "5.1E-10")
  "Neutron Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwl>.")

(define-constant neutron-g-factor
    ("-3.82608552" "0.00000090" "2.4E-7")
  "Neutron g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gnn>.")

(define-constant neutron-gyromagnetic-ratio
    ("1.83247174E8" "0.00000043E8" "2.4E-7")
  "Neutron gyromagnetic ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gamman>.")

(define-constant neutron-gyromagnetic-ratio-in-MHz/T
    ("29.1646935" "0.0000069" "2.4E-7")
  "Neutron gyromagnetic ratio in MHz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammanbar>.")

(define-constant neutron-magnetic-moment
    ("-9.6623653E-27" "0.0000023E-27" "2.4E-7")
  "Neutron magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munn>.")

(define-constant neutron-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.04187565E-3" "0.00000025E-3" "2.4E-7")
  "Neutron magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmub>.")

(define-constant neutron-magnetic-moment-to-nuclear-magneton-ratio
    ("-1.91304276" "0.00000045" "2.4E-7")
  "Neutron magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmun>.")

(define-constant neutron-mass
    ("1.67492750056E-27" "0.00000000085E-27" "5.1E-10")
  "Neutron mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mn>.")

(define-constant neutron-mass-energy-equivalent
    ("1.50534976514E-10" "0.00000000076E-10" "5.1E-10")
  "Neutron mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2>.")

(define-constant neutron-mass-energy-equivalent-in-MeV
    ("939.56542194" "0.00000048" "5.1E-10")
  "Neutron mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2mev>.")

(define-constant neutron-mass-in-u
    ("1.00866491606" "0.00000000040" "4.0E-10")
  "Neutron mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnu>.")

(define-constant neutron-molar-mass
    ("1.00866491712E-3" "0.00000000051E-3" "5.1E-10")
  "Neutron molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmn>.")

(define-constant neutron-relative-atomic-mass
    ("1.00866491606" "0.00000000040" "4.0E-10")
  "Neutron relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?arn>.")

(define-constant neutron-to-shielded-proton-magnetic-moment-ratio
    ("-0.68499694" "0.00000016" "2.4E-7")
  "Neutron to shielded proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmupp>.")

(define-constant neutron-electron-magnetic-moment-ratio
    ("1.04066884E-3" "0.00000024E-3" "2.4E-7")
  "Neutron-electron magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmue>.")

(define-constant neutron-electron-mass-ratio
    ("1838.68366200" "0.00000074" "4.0E-10")
  "Neutron-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsme>.")

(define-constant neutron-muon-mass-ratio
    ("8.89248408" "0.00000020" "2.2E-8")
  "Neutron-muon mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmmu>.")

(define-constant neutron-proton-magnetic-moment-ratio
    ("-0.68497935" "0.00000016" "2.4E-7")
  "Neutron-proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmup>.")

(define-constant neutron-proton-mass-difference
    ("2.30557461E-30" "0.00000067E-30" "2.9E-7")
  "Neutron-proton mass difference.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmp>.")

(define-constant neutron-proton-mass-difference-energy-equivalent
    ("2.07214712E-13" "0.00000060E-13" "2.9E-7")
  "Neutron-proton mass difference energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2>.")

(define-constant neutron-proton-mass-difference-energy-equivalent-in-MeV
    ("1.29333251" "0.00000038" "2.9E-7")
  "Neutron-proton mass difference energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2mev>.")

(define-constant neutron-proton-mass-difference-in-u
    ("1.38844948E-3" "0.00000040E-3" "2.9E-7")
  "Neutron-proton mass difference in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpu>.")

(define-constant neutron-proton-mass-ratio
    ("1.00137841946" "0.00000000040" "4.0E-10")
  "Neutron-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmp>.")

(define-constant neutron-tau-mass-ratio
    ("0.528779" "0.000036" "6.8E-5")
  "Neutron-tau mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmtau>.")

(define-constant Newtonian-constant-of-gravitation
    ("6.67430E-11" "0.00015E-11" "2.2E-5")
  "Newtonian constant of gravitation.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bg>.")

(define-constant Newtonian-constant-of-gravitation-over-h-bar-c
    ("6.70883E-39" "0.00015E-39" "2.2E-5")
  "Newtonian constant of gravitation over h-bar c.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bgspu>.")

(define-constant nuclear-magneton
    ("5.0507837393E-27" "0.0000000016E-27" "3.1E-10")
  "Nuclear magneton.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mun>.")

(define-constant nuclear-magneton-in-eV/T
    ("3.15245125417E-8" "0.00000000098E-8" "3.1E-10")
  "Nuclear magneton in eV/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munev>.")

(define-constant nuclear-magneton-in-inverse-meter-per-tesla
    ("2.54262341009E-2" "0.00000000079E-2" "3.1E-10")
  "Nuclear magneton in inverse meter per tesla.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshcminv>.")

(define-constant nuclear-magneton-in-K/T
    ("3.6582677706E-4" "0.0000000011E-4" "3.1E-10")
  "Nuclear magneton in K/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munskk>.")

(define-constant nuclear-magneton-in-MHz/T
    ("7.6225932188" "0.0000000024" "3.1E-10")
  "Nuclear magneton in MHz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshhz>.")

(define-constant Planck-constant
    ("6.62607015E-34" "0" "0")
  "Planck constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?h>.")

(define-constant Planck-constant-in-eV/Hz
    ((with-early-bindings (/ h e)) "0" "0")
  "Planck constant in eV/Hz.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hev>.")

(define-constant Planck-length
    ("1.616255E-35" "0.000018E-35" "1.1E-5")
  "Planck length.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkl>.")

(define-constant Planck-mass
    ("2.176434E-8" "0.000024E-8" "1.1E-5")
  "Planck mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkm>.")

(define-constant Planck-mass-energy-equivalent-in-GeV
    ("1.220890E19" "0.000014E19" "1.1E-5")
  "Planck mass energy equivalent in GeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkmc2gev>.")

(define-constant Planck-temperature
    ("1.416784E32" "0.000016E32" "1.1E-5")
  "Planck temperature.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plktmp>.")

(define-constant Planck-time
    ("5.391247E-44" "0.000060E-44" "1.1E-5")
  "Planck time.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkt>.")

(define-constant proton-charge-to-mass-quotient
    ("9.5788331430E7" "0.0000000030E7" "3.1E-10")
  "Proton charge to mass quotient.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esmp>.")

(define-constant proton-Compton-wavelength
    ("1.32140985360E-15" "0.00000000041E-15" "3.1E-10")
  "Proton Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwl>.")

(define-constant proton-g-factor
    ("5.5856946893" "0.0000000016" "2.9E-10")
  "Proton g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gp>.")

(define-constant proton-gyromagnetic-ratio
    ("2.6752218708E8" "0.0000000011E8" "4.3E-10")
  "Proton gyromagnetic ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammap>.")

(define-constant proton-gyromagnetic-ratio-in-MHz/T
    ("42.577478461" "0.000000018" "4.3E-10")
  "Proton gyromagnetic ratio in MHz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapbar>.")

(define-constant proton-magnetic-moment
    ("1.41060679545E-26" "0.00000000060E-26" "4.3E-10")
  "Proton magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mup>.")

(define-constant proton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.52103220230E-3" "0.00000000045E-3" "3.0E-10")
  "Proton magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmub>.")

(define-constant proton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.79284734463" "0.00000000082" "2.9E-10")
  "Proton magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmun>.")

(define-constant proton-magnetic-shielding-correction
    ("2.56715E-5" "0.00041E-5" "1.6E-4")
  "Proton magnetic shielding correction.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmapp>.")

(define-constant proton-mass
    ("1.67262192595E-27" "0.00000000052E-27" "3.1E-10")
  "Proton mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mp>.")

(define-constant proton-mass-energy-equivalent
    ("1.50327761802E-10" "0.00000000047E-10" "3.1E-10")
  "Proton mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2>.")

(define-constant proton-mass-energy-equivalent-in-MeV
    ("938.27208943" "0.00000029" "3.1E-10")
  "Proton mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2mev>.")

(define-constant proton-mass-in-u
    ("1.0072764665789" "0.0000000000083" "8.3E-12")
  "Proton mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpu>.")

(define-constant proton-molar-mass
    ("1.00727646764E-3" "0.00000000031E-3" "3.1E-10")
  "Proton molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmp>.")

(define-constant proton-relative-atomic-mass
    ("1.0072764665789" "0.0000000000083" "8.3E-12")
  "Proton relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?arp>.")

(define-constant proton-rms-charge-radius
    ("8.4075E-16" "0.0064E-16" "7.6E-4")
  "Proton rms charge radius.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rp>.")

(define-constant proton-electron-mass-ratio
    ("1836.152673426" "0.000000032" "1.7E-11")
  "Proton-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsme>.")

(define-constant proton-muon-mass-ratio
    ("8.88024338" "0.00000020" "2.2E-8")
  "Proton-muon mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmmu>.")

(define-constant proton-neutron-magnetic-moment-ratio
    ("-1.45989802" "0.00000034" "2.4E-7")
  "Proton-neutron magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmunn>.")

(define-constant proton-neutron-mass-ratio
    ("0.99862347797" "0.00000000040" "4.0E-10")
  "Proton-neutron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmn>.")

(define-constant proton-tau-mass-ratio
    ("0.528051" "0.000036" "6.8E-5")
  "Proton-tau mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmtau>.")

(define-constant quantum-of-circulation
    ("3.6369475467E-4" "0.0000000011E-4" "3.1E-10")
  "Quantum of circulation.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?qucirchs2me>.")

(define-constant quantum-of-circulation-times-2
    ("7.2738950934E-4" "0.0000000023E-4" "3.1E-10")
  "Quantum of circulation times 2.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hsme>.")

(define-constant reduced-Compton-wavelength
    ("3.8615926744E-13" "0.0000000012E-13" "3.1E-10")
  "Reduced Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwlbar>.")

(define-constant reduced-muon-Compton-wavelength
    ("1.867594306E-15" "0.000000042E-15" "2.2E-8")
  "Reduced muon Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwlbar>.")

(define-constant reduced-neutron-Compton-wavelength
    ("2.1001941520E-16" "0.0000000011E-16" "5.1E-10")
  "Reduced neutron Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwlbar>.")

(define-constant reduced-Planck-constant
    ((with-early-bindings (/ h 2 pi)) "0" "0")
  "Reduced Planck constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbar>.")

(define-constant reduced-Planck-constant-in-eV-s
    ((with-early-bindings (/ h 2 pi e)) "0" "0")
  "Reduced Planck constant in eV s.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbarev>.")

(define-constant reduced-Planck-constant-times-c-in-MeV-fm
    ((with-early-bindings (* c (/ h 2 pi e) 1000000000)) "0" "0")
  "Reduced Planck constant times c in MeV fm.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbcmevf>.")

(define-constant reduced-proton-Compton-wavelength
    ("2.10308910051E-16" "0.00000000066E-16" "3.1E-10")
  "Reduced proton Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwlbar>.")

(define-constant reduced-tau-Compton-wavelength
    ("1.110538E-16" "0.000075E-16" "6.8E-5")
  "Reduced tau Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwlbar>.")

(define-constant Rydberg-constant
    ("10973731.568157" "0.000012" "1.1E-12")
  "Rydberg constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ryd>.")

(define-constant Rydberg-constant-times-c-in-Hz
    ("3.2898419602500E15" "0.0000000000036E15" "1.1E-12")
  "Rydberg constant times c in Hz.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydchz>.")

(define-constant Rydberg-constant-times-hc-in-eV
    ("13.605693122990" "0.000000000015" "1.1E-12")
  "Rydberg constant times hc in eV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcev>.")

(define-constant Rydberg-constant-times-hc-in-J
    ("2.1798723611030E-18" "0.0000000000024E-18" "1.1E-12")
  "Rydberg constant times hc in J.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcj>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-100000Pa
    ("-1.15170753496" "0.00000000047" "4.1E-10")
  "Sackur-Tetrode constant (1 K, 100000 Pa).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0sr>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-101325Pa
    ("-1.16487052149" "0.00000000047" "4.0E-10")
  "Sackur-Tetrode constant (1 K, 101325 Pa).

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0srstd>.")

(define-constant second-radiation-constant
    ((with-early-bindings (/ (* h c) k)) "0" "0")
  "Second radiation constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c22ndrc>.")

(define-constant shielded-helion-gyromagnetic-ratio
    ("2.0378946078E8" "0.0000000018E8" "8.7E-10")
  "Shielded helion gyromagnetic ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahp>.")

(define-constant shielded-helion-gyromagnetic-ratio-in-MHz/T
    ("32.434100033" "0.000000028" "8.7E-10")
  "Shielded helion gyromagnetic ratio in MHz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahpbar>.")

(define-constant shielded-helion-magnetic-moment
    ("-1.07455311035E-26" "0.00000000093E-26" "8.7E-10")
  "Shielded helion magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhp>.")

(define-constant shielded-helion-magnetic-moment-to-Bohr-magneton-ratio
    ("-1.15867149457E-3" "0.00000000094E-3" "8.1E-10")
  "Shielded helion magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmub>.")

(define-constant shielded-helion-magnetic-moment-to-nuclear-magneton-ratio
    ("-2.1274977624" "0.0000000017" "8.1E-10")
  "Shielded helion magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmun>.")

(define-constant shielded-helion-to-proton-magnetic-moment-ratio
    ("-0.76176657721" "0.00000000066" "8.6E-10")
  "Shielded helion to proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmup>.")

(define-constant shielded-helion-to-shielded-proton-magnetic-moment-ratio
    ("-0.7617861334" "0.0000000031" "4.0E-9")
  "Shielded helion to shielded proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmupp>.")

(define-constant shielded-proton-gyromagnetic-ratio
    ("2.675153194E8" "0.000000011E8" "4.1E-9")
  "Shielded proton gyromagnetic ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapp>.")

(define-constant shielded-proton-gyromagnetic-ratio-in-MHz/T
    ("42.57638543" "0.00000017" "4.1E-9")
  "Shielded proton gyromagnetic ratio in MHz/T.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammappbar>.")

(define-constant shielded-proton-magnetic-moment
    ("1.4105705830E-26" "0.0000000058E-26" "4.1E-9")
  "Shielded proton magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupp>.")

(define-constant shielded-proton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.5209931551E-3" "0.0000000062E-3" "4.1E-9")
  "Shielded proton magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmub>.")

(define-constant shielded-proton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.792775648" "0.000000011" "4.1E-9")
  "Shielded proton magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmun>.")

(define-constant shielding-difference-of-d-and-p-in-HD
    ("1.98770E-8" "0.00010E-8" "5.0E-5")
  "Shielding difference of d and p in HD.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sdp>.")

(define-constant shielding-difference-of-t-and-p-in-HT
    ("2.39450E-8" "0.00020E-8" "8.4E-5")
  "Shielding difference of t and p in HT.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stp>.")

(define-constant speed-of-light-in-vacuum
    ("299792458" "0" "0")
  "Speed of light in vacuum.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c>.")

(define-constant standard-acceleration-of-gravity
    ("9.80665" "0" "0")
  "Standard acceleration of gravity.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gn>.")

(define-constant standard-atmosphere
    ("101325" "0" "0")
  "Standard atmosphere.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdatm>.")

(define-constant standard-state-pressure
    ("100000" "0" "0")
  "Standard-state pressure.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdspr>.")

(define-constant Stefan-Boltzmann-constant
    ((with-early-bindings (/ (* 8 (expt pi 5) (expt k 4)) 60 (expt h 3) (expt c 2))) "0" "0")
  "Stefan-Boltzmann constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigma>.")

(define-constant tau-Compton-wavelength
    ("6.97771E-16" "0.00047E-16" "6.8E-5")
  "Tau Compton wavelength.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwl>.")

(define-constant tau-energy-equivalent
    ("1776.86" "0.12" "6.8E-5")
  "Tau energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2mev>.")

(define-constant tau-mass
    ("3.16754E-27" "0.00021E-27" "6.8E-5")
  "Tau mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtau>.")

(define-constant tau-mass-energy-equivalent
    ("2.84684E-10" "0.00019E-10" "6.8E-5")
  "Tau mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2>.")

(define-constant tau-mass-in-u
    ("1.90754" "0.00013" "6.8E-5")
  "Tau mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauu>.")

(define-constant tau-molar-mass
    ("1.90754E-3" "0.00013E-3" "6.8E-5")
  "Tau molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmtau>.")

(define-constant tau-electron-mass-ratio
    ("3477.23" "0.23" "6.8E-5")
  "Tau-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausme>.")

(define-constant tau-muon-mass-ratio
    ("16.8170" "0.0011" "6.8E-5")
  "Tau-muon mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmmu>.")

(define-constant tau-neutron-mass-ratio
    ("1.89115" "0.00013" "6.8E-5")
  "Tau-neutron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmn>.")

(define-constant tau-proton-mass-ratio
    ("1.89376" "0.00013" "6.8E-5")
  "Tau-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmp>.")

(define-constant Thomson-cross-section
    ("6.6524587051E-29" "0.0000000062E-29" "9.3E-10")
  "Thomson cross section.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmae>.")

(define-constant triton-g-factor
    ("5.957924930" "0.000000012" "2.0E-9")
  "Triton g factor.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gtn>.")

(define-constant triton-magnetic-moment
    ("1.5046095178E-26" "0.0000000030E-26" "2.0E-9")
  "Triton magnetic moment.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mut>.")

(define-constant triton-magnetic-moment-to-Bohr-magneton-ratio
    ("1.6223936648E-3" "0.0000000032E-3" "2.0E-9")
  "Triton magnetic moment to Bohr magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmub>.")

(define-constant triton-magnetic-moment-to-nuclear-magneton-ratio
    ("2.9789624650" "0.0000000059" "2.0E-9")
  "Triton magnetic moment to nuclear magneton ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmun>.")

(define-constant triton-mass
    ("5.0073567512E-27" "0.0000000016E-27" "3.1E-10")
  "Triton mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mt>.")

(define-constant triton-mass-energy-equivalent
    ("4.5003878119E-10" "0.0000000014E-10" "3.1E-10")
  "Triton mass energy equivalent.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2>.")

(define-constant triton-mass-energy-equivalent-in-MeV
    ("2808.92113668" "0.00000088" "3.1E-10")
  "Triton mass energy equivalent in MeV.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2mev>.")

(define-constant triton-mass-in-u
    ("3.01550071597" "0.00000000010" "3.4E-11")
  "Triton mass in u.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtu>.")

(define-constant triton-molar-mass
    ("3.01550071913E-3" "0.00000000094E-3" "3.1E-10")
  "Triton molar mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmt>.")

(define-constant triton-relative-atomic-mass
    ("3.01550071597" "0.00000000010" "3.4E-11")
  "Triton relative atomic mass.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?art>.")

(define-constant triton-to-proton-magnetic-moment-ratio
    ("1.0666399189" "0.0000000021" "2.0E-9")
  "Triton to proton magnetic moment ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmup>.")

(define-constant triton-electron-mass-ratio
    ("5496.92153551" "0.00000021" "3.8E-11")
  "Triton-electron mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsme>.")

(define-constant triton-proton-mass-ratio
    ("2.99371703403" "0.00000000010" "3.4E-11")
  "Triton-proton mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsmp>.")

(define-constant unified-atomic-mass-unit
    ("1.66053906892E-27" "0.00000000052E-27" "3.1E-10")
  "Unified atomic mass unit.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ukg>.")

(define-constant vacuum-electric-permittivity
    ("8.8541878188E-12" "0.0000000014E-12" "1.6E-10")
  "Vacuum electric permittivity.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ep0>.")

(define-constant vacuum-magnetic-permeability
    ("1.25663706127E-6" "0.00000000020E-6" "1.6E-10")
  "Vacuum magnetic permeability.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu0>.")

(define-constant von-Klitzing-constant
    ((with-early-bindings (/ h (expt e 2))) "0" "0")
  "Von Klitzing constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk>.")

(define-constant W-to-Z-mass-ratio
    ("0.88145" "0.00013" "1.5E-4")
  "W to Z mass ratio.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rmwmz>.")

(define-constant weak-mixing-angle
    ("0.22305" "0.00023" "1.0E-3")
  "Weak mixing angle.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sin2th>.")

(define-constant Wien-frequency-displacement-law-constant
    ((with-early-bindings (let ((x
                                 (newton
                                  (lambda (x)
                                    (let ((a (- x 3)) (b (- x 2)))
                                      (+ (/ a b) (/ 3 (* b (exp x))))))
                                  (read-from-string "2.8214393721220787L0"))))
                            (/ (* x k) h))) "0" "0")
  "Wien frequency displacement law constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bpwien>.")

(define-constant Wien-wavelength-displacement-law-constant
    ((with-early-bindings (let ((x
                                 (newton
                                  (lambda (x)
                                    (let ((a (- x 5)) (b (- x 4)))
                                      (+ (/ a b) (/ 5 (* b (exp x))))))
                                  (read-from-string "4.965114231744276L0"))))
                            (/ (* h c) x k))) "0" "0")
  "Wien wavelength displacement law constant.

2022 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bwien>.")

;;;; Backward Compatibility

(export '220-lattice-spacing-of-silicon)
(define-symbol-macro 220-lattice-spacing-of-silicon
  lattice-spacing-of-ideal-silicon-at-220)

(export 'bohr-magneton-in-inverse-meters-per-tesla)
(define-symbol-macro bohr-magneton-in-inverse-meters-per-tesla
  bohr-magneton-in-inverse-meter-per-tesla)

(export 'boltzmann-constant-in-inverse-meters-per-kelvin)
(define-symbol-macro boltzmann-constant-in-inverse-meters-per-kelvin
  boltzmann-constant-in-inverse-meter-per-kelvin)

(export 'cu-x-unit)
(define-symbol-macro cu-x-unit
  copper-x-unit)

(export 'mo-x-unit)
(define-symbol-macro mo-x-unit
  molybdenum-x-unit)

(export 'electric-constant)
(define-symbol-macro electric-constant
  vacuum-electric-permittivity)

(export 'magnetic-constant)
(define-symbol-macro magnetic-constant
  vacuum-magnetic-permeability)

;;; codata-2022.lisp ends here
