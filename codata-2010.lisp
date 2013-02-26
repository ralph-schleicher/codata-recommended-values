;; codata-2010.lisp --- 2010 CODATA recommended values.

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

;;; Commentary:

;; Automatically generated, do not edit!

;;; Code:

(in-package :common-lisp-user)

(defpackage :codata-recommended-values-2010
  (:use :common-lisp :codata-recommended-values-common))

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
    (6.64465675L-27 0.00000029L-27 4.4L-8)
  "Alpha particle mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mal>.")

(define-constant alpha-particle-mass-energy-equivalent
    (5.97191967L-10 0.00000026L-10 4.4L-8)
  "Alpha particle mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2>.")

(define-constant alpha-particle-mass-energy-equivalent-in-MeV
    (3727.379240L0 0.000082L0 2.2L-8)
  "Alpha particle mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malc2mev>.")

(define-constant alpha-particle-mass-in-u
    (4.001506179125L0 0.000000000062L0 1.5L-11)
  "Alpha particle mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malu>.")

(define-constant alpha-particle-molar-mass
    (4.001506179125L-3 0.000000000062L-3 1.5L-11)
  "Alpha particle molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmal>.")

(define-constant alpha-particle-electron-mass-ratio
    (7294.2995361L0 0.0000029L0 4.0L-10)
  "Alpha particle-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsme>.")

(define-constant alpha-particle-proton-mass-ratio
    (3.97259968933L0 0.00000000036L0 9.0L-11)
  "Alpha particle-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?malsmp>.")

(define-constant Angstrom-star
    (1.00001495L-10 0.00000090L-10 9.0L-7)
  "Angstrom star.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?angstar>.")

(define-constant atomic-mass-constant
    (1.660538921L-27 0.000000073L-27 4.4L-8)
  "Atomic mass constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?u>.")

(define-constant atomic-mass-constant-energy-equivalent
    (1.492417954L-10 0.000000066L-10 4.4L-8)
  "Atomic mass constant energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tuj>.")

(define-constant atomic-mass-constant-energy-equivalent-in-MeV
    (931.494061L0 0.000021L0 2.2L-8)
  "Atomic mass constant energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muc2mev>.")

(define-constant atomic-mass-unit-electron-volt-relationship
    (931.494061L6 0.000021L6 2.2L-8)
  "Atomic mass unit-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uev>.")

(define-constant atomic-mass-unit-hartree-relationship
    (3.4231776845L7 0.0000000024L7 7.0L-10)
  "Atomic mass unit-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhr>.")

(define-constant atomic-mass-unit-hertz-relationship
    (2.2523427168L23 0.0000000016L23 7.0L-10)
  "Atomic mass unit-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uhz>.")

(define-constant atomic-mass-unit-inverse-meter-relationship
    (7.5130066042L14 0.0000000053L14 7.0L-10)
  "Atomic mass unit-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uminv>.")

(define-constant atomic-mass-unit-joule-relationship
    (1.492417954L-10 0.000000066L-10 4.4L-8)
  "Atomic mass unit-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uj>.")

(define-constant atomic-mass-unit-kelvin-relationship
    (1.08095408L13 0.00000098L13 9.1L-7)
  "Atomic mass unit-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?uk>.")

(define-constant atomic-mass-unit-kilogram-relationship
    (1.660538921L-27 0.000000073L-27 4.4L-8)
  "Atomic mass unit-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ukg>.")

(define-constant atomic-unit-of-first-hyperpolarizability
    (3.206361449L-53 0.000000071L-53 2.2L-8)
  "Atomic unit of first hyperpolarizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auhypol>.")

(define-constant atomic-unit-of-second-hyperpolarizability
    (6.23538054L-65 0.00000028L-65 4.4L-8)
  "Atomic unit of second hyperpolarizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?au2hypol>.")

(define-constant atomic-unit-of-action
    (1.054571726L-34 0.000000047L-34 4.4L-8)
  "Atomic unit of action.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tthbar>.")

(define-constant atomic-unit-of-charge
    (1.602176565L-19 0.000000035L-19 2.2L-8)
  "Atomic unit of charge.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?te>.")

(define-constant atomic-unit-of-charge-density
    (1.081202338L12 0.000000024L12 2.2L-8)
  "Atomic unit of charge density.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucd>.")

(define-constant atomic-unit-of-current
    (6.62361795L-3 0.00000015L-3 2.2L-8)
  "Atomic unit of current.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aucur>.")

(define-constant atomic-unit-of-electric-dipole-moment
    (8.47835326L-30 0.00000019L-30 2.2L-8)
  "Atomic unit of electric dipole moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auedm>.")

(define-constant atomic-unit-of-electric-field
    (5.14220652L11 0.00000011L11 2.2L-8)
  "Atomic unit of electric field.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefld>.")

(define-constant atomic-unit-of-electric-field-gradient
    (9.71736200L21 0.00000021L21 2.2L-8)
  "Atomic unit of electric field gradient.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auefg>.")

(define-constant atomic-unit-of-electric-polarizability
    (1.6487772754L-41 0.0000000016L-41 9.7L-10)
  "Atomic unit of electric polarizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auepol>.")

(define-constant atomic-unit-of-electric-potential
    (27.21138505L0 0.00000060L0 2.2L-8)
  "Atomic unit of electric potential.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auep>.")

(define-constant atomic-unit-of-electric-quadrupole-moment
    (4.486551331L-40 0.000000099L-40 2.2L-8)
  "Atomic unit of electric quadrupole moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aueqm>.")

(define-constant atomic-unit-of-energy
    (4.35974434L-18 0.00000019L-18 4.4L-8)
  "Atomic unit of energy.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thr>.")

(define-constant atomic-unit-of-force
    (8.23872278L-8 0.00000036L-8 4.4L-8)
  "Atomic unit of force.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auforce>.")

(define-constant atomic-unit-of-length
    (0.52917721092L-10 0.00000000017L-10 3.2L-10)
  "Atomic unit of length.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tbohrrada0>.")

(define-constant atomic-unit-of-magnetic-dipole-moment
    (1.854801936L-23 0.000000041L-23 2.2L-8)
  "Atomic unit of magnetic dipole moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumdm>.")

(define-constant atomic-unit-of-magnetic-flux-density
    (2.350517464L5 0.000000052L5 2.2L-8)
  "Atomic unit of magnetic flux density.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumfd>.")

(define-constant atomic-unit-of-magnetizability
    (7.891036607L-29 0.000000013L-29 1.6L-9)
  "Atomic unit of magnetizability.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumag>.")

(define-constant atomic-unit-of-mass
    (9.10938291L-31 0.00000040L-31 4.4L-8)
  "Atomic unit of mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ttme>.")

(define-constant atomic-unit-of-momentum
    (1.992851740L-24 0.000000088L-24 4.4L-8)
  "Atomic unit of momentum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aumom>.")

(define-constant atomic-unit-of-permittivity
    ((with-early-bindings (/ (expt e 2) a Eh)) 0 0)
  "Atomic unit of permittivity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auperm>.")

(define-constant atomic-unit-of-time
    (2.418884326502L-17 0.000000000012L-17 5.0L-12)
  "Atomic unit of time.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?aut>.")

(define-constant atomic-unit-of-velocity
    (2.18769126379L6 0.00000000071L6 3.2L-10)
  "Atomic unit of velocity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?auvel>.")

(define-constant Avogadro-constant
    (6.02214129L23 0.00000027L23 4.4L-8)
  "Avogadro constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?na>.")

(define-constant Bohr-magneton
    (927.400968L-26 0.000020L-26 2.2L-8)
  "Bohr magneton.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mub>.")

(define-constant Bohr-magneton-in-eV/T
    (5.7883818066L-5 0.0000000038L-5 6.5L-10)
  "Bohr magneton in eV/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubev>.")

(define-constant Bohr-magneton-in-Hz/T
    (13.99624555L9 0.00000031L9 2.2L-8)
  "Bohr magneton in Hz/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshhz>.")

(define-constant Bohr-magneton-in-inverse-meters-per-tesla
    (46.6864498L0 0.0000010L0 2.2L-8)
  "Bohr magneton in inverse meters per tesla.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubshcminv>.")

(define-constant Bohr-magneton-inK/T
    (0.67171388L0 0.00000061L0 9.1L-7)
  "Bohr magneton in K/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mubskk>.")

(define-constant Bohr-radius
    (0.52917721092L-10 0.00000000017L-10 3.2L-10)
  "Bohr radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bohrrada0>.")

(define-constant Boltzmann-constant
    (1.3806488L-23 0.0000013L-23 9.1L-7)
  "Boltzmann constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?k>.")

(define-constant Boltzmann-constant-in-eV/K
    (8.6173324L-5 0.0000078L-5 9.1L-7)
  "Boltzmann constant in eV/K.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tkev>.")

(define-constant Boltzmann-constant-in-Hz/K
    (2.0836618L10 0.0000019L10 9.1L-7)
  "Boltzmann constant in Hz/K.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshhz>.")

(define-constant Boltzmann-constant-in-inverse-meters-per-kelvin
    (69.503476L0 0.000063L0 9.1L-7)
  "Boltzmann constant in inverse meters per kelvin.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kshcminv>.")

(define-constant characteristic-impedance-of-vacuum
    ((with-early-bindings (* mu c)) 0 0)
  "Characteristic impedance of vacuum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?z0>.")

(define-constant classical-electron-radius
    (2.8179403267L-15 0.0000000027L-15 9.7L-10)
  "Classical electron radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?re>.")

(define-constant Compton-wavelength
    (2.4263102389L-12 0.0000000016L-12 6.5L-10)
  "Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwl>.")

(define-constant Compton-wavelength-over-2-pi
    (386.15926800L-15 0.00000025L-15 6.5L-10)
  "Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ecomwlbar>.")

(define-constant conductance-quantum
    (7.7480917346L-5 0.0000000025L-5 3.2L-10)
  "Conductance quantum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?conqu2e2sh>.")

(define-constant conventional-value-of-Josephson-constant
    (483597.9L9 0 0)
  "Conventional value of Josephson constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj90>.")

(define-constant conventional-value-of-von-Klitzing-constant
    (25812.807L0 0 0)
  "Conventional value of von Klitzing constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk90>.")

(define-constant Cu-x-unit
    (1.00207697L-13 0.00000028L-13 2.8L-7)
  "Cu x unit.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xucukalph1>.")

(define-constant deuteron-g-factor
    (0.8574382308L0 0.0000000072L0 8.4L-9)
  "Deuteron g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gdn>.")

(define-constant deuteron-magnetic-moment
    (0.433073489L-26 0.000000010L-26 2.4L-8)
  "Deuteron magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mud>.")

(define-constant deuteron-magnetic-moment-to-Bohr-magneton-ratio
    (0.4669754556L-3 0.0000000039L-3 8.4L-9)
  "Deuteron magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmub>.")

(define-constant deuteron-magnetic-moment-to-nuclear-magneton-ratio
    (0.8574382308L0 0.0000000072L0 8.4L-9)
  "Deuteron magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmun>.")

(define-constant deuteron-mass
    (3.34358348L-27 0.00000015L-27 4.4L-8)
  "Deuteron mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?md>.")

(define-constant deuteron-mass-energy-equivalent
    (3.00506297L-10 0.00000013L-10 4.4L-8)
  "Deuteron mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2>.")

(define-constant deuteron-mass-energy-equivalent-in-MeV
    (1875.612859L0 0.000041L0 2.2L-8)
  "Deuteron mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdc2mev>.")

(define-constant deuteron-mass-in-u
    (2.013553212712L0 0.000000000077L0 3.8L-11)
  "Deuteron mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdu>.")

(define-constant deuteron-molar-mass
    (2.013553212712L-3 0.000000000077L-3 3.8L-11)
  "Deuteron molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmd>.")

(define-constant deuteron-rms-charge-radius
    (2.1424L-15 0.0021L-15 9.8L-4)
  "Deuteron rms charge radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rd>.")

(define-constant deuteron-electron-magnetic-moment-ratio
    (-4.664345537L-4 0.000000039L-4 8.4L-9)
  "Deuteron-electron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmuem>.")

(define-constant deuteron-electron-mass-ratio
    (3670.4829652L0 0.0000015L0 4.0L-10)
  "Deuteron-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsme>.")

(define-constant deuteron-neutron-magnetic-moment-ratio
    (-0.44820652L0 0.00000011L0 2.4L-7)
  "Deuteron-neutron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmunn>.")

(define-constant deuteron-proton-magnetic-moment-ratio
    (0.3070122070L0 0.0000000024L0 7.7L-9)
  "Deuteron-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mudsmup>.")

(define-constant deuteron-proton-mass-ratio
    (1.99900750097L0 0.00000000018L0 9.2L-11)
  "Deuteron-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mdsmp>.")

(define-constant electric-constant
    ((with-early-bindings (/ (* mu (expt c 2)))) 0 0)
  "Electric constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ep0>.")

(define-constant electron-charge-to-mass-quotient
    (-1.758820088L11 0.000000039L11 2.2L-8)
  "Electron charge to mass quotient.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esme>.")

(define-constant electron-g-factor
    (-2.00231930436153L0 0.00000000000053L0 2.6L-13)
  "Electron g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gem>.")

(define-constant electron-gyromagnetic-ratio
    (1.760859708L11 0.000000039L11 2.2L-8)
  "Electron gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammae>.")

(define-constant electron-gyromagnetic-ratio-over-2-pi
    (28024.95266L0 0.00062L0 2.2L-8)
  "Electron gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammaebar>.")

(define-constant electron-magnetic-moment
    (-928.476430L-26 0.000021L-26 2.2L-8)
  "Electron magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muem>.")

(define-constant electron-magnetic-moment-anomaly
    (1.15965218076L-3 0.00000000027L-3 2.3L-10)
  "Electron magnetic moment anomaly.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ae>.")

(define-constant electron-magnetic-moment-to-Bohr-magneton-ratio
    (-1.00115965218076L0 0.00000000000027L0 2.6L-13)
  "Electron magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmub>.")

(define-constant electron-magnetic-moment-to-nuclear-magneton-ratio
    (-1838.28197090L0 0.00000075L0 4.1L-10)
  "Electron magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmun>.")

(define-constant electron-mass
    (9.10938291L-31 0.00000040L-31 4.4L-8)
  "Electron mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?me>.")

(define-constant electron-mass-energy-equivalent
    (8.18710506L-14 0.00000036L-14 4.4L-8)
  "Electron mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2>.")

(define-constant electron-mass-energy-equivalent-in-MeV
    (0.510998928L0 0.000000011L0 2.2L-8)
  "Electron mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec2mev>.")

(define-constant electron-mass-in-u
    (5.4857990946L-4 0.0000000022L-4 4.0L-10)
  "Electron mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?meu>.")

(define-constant electron-molar-mass
    (5.4857990946L-7 0.0000000022L-7 4.0L-10)
  "Electron molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mme>.")

(define-constant electron-to-alpha-particle-mass-ratio
    (1.37093355578L-4 0.00000000055L-4 4.0L-10)
  "Electron to alpha particle mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmalpha>.")

(define-constant electron-to-shielded-helion-magnetic-moment-ratio
    (864.058257L0 0.000010L0 1.2L-8)
  "Electron to shielded helion magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmuhp>.")

(define-constant electron-to-shielded-proton-magnetic-moment-ratio
    (-658.2275971L0 0.0000072L0 1.1L-8)
  "Electron to shielded proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmupp>.")

(define-constant electron-volt
    (1.602176565L-19 0.000000035L-19 2.2L-8)
  "Electron volt.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tevj>.")

(define-constant electron-volt-atomic-mass-unit-relationship
    (1.073544150L-9 0.000000024L-9 2.2L-8)
  "Electron volt-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evu>.")

(define-constant electron-volt-hartree-relationship
    (3.674932379L-2 0.000000081L-2 2.2L-8)
  "Electron volt-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhr>.")

(define-constant electron-volt-hertz-relationship
    (2.417989348L14 0.000000053L14 2.2L-8)
  "Electron volt-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evhz>.")

(define-constant electron-volt-inverse-meter-relationship
    (8.06554429L5 0.00000018L5 2.2L-8)
  "Electron volt-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evminv>.")

(define-constant electron-volt-joule-relationship
    (1.602176565L-19 0.000000035L-19 2.2L-8)
  "Electron volt-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evj>.")

(define-constant electron-volt-kelvin-relationship
    (1.1604519L4 0.0000011L4 9.1L-7)
  "Electron volt-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evk>.")

(define-constant electron-volt-kilogram-relationship
    (1.782661845L-36 0.000000039L-36 2.2L-8)
  "Electron volt-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?evkg>.")

(define-constant electron-deuteron-magnetic-moment-ratio
    (-2143.923498L0 0.000018L0 8.4L-9)
  "Electron-deuteron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmud>.")

(define-constant electron-deuteron-mass-ratio
    (2.7244371095L-4 0.0000000011L-4 4.0L-10)
  "Electron-deuteron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmd>.")

(define-constant electron-helion-mass-ratio
    (1.8195430761L-4 0.0000000017L-4 9.2L-10)
  "Electron-helion mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmh>.")

(define-constant electron-muon-magnetic-moment-ratio
    (206.7669896L0 0.0000052L0 2.5L-8)
  "Electron-muon magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmumum>.")

(define-constant electron-muon-mass-ratio
    (4.83633166L-3 0.00000012L-3 2.5L-8)
  "Electron-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmmu>.")

(define-constant electron-neutron-magnetic-moment-ratio
    (960.92050L0 0.00023L0 2.4L-7)
  "Electron-neutron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmunn>.")

(define-constant electron-neutron-mass-ratio
    (5.4386734461L-4 0.0000000032L-4 5.8L-10)
  "Electron-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmn>.")

(define-constant electron-proton-magnetic-moment-ratio
    (-658.2106848L0 0.0000054L0 8.1L-9)
  "Electron-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muemsmup>.")

(define-constant electron-proton-mass-ratio
    (5.4461702178L-4 0.0000000022L-4 4.1L-10)
  "Electron-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmp>.")

(define-constant electron-tau-mass-ratio
    (2.87592L-4 0.00026L-4 9.0L-5)
  "Electron-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmtau>.")

(define-constant electron-triton-mass-ratio
    (1.8192000653L-4 0.0000000017L-4 9.1L-10)
  "Electron-triton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mesmt>.")

(define-constant elementary-charge
    (1.602176565L-19 0.000000035L-19 2.2L-8)
  "Elementary charge.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?e>.")

(define-constant elementary-charge-over-h
    (2.417989348L14 0.000000053L14 2.2L-8)
  "Elementary charge over h.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esh>.")

(define-constant Faraday-constant
    (96485.3365L0 0.0021L0 2.2L-8)
  "Faraday constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f>.")

(define-constant Faraday-constant-for-conventional-electric-current
    (96485.3321L0 0.0043L0 4.4L-8)
  "Faraday constant for conventional electric current.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?f90>.")

(define-constant Fermi-coupling-constant
    (1.166364L-5 0.000005L-5 4.3L-6)
  "Fermi coupling constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gf>.")

(define-constant fine-structure-constant
    (7.2973525698L-3 0.0000000024L-3 3.2L-10)
  "Fine-structure constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alph>.")

(define-constant first-radiation-constant
    (3.74177153L-16 0.00000017L-16 4.4L-8)
  "First radiation constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c11strc>.")

(define-constant first-radiation-constant-for-spectral-radiance
    (1.191042869L-16 0.000000053L-16 4.4L-8)
  "First radiation constant for spectral radiance.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c1l>.")

(define-constant Hartree-energy
    (4.35974434L-18 0.00000019L-18 4.4L-8)
  "Hartree energy.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hr>.")

(define-constant Hartree-energy-in-eV
    (27.21138505L0 0.00000060L0 2.2L-8)
  "Hartree energy in eV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?threv>.")

(define-constant hartree-atomic-mass-unit-relationship
    (2.9212623246L-8 0.0000000021L-8 7.0L-10)
  "Hartree-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hru>.")

(define-constant hartree-electron-volt-relationship
    (27.21138505L0 0.00000060L0 2.2L-8)
  "Hartree-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrev>.")

(define-constant hartree-hertz-relationship
    (6.579683920729L15 0.000000000033L15 5.0L-12)
  "Hartree-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrhz>.")

(define-constant hartree-inverse-meter-relationship
    (2.194746313708L7 0.000000000011L7 5.0L-12)
  "Hartree-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrminv>.")

(define-constant hartree-joule-relationship
    (4.35974434L-18 0 0)
  "Hartree-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrj>.")

(define-constant hartree-kelvin-relationship
    (3.1577504L5 0.0000029L5 9.1L-7)
  "Hartree-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrk>.")

(define-constant hartree-kilogram-relationship
    (4.85086979L-35 0.00000021L-35 4.4L-8)
  "Hartree-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hrkg>.")

(define-constant helion-g-factor
    (-4.255250613L0 0.000000050L0 1.2L-8)
  "Helion g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ghn>.")

(define-constant helion-magnetic-moment
    (-1.074617486L-26 0.000000027L-26 2.5L-8)
  "Helion magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muh>.")

(define-constant helion-magnetic-moment-to-Bohr-magneton-ratio
    (-1.158740958L-3 0.000000014L-3 1.2L-8)
  "Helion magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmub>.")

(define-constant helion-magnetic-moment-to-nuclear-magneton-ratio
    (-2.127625306L0 0.000000025L0 1.2L-8)
  "Helion magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhsmun>.")

(define-constant helion-mass
    (5.00641234L-27 0.00000022L-27 4.4L-8)
  "Helion mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mh>.")

(define-constant helion-mass-energy-equivalent
    (4.49953902L-10 0.00000020L-10 4.4L-8)
  "Helion mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2>.")

(define-constant helion-mass-energy-equivalent-in-MeV
    (2808.391482L0 0.000062L0 2.2L-8)
  "Helion mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhc2mev>.")

(define-constant helion-mass-in-u
    (3.0149322468L0 0.0000000025L0 8.3L-10)
  "Helion mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhu>.")

(define-constant helion-molar-mass
    (3.0149322468L-3 0.0000000025L-3 8.3L-10)
  "Helion molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmh>.")

(define-constant helion-electron-mass-ratio
    (5495.8852754L0 0.0000050L0 9.2L-10)
  "Helion-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsme>.")

(define-constant helion-proton-mass-ratio
    (2.9931526707L0 0.0000000025L0 8.2L-10)
  "Helion-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mhsmp>.")

(define-constant hertz-atomic-mass-unit-relationship
    (4.4398216689L-24 0.0000000031L-24 7.0L-10)
  "Hertz-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzu>.")

(define-constant hertz-electron-volt-relationship
    (4.135667516L-15 0.000000091L-15 2.2L-8)
  "Hertz-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzev>.")

(define-constant hertz-hartree-relationship
    (1.5198298460045L-16 0.0000000000076L-16 5.0L-12)
  "Hertz-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzhr>.")

(define-constant hertz-inverse-meter-relationship
    ((with-early-bindings (/ c)) 0 0)
  "Hertz-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzminv>.")

(define-constant hertz-joule-relationship
    (6.62606957L-34 0.00000029L-34 4.4L-8)
  "Hertz-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzj>.")

(define-constant hertz-kelvin-relationship
    (4.7992434L-11 0.0000044L-11 9.1L-7)
  "Hertz-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzk>.")

(define-constant hertz-kilogram-relationship
    (7.37249668L-51 0.00000033L-51 4.4L-8)
  "Hertz-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hzkg>.")

(define-constant inverse-fine-structure-constant
    (137.035999074L0 0.000000044L0 3.2L-10)
  "Inverse fine-structure constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?alphinv>.")

(define-constant inverse-meter-atomic-mass-unit-relationship
    (1.33102505120L-15 0.00000000094L-15 7.0L-10)
  "Inverse meter-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvu>.")

(define-constant inverse-meter-electron-volt-relationship
    (1.239841930L-6 0.000000027L-6 2.2L-8)
  "Inverse meter-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvev>.")

(define-constant inverse-meter-hartree-relationship
    (4.556335252755L-8 0.000000000023L-8 5.0L-12)
  "Inverse meter-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhr>.")

(define-constant inverse-meter-hertz-relationship
    (299792458 0 0)
  "Inverse meter-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvhz>.")

(define-constant inverse-meter-joule-relationship
    (1.986445684L-25 0.000000088L-25 4.4L-8)
  "Inverse meter-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvj>.")

(define-constant inverse-meter-kelvin-relationship
    (1.4387770L-2 0.0000013L-2 9.1L-7)
  "Inverse meter-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvk>.")

(define-constant inverse-meter-kilogram-relationship
    (2.210218902L-42 0.000000098L-42 4.4L-8)
  "Inverse meter-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?minvkg>.")

(define-constant inverse-of-conductance-quantum
    (12906.4037217L0 0.0000042L0 3.2L-10)
  "Inverse of conductance quantum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?invconqu>.")

(define-constant Josephson-constant
    (483597.870L9 0.011L9 2.2L-8)
  "Josephson constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kjos>.")

(define-constant joule-atomic-mass-unit-relationship
    (6.70053585L9 0.00000030L9 4.4L-8)
  "Joule-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ju>.")

(define-constant joule-electron-volt-relationship
    (6.24150934L18 0.00000014L18 2.2L-8)
  "Joule-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jev>.")

(define-constant joule-hartree-relationship
    (2.29371248L17 0.00000010L17 4.4L-8)
  "Joule-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhr>.")

(define-constant joule-hertz-relationship
    (1.509190311L33 0.000000067L33 4.4L-8)
  "Joule-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jhz>.")

(define-constant joule-inverse-meter-relationship
    (5.03411701L24 0.00000022L24 4.4L-8)
  "Joule-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jminv>.")

(define-constant joule-kelvin-relationship
    (7.2429716L22 0.0000066L22 9.1L-7)
  "Joule-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jk>.")

(define-constant joule-kilogram-relationship
    ((with-early-bindings (/ (expt c 2))) 0 0)
  "Joule-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?jkg>.")

(define-constant kelvin-atomic-mass-unit-relationship
    (9.2510868L-14 0.0000084L-14 9.1L-7)
  "Kelvin-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ku>.")

(define-constant kelvin-electron-volt-relationship
    (8.6173324L-5 0.0000078L-5 9.1L-7)
  "Kelvin-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kev>.")

(define-constant kelvin-hartree-relationship
    (3.1668114L-6 0.0000029L-6 9.1L-7)
  "Kelvin-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khr>.")

(define-constant kelvin-hertz-relationship
    (2.0836618L10 0.0000019L10 9.1L-7)
  "Kelvin-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?khz>.")

(define-constant kelvin-inverse-meter-relationship
    (69.503476L0 0.000063L0 9.1L-7)
  "Kelvin-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kminv>.")

(define-constant kelvin-joule-relationship
    (1.3806488L-23 0.0000013L-23 9.1L-7)
  "Kelvin-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kj>.")

(define-constant kelvin-kilogram-relationship
    (1.5361790L-40 0.0000014L-40 9.1L-7)
  "Kelvin-kilogram relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kkg>.")

(define-constant kilogram-atomic-mass-unit-relationship
    (6.02214129L26 0.00000027L26 4.4L-8)
  "Kilogram-atomic mass unit relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgu>.")

(define-constant kilogram-electron-volt-relationship
    (5.60958885L35 0.00000012L35 2.2L-8)
  "Kilogram-electron volt relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgev>.")

(define-constant kilogram-hartree-relationship
    (2.061485968L34 0.000000091L34 4.4L-8)
  "Kilogram-hartree relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghr>.")

(define-constant kilogram-hertz-relationship
    (1.356392608L50 0.000000060L50 4.4L-8)
  "Kilogram-hertz relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kghz>.")

(define-constant kilogram-inverse-meter-relationship
    (4.52443873L41 0.00000020L41 4.4L-8)
  "Kilogram-inverse meter relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgminv>.")

(define-constant kilogram-joule-relationship
    ((with-early-bindings (expt c 2)) 0 0)
  "Kilogram-joule relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgj>.")

(define-constant kilogram-kelvin-relationship
    (6.5096582L39 0.0000059L39 9.1L-7)
  "Kilogram-kelvin relationship.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?kgk>.")

(define-constant lattice-parameter-of-silicon
    (543.1020504L-12 0.0000089L-12 1.6L-8)
  "Lattice parameter of silicon.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?asil>.")

(define-constant Loschmidt-constant-at-273.15K-and-100000Pa
    (2.6516462L25 0.0000024L25 9.1L-7)
  "Loschmidt constant (273.15 K, 100000 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0>.")

(define-constant Loschmidt-constant-at-273.15K-and-101325Pa
    (2.6867805L25 0.0000024L25 9.1L-7)
  "Loschmidt constant (273.15 K, 101325 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?n0std>.")

(define-constant magnetic-constant
    ((with-early-bindings mu) 0 0)
  "Magnetic constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu0>.")

(define-constant magnetic-flux-quantum
    (2.067833758L-15 0.000000046L-15 2.2L-8)
  "Magnetic flux quantum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?flxquhs2e>.")

(define-constant Mo-x-unit
    (1.00209952L-13 0.00000053L-13 5.3L-7)
  "Mo x unit.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?xumokalph1>.")

(define-constant molar-gas-constant
    (8.3144621L0 0.0000075L0 9.1L-7)
  "Molar gas constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?r>.")

(define-constant molar-mass-constant
    (1L-3 0 0)
  "Molar mass constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mu>.")

(define-constant molar-mass-of-carbon-12
    (12L-3 0 0)
  "Molar mass of carbon-12.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mm12c>.")

(define-constant molar-Planck-constant
    (3.9903127176L-10 0.0000000028L-10 7.0L-10)
  "Molar Planck constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nah>.")

(define-constant molar-Planck-constant-times-c
    (0.119626565779L0 0.000000000084L0 7.0L-10)
  "Molar Planck constant times c.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nahc>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-100000Pa
    (22.710953L-3 0.000021L-3 9.1L-7)
  "Molar volume of ideal gas (273.15 K, 100000 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvol>.")

(define-constant molar-volume-of-ideal-gas-at-273.15K-and-101325Pa
    (22.413968L-3 0.000020L-3 9.1L-7)
  "Molar volume of ideal gas (273.15 K, 101325 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolstd>.")

(define-constant molar-volume-of-silicon
    (12.05883301L-6 0.00000080L-6 6.6L-8)
  "Molar volume of silicon.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mvolsil>.")

(define-constant muon-Compton-wavelength
    (11.73444103L-15 0.00000030L-15 2.5L-8)
  "Muon Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwl>.")

(define-constant muon-Compton-wavelength-over-2-pi
    (1.867594294L-15 0.000000047L-15 2.5L-8)
  "Muon Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mcomwlbar>.")

(define-constant muon-g-factor
    (-2.0023318418L0 0.0000000013L0 6.3L-10)
  "Muon g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gmum>.")

(define-constant muon-magnetic-moment
    (-4.49044807L-26 0.00000015L-26 3.4L-8)
  "Muon magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumum>.")

(define-constant muon-magnetic-moment-anomaly
    (1.16592091L-3 0.00000063L-3 5.4L-7)
  "Muon magnetic moment anomaly.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?amu>.")

(define-constant muon-magnetic-moment-to-Bohr-magneton-ratio
    (-4.84197044L-3 0.00000012L-3 2.5L-8)
  "Muon magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmub>.")

(define-constant muon-magnetic-moment-to-nuclear-magneton-ratio
    (-8.89059697L0 0.00000022L0 2.5L-8)
  "Muon magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmun>.")

(define-constant muon-mass
    (1.883531475L-28 0.000000096L-28 5.1L-8)
  "Muon mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmu>.")

(define-constant muon-mass-energy-equivalent
    (1.692833667L-11 0.000000086L-11 5.1L-8)
  "Muon mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2>.")

(define-constant muon-mass-energy-equivalent-in-MeV
    (105.6583715L0 0.0000035L0 3.4L-8)
  "Muon mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuc2mev>.")

(define-constant muon-mass-in-u
    (0.1134289267L0 0.0000000029L0 2.5L-8)
  "Muon mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmuu>.")

(define-constant muon-molar-mass
    (0.1134289267L-3 0.0000000029L-3 2.5L-8)
  "Muon molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmmu>.")

(define-constant muon-electron-mass-ratio
    (206.7682843L0 0.0000052L0 2.5L-8)
  "Muon-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusme>.")

(define-constant muon-neutron-mass-ratio
    (0.1124545177L0 0.0000000028L0 2.5L-8)
  "Muon-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmn>.")

(define-constant muon-proton-magnetic-moment-ratio
    (-3.183345107L0 0.000000084L0 2.6L-8)
  "Muon-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mumumsmup>.")

(define-constant muon-proton-mass-ratio
    (0.1126095272L0 0.0000000028L0 2.5L-8)
  "Muon-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmp>.")

(define-constant muon-tau-mass-ratio
    (5.94649L-2 0.00054L-2 9.0L-5)
  "Muon-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmusmtau>.")

(define-constant natural-unit-of-action
    (1.054571726L-34 0.000000047L-34 4.4L-8)
  "Natural unit of action.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thbar>.")

(define-constant natural-unit-of-action-in-eV-s
    (6.58211928L-16 0.00000015L-16 2.2L-8)
  "Natural unit of action in eV s.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?thbarev>.")

(define-constant natural-unit-of-energy
    (8.18710506L-14 0.00000036L-14 4.4L-8)
  "Natural unit of energy.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tmec2>.")

(define-constant natural-unit-of-energy-in-MeV
    (0.510998928L0 0.000000011L0 2.2L-8)
  "Natural unit of energy in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tmec2mev>.")

(define-constant natural-unit-of-length
    (386.15926800L-15 0.00000025L-15 6.5L-10)
  "Natural unit of length.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tecomwlbar>.")

(define-constant natural-unit-of-mass
    (9.10938291L-31 0.00000040L-31 4.4L-8)
  "Natural unit of mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tme>.")

(define-constant natural-unit-of-momentum
    (2.73092429L-22 0.00000012L-22 4.4L-8)
  "Natural unit of momentum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mec>.")

(define-constant natural-unit-of-momentum-in-MeV/c
    (0.510998928L0 0.000000011L0 2.2L-8)
  "Natural unit of momentum in MeV/c.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mecmevsc>.")

(define-constant natural-unit-of-time
    (1.28808866833L-21 0.00000000083L-21 6.5L-10)
  "Natural unit of time.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?nut>.")

(define-constant natural-unit-of-velocity
    (299792458 0 0)
  "Natural unit of velocity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tc>.")

(define-constant neutron-Compton-wavelength
    (1.3195909068L-15 0.0000000011L-15 8.2L-10)
  "Neutron Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwl>.")

(define-constant neutron-Compton-wavelength-over-2-pi
    (0.21001941568L-15 0.00000000017L-15 8.2L-10)
  "Neutron Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ncomwlbar>.")

(define-constant neutron-g-factor
    (-3.82608545L0 0.00000090L0 2.4L-7)
  "Neutron g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gnn>.")

(define-constant neutron-gyromagnetic-ratio
    (1.83247179L8 0.00000043L8 2.4L-7)
  "Neutron gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gamman>.")

(define-constant neutron-gyromagnetic-ratio-over-2-pi
    (29.1646943L0 0.0000069L0 2.4L-7)
  "Neutron gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammanbar>.")

(define-constant neutron-magnetic-moment
    (-0.96623647L-26 0.00000023L-26 2.4L-7)
  "Neutron magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munn>.")

(define-constant neutron-magnetic-moment-to-Bohr-magneton-ratio
    (-1.04187563L-3 0.00000025L-3 2.4L-7)
  "Neutron magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmub>.")

(define-constant neutron-magnetic-moment-to-nuclear-magneton-ratio
    (-1.91304272L0 0.00000045L0 2.4L-7)
  "Neutron magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmun>.")

(define-constant neutron-mass
    (1.674927351L-27 0.000000074L-27 4.4L-8)
  "Neutron mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mn>.")

(define-constant neutron-mass-energy-equivalent
    (1.505349631L-10 0.000000066L-10 4.4L-8)
  "Neutron mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2>.")

(define-constant neutron-mass-energy-equivalent-in-MeV
    (939.565379L0 0.000021L0 2.2L-8)
  "Neutron mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnc2mev>.")

(define-constant neutron-mass-in-u
    (1.00866491600L0 0.00000000043L0 4.2L-10)
  "Neutron mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnu>.")

(define-constant neutron-molar-mass
    (1.00866491600L-3 0.00000000043L-3 4.2L-10)
  "Neutron molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmn>.")

(define-constant neutron-to-shielded-proton-magnetic-moment-ratio
    (-0.68499694L0 0.00000016L0 2.4L-7)
  "Neutron to shielded proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmupp>.")

(define-constant neutron-electron-magnetic-moment-ratio
    (1.04066882L-3 0.00000025L-3 2.4L-7)
  "Neutron-electron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmue>.")

(define-constant neutron-electron-mass-ratio
    (1838.6836605L0 0.0000011L0 5.8L-10)
  "Neutron-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsme>.")

(define-constant neutron-muon-mass-ratio
    (8.89248400L0 0.00000022L0 2.5L-8)
  "Neutron-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmmu>.")

(define-constant neutron-proton-magnetic-moment-ratio
    (-0.68497934L0 0.00000016L0 2.4L-7)
  "Neutron-proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munsmup>.")

(define-constant neutron-proton-mass-difference
    (2.30557392L-30 0.00000076L-30 3.3L-7)
  "Neutron-proton mass difference.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmp>.")

(define-constant neutron-proton-mass-difference-energy-equivalent
    (2.07214650L-13 0.00000068L-13 3.3L-7)
  "Neutron-proton mass difference energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2>.")

(define-constant neutron-proton-mass-difference-energy-equivalent-in-MeV
    (1.29333217L0 0.00000042L0 3.3L-7)
  "Neutron-proton mass difference energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpc2mev>.")

(define-constant neutron-proton-mass-difference-in-u
    (0.00138844919L0 0.00000000045L0 3.3L-7)
  "Neutron-proton mass difference in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnmmpu>.")

(define-constant neutron-proton-mass-ratio
    (1.00137841917L0 0.00000000045L0 4.5L-10)
  "Neutron-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmp>.")

(define-constant neutron-tau-mass-ratio
    (0.528790L0 0.000048L0 9.0L-5)
  "Neutron-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mnsmtau>.")

(define-constant Newtonian-constant-of-gravitation
    (6.67384L-11 0.00080L-11 1.2L-4)
  "Newtonian constant of gravitation.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bg>.")

(define-constant Newtonian-constant-of-gravitation-over-h-bar-c
    (6.70837L-39 0.00080L-39 1.2L-4)
  "Newtonian constant of gravitation over h-bar c.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bgspu>.")

(define-constant nuclear-magneton
    (5.05078353L-27 0.00000011L-27 2.2L-8)
  "Nuclear magneton.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mun>.")

(define-constant nuclear-magneton-in-eV/T
    (3.1524512605L-8 0.0000000022L-8 7.1L-10)
  "Nuclear magneton in eV/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munev>.")

(define-constant nuclear-magneton-in-inverse-meters-per-tesla
    (2.542623527L-2 0.000000056L-2 2.2L-8)
  "Nuclear magneton in inverse meters per tesla.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshcminv>.")

(define-constant nuclear-magneton-inK/T
    (3.6582682L-4 0.0000033L-4 9.1L-7)
  "Nuclear magneton in K/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munskk>.")

(define-constant nuclear-magneton-in-MHz/T
    (7.62259357L0 0.00000017L0 2.2L-8)
  "Nuclear magneton in MHz/T.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?munshhz>.")

(define-constant Planck-constant
    (6.62606957L-34 0.00000029L-34 4.4L-8)
  "Planck constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?h>.")

(define-constant Planck-constant-in-eV-s
    (4.135667516L-15 0.000000091L-15 2.2L-8)
  "Planck constant in eV s.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hev>.")

(define-constant Planck-constant-over-2-pi
    (1.054571726L-34 0.000000047L-34 4.4L-8)
  "Planck constant over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbar>.")

(define-constant Planck-constant-over-2-pi-in-eV-s
    (6.58211928L-16 0.00000015L-16 2.2L-8)
  "Planck constant over 2 pi in eV s.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbarev>.")

(define-constant Planck-constant-over-2-pi-times-c-in-MeV-fm
    (197.3269718L0 0.0000044L0 2.2L-8)
  "Planck constant over 2 pi times c in MeV fm.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hbcmevf>.")

(define-constant Planck-length
    (1.616199L-35 0.000097L-35 6.0L-5)
  "Planck length.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkl>.")

(define-constant Planck-mass
    (2.17651L-8 0.00013L-8 6.0L-5)
  "Planck mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkm>.")

(define-constant Planck-mass-energy-equivalent-in-GeV
    (1.220932L19 0.000073L19 6.0L-5)
  "Planck mass energy equivalent in GeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkmc2gev>.")

(define-constant Planck-temperature
    (1.416833L32 0.000085L32 6.0L-5)
  "Planck temperature.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plktmp>.")

(define-constant Planck-time
    (5.39106L-44 0.00032L-44 6.0L-5)
  "Planck time.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?plkt>.")

(define-constant proton-charge-to-mass-quotient
    (9.57883358L7 0.00000021L7 2.2L-8)
  "Proton charge to mass quotient.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?esmp>.")

(define-constant proton-Compton-wavelength
    (1.32140985623L-15 0.00000000094L-15 7.1L-10)
  "Proton Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwl>.")

(define-constant proton-Compton-wavelength-over-2-pi
    (0.21030891047L-15 0.00000000015L-15 7.1L-10)
  "Proton Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?pcomwlbar>.")

(define-constant proton-g-factor
    (5.585694713L0 0.000000046L0 8.2L-9)
  "Proton g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gp>.")

(define-constant proton-gyromagnetic-ratio
    (2.675222005L8 0.000000063L8 2.4L-8)
  "Proton gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammap>.")

(define-constant proton-gyromagnetic-ratio-over-2-pi
    (42.5774806L0 0.0000010L0 2.4L-8)
  "Proton gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapbar>.")

(define-constant proton-magnetic-moment
    (1.410606743L-26 0.000000033L-26 2.4L-8)
  "Proton magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mup>.")

(define-constant proton-magnetic-moment-to-Bohr-magneton-ratio
    (1.521032210L-3 0.000000012L-3 8.1L-9)
  "Proton magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmub>.")

(define-constant proton-magnetic-moment-to-nuclear-magneton-ratio
    (2.792847356L0 0.000000023L0 8.2L-9)
  "Proton magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmun>.")

(define-constant proton-magnetic-shielding-correction
    (25.694L-6 0.014L-6 5.3L-4)
  "Proton magnetic shielding correction.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmapp>.")

(define-constant proton-mass
    (1.672621777L-27 0.000000074L-27 4.4L-8)
  "Proton mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mp>.")

(define-constant proton-mass-energy-equivalent
    (1.503277484L-10 0.000000066L-10 4.4L-8)
  "Proton mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2>.")

(define-constant proton-mass-energy-equivalent-in-MeV
    (938.272046L0 0.000021L0 2.2L-8)
  "Proton mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpc2mev>.")

(define-constant proton-mass-in-u
    (1.007276466812L0 0.000000000090L0 8.9L-11)
  "Proton mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpu>.")

(define-constant proton-molar-mass
    (1.007276466812L-3 0.000000000090L-3 8.9L-11)
  "Proton molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmp>.")

(define-constant proton-rms-charge-radius
    (0.8775L-15 0.0051L-15 5.9L-3)
  "Proton rms charge radius.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rp>.")

(define-constant proton-electron-mass-ratio
    (1836.15267245L0 0.00000075L0 4.1L-10)
  "Proton-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsme>.")

(define-constant proton-muon-mass-ratio
    (8.88024331L0 0.00000022L0 2.5L-8)
  "Proton-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmmu>.")

(define-constant proton-neutron-magnetic-moment-ratio
    (-1.45989806L0 0.00000034L0 2.4L-7)
  "Proton-neutron magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupsmunn>.")

(define-constant proton-neutron-mass-ratio
    (0.99862347826L0 0.00000000045L0 4.5L-10)
  "Proton-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmn>.")

(define-constant proton-tau-mass-ratio
    (0.528063L0 0.000048L0 9.0L-5)
  "Proton-tau mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mpsmtau>.")

(define-constant quantum-of-circulation
    (3.6369475520L-4 0.0000000024L-4 6.5L-10)
  "Quantum of circulation.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?qucirchs2me>.")

(define-constant quantum-of-circulation-times-2
    (7.2738951040L-4 0.0000000047L-4 6.5L-10)
  "Quantum of circulation times 2.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?hsme>.")

(define-constant Rydberg-constant
    (10973731.568539L0 0.000055L0 5.0L-12)
  "Rydberg constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?ryd>.")

(define-constant Rydberg-constant-times-c-in-Hz
    (3.289841960364L15 0.000000000017L15 5.0L-12)
  "Rydberg constant times c in Hz.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydchz>.")

(define-constant Rydberg-constant-times-hc-in-eV
    (13.60569253L0 0.00000030L0 2.2L-8)
  "Rydberg constant times hc in eV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcev>.")

(define-constant Rydberg-constant-times-hc-in-J
    (2.179872171L-18 0.000000096L-18 4.4L-8)
  "Rydberg constant times hc in J.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rydhcj>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-100000Pa
    (-1.1517078L0 0.0000023L0 2.0L-6)
  "Sackur-Tetrode constant (1 K, 100000 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0sr>.")

(define-constant Sackur-Tetrode-constant-at-1K-and-101325Pa
    (-1.1648708L0 0.0000023L0 1.9L-6)
  "Sackur-Tetrode constant (1 K, 101325 Pa).

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?s0srstd>.")

(define-constant second-radiation-constant
    (1.4387770L-2 0.0000013L-2 9.1L-7)
  "Second radiation constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c22ndrc>.")

(define-constant shielded-helion-gyromagnetic-ratio
    (2.037894659L8 0.000000051L8 2.5L-8)
  "Shielded helion gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahp>.")

(define-constant shielded-helion-gyromagnetic-ratio-over-2-pi
    (32.43410084L0 0.00000081L0 2.5L-8)
  "Shielded helion gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammahpbar>.")

(define-constant shielded-helion-magnetic-moment
    (-1.074553044L-26 0.000000027L-26 2.5L-8)
  "Shielded helion magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhp>.")

(define-constant shielded-helion-magnetic-moment-to-Bohr-magneton-ratio
    (-1.158671471L-3 0.000000014L-3 1.2L-8)
  "Shielded helion magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmub>.")

(define-constant shielded-helion-magnetic-moment-to-nuclear-magneton-ratio
    (-2.127497718L0 0.000000025L0 1.2L-8)
  "Shielded helion magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmun>.")

(define-constant shielded-helion-to-proton-magnetic-moment-ratio
    (-0.761766558L0 0.000000011L0 1.4L-8)
  "Shielded helion to proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmup>.")

(define-constant shielded-helion-to-shielded-proton-magnetic-moment-ratio
    (-0.7617861313L0 0.0000000033L0 4.3L-9)
  "Shielded helion to shielded proton magnetic moment ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muhpsmupp>.")

(define-constant shielded-proton-gyromagnetic-ratio
    (2.675153268L8 0.000000066L8 2.5L-8)
  "Shielded proton gyromagnetic ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammapp>.")

(define-constant shielded-proton-gyromagnetic-ratio-over-2-pi
    (42.5763866L0 0.0000010L0 2.5L-8)
  "Shielded proton gyromagnetic ratio over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gammappbar>.")

(define-constant shielded-proton-magnetic-moment
    (1.410570499L-26 0.000000035L-26 2.5L-8)
  "Shielded proton magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mupp>.")

(define-constant shielded-proton-magnetic-moment-to-Bohr-magneton-ratio
    (1.520993128L-3 0.000000017L-3 1.1L-8)
  "Shielded proton magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmub>.")

(define-constant shielded-proton-magnetic-moment-to-nuclear-magneton-ratio
    (2.792775598L0 0.000000030L0 1.1L-8)
  "Shielded proton magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?muppsmun>.")

(define-constant speed-of-light-in-vacuum
    (299792458 0 0)
  "Speed of light in vacuum.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?c>.")

(define-constant standard-acceleration-of-gravity
    (9.80665L0 0 0)
  "Standard acceleration of gravity.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gn>.")

(define-constant standard-atmosphere
    (101325 0 0)
  "Standard atmosphere.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdatm>.")

(define-constant standard-state-pressure
    (100000 0 0)
  "Standard-state pressure.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?stdspr>.")

(define-constant Stefan-Boltzmann-constant
    (5.670373L-8 0.000021L-8 3.6L-6)
  "Stefan-Boltzmann constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigma>.")

(define-constant tau-Compton-wavelength
    (0.697787L-15 0.000063L-15 9.0L-5)
  "Tau Compton wavelength.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwl>.")

(define-constant tau-Compton-wavelength-over-2-pi
    (0.111056L-15 0.000010L-15 9.0L-5)
  "Tau Compton wavelength over 2 pi.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tcomwlbar>.")

(define-constant tau-mass
    (3.16747L-27 0.00029L-27 9.0L-5)
  "Tau mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtau>.")

(define-constant tau-mass-energy-equivalent
    (2.84678L-10 0.00026L-10 9.0L-5)
  "Tau mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2>.")

(define-constant tau-mass-energy-equivalent-in-MeV
    (1776.82L0 0.16L0 9.0L-5)
  "Tau mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauc2mev>.")

(define-constant tau-mass-in-u
    (1.90749L0 0.00017L0 9.0L-5)
  "Tau mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtauu>.")

(define-constant tau-molar-mass
    (1.90749L-3 0.00017L-3 9.0L-5)
  "Tau molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmtau>.")

(define-constant tau-electron-mass-ratio
    (3477.15L0 0.31L0 9.0L-5)
  "Tau-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausme>.")

(define-constant tau-muon-mass-ratio
    (16.8167L0 0.0015L0 9.0L-5)
  "Tau-muon mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmmu>.")

(define-constant tau-neutron-mass-ratio
    (1.89111L0 0.00017L0 9.0L-5)
  "Tau-neutron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmn>.")

(define-constant tau-proton-mass-ratio
    (1.89372L0 0.00017L0 9.0L-5)
  "Tau-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtausmp>.")

(define-constant Thomson-cross-section
    (0.6652458734L-28 0.0000000013L-28 1.9L-9)
  "Thomson cross section.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sigmae>.")

(define-constant triton-g-factor
    (5.957924896L0 0.000000076L0 1.3L-8)
  "Triton g factor.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?gtn>.")

(define-constant triton-magnetic-moment
    (1.504609447L-26 0.000000038L-26 2.6L-8)
  "Triton magnetic moment.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mut>.")

(define-constant triton-magnetic-moment-to-Bohr-magneton-ratio
    (1.622393657L-3 0.000000021L-3 1.3L-8)
  "Triton magnetic moment to Bohr magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmub>.")

(define-constant triton-magnetic-moment-to-nuclear-magneton-ratio
    (2.978962448L0 0.000000038L0 1.3L-8)
  "Triton magnetic moment to nuclear magneton ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mutsmun>.")

(define-constant triton-mass
    (5.00735630L-27 0.00000022L-27 4.4L-8)
  "Triton mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mt>.")

(define-constant triton-mass-energy-equivalent
    (4.50038741L-10 0.00000020L-10 4.4L-8)
  "Triton mass energy equivalent.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2>.")

(define-constant triton-mass-energy-equivalent-in-MeV
    (2808.921005L0 0.000062L0 2.2L-8)
  "Triton mass energy equivalent in MeV.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtc2mev>.")

(define-constant triton-mass-in-u
    (3.0155007134L0 0.0000000025L0 8.2L-10)
  "Triton mass in u.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtu>.")

(define-constant triton-molar-mass
    (3.0155007134L-3 0.0000000025L-3 8.2L-10)
  "Triton molar mass.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mmt>.")

(define-constant triton-electron-mass-ratio
    (5496.9215267L0 0.0000050L0 9.1L-10)
  "Triton-electron mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsme>.")

(define-constant triton-proton-mass-ratio
    (2.9937170308L0 0.0000000025L0 8.2L-10)
  "Triton-proton mass ratio.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?mtsmp>.")

(define-constant unified-atomic-mass-unit
    (1.660538921L-27 0.000000073L-27 4.4L-8)
  "Unified atomic mass unit.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?tukg>.")

(define-constant von-Klitzing-constant
    (25812.8074434L0 0.0000084L0 3.2L-10)
  "Von Klitzing constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?rk>.")

(define-constant weak-mixing-angle
    (0.2223L0 0.0021L0 9.5L-3)
  "Weak mixing angle.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?sin2th>.")

(define-constant Wien-frequency-displacement-law-constant
    (5.8789254L10 0.0000053L10 9.1L-7)
  "Wien frequency displacement law constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bpwien>.")

(define-constant Wien-wavelength-displacement-law-constant
    (2.8977721L-3 0.0000026L-3 9.1L-7)
  "Wien wavelength displacement law constant.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?bwien>.")

(define-constant 220-lattice-spacing-of-silicon
    (192.0155714L-12 0.0000032L-12 1.6L-8)
  "{220} lattice spacing of silicon.

2010 CODATA recommended value.

See <http://physics.nist.gov/cgi-bin/cuu/Value?d220sil>.")

;; codata-2010.lisp ends here
