(define-module (symex packages angr)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages version-control)
  #:use-module ((gnu packages engineering) #:prefix engineering:)
  #:use-module (gnu packages python-xyz))

;; XXX: angr needs unicorn 2.X, Guix only packages unicorn 1.X
;;
;; See https://issues.guix.gnu.org/63442
(define-public unicorn
  (package
    (name "unicorn")
    (version "2.0.1.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32 "0mlfs8qfi0clyncfkbxp6in0cpl747510i6bqymwid43xcirbikz"))))
    ;; NOTE: Unicorn 2 using pyproject to build
    (build-system pyproject-build-system)
    (native-inputs (list cmake pkg-config))
    (home-page "http://www.unicorn-engine.org")
    (synopsis "Unicorn CPU emulator framework")
    (description
     "Uniforn is a lightweight, multi-platform, multi-architecture CPU emulator
                 framework based on QEMU.")
    (license license:gpl2+)))

;; Custom python-capstone verison with a backport for a capstone regression.
;;
;; See: https://github.com/capstone-engine/capstone/pull/2240
(define-public python-capstone
  (package-with-patches engineering:python-capstone
                        (search-patches "patches/python-capstone-fix-python-constants.patch")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public python-rpyc
  (package
    (name "python-rpyc")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rpyc" version))
       (sha256
        (base32 "0lmzrxc6f0sg6k0yvi6f2z44dkhiankdadv44sp1ibwzhxs328zj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-hatchling python-plumbum))
    (home-page "https://github.com/tomerfiliba-org/rpyc")
    (synopsis
     "Remote Python Call (RPyC) is a symmetric distributed computing library")
    (description
     "This Python module enables remote procedure calls, clustering, and
distributed-computing.  For this purpose, it makes use of object-proxying, a technique
that employs python's dynamic nature, to overcome the physical boundaries between
processes and computers, so that remote objects can be manipulated as if they
were local.")
    (license license:expat)))

(define-public python-nampa
  (package
    (name "python-nampa")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nampa" version))
       (sha256
        (base32 "0k6cq2gflpkm40qhgqbbcjmq5lq589c15bmk567qyh3d08062hvd"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-future))
    (home-page "https://github.com/kenoph/nampa")
    (synopsis "Python implementation of IDA Pro's FLIRT technology")
    (description
     "This Python module implements the Fast Library Identification
and Recognition Technology (FLIRT).  This technology is useful for identifying
common library subroutines in disassembled binaries.")
    (license license:lgpl3)))

(define-public python-mulpyplexer
  (package
    (name "python-mulpyplexer")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mulpyplexer" version))
       (sha256
        (base32 "0c5xzci1djy1yi9hxxh8g67l6ms8r7ad7ja20pv8hfbdysdrwkhl"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/zardus/mulpyplexer/")
    (synopsis "Multiplexes interactions with lists of Python objects")
    (description "This module provides utilities for multiplexing
interactions with lists of Python objects.")
    (license license:bsd-3)))

(define-public python-cppheaderparser
  (package
    (name "python-cppheaderparser")
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "CppHeaderParser" version))
       (sha256
        (base32 "0hncwd9y5ayk8wa6bqhp551mcamcvh84h89ba3labc4mdm0k0arq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-ply))
    (home-page "http://senexcanis.com/open-source/cppheaderparser/")
    (synopsis "Parser for C++ header files")
    (description
     "Parse C++ header files and generate a data structure representing the class.")
    (license license:bsd-3)))

(define-public python-pysmt
  (package
    (name "python-pysmt")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PySMT" version))
       (sha256
        (base32 "0izcl2wma7sid4a8a2qc9rzi8fnffsc8p97cx6njb2sc9rnnly8c"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/pysmt/pysmt")
    (synopsis
     "A solver-agnostic library for SMT formula manipulation and solving")
    (description
     "This Python module provides a solver-agnostic abstraction for
working with Satisfiability Modulo Theory (SMT) formulas  For example,
it allows manipulation and solving such formulas.")
    (license license:asl2.0)))

(define-public python-claripy
  (package
    (name "python-claripy")
    (version "9.2.46")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "claripy" version))
       (sha256
        (base32 "137pfd765jdg4xsn2fsi3k3mj2fg7qypm9k27bb5qpz8fbqddnjv"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "setup.cfg"
                     ;; See https://github.com/angr/claripy/commit/d1fe2df65afa137454d0da83a95be925f71ff262
                     ;; XXX: It seems the native z3 library is called z3 not z3-solver.
                     (("z3-solver==4.10.2.0")
                      ""))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-cachetools python-decorator python-pysmt
                             z3))
    (home-page "https://github.com/angr/claripy")
    (synopsis "Abstraction layer for constraint solvers")
    (description
     "This Python module provides an abstraction layer for interacting
with constraint solvers.  Specifically, it is intended to be used with
SMT solvers and is built on top of the Z3 solver.")
    (license license:bsd-2)))

(define-public python-pyvex
  (package
    (name "python-pyvex")
    (version "9.2.46")
    (source
     (origin
       (method url-fetch)
       (patches (search-patches "patches/python-pyvex-remove-angr-dependency.patch"))
       (uri (pypi-uri "pyvex" version))
       (sha256
        (base32 "1v64rn7gxy6fg065bgsy38z6r494k5ri5r6sn4g08hjj32ihx1ka"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (with-directory-excursion "tests"
                           (invoke "python" "-m" "unittest")))))

                   (add-before 'build 'set-cc-native
                     (lambda _
                       (setenv "CC" "gcc")
                       (setenv "CC_NATIVE" "gcc"))))))
    (propagated-inputs (list python-archinfo python-bitstring python-cffi))
    (inputs (list gcc-toolchain))
    (home-page "https://github.com/angr/pyvex")
    (synopsis "Python interface to libVEX and VEX IR")
    (description
     "This package provides a Python interface the libVEX and VEX IR.
VEX is the intermediate representation (also known as intermediate
language) used by the Valgrind analysis tool.  As such, VEX is designed
to enable all kinds of binary analysis tasks.")
    (license license:bsd-2)))

(define-public python-cle
  (package
    (name "python-cle")
    (version "9.2.46")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cle" version))
       (sha256
        (base32 "0mswv9gd2p2ws7zfsshqv5ybbj27wkdwakdcknq4vsrx9ry9k4yc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))
    (propagated-inputs (list python-pefile python-pyelftools python-pyvex
                             python-sortedcontainers))
    (native-inputs (list python-cffi))
    (home-page "https://github.com/angr/cle")
    (synopsis "Python loader for binaries and their associated libraries")
    (description
     "CLE loads binaries and their associated libraries, resolves
imports and provides an abstraction of process memory the same way as if
it was loader by the operating system's loader.")
    (license license:bsd-2)))

(define-public python-keystone-engine
  (package
    (name "python-keystone-engine")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keystone-engine" version))
       (sha256
        (base32 "1xahdr6bh3dw5swrc2r8kqa8ljhqlb7k2kxv5mrw5rhcmcnzcyig"))))
    (inputs (list cmake))
    (build-system pyproject-build-system)
    (home-page "https://www.keystone-engine.org")
    (synopsis
     "Lightweight multi-platform, multi-architecture assembler framework")
    (description
     "Keystone is a lightweight multi-platform, multi-architecture
assembler framework.  It supports a wide-range of different architectures
and offers an intuitive architecture-neutral API for interacting with
assembly for these architectures.")
    (license license:gpl2)))

(define-public python-archinfo
  (package
    (name "python-archinfo")
    (version "9.2.46")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "archinfo" version))
       (sha256
        (base32 "037xfq3wcf8ngayxz9623l4646m780v2102mfbygpzbkkjha1966"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-capstone python-keystone-engine))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
    (home-page "https://github.com/angr/archinfo")
    (synopsis "Extract architecture-specific information from binaries")
    (description
     "Collection of classes that contain architecture-specific information
information.  Useful for cross-architecture tools (such as @code{python-pyvex}).")
    (license license:bsd-2)))

(define-public python-ailment
  (package
    (name "python-ailment")
    (version "9.2.46")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ailment" version))
       (sha256
        (base32 "073fcssbjis1ckwv2w0dcz2dfl6715bj4d4qdhspajj911mvng2f"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
    (home-page "https://github.com/angr/ailment")
    (synopsis "The angr intermediate language")
    (description
     "This Python module implements an @acronym{IL, Intermediate
Language}, also known as @acronym{IR, Intermediate Representation),
used by the angr binary analysis platform.")
    (license license:bsd-2)))

(define-public python-itanium-demangle
  (package
    (name "python-itanium-demangle")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whitequark/python-itanium_demangler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q47aqm5z3db6pasdzw05d6236vnb8hnapfy88fcmn9dr5ym98r3"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
    (home-page "https://github.com/whitequark/python-itanium_demangler")
    (synopsis "Pure Python Itanium C++ ABI demangler")
    (description
     "This Python module provides an implementation of the Itanium C++
ABI symbol mangling language.  The demangler generates an abstract
syntax tree from mangled symbols, which can be used for directly
extracting type information.")
    (license license:bsd-0)))

(define-public python-angr
  (package
    (name "python-angr")
    (version "9.2.46")
    (source
     (origin
       (method git-fetch)
       (patches (search-patches "patches/python-angr-addition-type-error.patch"
                                "patches/python-angr-check-exec-deps.patch"))
       (uri (git-reference
             (url "https://github.com/angr/angr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18y9wyf7va7gvp9zd6lhw82j9a2x2ajsvbawh96xnxzml0jwlwjm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-tests
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((coreutils (assoc-ref inputs "coreutils")))
                         (substitute* "tests/test_vault.py"
                           (("/bin/false")
                            (string-append coreutils "/bin/false")))
                         (substitute* "tests/common.py"
                           (("\\[\"cc\"\\]")
                            "[\"gcc\"]")))))
                   (replace 'check
                     (lambda* (#:key inputs tests? #:allow-other-keys)
                       (when tests?
                         (copy-recursively (assoc-ref inputs "binaries")
                                           "../binaries")
                         (with-directory-excursion "tests"
                           ;; test_mips32_missing_offset_in_instructions fails
                           ;; with capstone 5 and passes with capstone 4. Might
                           ;; be a capstone regressions, needs investigation.
                           (invoke "pytest" "-vv" "-x" "--dist" "loadfile"
                                   "-k" "not test_mips32_missing_offset_in_instructions"
                                   "-n" (number->string (parallel-job-count)))))))
                   (add-before 'build 'set-cc
                     (lambda _
                       (setenv "CC" "gcc"))))))
    (propagated-inputs (list python-ailment
                             python-archinfo
                             python-cachetools
                             python-capstone
                             python-cffi
                             python-claripy
                             python-cle
                             python-colorama
                             python-cppheaderparser
                             python-dpkt
                             python-gitpython
                             python-mulpyplexer
                             python-nampa
                             python-networkx
                             python-protobuf
                             python-psutil
                             python-itanium-demangle
                             python-pycparser
                             python-pyvex
                             python-progressbar2
                             python-rpyc
                             python-sortedcontainers
                             python-sqlalchemy
                             python-sympy
                             unicorn))
    (native-inputs `(("python-pytest" ,python-pytest)
                     ("python-pytest-xdist" ,python-pytest-xdist)
                     ("binaries"
                      ;; This repository ships several binaries used only for testing
                      ;; purpose.  The binaries are not executed and not part of the
                      ;; angr distribution, they are only used to test angr's binary
                      ;; analysis capabilities.  In the context of the GNU FSDG, these
                      ;; files should be considered non-functional data.
                      ,(origin
                         (method git-fetch)
                         (uri (git-reference (url
                                              "https://github.com/angr/binaries")
                                             (commit (string-append "v"
                                                                    version))))
                         (file-name (git-file-name "angr-binaries" version))
                         (sha256 (base32
                                  "1f286b2239zavxzwg1184hj1zs380cr9qr549mvy3vywvm8bsmgr"))))))
    (home-page "https://github.com/angr/angr")
    (synopsis "Multi-architecture binary analysis toolkit")
    (description
     "This package provides a versatile binary analysis platform with the
ability to perform dynamic symbolic execution as well as various
static analyses directly on binaries.  As such, it can be used for all
kinds of reverse engineering, vulnerability discovery, exploit
generation, and software testing purposes.")
    (license license:bsd-2)))
