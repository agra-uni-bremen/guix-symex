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
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python-build)
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
    (build-system pyproject-build-system)
    (native-inputs (list cmake pkg-config))
    (home-page "https://www.unicorn-engine.org")
    (synopsis "Generic CPU emulator framework")
    (description
     "Uniforn is a lightweight, multi-platform, multi-architecture CPU
emulator framework based on QEMU.")
    (license license:gpl2+)))

;; Custom python-capstone verison with a backport for a capstone regression.
;;
;; See: https://github.com/capstone-engine/capstone/pull/2240
(define-public python-capstone
  (package-with-patches engineering:python-capstone
                        (search-patches "patches/python-capstone-fix-python-constants.patch")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public python-pyformlang
  (package
    (name "python-pyformlang")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyformlang" version))
       (sha256
        (base32 "0szgy4pqfixmswjs37qgma4qa3bsadpp3l1xflrpfi10aa8hh2sp"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-networkx python-numpy python-pydot))
    (home-page "https://github.com/Aunsiels/pyformlang")
    (synopsis "A python framework for formal grammars")
    (description
     "This package provides a python framework for formal grammars")
    (license license:expat)))

(define-public python-unique-log-filter
  (package
    (name "python-unique-log-filter")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twizmwazin/unique_log_filter")
             (commit (string-append "v" version))))
       (sha256
         (base32 "036mh6nqskck2fa1q2inasqxb9wcz2p09qcpldnnffzcy1a6kzba"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "python" "test_unique_log_filter.py")))))))
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/twizmwazin/unique_log_filter")
    (synopsis "A log filter that removes duplicate log messages")
    (description
     "This package provides a log filter that removes duplicate log messages.")
    (license license:bsd-2)))

(define-public python-backports-strenum
  (package
    (name "python-backports-strenum")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "backports_strenum" version))
       (sha256
        (base32 "0514yj1391k6pbs2cch6i57hidwb3236wngh2ivlk6186h3j9ibp"))))
    (native-inputs (list python-poetry-core))
    (build-system pyproject-build-system)
    ;; TODO: Running tests requires a new version of poetry in Guix.
    (arguments
     (list
      #:tests? #f))
    (home-page "https://github.com/clbarnes/backports.strenum")
    (synopsis "Backport of additions to the 'strenum' module")
    (description
     "Base class for creating enumerated constants that are also subclasses of str.")
    (license license:expat)))

(define-public python-rpyc
  (package
    (name "python-rpyc")
    (version "5.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tomerfiliba-org/rpyc")
             (commit version)))
       (sha256
        (base32 "15mnp9qkyw3mmxmr5y4kf3xkvxyp00n892vqaqwznr7al35apgnr"))
       (snippet '(begin
                   ;; Disable deploy tests, these rely on OpenSSH and require
                   ;; configuring the SSH client manually to accept the host key.
                   (delete-file "tests/test_deploy.py")
                   ;; Disable tests requiring network access.  These tests
                   ;; presently fail with the error "Network is unreachable".
                   (delete-file "tests/test_registry.py")))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
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
    (home-page "https://github.com/thebabush/nampa")
    (synopsis "Python implementation of IDA Pro's FLIRT technology")
    (description
     "This Python module implements the @acronym{FLIRT, Fast Library Identification
and Recognition Technology}.  This technology is useful for identifying
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
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "python" "mulpyplexer.py")))))))
    (home-page "https://github.com/zardus/mulpyplexer/")
    (synopsis "Multiplexes interactions with lists of Python objects")
    (description "This module provides utilities for multiplexing
interactions with lists of Python objects.")
    (license license:bsd-2)))

(define-public python-nanobind
  (package
    (name "python-nanobind")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (recursive? #t)
             (url "https://github.com/wjakob/nanobind")
             (commit (string-append "v" version))))
       (sha256
         (base32 "1aqdl533w3zc72hi73khxy5jh93x2lzmik86q6d864gc1smh7k7a"))))
    (build-system pyproject-build-system)
    (native-inputs (list cmake))
    (home-page "https://github.com/wjakob/nanobind")
    (synopsis "Provides tiny and efficient C++/Python bindings")
    (description "binding library that exposes C++ types in Python and vice
versa.  It is reminiscent of Boost.Python and pybind11 and uses near-identical
syntax.")
    (license license:bsd-3)))

(define-public python-pypcode
  (package
    (name "python-pypcode")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pypcode" version))
       (sha256
        (base32 "16533psi2rvfk6lxn4m2dk07v1m6skpz30vv0rwml85v5irqrv9h"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
    (native-inputs (list cmake))
    (inputs (list python-nanobind))
    (home-page "https://github.com/angr/pypcode")
    (synopsis "Machine code disassembly and IR translation library")
    (description "Machine code disassembly and IR translation library")
    (license (list license:bsd-2 license:asl2.0))))

(define-public python-pysmt
  (package
    (name "python-pysmt")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (patches (search-patches "patches/python-pysmt-fix-pow-return-type.patch"
                 "patches/python-pysmt-fix-smtlib-serialization-test.patch"))
       (uri (git-reference
             (url "https://github.com/pysmt/pysmt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrxv23y5ip4ijfx5pvbwc2fq4zg9jz42wc9zqgqm0g0mjc9ckvh"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'set-pysmt-solver
                    (lambda _
                      (setenv "PYSMT_SOLVER" "z3"))))))
    (native-inputs (list python-pytest))
    (propagated-inputs (list z3))
    (home-page "https://github.com/pysmt/pysmt")
    (synopsis
     "Solver-agnostic library for SMT formula manipulation and solving")
    (description
     "This Python module provides a solver-agnostic abstraction for
working with @acronym{SMT, Satisfiability Modulo Theory} formulas.  For example,
it allows manipulation and solving such formulas.")
    (license license:asl2.0)))

(define-public python-claripy
  (package
    (name "python-claripy")
    ;; Must be the same version as python-angr.
    (version "9.2.100")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/angr/claripy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z1r21d4kdf70jsg2g1cckvly8cfnlgsbvsiw8b1px3wamidfhwf"))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "setup.cfg"
                     ;; Relax the z3 version constraint.
                     ;; See https://github.com/angr/claripy/commit/d1fe2df
                     (("z3-solver==4.10.2.0")
                      ""))))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
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
    ;; Must be the same version as python-angr.
    (version "9.2.100")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvex" version))
       (sha256
        (base32 "0xm3q36f5kpzxhgf22khhifzakjzcwsh263cw16kc9mimcpyja74"))))
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
                       (setenv "CC" #$(cc-for-target))
                       (setenv "CC_NATIVE" "gcc"))))))
    (propagated-inputs (list python-archinfo python-bitstring python-cffi))
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
    ;; Must be the same version as python-angr.
    (version "9.2.100")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cle" version))
       (sha256
        (base32 "1p3fq04nfpzfclwcwiswv8bmr05qrp8zyr4azbnkc6gvp7bi3sw6"))))
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
    (native-inputs (list cmake))
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
    ;; Must be the same version as python-angr.
    (version "9.2.100")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "archinfo" version))
       (sha256
        (base32 "0zpqvkcvmr59g8fwn5sq0mrjw3h6k7hf87npc5pdz7jpi7hv2xyx"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-backports-strenum python-capstone python-keystone-engine))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch
                    (lambda _
                      (substitute* "setup.cfg"
                        (("backports.strenum")
                         "backports_strenum"))))
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
    ;; Must be the same version as python-angr.
    (version "9.2.100")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ailment" version))
       (sha256
        (base32 "15md0laxqywhi3vsqn96qkg95nw15d7xh2ch1pbnza15ybxxzx5b"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    ;; Many tests are skipped due to cyclic dependencies.
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (with-directory-excursion "tests"
                          (invoke "python" "-m" "unittest"))))))))
    (home-page "https://github.com/angr/ailment")
    (synopsis "The angr intermediate language")
    (description
     "This Python module implements an @acronym{IL, Intermediate Language},
also known as @acronym{IR, Intermediate Representation}, used by the angr
binary analysis platform.")
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
    (version "9.2.100")
    (source
     (origin
       (method git-fetch)
       (patches (search-patches "patches/python-angr-check-exec-deps.patch"
                                "patches/python-angr-pcode-riscv32-support.patch"))
       (uri (git-reference
             (url "https://github.com/angr/angr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j8bqy4wkqaln75pyysh4v583sp3ridnawda5p88j9baplpb2g0x"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let ((coreutils (assoc-ref inputs "coreutils")))
                         ;; Relax constraint on python-capstone, there is one failing test with 5.0.1.
                         ;;
                         ;; See also: https://github.com/angr/angr/pull/4087
                         (substitute* "setup.cfg"
                          (("capstone==5.0.0.post1")
                           "capstone"))
                         ;; Relax constraint on python-rich (seems to be too strict anyhow).
                         (substitute* "setup.cfg"
                           (("rich>=13.1.0")
                            "rich"))
                         (substitute* "tests/common.py"
                           (("\\[\"cc\"\\]")
                            "[\"gcc\"]")))))
                   (replace 'check
                     (lambda* (#:key inputs tests? #:allow-other-keys)
                       (when tests?
                         (copy-recursively #$(this-package-native-input "binaries")
                                           "../binaries")
                         (with-directory-excursion "tests"
                           ;; test_mips32_missing_offset_in_instructions fails
                           ;; with capstone 5 and passes with capstone 4. Might
                           ;; be a capstone regressions, needs investigation.
                           ;;
                           ;; test_concrete_memset is a non-deterministic benchmark.
                           (invoke "pytest" "-vv" "-x" "--dist" "loadfile"
                                   "-k" "not test_mips32_missing_offset_in_instructions and not test_concrete_memset"
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
                             python-pyformlang
                             python-rich
                             python-rpyc
                             python-sortedcontainers
                             python-sqlalchemy
                             python-sympy
                             python-unique-log-filter
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
                                  "0ng85zs13dvnvn7ny6jxf0735xi5knj16axnc2gbyvr4p7xmg92k"))))))
    (home-page "https://github.com/angr/angr")
    (synopsis "Multi-architecture binary analysis toolkit")
    (description
     "This package provides a versatile binary analysis platform with the
ability to perform dynamic symbolic execution as well as various
static analyses directly on binaries.  As such, it can be used for all
kinds of reverse engineering, vulnerability discovery, exploit
generation, and software testing purposes.")
    (license license:bsd-2)))
