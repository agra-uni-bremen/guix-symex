(define-module (symex packages angr)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages python-xyz))

(define-public python-rpyc
  (package
    (name "python-rpyc")
    (version "5.3.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rpyc" version))
              (sha256
               (base32
                "0lmzrxc6f0sg6k0yvi6f2z44dkhiankdadv44sp1ibwzhxs328zj"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-hatchling python-plumbum))
    (home-page "https://github.com/tomerfiliba-org/rpyc")
    (synopsis
     "Remote Python Call (RPyC) is a transparent and symmetric distributed computing library")
    (description
     "Remote Python Call (RPyC) is a transparent and symmetric distributed computing
library")
    (license #f)))

(define-public python-nampa
  (package
    (name "python-nampa")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nampa" version))
              (sha256
               (base32
                "0k6cq2gflpkm40qhgqbbcjmq5lq589c15bmk567qyh3d08062hvd"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-future))
    (home-page "https://github.com/kenoph/nampa")
    (synopsis "FLIRT signatures for python")
    (description "FLIRT signatures for python")
    (license #f)))

(define-public python-mulpyplexer
  (package
    (name "python-mulpyplexer")
    (version "0.09")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mulpyplexer" version))
              (sha256
               (base32
                "0c5xzci1djy1yi9hxxh8g67l6ms8r7ad7ja20pv8hfbdysdrwkhl"))))
    (build-system pyproject-build-system)
    (home-page "")
    (synopsis
     "A module that multiplexes interactions with lists of python objects.")
    (description
     "This package provides a module that multiplexes interactions with lists of
python objects.")
    (license license:bsd-3)))

(define-public python-cppheaderparser
  (package
    (name "python-cppheaderparser")
    (version "2.7.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "CppHeaderParser" version))
              (sha256
               (base32
                "0hncwd9y5ayk8wa6bqhp551mcamcvh84h89ba3labc4mdm0k0arq"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-ply))
    (home-page "http://senexcanis.com/open-source/cppheaderparser/")
    (synopsis
     "Parse C++ header files and generate a data structure representing the class")
    (description
     "Parse C++ header files and generate a data structure representing the class")
    (license license:bsd-3)))

(define-public python-pysmt
  (package
    (name "python-pysmt")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PySMT" version))
              (sha256
               (base32
                "0izcl2wma7sid4a8a2qc9rzi8fnffsc8p97cx6njb2sc9rnnly8c"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (home-page "http://www.pysmt.org")
    (synopsis
     "A solver-agnostic library for SMT Formulae manipulation and solving")
    (description
     "This package provides a solver-agnostic library for SMT Formulae manipulation
and solving")
    (license #f)))

(define-public python-claripy
  (package
    (name "python-claripy")
    (version "9.2.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "claripy" version))
              (sha256
               (base32
                "137pfd765jdg4xsn2fsi3k3mj2fg7qypm9k27bb5qpz8fbqddnjv"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (substitute* "setup.cfg"
                     ;; See https://github.com/angr/claripy/commit/d1fe2df65afa137454d0da83a95be925f71ff262
                     ;; XXX: It seems the native z3 library is called z3 not z3-solver.
                     (("z3-solver==4.10.2.0") ""))))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-cachetools python-decorator python-pysmt z3))
    (home-page "https://github.com/angr/claripy")
    (synopsis "An abstraction layer for constraint solvers")
    (description "An abstraction layer for constraint solvers")
    (license license:bsd-2)))

(define-public python-pyvex
  (package
    (name "python-pyvex")
    (version "9.2.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyvex" version))
              (sha256
               (base32
                "1v64rn7gxy6fg065bgsy38z6r494k5ri5r6sn4g08hjj32ihx1ka"))))
    (build-system pyproject-build-system)
    (arguments
      (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-cc-native
              (lambda _
                (setenv "CC" "gcc")
                (setenv "CC_NATIVE" "gcc"))))))
    (propagated-inputs (list python-archinfo python-bitstring python-cffi))
    (inputs (list gcc-toolchain))
    (home-page "https://github.com/angr/pyvex")
    (synopsis "A Python interface to libVEX and VEX IR")
    (description
     "This package provides a Python interface to libVEX and VEX IR")
    (license license:bsd-2)))

(define-public python-cle
  (package
    (name "python-cle")
    (version "9.2.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cle" version))
              (sha256
               (base32
                "0mswv9gd2p2ws7zfsshqv5ybbj27wkdwakdcknq4vsrx9ry9k4yc"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-pefile python-pyelftools python-pyvex
                             python-sortedcontainers))
    (native-inputs (list python-cffi))
    (home-page "https://github.com/angr/cle")
    (synopsis "|")
    (description "|")
    (license license:bsd-2)))

(define-public python-archinfo
  (package
    (name "python-archinfo")
    (version "9.2.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "archinfo" version))
              (sha256
               (base32
                "037xfq3wcf8ngayxz9623l4646m780v2102mfbygpzbkkjha1966"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/angr/archinfo")
    (synopsis
     "Classes with architecture-specific information useful to other projects.")
    (description
     "Classes with architecture-specific information useful to other projects.")
    (license license:bsd-2)))

(define-public python-ailment
  (package
    (name "python-ailment")
    (version "9.2.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ailment" version))
              (sha256
               (base32
                "073fcssbjis1ckwv2w0dcz2dfl6715bj4d4qdhspajj911mvng2f"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/angr/ailment")
    (synopsis "The angr intermediate language.")
    (description "The angr intermediate language.")
    (license license:bsd-2)))

(define-public python-itanium_demangle
  (package
    (name "python-itanium_demangle")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/whitequark/python-itanium_demangler")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256 (base32 "1q47aqm5z3db6pasdzw05d6236vnb8hnapfy88fcmn9dr5ym98r3"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/whitequark/python-itanium_demangler")
    (synopsis "Pure Python Itanium C++ ABI demangler")
    (description "Pure Python Itanium C++ ABI demangler")
    (license license:bsd-0)))

(define-public unicorn
  (package
    (name "unicorn")
    (version "2.0.1.post1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri name version))
              (sha256 (base32 "0mlfs8qfi0clyncfkbxp6in0cpl747510i6bqymwid43xcirbikz"))))
    ;; NOTE: Unicorn 2 using pyproject to build
    (build-system pyproject-build-system)
    (native-inputs (list cmake pkg-config))
    (home-page "http://www.unicorn-engine.org")
    (synopsis "Unicorn CPU emulator framework")
    (description "Uniforn is a lightweight, multi-platform, multi-architecture CPU emulator
                 framework based on QEMU.")
    (license license:gpl2+)))

(define-public python-angr
  (package
    (name "python-angr")
    (version "9.2.46")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "angr" version))
              (sha256
               (base32
                "0liv0afmp3fnmkscqpdq4gqgnhvfpj7jh2y5b2998drn30gyxcgb"))))
    (build-system pyproject-build-system)
    (arguments
      (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-cc-native
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
                             python-itanium_demangle
                             python-pycparser
                             python-pyvex
                             python-progressbar2
                             python-rpyc
                             python-sortedcontainers
                             python-sympy
                             unicorn))
    (home-page "https://github.com/angr/angr")
    (synopsis
     "A multi-architecture binary analysis toolkit, with the ability to perform dynamic symbolic execution and various static analyses on binaries")
    (description
     "This package provides a multi-architecture binary analysis toolkit, with the
  ability to perform dynamic symbolic execution and various static analyses on
  binaries")
    (license license:bsd-2)))

(define-public python-angr-platforms
  (let ((commit "06db4e6a594af47aaeb0a5071f2cdb9a8c30f7f5"))
    (package
      (name "python-angr-platforms")
      (version (git-version "20230220" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/angr/angr-platforms")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256 (base32 "1pq5wm32zi6lm02k2mni6a5cw4nd2z87xqnchy3vzkkig46lggnc"))))
      (build-system pyproject-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs (list python-angr python-cle python-archinfo python-pyvex))
      (synopsis "A collection of extensions to angr")
      (description "Support for handling various new platforms with angr")
      (home-page "https://github.com/angr/angr-platforms")
      (license license:expat))))
