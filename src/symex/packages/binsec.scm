(define-module (symex packages binsec)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml))

(define-public ocaml-iso8601
  (package
    (name "ocaml-iso8601")
    (version "0.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-community/ISO8601.ml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nzadswspizi7s6sf67icn2xgc3w150x8vdg5nk1mjrm2s98n6d3"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-stdlib-shims ocaml-core-unix ocaml-ounit))
    (synopsis "Parser and printer for date-times in ISO8601")
    (description "ISO 8601 and RFC 3339 date parsing for OCaml.")
    (home-page "https://github.com/ocaml-community/ISO8601.ml")
    (license license:expat)))

(define-public ocaml-toml
  (package
    (name "ocaml-toml")
    (version "7.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-toml/To.ml")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z2873mj3i6h9cg8zlkipcjab8jympa4c4avhk4l04755qzphkds"))))
    (build-system dune-build-system)
    (propagated-inputs (list ocaml-base ocaml-mdx ocaml-menhir ocaml-iso8601))
    (synopsis "TOML library for OCaml")
    (description
     "This package provides an OCaml library for interacting
with files in the TOML configuration format.  Specifically, it provides
parser, a serializer, and a printer for TOML.")
    (home-page "https://github.com/ocaml-toml/To.ml")
    (license license:expat)))

(define-public ocaml-pp
  (package
    (name "ocaml-pp")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ocaml-dune/pp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ylwb8lbjzj1prnal3c5p404dvh7bv4s19cvgrplnd7s46lvnj50"))))
    (build-system dune-build-system)
    (native-inputs (list ocaml-ppx-expect))
    (home-page "https://github.com/ocaml-dune/pp")
    (synopsis "Pretty-printing library")
    (description
     "Pretty-printing library allowing custom formatting of defined
types and values.  The API is intended as an alternative to the
@code{Format} module of the OCaml standard library.")
    (license license:expat)))

(define-public ocaml-grain-dypgen
  (package
    (name "ocaml-grain-dypgen")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grain-lang/dypgen")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jyxkvi75nchk5kmhqixmjy70z55gmlqa83pxn0hsv2qxvyqxavw"))))
    (build-system ocaml-build-system)
    (arguments
     (list
      ;; Upstream does not have a test suite.
      #:tests? #f
      #:make-flags #~(let ((out #$output))
                       (list (string-append "OCAMLLIBDIR=" out
                                            "/lib/ocaml/site-lib")
                             (string-append "BINDIR=" out "/bin")
                             (string-append "MANDIR=" out "/share/man")))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure))))
    (native-inputs (list ocaml-findlib))
    (properties `((upstream-name . "grain_dypgen")))
    (home-page "https://github.com/grain-lang/dypgen")
    (synopsis "Self-extensible parsers and lexers for OCaml")
    (description
     "@acronym{GLR, generalized LR} parser generator for OCaml, it is
able to generate self-extensible parsers (also called adaptive parsers)
as well as extensible lexers for the parsers it produces.")
    (license license:cecill-b)))

(define-public ocaml-stdune
  (package
    (inherit dune)
    (name "ocaml-stdune")
    (build-system dune-build-system)
    (arguments
     '(#:package "stdune"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs (list ocaml-dyn ocaml-ordering ocaml-pp ocaml-csexp
                             ocaml-odoc))
    (synopsis "Dune's unstable standard library")
    (description
     "This library ships the Dune standard library, the library does not
provide any backwards compatibility or stability guarantees.")
    (license license:expat)))

(define-public ocaml-ordering
  (package
    (inherit dune)
    (name "ocaml-ordering")
    (build-system dune-build-system)
    (arguments
     '(#:package "ordering"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs (list ocaml-odoc))
    (synopsis "Element ordering library provided by Dune")
    (description "Element ordering library provided by Dune.")
    (license license:expat)))

(define-public ocaml-dyn
  (package
    (inherit dune)
    (name "ocaml-dyn")
    (build-system dune-build-system)
    (arguments
     '(#:package "dyn"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs (list ocaml-ordering ocaml-pp ocaml-odoc))
    (synopsis "Dynamic type")
    (description "Dynamic type")
    (license license:expat)))

(define-public ocaml-dune-private-libs
  (package
    (inherit dune)
    (name "ocaml-dune-private-libs")
    (build-system dune-build-system)
    (arguments
     '(#:package "dune-private-libs"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs (list ocaml-csexp ocaml-pp ocaml-dyn ocaml-stdune
                             ocaml-odoc))
    (synopsis "Private libraries of Dune")
    (description
     "This OCaml library provides several private APIs shared between
various Dune-internal packages.  It is not intended for public use by
the authors and does therefore not provide any stability guarantees.
Nonetheless, many OCaml packages depend on this library.")
    (license license:expat)))

(define-public ocaml-dune-site
  (package
    (inherit dune)
    (name "ocaml-dune-site")
    (build-system dune-build-system)
    (arguments
     '(#:package "dune-site"
       ;; No separate test suite from dune.
       #:tests? #f))
    (propagated-inputs (list ocaml-dune-private-libs ocaml-odoc))
    (synopsis "Embed locations information inside executable and libraries")
    (description "This OCaml library allows embedding information inside
executable binaries and libraries, it is provided by Dune.")
    (license license:expat)))

(define-public binsec
  (package
    (name "binsec")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/binsec/binsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k6prp2nc9whvxxnnqpg0f5pr7arikwv2bhzna5yq6v629i70c7q"))))
    (build-system dune-build-system)
    (native-inputs (list gmp ocaml-qcheck ocaml-ounit2))
    (propagated-inputs (list ocaml-base
                             ocaml-dune-site
                             ocaml-menhir
                             ocaml-graph
                             ocaml-zarith
                             ocaml-grain-dypgen
                             ocaml-toml))
    (synopsis "Binary-level analysis platform")
    (description
     "BINSEC is a binary analysis platform which implements analysis
techniques such as symbolic execution.  The goal of BINSEC is to improve
software security at the binary level through binary analysis.  BINSEC
is a research tool which relies on prior work in binary code analysis
at the intersection of formal methods, program analysis security and
software engineering.")
    (home-page "https://binsec.github.io/")
    (license license:lgpl2.1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public cadical
  (package
    (name "cadical")
    (version "1.9.5")
    (source
     (origin
       (method git-fetch)
       (patches (search-patches "patches/cadical-shared-library.patch"))
       (uri (git-reference
             (url "https://github.com/arminbiere/cadical")
             (commit (string-append "rel-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vxky42s6md6ys85jkm5p1nnryrx9hafdc5l8fqfqpx3qp7sw0lq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list (string-append "CXX="
                                          #$(cxx-for-target))
                           (string-append "LDFLAGS=-Wl,-rpath="
                                          (assoc-ref %outputs "out") "/lib"))
      #:phases #~(modify-phases %standard-phases
                   (replace 'configure
                     (lambda _
                       (invoke "./configure")))
                   (replace 'install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let* ((outdir (assoc-ref outputs "out"))
                              (incdir (string-append outdir "/include"))
                              (libdir (string-append outdir "/lib"))
                              (bindir (string-append outdir "/bin")))
                         (install-file "build/cadical" bindir)
                         (install-file "build/mobical" bindir)
                         (install-file "src/ccadical.h" incdir)
                         (install-file "src/cadical.hpp" incdir)
                         (install-file "build/libcadical.so" libdir)
                         (install-file "build/libcadical.so.0" libdir)
                         (install-file "build/libcadical.so.0.0.0" libdir)))))))
    (synopsis "A simplified satisfiability solver")
    (description
     "CaDiCaL is a solver for the boolean satisfiability problem which utilizes
@acronym{CDCL, Conflict-driven clause learning}. In comparison to existing solvers,
it supposed to be easy to modify and extend.")
    (home-page "https://github.com/arminbiere/cadical")
    (license license:expat)))

(define-public symfpu
  (let ((commit "c3acaf62b137c36aae5eb380f1d883bfa9095f60"))
    (package
      (name "symfpu")
      (version (git-version "20190518" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/martin-cs/symfpu")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0wfx11a1fb2fd6zvz5b1w9l4ypqbnqvmmijfxp0hjcrm7id19v52"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:phases #~(modify-phases %standard-phases
                     (delete 'configure)
                     (replace 'install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (let* ((outdir (assoc-ref outputs "out"))
                                (incdir (string-append outdir
                                                       "/include/symfpu"))
                                (libdir (string-append outdir "/lib"))

                                (coredir (string-append incdir "/core"))
                                (utilsdir (string-append incdir "/utils")))
                           (mkdir-p coredir)
                           (mkdir-p utilsdir)
                           (copy-recursively "core" coredir)
                           (copy-recursively "utils" utilsdir)
                           (delete-file (string-append coredir "/Makefile"))
                           (delete-file (string-append utilsdir "/Makefile"))
                           (mkdir-p (string-append libdir "/pkgconfig"))
                           (with-output-to-file (string-append libdir
                                                 "/pkgconfig/symfpu.pc")
                             (lambda _
                               (format #t
                                "prefix=~a~@
                                      exec_prefix=${prefix}~@
                                      includedir=${prefix}/include~@
                                      ~@
                                      ~@
                                      Name: symfpu~@
                                      Version: ~a~@
                                      Description: an implementation of IEEE-754 floating-point numbers~@
                                      Cflags: -I${includedir}~%"
                                outdir
                                #$version)))))))))
      (synopsis
       "A concrete and symbolic implementation of IEEE-754 floating-point numbers")
      (description
       "This library provides a C++ implementation of concrete and symbolic semantics
for floating point numbers as defined in IEEE Standard for Floating-Point Arithmetic (IEEE 754).")
      (home-page "https://github.com/martin-cs/symfpu")
      (license license:gpl3))))

(define-public bitwuzla
  (package
    (name "bitwuzla")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bitwuzla/bitwuzla")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qphw7cgq1y4bhnikafq6f57zg40yv13j867qbc0lbvmd7i5aiv4"))))
    (build-system meson-build-system)
    (arguments
     ;; TODO: Enable tests
     `(#:tests? #f
       #:configure-flags '("-Dtesting=enabled" "-Ddefault_library=shared"
                           "-Dkissat=true")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'make-git-optional
                    (lambda _
                      (substitute* "src/meson.build"
                        (("find_program\\('git'\\)")
                         "find_program('git', required: false)")))))))
    (native-inputs (list pkg-config
                         googletest
                         python
                         gmp
                         cadical
                         symfpu
                         kissat))
    (synopsis "SMT solver optimized for the theory of bit-vectors")
    (description
     "@acronym{SMT, Satisfiability Modulo Theories} solver for the
theories of fixed-size bit-vectors, floating-point arithmetic, arrays,
uninterpreted functions and their combinations.")
    (home-page "https://bitwuzla.github.io/")
    (license license:expat)))
