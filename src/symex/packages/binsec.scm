(define-module (symex packages binsec)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system ocaml)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml))

(define-public ocaml-ISO8601
  (package
    (name "ocaml-ISO8601")
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
    (propagated-inputs (list ocaml-base ocaml-mdx ocaml-menhir ocaml-ISO8601))
    (synopsis "TOML library for OCaml")
    (description
     "This package provides an OCaml library for interacting
with files in the TOML configuration format.  Specifically, it provides
parser, a serializer and a printer for TOML.")
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
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/binsec/binsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j6lwj20jaq0702v2fqvsrax1400zqbvz5q2cmjqhvrjzcfcl0kr"))))
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
     "BINSEC is a toolset to help improve software security
at the binary level.  It relies on research in binary code analysis, at
the intersection of formal methods, program analysis, security and
software engineering.")
    (home-page "https://binsec.github.io/")
    (license license:lgpl2.1)))
