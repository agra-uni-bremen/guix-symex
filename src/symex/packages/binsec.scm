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
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages maths))

(define-public binsec-next
  (package
    (name "binsec-next")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/binsec/binsec")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07ks8dpizc2wr6250js7vh8cc1yvbpv7j0swyg66j1mqh1agdnbh"))))
    (build-system dune-build-system)
    (native-inputs (list gmp ocaml-qcheck ocaml-ounit2))
    (propagated-inputs (list dune-site
                             ocaml-base
                             ocaml-menhir
                             ocaml-graph
                             ocaml-zarith
                             ocaml-grain-dypgen
                             ocaml-toml
                             ocaml-z3
                             z3))
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
    (synopsis "Simplified CDCL-based satisfiability solver")
    (description
     "CaDiCaL is a solver for the boolean satisfiability problem which utilizes
@acronym{CDCL, Conflict-driven clause learning}.  In comparison to existing solvers,
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
