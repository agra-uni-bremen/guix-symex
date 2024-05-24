(define-module (symex packages binsym)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check))

(define-public ghc-natural-transformation
  (package
    (name "ghc-natural-transformation")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "natural-transformation" version))
       (sha256
        (base32 "1by8xwjc23l6pa9l4iv7zp82dykpll3vc3hgxk0pgva724n8xhma"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "natural-transformation")))
    (native-inputs (list ghc-quickcheck-instances ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("10"
                         "18d14fasp1l5xdfgp8swgcyyjd3irqj19cn298ksx9wiw43j818p")))
    (home-page "https://github.com/ku-fpg/natural-transformation")
    (synopsis "A natural transformation package.")
    (description
     "This package provides a natural transformation transforms a container @@f a@@
  into another container @@g a@@.  Natural transformations act as functor
  morphisms in category theory. .  The naming of ~>', :~> and $$ were taken, with
  permission, from Edward Kmett's @@indexed@@ package.")
    (license license:bsd-3)))

(define-public ghc-freer-simple
  (package
    (name "ghc-freer-simple")
    (version "1.2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "freer-simple" version))
       (sha256
        (base32 "11ypffdkpaxc03hlik6ymilhnk41fy7m92zzwqjma97g614vn0lw"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "freer-simple")))
    (inputs (list ghc-natural-transformation ghc-transformers-base))
    (native-inputs (list ghc-quickcheck ghc-tasty ghc-tasty-hunit
                         ghc-tasty-quickcheck))
    (home-page "https://github.com/lexi-lambda/freer-simple")
    (synopsis "A friendly effect system for Haskell.")
    (description
     "An implementation of an effect system for Haskell (a fork of
  <http://hackage.haskell.org/package/freer-effects freer-effects>), which is
  based on the work of Oleg Kiselyov et al.: . *
  <http://okmij.org/ftp/Haskell/extensible/more.pdf Freer Monads, More Extensible
  Effects> * <http://okmij.org/ftp/Haskell/zseq.pdf Reflection without Remorse> *
  <http://okmij.org/ftp/Haskell/extensible/exteff.pdf Extensible Effects> .  The
  key features are: . * An efficient effect system for Haskell - as a library! *
  Reimplementations of several common Haskell monad transformers as effects. *
  Core components for defining your own Effects.")
    (license license:bsd-3)))

(define-public ghc-data-array-byte
  (package
    (name "ghc-data-array-byte")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-array-byte" version))
       (sha256
        (base32 "002n0af7q08q3fmgsc5b47s1clirxy0lrqglwxzhabg0nfhfrdhv"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "data-array-byte")))
    (native-inputs (list ghc-quickcheck-classes-base ghc-tasty
                         ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision ("1"
                         "1nma7gz7lhain6jvwb3w3s53716ss8ypkk93gxpsaaz824svvw9f")))
    (home-page "https://github.com/Bodigrim/data-array-byte")
    (synopsis "Compatibility layer for Data.Array.Byte")
    (description
     "Compatibility layer for
  [Data.Array.Byte](https://hackage.haskell.org/package/base/docs/Data-Array-Byte.html),
  providing boxed wrappers for @@ByteArray#@@ and @@MutableByteArray#@@ and
  relevant instances for GHC < 9.4.  Include it into your Cabal file: . >
  build-depends: base > if impl(ghc < 9.4) > build-depends: data-array-byte .  and
  then @@import Data.Array.Byte@@ unconditionally.")
    (license license:bsd-3)))

(define-public ghc-melf
  (package
    (name "ghc-melf")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "melf" version))
       (sha256
        (base32 "0ivfzsw36qy0m93h353r3963vmhirzg3a5mirqn1hfbsk24xf1dx"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "melf")))
    (inputs (list ghc-lens ghc-prettyprinter ghc-optparse-applicative))
    (native-inputs (list ghc-tasty
                         ghc-tasty-golden
                         ghc-tasty-hunit
                         ghc-tasty
                         ghc-tasty-hunit
                         ghc-tasty
                         ghc-tasty-golden
                         ghc-tasty-hunit))
    (home-page "https://github.com/aleksey-makarov/melf")
    (synopsis "An Elf parser")
    (description "Parser for ELF object format")
    (license license:bsd-3)))

(define-public ghc-filepath
  (package
    (name "ghc-filepath")
    (version "1.4.100.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "filepath" version))
       (sha256
        (base32 "1bg9jr7nr6ki62d1srqvjlvrylq29zj8qi75kl7xybvw6i8651w2"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "filepath")))
    (native-inputs (list ghc-quickcheck ghc-quickcheck ghc-quickcheck
                         ghc-quickcheck ghc-quickcheck-classes-base))
    (home-page "https://github.com/haskell/filepath/blob/master/README.md")
    (synopsis "Library for manipulating FilePaths in a cross platform way.")
    (description
     "This package provides functionality for manipulating @@FilePath@@ values, and is
  shipped with <https://www.haskell.org/ghc/ GHC>.  It provides two variants for
  filepaths: .  1.  legacy filepaths: @@type FilePath = String@@ .  2.  operating
  system abstracted filepaths (@@OsPath@@): internally unpinned
  @@ShortByteString@@ (platform-dependent encoding) .  It is recommended to use
  @@OsPath@@ when possible, because it is more correct. .  For each variant there
  are three main modules: . * \"System.FilePath.Posix\" / \"System.OsPath.Posix\"
  manipulates POSIX\\/Linux style @@FilePath@@ values (with @@\\/@@ as the path
  separator). . * \"System.FilePath.Windows\" / \"System.OsPath.Windows\" manipulates
  Windows style @@FilePath@@ values (with either @@\\\\@@ or @@\\/@@ as the path
  separator, and deals with drives). . * \"System.FilePath\" / \"System.OsPath\" for
  dealing with current platform-specific filepaths . \"System.OsString\" is like
  \"System.OsPath\", but more general purpose.  Refer to the documentation of those
  modules for more information. .  An introduction into the new API can be found
  in this
  <https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html blog
  post>.  Code examples for the new API can be found
  <https://github.com/hasufell/filepath-examples here>.")
    (license license:bsd-3)))

(define-public ghc-bv
  (package
    (name "ghc-bv")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bv" version))
       (sha256
        (base32 "1nkvqwqcjl57p6ir0sllb54vbj6q0l3s3w7z3z2svxjq2ymqk884"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "bv")))
    (home-page "https://github.com/iagoabal/haskell-bv")
    (synopsis "Bit-vector arithmetic library")
    (description "Bit-vectors implemented as a thin wrapper over integers.")
    (license license:bsd-3)))

(define-public ghc-deepseq
  (package
    (name "ghc-deepseq")
    (version "1.4.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "deepseq" version))
       (sha256
        (base32 "0p8nmji6r9171mrmnnsm1x396pz6q0vks0afy475vny73i1rx1a7"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "deepseq")))
    (home-page "http://hackage.haskell.org/package/deepseq")
    (synopsis "Deep evaluation of data structures")
    (description
     "This package provides methods for fully evaluating data structures (\\\"deep
  evaluation\\\").  Deep evaluation is often used for adding strictness to a
  program, e.g. in order to force pending exceptions, remove space leaks, or force
  lazy I/O to happen.  It is also useful in parallel programs, to ensure pending
  work does not migrate to the wrong thread. .  The primary use of this package is
  via the deepseq function, a \\\"deep\\\" version of seq'.  It is implemented on top
  of an NFData typeclass (\\\"Normal Form Data\\\", data structures with no
  unevaluated components) which defines strategies for fully evaluating different
  data types.  See module documentation in \"Control.DeepSeq\" for more details.")
    (license license:bsd-3)))

(define-public ghc-hedgehog-classes
  (package
    (name "ghc-hedgehog-classes")
    (version "0.2.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "hedgehog-classes" version))
       (sha256
        (base32 "0z9ik5asddc2pnz430jsi1pyahkh6jy36ng0vwm7ywcq7cvhcvlz"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hedgehog-classes")))
    (inputs (list ghc-hedgehog
                  ghc-pretty-show
                  ghc-silently
                  ghc-wl-pprint-annotated
                  ghc-aeson
                  ghc-semirings
                  ghc-comonad
                  ghc-vector
                  ghc-primitive))
    (arguments
     `(#:cabal-revision ("1"
                         "06d6wjpg2dqip714az5w01kgz3rl4w8i61l09fb4mx79a43y6pa5")))
    (home-page "https://github.com/hedgehogqa/haskell-hedgehog-classes")
    (synopsis "Hedgehog will eat your typeclass bugs")
    (description
     "This library provides Hedgehog properties to ensure that typeclass instances
  adhere to the set of laws that they are supposed to.  There are other libraries
  that do similar things, such as `genvalidity-hspec` and `checkers`.  This
  library differs from other solutions by not introducing any new typeclasses that
  the user needs to learn, and otherwise minimal API overhead. .  This library is
  directly inspired by `quickcheck-classes`.")
    (license license:bsd-3)))

(define-public ghc-parameterized-utils
  (package
    (name "ghc-parameterized-utils")
    (version "2.1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "parameterized-utils" version))
       (sha256
        (base32 "118inzvvr72bfr1pzgxglrpd2fsz0kn9hk791imygl0fv1258rb6"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "parameterized-utils")))
    (inputs (list ghc-base-orphans
                  ghc-th-abstraction
                  ghc-constraints
                  ghc-hashable
                  ghc-hashtables
                  ghc-indexed-traversable
                  ghc-lens
                  ghc-profunctors
                  ghc-vector))
    (native-inputs (list ghc-hedgehog
                         ghc-tasty
                         ghc-tasty-ant-xml
                         ghc-tasty-hunit
                         ghc-tasty-hedgehog
                         ghc-hedgehog-classes))
    (arguments
     `(#:cabal-revision ("1"
                         "126p5f4craqwlzqpj0rbrnrl83ykvkb8w6lz3sg4m9d91sqixfrh")))
    (home-page "https://github.com/GaloisInc/parameterized-utils")
    (synopsis
     "Classes and data structures for working with data-kind indexed types")
    (description
     "This package contains collection classes and type representations used for
  working with values that have a single parameter.  It's intended for things like
  expression libraries where one wishes to leverage the Haskell type-checker to
  improve type-safety by encoding the object language type system into data kinds.")
    (license license:bsd-3)))

(define-public ghc-libriscv
  (let ((commit "d7560f0aea8a5726a0bc1938f762f38b063a61a4"))
    (package
      (name "ghc-libriscv")
      (version (git-version "20240521" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/agra-uni-bremen/libriscv.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zdp9xyxv57i447ax44yadhmzwb8f9qf5cy4m207pjxl1phkamaj"))))
      (build-system haskell-build-system)
      (arguments
       (list
        #:tests? #f
        #:haddock? #f))
      (inputs (list ghc-filepath
                    ghc-template-haskell
                    ghc-file-embed
                    ghc-extra
                    ghc-yaml
                    ghc-bv
                    ghc-exceptions
                    ghc-melf
                    ghc-freer-simple
                    ghc-transformers
                    ghc-optparse-applicative
                    ghc-parameterized-utils))
      (synopsis
       "Extensible implementation of the RISC-V ISA based on FreeMonads")
      (description "A Versatile and Flexible RISC-V Model")
      (home-page "https://github.com/agra-uni-bremen/libriscv")
      (license license:expat))))

(define-public ghc-z3
  (let ((commit "b77a17e5eeb7db82656bcbcd66c6e952207e69ca"))
    (package
      (name "ghc-z3")
      (version (git-version "20230604" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/IagoAbal/haskell-z3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11pabc9j52754adfdza8dcs710pcj3dfh5dnq045j952026255mj"))))
      (build-system haskell-build-system)
      (properties '((upstream-name . "z3")))
      (arguments
       (list
        #:tests? #f
        #:haddock? #f))
      (inputs (list ghc-transformers))
      (propagated-inputs (list z3))
      (home-page "https://github.com/IagoAbal/haskell-z3")
      (synopsis "Bindings for the Z3 Theorem Prover")
      (description "Unoffical Haskell bindings for the Z3 theorem prover")
      (license license:bsd-3))))

(define-public ghc-binsym
  (let ((commit "e0ad55fb6b256e95fbed97be62d85521a189414e"))
    (package
      (name "ghc-binsym")
      (version (git-version "20240524" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/agra-uni-bremen/binsym.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l3n1s43xpx46nsz8mjagp8wpyr8jfg54l44zxmxbxsmm2bzi4rg"))))
      (build-system haskell-build-system)
      (arguments
       (list
        #:tests? #f
        #:haddock? #f))
      (inputs (list ghc-libriscv
                    ghc-freer-simple
                    ghc-random
                    ghc-bv
                    ghc-z3
                    ghc-optparse-applicative))
      (synopsis "Symbolic execution for RISC-V machine code")
      (description
       "Symbolic execution for RISC-V machine code based on the formal LibRISCV ISA model")
      (home-page "https://github.com/agra-uni-bremen/BinSym")
      (license license:expat))))
