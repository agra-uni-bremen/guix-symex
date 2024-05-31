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
    (synopsis "Natural transformation for containers")
    (description
     "This package provides a natural transformation transforms a container
into another container.  Natural transformations act as functor morphisms in
category theory.")
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
    (synopsis "Friendly effect system for Haskell")
    (description
     "An implementation of an effect system for Haskell based on the works
of Oleg Kiselyov et al. on freer monads.")
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
     "Compatibility layer for Data.Array.Byte, providing boxed wrappers for
ByteArray and MutableByteArray as well as relevant instances for GHC < 9.4.")
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
    (synopsis "Monadic Elf parser for Haskell")
    (description "Monadic parser for the @acronym{ELF, Executable and Linkable
Format} object format.")
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
     "This package provides methods for fully evaluating data structures (deep
evaluation).  Deep evaluation is often used for adding strictness to a program,
e.g. in order to force pending exceptions, remove space leaks, or force lazy
I/O to happen.")
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
     "This library provides Hedgehog properties to ensure that typeclass
instances adhere to the set of laws that they are supposed to.")
    (license license:bsd-3)))

(define-public ghc-parameterized-utils
  (package
    (name "ghc-parameterized-utils")
    (version "2.1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "parameterized-utils" version))
       (sha256
        (base32 "026lrdnw5rjvny380rmrzpwhmcfgp551whbk0mnx2y2q6ij0sjfm"))))
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
    (home-page "https://github.com/GaloisInc/parameterized-utils")
    (synopsis
     "Classes and data structures for working with data-kind indexed types")
    (description
     "This package contains collection classes and type representations used
for working with values that have a single parameter.  It's intended for things
like expression libraries where one wishes to leverage the Haskell type-checker
to improve type-safety by encoding the object language type system into data
kinds.")
    (license license:bsd-3)))

(define-public ghc-libriscv
  (let ((commit "fb3a8b992622e04b5cbf70e3726e4627bf639a6f"))
    (package
      (name "ghc-libriscv")
      (version (git-version "20240521" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/agra-uni-bremen/libriscv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zdp9xyxv57i447ax44yadhmzwb8f9qf5cy4m207pjxl1phkamaj"))))
      (build-system haskell-build-system)
      (arguments
       (list
        #:tests? #f
        #:haddock? #f))
      (inputs (list ghc-template-haskell
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
  (let ((commit "3a824003edbea8fe86b67e52ff3bf6ddd1ccc8a3"))
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
          (base32 "107j3ajqzq7nf41aijxidar5qqb7sqnq9sn8mi5nnc48g8bii66j"))))
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
