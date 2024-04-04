(define-module (symex packages klee)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages llvm))

(define-public klee-uclibc
  (let ((commit "955d502cc1f0688e82348304b053ad787056c754"))
    (package
      (name "klee-uclibc")
      (version (git-version "20230612" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/klee/klee-uclibc")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12fnr5mq80cxwvv09gi844mi31jgi8067swagxnlxlhxj4mi125j"))))
      (build-system gnu-build-system)
      ;; Only x86_64 is presently supported by KLEE upstream.
      (supported-systems '("x86_64-linux"))
      (arguments
       `(#:tests? #f ;upstream uClibc tests do not work in the fork
         #:phases (modify-phases %standard-phases
                    ;; Disable locales as these would have to be downloaded and
                    ;; shouldn't really be needed for symbolic execution either.
                    (add-after 'unpack 'patch-config
                      (lambda _
                        (substitute* "klee-premade-configs/x86_64/config"
                          (("UCLIBC_DOWNLOAD_PREGENERATED_LOCALE_DATA=y")
                           "UCLIBC_DOWNLOAD_PREGENERATED_LOCALE_DATA=n")
                          (("UCLIBC_PREGENERATED_LOCALE_DATA=y")
                           "UCLIBC_PREGENERATED_LOCALE_DATA=n")
                          (("UCLIBC_HAS_LOCALE=y")
                           "UCLIBC_HAS_LOCALE=n")
                          (("UCLIBC_HAS_XLOCALE=y")
                           "UCLIBC_HAS_XLOCALE=n"))))

                    ;; Upstream uses a custom non-GNU configure script written
                    ;; in Python, replace the default configure phase accordingly.
                    (replace 'configure
                      (lambda _
                        (invoke "./configure"
                                "--make-llvm-lib"
                                "--enable-release")))

                    ;; Custom install phase to only install the libc.a file manually.
                    ;; This is the only file which is used/needed by KLEE itself.
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (install-file "lib/libc.a"
                                      (string-append (assoc-ref outputs "out")
                                                     "/lib")))))))
      (inputs (list clang-toolchain-13))
      ;; ncurses is only needed for the `make menuconfig` interface.
      (native-inputs (list python ncurses))
      (synopsis "Variant of uClibc tailored to symbolic execution")
      (description
       "Modified version of uClibc for symbolic execution of
Unix userland software.  This library can only be used in conjunction
with the @code{klee} package.")
      (home-page "https://klee-se.org/")
      (license license:lgpl2.1))))

(define-public klee
  (package
    (name "klee")
    (version "3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/klee/klee")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0dj20nazkcq84ryr87dihvjznapsbl1n21sa8dhhnb0wsad5d6fb"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    ;; Only x86_64 is presently supported by KLEE upstream.
    (supported-systems '("x86_64-linux"))
    (arguments
     `(#:test-target "systemtests"
       ;; Default (RelWithDebInfo) build type causes an internal error
       ;; in clang while compiling KLEE, hence use a different build type.
       #:build-type "Release"
       #:strip-directories '("bin")
       #:configure-flags ,#~(list "-DENABLE_KLEE_ASSERTS=OFF"
                                  "-DENABLE_TCMALLOC=ON"
                                  "-DENABLE_POSIX_RUNTIME=ON"
                                  (string-append "-DKLEE_UCLIBC_PATH="
                                                 #$klee-uclibc))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-lit-config
                    (lambda _
                      ;; Make sure that we retain the value of the GUIX_PYTHONPATH
                      ;; environment variable in the test environmented created by
                      ;; python-lit. Otherwise, the test scripts won't be able to
                      ;; find the python-tabulate dependency, causing test failures.
                      (substitute* "test/lit.cfg"
                        (("addEnv\\('PWD'\\)" env)
                         (string-append env "\n" "addEnv('GUIX_PYTHONPATH')"))))))))
    ;; KLEE operates on LLVM IR generated by a specific toolchain version.
    ;; Propergate the toolchain to allow users to transform code to this version.
    (propagated-inputs (list clang-toolchain-13 llvm-13 python python-tabulate))
    (inputs (list z3 gperftools sqlite))
    (native-inputs (list python-lit))
    (synopsis
     "Symbolic execution engine built on top of the LLVM compiler infastructure")
    (description
     "Dynamic symbolic execution engine built on top of
LLVM.  Symbolic execution is an automated software testing technique,
KLEE leverage this technique to automatically generate test cases for
software compiled to LLVM IR.")
    (home-page "https://klee-se.org/")
    (license (list license:expat license:bsd-4))))
