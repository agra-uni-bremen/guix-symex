(define-module (symex packages symex-vp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages maths))

(define-public symex-vp
  (let ((commit "63d0a5bd25a2ceff0dc71b3503edf4f1d3194b43"))
    (package
      (name "symex-vp")
      (version (git-version "20230703" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/agra-uni-bremen/symex-vp")
               (commit commit)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1mpcsaym9k0yjbgy50wah4g6i5mkgmb4yyzvgps8gnh3811h6wrj"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f
        #:phases #~(modify-phases %standard-phases
                     (add-before 'configure 'change-directory
                       (lambda _
                         (chdir "vp")))
                     (replace 'install
                       (lambda* (#:key outputs #:allow-other-keys)
                         (let* ((out (assoc-ref outputs "out"))
                                (bin (string-append out "/bin")))
                           (install-file "bin/symex-vp" bin)
                           (install-file "bin/hifive-vp" bin)))))))
      (inputs (list llvm boost z3))
      (synopsis "Symbolic execution engine for RISC-V embedded firmware")
      (description
       "Symbolically executes RISC-V firmware and supports SystemC peripherals")
      (home-page "https://agra.informatik.uni-bremen.de/projects/risc-v/")
      (license license:expat))))
