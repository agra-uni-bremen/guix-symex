(define-module (symex packages)
  #:use-module ((gnu packages) #:prefix gnu:)
  #:use-module (srfi srfi-1)
  #:export (search-patches))

;; Version of search-patches with custom search-path.
(define-syntax-rule (search-patches file-name ...)
  (let ((pkgs (dirname (current-filename))))
    (parameterize ((gnu:%patch-path (list (string-append pkgs "/patches"))))
                  (list (gnu:search-patch file-name) ...))))
