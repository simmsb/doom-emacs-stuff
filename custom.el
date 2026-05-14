;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(jinx vterm))
 '(safe-local-variable-values
   '((lsp-rust-analyzer-cargo-extra-args
      . ["-Z" "build-std=core,alloc,panic_abort" "-Z"
         "build-std-features=optimize_for_size"])
     (lsp-rust-analyzer-cargo-extra-args quote
      ("-Z" "build-std=core,alloc,panic_abort" "-Z"
       "build-std-features=optimize_for_size"))
     (lsp-rust-analyzer-cargo-extra-args quote
      ("-Zbuild-std=core,alloc,panic_abort"
       "-Zbuild-std-features=optimize_for_size"))
     (langtool-default-language . "de-DE"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
