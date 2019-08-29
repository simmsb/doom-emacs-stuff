;;; app/soundcliud/config.el -*- lexical-binding: t; -*-


(use-package! emms
  :config
  (emms-standard)
  (emms-default-players))


(use-package! soundcloud
  :after emms)
