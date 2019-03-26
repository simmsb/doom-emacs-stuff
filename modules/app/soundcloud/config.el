;;; app/soundcliud/config.el -*- lexical-binding: t; -*-


(def-package! emms
  :config
  (emms-standard)
  (emms-default-players))


(def-package! soundcloud
  :after emms)
