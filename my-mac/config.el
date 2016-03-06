(cua-mode t)
(setq windmove-wrap-around t
      cua-keep-region-after-copy t)

;; Function available in railwaycat Emacs only:
;; `mac-auto-operator-composition-mode' automatically composes
;; consecutive occurrences of characters consisting of the elements
;; of `mac-auto-operator-composition-characters' if the font
;; supports such a composition.
(mac-auto-operator-composition-mode t)
