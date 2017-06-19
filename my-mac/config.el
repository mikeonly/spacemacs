;; Enable default copy-pasting behaviour. 
(cua-mode t)
(setq cua-highlight-region-shift-only t)

(setq windmove-wrap-around t
      cua-keep-region-after-copy t)

;; Prevent clipboard buffer from cluttering by disabling copy-before killing behaviour. 
(setq save-interprogram-paste-before-kill nil)

