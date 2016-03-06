;; TODO:
;; Create a directory which contains private layers
;; Add mac-mode into layer
;; It should include: mac-key-mode
;; Also require `redo+'
;; Makes use of
;; :commands in use-package creates auto-load references for commands
;; :mode keyword adds an entry to `auto-mode-alist' and an auto-load

;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

;; Use these key-bindings only on mac graphical display.
(when (display-graphic-p)
  (setq
   ;; ⌃ (Control) → Meta
   mac-control-modifier 'meta
   ;; ⌘ (Command) → Control
   mac-command-modifier 'control
   ;; Right ⌥ (Option) → Alt
   mac-right-option-modifier 'alt
   mac-right-control-modifier 'alt
   ;; Right ⌘ (Command) → super
   mac-right-command-modifier 'super
   ;; fn → Hyper
   mac-function-modifier 'hyper))

;; packages.el:
;; redo+
;; cua-mode

;; config.el:
;; misc congiuration

;; funcs.el
;; general utility functions

;; keybingings.el
;; define general keybindings

;; Additionally, for each local package (see the next section), there
;; should be a folder <layer>/local/<package>/ containing the source
;; code for that package. Before initializing that package, Spacemacs
;; will add this folder to the load path for you.

