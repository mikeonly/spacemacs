;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/Dev/spacemacs/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables auto-completion-enable-sort-by-usage t)
     emacs-lisp
     extra-langs
     git
     (ibuffer :variables ibuffer-group-buffers-by nil)
     markdown
     ranger
     semantic
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'zsh)
     (spell-checking :variables syntax-checking-enable-by-default nil)
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)

     ;; Custom
     my-mac
     my-ibuffer
     my-font
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-excluded-packages
   '(
     arduino-mode
     julia-mode
     qml-mode
     scad-mode
     stan-mode
     )
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'emacs
   dotspacemacs-verbose-loading t
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 8
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "<kp-enter>"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "<enter>"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 1
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq-default

   ;; Miscellaneous
   frame-title-format '(buffer-file-name "%b" "%b")
   require-final-newline t
   vc-follow-symlinks t

   ;; Ranger
   ranger-override-dired t

   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil

   ;; Matlab
   matlab-auto-fill nil
   matlab-fill-code nil
   matlab-functions-have-end t
   matlab-indent-function-body nil

   ;; Shell
   shell-default-term-shell "/bin/zsh"
   ))

(defun dotspacemacs/user-config ()
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)
  (spaceline-toggle-hud-off)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
