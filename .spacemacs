;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional ths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-enable-lazy-installation 'nil
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/Developer/spacemacs-config/")
   dotspacemacs-configuration-layers
   '(
     colors
     javascript
     json
     (auto-completion :variables auto-completion-enable-sort-by-usage t)
     emacs-lisp
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-diff-side 'right)
     finance
     git
     github
     (ibuffer :variables ibuffer-group-buffers-by nil)
     markdown
     (latex :variables latex-enable-auto-fill t)
     python
     ranger
     ruby
     semantic
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'zsh)
     (spell-checking :variables syntax-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all
              )

     ;; Custom
     my-mac
     my-ibuffer
     )
   dotspacemacs-additional-packages '(hlinum)
   dotspacemacs-excluded-packages
   '(
     arduino-mode
     evil-ledger
     hledger-mode
     julia-mode
     qml-mode
     scad-mode
     stan-mode
     vi_tilde-fringe
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
   dotspacemacs-themes '(dracula)
   dotspacemacs-colorize-cursor-according-to-state nil
   dotspacemacs-default-font '("Input Mono Compressed"
                               :size 12
                               :weight extra-light
                               :width normal
                               :powerline-scale 1.2)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "<kp-enter>"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-major-mode-leader-key nil
   dotspacemacs-major-mode-emacs-leader-key "<f19>"
   dotspacemacs-distinguish-gui-tab nil
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
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers '(:enabled-for-modes
                               prog-mode
                               latex-mode
                               :relative nil
                               :disabled-for-modes dired-mode
                               doc-view-mode
                               markdown-mode
                               org-mode
                               pdf-view-mode)
   dotspacemacs-folding-method 'origami
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
   srecode-map-base-template-dir ".cache/"

   ;; Miscellaneous
   frame-title-format '(buffer-file-name "%b" "%b")
   require-final-newline t
   vc-follow-symlinks t

   ;; Ranger
   ranger-override-dired t

   ;; Smartparens
   sp-highlight-pair-overlay t
   sp-highlight-wrap-overlay t
   sp-highlight-wrap-tag-overlay nil

   ;; Matlab
   matlab-auto-fill nil
   matlab-fill-code nil
   matlab-functions-have-end t
   matlab-indent-function-body nil

   ;; Shell
   shell-default-term-shell "/bin/zsh")

  ;; Theming
  (setq theming-modifications
        '((dracula
           ;; Font locking
           (font-lock-comment-face :slant italic)
           (font-lock-string-face :slant italic)

           ;; Flycheck
           ;; (flycheck-fringe-error :background nil)
           ;; (flycheck-fringe-warning :background nil)
           ;; (flycheck-fringe-info :background nil)

           ;; Other
           (company-tooltip-annotation
            :foreground "#ff9eb8" :background "#49483e")
           (term :foreground nil :background nil)
           (ledger-font-xact-highlight-face :background "#393B4C")
           ;; (ledger-font-periodic-xact-face :inherit t
                                           ;; :background nil)

           ;; LaTeX
           (font-latex-math-face :foreground "#B3B9D6")
           (font-latex-script-char-face :foreground "#B3B9D6")

           ;; Perfect current-line and selection region combination.
           (hl-line :background "#303240")
           (linum-highlight-face :background "#44475a")
           (region :inherit t
                   :foreground nil
                   :background "#44475a")))))

(defun dotspacemacs/user-config ()
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)
  (spaceline-toggle-hud-off)
  (spacemacs/toggle-vi-tilde-fringe-off)

  ;; Git
  (setq magit-repository-directories '("~/Developer/"))
  (global-git-commit-mode t)

  ;; Make C-z always trigger undo action.
  (define-key evil-emacs-state-map (kbd "C-z") nil)

  ;; hlinum
  (hlinum-activate)

  ;; Programming 
  (add-hook 'prog-mode-hook 'rainbow-mode)

  ;; Ledger
  (use-package ledger-mode
    :mode ("\\.j\\'")
    :config
    (setq ledger-init-file-name "~/Google Drive/Finances/Private/meta.j")
    (setq ledger-binary-path "/usr/local/bin/hledger")
    (setq ledger-occur-use-face-shown nil))

  ;; Automatically surround with $ in LaTex-math-mode.
  ;; https://tex.stackexchange.com/a/148610/71800
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (let ((math (reverse (append LaTeX-math-list LaTeX-math-default))))
       (while math
         (let ((entry (car math))
               value)
           (setq math (cdr math))
           (if (listp (cdr entry))
               (setq value (nth 1 entry))
             (setq value (cdr entry)))
           (if (stringp value)
               (fset (intern (concat "LaTeX-math-" value))
                     (list 'lambda (list 'arg) (list 'interactive "*P")
                           (list 'LaTeX-math-insert value
                                 '(null (texmathp)))))))))))

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)))
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (setq TeX-view-program-list '(("/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-engine 'luatex))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (writeroom-mode visual-fill-column seeing-is-believing prettier-js helm-git-grep evil-ediff doom-modeline eldoc-eval shrink-path yasnippet-snippets yapfify xterm-color ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org symon string-inflection stickyfunc-enhance srefactor spaceline-all-the-icons smeargle shell-pop rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe restart-emacs rbenv ranger rake rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort popwin pippel pipenv pip-requirements persp-mode password-generator paradox overseer osx-trash origami org-plus-contrib org-bullets open-junk-file neotree nameless multi-term move-text mmm-mode minitest markdown-toc magithub magit-svn magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode link-hint ledger-mode json-navigator json-mode js2-refactor js-doc indent-guide importmagic ibuffer-projectile hungry-delete hlinum hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gitignore helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-ledger flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav editorconfig dumb-jump dracula-theme dotenv-mode diminish diff-hl define-word cython-mode counsel-projectile company-tern company-statistics company-auctex company-anaconda comment-dwim-2 column-enforce-mode color-identifiers-mode clean-aindent-mode chruby centered-cursor-mode bundler browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk aggressive-indent ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:foreground "#ff9eb8" :background "#49483e"))))
 '(font-latex-math-face ((t (:foreground "#B3B9D6"))))
 '(font-latex-script-char-face ((t (:foreground "#B3B9D6"))))
 '(font-latex-sectioning-0-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-1-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-2-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-3-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-4-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-5-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-slide-title-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-string-face ((t (:slant italic))))
 '(hl-line ((t (:background "#303240"))))
 '(info-title-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(ledger-font-xact-highlight-face ((t (:background "#393B4C"))))
 '(linum-highlight-face ((t (:background "#44475a"))))
 '(markdown-header-face ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-document-title ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-7 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-8 ((t (:inherit default :height 1.0 :weight bold))))
 '(region ((t (:inherit t :foreground nil :background "#44475a"))))
 '(term ((t (:foreground nil :background nil)))))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(package-selected-packages
   (quote
    (yapfify xterm-color ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org stickyfunc-enhance srefactor spaceline powerline smeargle shell-pop rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv ranger rake rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort popwin pip-requirements persp-mode pcre2el paradox spinner osx-trash origami orgit org-plus-contrib org-bullets open-junk-file neotree multi-term move-text mmm-mode minitest markdown-toc markdown-mode magit-gitflow magit-popup magit-gh-pulls macrostep lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint ledger-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc indent-guide ibuffer-projectile hydra lv hy-mode hungry-delete hlinum hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-ledger flycheck pkg-info epl flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit transient git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump diminish diff-hl define-word cython-mode company-tern dash-functional tern company-statistics company-auctex company-anaconda company comment-dwim-2 column-enforce-mode color-identifiers-mode coffee-mode clean-aindent-mode chruby bundler inf-ruby bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex-latexmk auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:foreground "#ff9eb8" :background "#49483e"))))
 '(font-latex-math-face ((t (:foreground "#B3B9D6"))))
 '(font-latex-script-char-face ((t (:foreground "#B3B9D6"))))
 '(font-latex-sectioning-0-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-1-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-2-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-3-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-4-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-5-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-slide-title-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-string-face ((t (:slant italic))))
 '(hl-line ((t (:background "#303240"))))
 '(info-title-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(ledger-font-xact-highlight-face ((t (:background "#393B4C"))))
 '(linum-highlight-face ((t (:background "#44475a"))))
 '(markdown-header-face ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-document-title ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-7 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-8 ((t (:inherit default :height 1.0 :weight bold))))
 '(region ((t (:inherit t :foreground nil :background "#44475a"))))
 '(term ((t (:foreground nil :background nil)))))
