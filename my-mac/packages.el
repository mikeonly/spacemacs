;;; packages.el --- my-mac layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: mikeonly <limestore@Gibbs.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst my-mac-packages
  '(
    exec-path-from-shell
    osx-trash
    (mac-key-mode :location local)
    comment-dwim-2
    ))

(defun my-mac/post-init-exec-path-from-shell ()
  ;; Use environmental variable in M-!
  (exec-path-from-shell-initialize)
  )

(defun my-mac/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
             (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))

(defun my-mac/init-mac-key-mode ()
  (use-package mac-key-mode
    :diminish mac-key-mode
    :config (mac-key-mode t)))

(defun my-mac/post-init-mac-key-mode ()
  (spacemacs/set-leader-keys-for-minor-mode 'mac-key-mode "iq" 'quoted-insert)
  (when (configuration-layer/package-usedp 'redo+)
    (define-key mac-key-mode-map [(control shift z)] 'redo))
  (when (configuration-layer/package-usedp 'comment-dwim-2)
    (define-key mac-key-mode-map [(control /)] 'comment-dwim-2))
  (dolist (cmd '(delete-word backward-delete-word))
    (put cmd 'CUA 'move)))

(defun my-mac/init-comment-dwim-2 ()
   (setq comment-dwim-2--inline-comment-behavior 'reindent-comment))

;;; packages.el ends here
