;;; packages.el --- my-ibuffer layer packages file for Spacemacs.

; Author: https://github.com/TheBB/
(setq my-ibuffer-packages '(ibuffer))

(defun my-ibuffer/post-init-ibuffer ()
  (setq ibuffer-show-empty-filter-groups nil)
  (defun my-ibuffer-previous-line ()
    (interactive) (previous-line)
    (if (<= (line-number-at-pos) 2)
        (goto-line (- (count-lines (point-min) (point-max)) 2))))
  (defun my-ibuffer-next-line ()
    (interactive) (next-line)
    (if (> (line-number-at-pos) (- (count-lines (point-min) (point-max)) 2))
        (goto-line 3))))

(defun my-ibuffer/pre-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (setq ibuffer-saved-filter-groups 
          (quote (("Default"
                   ("Dired" (mode . dired-mode))
                   ("IRC" (mode . erc-mode))
                   ("Emacs" (or (name . "\\*Messages\\*")
                                (name . "\\*Compile-Log\\*")
                                (name . "\\*scratch\\*")
                                (name . "\\*spacemacs\\*")
                                (name . "\\*emacs\\*")))
                   ("Help" (name . "\\*Help\\*"))
                   ("Helm" (name . "\\*helm*"))
                   ))))
    (defvar my-ibuffer-collapsed-groups (list "Helm" "Emacs"))
    (defadvice ibuffer (after collapse-helm)
      "Collapses filter groups from the list `my-ibuffer-collapsed-groups'."
      (dolist (group my-ibuffer-collapsed-groups)
        (progn
          (goto-char 1)
          (when (search-forward (concat "[ " group " ]") (point-max) t)
            (progn
              (move-beginning-of-line nil)
              (ibuffer-toggle-filter-group))))))
    (ad-activate 'ibuffer)
    (define-key ibuffer-mode-map (kbd "<up>") 'my-ibuffer-previous-line)
    (define-key ibuffer-mode-map (kbd "<down>") 'my-ibuffer-next-line)    
    (add-hook 'ibuffer-mode-hook '(lambda ()
                                    (ibuffer-auto-mode 1)
                                    (ibuffer-switch-to-saved-filter-groups "Default")))))
