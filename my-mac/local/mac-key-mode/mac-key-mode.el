;;; mac-key-mode.el --- provide mac-style key bindings

(defgroup mac-key-mode nil
  "Mac-style key-binding mode."
  :group 'mac
  :version "24.5")
(defconst mac-key-mode-lighter
  "" ;; the Apple mark
  "A lighter string which is displayed in the modeline
  when `mac-key-mode' is on.")
(defcustom mac-key-mode-hook nil
  "The hook to run when mac-key-mode is toggled."
  :type 'hook
  :group 'mac-key-mode)

(defvar mac-key-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map [(control w)] 'delete-window)
    (define-key map [(control shift w)] 'delete-frame)
    (define-key map [(control shift n)] 'make-frame-command)

    (define-key map [(control s)] 'save-buffer)
    (define-key map [(control shift s)] 'write-file)
    (define-key map [(control q)] 'save-buffers-kill-emacs)

    (define-key map [(control a)] 'mark-whole-buffer)
    (define-key map [(control f)] 'isearch-forward)
    (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)

    (define-key map [(control meta f)] 'occur)

    (define-key map [(control shift g)] 'goto-line)

    (define-key map [(control up)] 'beginning-of-buffer)
    (define-key map [(control down)] 'end-of-buffer)
    (define-key map [(control left)] 'smarter-move-beginning-of-line)
    (define-key map [(control right)] 'end-of-visual-line)
    (define-key map [(control backspace)] 'backward-kill-visual-line)
    (define-key map [(control delete)] 'kill-visual-line)

    (define-key map [(alt up)] 'backward-paragraph)
    (define-key map [(alt down)] 'forward-paragraph)
    (define-key map [(alt left)] 'left-word)
    (define-key map [(alt right)] 'right-word)
    (define-key map [(alt backspace)] 'backward-kill-word)
    (define-key map [(alt delete)] 'forward-kill-word)

    (define-key map [(alt control up)] (my-ignore-error-wrapper 'windmove-up))
    (define-key map [(alt control down)] (my-ignore-error-wrapper 'windmove-down))
    (define-key map [(alt control right)] (my-ignore-error-wrapper 'windmove-right))
    (define-key map [(alt control left)] (my-ignore-error-wrapper 'windmove-left))

    (define-key map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
    ;; (define-key map [] 'scroll-up-command) ; It's taken by cua-mode
    map)
  "Keymap for `mac-key-mode'.")


;;;###autoload
(define-minor-mode mac-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on if arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'mac-key-mode
  :lighter (" " mac-key-mode-lighter)
  :keymap 'mac-key-mode-map)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun backward-kill-visual-line (arg)
  "Kill ARG visual lines backward."
  (interactive "p")
  (funcall (if visual-line-mode #'kill-visual-line #'kill-line)
           (- 1 arg)))

(provide 'mac-key-mode)
;;; mac-key-mode.el ends here.
