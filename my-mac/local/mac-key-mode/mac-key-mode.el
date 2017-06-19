;;; mac-key-mode.el --- provide mac-style key bindings

;;; Code:
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

    (define-key map [(control a)] 'mark-whole-buffer)

    (define-key map [(control shift g)] 'goto-line)

    (define-key map [(control f)] 'helm-swoop)

    (define-key map [(control z)] 'undo-tree-undo)
    (define-key map [(control shift z)] 'undo-tree-redo)

    (define-key map [(control up)] 'beginning-of-buffer)
    (define-key map [(control down)] 'end-of-buffer)
    (define-key map [(control left)] 'smarter-move-beginning-of-line)
    (define-key map [(control right)] 'end-of-visual-line)
    (define-key map [(control backspace)] 'backward-delete-line)
    (define-key map [(control delete)] 'kill-visual-line)
    (define-key map (kbd "C-,") 'pop-to-mark-command) ; TODO Maybe map

    ;; Doesn't work as I want it for now.
    (define-key map (kbd "<M-up>") 'move-line-up)
    (define-key map (kbd "<M-down>") 'move-line-down)

    (define-key map [(alt up)] 'backward-paragraph)
    (define-key map [(alt down)] 'forward-paragraph)
    (define-key map [(alt left)] 'left-word)
    (define-key map [(alt right)] 'right-word)

    ;; Deletion
    (define-key map [(alt backspace)] 'backward-delete-word)
    (define-key map [(alt delete)] 'forward-delete-word)

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
  :lighter ("" mac-key-mode-lighter)
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

(defun backward-delete-line (arg)
  "Delete ARG visual lines backward."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (smarter-move-beginning-of-line arg) (point)))))

;; https://www.emacswiki.org/emacs/BackwardDeleteWord
(defun forward-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word arg) (point)))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (forward-delete-word (- arg)))
(provide 'mac-key-mode)

;; https://www.emacswiki.org/emacs/MoveLine
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
;;; mac-key-mode.el ends here.
