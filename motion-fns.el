;;; motion-fns.el --- motion and insertion functions

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Public domain.

;; $Id: motion-fns.el,v 1.5 2015/08/27 03:24:44 friedman Exp $

;;; Commentary:
;;; Code:

;; Internal variable, not a user option.
(defvar other-window-direction 1
  "Direction of motion for next repeated command `other-window-directionally'.")

;;;###autoload
(defun narrow-to-defun ()
  "Restrict editing in this buffer to the current defun
as defined by the functions `beginning-of-defun' and `end-of-defun'.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.
``\\[widen]'' makes all visible again.
See also `narrow-to-region'."
  (interactive)
  (save-excursion
    (let (end)
      (end-of-defun)
      (setq end (point))
      (beginning-of-defun)
      (narrow-to-region (point) end))))

;;;###autoload
(defun narrow-to-regexp (regexp)
  "Restrict editing in this buffer to the regular expression REGEXP.
The search for text matching the regular expression begins at point (not
from the beginning of the buffer), but the resulting narrowed region will
start at any following point matched by the regexp.  The regular expression
search is bounded by any narrowing currently in effect; to avoid this,
widen the buffer first.

See also: `widen', `narrow-to-region'"
  (interactive "sNarrow to regexp: ")
  (save-match-data
    (save-excursion
      (re-search-forward regexp))
    (widen)
    (narrow-to-region (match-beginning 0) (match-end 0))))

;;;###autoload
(defun narrow-to-sexp ()
  "Restrict editing in this buffer to the current sexp.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.
``\\[widen]'' makes all visible again.
See also `narrow-to-region'."
  (interactive)
  (let (end)
    (save-excursion
      ;; It's better to go forward before going back, since point might
      ;; already be at the beginning of the sexp.  If at end, we go on to
      ;; the next sexp.
      (forward-sexp-safe 1)
      (setq end (point))
      (forward-sexp-safe -1)
      (narrow-to-region (point) end))))


;; forward-sexp calls scan-sexps, which returns an error if it hits the
;; beginning or end of the sexp.
;;;###autoload
(defun forward-sexp-safe (&optional count)
  "Move forward across one balanced expression (sexp).
With argument, do it that many times.  Negative arg -COUNT means
move backward across COUNT balanced expressions.
Return distance in buffer moved, or nil."
  (or count (setq count 1))
  (condition-case errlist
      (- (- (point) (progn
                      (let ((parse-sexp-ignore-comments t))
                        (forward-sexp count))
                      (point))))
    (error
     (if (string= (car (cdr errlist))
                  "Containing expression ends prematurely")
         nil
       (error "%s" (car (cdr errlist)))))))

;;;###autoload
(defun goto-longest-line ()
  "Go to longest line in buffer."
  (interactive)
  (let ((longest-line 0)
        (line 0)
        (length 0))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq length (current-column))
      (setq longest-line 0)
      (while (zerop (forward-line 1))
        (setq line (1+ line))
        (end-of-line)
        (cond ((> (current-column) length)
               (setq length (current-column))
               (setq longest-line line)))))
    (goto-line (1+ longest-line))))

;; This used to be called move-to-column-force, but Emacs 20 defined that in rect.el.
;; Subsequently, move-to-column changed to take a 2nd argument to force.
;;;###autoload
(defun move-to-column-rigidly (column)
  "Sets point at column COLUMN on the current line, appending spaces if
end-of-line precedes desired column.  To just move to a column or end
of line (whichever comes first), use move-to-column instead."
  (interactive "NForce move to column: ")
  (let ((column-reached (move-to-column column)))
    (or (equal column column-reached)
        (insert-char ?  (- column column-reached))))
  column)

;;;###autoload
(defun other-window-directionally (&optional arg all-frames)
  "Select the ARG'th different window on this frame.
All windows on current frame are arranged in a cyclic order.
This command selects the window ARG steps away in that order.
A negative ARG moves in the opposite order.  ARG defaults to 1.

When called interactively, a universal prefix argument will cycle through
half the windows on the selected frame.

When called from a program, if the optional second argument ALL-FRAMES is
non-nil, cycle through all frames.

When called interactively repeatedly, continue selecting other windows in
the same direction as before, either forward or backward.  Thus, if this
command were bound to the key M-RET \(which is recommended for
convenience\), the sequence M-- M-RET M-RET would select the 2nd previous
window; the sequence M-- -2 M-RET M-RET would select the 3rd previous
window; and so on."
  (interactive "P")
  (cond ((not (interactive-p))
         ;; A non-interactive call does not affect
         ;; default direction for future calls
         (other-window (or arg 1) all-frames))
        ((consp arg)
         (setq other-window-direction 1)
         (other-window (/ (count-windows) 2)))
        ((eq '- arg)
         (other-window (setq other-window-direction -1)))
        ((numberp arg)
         (setq other-window-direction (if (< arg 0) -1 1))
         (other-window arg all-frames))
        ((eq this-command last-command)
         (other-window other-window-direction all-frames))
        ((null arg)
         (other-window (setq other-window-direction 1)))))

(provide 'motion-fns)

;;; motion-fns.el ends here.
