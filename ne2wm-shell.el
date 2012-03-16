;;; ne2wm-shell.el --- shell utilities for e2wm

;; Copyright (C) 2012  Takafumi Arakaki

;; Author: Takafumi Arakaki
;; Keywords: tools, window manager

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'term nil t)

(require 'ne2wm-utils)


(defvar ne2wm:toggle-buffer-with-callbacks-next-buffer nil
  "[internal] A buffer local variable to store the next buffer to
go when `ne2wm:toggle-buffer-with-callbacks' is called in this
buffer.")
(make-variable-buffer-local 'ne2wm:toggle-buffer-with-callbacks-next-buffer)


(defun ne2wm:toggle-buffer-with-callbacks
  (target-name cb-init &optional cb-at-target)
  "Go to the buffer named TARGET-NAME or go back to the original buffer.

If the target buffer does not exist, the call back CB-INIT will be
called. This function must make the target buffer and switch to it.
The optional argument CB-AT-TARGET is a function to be called when
the target buffer is shown.

The original buffer is the buffer where this function is called and
stored in the variable `ne2wm:toggle-buffer-with-callbacks-next-buffer'
which is buffer local in the *target* buffer."
  (let* ((original-buffer (window-buffer))
         (original-name (buffer-name original-buffer)))
    (if (and target-name
             (equal target-name original-name))
        (e2wm:aif ne2wm:toggle-buffer-with-callbacks-next-buffer
            (ne2wm:pst-buffer-set-current-window it)
          (message "no next-buffer is set"))
      (unless (get-buffer target-name)
        (funcall cb-init))
      (ne2wm:pst-buffer-set-current-window target-name)
      (with-current-buffer target-name
        (setq ne2wm:toggle-buffer-with-callbacks-next-buffer original-buffer)
        (when cb-at-target (funcall cb-at-target))))))


(defun ne2wm:toggle-shell-ansi-term (&optional change-directory)
  "Toggle ansi-term in the current window.

When the prefix argument is given, current working directory in
the shell will be changed to the directory of the buffer."
  (interactive "P")
  (let ((target-buffer
         (when (boundp 'term-ansi-buffer-name)
           ;; `term-ansi-buffer-name' is actually a buffer; strange!
           (buffer-name term-ansi-buffer-name)))
        (cd-cmd (format "cd %s\n" default-directory)))
    (if (not target-buffer)
        (message "term-ansi-buffer-name is not set")
      (ne2wm:toggle-buffer-with-callbacks
       target-buffer
       (lambda () (ansi-term explicit-shell-file-name))
       (when change-directory
         (lambda () (term-send-raw-string cd-cmd)))))))


(provide 'ne2wm-shell)
;;; ne2wm-shell.el ends here
