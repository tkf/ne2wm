;;; ne2wm-toggle-shell.el --- shell/ansi-term utilities for e2wm

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

(require 'ne2wm-toggle-core)
(require 'term nil t)


;;;###autoload
(defun ne2wm:toggle-ansi-term (&optional change-directory)
  "Toggle ansi-term in the current window.

When the prefix argument is given, current working directory in
the shell will be changed to the directory of the buffer."
  (interactive "P")
  (let ((target-buffer
         (when (and (boundp 'term-ansi-buffer-name)
                    term-ansi-buffer-name)
           ;; `term-ansi-buffer-name' is actually a buffer; strange!
           (buffer-name term-ansi-buffer-name)))
        (cd-cmd (format "cd %s\n" default-directory)))
    (ne2wm:toggle-buffer-with-callbacks
     target-buffer
     (lambda () (ansi-term explicit-shell-file-name))
     (when change-directory
       (lambda () (term-send-raw-string cd-cmd))))))


;;;###autoload
(defun ne2wm:toggle-shell (&optional change-directory)
  "Toggle *shell* buffer in the current window.

When the prefix argument is given, current working directory in
the shell will be changed to the directory of the buffer."
  (interactive "P")
  (let ((target-buffer "*shell*")
        (cd-cmd (format "cd %s\n" default-directory)))
    (ne2wm:toggle-buffer-with-callbacks
     target-buffer
     #'shell
     (when change-directory
       (lambda ()
         (goto-char (point-max))
         (insert cd-cmd))))))


(provide 'ne2wm-toggle-shell)
;;; ne2wm-toggle-shell.el ends here
