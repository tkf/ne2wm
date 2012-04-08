;;; ne2wm-toggle-eshell.el --- eshell utilities for e2wm

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
(require 'eshell nil t)
(require 'em-prompt nil t) ; to suppress warning about `eshell-emit-prompt'


;;;###autoload
(defun ne2wm:toggle-eshell (&optional change-directory)
  "Toggle eshell in the current window.

When the prefix argument is given, current working directory in
the shell will be changed to the directory of the buffer."
  (interactive "P")
  (let ((target-buffer eshell-buffer-name)
        (cd-cmd (format "cd %s\n" default-directory)))
    (ne2wm:toggle-buffer-with-callbacks
     target-buffer
     #'eshell
     (when change-directory
       (lambda ()
         (eshell-interactive-print cd-cmd)
         (eshell-emit-prompt))))))


(provide 'ne2wm-toggle-eshell)
;;; ne2wm-toggle-eshell.el ends here
