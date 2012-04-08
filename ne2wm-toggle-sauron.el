;;; ne2wm-toggle-sauron.el --- sauron utilities for e2wm

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
(require 'sauron)


;;;###autoload
(defun ne2wm:toggle-sauron (&optional log-buffer)
  "Toggle eshell in the current window.

When the prefix argument is given, visit the *Sauron Log buffer*."
  (interactive "P")
  (let ((sauron-separate-frame nil)
        (target-buffer
         (let ((buffer-name (buffer-name)))
           (if (member buffer-name (list sr-log-buffer-name sr-buffer-name))
               buffer-name
             (if log-buffer sr-log-buffer-name sr-buffer-name)))))
    (ne2wm:toggle-buffer-with-callbacks
     target-buffer
     #'sauron-start)))


(provide 'ne2wm-toggle-sauron)
;;; ne2wm-toggle-sauron.el ends here
