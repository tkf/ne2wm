;;; ne2wm-pst-magit+.el --- magit+ perspective

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

(require 'e2wm)
(require 'ne2wm-popwin)


(e2wm:pst-class-register
 (make-e2wm:$pst-class
  :name   'magit+
  :extend 'magit
  :title  "Magit+"
  :popup  'ne2wm:dp-magit+-popup))


(defun ne2wm:dp-magit+-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP MAGIT+ popup : %s (current %s / backup %s)"
                  buf cb e2wm:override-window-cfg-backup))
  (unless (e2wm:vcs-select-if-plugin buf)
    (let ((buf-name (buffer-name buf)))
      (e2wm:with-advice
       (cond
        ((equal buf-name magit-commit-buffer-name)
         ;; displaying commit objects in the main window
         (e2wm:pst-buffer-set 'main buf t nil))
        ((string-match "^\\*magit: .*\\*$" buf-name)
         ;; displaying status object in the status window
         (e2wm:pst-buffer-set 'status buf t t))
        ((buffer-file-name buf)
         ;; displaying file buffer in the main window
         (e2wm:pst-buffer-set 'main buf t t))
        (t
         ;; only the following line differs from `e2wm:dp-magit-popup'
         (ne2wm:popup-sub-appropriate-select buf)))))))


(defun ne2wm:dp-magit+ ()
  (interactive)
  (e2wm:pst-change 'magit+))


(provide 'ne2wm-pst-magit+)
;;; ne2wm-pst-magit+.el ends here
