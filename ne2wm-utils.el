;;; ne2wm-utils.el --- utility functions

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
(require 'e2wm-vcs)

(require 'ne2wm-pst-magit+)
(require 'ne2wm-pst-monky+)

;;; Commands

(defun ne2wm:toggle-sub (&optional move-buffer)
  "Toggle on/off of e2wm sub window.

If the universal prefix argument is given, the current buffer
will be shown in the next window."
  (interactive "P")
  (let ((prev-buf (current-buffer)))
    (e2wm:aif (e2wm:pst-window-toggle
               'sub t (e2wm:$pst-main (e2wm:pst-get-instance)))
        (when move-buffer
          (wlf:set-buffer (e2wm:pst-get-wm) it prev-buf)))))


(defun ne2wm:pst-update-windows-command ()
  "Reset window configurations.

This is an extend version of `e2wm:pst-update-windows-command'.

Additional feature:
* Move buffer shown in the main window to the top of the history list.
"
  (interactive)
  (let* ((wm (e2wm:pst-get-wm))
         (wname (e2wm:$pst-main (e2wm:pst-get-instance)))
         (buf (wlf:get-buffer wm wname))
         (curwname (wlf:get-window-name wm (selected-window))))
    (e2wm:history-add buf)
    (e2wm:pst-update-windows-command)
    (e2wm:pst-window-select curwname))) ; avoid changing window focus


(defun ne2wm:open-vcs ()
  "Open VCS perspective depending on the VCS used in the project.

Currently, only Magit (Git) and Monky (Mercurial) are supported."
  (interactive)
  (let ((dir default-directory))
    (cond
     ;; magit
     ((and dir (magit-get-top-dir dir))
      (ne2wm:dp-magit+))
     ;; monky
     ((condition-case err
          (monky-get-root-dir)
        (error nil))
      (ne2wm:dp-monky+))
     ;; FIXME: support SVN
     ;; otherwise
     (t
      (message "Not in VCS")))))


;;; Misc

(defun ne2wm:load-files (&optional regex dir)
  (let* ((dir (or dir (file-name-directory load-file-name)))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (mapc #'load files)))


(provide 'ne2wm-utils)
;;; ne2wm-utils.el ends here
