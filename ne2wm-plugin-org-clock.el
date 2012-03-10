;;; ne2wm-plugin-org-clock.el --- org-clock plugin

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
(require 'org nil t)


(defun ne2wm:def-plugin-org-clock (frame wm winfo)
  "Show clocked-in task and its ancestors

This plugin provides two things:
* Show task name in its mode line.
  No buffer name, minor modes etc. to hide the task name.
* Show ancestors of the current task in the plugin buffer.
  It makes the context of the current task clear.
"
  (let ((wname (wlf:window-name winfo))
        (buf (get-buffer-create " *WM:Org clock*"))
        (selected-wname (wlf:get-window-name wm (selected-window))))
    (with-current-buffer buf
      (setq truncate-lines t)
      (setq mode-line-format 'global-mode-string))
    (wlf:set-buffer wm wname buf)
    ;; show org-clock window
    (if (and (fboundp 'org-clocking-p) (org-clocking-p))
        (progn
          (ne2wm:def-plugin-org-clock-insert-tree buf)
          (wlf:show wm wname))
      (wlf:hide wm wname))
    ;; focus goes to org-clock window w/o the following explicit (re)select
    (when selected-wname
      (wlf:select wm selected-wname))))

(e2wm:plugin-register 'org-clock
                      "Org clock"
                      'ne2wm:def-plugin-org-clock)

(defun ne2wm:def-plugin-org-clock-wname ()
  "Return t if current perspective uses org-clock plugin, nil otherwise"
  (loop with wm = (e2wm:pst-get-wm)
        for wname in (mapcar 'wlf:window-name (wlf:wset-winfo-list wm))
        if (eq 'org-clock (e2wm:pst-window-plugin-get wm wname))
        return wname))

(defun ne2wm:def-plugin-org-clock-show ()
  "Show org-clock plugin if available"
  (e2wm:aif (ne2wm:def-plugin-org-clock-wname)
      (let ((wm (e2wm:pst-get-wm))
            (wname it))
        (ne2wm:def-plugin-org-clock-insert-tree (wlf:get-buffer wm wname))
        (wlf:show wm wname))))

(defun ne2wm:def-plugin-org-clock-hide ()
  "Hide org-clock plugin"
  (e2wm:aif (ne2wm:def-plugin-org-clock-wname)
      (wlf:hide (e2wm:pst-get-wm) it)))

(add-hook 'org-clock-in-hook 'ne2wm:def-plugin-org-clock-show)
(add-hook 'org-clock-out-hook 'ne2wm:def-plugin-org-clock-hide)

(defun ne2wm:def-plugin-org-clock-show-hide ()
  "Show org-clock plugin if clocking a task, hide otherwise"
  (if (and (fboundp 'org-clocking-p) (org-clocking-p))
      (ne2wm:def-plugin-org-clock-show)
    (ne2wm:def-plugin-org-clock-hide)))

(defun ne2wm:def-plugin-org-clock-insert-tree (buf)
  (let ((nodes nil))
    (save-excursion
      (with-current-buffer (marker-buffer org-clock-marker)
        (goto-char org-clock-marker)
        (push (org-get-heading) nodes)
        (while (org-up-heading-safe)
          (push (org-get-heading) nodes))))
    (save-excursion
      (with-current-buffer buf
        (erase-buffer)
        (loop for head in nodes
              for n from 1
              for stars = (mapconcat 'identity
                                     (loop for i from 1 to n collect "*") "")
              do (insert stars " " head "\n"))))))


(provide 'ne2wm-plugin-org-clock)
;;; ne2wm-plugin-org-clock.el ends here
