;;; ne2wm-pst-one+.el --- one+ perspective

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


(defvar ne2wm:c-one+-recipe
  '(- (:upper-size-ratio 0.7) main
      (- (:lower-size 1) sub org-clock)))


(defvar ne2wm:c-one+-winfo
  '((:name main)
    (:name sub :default-hide t)
    (:name org-clock :plugin org-clock)))


(e2wm:pst-class-register
 (make-e2wm:$pst-class
  :name   'one+
  :extend 'base
  :title  "One+"
  :init   'ne2wm:dp-one+-init
  :main   'main
  :switch 'ne2wm:dp-one+-switch
  :popup  'ne2wm:dp-one+-popup
  :keymap 'ne2wm:dp-one+-minor-mode-map))


(defun ne2wm:dp-one+-init ()
  (wlf:no-layout ne2wm:c-one+-recipe ne2wm:c-one+-winfo))


(defun ne2wm:dp-one+-switch (buf)
  (e2wm:message "#DP ONE+ switch : %s / %S"
                buf (e2wm:history-recordable-p buf))
  (cond
   ;; show on the main window
   ((e2wm:history-recordable-p buf)
    (e2wm:pst-show-history-main)
    t)
   (t nil)))


(defun ne2wm:dp-one+-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP ONE+ popup : %s (current %s / backup %s)"
                  buf cb e2wm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf)))
    (cond
     ((e2wm:history-recordable-p buf)
      (e2wm:pst-show-history-main)
      t)
     ((e2wm:document-buffer-p buf)
      (e2wm:pst-buffer-set 'main buf)
      t)
     (t
      (ne2wm:dp-one+-popup-sub buf)
      t))))


(defun ne2wm:dp-one+-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))


(defun ne2wm:dp-one+ ()
  (interactive)
  (e2wm:pst-change 'one+))


(defvar ne2wm:dp-one+-minor-mode-map nil)


(provide 'ne2wm-pst-one+)
;;; ne2wm-pst-one+.el ends here
