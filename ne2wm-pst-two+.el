;;; ne2wm-pst-two+.el --- two+ perspective

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
(require 'ne2wm-buffer-p)


(defvar ne2wm:c-two+-recipe
  '(- (:upper-size-ratio 0.8)
      (| left
         (- (:upper-size-ratio 0.8)
            right (- (:lower-size 0.05) history org-clock)))
      sub))

(defvar ne2wm:c-two+-winfo
  '((:name left)
    (:name right)
    (:name org-clock :plugin org-clock)
    (:name sub :buffer "*Help*" :default-hide t)
    (:name history :plugin history-list2 :default-hide nil)))


(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'two+
   :extend 'two
   :title  "Two Columns+"
   :init   'ne2wm:dp-two+-init
   :switch 'ne2wm:dp-two+-switch
   :popup  'ne2wm:dp-two+-popup))


(defun ne2wm:dp-two+-init ()
  (let ((e2wm:c-two-recipe ne2wm:c-two+-recipe)
        (e2wm:c-two-winfo ne2wm:c-two+-winfo))
    (e2wm:$pst-class-super)))


(defun ne2wm:dp-two+-popup (buf)
  "Extended version of `e2wm:dp-two-popup'."
  (e2wm:message "#DP TWO+ popup : %s" buf)
  (let ((buf-name (buffer-name buf)))
    (cond
     ;; Buffer specific configurations:
     ((equal "*info*" buf-name)
      (e2wm:message ">>> (equal \"*info*\" buf-name='%S')" buf-name)
      (e2wm:pst-show-history-main)
      (e2wm:pst-window-select-main)
      t)
     ((or (ne2wm:vcs-status-buffer-p buf)
          (ne2wm:vcs-log-buffer-p buf))
      (e2wm:pst-buffer-set 'right buf t t)
      t)
     ((ne2wm:howm-contents-buffer-p buf)
      (e2wm:pst-buffer-set 'right buf t)
      t)
     ((ne2wm:vcs-commit-buffer-p buf)
      (e2wm:pst-buffer-set 'left buf t)
      t)
     ;; More generic configurations:
     ((e2wm:document-buffer-p buf)
      (e2wm:message ">>> (e2wm:document-buffer-p buf='%S')" buf)
      ;; (e2wm:history-add buf) ; it does e2wm:history-recordable-p inside
      (cond ;; If already opened in a window, open in that one.
       ((equal buf (e2wm:pst-buffer-get 'left))
        (e2wm:pst-buffer-set 'left buf t))
       ((equal buf (e2wm:pst-buffer-get 'right))
        (e2wm:pst-buffer-set 'right buf t))
       (t ;; open document in sub window
        (ne2wm:popup-sub-appropriate-select buf)))
      t)
     ((e2wm:history-recordable-p buf)
      (e2wm:message ">>> (e2wm:history-recordable-p buf='%S')" buf)
      (e2wm:pst-show-history-main)
      (e2wm:pst-window-select-main)
      t)
     (t
      (e2wm:message ">>> t")
      (ne2wm:popup-sub-appropriate-select buf)
      t))))


(defun ne2wm:dp-two+-switch (buf)
  "Extended version of `e2wm:dp-two-switch'.

This function treats howm windows.  To see how howm creates its
window, see `riffle-setup-window-configuration'."
  (e2wm:message "#DP TWO+ switch : %s" buf)
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window))
        (buf-name (buffer-name buf)))
    (cond
     ;; Buffer specific configurations:
     ((ne2wm:howm-summary-buffer-p buf)
      (e2wm:pst-buffer-set 'left buf t t)
      t)
     ((ne2wm:howm-contents-buffer-p buf)
      (e2wm:pst-buffer-set 'right buf t)
      t)
     ;; I'm in the left window
     ((eql curwin (wlf:get-window wm 'left))
      (cond
       ((eql (get-buffer buf) (wlf:get-buffer wm 'left))
        (e2wm:pst-update-windows)
        (e2wm:pst-buffer-set 'right buf)
        t)
       ((e2wm:history-recordable-p buf)
        (e2wm:pst-show-history-main)
        t)
       (t
        nil)))
     ;; I'm in the right window
     ((eql curwin (wlf:get-window wm 'right))
      (e2wm:pst-buffer-set 'right buf)
      (e2wm:dp-two-update-history-list)
      nil)
     ;; do the default otherwise
     (t nil))))


(defun ne2wm:dp-two+ ()
  (interactive)
  (e2wm:pst-change 'two+))


(provide 'ne2wm-pst-two+)
;;; ne2wm-pst-two+.el ends here
