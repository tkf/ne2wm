;;; ne2wm-pst-vc-annotate.el --- a perspective to use vc-annotate command

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


(defvar ne2wm:c-vca-recipe
  '(- (:upper-size-ratio 0.7)
      (| main right)
      sub))

(defvar ne2wm:c-vca-winfo
  '((:name main)                 ; ":plugin vc-annotate" does not work
    (:name right :default-hide t)
    (:name sub :default-hide t)))

(defvar ne2wm:c-vca-show-main-regexp
  "\\*\\(Annotate .*\\)\\*")

(defvar ne2wm:c-vca-show-right-regexp
  "\\*\\(vc-diff\\)\\*")

(e2wm:pst-class-register
 (make-e2wm:$pst-class
  :name   'vca
  :extend 'base
  :title  "VC Annotate"
  :init   'ne2wm:dp-vca-init
  :start  'ne2wm:dp-vca-start
  :main   'main
  :switch 'ne2wm:dp-vca-switch
  :popup  'ne2wm:dp-vca-popup
  :leave  'ne2wm:dp-vca-leave
  :keymap 'ne2wm:dp-vca-minor-mode-map))

(defun ne2wm:dp-vca-init ()
  (wlf:no-layout ne2wm:c-vca-recipe ne2wm:c-vca-winfo))

(defun ne2wm:dp-vca-start (wm)
  (let* ((buf (or prev-selected-buffer
                  (e2wm:history-get-main-buffer))))
    (call-interactively 'vc-annotate)))

(defun ne2wm:dp-vca-leave (wm)
  (setq prev-selected-buffer nil))

(defun ne2wm:dp-vca-switch (buf)
  (e2wm:message "#DP VCA switch : %s / %S" buf (e2wm:history-recordable-p buf))
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm)))
    (cond
     ;; show on main (annotation)
     ((and ne2wm:c-vca-show-main-regexp
           (string-match ne2wm:c-vca-show-main-regexp buf-name))
      (e2wm:pst-buffer-set 'main buf t)
      t)
     ;; show on right and select
     ((or (and ne2wm:c-vca-show-right-regexp
               (string-match ne2wm:c-vca-show-right-regexp buf-name))
          (e2wm:history-recordable-p buf))
      (e2wm:pst-buffer-set 'right buf t t)
      t))))

(defun ne2wm:dp-vca-popup (buf)
  (let ((cb (current-buffer)))
    (e2wm:message "#DP VCA popup : %s (current %s / backup %s)"
                  buf cb e2wm:override-window-cfg-backup))
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm)))
    (cond
     ;; show on main (annotation)
     ((and ne2wm:c-vca-show-main-regexp
           (string-match ne2wm:c-vca-show-main-regexp buf-name))
      (e2wm:pst-buffer-set 'main buf t)
      t)
     ;; when in sub (borrowed from `e2wm:dp-code-popup')
     ((and e2wm:override-window-cfg-backup
           (eq (selected-window) (wlf:get-window wm 'sub)))
      (setq e2wm:override-window-cfg-backup nil)
      (set-window-buffer (wlf:get-window wm 'right) buf)
      t)
     ;; show on right and select
     ((or (and ne2wm:c-vca-show-right-regexp
               (string-match ne2wm:c-vca-show-right-regexp buf-name))
          (e2wm:history-recordable-p buf))
      (e2wm:pst-buffer-set 'right buf t t)
      t)
     ;; otherwise popup sub
     (t
      (ne2wm:dp-vca-popup-sub buf)
      t))))

(defun ne2wm:dp-vca-popup-sub (buf)
  (let ((wm (e2wm:pst-get-wm))
        (not-minibufp (= 0 (minibuffer-depth))))
    (e2wm:with-advice
     (e2wm:pst-buffer-set 'sub buf t not-minibufp))))

(defun ne2wm:dp-vca ()
  (interactive)
  (e2wm:pst-change 'vca))

(defun ne2wm:dp-vca-toggle-right-command ()
  (interactive)
  (e2wm:pst-window-toggle 'right t (e2wm:$pst-main (e2wm:pst-get-instance))))

(defvar ne2wm:dp-vca-minor-mode-map
  (e2wm:define-keymap
   '(("prefix r" . ne2wm:dp-vca-toggle-right-command)
     ) e2wm:prefix-key))


(provide 'ne2wm-pst-vc-annotate)
;;; ne2wm-pst-vc-annotate.el ends here
