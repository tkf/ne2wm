;;; ne2wm-pst-three+.el --- three+ perspective

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
(require 'ne2wm-plugin-history-list+)
(require 'ne2wm-pst-two+)


(defvar ne2wm:c-three+-recipe
  '(| (:left-size-ratio 0.22)
      (- (:upper-size 7)
         org-clock
         (- (- imenu history ) sub))
      (| (:left-size-ratio 0.666)
         (| (:left-size-ratio 0.5) left right) third)))


(defvar ne2wm:c-three+-winfo
  '((:name left)
    (:name right)
    (:name third)
    (:name imenu :plugin imenu)
    (:name history :plugin history-list+)
    (:name org-clock :plugin org-clock)
    (:name sub :default-hide t)))


(defvar ne2wm:dp-three+-minor-mode-map
  (e2wm:define-keymap
   '(("C-." . ne2wm:def-plugin-history-list+-forward-current-command)
     ("C-," . ne2wm:def-plugin-history-list+-back-current-command)
     ("C->" . ne2wm:def-plugin-history-list+-forward-other-command)
     ("C-<" . ne2wm:def-plugin-history-list+-back-other-command)
     ("M-\\" . ne2wm:win-ring-push)
     ("M-~"  . ne2wm:win-ring-rotate))
     e2wm:prefix-key))


(e2wm:pst-class-register
 (make-e2wm:$pst-class
  :name   'three+
  :extend 'base
  :title  "Three"
  :init   'ne2wm:dp-three+-init
  :start  'ne2wm:dp-three+-start
  :main   'left
  :switch 'ne2wm:dp-three+-switch
  :popup  'ne2wm:dp-three+-popup
  :keymap 'ne2wm:dp-three+-minor-mode-map))


(defun ne2wm:dp-three+-init ()
  (let*
      ((three+-wm
        (wlf:no-layout
         ne2wm:c-three+-recipe
         ne2wm:c-three+-winfo))
       (buf (or prev-selected-buffer
                (e2wm:history-get-main-buffer)))
       (buf2 (e2wm:history-get-prev buf))
       (buf3 (e2wm:history-get-prev buf2)))
    (wlf:set-buffer three+-wm 'left buf)
    (wlf:set-buffer three+-wm 'right buf2)
    (wlf:set-buffer three+-wm 'third buf3)
    three+-wm))


(defun ne2wm:dp-three+-start (wm-unused)
  (setq ne2wm:win-ring '(left right third))
  (ne2wm:def-plugin-history-list+-setup ne2wm:win-ring '("<" ">" "v")))


(defun ne2wm:dp-three+-switch/popup (buf)
  "Common part used in switch and popup"
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (cond
     ((ne2wm:vcs-status-buffer-p buf)
      (e2wm:pst-buffer-set 'left buf t t)
      (e2wm:plugin-exec-update-by-plugin-name curwin wm 'history-list+)
      t)
     ((ne2wm:vcs-log-buffer-p buf)
      (e2wm:pst-buffer-set 'third buf t t)
      (e2wm:plugin-exec-update-by-plugin-name curwin wm 'history-list+)
      t)
     ((ne2wm:howm-contents-buffer-p buf)
      (e2wm:pst-buffer-set 'right buf t)
      (e2wm:plugin-exec-update-by-plugin-name curwin wm 'history-list+)
      t)
     ((ne2wm:vcs-commit-buffer-p buf)
      (e2wm:pst-buffer-set 'right buf t)
      (e2wm:plugin-exec-update-by-plugin-name curwin wm 'history-list+)
      t)
     (t nil))))


(defun ne2wm:dp-three+-switch (buf)
  (e2wm:message "#DP THREE+ switch : %s" buf)
  (cond
   ((ne2wm:dp-three+-switch/popup buf)
    t)
   (t
    (unless (ne2wm:dp-two+-switch buf)
      (let ((wm (e2wm:pst-get-wm))
            (curwin (selected-window)))
        (cond
         ;; in the `third' window
         ((eql curwin (wlf:get-window wm 'third))
          (e2wm:pst-buffer-set 'third buf)
          (e2wm:plugin-exec-update-by-plugin-name curwin wm 'history-list+)
          t)
         ;; otherwise
         (t nil)))))))


(defun ne2wm:dp-three+-popup (buf)
  (e2wm:message "#DP THREE+ popup : %s" buf)
  (let ((buf-name (buffer-name buf))
        (wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (cond
     ;; Buffer specific configurations:
     ((ne2wm:dp-three+-switch/popup buf)
      t)
     ((equal "*info*" buf-name)
      (e2wm:message ">>> (equal \"*info*\" buf-name='%S')" buf-name)
      (e2wm:pst-buffer-set 'third buf t t)
      t)
     ;; More generic configurations:
     ((e2wm:document-buffer-p buf)
      (e2wm:message ">>> (e2wm:document-buffer-p buf='%S')" buf)
      (e2wm:pst-buffer-set 'third buf t t)
      t)
     ((e2wm:history-recordable-p buf)
      (e2wm:message ">>> (e2wm:history-recordable-p buf='%S')" buf)
      (cond
       ((eql curwin (wlf:get-window wm 'right))
        (e2wm:pst-show-history-main)
        (e2wm:pst-window-select-main)
        t)
       (t
        (e2wm:pst-update-windows)
        (e2wm:pst-buffer-set 'right buf t t)
        t))
      t)
     (t
      (e2wm:message ">>> t")
      (ne2wm:popup-sub-appropriate-select buf)
      t))))


(defun ne2wm:dp-three+ ()
  (interactive)
  (e2wm:pst-change 'three+))


(defun ne2wm:dp-three+-setup-two-columns ()
  "Setup helper function to use three+ like two+.

Call this function in your Emacs setup to use three+ in two
columns window configuration.  This is useful in small display.
Note that imenu plugin will not be shown."

  (setq
   ne2wm:c-three+-recipe
   '(- (:upper-size-ratio 0.8)
       (| (- (:upper-size-ratio 0.5)
             left (- (:upper-size-ratio)
                     third imenu))
          (- (:upper-size-ratio 0.8)
             right (- (:lower-size 0.05) history org-clock)))
       sub))

  (setq
   ne2wm:c-three+-winfo
   '((:name left)
     (:name right)
     (:name third)
     (:name imenu :plugin imenu :default-hide t)
     (:name history :plugin history-list+)
     (:name org-clock :plugin org-clock)
     (:name sub :default-hide t))))



(provide 'ne2wm-pst-three+)
;;; ne2wm-pst-three+.el ends here
