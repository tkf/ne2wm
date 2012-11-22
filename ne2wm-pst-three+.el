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
(eval-when-compile (defvar prev-selected-buffer))


(defvar ne2wm:c-three+-recipe
  '(| (:right-size 246)
      (| (:left-size 20)
         codethumb
         (- (:upper-size 7)
            org-clock
            (- imenu history )))
      (| (:left-size 163)
         (- (:lower-size 20) (| left right) sub) third)))


(defvar ne2wm:c-three+-winfo
  '((:name left)
    (:name right)
    (:name third)
    (:name imenu :plugin imenu)
    (:name history :plugin history-list+)
    (:name org-clock :plugin org-clock)
    (:name codethumb :plugin codethumb :default-hide t)
    (:name sub :default-hide t)))


(defvar ne2wm:dp-three+-minor-mode-map
  (e2wm:define-keymap
   '(("C-." . ne2wm:def-plugin-history-list+-forward-current-command)
     ("C-," . ne2wm:def-plugin-history-list+-back-current-command)
     ("C->" . ne2wm:def-plugin-history-list+-forward-other-command)
     ("C-<" . ne2wm:def-plugin-history-list+-back-other-command)
     ("M-\\" . ne2wm:win-ring-push)
     ("M-~"  . ne2wm:win-ring-rotate)
     ("M-_"  . ne2wm:same-buffer-in-next-window)
     ("prefix c" . ne2wm:dp-three+-toggle-codethumb))
     e2wm:prefix-key))


(e2wm:pst-class-register
 (let ((class (make-e2wm:$pst-class
               :name   'three+
               :extend 'two           ; two+ does not work here. why??
               :title  "Three"
               :init   'ne2wm:dp-three+-init
               :start  'ne2wm:dp-three+-start
               :main   'left
               :switch 'ne2wm:dp-three+-switch
               :popup  'ne2wm:dp-three+-popup
               :keymap 'ne2wm:dp-three+-minor-mode-map)))
   ;; Check if installed e2wm.el supports display method
   (when (fboundp 'e2wm:$pst-class-display)
     (setf (e2wm:$pst-class-display class) 'ne2wm:dp-three+-display))
   class))


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
  (let ((wins '(left right third)))
    (ne2wm:win-ring-set wins)
    (ne2wm:def-plugin-history-list+-setup wins '("<" ">" "v"))))


(defun ne2wm:dp-three+-switch (buf)
  (e2wm:message "#DP THREE+ switch : %s" buf)
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (cond
     ((ne2wm:dp-two+-switch buf)        ; `left' or `right' window
      t)
     ((eql curwin (wlf:get-window wm 'third)) ; `third' window
      (e2wm:pst-buffer-set 'third buf)
      (e2wm:plugin-exec-update-by-plugin-name curwin wm 'history-list+)
      t)
     (t nil))))


(defun ne2wm:dp-three+-popup (buf)
  (e2wm:message "#DP THREE+ popup : %s" buf)
  (let* ((buf-name (buffer-name buf))
         (wm (e2wm:pst-get-wm))
         (curwin (selected-window))
         (wname (and wm curwin (wlf:get-window-name wm curwin))))
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
      (e2wm:pst-update-windows)
      (let ((wname-other (case wname
                           (left  'right)
                           (right 'third)
                           (third 'right)
                           (t     'left))))
        (e2wm:pst-buffer-set wname-other buf t t))
      t)
     (t
      (e2wm:message ">>> t")
      (ne2wm:popup-sub-appropriate-select buf)
      t))))

(defun ne2wm:dp-three+-display (buf)
  (e2wm:message "#DP THREE+ display : %s" buf)
  (cond
   ((e2wm:document-buffer-p buf)
    (e2wm:pst-buffer-set 'third buf)
    t)
   ((e2wm:history-recordable-p buf)
    (let ((wm (e2wm:pst-get-wm))
          (curwin (selected-window)))
      ;; show in the other window, but don't select.
      (if (or (eql curwin (wlf:get-window wm 'left))
              (eql curwin (wlf:get-window wm 'third)))
          (e2wm:pst-buffer-set 'right buf)
        (e2wm:pst-buffer-set 'left buf)))
    t)
   ((let* ((wm (e2wm:pst-get-wm))
           (subwin (wlf:get-window wm 'sub)))
      (or (eq subwin (selected-window))
          (eq subwin (minibuffer-selected-window))))
    (e2wm:pst-buffer-set 'left buf)
    t)
   (t
    (e2wm:pst-buffer-set 'sub buf t)
    t)))


(defun ne2wm:dp-three+ ()
  (interactive)
  (e2wm:pst-change 'three+))


(defun ne2wm:dp-three+-toggle-codethumb ()
  (interactive)
  (wlf:toggle (e2wm:pst-get-wm) 'codethumb)
  (e2wm:plugin-exec-update-by-plugin-name
   (selected-frame) (e2wm:pst-get-wm) 'codethumb))


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
