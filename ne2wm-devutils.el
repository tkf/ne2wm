;;; ne2wm-devutils.el --- development tools

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

(eval-when-compile (require 'cl))
(require 'e2wm)


(defun ne2wm:devutils-insert-slot-value (pst-class slot-name slot-format)
  (let ((slot (intern-soft (format slot-format slot-name))))
    (when slot
      (insert (format "  - %S: %S\n" slot-name (funcall slot pst-class))))))


(defun ne2wm:devutils-insert-pretty-pst-class (pst-class)
  (let ((name (e2wm:$pst-class-name pst-class)))
    (insert (format "%s:\n" name))
    (mapc (lambda (n)
            (if (not (eql n 'extend))
                (ne2wm:devutils-insert-slot-value pst-class n
                                                  "e2wm:$pst-class-%S")
              (insert (format "  - extend: %S\n"
                              (e2wm:aif (e2wm:$pst-class-extend pst-class)
                                  (e2wm:$pst-class-name it))))))
          '(title extend init main start update
                  switch popup leave keymap
                  save))))


(defun ne2wm:devutils-list-pst-classes ()
  (interactive)
  (let ((buffer (get-buffer-create "*ne2wm:devutils-list-pst-classes*")))
    (with-current-buffer buffer
      (erase-buffer)
      (mapc (lambda (c)
              (ne2wm:devutils-insert-pretty-pst-class c)
              (insert "\n"))
            e2wm:pst-list)
      (goto-char (point-min))
      (pop-to-buffer buffer))))


(defun ne2wm:devutils-list-plugins ()
  (interactive)
  (let ((buffer (get-buffer-create "*ne2wm:devutils-list-plugins*")))
    (with-current-buffer buffer
      (erase-buffer)
      (loop for p in e2wm:plugin-list
            for name = (e2wm:$plugin-name p)
            for title = (e2wm:$plugin-title p)
            for update = (e2wm:$plugin-update p)
            do (insert (format "\
%S:
  - title: %S
  - update: %S
" name title update))))
    (goto-char (point-min))
    (pop-to-buffer buffer)))


(provide 'ne2wm-devutils)
;;; ne2wm-devutils.el ends here
