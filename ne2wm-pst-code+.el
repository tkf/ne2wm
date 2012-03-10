;;; ne2wm-pst-code+.el --- code+ perspective

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
  :name   'code+
  :extend 'code
  :title  "Code+"
  ;; workaround: e2wm only support 1st order inheritance
  :update 'e2wm:dp-base-update
  :popup  'ne2wm:dp-code+-popup))


(defun ne2wm:dp-code+-popup (buffer)
  (flet ((e2wm:dp-code-popup (b) (ne2wm:popup-sub-appropriate-select b)))
    (e2wm:$pst-class-super)))


(defun ne2wm:dp-code+ ()
  (interactive)
  (e2wm:pst-change 'code+))


(provide 'ne2wm-pst-code+)
;;; ne2wm-pst-code+.el ends here
