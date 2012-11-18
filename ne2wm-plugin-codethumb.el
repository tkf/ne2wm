;;; ne2wm-plugin-codethumb.el --- Short summary

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ne2wm-plugin-codethumb.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; ne2wm-plugin-codethumb.el is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ne2wm-plugin-codethumb.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'e2wm)
(eval-when-compile (require 'codethumb nil t))
(declare-function codethumb:start-timer "codethumb")

(defun ne2wm:def-plugin-codethumb (_ wm winfo)
  (with-current-buffer (e2wm:history-get-main-buffer)
    (codethumb:start-timer))
  (wlf:set-buffer wm (wlf:window-name winfo) codethumb:buffer))

(e2wm:plugin-register 'codethumb
                      "CodeThumb"
                      'ne2wm:def-plugin-codethumb)

(provide 'ne2wm-plugin-codethumb)

;;; ne2wm-plugin-codethumb.el ends here
