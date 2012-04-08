;;;; ne2wm-toggle.el --- buffer-toggling utilities for e2wm

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

(autoload 'ne2wm:toggle-ansi-term "ne2wm-toggle-shell" nil t)
(autoload 'ne2wm:toggle-shell "ne2wm-toggle-shell" nil t)
(autoload 'ne2wm:toggle-eshell "ne2wm-toggle-eshell" nil t)
(autoload 'ne2wm:toggle-sauron "ne2wm-toggle-sauron" nil t)

(defalias 'ne2wm:toggle-shell-ansi-term 'ne2wm:toggle-ansi-term)

(provide 'ne2wm-toggle)
;;; ne2wm-toggle.el ends here
