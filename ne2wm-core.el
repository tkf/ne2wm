;;; ne2wm-core.el --- magit+ perspective

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

(defun ne2wm:apropos-recipe (recipe)
  "Convert RECIPE appropriately.
If RECIPE is a usual recursive list, RECIPE is returned as is.
If RECIPE is a callable function, it will be called without argument.
The function must return a valid recipe."
  (let ((arecipe
         (cond
          ((functionp recipe)
           (funcall recipe))
          (t recipe))))
    (assert (listp arecipe) nil
            "Cannot convert '%S' into a list. Got: '%S'."
            recipe arecipe)
    arecipe))


(provide 'ne2wm-core)
;;; ne2wm-core.el ends here
