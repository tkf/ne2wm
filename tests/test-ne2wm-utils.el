;;; test-ne2wm-utils.el --- Tests for ne2wm-utils.el

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; test-ne2wm-utils.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; test-ne2wm-utils.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with test-ne2wm-utils.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'ne2wm-utils)

(ert-deftest ne2wm:find-next-in-seq ()
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'b) 'c))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'd) 'a))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'b 2) 'd))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'b -1) 'a)))

(provide 'test-ne2wm-utils)

;;; test-ne2wm-utils.el ends here
