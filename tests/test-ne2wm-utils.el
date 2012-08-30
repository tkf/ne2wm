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
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'b -1) 'a))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'c nil t) 'b))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'a nil t) 'd))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'd 2 t) 'b))
  (should (eql (ne2wm:find-next-in-seq '(a b c d) 'a -1 t) 'b)))

(ert-deftest ne2wm:find-next-in-seq2 ()
  (let ((seq '(0 1 2 3 4)))
    (should (equal (ne2wm:find-next-in-seq seq 1) 2))
    (should (equal (ne2wm:find-next-in-seq seq 4) 0))
    (should (equal (ne2wm:find-next-in-seq seq 1 1) 2))
    (should (equal (ne2wm:find-next-in-seq seq 4 1) 0))
    (should (equal (ne2wm:find-next-in-seq seq 1 0) 1))
    (should (equal (ne2wm:find-next-in-seq seq 4 0) 4))
    (should (equal (ne2wm:find-next-in-seq seq 1 2) 3))
    (should (equal (ne2wm:find-next-in-seq seq 4 2) 1))
    (should (equal (ne2wm:find-next-in-seq seq 3 2) 0))
    (should (equal (ne2wm:find-next-in-seq seq 1 -1) 0))
    (should (equal (ne2wm:find-next-in-seq seq 0 -1) 4))
    (should (equal (ne2wm:find-next-in-seq seq 4 -1) 3))))

(ert-deftest ne2wm:find-neighbor ()
  (should (eq (ne2wm:find-neighbor '(a b c d) 'a) 'b))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'b) 'c))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'c) 'd))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'd) 'c))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'x) 'c))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'a t) 'b))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'b t) 'a))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'c t) 'b))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'd t) 'c))
  (should (eq (ne2wm:find-neighbor '(a b c d) 'x t) 'b)))

(provide 'test-ne2wm-utils)

;;; test-ne2wm-utils.el ends here
