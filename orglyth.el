;;; orglyth.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@Naoya-MacBook-Air.local>
;; Keywords:

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

;; depends on rejeep/f

;;; Code:

(require 'f)
(require 'org)

(defgroup orglyth nil
  "A orglyth settings org."
  :group 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  functions
;;

(defmacro orglyth-add-list-to-list (dest-lst source-lst &optional append)
  "Add to DEST-LST for SOURCE-LST in a destructive.
Defaltly, add at the beginning of the list, but when APPEND is non-nil,
SOURCE-LST is added at the end.
this function is minor change from `add-to-list'."
  `(progn
     (mapc (lambda (x)
             (add-to-list ,dest-lst x ,append))
           (if ,append ,source-lst (reverse ,source-lst)))
     ,dest-lst))

;; add :export keyword to #+include: syntax
;; see also `org-export-expand-include-keyword'
(defun orglyth-:export-include ())
(advice-add org-export-expand-include-keyword :before #'orglyth-:export-include)

(provide 'orglyth)
;;; orglyth.el ends here

