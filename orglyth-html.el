;;; orglyth.el ---                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Conao

;; Author: Conao
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

;; 

;;; Code:

;; variables

(require 'ox-html)

(defvar orglyth-html-root-path "~/public_html/orglyth-html/"
  "orglyth-html sorce root path.")

(defvar orglyth-html-sorce-path (concat orglyth-html-root-path "src/")
  "orglyth-html sorce path.")

(defvar orglyth-html-pc-path (concat orglyth-html-root-path "archives/")
  "orglyth-html pc page path")

(defvar orglyth-html-amp-path (concat orglyth-html-root-path "amp/")
  "orglyth-html amp page path")

(defvar orglyth-html-mobile-path (concat orglyth-html-root-path "mobile/")
  "orglyth-html mobile page path (not amp)")

(defvar orglyth-html-compile-for-pc t
  "Non-nil means compile html for pc.")

(defvar orglyth-html-compile-for-amp nil
  "Non-nil means compile html for amp.")

(defvar orglyth-html-compile-for-mobile nil
  "Non-nil means compile html for mobile.")

(defvar orglyth-html-template-parts-path (concat orglyth-html-root-path "parts/")
  "orglyth-html template parts path.")

(setq org-publish-project-alist
      ;; http://technohabits.blogspot.com/2013/01/org-modepublishingweb.html
      `(
        ;; 検証用
        ("local-orgfiles"
         :base-directory ,orglyth-html-sorce-path
         :publishing-directory orglyth-html-pc-path,orglyth-html-pc-path
         :style ""
         :section-numbers nil
         :table-of-contents nil)
        ("local-resources"
         :base-directory ,orglyth-html-sorce-path
         :publishing-directory ,orglyth-html-pc-path
         :base-extension "jpg\\|gif\\|png\\|css"
         :publishing-function org-publish-attachment)
        ("local" :components ("local-orgfiles" "local-resources"))
        
        ;; 本番用
        ("web-orgfiles"
         :base-directory "/home/hoge/mysite-src"
         :publishing-directory "/ftp:hoge@www.example.com:/"
         :style ""
         :section-numbers nil
         :table-of-contents nil)
        ("web-resources"
         :base-directory "/home/hoge/mysite-src"
         :publishing-directory "/ftp:hoge@www.example.com:/"
         :base-extension "jpg\\|gif\\|png\\|css"
         :publishing-function org-publish-attachment)
        ("website" :components ("web-orgfiles" "web-resources"))
        ))

;; functions

(provide 'orglyth-html)
;;;orglyth-html.el ends here
