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

(defvar orglyth-html-local-root-path "~/public_html/orglyth/"
  "orglyth-html sorce root path.")

(defvar orglyth-html-remote-root-path "~/www/orglyth/"
  "orglyth-html remote root path")

(defvar orglyth-html-ftp-address "/ftp:conao3@conao3.com:"
  "orglyth-html ftp address")

(defvar orglyth-html-sorce-dir-name "src")
(defvar orglyth-html-pc-dir-name "archives")
(defvar orglyth-html-amp-dir-name "amp")
(defvar orglyth-html-mobile-dir-name "mobile")

;; local path
(defvar orglyth-html-local-sorce-path
  (concat orglyth-html-local-root-path orglyth-html-sorce-dir-name "/")
  "orglyth-html sorce path.")

(defvar orglyth-html-local-pc-path
  (concat orglyth-html-local-root-path orglyth-html-pc-dir-name "/")
  "orglyth-html pc page path")

(defvar orglyth-html-local-amp-path
  (concat orglyth-html-local-root-path orglyth-html-amp-dir-name "/")
  "orglyth-html amp page path")

(defvar orglyth-html-local-mobile-path
  (concat orglyth-html-local-root-path orglyth-html-mobile-dir-name "/")
  "orglyth-html mobile page path (not amp)")

;; remote path
(defvar orglyth-html-remote-sorce-path
  (concat orglyth-html-ftp-address orglyth-html-remote-root-path orglyth-html-sorce-dir-name "/")
  "orglyth-html sorce path.")

(defvar orglyth-html-remote-pc-path
  (concat orglyth-html-ftp-address orglyth-html-remote-root-path orglyth-html-pc-dir-name "/")
  "orglyth-html pc page path")

(defvar orglyth-html-remote-amp-path
  (concat orglyth-html-ftp-address orglyth-html-remote-root-path orglyth-html-amp-dir-name "/")
  "orglyth-html amp page path")

(defvar orglyth-html-remote-mobile-path
  (concat orglyth-html-ftp-address orglyth-html-remote-root-path orglyth-html-mobile-dir-name "/")
  "orglyth-html mobile page path (not amp)")


(defvar orglyth-html-compile-for-pc t
  "Non-nil means compile html for pc.")

(defvar orglyth-html-compile-for-amp nil
  "Non-nil means compile html for amp.")

(defvar orglyth-html-compile-for-mobile nil
  "Non-nil means compile html for mobile.")

(defvar orglyth-html-template-parts-path (concat orglyth-html-local-root-path "parts/")
  "orglyth-html template parts path.")

(add-list-to-list 'org-publish-project-alist
             ;; http://technohabits.blogspot.com/2013/01/org-modepublishingweb.html
             `(
               ;; 検証用
               ("local-orgfiles"
                :base-directory ,orglyth-html-local-sorce-path
                :publishing-directory ,orglyth-html-local-pc-path
                :recursive t
                :base-extension "org"
                :style ""
                :auto-sitemap t
                :sitemap-title "Sitemap!!"
                :section-numbers nil
                :table-of-contents nil
                :publishing-function org-html-publish-to-html)
               ("local-resources"
                :base-directory ,orglyth-html-local-sorce-path
                :publishing-directory ,orglyth-html-local-pc-path
                :recursive t
                :base-extension "jpg\\|gif\\|png\\|css"
                :publishing-function org-publish-attachment)
               ("local" :components ("local-orgfiles" "local-resources"))
               
               ;; 本番用
               ("web-orgfiles"
                :base-directory ,orglyth-html-local-sorce-path
                :publishing-directory ,orglyth-html-remote-pc-path
                :recursive t
                :base-extension "org"
                :style ""
                :auto-sitemap t
                :sitemap-title "Sitemap!!"
                :section-numbers nil
                :table-of-contents nil
                :publishing-function org-html-publish-to-html)
               ("web-resources"
                :base-directory ,orglyth-html-local-sorce-path
                :publishing-directory ,orglyth-html-remote-pc-path
                :recursive t
                :base-extension "jpg\\|gif\\|png\\|css"
                :publishing-function org-publish-attachment)
               ("website" :components ("web-orgfiles" "web-resources"))
               ))

;; functions

(provide 'orglyth-html)
;;;orglyth-html.el ends here
