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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  option frags
;;

(defvar orglyth-html-enable-opiton t
  "When non-nil, enable recommended options in ox-html.")

(defvar orglyth-html-use-ftp t
  "When non-nil, remote root path will be setted
`orglyth-html-ftp-address':`orglyth-html-remote-root-path'.

When nil, remote root path will be setted `orglyth-html-remote-root-path'
`sshfs' (if there are not, install console) can mount ssh folder as normal drive.
OS X:
  > brew cask install osxfuse
  > brew install sshfs
  > mkdir sakura
  > sshfs conao3@conao3.sakura.ne.jp:/home/conao3/www/orglyth sakura
  >
  > diskutil unmount sakura    # unmount sshfs")

(defvar orglyth-html-compile-for-pc t
  "Non-nil means compile html for pc.")

(defvar orglyth-html-compile-for-amp nil
  "Non-nil means compile html for amp.")

(defvar orglyth-html-compile-for-mobile nil
  "Non-nil means compile html for mobile.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  custom variables
;;

(defvar orglyth-html-local-root-path "~/public_html/orglyth/"
  "orglyth-html sorce root path.")

(defvar orglyth-html-remote-root-path "~/www/orglyth/"
  "orglyth-html remote root path")

(defvar orglyth-html-ftp-address "/ftp:conao3@conao3.com"
  "orglyth-html ftp address")

(defvar orglyth-html-source-dir-name "src")
(defvar orglyth-html-pc-dir-name "archives")
(defvar orglyth-html-amp-dir-name "amp")
(defvar orglyth-html-mobile-dir-name "mobile")
(defvar orglyth-html-template-dir-name "parts")

;; local path
(defvar orglyth-html-local-sorce-path
  (concat orglyth-html-local-root-path orglyth-html-source-dir-name "/")
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

(defvar orglyth-html-template-parts-path
  (concat orglyth-html-local-root-path orglyth-html-template-dir-name "/")
  "orglyth-html template parts path.")

;; remote path
(defvar orglyth-html-remote-sorce-path
  (if orglyth-html-use-ftp
      (concat orglyth-html-ftp-address ":" orglyth-html-remote-root-path orglyth-html-source-dir-name "/")
    (concat orglyth-html-remote-root-path orglyth-html-source-dir-name "/"))
  "orglyth-html sorce path.")

(defvar orglyth-html-remote-pc-path
  (if orglyth-html-use-ftp
      (concat orglyth-html-ftp-address ":" orglyth-html-remote-root-path orglyth-html-pc-dir-name "/")
    (concat orglyth-html-remote-root-path orglyth-html-pc-dir-name "/"))
  "orglyth-html pc page path")

(defvar orglyth-html-remote-amp-path
  (if orglyth-html-use-ftp
      (concat orglyth-html-ftp-address ":" orglyth-html-remote-root-path orglyth-html-amp-dir-name "/")
    (concat orglyth-html-remote-root-path orglyth-html-amp-dir-name "/"))
  "orglyth-html amp page path")

(defvar orglyth-html-remote-mobile-path
  (if orglyth-html-use-ftp
      (concat orglyth-html-ftp-address ":" orglyth-html-remote-root-path orglyth-html-mobile-dir-name "/")
    (concat orglyth-html-remote-root-path orglyth-html-mobile-dir-name "/"))
  "orglyth-html mobile page path (not amp)")

(defvar orglyth-html-preamble-format '(("ja" "
<div align=right>
初稿: %d
</div>
<div align=right>
最終更新日: %C
</div>

<div align=right>
<a href=\"http://conao3.com/\">ホーム</a> |
<a href=\"index.html\">文書トップ</a> | <a href=\"sitemap.html\">目次</a>
</div>
")))

(defvar orglyth-html-postamble-format '(("ja" "
<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>")))

<<<<<<< HEAD
(defvar orglyth-html-footnotes-section "
<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>")

(defvar orglyth-html-default-org-option
  `("default"
    ;; https://orgmode.org/manual/Selecting-files.html#Selecting-files
    :base-directory ,orglyth-html-local-sorce-path
    :publishing-directory ,orglyth-html-local-pc-path
    :recursive t

    ;; https://orgmode.org/manual/Publishing-options.html#Publishing-options
    :language "ja"
    :html-checkbox-type unicode
    :html-doctype "html5"
    :html-footnotes-section ,orglyth-html-footnotes-section
    ;; :html-footnote-format
    :html-head-include-default-style nli
    :html-head-include-scripts nil
    :html-preamble-format ,orglyth-html-preamble-format
    :html-postamble-format ,orglyth-html-postamble-format
    
    :base-extension "org"
    :style ""
    :auto-sitemap t
    :sitemap-filename "index.org"
    :sitemap-title "Sitemap!!"
    :makeindex t
    :section-numbers t

    ;; https://orgmode.org/manual/Publishing-action.html#Publishing-action
    :publishing-function org-html-publish-to-html)
  "default options fot orglyth-html")

(defvar orglyth-html-default-resources-option
  `("default"
    :base-directory ,orglyth-html-local-sorce-path
    :publishing-directory ,orglyth-html-local-pc-path
    :recursive t
    :base-extension "jpg\\|gif\\|png\\|css"
    :publishing-function org-publish-attachment))

(defvar orglyth-html-default-index-option
  `("default"
    :base-directory ,orglyth-html-local-root-path
    :publishing-directory ,orglyth-html-local-root-path
    :base-extension "org"
    :preparation-function orglyth-html-create-root-index-org
    :publishing-function org-html-publish-to-html))

(orglyth-add-list-to-list 'org-publish-project-alist
                          ;; http://technohabits.blogspot.com/2013/01/org-modepublishingweb.html
                          `(
                            ;; 検証用
                            ,(cons "local-rootindex" (cdr orglyth-html-default-index-option))
                            ,(cons "local-orgfiles" (cdr orglyth-html-default-org-option))
                            ,(cons "local-resources" (cdr orglyth-html-default-resources-option))
                            ("local" :components ("local-rootindex" "local-orgfiles" "local-resources"))
                            
                            ;; 本番用
                            ,(cons "web-rootindex" (cdr orglyth-html-default-index-option))
                            ,(cons "web-orgfiles" (cdr orglyth-html-default-org-option))
                            ,(cons "web-resources" (cdr orglyth-html-default-resources-option))
                            ("website" :components ("web-rootindex" "web-orgfiles" "web-resources"))))

(when orglyth-html-enable-opiton
  (setq org-html-with-latex                 'mathjax
        org-html-htmlize-output-type        'css
        org-html-coding-system              'utf-8
        )
  (setq org-html-mathjax-options
        '((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML")
          (scale "100")
          (align "center")
          (font "TeX")
          (linebreaks "false")
          (autonumber "AMS")
          (indent "0em")
          (multlinewidth "85%")
          (tagindent ".8em")
          (tagside "right")))

  (setq org-html-mathjax-template
        "<script type=\"text/x-mathjax-config\">
    MathJax.Hub.Config({
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": { scale: %SCALE,
                        linebreaks: { automatic: \"%LINEBREAKS\" },
                        webFont: \"%FONT\"
                       },
        SVG: {scale: %SCALE,
              linebreaks: { automatic: \"%LINEBREAKS\" },
              font: \"%FONT\"},
        NativeMML: {scale: %SCALE},
        TeX: { equationNumbers: {autoNumber: \"%AUTONUMBER\"},
               MultLineWidth: \"%MULTLINEWIDTH\",
               TagSide: \"%TAGSIDE\",
               TagIndent: \"%TAGINDENT\"
             }
});
</script>
<script type=\"text/javascript\"
        src=\"%PATH\"></script>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  functions
;;

(defun orglyth-html-write-str (str filename)
  (with-temp-buffer
    (insert str)
    (write-region (point-min) (point-max) filename)))

(defun orglyth-html-publish-sitemap-around (func &rest args)
  "Advice around `org-publish-sitemap' as FUNC.
ARGS is argument.
Override `org-publish-sitemap' when publish site.
Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is `sitemap.org'"
  ;; (let ((sitemap-builder (org-publish-property :sitemap-function project)))
  ;;   (funcall sitemap-builder project sitemap-filename)))
  
  )

(advice-add #'org-publish-sitemap :around #'orglyth-html-publish-sitemap-around)
(advice-remove #'org-publish-sitemap #'orglyth-html-publish-sitemap-around)

(defun orglyth-html-create-root-index-org (plist)
  "create root index.org"
  (catch 'orglyth-error
    (message "create-root-index-org start!!")
    (message (or (plist-get plist :base-directory)
                 (progn (warn "not set :base-directory, exit create-root-index-org")
                        (throw 'orglyth-error "not set variable"))
                 ))
    (let* ((base-dir (or (plist-get plist :base-directory)
                         (progn (warn "not set :base-directory, exit create-root-index-org")
                                (throw 'orglyth-error "not set variable"))))
           (dest-dir (or (plist-get plist :publishing-directory)
                         (progn (warn "not set :publishing-directory, exit create-root-index-org")
                                (throw 'orglyth-error "not set variable"))))
           (title (or (plist-get plist :sitemap-title)))
           (filepath (concat base-dir "/index.org")))
      (orglyth-html-write-str (concat
                               "#+title"
                               title)
                              filepath))))

;; http://davidaventimiglia.com/blogging_with_emacs.html
(defun dav-org-publish-org-sitemap (project &optional sitemap-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (indent-str (make-string 2 ?\ ))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-title (or (plist-get project-plist :sitemap-title)
                            (concat "Sitemap for project " (car project))))
         (sitemap-style (or (plist-get project-plist :sitemap-style)
                            'tree))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         (ifn (file-name-nondirectory sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filename))))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link (file-relative-name file dir))
              (oldlocal localdir)
              ;; bind new variable prefix
              (prefix (concat
                       (format-time-string org-publish-sitemap-date-format (org-publish-find-date file))
                       " : ")))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename sitemap-filename)
                         (file-truename file))
            (if (eq sitemap-style 'list)
                (message "Generating list-style sitemap for %s" sitemap-title)
              (message "Generating tree-style sitemap for %s" sitemap-title)
              (setq localdir (concat (file-name-as-directory dir)
                                     (file-name-directory link)))
              (unless (string= localdir oldlocal)
                (if (string= localdir dir)
                    (setq indent-str (make-string 2 ?\ ))
                  (let ((subdirs
                         (split-string
                          (directory-file-name
                           (file-name-directory
                            (file-relative-name localdir dir))) "/"))
                        (subdir "")
                        (old-subdirs (split-string
                                      (file-relative-name oldlocal dir) "/")))
                    (setq indent-str (make-string 2 ?\ ))
                    (while (string= (car old-subdirs) (car subdirs))
                      (setq indent-str (concat indent-str (make-string 2 ?\ )))
                      (pop old-subdirs)
                      (pop subdirs))
                    (dolist (d subdirs)
                      (setq subdir (concat subdir d "/"))
                      (insert (concat indent-str
                                      " + "
                                      prefix  ;; insert prefix
                                      d "\n"))
                      (setq indent-str (make-string
                                        (+ (length indent-str) 2) ?\ )))))))
            ;; This is common to 'flat and 'tree
            (let ((entry
                   ;; Invoke new helper function
                   (dav-org-publish-format-file-entry
                    org-publish-sitemap-file-entry-format file project-plist))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              (cond ((string-match-p regexp entry)
                     (string-match regexp entry)
                     (insert (concat indent-str
                                     " + " (match-string 1 entry)
                                     prefix  ;; insert prefix
                                     "[[file:" link "]["
                                     (match-string 2 entry)
                                     "]]" (match-string 3 entry) "\n")))
                    (t
                     (insert (concat indent-str
                                     " + "
                                     prefix  ;; insert prefix
                                     "[[file:" link "]["
                                     entry
                                     "]]\n"))))))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(provide 'orglyth-html)
;;;orglyth-html.el ends here
