;;; orglyth-latex.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@186-222.cup.hiroshima-u.ac.jp>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      Recommended settings below.
;;
;;
;; (use-package el-get :ensure t
;;   :if (or (executable-find "git")
;;           (message "'git' couldn't found.  el-get can't download any packages")
;;           (defmacro el-get (&rest arg) nil))
;;   :config
;;   (setq el-get-git-shallow-clone  t
;;         el-get-emacswiki-base-url "http://www.emacswiki.org/emacs/download/"))
;; 
;; (use-package orglyth
;;   :init (el-get-bundle conao/orglyth)
;;   :config
;;   (use-package orglyth-latex
;;     :config
;;     (setq orglyth-latex-enable-opiton t)
;;     (use-package ox-latex-subfigure
;;       :init (el-get-bundle linktohack/ox-latex-subfigure))))


;;; Code:

(require 'orglyth)
(require 'ox-latex)

(defgroup orglyth-latex nil
  "A orglyth-latex publish your latex file."
  :group 'orglyth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  option frags
;;

(defcustom orglyth-latex-enable-option nil
  "When non-nil, enable recommended options in ox-latex."
  :group 'orglyth-html
  :type 'boolean)

(defun orglyth-latex-init ()
  "Set various variables.  When `orglyth-latex-enable-option' is t."
  (if (not orglyth-latex-enable-option)
      (message "Nothing to be done. `org-lyth-latex-enable-option' is nil")
    
    (setq org-latex-default-class "org-jsarticle")

    (setq org-latex-hyperref-template
          "\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  pdfborder={0 0 0},
  colorlinks=false,
  linkcolor=blue
}
")

    (setq org-latex-pdf-process '("uplatex %f"
                                  "uplatex %f"
                                  "bibtex %b"
                                  "uplatex %f"
                                  "uplatex %f"
                                  "dvipdfmx %b.dvi"
                                  ;; "find . -type f -name '*.xbb' -print0 | xargs -0 rm"
                                  ))
    
    (orglyth-add-list-to-list 'org-latex-logfiles-extensions '("dvi" "bbl"))
    
    (orglyth-add-list-to-list 'org-latex-classes
                              '(("org-jsarticle"
                                 "\\documentclass[uplatex, dvipdfmx]{jsarticle}"
                                 ("\\section{%s}" . "\\section*{%s}")
                                 ("\\subsection{%s}" . "\\subsection*{%s}")
                                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                                
                                ("org-beamer"
                                 "\\documentclass[dvipdfmx,12pt]{beamer}"
                                 ("\\section{%s}" . "\\section*{%s}")
                                 ("\\subsection{%s}" . "\\subsection*{%s}")
                                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

    (orglyth-add-list-to-list
     'org-latex-packages-alist
     '(
       ;;;;;;;;;;;;;;;;;;;;
       ;; org depends default packeages
       
       ("utf8" "inputenc")       ;; enable unicode input
       ("T1" "fontenc")          ;; enable unicode output
       ("" "graphicx")           ;; insert figures
       ("" "grffile")            ;; enable strange filenames
       ("" "longtable")          ;; long table with page break
       ("" "wrapfig")            ;; text wrap figure
       ("" "rotating")           ;; text rotate
       ("normalem" "ulem")       ;; text decoration
       ("" "textcomp")           ;; symbol font
       ("" "capt-of")            ;; add caption at not float env
       ("" "hyperref")           ;; hyperlink
       ("" "amsmath, amssymb")   ;; math packages

       ;;;;;;;;;;;;;;;;;;;;
       ;; my optionnal packages
       
       ("" "pxjahyper")          ;; pdf bookmark label
       ("" "listings")           ;; code include
       ("" "fancyhdr")           ;; header, footer editing
       ("" "mdframed")           ;; framing
       ("" "here")               ;; figure put here
       ("" "lscape")             ;; landscape text, portrait page
       ("" "physics")            ;; math useful macros
       ("" "okumacro")           ;; useful macros by Dr.okumura
       ("" "framed")             ;; framing
       ("" "xcolor")             ;; pick color
       ("" "multicol")           ;; multi columns
       ("" "newtxtext")          ;; tx font
       ("" "newtxmath")          ;; tx math font
       ("" "geometry")           ;; page layout
       ("" "mathtools")          ;; enhance the appearance for amsmath
       ("" "subcaption")         ;; multiple figures
       "\\geometry{
top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot}"
       "\\pagestyle{fancy}"
       "\\rhead{\\thepage{}}"
       "\\mathtoolsset{showonlyrefs=true}"
       ))
    
    (when (executable-find "kpsewhich")
      ;; unicode code include
      (unless (string= (shell-command-to-string "kpsewhich jlisting.sty") "")
        (orglyth-add-list-to-list 'org-latex-packages-alist
                                  '(("" "jlisting")) t)
        (setq org-latex-listings         'listings
              org-latex-listings-options nil))
      
      (unless (string= (shell-command-to-string "kpsewhich listingsextra.sty") "")
        (orglyth-add-list-to-list 'org-latex-packages-alist
                                  '(("" "listingsextra")) t))
      
      (unless (string= (shell-command-to-string "kpsewhich listingssetup.sty") "")
        (orglyth-add-list-to-list 'org-latex-packages-alist
                                  '(("" "listingssetup")) t)))
    (orglyth-add-list-to-list 'org-latex-listings-langs '((shell "bash")
                                                          ))
    (message "orglyth-latex-init")))


(provide 'orglyth-latex)
;;; orglyth-latex.el ends here

