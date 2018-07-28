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

;; 

;;; Code:

(require 'orglyth)
(require 'org)
(require 'ox-latex)

(defgroup orglyth-latex nil
  "A orglyth-latex publish your latex file."
  :group 'orglyth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  option frags
;;

(defcustom orglyth-latex-enable-opiton nil
  "When non-nil, enable recommended options in ox-latex."
  :group 'orglyth-html
  :type 'boolean)

(when orglyth-latex-enable-opiton
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
  
  (add-list-to-list 'org-latex-logfiles-extensions '("dvi" "bbl"))
  
  (add-list-to-list 'org-latex-classes
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
  
  (when (executable-find "kpsewhich")
    ;; unicode code include
    (unless (string= (shell-command-to-string "kpsewhich jlisting.sty") "")
      (add-list-to-list 'org-latex-packages-alist
                        '(("" "jlisting")) t)
      (setq org-latex-listings         'listings
            org-latex-listings-options nil))
    
    (unless (string= (shell-command-to-string "kpsewhich listingsextra.sty") "")
      (add-list-to-list 'org-latex-packages-alist
                        '(("" "listingsextra")) t))
    
    (unless (string= (shell-command-to-string "kpsewhich listingssetup.sty") "")
      (add-list-to-list 'org-latex-packages-alist
                        '(("" "listingssetup")) t)))
  (add-list-to-list 'org-latex-listings-langs '((shell "bash")
                                                ))
  )


(provide 'orglyth-latex)
;;; orglyth-latex.el ends here

