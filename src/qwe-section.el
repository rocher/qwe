;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-section.el}
;;.cfg.prj.file.desc      Section numbering.
;;.cfg.prj.compile        [[elisp:(byte-compile-file (buffer-file-name))][byte-compile-file]]
;;.cfg.prj.copyright      Copyright (C) 2004-2020  Francesc Rocher
;;.cfg.prj.license        GPL v3
;;.cfg.prj.license++
;;.ver This file is part of QWE.
;;.ver
;;.ver QWE is free software: you can redistribute it and/or modify
;;.ver it under the terms of the GNU General Public License as published by
;;.ver the Free Software Foundation, either version 3 of the License, or
;;.ver (at your option) any later version.
;;.ver
;;.ver QWE is distributed in the hope that it will be useful,
;;.ver but WITHOUT ANY WARRANTY; without even the implied warranty of
;;.ver MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;.ver GNU General Public License for more details.
;;.ver
;;.ver You should have received a copy of the GNU General Public License
;;.ver along with QWE.  If not, see <http://www.gnu.org/licenses/>.
;;.cfg.prj.license--
;;.cfg.doc.parent qwe.el
;;.cfg.doc.style  book
;;.cfg.header.filename ../header.qwe
;;.cfg.header.end [[qwe:hdr-update][update]]
;;.cmt _____________________________________________________________________________
;;.cmt
;;.todo THIS FILE NEEDS A DEEP REFACTORING
;;.cmt
;;.chp 5. Section Numbering
;;.cmt
;;.quo                                  Most project documents are best
;;.quo                        organized as sequential yet well-structured
;;.quo                        text. This begins with well-chosen chapters and
;;.quo                        sections, but may well extend to using textual
;;.quo                        building blocks consistently throughout a
;;.quo                        document.
;;.quo
;;.quo                                  {i/Andreas Ruping}
;;.quo                                  Agile Documentation
;;.quo                                  John Wiley, 2003
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       5. Section Numbering
;;
;;.toc.sec             5.1. Introduction
;;.toc.sse                   5.1.1. Module Dependencies
;;.toc.sse                   5.1.2. Debug Functions
;;.toc.sse                   5.1.3. External Functions
;;.toc.sse                   5.1.4. Private Interface
;;
;;.toc.sec             5.2. Constants
;;
;;.toc.sec             5.3. Variables
;;.toc.sse                   5.3.1. Custom Variables
;;.toc.sss                         5.3.1.1. qwe-section-numbering-custom-list
;;
;;.toc.sec             5.4. Section Management
;;.toc.sse                   5.4.1. qwe-section-insert
;;.toc.sse                   5.4.2. Helper functions
;;.toc.sss                         5.4.2.1. qwe-section-insert-chapter
;;.toc.sss                         5.4.2.2. qwe-section-insert-section
;;.toc.sss                         5.4.2.3. qwe-section-insert-subsection
;;.toc.sss                         5.4.2.4. qwe-section-insert-subsubsection
;;.toc.sss                         5.4.2.5. qwe-section-insert-paragraph
;;.toc.sse                   5.4.3. qwe-section-next
;;.toc.sse                   5.4.4. qwe-section-prev
;;
;;.toc.sec             5.5. Section Numbering
;;.toc.sse                   5.5.1. qwe-section-renumber
;;.toc.sse                   5.5.2. qwe-section-delete-numbers
;;
;;.toc.sec             5.6. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec 5.1. Introduction
;;
;;.sse 5.1.1. Module Dependencies

(eval-when-compile
  (require 'cl)
  (require 'qwe-lang))

;;.sse 5.1.2. Debug Functions
;; Eval these forms to enable debugging:

;;    [[elisp:][(setq warning-minimum-log-level :debug)]]
;;    [[elisp:][(setq warning-minimum-level :debug)]]
;;    [[elisp:][(qwe-debug-add-type 'qwe-sec)]]

;;.sse 5.1.3. External Functions

(defalias 'qwe-section--number-to-list 'version-to-list)

;;.sse 5.1.4. Private Interface

(defun qwe-section--list-p (l)
  (and (listp l)
       (let ((ok t))
         (dolist (i l ok)
           (unless
               (member i '(chapter section subsection subsubsection paragraph))
             (setq ok nil))))))

(defun qwe-section--get-number-list ()
  (let ((number-list nil))
        (save-excursion
          (while (looking-at "[ \t]")
            (forward-char 1))
          (when (looking-at "\\([0-9]+\\.\\)+[ \t]+[^ \t]+")
            (re-search-forward "\\([0-9]+\\.\\)+")
            (setq number-list
                  (qwe-section--number-to-list (match-string-no-properties 0))))
          (when (looking-at "[A-Z]\\.\\([0-9]+\\.\\)*[ \t]+[^ \t]+")
            (re-search-forward "\\([A-Z]\\)\\.\\(\\(?:[0-9]+\\.\\)*\\)")
            (setq number-list
                  `(,(match-string-no-properties 1)
                    ,@(qwe-section--number-to-list (match-string-no-properties 2))))))
        number-list))

(defun qwe-section--delete-this-number ()
  (while (looking-at "[ \t]")
    (delete-char 1))
  (if (looking-at "[0-9A-Z\\.]+\\.[ \t]+[^ \t]+")
      (progn
        (while (looking-at "[^ \t]")
          (delete-char 1))
        (while (looking-at "[ \t]")
          (delete-char 1)))))

(defun qwe-section--get-str (section-type)
  (when (symbolp section-type)
    (car (rassoc section-type qwe-section-str-type-alist))))

(defun qwe-section--get-str-from-string (section-string)
  (when (stringp section-string)
    (car (rassoc section-string qwe-section-str-string-alist))))

(defun qwe-section--get-string (section-type)
  (when (symbolp section-type)
    (car (rassoc section-type qwe-section-string-type-alist))))

(defun qwe-section--get-string-from-str (section-str)
  (when (stringp section-str)
    (cdr (assoc section-str qwe-section-str-string-alist))))

(defun qwe-section--get-type (section-str)
  (when (stringp section-str)
    (cdr (assoc section-str qwe-section-str-type-alist))))

(defun qwe-section--goto (section &optional backward)
  (let ((section-str nil)
        (found nil))
    (when (and (symbolp section)
               (setq section-str (qwe-section--get-str section)))
      (setq section-str (concat (qwe-doc-delimiter qwe-doc) "." section-str)))
    (when (and (stringp section)
               (qwe-section--get-type section))
      (setq section-str (concat (qwe-doc-delimiter qwe-doc) "." section)))
    (when section-str
      (save-excursion
        (if backward
            ;; {e/Backward}
            (progn
              (goto-char (point-at-bol))
              (setq found (re-search-backward (concat section-str "\\>") nil t)))
          ;; {e/Foward}
          (progn
            (goto-char (point-at-eol))
            (setq found (re-search-forward (concat section-str "\\>") nil t))))))
    (when found
      (goto-char found)
      (skip-chars-forward section-str)
      (when (= (char-after) 32)
        (forward-char)))
    found))

(defun qwe-section--decimal-to-roman (decimal &optional lowercase unicode)
  "Converts to roman any integer between 1 and 4999."
  (let ((roman "")
        (unitas (if unicode
                    [ "Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" ]
                  [ "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX" ]))
        (decimum (if unicode
                     [ "Ⅹ" "ⅩⅩ" "ⅩⅩⅩ" "ⅩⅬ" "Ⅼ" "ⅬⅩ" "ⅬⅩⅩ" "ⅬⅩⅩⅩ" "ⅩⅭ" ]
                   [ "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC" ]))
        (centiens (if unicode
                      [ "Ⅽ" "ⅭⅭ" "ⅭⅭⅭ" "ⅭⅮ" "Ⅾ" "ⅮⅭ" "ⅮⅭⅭ" "ⅮⅭⅭⅭ" "ⅭⅯ" ]
                    [ "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM" ])))
    (when (>= decimal 1000)
      (setq roman (make-string (/ decimal 1000) ?M))
      (setq decimal (- decimal (* 1000 (/ decimal 1000)))))
    (when (>= decimal 100)
      (setq roman (concat roman (elt centiens (1- (/ decimal 100)))))
      (setq decimal (- decimal (* 100 (/ decimal 100)))))
    (when (>= decimal 10)
      (setq roman (concat roman (elt decimum (1- (/ decimal 10)))))
      (setq decimal (- decimal (* 10 (/ decimal 10)))))
    (when (>= decimal 1)
      (setq roman (concat roman (elt unitas (1- decimal)))))
    (if lowercase
        (downcase roman)
      roman)))


;;.sec 5.2. Constants

(defconst qwe-section-type-list
  ;;.todo add part
  '(chapter section subsection subsubsection paragraph)
  "List of section type names.")

(defconst qwe-section-str-list
  ;;.todo add "prt"
  '("chp" "sec" "sse" "sss" "par")
  "List of abbrevations used to create sections.")

(defconst qwe-section-string-list
  ;;.todo add "part"
  '("chapter" "section" "subsection" "subsubsection" "paragraph")
  "List of strings used to create sections.")

(defconst qwe-section-str-string-alist
  '(;;.todo ("prt" . "part")
    ("chp" . "chapter")
    ("sec" . "section")
    ("sse" . "subsection")
    ("sss" . "subsubsection")
    ("par" . "paragraph"))
  "Association list between section strings and section abbrevs.")

(defconst qwe-section-str-type-alist
  '(;;.todo add ("prt" . part)
    ("chp" . chapter)
    ("sec" . section)
    ("sse" . subsection)
    ("sss" . subsubsection)
    ("par" . paragraph))
  "Association list between section abbrevs and section type names.")

(defconst qwe-section-string-type-alist
  '(;;.todo add ("part" . part)
    ("chapter"    . chapter)
    ("section"    . section)
    ("subsection" . subsection)
    ("subsection" . subsubsection)
    ("paragraph"  . paragraph))
  "Association list between section strings and section type names.")

(defconst qwe-section-regexp
  (regexp-opt (append qwe-section-str-list qwe-section-string-list))
  "Regexp used to identify section elements.")

(defconst qwe-section-label-regexp
  "\\(?:\\[.*\\]\\)?"
  "Regexp used to identify section labels.")

(defconst qwe-section-style-alist
  '((article . (section subsection subsubsection))
    (book . (chapter section subsection subsubsection))))


;;.sec 5.3. Variables
;;
;;.sse 5.3.1. Custom Variables
;;
;;.sss.var 5.3.1.1. qwe-section-numbering-custom-list

(defcustom qwe-section-numbering-custom-list nil
  "List of section types to be numbered automatically.

Each element of the list is one of 'chapter', 'section',
'subsection', 'subsubsection' or 'paragraph'. It is possible to
specify zero or more section types to be numbered.

Section numbers are hierarchical, of the form 'x.y.z', where 'x'
is the number of the first element of the hierarchy, 'y' for the
second and so on. The hierarchy is determined by the elements of
the list. That is, if the list contains 'chapter', 'subsection'
and 'paragraph', then only these section types (including
appendixes) will be numbered.  The others ('section' and
'subsubsection') neither be numbered nor affect the hierarchical
number construction."
  :type '(set
          ;;.todo (const part)
          (const chapter)
          (const section)
          (const subsection)
          (const subsubsection)
          (const paragraph))
  :safe 'qwe-section--list-p
  :group 'qwe)


;;.sec 5.4. Section Management
;;
;;.sse.fn 5.4.1. qwe-section-insert

(defun qwe-section-insert (section-type &optional indent)
  (interactive "SSection type: ")
    (goto-char (point-at-bol))
    ;;.del (when (re-search-forward (qwe-doc-delimiter-regexp qwe-doc) (point-at-eol) t)
    (when (re-search-forward
           (concat "^[  ]+"
                   (qwe-doc-delimiter qwe-doc)
                   "\\."
                   (qwe-lang-class-regexp 'section)) (point-at-eol) t)
      (kill-line 0))
    (when indent (indent-according-to-mode))
    (when section-type
      (insert (concat (qwe-doc-delimiter qwe-doc) "." section-type " "))
      ;;(if t qwe-section-renumber ))))
      ))

;;.sse 5.4.2. Helper functions
;;
;;.sss.fn 5.4.2.1. qwe-section-insert-chapter

(defun qwe-section-insert-chapter ()
  (interactive "")
  (qwe-section-insert "chp"))

;;.sss.fn 5.4.2.2. qwe-section-insert-section

(defun qwe-section-insert-section ()
  (interactive "")
  (qwe-section-insert "sec"))

;;.sss.fn 5.4.2.3. qwe-section-insert-subsection

(defun qwe-section-insert-subsection ()
  (interactive "")
  (qwe-section-insert "sse"))

;;.sss.fn 5.4.2.4. qwe-section-insert-subsubsection

(defun qwe-section-insert-subsubsection ()
  (interactive "")
  (qwe-section-insert "sss"))

;;.sss.fn 5.4.2.5. qwe-section-insert-paragraph

(defun qwe-section-insert-paragraph ()
  (interactive "")
  (qwe-section-insert "par" t))

;;.sse.fn 5.4.3. qwe-section-next

(defun qwe-section-next (section-type)
  (interactive "SSection type: ")
  (unless (qwe-section--goto section-type)
    (message "Not found.")))

;;.sse.fn 5.4.4. qwe-section-prev

(defun qwe-section-prev (section-type)
  (interactive "SSection type: ")
  (unless (qwe-section--goto section-type t)
    (message "Not found.")))


;;.sec 5.5. Section Numbering
;;
;;.sse.fn 5.5.1. qwe-section-renumber

(defun qwe-section-renumber (&optional file-name master-doc chp-format regexp vcounters
                                       file-list vsection-type section-level-alist)
  "Renumber sections according to document style.

The first section element determines if there will be numbers or
not, what will be the first number."
  (interactive "")
  (let ((i 0)
        (str nil)
        (level 0)
        (file nil)
        (last-buffer (current-buffer))
        (buffer-modified (buffer-modified-p))
        (doc-var (if (null master-doc) 'qwe-doc 'master-doc)))
    (when file-name
      (find-file file-name)
      (setq buffer-modified (buffer-modified-p)))
    (qwe-debug 'qwe-sec "Processing file %s" (file-name-nondirectory (buffer-file-name)))
    (qwe-debug 'qwe-sec "   master-doc = %s" (prin1-to-string (not (null master-doc))))
    (qwe-debug 'qwe-sec "   file-list  = %s" (prin1-to-string file-list))
    (qwe-debug 'qwe-sec "   doc-var    = %s" (prin1-to-string doc-var))
    (qwe-debug 'qwe-sec "   numbering  = %s"
               (prin1-to-string (qwe-doc-numbering (eval doc-var))))

    ;; Sequences used:
    ;;.ver     vcounters           ; e.g. [ 3 2 1 0 0 ]   (last number was '3.2.1.')
    ;;.ver     file-list           ; e.g. ("/usr/share/qwe/tutorial.el")
    ;;.ver     vsection-type       ; e.g. [ "chp" "sec" "sse" ]
    ;;.ver     section-level-alist ; e.g. (("chp" . 0) ("sec" . 1) ("sse" . 2))

    ;;.par Mark current buffer-file-name as processed
    ;;
    (add-to-list 'file-list (buffer-file-name) t)

    (unless master-doc
      (setf (qwe-doc-numbering qwe-doc) nil)
      (setq chp-format "%d.")

      ;;.par Make a list of numerable section types
      ;;
      ;;.del (qwe-section-numbering-style
      ;;.del  qwe-section-numbering-style qwe-section-numbering-custom-list)
      (setq qwe-section-numbering-custom-list
            (cdr (assoc (qwe-doc-style qwe-doc) qwe-section-style-alist)))
      (setq i (length qwe-section-numbering-custom-list))
      ;;.del (setq i (* 2 (length qwe-section-numbering-custom-list)))
      (setq vsection-type (make-vector i ""))
      (setq vcounters (make-vector i 0))
      (setq i 0)
      (dolist (s qwe-section-numbering-custom-list) ;;.fix assume its value is correct and properly sorted
        (aset vsection-type i (qwe-section--get-str s))
        (setq i (1+ i)))
      (setq vsection-type (delete-duplicates vsection-type))

      ;;.par Compute the hierarchical level of each numerable section type
      ;;
      (dotimes (i (length vsection-type))
        (add-to-list 'section-level-alist `(,(aref vsection-type i) . ,level) t)
        (setq level (1+ level)))
      (setq vcounters (make-vector (length vsection-type) 0))

      ;;.par Make a regexp suitable to find section types and directives
      ;;
      (setq regexp
            (if (> (string-width (qwe-doc-delimiter qwe-doc)) 0)
                (concat "^[ \t]*"
                        (qwe-doc-delimiter qwe-doc) "\\."
                        "\\(?:\\(#appendix\\|#include\\)\\|\\("
                        qwe-section-regexp "\\)"
                        qwe-section-label-regexp "\\(?:\\.\\("
                        qwe-lang-PL-elts-regexp "\\)\\)? \\)")
              (concat "^\\."
                      "\\(?:\\(#appendix\\|#include\\)\\|\\("
                      qwe-section-regexp "\\)"
                      qwe-section-label-regexp "\\(?:\\.\\("
                      qwe-lang-PL-elts-regexp "\\)\\)? \\)"))))

    ;;.par Iterate over all numerable section elements and directives
    ;;
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq str (concat (match-string-no-properties 1)
                          (match-string-no-properties 2)))

        ;;.tip always use short section names
        (setq str (or (qwe-section--get-str-from-string str) str))

        (cond
         ((and (string= str "#appendix")     ;;.tip Directives
               (search (vector "chp") vsection-type :test 'string=)
               (not (string= "%c." chp-format)))
          (setq level (cdr (assoc "chp" section-level-alist)))
          (aset vcounters level (1- ?A))
          (setq chp-format "%c."))
         ((string= str "#include")

          ;;.wip++ Renumber the Included File
          ;;
          (skip-chars-forward " \t")
          (setq file (expand-file-name (thing-at-point 'filename)))

          ;; Avoid circular inclusions
          (or (file-exists-p file)
              (qwe-debug 'qwe-sec :warning "   No such file '%s'." file))
          (or (not (member file file-list))
              (qwe-debug 'qwe-sec "   file '%s' already processed." file))
          (when (and (file-exists-p file)
                     (not (member file file-list)))
            (qwe-debug 'qwe-sec "   going to process %s" (file-name-nondirectory file))
            ;;.del (find-file file)
            ;;.del (add-to-list 'file-list file t)
            (setq i
                  (qwe-section-renumber file (eval doc-var)
                                        chp-format regexp vcounters
                                        file-list vsection-type section-level-alist))
            (setq vcounters (elt i 0))
            (setq file-list (elt i 1))
            (switch-to-buffer last-buffer))
          ;;
          ;;.wip-- end
          )
         (t     ;;.tip Sections
          (cond
           ((null (qwe-doc-numbering (eval doc-var)))
            ;;.del (equal numbering nil)
            (if (search (vector str) vsection-type :test 'string=)
                (progn
                  (setq i (car (qwe-section--get-number-list)))
                  (if i
                      (progn
                        ;;.del (setq numbering 'yes)
                        (setf (qwe-doc-numbering (eval doc-var)) 'yes)
                        (aset vcounters 0 (1- i)))
                    ;;.del (setq numbering 'no)
                    (setf (qwe-doc-numbering (eval doc-var)) 'no))
                  (qwe-debug 'qwe-sec "   --> NUMBERING = %s"
                             (prin1-to-string (qwe-doc-numbering (eval doc-var))))
                  (goto-char (point-at-bol))))
            (qwe-section--delete-this-number))
           ((equal (qwe-doc-numbering (eval doc-var)) 'no)
            ;;.del (equal numbering 'no)
            (qwe-section--delete-this-number))
           ((equal (qwe-doc-numbering (eval doc-var)) 'yes)
            ;;.del (equal numbering 'yes)
            (qwe-section--delete-this-number)
            (when (search (vector str) vsection-type :test 'string=)

              ;; {u/Increment section-type counter and compute section-type number}
              (setq level (cdr (assoc str section-level-alist)))
              (aset vcounters level (1+ (aref vcounters level)))

              ;; {u/Insert section-type number}
              (dotimes (i (1+ level))
                (if (string= "chp" (aref vsection-type i))
                    (insert (format chp-format (aref vcounters 0)))
                  (insert (format "%d." (aref vcounters i)))))
              (insert " ")

              ;; {u/Set lower levels to 0}
              (setq level (1+ level))
              (while (< level (length vcounters))
                (aset vcounters level 0)
                (setq level (1+ level)))))))))

      ;;.todo Executar un hook, (run-hooks qwe-section-update-hook)

      ;; {e/If the buffer was initially not modified, pretend it has}
      ;; {e/not been modified.}
      (set-buffer-modified-p buffer-modified)
      ;;.error Cal gravar el fitxer
      ;; Usant només el canvi de flag es perd la taula de continugts.
      ;; Crear una custom-var que prengui la desició de gravar o no.
      ;; Fer el mateix pels altres usos de {t/set-buffer-modified-p}.

      )

    (list vcounters file-list)))

;;.sse.fn 5.5.2. qwe-section-delete-numbers

(defun qwe-section-delete-numbers ()
  (interactive "")
  (let ((str nil)
        (regexp nil)
        (buffer-modified (buffer-modified-p)))
    (if  (> (string-width (qwe-doc-delimiter qwe-doc)) 0) ;;.tip Support for  (qwe-doc-delimiter qwe-doc)=""
        (setq regexp
              (concat (qwe-doc-delimiter qwe-doc) "\\.\\("
                      qwe-section-regexp "\\)"
                      qwe-section-label-regexp "\\(\\.\\("
                      qwe-lang-PL-elts-regexp "\\)\\)? "))
      (setq regexp
            (concat "^\\.\\("
                    qwe-section-regexp "\\)"
                    qwe-section-label-regexp "\\(\\.\\("
                    qwe-lang-PL-elts-regexp "\\)\\)? ")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (qwe-section--delete-this-number)))
    (set-buffer-modified-p buffer-modified))
  ;;.todo Executar un hook, (run-hooks qwe-section-update-hook)
  )


;;.sec 5.6. Colophon

(provide 'qwe-section)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-section.el ends here
