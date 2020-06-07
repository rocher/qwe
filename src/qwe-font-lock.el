;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-font-lock.el}
;;.cfg.prj.file.desc      Font lock functions
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
;;.note Preamble

(eval-when-compile
  (require 'qwe-lang))

;;.toc.begin Table of Contents
;;
;;.toc.chp       7. Font-Lock Support
;;
;;.toc.sec             7.1. Support for Contexts
;;.toc.sse                   7.1.1. qwe-font-lock-keywords-for-contexts
;;.toc.sse                   7.1.2. qwe-font-lock-keywords-for-contexts-kb
;;.toc.sse                   7.1.3. qwe-font-lock-context-form
;;.toc.sse                   7.1.4. qwe-font-lock-context-kb-form
;;.toc.sse                   7.1.5. qwe-font-lock-keywords-for-contexts
;;.toc.sse                   7.1.6. qwe-font-lock-keywords-for-contexts-kb
;;
;;.toc.sec             7.2. Support for Links
;;.toc.sse                   7.2.1. qwe-font-lock-keywords-for-links
;;
;;.toc.sec             7.3. Support for Major Modes
;;.toc.sse                   7.3.1. qwe-font-lock-keywords-for-major-mode
;;
;;.toc.sec             7.4. Private Interface
;;.toc.sse                   7.4.1. Support for list items
;;.toc.sse                   7.4.2. qwe-font-lock--item
;;.toc.sse                   7.4.3. Support for images
;;.toc.sss                         7.4.3.1. qwe-font-lock--make-image
;;.toc.sss                         7.4.3.2. qwe-font-lock--image-display-property
;;.toc.sss                         7.4.3.3. qwe-font-lock--image-spacer
;;
;;.toc.sec             7.5. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt _____________________________________________________________________________
;;.cmt
;;.chp 7. Font-Lock Support
;;
;;.quo                                  A program is in some sense a
;;.quo                        permanent object in that it can have a long
;;.quo                        lifetime. For the future reader, comments in a
;;.quo                        program should be truly substantive. They
;;.quo                        should say something. They should assist the
;;.quo                        program reader.
;;.quo
;;.quo                                  {i/Henry Ledgard}
;;.quo                                  Professional Software Volume II: Programming Practice
;;.quo                                  Addison-Wesley, 1987
;;
;; This is chapter contains all font-lock stuff used by {t/QWE}. This is key for
;; correct visualization of {t/QWE} elements. To understand how the
;; implementation works, it is useful to have a look at the info page about
;; the [[info:][(elisp)Font Lock Mode]]. The [[info:][(elisp)Search-based Fontification]] section
;; explains in detail how to customize font-lock mode.
;;
;;.sec 7.1. Support for Contexts
;;
;;.sse.var 7.1.1. qwe-font-lock-keywords-for-contexts

(defvar qwe-font-lock-keywords-for-contexts (list)
  "Font-lock keywords for contexts.")

;;.sse.var 7.1.2. qwe-font-lock-keywords-for-contexts-kb

(defvar qwe-font-lock-keywords-for-contexts-kb (list)
  "Font-lock keywords for keyword-based contexts.")

;;.sse.fn 7.1.3. qwe-font-lock-context-form

(defun qwe-font-lock-context-form (delimiter name
                                             abbrev
                                             keyword-list
                                             text-regexp
                                             face-variable)
  (let ((regexp-ctx (regexp-opt `(,(symbol-name name)
                                  ,(or (and abbrev
                                            (symbol-name abbrev))
                                       ""))))
        (spec (format "qwe-face-spec-delimiter-%S" name)))
    (if (null keyword-list)
        `(,(concat "^[ \t]*\\(?1:" delimiter
                   "\\." regexp-ctx "[ \t]\\)[ \t]*"
                   "\\(?2:" text-regexp "$\\)")
          (1 (eval (intern ,spec)) t)
          (2 ,face-variable t))
      `(,(concat "^[ \t]*\\(?1:" delimiter
                 "\\." regexp-ctx "\\)"
                 "\\(?2:"
                    (car keyword-list)
                    "\\(?:\\+\\+\\|\\-\\-\\)?" ;;.tip Support for 'hideshow'
                 "\\)"
                 "\\(?:[ \t]+\\(?3:" text-regexp "$\\)\\)?")
        (1 (eval (intern ,spec)) t)
        (2 ,(cdr keyword-list) t)
        (3 ,face-variable t t)))))

;;.sse.fn 7.1.4. qwe-font-lock-context-kb-form

(defun qwe-font-lock-context-kb-form (delimiter name keyword-list)
  (let ((spec (intern (format "qwe-face-spec-delimiter-%S" name)))
        (face-str (format "qwe-face-context-%S-" name)))
    `(,(concat "^[ \t]*\\(?1:" delimiter
               "\\." (symbol-name name)
               "\\.\\(?2:" (regexp-opt keyword-list) "\\)[ \t]\\)"
               "[ \t]*"
               "\\(?3:.+$\\)")
      (1 ,spec t)
      (3 (eval
          (intern
           (concat ,face-str (match-string-no-properties 2)))) t))))

;;.sse.fn 7.1.5. qwe-font-lock-keywords-for-contexts

(defun qwe-font-lock-keywords-for-contexts (delimiter)
  (setq qwe-font-lock-keywords-for-contexts (list))
  (dolist (ctx qwe-lang-class-context-alist)
    (let ((ctx-name (car ctx))
          (ctx-elt  (cdr ctx)))
      (unless (qwe-lang-elt-context-keyword-based ctx-elt)
        (dolist (regexp-face
                 (qwe-lang-elt-context-text-list ctx-elt))
          (add-to-list 'qwe-font-lock-keywords-for-contexts
                       (qwe-font-lock-context-form
                        delimiter
                        ctx-name
                        (qwe-lang-elt-context-abbrev ctx-elt)
                        (qwe-lang-elt-context-keyword-list ctx-elt)
                        (car regexp-face)
                        (cdr regexp-face)) t))))))

;;.sse.fn 7.1.6. qwe-font-lock-keywords-for-contexts-kb

(defun qwe-font-lock-keywords-for-contexts-kb (delimiter)
  (setq qwe-font-lock-keywords-for-contexts-kb (list))
  (dolist (ctx qwe-lang-class-context-alist)
    (let ((ctx-name (car ctx))
          (ctx-elt  (cdr ctx)))
      (when (qwe-lang-elt-context-keyword-based ctx-elt)
        (add-to-list 'qwe-font-lock-keywords-for-contexts-kb
                     (qwe-font-lock-context-kb-form
                      delimiter
                      ctx-name
                      (qwe-lang-elt-context-keyword-list ctx-elt)) t)))))


;;.sec 7.2. Support for Links
;;
;;.sse.fn 7.2.1. qwe-font-lock-keywords-for-links

(defun qwe-font-lock-keywords-for-links (delimiter type)
  (let ((length (length delimiter)))
    (cond
     ((eq type 'simple)
      (if (zerop length)
          '("\\(\\[\\[\\)\\([^][]+?\\)\\(\\]\\]\\)"
            (1 qwe-face-spec-link-ctor t)
            (2 qwe-face-spec-link-text t)
            (3 qwe-face-spec-link-ctor t))
        `(,delimiter
          "\\(\\[\\[\\)\\([^][]+?\\)\\(\\]\\]\\)" nil nil
          (1 qwe-face-spec-link-ctor t)
          (2 qwe-face-spec-link-text t)
          (3 qwe-face-spec-link-ctor t))))
     ((eq type 'descriptive)
      (if (zerop length)
          '("\\(\\[\\[[^][]+?\\]\\[\\)\\([^][]+?\\)\\(\\]\\]\\)"
            (1 qwe-face-spec-link-ctor t)
            (2 qwe-face-spec-link-text t)
            (3 qwe-face-spec-link-ctor t))
        `(,delimiter
          "\\(\\[\\[[^][]+?\\]\\[\\)\\([^][]+?\\)\\(\\]\\]\\)" nil nil
          (1 qwe-face-spec-link-ctor t)
          (2 qwe-face-spec-link-text t)
          (3 qwe-face-spec-link-ctor t))))
     ((eq type 'img-simple)
      (if (zerop length)
          '("\\(\\[\\[image:\\(?:\\]\\[\\)?\\)\\([^][]+?\\)\\(\\]\\]\\)"
            (1 qwe-face-spec-link-ctor t)
            (2 `(,@qwe-face-spec-link-text
                 help-echo ,(format "image '%s'" (match-string-no-properties 2))
                 display ,(qwe-font-lock--image-display-property (match-string-no-properties 2) (match-beginning 2))) t)
            (3 qwe-face-spec-link-ctor t))
        `(,delimiter
          "\\(\\[\\[image:\\(?:\\]\\[\\)?\\)\\([^][]+?\\)\\(\\]\\]\\)" nil nil
          (1 qwe-face-spec-link-ctor t)
          (2 `(,@qwe-face-spec-link-text
               help-echo ,(format "image '%s'" (match-string-no-properties 2))
               display ,(qwe-font-lock--image-display-property (match-string-no-properties 2) (match-beginning 2))) t)
          (3 qwe-face-spec-link-ctor t))))
     ((eq type 'img-descriptive)
      ;;
      ;;.tab {T/1       }{S/2        }{T/3 }{S/4   }{T/5  }
      ;;.tab {T/[[image:}{S/file-name}{T/][}{S/desc}{T/] ]}
      ;;
      (if (zerop length)
          '(".*\\(\\[\\[image:\\)\\([^][]+?\\)\\(\\]\\[\\)\\([^][]*?\\)\\(\\]\\]\\)"
            (1 qwe-face-spec-link-ctor t)
            (2 qwe-face-spec-link-ctor t)
            (3 qwe-face-spec-link-ctor t)
            (4 `(,@qwe-face-spec-link-text
                 help-echo ,(format "image '%s'" (match-string-no-properties 2))
                 display ,(qwe-font-lock--image-display-property (match-string-no-properties 2) (match-beginning 2))) t)
            (5 qwe-face-spec-link-ctor t))
        `(,delimiter
          ".*\\(\\[\\[image:\\)\\([^][]+?\\)\\(\\]\\[\\)\\([^][]*?\\)\\(\\]\\]\\)" nil nil
          (1 qwe-face-spec-link-ctor t)
          (2 qwe-face-spec-link-ctor t)
          (3 qwe-face-spec-link-ctor t)
          (4 `(,@qwe-face-spec-link-text
               help-echo ,(format "image '%s'" (match-string-no-properties 2))
               display ,(qwe-font-lock--image-display-property (match-string-no-properties 2) (match-beginning 2))) t)
          (5 qwe-face-spec-link-ctor t))))
     ((eq type 'img-aligned)
      `(,(concat "^\\([ \t]*" delimiter "[ \t]*\\)"
                 "\\(\\[\\[image\\(?:\\-\\(?:left\\|center\\|right\\)\\)?\\:"
                 "\\(?:\\]\\[\\)?\\)\\([^][]+?\\)\\(\\]\\][ \t]*$\\)")
        (1 `(face qwe-face-default-text display ,(qwe-font-lock--image-spacer (match-string-no-properties 2) (qwe-font-lock--make-image (match-string-no-properties 3)))) t)
        (2 qwe-face-spec-link-ctor t)
        (3 `(,@qwe-face-spec-link-text
             help-echo ,(format "image '%s'" (match-string-no-properties 3))
             display ,(qwe-font-lock--image-display-property (match-string-no-properties 3) (match-beginning 3))) t)
        (4 qwe-face-spec-link-ctor t))))))


;;.sec 7.3. Support for Major Modes
;;
;;.sse.fn 7.3.1. qwe-font-lock-keywords-for-major-mode
;; This function returns a list with {e/all} font-lock keywords needed by
;; {t/QWE}. Every language class and element is somehow represented in this list.
;; See documentation of variable [[elisp:(describe-variable 'font-lock-keywords)][font-lock-keywords]] for more information.

(defun qwe-font-lock-keywords-for-major-mode (delimiter)
  "Process font-lock keywords for all QWE elements."
  (let ((qwe-line `(,(concat "\\(" delimiter " \\)\\(.*\\)")
                    (1 qwe-face-spec-delimiter-default t)
                    (2 (quote qwe-face-default-text) t))))

    (when (zerop (length delimiter))
      (setq delimiter "")
      (setq qwe-line '("^\\([^.].*\\)" (1 (quote qwe-face-default-text) t))))

    (qwe-font-lock-keywords-for-contexts delimiter)
    (qwe-font-lock-keywords-for-contexts-kb delimiter)

    ;; The lists starts here ...
    ;;
    `((,(concat delimiter "$") . (0 qwe-face-spec-delimiter-default t))
      ,qwe-line

      ;;.par LIST ITEMS
      ;;
      ;;.par Unnumbered list items
      (,(concat "\\(" delimiter "\\.\\([lL]\\)i\\([1-6]\\)\\)\\( \\)\\(.*$\\)")
       (1 qwe-face-spec-delimiter-default t)
       (5 (quote qwe-face-default-text) t)
       (4 `(face qwe-face-fixed
            display ,(qwe-font-lock--item
                      nil
                      (match-string-no-properties 2)
                      (match-string-no-properties 3))) t))

      ;;.par Numbered list items
      (,(concat "\\(" delimiter "\\.\\([nN]\\)i\\([1-6]\\)\\)\\( \\)\\(.*$\\)")
       (1 qwe-face-spec-delimiter-default t)
       (5 (quote qwe-face-default-text) t)
       (4 `(face qwe-face-fixed
            display ,(qwe-font-lock--item
                      t
                      (match-string-no-properties 2)
                      (match-string-no-properties 3))) t))

      ;;.par VISUAL ELEMENTS (1)
      ;;
      ;;.par lang-class-title
      (,(concat "^[ \t]*\\(" (qwe-doc-delimiter qwe-doc) "\\."
                "\\(" (qwe-lang-class-regexp 'title) "\\)"
                " \\)\\(.*\\)")
       (1 (eval (intern (concat "qwe-face-spec-delimiter-" (match-string-no-properties 2)))) t)
       (3 (intern (concat "qwe-face-" (match-string-no-properties 2))) t))

      ;;.par lang-class-section
      (,(concat "^[ \t]*\\(" (qwe-doc-delimiter qwe-doc) "\\."
                "\\(" (qwe-lang-class-regexp 'section) "\\)"
                qwe-lang-opt-PL-elts-regexp
                " \\)\\(.*\\)")
       (1 (eval (intern (concat "qwe-face-spec-delimiter-" (match-string-no-properties 2)))) t)
       (3 (intern (concat "qwe-face-" (match-string-no-properties 2))) t))      ;;.wip END

      ;;.par lang-class-environment
      (,(concat "^[ \t]*\\(" (qwe-doc-delimiter qwe-doc) "\\."
                "\\(" (qwe-lang-class-regexp 'environment) "\\)"
                "\\(?: \\|$\\)\\)\\(.*\\)")
       (1 (eval (intern (concat "qwe-face-spec-delimiter-" (match-string-no-properties 2)))) t)
       (3 (intern (concat "qwe-face-" (match-string-no-properties 2))) t))

      ;;.par lang-class-annotation
      (,(concat "\\(" (qwe-doc-delimiter qwe-doc) "\\."
                "\\(" (qwe-lang-class-regexp 'annotation) "\\)"
                "\\(?:\\+\\+\\|\\-\\-\\)?\\)"        ; support for hideshow
                "\\(" qwe-lang-opt-ID-regexp "\\)"   ; identifier
                "\\( .*\\)")                         ; up to the end of line
       (1 (eval (intern (concat "qwe-face-spec-delimiter-" (match-string-no-properties 2)))) t)
       (3  qwe-face-spec-element-id t)
       (4 (intern (concat "qwe-face-" (match-string-no-properties 2))) t))

      ;;.par lang-class-decoration
      (,(concat "\\(" (qwe-doc-delimiter qwe-doc) "\\."
                "\\(" (qwe-lang-class-regexp 'decoration) "\\)\\)"
                "\\(" qwe-lang-opt-ID-regexp "\\)"   ; identifier
                "\\( .*\\)")                         ; up to the end of line
       (1 qwe-face-spec-delimiter-decoration t)
       (3 qwe-face-spec-element-id t)
       (4 (intern (concat "qwe-face-" (match-string-no-properties 2))) t))

      ;;.par lang-class-context
      ,@qwe-font-lock-keywords-for-contexts
      ,@qwe-font-lock-keywords-for-contexts-kb

      ;;.par DIRECTIVES AND COMMANDS
      ;;
      ;;.par include
      ;;
      (,(concat "\\(" delimiter "\\.\\)\\(#include[ \t]+\\)\\(.*\\)")
       (1 qwe-face-spec-delimiter-default t)
       (2 (quote font-lock-preprocessor-face) t)
       (3 qwe-face-spec-include t))

      ;;.par appendix
      ;;
      (,(concat "\\(" delimiter "\\.\\)\\(#appendix$\\)")
       (1 qwe-face-spec-delimiter-default t)
       (2 (quote font-lock-preprocessor-face) t))

      ;;.todo Decide whether to make multiline paragraphs or not
      ;;
      ;;.par MULTILINE PARAGRAPHS
      ;;
      ;;.par new
      ;; Used to mark new portions of the file
      ;;
      ;;.ver (,(concat "\\([ \t]*" delimiter
      ;;.ver           "\\.new\\+\\)\n\\(\\(?:.\\|\n\\)*?\\)\\([ \t]*"
      ;;.ver           delimiter "\\.new-\\)")
      ;;.ver  (1 (quote qwe-face-delimiter-multiline) t)
      ;;.ver  (2 (quote qwe-face-new-text) append)
      ;;.ver  (3 (quote qwe-face-delimiter-multiline) t))
      ;;
      ;;.par rev
      ;; Used to mark revised parts of the file
      ;;
      ;;.ver (,(concat "\\([ \t]*" delimiter
      ;;.ver           "\\.rev\\+\\)\n\\(\\(?:.\\|\n\\)*?\\)\\([ \t]*"
      ;;.ver           delimiter "\\.rev-\\)")
      ;;.ver  (1 (quote qwe-face-delimiter-multiline) t)
      ;;.ver  (2 (quote qwe-face-rev-text) append)
      ;;.ver  (3 (quote qwe-face-delimiter-multiline) t))
      ;;
      ;;.par old
      ;; Used to mark old parts of the file
      ;;
      ;;.ver (,(concat "\\([ \t]*" delimiter
      ;;.ver           "\\.old\\+\\)\n\\(\\(?:.\\|\n\\)*?\\)\\([ \t]*"
      ;;.ver           delimiter "\\.old-\\)")
      ;;.ver  (1 (quote qwe-face-delimiter-multiline) t)
      ;;.ver  (2 (quote qwe-face-old-text) append)
      ;;.ver  (3 (quote qwe-face-delimiter-multiline) t))

      ;;.par LINKS AND REFERENCES
      ;;
      ;;.par simple links
      ;; Used to create simple links, e.g. [[http://qwe.sf.net]]
      ;;
      ,(qwe-font-lock-keywords-for-links delimiter 'simple)

      ;;.par links with description
      ;; Used to create descriptive links, e.g. [[par:links][a void link]].
      ;; Can be used to jump to [[anchor:][an anchor]].
      ;;
      ,(qwe-font-lock-keywords-for-links delimiter 'descriptive)

      ;;.par SPECIAL SUPPORT FOR IMAGES
      ;;
      ;;.par simple and separate syntax
      ;;
      ,(qwe-font-lock-keywords-for-links delimiter 'img-simple)

      ;;.par descriptive syntax
      ;;
      ,(qwe-font-lock-keywords-for-links delimiter 'img-descriptive)

      ;;.par aligned images
      ;;
      ,(qwe-font-lock-keywords-for-links delimiter 'img-aligned)

      ;;.par VISUAL ELEMENTS (and 2)
      ;;
      ;;.par format text

      ,(cond ((> (length delimiter) 0)
              `(,delimiter .
                           (,(concat
                              "\\(?1:{\\(?2:" qwe-lang-format-text-regexp "\\)/\\)"
                              "\\(?3:.*?\\)"
                              "\\(?4:}\\)")
                            nil
                            nil
                            (1 qwe-face-spec-format-text prepend)
                            (3 (intern (concat "qwe-face-" (match-string-no-properties 2))) prepend)
                            (4 qwe-face-spec-format-text prepend))))
             (t
              `(,(concat
                  "\\(?1:{\\(?2:" qwe-lang-format-text-regexp "\\)/\\)"
                  "\\(?3:.*?\\)"
                  "\\(?4:}\\)")
                (1 qwe-face-spec-format-text prepend)
                (3 (intern (concat "qwe-face-" (match-string-no-properties 2))) prepend)
                (4 qwe-face-spec-format-text prepend))))

      ;;.par SPECIAL SUPPORT FOR 'verbatim'
      ;;
      (,(concat "^[ \t]*\\(" (qwe-doc-delimiter qwe-doc) "\\."
                "\\(" (qwe-lang-elt-regexp 'verbatim) "\\)"
                "\\(?: \\|$\\)\\)\\(.*\\)")
       (1 qwe-lang-velt--environment-verbatim t)
       (3 '(face qwe-face-fixed
                 invisible nil
                 display nil
                 follow-link nil
                 help-echo nil
                 mouse-face nil
                 pointer nil) t)))))

;;.sec 7.4. Private Interface
;;
;;.sse 7.4.1. Support for list items
;;
;;.wip++ Experiment with list items

;;.todo Finnish this experiment, with customizable variables

(defvar qwe-list-items-space
  '("     "
    "       "
    "         "
    "           "
    "             "
    "               "))

(defvar qwe-list-items-bullets
  '("   ∙ "
    "     ∘ "
    "       ▸ "
    "         ▹ "
    "           - "
    "             · "))

(defvar qwe-numbered-items-space
  '("     "
    "       "
    "         "
    "           "
    "             "
    "               "))

(defvar qwe-numbered-items-bullets
  '("  #. "
    "    #. "
    "      #. "
    "        #. "
    "          #. "
    "            #. "))

;;.sse.fn 7.4.2. qwe-font-lock--item

(defun qwe-font-lock--item (numbered letter clevel)
  (let ((display "???")
        (level (string-to-int clevel))
        (ltype (or (and numbered "[nN]i") "[lL]i")))
    (if (or (string= letter "N")
            (string= letter "L"))
        (setq display
              (elt (if numbered
                       qwe-numbered-items-bullets
                     qwe-list-items-bullets)
                   (1- level)))
      (save-excursion
        (forward-line -1)
        (if (re-search-forward (concat (qwe-doc-delimiter qwe-doc)
                                       "\\." ltype clevel)
                               (point-at-eol) t)
            (setq display
                  (elt (if numbered
                           qwe-numbered-items-space
                         qwe-list-items-space)
                       (1- level)))
          (setq display
                (elt (if numbered
                         qwe-numbered-items-bullets
                       qwe-list-items-bullets)
                     (1- level))))))
    display))
;;.wip-- end

;;.sse 7.4.3. Support for images
;;
;;.sss.fn 7.4.3.1. qwe-font-lock--make-image
;; For the given file, this function returns the display property list for
;; an image link.

(defun qwe-font-lock--make-image (file-name)
  (when (file-exists-p file-name)
    (unless (file-name-absolute-p file-name)
      (setq file-name (concat default-directory file-name)))
    (append (create-image file-name nil nil)
            (list :ascent 'center
                  :relief qwe-image-border))))

;;.sss.fn 7.4.3.2. qwe-font-lock--image-display-property
;; Depending on the value of {t/qwe-images-autoload} or the text property {t/show},
;; this function returns the appropriate list of display properties. Don't
;; use it directly (note that with no previous search the function uses
;; {t/match-*} functions: the search has been done by font-lock ;)

(defun qwe-font-lock--image-display-property (file-name pos)
  (let ((display-plist nil))
    (if (or (qwe-doc-image-autoload qwe-doc)
            (get-text-property pos 'show))
        (setq display-plist (qwe-font-lock--make-image file-name)))
    display-plist))

;;.sss.fn 7.4.3.3. qwe-font-lock--image-spacer
;; This function is used to create a {t/(space FORM)} specification for aligned
;; images ({t/limage}, {t/cimage} and {t/rimage}).

(defun qwe-font-lock--image-spacer (img_str img) ;; img_str == "[[image-(left|center|right)"
  (let ((align (substring-no-properties img_str 8 9))) ; 2 3
    (cond
     ((string= "l" align)
      `(space :align-to 0))
     ((string= "c" align)
      `(space :align-to (+ center (-0.5 . ,img))))
     ((string= "r" align)
      `(space :align-to (- right ,img))))))


;;.sec 7.5. Colophon

(provide 'qwe-font-lock)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-font-lock.el ends here
