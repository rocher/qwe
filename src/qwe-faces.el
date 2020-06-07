;;.cfg.header.begin
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-faces.el}
;;.cfg.prj.file.desc      Faces used by {t/QWE}
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
  (require 'qwe-data))

;;.toc.begin                            Table of Contents
;;
;;.toc.chp       6. Faces
;;
;;.toc.sec             6.1. Customization Groups
;;
;;.toc.sec             6.2. Face Specs
;;.toc.sse                   6.2.1. qwe-face-spec-delimiter-common
;;.toc.sse                   6.2.2. qwe-face-spec-delimiter-invisible
;;.toc.sse                   6.2.3. qwe-face-spec-delimiter-label-default
;;.toc.sse                   6.2.4. qwe-face-spec-delimiter-label-decoration
;;.toc.sse                   6.2.5. qwe-face-spec-delimiter-text
;;.toc.sse                   6.2.6. qwe-face-spec-element-id
;;.toc.sse                   6.2.7. qwe-face-spec-format-text
;;.toc.sse                   6.2.8. qwe-face-spec-include
;;.toc.sse                   6.2.9. qwe-face-spec-link-ctor
;;.toc.sse                   6.2.10. qwe-face-spec-link-text
;;
;;.toc.sec             6.3. Buffer Local Variables
;;
;;.toc.sec             6.4. Default Faces
;;.toc.sse                   6.4.1. qwe-face-default-keyword
;;.toc.sse                   6.4.2. qwe-face-default-text
;;
;;.toc.sec             6.5. Delimiters, Formats and Images
;;.toc.sse                   6.5.1. qwe-face-delimiter
;;.toc.sse                   6.5.2. qwe-face-delimiter-annotation
;;.toc.sse                   6.5.3. qwe-face-delimiter-label
;;.toc.sse                   6.5.4. qwe-face-delimiter-multiline
;;.toc.sse                   6.5.5. qwe-face-delimiter-text
;;.toc.sse                   6.5.6. qwe-face-element-id
;;.toc.sse                   6.5.7. qwe-face-format-text
;;
;;.toc.sec             6.6. Directives
;;.toc.sse                   6.6.1. qwe-face-directive-keyword
;;.toc.sse                   6.6.2. qwe-face-directive-text
;;
;;.toc.sec             6.7. Environments
;;.toc.sse                   6.7.1. qwe-face-abstract
;;.toc.sse                   6.7.2. qwe-face-comment
;;.toc.sse                   6.7.3. qwe-face-delete
;;.toc.sse                   6.7.4. qwe-face-quotation
;;.toc.sse                   6.7.5. qwe-face-tabular
;;.toc.sse                   6.7.6. qwe-face-tty
;;.toc.sse                   6.7.7. qwe-face-verbatim
;;
;;.toc.sec             6.8. Titles
;;.toc.sse                   6.8.1. qwe-face-part
;;.toc.sse                   6.8.2. qwe-face-title
;;.toc.sse                   6.8.3. qwe-face-subtitle
;;
;;.toc.sec             6.9. Sections
;;.toc.sse                   6.9.1. qwe-face-chapter
;;.toc.sse                   6.9.2. qwe-face-section
;;.toc.sse                   6.9.3. qwe-face-subsection
;;.toc.sse                   6.9.4. qwe-face-subsubsection
;;.toc.sse                   6.9.5. qwe-face-paragraph
;;
;;.toc.sec             6.10. Annotations
;;.toc.sse                   6.10.1. qwe-face-default-annotation-text
;;.toc.sse                   6.10.2. qwe-face-note
;;.toc.sse                   6.10.3. qwe-face-anchor
;;.toc.sse                   6.10.4. qwe-face-todo
;;.toc.sse                   6.10.5. qwe-face-work-in-progress
;;.toc.sse                   6.10.6. qwe-face-fixme
;;.toc.sse                   6.10.7. qwe-face-warning
;;.toc.sse                   6.10.8. qwe-face-error
;;.toc.sse                   6.10.9. qwe-face-bug
;;
;;.toc.sec             6.11. Decorations
;;.toc.sse                   6.11.1. qwe-face-tip
;;.toc.sse                   6.11.2. qwe-face-box
;;.toc.sse                   6.11.3. qwe-face-bold-box
;;.toc.sse                   6.11.4. qwe-face-button-released
;;.toc.sse                   6.11.5. qwe-face-button-pressed
;;
;;.toc.sec             6.12. Format Text
;;.toc.sse                   6.12.1. qwe-face-bold
;;.toc.sse                   6.12.2. qwe-face-bold-italic
;;.toc.sse                   6.12.3. qwe-face-italic
;;.toc.sse                   6.12.4. qwe-face-fixed
;;.toc.sse                   6.12.5. qwe-face-underline
;;.toc.sse                   6.12.6. qwe-face-inverse-video
;;.toc.sse                   6.12.7. qwe-face-variable
;;.toc.sse                   6.12.8. qwe-face-keyword
;;.toc.sse                   6.12.9. qwe-face-string
;;.toc.sse                   6.12.10. qwe-face-function
;;.toc.sse                   6.12.11. qwe-face-highlight-yellow
;;.toc.sse                   6.12.12. qwe-face-highlight-green
;;.toc.sse                   6.12.13. qwe-face-highlight-orange
;;.toc.sse                   6.12.14. qwe-face-highlight-cyan
;;
;;.toc.sec             6.13. Tables
;;.toc.sse                   6.13.1. qwe-face-table-title-row
;;.toc.sse                   6.13.2. qwe-face-table-row
;;.toc.sse                   6.13.3. qwe-face-table-alternate-row
;;.toc.sse                   6.13.4. qwe-face-table-underlined-row
;;.toc.sse                   6.13.5. qwe-face-table-underlined-alternate-row
;;.toc.sse                   6.13.6. qwe-face-table-underlined-simple-row
;;.toc.sse                   6.13.7. qwe-face-table-box-row
;;.toc.sse                   6.13.8. qwe-face-table-bbox-row
;;.toc.sse                   6.13.9. qwe-face-table-raised-row
;;.toc.sse                   6.13.10. qwe-face-table-sunken-row
;;
;;.toc.sec             6.14. Links
;;.toc.sse                   6.14.1. qwe-face-link-text
;;.toc.sse                   6.14.2. qwe-face-link-over
;;.toc.sse                   6.14.3. qwe-face-link-followed
;;.toc.sse                   6.14.4. qwe-face-link-ctor
;;
;;.toc.sec             6.15. Colophon
;;.toc.end {Z/ update ToC }
;;
;;
;;.chp 6. Faces
;;
;;.quo                                  Nevertheless, we have an unfortunate
;;.quo                        tendency to value creativity as an end in
;;.quo                        itself and use it as an excuse for ignorance. I
;;.quo                        have known both researchers and developers who
;;.quo                        refused to look at previous work because they
;;.quo                        wanted to use their own ideas.
;;.quo
;;.quo                                  {i/David Parnas}
;;.quo                                  Why Software Jewels Are Rare
;;.quo                                  Computer, February 1996
;;
;; This chapter contains all faces used by {t/QWE}.
;;
;;.sec 6.1. Customization Groups
;; These are the customization groups under which faces can be customized.

(defgroup qwe-face-basics nil
  "Basic faces for QWE mode. These faces are inherited by faces
of group `qwe-faces-derived'."
 :group 'qwe)

(defgroup qwe-derived-faces nil
  "Faces derived from QWE basic faces.

These are used to more specific faces. It would be better to
change only the basic ones."
  :group 'qwe-basic-faces)


;;.sec 6.2. Face Specs
;; This section contains a large collection of face specs. A face spec is a
;; set of face attributes used to construct new faces or change existing
;; ones. Mainly, they are used to determine the way some things are
;; displayed. Much of the work here is based in the following references:
;;
;;.li1 [[info:][(elisp)Faces]]
;;
;;.li1 [[info:][(elisp)Display Property]]
;;
;;.li1 [[info:][(elisp)Invisible Text]]
;;
;;.sse.const 6.2.1. qwe-face-spec-delimiter-common

(defconst qwe-face-spec-delimiter-common
  '(invisible qwe--delimiter ;;.note See below
              front-sticky nil
              rear-nonsticky t)
  "Common face spec used to display delimiters.")

;; The {t/invisible} property of a face spec value is a symbol that, if it
;; appears on the {t/buffer-invisibility-spec} list, characters with this
;; property are invisible. In this case, {t/qwe--delimiter} is the symbol used
;; in {t/QWE} delimiters face specifications. Adding or removing it from the
;; {t/buffer-invisibility-spec} list will show or hide {t/QWE} delimiters.
;;
;; See [[symbol:buffer-invisibility-spec,lisp-mode][buffer-invisibility-spec]] for more information.
;;
;;.sse.const 6.2.2. qwe-face-spec-delimiter-invisible

(defconst qwe-face-spec-delimiter-invisible ;;.tip for qwe-face-spec-delimiter-* vars
  `(face qwe-face-delimiter
         ,@qwe-face-spec-delimiter-common)
  "Face spec used to hide delimiters.")

;;.sse.const 6.2.3. qwe-face-spec-delimiter-label-default

(defconst qwe-face-spec-delimiter-label-default ;;.tip for qwe-face-spec-delimiter-default
  `(face qwe-face-delimiter-label
         ;;.del display "  | " ;;.todo Do it user-customizable
         display " ·  "
         ,@qwe-face-spec-delimiter-common)
  "Default face spec for labels.

This is the string that appears by default when delimiters are
shown as labels.")

;;.sse.const 6.2.4. qwe-face-spec-delimiter-label-decoration

(defconst qwe-face-spec-delimiter-label-decoration ;;.tip for qwe-face-spec-delimiter-decoration
  `(face default  ;;.del qwe-face-delimiter-label
         display "<"     ;;.tip The character in front of this tip when delimiters are shown as labels
         ;;.del display "◀"  ;;.todo Do it user-customizable
         ,@qwe-face-spec-delimiter-common)
  "Face spec to display labels on decorations.")

;;.sse.const 6.2.5. qwe-face-spec-delimiter-text

(defconst qwe-face-spec-delimiter-text ;;.tip for qwe-face-spec-delimiter-* vars
  '(face qwe-face-delimiter-text
         invisible qwe--delimiter
         front-sticky nil
         rear-nonsticky t)
  "Face spec used to display delimiters as text.")

;;.sse.var 6.2.6. qwe-face-spec-element-id

(setq qwe-face-spec-element-id
      '(face qwe-face-element-id
             invisible qwe--element-id
             front-sticky nil
             rear-nonsticky t))

;;.sse.var 6.2.7. qwe-face-spec-format-text

(setq qwe-face-spec-format-text
      '(face qwe-face-format-text
             invisible qwe--format-text
             front-sticky nil
             rear-nonsticky t))

;;.sse.const 6.2.8. qwe-face-spec-include

(defconst qwe-face-spec-include
  '(face qwe-face-link-text
         follow-link t
         help-echo "mouse-1: visit this file"
         keymap qwe-mouse-map
         mouse-face qwe-face-link-over
         pointer hand
         include t)
  "Face spec used to in {t/include} directives.")

;;.sse.var 6.2.9. qwe-face-spec-link-ctor

(setq qwe-face-spec-link-ctor
      '(face qwe-face-link-ctor
             invisible qwe--link-ctor
             front-sticky nil
             rear-nonsticky t))

;;.sse.const 6.2.10. qwe-face-spec-link-text

(defconst qwe-face-spec-link-text
  '(face qwe-face-link-text
         follow-link t
         help-echo "mouse-1: follow this link"
         keymap qwe-mouse-map
         mouse-face qwe-face-link-over
         pointer hand)
  "Face spec used to display links.")


;;.sec 6.3. Buffer Local Variables
;; Variables needed by {t/font-lock}.

(setq qwe-face-spec-delimiter-default    qwe-face-spec-delimiter-text)
(setq qwe-face-spec-delimiter-decoration qwe-face-spec-delimiter-text)

;; These variables must be buffer-local in order to let users display things
;; in different ways in different buffers.

(make-variable-buffer-local 'qwe-face-spec-delimiter-default)
(make-variable-buffer-local 'qwe-face-spec-delimiter-decoration)
(make-variable-buffer-local 'qwe-face-spec-element-id)
(make-variable-buffer-local 'qwe-face-spec-format-text)
(make-variable-buffer-local 'qwe-face-spec-link-ctor)


;;.sec 6.4. Default Faces
;;
;;.sse.face 6.4.1. qwe-face-default-keyword

(defface qwe-face-default-keyword
  '((t (:foreground "grey30"
        :inherit    fixed-pitch)))
  "Default face to display QWE delimiters of contexts."
 :group 'qwe-basic-faces)

;;.sse.face 6.4.2. qwe-face-default-text

(defface qwe-face-default-text
  '((t (:inherit variable-pitch)))
  "Default face to display QWE text."
 :group 'qwe-basic-faces)


;;.sec 6.5. Delimiters, Formats and Images
;;
;;.sse.face 6.5.1. qwe-face-delimiter

(defface qwe-face-delimiter
  '((t (:family     "misc-fixed"
        :foreground "grey50"
        :height     80
        :slant      normal
        :weight     normal
        :width      semi-condensed)))
  "Common face to display QWE delimiters."
 :group 'qwe-basic-faces)

;;.sse.face 6.5.2. qwe-face-delimiter-annotation

(defface qwe-face-delimiter-annotation
  '((t (:box        (:line-width -1 :color "black")
        :foreground "grey20"
        :inherit    qwe-face-delimiter-text
        :slant      normal
        :weight     normal
        :width      semi-condensed)))
  "Face to display QWE delimiters of annotations."
 :group 'qwe-basic-faces)

;;.sse.face 6.5.3. qwe-face-delimiter-label

(defface qwe-face-delimiter-label
  '((t (:foreground "grey20"
        :background "grey95"
        :inherit    qwe-face-delimiter-text)))
  "Face to display QWE delimiters displayed as labels."
 :group 'qwe-basic-faces)

;;.sse.face 6.5.4. qwe-face-delimiter-multiline

(defface qwe-face-delimiter-multiline
  '((t (:background "grey90"
        :foreground "grey10"
        :inherit    qwe-face-delimiter-text)))
  "Face to display QWE delimiters in multiline paragraphs."
 :group 'qwe-basic-faces)

;;.sse.face 6.5.5. qwe-face-delimiter-text

(defface qwe-face-delimiter-text
  '((t (:height    0.8
        :inherit   font-lock-comment-face)))
  "Face to display QWE delimiters displayed as text."
 :group 'qwe-basic-faces)

;;.sse.face 6.5.6. qwe-face-element-id

(defface qwe-face-element-id
  '((t (:foreground "white"
        :background "black"
        :inherit    qwe-face-default-annotation-text)))
  "Face to display elements IDs."
 :group 'qwe-basic-faces)

;;.sse.face 6.5.7. qwe-face-format-text

(defface qwe-face-format-text
  '((t (:foreground "red3"
        :inherit    qwe-face-default-text)))
  "Face to display format text codes."
 :group 'qwe-basic-faces)


;;.sec 6.6. Directives
;;
;;.sse.face 6.6.1. qwe-face-directive-keyword

(defface qwe-face-directive-keyword
  '((t (:foreground "green4"
        :inherit    qwe-face-default-keyword
        :slant      normal
        :weight     bold)))
  "Default face to display keywords."
 :group 'qwe-basic-faces)

;;.sse.face 6.6.2. qwe-face-directive-text

(defface qwe-face-directive-text
  '((t (:foreground "green3"
        :inherit    qwe-face-default-text
        :underline  "black"
        :weight     bold)))
  "Face to display text in include directives."
 :group 'qwe-basic-faces)


;;.sec 6.7. Environments
;;
;;.sse.face 6.7.1. qwe-face-abstract

(defface qwe-face-abstract
  '((t (:inherit qwe-face-default-text
        :slant   italic)))
  "Face for 'qwe-text' part in abstracts."
 :group 'qwe-basic-faces)

;;.sse.face 6.7.2. qwe-face-comment

(defface qwe-face-comment
  '((t (:inherit font-lock-comment-face)))
  "Face for 'qwe-text' part in comments."
 :group 'qwe-basic-faces)

;;.sse.face 6.7.3. qwe-face-delete

(defface qwe-face-delete
  '((t (:inherit        font-lock-comment-face
        :strike-through t)))
  "Face to display code to be deleted."
 :group 'qwe-basic-faces)

;;.sse.face 6.7.4. qwe-face-quotation

(defface qwe-face-quotation
  '((t (:inherit qwe-face-default-text
        :slant   italic)))
  "Face to display quotations."
  :group 'qwe-basic-faces)

;;.sse.face 6.7.5. qwe-face-tabular

(defface qwe-face-tabular
  '((t (:inherit qwe-face-fixed)))
  "Face to display tables."
  :group 'qwe-basic-faces)

;;.sse.face 6.7.6. qwe-face-tty

(defface qwe-face-tty
  '((t (:inherit qwe-face-fixed
        :foreground "green4")))
  "Face to display tty output."
  :group 'qwe-basic-faces)

;;.sse.face 6.7.7. qwe-face-verbatim

(defface qwe-face-verbatim
  '((t (:inherit qwe-face-fixed)))
  "Face to display verbatim text."
  :group 'qwe-basic-faces)


;;.sec 6.8. Titles
;;
;;.sse.face 6.8.1. qwe-face-part

(defface qwe-face-part
  '((t (:height  3.0
        :inherit qwe-face-section)))
  "Face to display document titles."
  :group 'qwe-derived-faces)

;;.sse.face 6.8.2. qwe-face-title

(defface qwe-face-title
  '((t (:height  2.0
        :inherit qwe-face-section)))
  "Face to display document titles."
  :group 'qwe-derived-faces)

;;.sse.face 6.8.3. qwe-face-subtitle

(defface qwe-face-subtitle
  '((t (:height  1.5
        :inherit qwe-face-section
        :slant   italic
        :weight  normal)))
  "Face to display document subtitles."
 :group 'qwe-derived-faces)


;;.sec 6.9. Sections
;;
;;.sse.face 6.9.1. qwe-face-chapter

(defface qwe-face-chapter
  '((t (:height  1.2
        :inherit qwe-face-section)))
  "Face to display chapter titles."
 :group 'qwe-derived-faces)

;;.sse.face 6.9.2. qwe-face-section

(defface qwe-face-section
  '((t (:height  1.2
        :inherit qwe-face-subsection)))
  "Face to display section names."
 :group 'qwe-derived-faces)

;;.sse.face 6.9.3. qwe-face-subsection

(defface qwe-face-subsection
  '((t (:height  1.2
        :inherit qwe-face-subsubsection)))
  "Face to display subsection names."
 :group 'qwe-derived-faces)

;;.sse.face 6.9.4. qwe-face-subsubsection

(defface qwe-face-subsubsection
  '((t (:height  1.2
        :inherit qwe-face-paragraph)))
  "Face to display subsubsection names."
 :group 'qwe-derived-faces)

;;.sse.face 6.9.5. qwe-face-paragraph
;;
;;.par Recommnedation
;;     I recommend you to use computer modern fonts for sectioning texts,
;;     available in {t/TrueType} format at {t/CTAN}, the {e/Comprehensive} {t/TeX} {e/Archive}
;;     {e/Network}.

(defface qwe-face-paragraph
  '((t (:inherit   qwe-face-default-text
        :slant     normal
        :underline nil
        :weight    bold)))
  "Face to display paragraph names."
 :group 'qwe-basic-faces)


;;.sec 6.10. Annotations
;;
;;.sse.face 6.10.1. qwe-face-default-annotation-text

(defface qwe-face-default-annotation-text
  '((t (:box        (:line-width -1 :color "black")
        :foreground "black"
        :inherit    qwe-face-default-text)))
  "Common face to display text in annotations."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.2. qwe-face-note

(defface qwe-face-note
  '((t (:background "grey90"
        :inherit qwe-face-default-annotation-text)))
  "*Face to display notes."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.3. qwe-face-anchor

(defface qwe-face-anchor
  '((t (:inherit    qwe-face-default-text
        :slant      italic
        :underline  "black")))
  "*Face to display anchors"
 :group 'qwe-basic-faces)

;;.sse.face 6.10.4. qwe-face-todo

(defface qwe-face-todo
  '((t (:background "cyan"
        :box        (:line-width -1 :color "black")
        :inherit    qwe-face-default-annotation-text)))
  "Face to display TODO's."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.5. qwe-face-work-in-progress

(defface qwe-face-work-in-progress
  '((t (:slant italic
        :inherit qwe-face-default-annotation-text)))
  "*Face to display wip (work in progressI) annotations."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.6. qwe-face-fixme

(defface qwe-face-fixme
  '((t (:background "greenyellow"
        :box        (:line-width -1 :color "black")
        :inherit    qwe-face-default-annotation-text)))
  "Face to display FIXME annotations."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.7. qwe-face-warning

(defface qwe-face-warning
  '((t (:background "yellow"
        :box        (:line-width -1 :color "black")
        :foreground "black"
        :inherit    qwe-face-fixed
        :weight     bold)))
  "Face to display warnings."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.8. qwe-face-error

(defface qwe-face-error
  '((t (:background "red"
        :box        (:line-width -1 :color "black")
        :foreground "white"
        :inherit    qwe-face-fixed
        :weight     bold)))
  "*Face to display errors."
 :group 'qwe-basic-faces)

;;.sse.face 6.10.9. qwe-face-bug

(defface qwe-face-bug
  '((t (:background "violet red"
        :box        (:line-width -1 :color "black")
        :foreground "white"
        :inherit    qwe-face-fixed
        :weight     bold)))
  "*Face to display bugs."
 :group 'qwe-basic-faces)


;;.sec 6.11. Decorations
;;
;;.sse.face 6.11.1. qwe-face-tip

(defface qwe-face-tip
  '((t (:background "#ffffc0"
        :height     0.9
        :inherit    qwe-face-default-annotation-text)))
  "*Face to display tips.
These annotations should contain messages or key ideas about the
algorithm implemented."
 :group 'qwe-basic-faces)

;;.sse.face 6.11.2. qwe-face-box

(defface qwe-face-box
  '((t (:background "white"
        :box        (:line-width -1 :color "black")
        :inherit    qwe-face-fixed)))
  "Face to display text boxes."
 :group 'qwe-basic-faces)

;;.sse.face 6.11.3. qwe-face-bold-box

(defface qwe-face-bold-box
  '((t (:background "white"
        :box        (:line-width -2 :color "black")
        :inherit    qwe-face-fixed
        :weight     bold)))
  "Face to display text boxes."
 :group 'qwe-basic-faces)

;;.sse.face 6.11.4. qwe-face-button-released

(defface qwe-face-button-released
  '((t (:background "grey75"
        :box        (:line-width -2 :color "grey75" :style released-button)
        :inherit    qwe-face-default-annotation-text)))
  "Face to display released buttons."
 :group 'qwe-basic-faces)

;;.sse.face 6.11.5. qwe-face-button-pressed

(defface qwe-face-button-pressed
  '((t (:background "grey75"
        :box        (:line-width -2 :color "grey75" :style pressed-button)
        :inherit    qwe-face-default-annotation-text)))
  "Face to display pressed buttons."
 :group 'qwe-basic-faces)


;;.sec 6.12. Format Text
;;
;;.sse.face 6.12.1. qwe-face-bold

(defface qwe-face-bold
  '((t (:weight  bold)))
  "Face for bold style."
 :group 'qwe-basic-faces)

;;.sse.face 6.12.2. qwe-face-bold-italic

(defface qwe-face-bold-italic
  '((t (:slant   italic
        :weight  bold)))
  "Face for bold+italic style."
 :group 'qwe-basic-faces)

;;.sse.face 6.12.3. qwe-face-italic

(defface qwe-face-italic
  '((t (:slant   italic)))
  "Face for italic style."
 :group 'qwe-basic-faces)

;;.sse.face 6.12.4. qwe-face-fixed

(defface qwe-face-fixed
  '((t (:inherit fixed-pitch)))
  "Face for fixed style."
 :group 'qwe-basic-faces)

;;.sse.face 6.12.5. qwe-face-underline

(defface qwe-face-underline
  '((t (:underline t)))
  "Face for underline style."
 :group 'qwe-basic-faces)

;;.sse.face 6.12.6. qwe-face-inverse-video

(defface qwe-face-inverse-video
  '((t (:inverse-video t)))
  "Face for inverse video."
 :group 'qwe-basic-faces)

;;.sse.face 6.12.7. qwe-face-variable

(defface qwe-face-variable
  '((t (:inherit (fixed-pitch font-lock-variable-name-face))))
  "Face for variables"
  :group 'qwe-basic-faces)

;;.sse.face 6.12.8. qwe-face-keyword

(defface qwe-face-keyword
  '((t (:inherit (fixed-pitch font-lock-keyword-face))))
  "Face for keywords"
  :group 'qwe-basic-faces)

;;.sse.face 6.12.9. qwe-face-string

(defface qwe-face-string
  '((t (:inherit (fixed-pitch font-lock-string-face))))
  "Face for strings"
  :group 'qwe-basic-faces)

;;.sse.face 6.12.10. qwe-face-function

(defface qwe-face-function
  '((t (:inherit (fixed-pitch font-lock-function-name-face))))
  "Face for functions"
  :group 'qwe-basic-faces)

;;.sse.face 6.12.11. qwe-face-highlight-yellow

(defface qwe-face-highlight-yellow
  '((t (:background "yellow")))
  "Face used to highlighting yellow."
  :group 'qwe-basic-faces)

;;.sse.face 6.12.12. qwe-face-highlight-green

(defface qwe-face-highlight-green
  '((t (:background "greenyellow")))
  "Face used to highlighting green."
  :group 'qwe-basic-faces)

;;.sse.face 6.12.13. qwe-face-highlight-orange

(defface qwe-face-highlight-orange
  '((t (:background "#ffb510")))
  "Face used to highlighting orange."
  :group 'qwe-basic-faces)

;;.sse.face 6.12.14. qwe-face-highlight-cyan

(defface qwe-face-highlight-cyan
  '((t (:background "cyan")))
  "Face used to highlighting in cyan."
  :group 'qwe-basic-faces)


;;.sec 6.13. Tables
;;
;;.sse.face 6.13.1. qwe-face-table-title-row

(defface qwe-face-table-title-row
  '((t (:background "black"
        :foreground "white"
        :inherit    qwe-face-fixed
        :weight     bold)))
  "Face to display title rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.2. qwe-face-table-row

(defface qwe-face-table-row
  '((t (:background "#f0f0f0"
        :foreground "black"
        :inherit    qwe-face-fixed
        :weight     normal)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.3. qwe-face-table-alternate-row

(defface qwe-face-table-alternate-row
  '((t (:background "#d0d0d0"
        :foreground "black"
        :inherit    qwe-face-fixed
        :weight     normal)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.4. qwe-face-table-underlined-row

(defface qwe-face-table-underlined-row
  '((t (:background "#f0f0f0"
        :foreground "black"
        :inherit    qwe-face-fixed
        :underline  "black"
        :weight     normal)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.5. qwe-face-table-underlined-alternate-row

(defface qwe-face-table-underlined-alternate-row
  '((t (:background "#d0d0d0"
        :foreground "black"
        :inherit    qwe-face-fixed
        :underline  "black"
        :weight     normal)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.6. qwe-face-table-underlined-simple-row

(defface qwe-face-table-underlined-simple-row
  '((t (:inherit    qwe-face-fixed
        :underline  "black"
        :weight     normal)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.7. qwe-face-table-box-row

(defface qwe-face-table-box-row
  '((t (:background "white"
        :box        (:line-width -1 :color "black")
        :inherit    qwe-face-fixed
        :weight     normal)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.8. qwe-face-table-bbox-row

(defface qwe-face-table-bbx-row
  '((t (:background "white"
        :box        (:line-width -2 :color "black")
        :inherit    qwe-face-fixed
        :weight     bold)))
  "Face to display rows in tables."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.9. qwe-face-table-raised-row

(defface qwe-face-table-raised-row
  '((t (:background "#d0d0d0"
        :box        (:line-width -1 :color "#d0d0d0" :style released-button)
        :inherit    qwe-face-fixed
        :weight     normal)))
  "Face to display raised rows."
 :group 'qwe-basic-faces)

;;.sse.face 6.13.10. qwe-face-table-sunken-row

(defface qwe-face-table-sunken-row
  '((t (:background "#d0d0d0"
        :box        (:line-width -1 :color "#d0d0d0" :style pressed-button)
        :inherit    qwe-face-fixed
        :weight     normal)))
  "Face to display sunken row."
 :group 'qwe-basic-faces)


;;.sec 6.14. Links
;;
;;.sse.face 6.14.1. qwe-face-link-text

(defface qwe-face-link-text
  '((t (:foreground "blue"
        :inherit    qwe-face-default-text)))
  "Face for links."
 :group 'qwe-basic-faces)

;;.sse.face 6.14.2. qwe-face-link-over

(defface qwe-face-link-over
  '((t (:inherit    qwe-face-link-text
        :underline  "blue")))
  "Face for references."
 :group 'qwe-basic-faces)

;;.sse.face 6.14.3. qwe-face-link-followed

(defface qwe-face-link-followed
  '((t (:foreground "violet"
        :inherit    qwe-face-link-text)))
  "Face for followed links."
 :group 'qwe-basic-faces)

;;.sse.face 6.14.4. qwe-face-link-ctor

(defface qwe-face-link-ctor
  '((t (:foreground "magenta"
        :inherit     qwe-face-link-text)))
  "Face to display links."
 :group 'qwe-basic-faces)


;;.sec 6.15. Colophon

(provide 'qwe-faces)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-faces.el ends here
