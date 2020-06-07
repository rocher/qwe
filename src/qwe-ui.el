;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-ui.el}
;;.cfg.prj.file.desc      User Interface
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
;;.cfg.doc.style  book
;;.cfg.header.filename ../header.qwe
;;.cfg.header.end [[qwe:hdr-update][update]]
;;.cmt _____________________________________________________________________________
;;.cmt
;;.chp 9. User Interface
;;
;;.quo                                  Learning a new tool or technique
;;.quo                        actually lowers programmer productivity and
;;.quo                        product quality initially. The eventual benefit
;;.quo                        is achivied only after this learning curve is
;;.quo                        overcome. Therefore, it is worth adopting new
;;.quo                        tools and techniques, but only (a) if their
;;.quo                        value is seen realistically and (b) if patience
;;.quo                        is used in measuring benefits.
;;.quo
;;.quo                                  {i/Robert L. Grass}
;;.quo                                  Facts and Fallacies of Software Engineering
;;.quo                                  Addison-Wesley, 2003
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       9. User Interface
;;
;;.toc.sec             9.1. Introduction
;;.toc.sse                   9.1.1. Module dependencies
;;
;;.toc.sec             9.2. Display
;;.toc.sse                   9.2.1. Delimiters
;;.toc.sss                         9.2.1.1. qwe-show-delimiters
;;.toc.sss                         9.2.1.2. qwe-show-delimiters-cycle
;;.toc.sss                         9.2.1.3. qwe-show-delimiters--invisible
;;.toc.sss                         9.2.1.4. qwe-show-delimiters--label
;;.toc.sss                         9.2.1.5. qwe-show-delimiters--text
;;.toc.sse                   9.2.2. Element IDs
;;.toc.sss                         9.2.2.1. qwe-show-element-ids
;;.toc.sss                         9.2.2.2. qwe-toggle-show-element-ids
;;.toc.sse                   9.2.3. Links
;;.toc.sss                         9.2.3.1. qwe-show-links
;;.toc.sss                         9.2.3.2. qwe-toggle-show-links
;;.toc.sse                   9.2.4. Format Text
;;.toc.sss                         9.2.4.1. qwe-show-format-text
;;.toc.sss                         9.2.4.2. qwe-toggle-show-format-text
;;.toc.sse                   9.2.5. Images
;;.toc.sss                         9.2.5.1. qwe-image-autoload
;;.toc.sss                         9.2.5.2. qwe-display-images
;;.toc.sss                         9.2.5.3. qwe-hide-images
;;.toc.sss                         9.2.5.4. qwe-set-image-border
;;.toc.sss                         9.2.5.5. qwe-display-images--show-or-hide
;;.toc.sse                   9.2.6. Tracking selections
;;
;;.toc.sec             9.3. Modifying Contents
;;.toc.sse                   9.3.1. Format Text
;;.toc.sss                         9.3.1.1. qwe--format-text
;;.toc.sss                         9.3.1.2. qwe-format-default
;;.toc.sss                         9.3.1.3. qwe-format-bold
;;.toc.sss                         9.3.1.4. qwe-format-italic
;;.toc.sss                         9.3.1.5. qwe-format-bold-italic
;;.toc.sss                         9.3.1.6. qwe-format-underline
;;.toc.sss                         9.3.1.7. qwe-format-fixed
;;.toc.sss                         9.3.1.8. qwe-format-inverse
;;.toc.sss                         9.3.1.9. qwe-format-programming-string
;;.toc.sss                         9.3.1.10. qwe-format-programming-variable
;;.toc.sss                         9.3.1.11. qwe-format-programming-keyword
;;.toc.sss                         9.3.1.12. qwe-format-programming-function
;;.toc.sss                         9.3.1.13. qwe-format-highlight-yellow
;;.toc.sss                         9.3.1.14. qwe-format-highlight-green
;;.toc.sss                         9.3.1.15. qwe-format-highlight-orange
;;
;;.toc.sec             9.4. Keymaps
;;.toc.sse                   9.4.1. qwe-mode-map
;;.toc.sse                   9.4.2. qwe-mouse-map
;;.toc.sse                   9.4.3. qwe-tool-bar-map
;;
;;.toc.sec             9.5. Compatibility with 'hideshow' mode
;;.toc.sse                   9.5.1. qwe-hide-block
;;.toc.sse                   9.5.2. qwe-hs-hide-block
;;.toc.sse                   9.5.3. qwe-toggle-hideshow-compatibility
;;
;;.toc.sec             9.6. QWE Menu
;;
;;.toc.sec             9.7. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec 9.1. Introduction
;;
;;.sse 9.1.1. Module dependencies

(eval-when-compile
  (require 'qwe-data)
  (require 'qwe-link))


;;.sec 9.2. Display
;;
;;.sse 9.2.1. Delimiters
;;
;;.sss.fn 9.2.1.1. qwe-show-delimiters
;; This function lets user change how {t/QWE} delimiters are shown. They can be
;; hidden, shown as a small labels or as they are written.

(defun qwe-show-delimiters (type &optional nofontify)
  "Sets the way QWE delimiters are shown.

Argument TYPE can be 'invisible, 'label or 'text. See also
`qwe-show-delimiters-as-invisible',
`qwe-show-delimiters-as-labels',
`qwe-show-delimiters-as-text' and
`qwe-show-delimiters-cycle'."
  (interactive) ;;.todo Add 'completing-read' facilities if 'type' is nil
  (setf (qwe-doc-show-delimiters qwe-doc) type)

  (when (equal type 'invisible) ;;.tip hide delimiters

    (dolist (elt-name qwe-lang-elt-list)
      (set (intern (format "qwe-face-spec-delimiter-%S" elt-name))
           qwe-face-spec-delimiter-invisible))

    (setq qwe-lang-velt--environment-verbatim     qwe-face-spec-delimiter-invisible
          qwe-face-spec-delimiter-default    qwe-face-spec-delimiter-invisible
          qwe-face-spec-delimiter-decoration qwe-face-spec-delimiter-invisible)

    ;;.todo (run-hooks qwe-ui-delimiter-invisible)
    (add-to-invisibility-spec 'qwe--delimiter)
    (message "Hide QWE delimiters"))

  (when (equal type 'label) ;;.tip show delimiters as labels

    (dolist (elt-name qwe-lang-elt-list)
      (set (intern (format "qwe-face-spec-delimiter-%S" elt-name))
           (copy-list qwe-face-spec-delimiter-label-default))
      (plist-put (symbol-value (intern (format "qwe-face-spec-delimiter-%S" elt-name)))
                 'display (qwe-lang-elt-get-attribute elt-name 'label)))

    (dolist (elt-name qwe-lang-class-annotation-list)
      (set (intern (format "qwe-face-spec-delimiter-%S" elt-name))
           `(face qwe-face-delimiter-annotation
                  display ,(qwe-lang-elt-get-attribute elt-name 'label))))

    (setq qwe-lang-velt--environment-verbatim qwe-face-spec-delimiter-label-default)
    (plist-put 'qwe-lang-velt--environment-verbatim
               'display (qwe-lang-elt-get-attribute 'verbatim 'label))

    (setq qwe-face-spec-delimiter-default     qwe-face-spec-delimiter-label-default
          qwe-face-spec-delimiter-decoration  qwe-face-spec-delimiter-label-decoration)

    ;;.todo (run-hooks qwe-ui-delimiter-label)
    (remove-from-invisibility-spec 'qwe--delimiter)
    (message "Display QWE delimiters as labels"))

  (when (equal type 'text) ;;.tip show delimiters as text

    (dolist (elt-name qwe-lang-elt-list)
      (set (intern (format "qwe-face-spec-delimiter-%S" elt-name))
           qwe-face-spec-delimiter-text))

    (setq qwe-lang-velt--environment-verbatim       qwe-face-spec-delimiter-text
          qwe-face-spec-delimiter-default     qwe-face-spec-delimiter-text
          qwe-face-spec-delimiter-decoration  qwe-face-spec-delimiter-text)

    ;;.todo (run-hooks qwe-ui-delimiter-text)
    (remove-from-invisibility-spec 'qwe--delimiter)
    (message "Display QWE delimiters as text"))

  (unless nofontify (progn
                      (font-lock-fontify-buffer)
                      (redisplay))))

;;.sss.fn 9.2.1.2. qwe-show-delimiters-cycle

(defun qwe-show-delimiters-cycle ()
  "Change the way QWE delimiters are shown.

It does the same as `qwe-show-delimiters' but cycling between
'invisible, 'label and 'text."
  (interactive)
  (let* ((type (qwe-doc-show-delimiters qwe-doc))
         (new-type (cond ((equal type 'invisible) 'label)
                         ((equal type 'label) 'text)
                         (t 'invisible))))
    (qwe-show-delimiters new-type)))

;;.sss.fn 9.2.1.3. qwe-show-delimiters--invisible

(defun qwe-show-delimiters--invisible ()
  (interactive)
  (qwe-show-delimiters 'invisible))

;;.sss.fn 9.2.1.4. qwe-show-delimiters--label

(defun qwe-show-delimiters--label ()
  (interactive)
  (qwe-show-delimiters 'label))

;;.sss.fn 9.2.1.5. qwe-show-delimiters--text

(defun qwe-show-delimiters--text ()
  (interactive)
  (qwe-show-delimiters 'text))

;;.sse 9.2.2. Element IDs
;;
;;.sss.fn 9.2.2.1. qwe-show-element-ids

(defun qwe-show-element-ids (b &optional fontify)
  (interactive)
  (setf (qwe-doc-show-element-ids qwe-doc) b)
  (if b
      (remove-from-invisibility-spec 'qwe--element-id)
    (add-to-invisibility-spec 'qwe--element-id))
  (unless fontify (font-lock-fontify-buffer)))

;;.sss.fn 9.2.2.2. qwe-toggle-show-element-ids

(defun qwe-toggle-show-element-ids ()
  (interactive)
  (qwe-show-element-ids (not (qwe-doc-show-element-ids qwe-doc))))

;;.sse 9.2.3. Links
;;
;;.sss.fn 9.2.3.1. qwe-show-links

(defun qwe-show-links (b &optional fontify)
  (interactive)
  (setf (qwe-doc-show-links qwe-doc) b)
  (if b
      (remove-from-invisibility-spec 'qwe--link-ctor)
    (add-to-invisibility-spec 'qwe--link-ctor))
  (unless fontify (font-lock-fontify-buffer)))

;;.sss.fn 9.2.3.2. qwe-toggle-show-links

(defun qwe-toggle-show-links ()
  (interactive)
  (qwe-show-links (not (qwe-doc-show-links qwe-doc))))

;;.sse 9.2.4. Format Text
;;
;;.sss.fn 9.2.4.1. qwe-show-format-text

(defun qwe-show-format-text (b &optional fontify)
  (interactive)
  (setf (qwe-doc-show-format-text qwe-doc) b)
  (if b
      (remove-from-invisibility-spec 'qwe--format-text)
    (add-to-invisibility-spec 'qwe--format-text))
  (unless fontify (font-lock-fontify-buffer)))

;;.sss.fn 9.2.4.2. qwe-toggle-show-format-text

(defun qwe-toggle-show-format-text ()
  "Toggle format text display."
  (interactive)
  (qwe-show-format-text (not (qwe-doc-show-format-text qwe-doc))))

;;.sse 9.2.5. Images
;;
;;.sss.fn 9.2.5.1. qwe-image-autoload
;; Use this function to set the variable {t/qwe-doc-image-autoload}. Don't change
;; the variable directly: the change won't have effect until the next buffer
;; {e/fontification}.
;;
;; When {e/autoload} is {t/t}, then all images are always displayed. When it is {t/nil},
;; then the user can selectively display or hide each image.

(defun qwe--set-image-autoload (autoload)
  (interactive)
  (setf (qwe-doc-image-autoload qwe-doc) autoload)
  (font-lock-fontify-buffer))

(defmacro qwe-set-image-autoload ()
  (qwe--set-image-autoload t))

(defmacro qwe-unset-image-autoload ()
  (qwe--set-image-autoload nil))

(defmacro qwe-toggle-image-autoload ()
  (qwe--set-image-autoload (not (qwe-doc-image-autoload qwe-doc))))

;;.sss.fn 9.2.5.2. qwe-display-images

(defun qwe-display-images ()
  (interactive)
  (qwe-display-images--show-or-hide 1))

;;.sss.fn 9.2.5.3. qwe-hide-images

(defun qwe-hide-images ()
  (interactive)
  (qwe-display-images--show-or-hide 0))

;;.sss.fn 9.2.5.4. qwe-set-image-border

(defun qwe-set-image-border (width)
  (interactive)
  (setq qwe-image-border width)
  (font-lock-fontify-buffer))

;;.sss.fn 9.2.5.5. qwe-display-images--show-or-hide

(defun qwe-display-images--show-or-hide (show)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[image:\\(.*\\)\\]\\]" nil t)
      (let ((begin (match-beginning 0))
            (end (match-end 0))
            (file-name (match-string-no-properties 1)))
        (qwe-link-follow--image begin end file-name show)
        (goto-char end)))))

;;.sse 9.2.6. Tracking selections
;; These functions are useful to highlight {t/QWE} elements when users click on
;; some link or other kind of {t/QWE} element.

(defvar qwe--highlight nil)

(defun qwe-highlight-selection (begin end &optional face)
  (let ((hiface (or face 'highlight)))
    (setq qwe--highlight (make-overlay begin end))
    (overlay-put qwe--highlight 'face hiface)
    (add-hook 'pre-command-hook 'qwe-un-highlight-selection)))

(defun qwe-un-highlight-selection ()
  (delete-overlay qwe--highlight)
  (setq qwe--highlight nil)
  (remove-hook 'pre-command-hook 'qwe-un-highlight-selection))


;;.sec 9.3. Modifying Contents
;; Functions defined in this section let users change character formatting
;; and document structure.
;;
;;.sse 9.3.1. Format Text
;;
;;.sss.fn 9.3.1.1. qwe--format-text

(defun qwe--format-text (char)
  (let ((region-p (region-active-p))
        (beg (region-beginning))
        (end (region-end)))
    (when region-p
      (goto-char beg))

    (cond
     ((looking-at "\{[a-zA-Z]/")
      (delete-char 3))
     ((looking-back "\{[a-zA-Z]/")
      (backward-delete-char 3))
     (t
      (setq end (+ 3 end))))

    (insert (format "{%c/" char))
    (when region-p
      (goto-char end))
    (unless (or (looking-at "}")
                (looking-back "}"))
      (insert "}")
      (backward-char 1))))

;;.sss.fn 9.3.1.2. qwe-format-default

(defun qwe-format-default (begin end)
  (interactive "r")
  (save-excursion
    (when (and (goto-char begin)
               (looking-back "\{[a-zA-Z]/"))
      (setq begin (- begin 3)))
    (when (and (goto-char end)
               (looking-at "\}"))
      (setq end (+ end 1)))
    (goto-char begin)
    (while (re-search-forward "\{[a-zA-Z]/\\([^\}]*\\)\}" end t)
      (replace-match "\\1" t)
      (setq end (- end 4)))))

;;.sss.fn 9.3.1.3. qwe-format-bold

(defun qwe-format-bold ()
  (interactive)
  (qwe--format-text ?b))

;;.sss.fn 9.3.1.4. qwe-format-italic

(defun qwe-format-italic ()
  (interactive)
  (qwe--format-text ?e))

;;.sss.fn 9.3.1.5. qwe-format-bold-italic

(defun qwe-format-bold-italic ()
  (interactive)
  (qwe--format-text ?i))

;;.sss.fn 9.3.1.6. qwe-format-underline

(defun qwe-format-underline ()
  (interactive)
  (qwe--format-text ?u))

;;.sss.fn 9.3.1.7. qwe-format-fixed

(defun qwe-format-fixed ()
  (interactive)
  (qwe--format-text ?t))

;;.sss.fn 9.3.1.8. qwe-format-inverse

(defun qwe-format-inverse ()
  (interactive)
  (qwe--format-text ?I))

;;.sss.fn 9.3.1.9. qwe-format-programming-string

(defun qwe-format-programming-string ()
  (interactive)
  (qwe--format-text ?s))

;;.sss.fn 9.3.1.10. qwe-format-programming-variable

(defun qwe-format-programming-variable ()
  (interactive)
  (qwe--format-text ?v))

;;.sss.fn 9.3.1.11. qwe-format-programming-keyword

(defun qwe-format-programming-keyword ()
  (interactive)
  (qwe--format-text ?k))

;;.sss.fn 9.3.1.12. qwe-format-programming-function

(defun qwe-format-programming-function ()
  (interactive)
  (qwe--format-text ?f))

;;.sss.fn 9.3.1.13. qwe-format-highlight-yellow

(defun qwe-format-highlight-yellow ()
  (interactive)
  (qwe--format-text ?y))

;;.sss.fn 9.3.1.14. qwe-format-highlight-green

(defun qwe-format-highlight-green ()
  (interactive)
  (qwe--format-text ?g))

;;.sss.fn 9.3.1.15. qwe-format-highlight-orange

(defun qwe-format-highlight-orange ()
  (interactive)
  (qwe--format-text ?o))

;;.del ;;.wip++ CANVI DE PARÀGRAFS
;;.del ;;qwe
;;.del ;;.sec 9.4. Paragraphs
;;.del ;;
;;.del ;;.sse 9.4.1. Private functions
;;.del ;;
;;.del ;;.sss.fn 9.4.1.1. qwe--paragraph-line
;;.del
;;.del (defun qwe--paragraph-line (type)
;;.del   (save-excursion
;;.del     (goto-char (point-at-bol))
;;.del     (if (looking-at (concat (qwe-doc-delimiter-regexp qwe-doc) ".*"))
;;.del             ;;.ver
;;.del )))
;;.del
;;.del ;;.sse.fn 9.4.2. qwe--paragraph
;;.del
;;.del (defun qwe--paragraph (type)
;;.del   (let ((region-p (region-active-p))
;;.del         (region nil))))
;;.del
;;.del ;;.wip-- END


;;.sec 9.4. Keymaps
;; Here are defined all the keymaps used by {t/QWE}.
;;
;;.sse.var 9.4.1. qwe-mode-map

(defvar qwe-mode-map nil
  "QWE mode map.")
(setq qwe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cqc"  'qwe-section-insert-chapter)
    (define-key map "\C-cqs"  'qwe-section-insert-section)
    (define-key map "\C-cqu"  'qwe-section-insert-subsection)
    (define-key map "\C-cqb"  'qwe-section-insert-subsubsection)
    (define-key map "\C-cqp"  'qwe-section-insert-paragraph)
    (define-key map "\C-cqdi" 'qwe-show-delimiters--invisible)
    (define-key map "\C-cqdl" 'qwe-show-delimiters--label)
    (define-key map "\C-cqdt" 'qwe-show-delimiters--text)
    (define-key map "\C-cqi"  'qwe-toggle-show-element-ids)
    (define-key map "\C-cql"  'qwe-toggle-show-links)
    (define-key map "\C-cqf"  'qwe-toggle-show-format-text)
    (define-key map "\C-cqtd" 'qwe-format-default)
    (define-key map "\C-cqtb" 'qwe-format-bold)
    (define-key map "\C-cqti" 'qwe-format-italic)
    (define-key map "\C-cqtI" 'qwe-format-bold-italic)
    (define-key map "\C-cqtu" 'qwe-format-underline)
    (define-key map "\C-cqtf" 'qwe-format-fixed)
    (define-key map "\C-cqtg" 'qwe-format-highlight-green)
    (define-key map "\C-cqty" 'qwe-format-highlight-yellow)
    (define-key map "\C-cqtc" 'qwe-format-highlight-cyan)
    (define-key map "\C-cqto" 'qwe-format-highlight-orange)
    map))

;;.sse.var 9.4.2. qwe-mouse-map

(defvar qwe-mouse-map nil
  "QWE mouse map.")
(setq qwe-mouse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'qwe-link-follow)
    (define-key map [mouse-2] 'qwe-link-follow)
    map))

;;.wip++ (pre-pre-ALPHA status) Support toolbars for formatting, sectioning, navigating, etc
;;
;;.sse.var 9.4.3. qwe-tool-bar-map

(defvar qwe-tool-bar-map nil
  "QWE tool-bar map.")

(setq qwe-tool-bar-map
      (let ((map (make-sparse-keymap)))
        (tool-bar-add-item "chapter"
                           '(lambda() (interactive) (qwe-section-insert "chp"))
                           'qtb-insert-section-chp
                           :help "Insert chapter")
        (tool-bar-add-item "section"
                           '(lambda() (interactive)
                              (qwe-section-insert "sec")
                              (qwe-section-renumber))
                           'qtb-insert-section-sec
                           :help "Insert section")
        map))

(setq qwe-tool-bar-map (make-sparse-keymap))
(define-key qwe-tool-bar-map [tool-bar qtb]
  '(menu-item "renumber" 'qwe-section-renumber
              :image (image :type png
                            :file "~/local/fret/devel/qwe/img/section.xpm")))

;;.wip-- end


;;.todo HIDESHOW compatibility must be a new extension ...
;;         ... and probably must use {t/'hideshow'} implementation to do its work
;;
;;.warning SO DON'T USE IT UNLESS YOU KNOW WHAT YOU'RE DOING
;;
;;.sec 9.5. Compatibility with 'hideshow' mode
;; {t/Hideshow} mode let users hide or show blocks of comments or code. Blocks
;; are defined per mode depending on the programing language. If you enable
;; {t/hideshow} compatibility then you'll be able to hide or show mixed blocks of
;; code and comments, also called {e/QWE blocks}. These blocks are explicitly
;; defined by the user.
;;
;; The way to define a QWE block is with annotations and contexts. By simply
;; adding {t/'++'} or {t/'--'} at the end of such elements the user is explicitly
;; delimiting the begining and the end (respectively) of such a block. The
;; next example also shows you how to hide ro show this block:
;;
;;.note++[example] Start
;;
;;.par Starting
;; First at all you need to have enabled [[symbol:hs-minor-mode,lisp-mode][hs-minor-mode]]. Then, evaluate
;;
;;     [[elisp:(qwe-enable-hideshow t)][(qwe-enable-hideshow t)]]
;;
;; or customize the variabe [[elisp:(customize-variable 'qwe-enable-hideshow)][qwe-enable-hideshow]].
;;
;;.par Managing QWE blocks
;; If you have enabled QWE hideshow compatibility, then... ;;.todo COMPLETE!!
;;
;;.note--[exmaple] End
;;
;;.sse.fn 9.5.1. qwe-hide-block

(defun qwe-hide-block ()
  (if qwe-doc
      (save-excursion
        (let ((end (point-at-eol))
              (begin (point-at-bol))
              (regexp (concat
                       ;;.err This can merge annotations with contexts
                       (qwe-doc-delimiter qwe-doc) "\\."
                       ;;.bug (qwe-elt--regexp-opt-for 'annotation 'context)
                       )))
          (re-search-forward regexp end t)
          (or (and (looking-at "\\+\\+")
                   (setq begin end)
                   (re-search-forward (concat regexp "--") nil t)
                   (setq end (point-at-eol))
                   (hs-make-overlay begin end 'code)
                   t)
              (and (looking-at "--")
                   ;;.err Test weather it is the same type of annotation
                   (re-search-backward (concat regexp "\\+\\+") nil t)
                   (setq begin (point-at-eol))
                   (hs-make-overlay begin end 'code)
                   (message "qwe-hide-block")
                   t))))))

;;.sse.fn 9.5.2. qwe-hs-hide-block

(defadvice hs-hide-block (around qwe-hs-hide-block)
  "Try to hide annotation blocks before trying to hide blocks."
  (unless (qwe-hide-block)
    ad-do-it))

(defun qwe-hs-toggle-advise ()
  (when qwe-enable-hideshow
    (ad-enable-advice 'hs-hide-block 'around 'qwe-hs-hide-block)
    (ad-activate 'hs-hide-block))
  (unless qwe-enable-hideshow
    (ad-disable-advice 'hs-hide-block 'around 'qwe-hs-hide-block)
    ; (ad-deactivate 'hs-hide-block) ;;.warning May be other advices are also active
    ))

;;.sse.fn 9.5.3. qwe-toggle-hideshow-compatibility

(defun qwe-toggle-hideshow-compatibility ()
  (interactive "")
  (setq qwe-enable-hideshow (not qwe-enable-hideshow))
  (qwe-hs-toggle-advise))

(qwe-hs-toggle-advise)


;;.sec 9.6. QWE Menu

(defvar qwe-menu nil
  "QWE menu.")

(easy-menu-define qwe-menu qwe-mode-map "QWE menu"
  '("QWE"
    ( "Text format"
      [ "default"             qwe-format-default ]
      [ "bold"                qwe-format-bold ]
      [ "italic"              qwe-format-italic ]
      [ "bold-italic"         qwe-format-bold-italic ]
      [ "underline"           qwe-format-underline ]
      [ "fixed"               qwe-format-fixed ]
      [ "inverse"             qwe-format-inverse ]
      ( "programming"
        [ "string"            qwe-format-programming-string ]
        [ "variable"          qwe-format-programming-variable ]
        [ "keyword"           qwe-format-programming-keyword ]
        [ "function"          qwe-format-programming-function ]
       )
      ( "highlight"
        [ "yellow"            qwe-format-highlight-yellow ]
        [ "green"             qwe-format-highlight-green ]
        [ "orange"            qwe-format-highlight-orange ]
        [ "cyan"              qwe-format-highlight-cyan ]
       )
      )
    ( "Environment"
      [ "text"                qwe-paragraph-text ]
      [ "abstract"            qwe-paragraph-abstract ]
      [ "quotation"           qwe-paragraph-quotation ]
      [ "verbatim"            qwe-paragraph-verbatim ]
      [ "comment"             qwe-paragraph-comment ]
      [ "terminal"            qwe-paragraph-terminal ]
      [ "deleted"             qwe-paragraph-deleted ]
     )
    ( "Sectioning"
      ;;.del [ "Title"               (qwe-section-insert "ttl") :help "Inserts the document title" ]
      ;;.del [ "Subtitle"            (qwe-section-insert "stl") :help "Inserts the document subtitle" ]
      [ "Chapter"             qwe-section-insert-chapter       :help "Insert a new chapter" ]
      [ "Section"             qwe-section-insert-section       :help "Insert a new section" ]
      [ "SubSection"          qwe-section-insert-subsection    :help "Insert a new subsection" ]
      [ "Subsubsection"       qwe-section-insert-subsubsection :help "Insert a new subsubsection" ]
      [ "Paragraph"           qwe-section-insert-paragraph     :help "insert a new paragraph"]
      "---"
      [ "Renumber"            qwe-section-renumber       :help "Computes section numbering" ]
      [ "No numbers"          qwe-section-delete-numbers :help "Remove section numbers" ]
      )
    ( "Annotation"
      [ "Note"                (qwe-insert-annotation "note") ]
      [ "Todo"                (qwe-insert-annotation "todo") ]
      [ "WIP"                 (qwe-insert-annotation "wip") ]
      [ "Fixme"               (qwe-insert-annotation "fixme") ]
      [ "Warning"             (qwe-insert-annotation "warning") ]
      [ "Error"               (qwe-insert-annotation "error") ]
      [ "Bug"                 (qwe-insert-annotation "bug") ]
     )
    ( "Decoration"
      [ "Tip"                 (qwe-insert-annotation "tip") ]
      [ "Box"                 (qwe-insert-annotation "box") ]
      [ "Bold box"            (qwe-insert-annotation "boldbox") ]
      [ "Released button"     (qwe-insert-annotation "released") ]
      [ "Pressed button"      (qwe-insert-annotation "pressed") ]
     )
    ( "Link"
      ( "Section"
        [ "Chapter"           (insert "[[chp:][<text>]]") ]
        [ "Appendix"          (insert "[[app:][<text>]]") ]
        [ "Section"           (insert "[[sec:][<text>]]") ]
        [ "Subsection"        (insert "[[sse:][<text>]]") ]
        [ "Subsubsection"     (insert "[[sss:][<text>]]") ]
        )
      ( "Command"
        [ "Elisp"             (insert "[[elisp:][(<expression>)]]") ]
        [ "Shell"             (insert "[[shell:][<command>]]") ]
        [ "Compile"           (insert "[[compile:][<command>]]") ]
       )
      ( "Help"
        [ "Man page"          (insert "[[man:][<man-page>]]") ]
        [ "Info node"         (insert "[[info:][(<file>)<node>]]") ]
        [ "Symbol"            (insert "[[symbol:][<symbol>,<mode>]]") ]
       )
      ( "External files"
        [ "File"              (insert "[[file:][<filename>]]") ]
        [ "Anchor in file"    (insert "[[file:<filename>,a:][<anchor>]]") ]
        )
      ( "URL"
        [ "HTTP"              (insert "[[http://<site>]]") ]
        [ "FTP"               (insert "[[ftp://<site>]]") ]
        [ "Other"             (insert "[[<URL>]]") ]
       )
      ( "Image"
        [ "Inline"            (insert "[[image:][<filename>]]") ]
        [ "Left aligned"      (insert "[[image-left:][<filename>]]") ]
        [ "Centered"          (insert "[[image-center:][<filename>]]") ]
        [ "Right aligned"     (insert "[[image-right:][<filename>]]") ]
       )
      )
    "---"
    ( "Goto next"
      [ "Chapter"             (qwe-section-next 'chapter) ]
      [ "Section"             (qwe-section-next 'section) ]
      [ "Subsection"          (qwe-section-next 'subsection) ]
      [ "Subsubection"        (qwe-section-next 'subsubsection) ]
      [ "Paragraph"           (qwe-section-next 'paragraph) ]
      "---"
      [ "Note"                (qwe-lang-elt-re-search 'note) ]
      [ "Todo"                (qwe-lang-elt-re-search 'todo) ]
      [ "WIP"                 (qwe-lang-elt-re-search 'wip) ]
      [ "Fixme"               (qwe-lang-elt-re-search 'fixme) ]
      [ "Warning"             (qwe-lang-elt-re-search 'warning) ]
      [ "Error"               (qwe-lang-elt-re-search 'error) ]
      [ "Bug"                 (qwe-lang-elt-re-search 'bug) ]
      )
    ( "Goto previous"
      [ "Chapter"             (qwe-section-prev 'chapter) ]
      [ "Section"             (qwe-section-prev 'section) ]
      [ "Subsection"          (qwe-section-prev 'subsection) ]
      [ "Subsubection"        (qwe-section-prev 'subsubsection) ]
      [ "Paragraph"           (qwe-section-prev 'paragraph) ]
      "---"
      [ "Note"                (qwe-lang-elt-re-search 'note nil nil nil t) ]
      [ "Todo"                (qwe-lang-elt-re-search 'todo nil nil nil t) ]
      [ "WIP"                 (qwe-lang-elt-re-search 'work-in-progress nil nil nil t) ]
      [ "Fixme"               (qwe-lang-elt-re-search 'fixme nil nil nil t) ]
      [ "Warning"             (qwe-lang-elt-re-search 'warning nil nil nil t) ]
      [ "Error"               (qwe-lang-elt-re-search 'error nil nil nil t) ]
      [ "Bug"                 (qwe-lang-elt-re-search 'bug nil nil nil t) ]
      )
    [ "Go back"               (qwe-link-history-go-back) ]
    [ "Go forward"            (qwe-link-history-go-forward) ]
    "---"
    ("Preferences"
     ( "Show images"
       [ "Autoload"           (qwe-toggle-image-autoload)
         :active              t
         :style               toggle
         :selected            (qwe-doc-image-autoload qwe-doc) ]
       [ "Display all images" (qwe-display-images)
         :active              (not (qwe-doc-image-autoload qwe-doc)) ]
       [ "Hide all images"    (qwe-hide-images)
         :active              (not (qwe-doc-image-autoload qwe-doc)) ]
       "---"
       [ "No border"          (qwe-set-image-border 0)
         :style               radio
         :selected            (= qwe-image-border 0) ]
       [ "Simple border"      (qwe-set-image-border 1)
         :style               radio
         :selected            (= qwe-image-border 1) ]
       [ "Double border"      (qwe-set-image-border 2)
         :style               radio
         :selected            (= qwe-image-border 2) ]
       )
     ( "Show delimiters as"
       [ "Text"               qwe-show-delimiters--text
         :style               radio
         :selected            (equal (qwe-doc-show-delimiters qwe-doc) 'text)
         :help "Show qwe delimiters as text" ]
       [ "Label"              qwe-show-delimiters--label
         :style               radio
         :selected            (equal (qwe-doc-show-delimiters qwe-doc) 'label)
         :help "Show qwe delimiters as small labels" ]
       [ "Invisible"          qwe-show-delimiters--invisible
         :style               radio
         :selected            (equal (qwe-doc-show-delimiters qwe-doc) 'invisible)
         :help "Hide qwe delimiters" ]
       )
     [ "Show element IDs"     qwe-toggle-show-element-ids
       :active                t
       :style                 toggle
       :selected              (qwe-doc-show-element-ids qwe-doc)
       :help "Show / Hide element IDs" ]
     [ "Show links"           qwe-toggle-show-links
       :active                t
       :style                 toggle
       :selected              (qwe-doc-show-links qwe-doc)
       :help "Show / Hide links" ]
     [ "Show format text"     qwe-toggle-show-format-text
       :active                t
       :style                 toggle
       :selected              (qwe-doc-show-format-text qwe-doc)
       :help "Show / Hide format text" ]
     [ "Enable hidesow compatibility" qwe-toggle-hideshow-compatibility
       :active                t
       :style                 toggle
       :selected              qwe-enable-hideshow
       :help "Enables / disables hideshow compatibility" ]
     [ "Customize QWE"        (customize-group 'qwe) ]
     )))


;;.sec 9.7. Colophon

(provide 'qwe-ui)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-ui.el ends here
