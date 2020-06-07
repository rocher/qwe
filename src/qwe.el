;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe.el}
;;.cfg.prj.file.desc      {t/QWE}{e/'s not }{t/WEB}{e/ for }{t/Emacs}
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
;;.cfg.doc.master   yes
;;.cfg.doc.style    book
;;.cfg.doc.title    QWE
;;.cfg.doc.subtitle QWE's not WEB for Emacs
;;.cfg.header.filename ../header.qwe
;;.cfg.header.end   [[qwe:hdr-update][update]]
;;.cmt _____________________________________________________________________________
;;.cmt
;;[[image-center:../img/qwe.png]]
;;
;;[[image-center:../img/qwe-title.png]]
;;.cmt
;;.quo                                  There are two familiar variants of
;;.quo                        the documentation problem: a) you write all the
;;.quo                        internal documentation that you know you need
;;.quo                        and you pay a terrible price for it, or b) you
;;.quo                        don't write all the internal documentation you
;;.quo                        need and you pay a terrible price for that.
;;.cmt
;;.quo                                  {i/Tom DeMarco}
;;.quo                                  Why Does Software Cost So Much?
;;.quo                                  Dorset House, 1995
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       1. Introduction
;;
;;.toc.sec             1.1. Module Dependencies
;;
;;.toc.sec             1.2. Debug Functions
;;
;;.toc.chp       2. QWE Minor Mode
;;
;;.toc.sec             2.1. qwe-mode
;;.toc.sse                   2.1.1. qwe-mode--enable
;;.toc.sse                   2.1.2. qwe-mode--disable
;;.toc.sse                   2.1.3. Subdocuments
;;
;;.toc.chp       C. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.chp 1. Introduction
;;.cmt
;;.quo                                  Common types of comments used are
;;.quo                        prologue comments and in-line
;;.quo                        comments. Prologue comments precede a program
;;.quo                        or module and describe goals. In-line comments,
;;.quo                        within the program code, describe how these
;;.quo                        goals are achieved.
;;.cmt
;;.quo                                  {i/Penny Grubb and Armstrong Takang}
;;.quo                                  Software Maintenance: Concepts and Practice, 2003
;;
;; This file is the main entry point to the implementation of {t/QWE} for
;; {t/Emacs}. The implementation is divided into modules, each one contained in a
;; separate file, implementing a coherent set of functions. The document as a
;; whole is not intended to be {e/compilation unit}, so every file has its own
;; entity when it comes to the {t/Elisp} interpreter.
;;
;;.par QWE for QWE
;; {t/QWE} is implemented using {t/QWE} itself (couldn't be another way). You should
;; first install and use {t/QWE} to better browse the code.
;;
;;
;;.sec 1.1. Module Dependencies
;; These are required modules for {t/QWE} to work. Additional extensions must be
;; loaded separately.

(require 'qwe-data)       ; Data structures
(require 'qwe-ui)         ; User interface
(require 'qwe-section)    ; Sectioning
(require 'qwe-faces)      ; Faces (a huge set of)
(require 'qwe-font-lock)  ; Font-lock definitions
(require 'qwe-link)       ; Links and references
(require 'qwe-cfg)        ; CORE Extension: QWE config
(require 'qwe-toc)        ; CORE Extension: QWE ToC


;;.sec 1.2. Debug Functions
;; This functions are used for debugging purposes.

(setq qwe-debug-type-list nil)

(defun qwe-debug-add-type (type)
  (add-to-list 'qwe-debug-type-list type))

(defun qwe-debug-del-type (type)
  (delete type qwe-debug-type-list))

(defun qwe-debug (type &rest args)
  (when (member type qwe-debug-type-list)
    (eval `(lwarn type :debug ,@args))))

(defun qwe-debug-enable ()
  (setq warning-minimum-log-level :debug)
  (setq warning-minimum-level :debug)
  (qwe-debug-add-type 'qwe))

;; Use this link to evaluate previous function [[elisp:][(qwe-debug-enable)]]


;;.chp 2. QWE Minor Mode
;;.cmt
;;.quo                                  Minor modes are optional features
;;.quo                        which you can turn on or off. [...]  All the
;;.quo                        minor modes are independent of each other and
;;.quo                        of the selected major mode.
;;.cmt
;;.quo                                  {i/The Emacs Editor}
;;.quo                                  [[info:][(emacs)Minor Modes]]
;;
;; Here you will find several useful functions to start using {t/QWE} mode. The
;; most important one is {t/qwe-mode}.
;;
;;.sec.fn 2.1. qwe-mode
;; This is the main entry point to the {t/QWE} {e/midor mode}. This function brings
;; the users all the functionality of the {t/QWE} Emacs minor mode. It is defined
;; using the lisp macro {t/define-minor-mode}.
;;
;;.par References
;; [[info:][(elisp)Defining Minor Modes]]

(define-minor-mode qwe-mode
  "QWE mode.
With no argument, this command toggles QWE mode. Non-null
prefix-argument turns on QWE mode. Null prefix argument turns
off QWE mode."

  :init-value nil
  :lighter " qwe" ;;.tip indicator for the mode line
  :keymap qwe-mode-map
  :group 'qwe
  :type 'boolean
  :require 'qwe

  (when qwe-mode
    (qwe-debug 'qwe "qwe-mode turned on")
    (qwe-mode--enable))

  (when (not qwe-mode)
    (lwarn 'qwe :debug "qwe-mode turned off")
    (qwe-mode--disable)))

;; Register minor mode keymaps for {t/QWE}.

(add-to-list 'minor-mode-map-alist (cons 'qwe-mode qwe-mouse-map))

;;.sse.fn 2.1.1. qwe-mode--enable
;; To start using {t/QWE} on a given file it is necessary to initialize some
;; variables. Each document has a buffer-local variable called {t/qwe-doc}. See
;; section [[file:qwe-data.el,sec:][Document Structure]] for more information.

(defun qwe-mode--enable ()

  ;;.par Document structure
  ;; Create a new buffer-local document structure and fill it with default
  ;; values or values obtained from file-local variables and config items.

  (make-variable-buffer-local 'qwe-doc)
  (setq qwe-doc (make-qwe-doc))

  ;;.par Delimiters
  ;; All {t/QWE} comments are delimited by default by the string {t/`;qwe'}. It is
  ;; known as the {e/qwe-delimiter} string and consists of two different parts:
  ;;
  ;;    1. {t/`;'} is the {e/inline-comment} delimiter and depends exclusively on the
  ;;         programming language you are using {t/QWE} with.
  ;;
  ;;    2. {t/`qwe'} is the user customizable {e/delimiter tag}. It is used to
  ;;         actually differentiate {e/normal} inline comments from {t/QWE} stuff.
  ;;
  ;; Different {t/QWE} documents on the same programming language can be
  ;; configured to use different {e/qwe-delimiters}.

  (setf (qwe-doc-comment-delimiter qwe-doc)
        (cdr (assoc major-mode qwe-comment-delimiter-for-mode-alist)))
  (setf (qwe-doc-delimiter-tag qwe-doc) qwe-delimiter-tag)
  (setf (qwe-doc-delimiter qwe-doc) (concat (qwe-doc-comment-delimiter qwe-doc)
                                            (qwe-doc-delimiter-tag qwe-doc)))

  ;;.del ;;.par Regexps
  ;;.del ;; Regular expression to find {t/QWE} delimiters on the current document. It
  ;;.del ;; depends on the programming language and on {t/qwe-delimiter-tag}.
  ;;.del
  ;;.del (setf (qwe-doc-delimiter-regexp qwe-doc)
  ;;.del       (concat "^[ \t]*"
  ;;.del               (qwe-doc-comment-delimiter qwe-doc)
  ;;.del               (qwe-doc-delimiter-tag qwe-doc)
  ;;.del               "\\(\\.\\(" qwe-context-regexp "\\)\\)+ "))

  ;;.par Display
  ;; Initial display of {t/QWE} delimiters and format text.
  ;;.war obsolete initialization
  ;;.del (setf (qwe-doc-show-delimiters qwe-doc) qwe-show-delimiters)     ;;.tip init delimiter
  ;;.del (qwe-show-delimiters (qwe-doc-show-delimiters qwe-doc) t)
  ;;.del (setf (qwe-doc-show-element-ids qwe-doc) qwe-show-element-ids)   ;;.tip init element-id
  ;;.del (qwe-show-element-ids (qwe-doc-show-element-ids qwe-doc) t)
  ;;.del (setf (qwe-doc-show-links qwe-doc) qwe-show-link-ctors)     ;;.tip init link-ctors
  ;;.del (qwe-show-links (qwe-doc-show-link-ctors qwe-doc) t)
  ;;.del (setf (qwe-doc-show-format-text qwe-doc) qwe-show-format-text) ;;.tip init format-text
  ;;.del (qwe-show-format-text (qwe-doc-show-format-text qwe-doc) t)

  ;;.par Document style

  (setf (qwe-doc-style qwe-doc)
        (or (intern-soft (qwe-cfg-get-value "doc.style"))
            qwe-default-doc-style))

  ;;.par Font-Lock mode
  ;; Enabling {e/font-lock} for {t/QWE}. Here we store the original value of
  ;; {t/font-lock-extra-managed-props}. It will be restored when exiting
  ;; {t/qwe-mode}.

  ;;.del (setf (qwe-doc-font-lock-keywords qwe-doc)
  ;;.del       (qwe-font-lock-keywords-for-major-mode (qwe-doc-delimiter qwe-doc)))
  ;;.del (qwe-doc-comment-delimiter qwe-doc)
  (setf (qwe-doc-font-lock-extra-managed-props qwe-doc)
        font-lock-extra-managed-props)
  (setq font-lock-extra-managed-props '(display ;;.todo must depend on qwe-image-autoload
                                        front-sticky
                                        invisible
                                        mouse-face
                                        pointer
                                        rear-nonsticky
                                        read-only))
  (add-to-invisibility-spec 'qwe--delimiter)

  ;;.todo (setq font-lock-multiline t)

  (font-lock-add-keywords nil (qwe-font-lock-keywords-for-major-mode
                               (qwe-doc-delimiter qwe-doc)))
  ;;.del (redisplay))

  ;;.par Visualization

  (let ((cfg-item nil))
    (setq cfg-item (qwe-cfg-get-value "doc.show.delimiters"))
    (qwe-show-delimiters (if cfg-item
                             (intern-soft cfg-item)
                           qwe-show-delimiters) t)
    (setq cfg-item (qwe-cfg-get-value "doc.show.format-text"))
    (qwe-show-format-text (if cfg-item
                              (string= "yes" cfg-item)
                            qwe-show-format-text) t)
    (setq cfg-item (qwe-cfg-get-value "doc.show.element-ids"))
    (qwe-show-element-ids (if cfg-item
                              (string= "yes" cfg-item)
                            qwe-show-element-ids) t)
    (setq cfg-item (qwe-cfg-get-value "doc.show.links"))
    (qwe-show-links (if cfg-item
                        (string= "yes" cfg-item)
                      qwe-show-links) t)
    (setq cfg-item (qwe-cfg-get-value "doc.image.autoload"))
    (setf (qwe-doc-image-autoload qwe-doc)
          (if cfg-item
              (string= "yes" cfg-item)
            qwe-image-autoload))
    (setq cfg-item (qwe-cfg-get-value "doc.image.border"))
    (setf (qwe-doc-image-border qwe-doc)
          (if cfg-item
              (string-to-number cfg-item)
            qwe-image-border)))

  (unless (qwe-doc-image-autoload qwe-doc)
    (if (string= "yes" (qwe-cfg-get-value "doc.image.display"))
        (qwe-display-images)
      (qwe-hide-images)))

  ;;.par Additional information
  ;; Initialize document internal structure components and modification

  (setf (qwe-doc-chars-modified-tick qwe-doc) (buffer-chars-modified-tick))

  ;;.wip++ Initialize fill regexps
  ;;
  ;;.war Some incompatibilities detected
  ;;      After loading {t/QWE}, text-mode buffers cannot format paragraphs
  ;;      properly any more  (this happened once, al least)

  (setq adaptive-fill-regexp (concat  ;;.fix not so sure
                              "[ \t]*"
                              (qwe-doc-delimiter qwe-doc)
                              "\\(?:\\.[^ \t]+\\)?[ \t]+"))
  (setq paragraph-separate (concat
                            "[ \t]*"
                            (qwe-doc-delimiter qwe-doc)
                            "\\(?:[ \t]*$\\|\\." qwe-section-regexp "\\)"))
  (setq paragraph-start (concat
                         "[ \t]*"
                         (qwe-doc-delimiter qwe-doc)
                         "\\(\f\\|[     ]*$\\|\\*+ \\|\f\\|[    ]*\\([-+*][     ]+\\|[0-9]+[.)][        ]+\\)\\|[       ]*[:|]\\)"))
  ;;.wip-- All that stuff should depend on a custom variable value??

  ;;.par Hooks
  ;; Now that {t/qwe-doc} is initialized, run hooks

  (run-hooks 'qwe-mode-enable-hook)

  ;;.wip Skip header
  (if (qwe-cfg-get-value "header.skip")
    (progn
      (goto-char (point-min))
      (re-search-forward "\\.cfg\\.header\\.skip")
      (forward-line (1+ scroll-margin))
      (recenter 1))
    (redisplay))
  ;;.wip end

  (font-lock-fontify-buffer)
  )

;;.sse.fn 2.1.2. qwe-mode--disable

(defun qwe-mode--disable ()
  (run-hooks 'qwe-mode-disable-hook)
  (qwe-show-delimiters 'text)
  (qwe-show-format-text 'text)
  (font-lock-fontify-buffer)
  ;;.del (font-lock-remove-keywords nil (qwe-doc-font-lock-keywords qwe-doc))
  (font-lock-remove-keywords nil (qwe-font-lock-keywords-for-major-mode
                                  (qwe-doc-delimiter qwe-doc)))
  ;;.todo (setq font-lock-multiline qwe-font-lock-multiline)
  (setq font-lock-extra-managed-props
        (qwe-doc-font-lock-extra-managed-props qwe-doc))
  (font-lock-fontify-buffer))

;;.sse 2.1.3. Subdocuments
;; {t/QWE} documents can contain several {e/sub-documents}. A sub-document is nothing
;; else than another {t/QWE} document. The only difference is that when a
;; document is included into another master document, it shares section
;; numbers and is called {e/sub-document}.
;;
;; These are the sub-documents included by this master document. Includes
;; directives automatiaclly become links to the referred files (try to click
;; their names).
;;
;;.#include qwe-lang.el
;;.#include qwe-data.el
;;.#include qwe-section.el
;;.#include qwe-faces.el
;;.#include qwe-font-lock.el
;;.#include qwe-link.el
;;.#include qwe-ui.el
;;.#include qwe-exi.el
;;.#appendix
;;.#include ../ext/qwe-cfg.el
;;.#include ../ext/qwe-toc.el
;;
;; In this case each sub-document contins a specific functionality o a
;; coherent set of functions or data structures. But don't be confused: it
;; has nothing to do with the {t/Elisp} interpreter, but only with the document
;; contents and generation. Modules are used by this file [[sec:Module Dependencies][the usual way]].


;;.chp C. Colophon

(provide 'qwe)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode qwe-show-delimiters: invisible
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode End:
;;.bbx qwe.el ends here
