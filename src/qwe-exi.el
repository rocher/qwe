;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-exi.el}
;;.cfg.prj.file.desc      {t/QWE} Emacs Extesion Interface
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
;;
;;.note Preamble

(eval-when-compile
  (require 'cl)
  (require 'qwe-faces)
  (require 'qwe-font-lock)
  (require 'qwe-lang))

;;.toc.begin                            Table of Contents
;;
;;.toc.chp       10. QWE Emacs Extension Interface
;;
;;.toc.sec             10.1. Creating new Contexts
;;.toc.sse                   10.1.1. qwe-exi-new-context
;;.toc.sse                   10.1.2. qwe-exi-new-context-kb
;;
;;.toc.sec             10.2. Adding new Hyperlink Types
;;
;;.toc.sec             10.3. Working with Menus
;;.toc.sse                   10.3.1. New Menu Entries
;;.toc.sse                   10.3.2. Inserting Sub-Menus
;;
;;.toc.sec             10.4. Customization
;;
;;.toc.sec             10.5. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.chp 10. QWE Emacs Extension Interface
;;.cmt
;;.quo                                  Numerous interfaces (i.e. entry
;;.quo                        points) are cited frequently by developers when
;;.quo                        they describe their intuitive definition of
;;.quo                        code decay. As the number of interfaces
;;.quo                        increases, increasing attention must be
;;.quo                        directed to possible side-effects of changes in
;;.quo                        other sections of code.
;;.cmt
;;.quo                                  {i/Eick et al.}
;;.quo                                  Does Code Decay?
;;.quo                                  IEEE Transactions on Software Engineering, January 2001
;;
;; This is {t/EXI}, the very first {t/QWE} {e/EXtension Interface} implementation that
;; let users or programmers extend {t/QWE} core functionality, write new
;; extensions (called {e/contexts}) to interact with {t/QWE} from other applications
;; or from other Emacs modules. Few contexts have been implemented, at the
;; moment of this writing, distributed along with {t/QWE}: {t/'qwe-toc'}, to
;; automatically generate tables of contents; {t/'qwe-cfg'}, to manage
;; configuration items used by other contexts; and {t/QWEB}, a literate
;; programming system implemented on top of {t/QWE}.
;;
;; The author has in mind a couple of new contexts: {t/qwe-dox} and {t/qwe-org}. The
;; former let users interact with {t/doxygen} from {t/QWE} with few commands. The
;; later let users interact from and to {t/'org-mode'} buffers. If you are
;; interested in some of these extensions, contact the author to help to
;; define how would be these interactions, or to collaborate with the
;; implementation.
;;
;; So as these two extensions have different needs, the hope is that if the
;; extension interface is enough for them, then almost any othe extension
;; will have enough too.
;;
;; In a not so far future, once {t/QWE} had been implemented in a number of
;; editors, {t/QWE} might have its own {e/extension language} independent of the
;; facilities provided by the target editor it is running under.
;;
;;
;;.sec 10.1. Creating new Contexts
;; The first thing every new extension needs is to define its own
;; {e/context}. This will let it work with a unique set of {e/qwe-tags}. For
;; instance, {t/QWE} delimiters of {t/qwe-toc} extension look like:
;;
;;    {e/xx}{t/.qwe.toc}{e/.yyy}
;;
;; where {e/xx} depends on the programming language being used and {e/yy} is used by
;; {t/qwe-toc}. {t/QWE} transparently treats these delimiters as if they were {e/normal}
;; delimiters, but does nothing with them. {t/qwe-toc} is responsible to do
;; something useful with them.
;;
;;.sse.fn 10.1.1. qwe-exi-new-context

(defun qwe-exi-new-context (name abbrev label keyword-list text-list)
  ;;
  ;; {t/keyword-list } is {t/ nil } or an association list of the form
  ;;
  ;;.ver (("keyword-1" "keyword-2" .. "keyword-N") . face)
  ;;
  ;; {t/text-list } is an association list of the form
  ;;.ver  ((regexp . face) (regexp . face) .. )
  ;;
  (qwe-lang-new-elt-context name abbrev label nil keyword-list text-list)
  (qwe-lang-new-velt name))

;;.sse.fn 10.1.2. qwe-exi-new-context-kb

(defun qwe-exi-new-context-kb (name abbrev label keyword-list)
  "Creates a new context based on a list of keywords.

Keyword based contexts fontify their accompanying text depending
on the keyword matched."
  (qwe-lang-new-elt-context name abbrev label t keyword-list nil)
  (qwe-lang-new-velt name))

;;.sec 10.2. Adding new Hyperlink Types
;; Some extensions need new hyperlinks types. In fact, extensions could be
;; used just for this purpose, and nothing else. This would be the case in
;; which the extensions is used to link to another Emacs mode or even an
;; external application. Syntax rules used to write hyperlinks based on new
;; definitions created with this interface are the same. The mechanism used
;; is based on the {e/link type} you write. That is:
;;
;;.ver    [[type:reference]]
;;.ver    [[type:][reference]]
;;.ver    [[type:reference][description]]
;;
;; where {t/type} depends on the link type defined.
;;
;;.par.fn qwe-face-spec-exi-link
;; Use this function to create a face-spec. This face-spec mut be then used
;; at the moment of the context creation. See section [[file:qwe-toc.el,sec:][QToC Elements]] for a
;; working example.

(defun qwe-face-spec-exi-link (face callback &optional help &rest properties)
  `(face ,face
         callback ,callback
         follow-link t
         help-echo ,(or help "mouse-1: follow this link")
         keymap qwe-mouse-map
         pointer hand
         ,@properties))


;;.sec 10.3. Working with Menus
;;
;;.sse 10.3.1. New Menu Entries
;;
;;.par.fn qwe-exi-add-menu-bar

(defun qwe-exi-add-menu-bar (symbol menu menu-doc)
  (let ((temp-def-key nil))
    (fset 'temp-def-key (symbol-function 'define-key))
    (fset 'define-key (symbol-function 'define-key-after))
    (easy-menu-do-define symbol qwe-mode-map menu-doc menu)
    (fset 'define-key (symbol-function 'temp-def-key))))

;;.sse 10.3.2. Inserting Sub-Menus
;;
;;.par.fn qwe-exi-add-to-menu-preferences

(defun qwe-exi-add-to-menu-preferences (name callback)
  (easy-menu-add-item qwe-menu
                      '("Preferences")
                      `[,name ,callback t]
                      "Customize QWE"))

(defun qwe-exi-add-to-menu-preferences-item (item)
  (easy-menu-add-item qwe-menu
                      '("Preferences")
                      item
                      "Customize QWE"))

;;.par.fn qwe-exi-delete-from-menu-preferences

(defun qwe-exi-delete-from-menu-preferences (menu-item)
  (easy-menu-remove-item qwe-menu '("Preferences") menu-item))


;;.sec 10.4. Customization
;; Extensions can use this customization group to define variables, faces
;; and, in general, any customizable object. Using this customization group
;; users will be able to customize several extensions in a unique
;; place. However, if an extension has a large set of customizable objects,
;; then might be better for it to have its own customization group.

(defgroup qwe-ext nil
  "Customizable objects of extensions."
  :group 'qwe-ext)


;;.sec 10.5. Colophon

(provide 'qwe-exi)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-exi.el ends here
