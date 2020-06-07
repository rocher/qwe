;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-data.el}
;;.cfg.prj.file.desc      Data structures, constants and global variables
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
;;.chp 4. Constants, Variables and Data Structures
;;.cmt
;;.todo Quotation here
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       4. Constants, Variables and Data Structures
;;
;;.toc.sec             4.1. Introduction
;;.toc.sse                   4.1.1. Module dependencies
;;
;;.toc.sec             4.2. Constants
;;.toc.sse                   4.2.1. qwe-version
;;
;;.toc.sec             4.3. Customization Group
;;
;;.toc.sec             4.4. Custom Variables
;;.toc.sse                   4.4.1. Default Document Values
;;.toc.sss                         4.4.1.1. qwe-default-doc-style
;;.toc.sss                         4.4.1.2. qwe-delimiter-tag
;;.toc.sse                   4.4.2. Initial Values
;;.toc.sss                         4.4.2.1. qwe-show-delimiters
;;.toc.sss                         4.4.2.2. qwe-show-element-ids
;;.toc.sss                         4.4.2.3. qwe-show-links
;;.toc.sss                         4.4.2.4. qwe-show-format-text
;;.toc.sss                         4.4.2.5. qwe-image-autoload
;;.toc.sss                         4.4.2.6. qwe-image-border
;;.toc.sss                         4.4.2.7. qwe-enable-hideshow
;;.toc.sse                   4.4.3. Other Variables
;;.toc.sss                         4.4.3.1. qwe-comment-delimiter-for-mode-alist
;;
;;.toc.sec             4.5. Document Structure
;;
;;.toc.sec             4.6. Hooks
;;
;;.toc.sec             4.7. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec 4.1. Introduction
;; This chapter defines constants, variables and data structures needed by
;; {t/QWE}. Here are only generic definitions. Other modules define their own
;; data structures, custom variables, etc. in the file they are contained.
;;
;;.sse 4.1.1. Module dependencies

(eval-when-compile
  (require 'cl))


;;.sec 4.2. Constants
;;
;;.sse.const 4.2.1. qwe-version

(defconst qwe-version "0.9.5")


;;.sec 4.3. Customization Group
;; This is the main customization group under which all customizable items of
;; {t/QWE} shall appear. Other modules may define their own customization groups
;; under this group.

(defgroup qwe nil
  "Qwe's not Web for Emacs."
  :group 'tools
  :version "23.1")


;;.sec 4.4. Custom Variables
;; These are user-customizable variables. Other modules may define their own
;; customization variables under their customization groups.
;;
;;.sse 4.4.1. Default Document Values
;;
;;.sss.var 4.4.1.1. qwe-default-doc-style
;; The style of a document defines what sections can contain. It resembles
;; the {t/LaTeX} document class.
;;
;;.todo Add support for 'report' and 'tutorial' document style

(defcustom qwe-default-doc-style 'article
  "Default document style.

Style 'book' should be used on documents with chapters and
appendixes.  Style 'article' should be used on documents without
neither chapters nor appendixes.  Style 'custom' should be used
in conjunction with variable
`qwe-section-numbering-custom-list'."
  ;; See [[file:qwe-section.el,sss:][qwe-section-numbering-custom-list]]

  :type '(choice :tag "Style"
                 (const :tag "article" article)
                 (const :tag "book"    book)
                 ;;.todo (const :tag "report"  report)
                 ;;.todo (const :tag "tutorial" tutorial)
                 (const :tag "custom"  custom))
  :safe  (lambda (s)
            (and (symbolp s)
                 (or (equal s 'article)
                     (equal s 'book)
                     ;;.todo (equal s 'report)
                     ;;.todo (equal s 'tutorial)
                     (equal s 'custom))))
  :group 'qwe)

;;.sss.var 4.4.1.2. qwe-delimiter-tag

(defcustom qwe-delimiter-tag "qwe"
  "Default string used as a delimiter tag.

Delimiter tags can be empty strings, as well, or any string of
your convenience. Ususally, the shorter the better."
  :type  'string
  :safe   (lambda (s) (stringp s))
  :group 'qwe)

;;.sse 4.4.2. Initial Values
;;
;;.sss.var 4.4.2.1. qwe-show-delimiters

(defcustom qwe-show-delimiters 'text
  "Hide or show delimiters, shown as text or labels.

Delimiters can be displayed in three different ways.  `text'
shows them as they are written and let you see the real contents
of the document.  `label' displays delimiters as little labels,
indicating in some way the nature of the tag.  `invisible'
completely hides delimiters."
  :type '(choice :tag "Show"
                 (const :tag "text"      text)
                 (const :tag "label"     label)
                 (const :tag "invisible" invisible))
  :safe   (lambda (s)
            (and (symbolp s)
                 (or (equal s 'text)
                     (equal s 'label)
                     (equal s 'invisible))))
  :group 'qwe)

;;.sss.var 4.4.2.2. qwe-show-element-ids

(defcustom qwe-show-element-ids nil
  "Show or hide element IDs."

  :type  'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe)

;;.sss.var 4.4.2.3. qwe-show-links

(defcustom qwe-show-links nil
  "Show or hide link contructors."

  :type  'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe)

;;.sss.var 4.4.2.4. qwe-show-format-text

(defcustom qwe-show-format-text nil
  "Show or hide format text codes.

Format text codes are those used to format some parts of the text
and to write links and anchors. This variable controls the way
these characters ara initially displayed."
  :type  'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe)

;;.sss.var 4.4.2.5. qwe-image-autoload

(defcustom qwe-image-autoload t
  "Autoload images.

If t, all links referring to images will be automatically
loaded."
  :type 'boolean
  :safe (lambda (b) (booleanp b))
  :group 'qwe)

;;.sss.var 4.4.2.6. qwe-image-border

(defcustom qwe-image-border 0
  "Border width."
  :type 'integer
  :safe(lambda (n) (and (integerp n)
                        (<= 0 n)
                        (<= n 2)))
  :group 'qwe)

;;.sss.var 4.4.2.7. qwe-enable-hideshow

(defcustom qwe-enable-hideshow nil
  "Enable hideshow compatibility"
  :type 'boolean
  :safe (lambda (b) (booleanp b))
  :group 'qwe)

;;.sse 4.4.3. Other Variables
;;
;;.sss.var 4.4.3.1. qwe-comment-delimiter-for-mode-alist

(defcustom qwe-comment-delimiter-for-mode-alist
  '((ada-mode . "--")
    (asm-mode . ";")
    (autoconf-mode . "dnl ")
    (awk-mode . "#")
    (bibtex-mode . "@COMMENT ")
    (c-mode . "//")
    (c++-mode . "//")
    (cfengine . "#")
    (change-log-mode . ";;")
    (common-lisp-mode . ";")
    (cperl-mode . "#")
    (dcl-mode . "$! ")
    (delphi-mode . "//")
    (dns-mode . ";")
    (doctex-mode . "%")
    (dsssl-mode . ";")
    (erlang-mode . "%")
    (emacs-lisp-mode . ";")
    (f90-mode . "!")
    (fortran-mode . "!")
    (gdb-script-mode . "#")
    (icon-mode . "#")
    (idl-mode . "//")
    (idlwave-mode . ";")
    (java-mode . "//")
    (jython-mode . "#")
    (LaTex-mode . "%")
    (ledit-mode . ";")
    (lisp-mode . ";")
    (lisp-interaction-mode . ";")
    (m4-mode . "#")
    (makefile-mode . "#")
    (makefile-automake-mode . "#")
    (makefile-bsdmake-mode . "#")
    (makefile-gmake-mode . "#")
    (makefile-imake-mode . "#")
    (makefile-makepp-mode . "#")
    (metafont-mode . "%")
    (metapost-mode . "%")
    (mixal-mode . "*")
    (objc-mode . "//")
    (octave-mode . "#")
    (perl-mode . "#")
    (pike-mode . "//")
    (plain-TeX-mode . "%")
    (plain-tex-mode . "%")
    (prolog-mode . "%")
    (ps-mode . "%")
    (python-mode . "#")
    (ruby-mode . "#")
    (scheme-mode . ";")
    (sh-mode . "#")
    (shell-script-mode . "#")
    (sieve-mode . "#")
    (simula-mode . "!")
    (slitex-mode . "%")
    (snmp-mode . "--")
    (sql-mode . "--")
    (tcl-mode . "#")
    (TeX-mode . "%")
    (tex-mode . "%")
    (text-mode . "")
    (texinfo-mode . "@c")
    (vhdl-mode . "--")
    (zone-mode . ";"))

  "Alist of major modes and inline comment delimiters.

Inline comments are those that use an arbitrary delimiter or
sequence of tokens to indicate the beginning of a comment and
extends up to the end of line.

This variable contains the inline comment delimiters of
programming languages supported by an Emacs' major mode.

Please note that at the moment of this writing, some programming
languages are not yet supported by Emacs with a major mode
designed to this end. Write to the author of QWE to request
support for them in future releases (please, don't ask for
support for INTERCAL, FALSE, Befunge, Brainfuck or Whitespace)."

  :type '(repeat (cons :format "%v"
                       (symbol :tag "Mode")
                       (sexp :tag "String")))
  :group 'qwe)


;;.sec 4.5. Document Structure
;; This is the structure that contains all information of a {e/document}. In this
;; context, a document is any buffer with {t/qwe-mode} minor mode enabled. Every
;; document has a buffer local variable of that type. See [[file:qwe.el,a:][qwe-doc creation]].
;;
;; Every row of the table below (contained within the structure definition)
;; shows a document variable, its default value and the name of the custom
;; variable used to initilize its value.

(defstruct qwe-doc

  ;; {T/ Attrib                       | Def  | Custom Variable       | cfg-item             }
  ;; {S/ Document style and numbering                                                       }
  (style                         nil)  ;; {R/ qwe-default-doc-style  }{y/  cfg.doc.style                       }
  (numbering                     nil)  ;; {R/                        }
  (initial-number                1)    ;; {R/                        }
  (initial-letter                ?A)   ;; {R/                        }
  (final-number                  nil)  ;; {R/                        }
  (final-letter                  nil)  ;; {R/                        }
  ;; {S/ Master/slave documents                                                             }
  (master                        nil)  ;; {R/                        }{g/  cfg.doc.master                    }
  (parent                        nil)  ;; {R/                        }{g/  cfg.doc.parent                     }
  ;; {S/ Delimiter strings                                                                  }
  (comment-delimiter             nil)  ;; {R/                        }
  (delimiter-tag   qwe-delimiter-tag)  ;; {R/                        }
  (delimiter                     nil)  ;; {R/                        }
  (delimiter-regexp              nil)  ;; {R/                        }
  ;; {S/ Presentation                                                                       }
  (show-delimiters                      qwe-show-delimiters)   ;; {o/ cfg.doc.show.delimiters       }
  (show-format-text                     qwe-show-format-text)  ;; {o/ cfg.doc.show.format-text  }
  (show-element-ids                     qwe-show-element-ids)  ;; {o/ cfg.doc.show.element-ids    }
  (show-links                           qwe-show-links)        ;; {o/ cfg.doc.show.links                }
  (enable-hideshow                      qwe-enable-hideshow)   ;;
  (image-autoload                       qwe-image-autoload)    ;; {o/ cfg.doc.image.autoload       }
  (image-border                         qwe-image-border)      ;; {o/ cfg.doc.image.border           }
  ;; {S/ Font-Lock support                                                                  }
  (font-lock-extra-managed-props nil)  ;; {R/                        }
  ;; {S/ Information                                                                        }
  (chars-modified-tick          0)     ;;  {R/                        }
  ;; {T/                                                                                                  }
  )


;;.sec 4.6. Hooks
;; These are standard hooks provided by {t/QWE}.

(defvar qwe-mode-enable-hook nil
  "Normal hook run just after qwe-mode has been activated.")

(defvar qwe-mode-disable-hook nil
  "Normal hook run just before qwe-mode disabling.")


;;.sec 4.7. Colophon

(provide 'qwe-data)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-data.el ends here
