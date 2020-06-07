;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-lang.el}
;;.cfg.prj.file.desc      QWE Language definitions
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
;;.chp QWE Language Definitions
;;.cmt
;;.quo                                  I use the following list of
;;.quo                        requirements to imply a definition of a
;;.quo                        literate program and the minimum set of tools
;;.quo                        which are needed to prepare, use, and study the
;;.quo                        resulting code:
;;.quo                             The high-level language code and the
;;.quo                        system documentation of the program come from
;;.quo                        the same set of source files.
;;.quo                                  [...]
;;.quo                             Cross references, indices, and different
;;.quo                        fonts for text, high-level language keywords,
;;.quo                        variable names, and literals should be
;;.quo                        reasonably automatic and obvious in the source
;;.quo                        and the documentation.
;;.cmt
;;.quo                                  {i/Bart Childs}
;;.quo                                  Literate Programming, A Practitioner's View
;;.quo                                  Tugboat, December 1992
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       QWE Language Definitions
;;
;;.toc.sec             Introduction
;;.toc.sse                   Module dependencies
;;
;;.toc.sec             Language Classes and Elements
;;.toc.sse                   Structures
;;.toc.sss                         qwe-lang-class
;;.toc.sss                         qwe-lang-elt
;;.toc.sss                         qwe-lang-elt-context
;;.toc.sse                   Constants and Variables
;;.toc.sss                         qwe-lang-elt-attribute-list
;;.toc.sss                         qwe-lang-class-list
;;.toc.sss                         qwe-lang-class-alist
;;.toc.sss                         qwe-lang-class-C-list
;;.toc.sss                         qwe-lang-class-C-alist
;;.toc.sss                         qwe-lang-elt-list
;;.toc.sss                         qwe-lang-elt-alist
;;.toc.sse                   Language class constructor
;;.toc.sse                   Language element base constructor
;;.toc.sse                   Language element constructor
;;.toc.sse                   Context element constructor
;;
;;.toc.sec             Optional Designators
;;.toc.sse                   Identifiers (IDs)
;;.toc.sss                         qwe-lang-ID-regexp
;;.toc.sss                         qwe-lang-opt-ID-regexp
;;.toc.sse                   Interaction with Programming Languages (PL)
;;.toc.sss                         qwe-lang-PL-elts-list
;;.toc.sss                         qwe-lang-PL-elts-regexp
;;.toc.sss                         qwe-lang-opt-PL-elts-regexp
;;.toc.sse                   Context identifiers list
;;
;;.toc.sec             Visual Elements
;;.toc.sss                         qwe-lang-new-velt
;;
;;.toc.sec             Functions to work with language elements
;;.toc.sse                   qwe-lang-class-name-p
;;.toc.sse                   qwe-lang-elt-name-p
;;.toc.sse                   qwe-lang-elt-attribute-name-p
;;.toc.sse                   qwe-lang-elt-get-attribute
;;.toc.sse                   qwe-lang-elt-set-attribute
;;.toc.sse                   qwe-lang-regexp
;;.toc.sse                   qwe-lang-class-regexp
;;.toc.sse                   qwe-lang-elt-regexp
;;.toc.sse                   qwe-lang-elt-re-search
;;
;;.toc.sec             Module Initialization
;;.toc.sse                   Creation of language classes
;;.toc.sse                   Declaration of dynamic variables
;;.toc.sss                         qwe-lang-class-C-list
;;.toc.sss                         qwe-lang-class-C-alist
;;.toc.sse                   Creation of language elements
;;.toc.sss                         {e/Core} language elements
;;.toc.sss                         Format text
;;.toc.sss                         Creation of visual elements
;;
;;.toc.sec             Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec Introduction
;; This file contains main {t/QWE} language definitions. Language constructs are
;; grouped togethter into several {e/language classes}. Each class contains
;; several {e/language element} descriptors. In turn, each descriptor is a set of
;; attributes that characterizes a language element.
;;
;; Language elements described here don't include neither formatting
;; characters nor links. These are described later in other files.
;;
;;.sse Module dependencies
;; To properly compile this file some {t/'common-lisp'} features are required.

(eval-when-compile
  (require 'cl))


;;.sec Language Classes and Elements
;;
;;.sse Structures
;;
;;.sss.struct qwe-lang-class
;; Each language class is defined using an instance of this structure. Its
;; contents is slightly provisional, yet.

(defstruct (qwe-lang-class
            (:constructor nil)
            (:constructor new-qwe-lang-class (opt-IDs opt-PL-elts)))
  (opt-IDs     nil)    ; elements can have optional IDs
  (opt-PL-elts nil))   ; elements can have optional PL-elements

;;.sss.struct qwe-lang-elt
;; Each language element is defined using an instance of this structure. It
;; may change in a near future ...

(defstruct (qwe-lang-elt
            (:constructor nil)
            (:constructor new-qwe-lang-elt (class abbrev label)))
  (class  nil)
  (abbrev nil)
  (label  nil))

;;.sss.struct qwe-lang-elt-context
;; Contexts are special elements used by {t/QWE} extensions. Contexts can be
;; {e/normal} or {e/keyword-based}. The main difference between them is the way the
;; fontification takes place. Normal contexts define a list of kewords and a
;; list of text to match.

;; Users can use following form when using contexts:
;;
;;.ver  ;;qwe.ctx.key1.key2  Text or title ... up to the end of line.
;;
;; Depending on the extension, contexts can be explicitly defined so that not
;; all keywords are valid nor any text is acceptable.
;;
;; As an example of {e/normal} contexts we have {e/Config} and {e/WEB}. The first uses
;; any keyword and any text, so that any keyword represents a configuration
;; item which can take any string as value. The second, {e/WEB}, defines no valid
;; keywords but special text forms, like these:
;;
;;.ver    "\\`<.+>\\+?=\\'"  and  "\\`<.+>\\'"
;;
;; The first regular expression is used to match web-block definitions, like
;;
;;.Li1 {t/;;qwe.web <Variable Declaration>=    }  {e/first-time block}
;;.Li1 {t/;;qwe.web <Variable Instatiation>+=  }  {e/continuation block}
;;
;; whilst the second one is used to match web-block usage:
;;
;;.Li1 {t/;;qwe.web <Variable Declaration>}
;;.Li1 {t/;;qwe.web <Variable Instatiation>}
;;
;; Fontification occurs depending on the web-block type.
;;
;; {e/Table of} {e/Contents} context, for instance, is keyword-based. This means that
;; it defines a set of keywords, and text is fontified depending on the
;; keyword matched. The set of keywords might look like this:
;;
;;.ver    { "chp", "sec", "sse", "sss" }
;;
;; This way, in a line containig
;;
;;.ver    ;;qwe.toc.sse    3.2.1 Subsubsection title
;;
;; the text {e/'3.2.1 Subsubsection title'} is fontified with (previously defined
;; elsewhere) {t/'qwe-face-context-toc-sse'}. In general, keyword-based contexts
;; must define a face for every keyword with a name matching:
;;
;;.ver    qwe-face-context-NAME-KEYWORD
;;
;; where {e/NAME} is the name of the context and {e/KEYWORD} the currently matched
;; keyword. All this is better understood by looking at the {e/core} extension
;; source files that come with {t/QWE}.

(defstruct (qwe-lang-elt-context
            (:include qwe-lang-elt)
            (:constructor nil)
            (:constructor new-qwe-lang-elt-context (class
                                                    abbrev
                                                    label
                                                    &optional
                                                    keyword-based
                                                    keyword-list
                                                    text-list)))
  (keyword-based nil)
  (keyword-list nil)
  (text-list nil))

;;.sse Constants and Variables
;;
;;.sss.const qwe-lang-elt-attribute-list

(defconst qwe-lang-elt-attribute-list
  '(class abbrev label))

;;.sss.const qwe-lang-class-list
;; This is a plain list containing all class names. See [[sec:][Module Initialization]]
;; for more information.

(defvar qwe-lang-class-list nil)

;;.sss.var qwe-lang-class-alist
;; This variable is an assotiation list containing all language classes
;; defined. It can be accessed using {t/'assoc'}, in which case an instance of
;; the structure {t/'qwe-lang-class'} is obtained. Its value is computed at
;; [[sec:][Module Initialization]].

(defvar qwe-lang-class-alist nil)

;;.sss.var qwe-lang-class-C-list
;; This is a set of variables, one for each language class. They are created
;; end their value computed at [[sec:][Module Initialization]].
;;
;; They are plain lists containing all languages element names pertaining to
;; the class.
;;
;;.sss.var qwe-lang-class-C-alist
;; This is a set of variables created and computed at [[sec:][Module Initialization]].
;;
;; They are associative lists containing a {t/qwe-lang-elt} object for each
;; language element class.

;;.sss.var qwe-lang-elt-list
;; This variable contains a plain list with just the names of all language
;; elements defined.

(defvar qwe-lang-elt-list nil)

;;.sss.var qwe-lang-elt-alist
;; This variable is an assotiation list containing all language elements
;; defined. It can be accessed using {t/'assoc'}, in which case an instance of
;; the structure {t/'qwe-lang-elt'} is obtained.

(defvar qwe-lang-elt-alist nil)

;; For every class {t/C} defined, there is also defined its corresponding
;; association list of the form
;;
;;.ver    qwe-lang-class-C-alist
;;
;; Elements contained in these variables are the same contained in
;; {t/'qwe-lang-elt-alist}. So
;;
;;.ver    (assoc 'chapter qwe-lang-elt-alist)
;;
;; and
;;
;;.ver    (assoc 'chapter qwe-lang-class-section-alist)
;;
;; are exactly the same lisp object. Lists {t/'qwe-lang-class-C-alist} are
;; provided to iterate over all elements of the same class.

;;.sse.fn Language class constructor

(defun qwe-lang-new-class (class-name opt-IDs opt-PL-elts)
  (let ((class `(,class-name . ,(new-qwe-lang-class opt-IDs opt-PL-elts))))
    (add-to-list 'qwe-lang-class-list class-name t)
    (add-to-list 'qwe-lang-class-alist class t)))

;;.sse.fn Language element base constructor

(defun qwe-lang-elt-base-ctor (qwe-lang-elt)
  (let* ((class (qwe-lang-elt-class (cdr qwe-lang-elt)))
         (class-list (intern (format "qwe-lang-class-%S-list" class)))
         (class-alist (intern (format "qwe-lang-class-%S-alist" class))))
    (add-to-list 'qwe-lang-elt-alist qwe-lang-elt t)
    (unless (boundp class-alist)
      (set class-alist nil))
    (add-to-list 'qwe-lang-elt-list name t)
    (add-to-list class-list name t)
    (add-to-list class-alist qwe-lang-elt t)))

;;.sse.fn Language element constructor
;; This function is used to create language elements, the constructor of the
;; {t/qwe-lang-elt'} structure. It also updates the alist {t/'qwe-lang-elt-alist'}
;; and the corresponding language class list and alist.

(defun qwe-lang-new-elt (class name abbrev label)
  (let* ((qwe-lang-elt `(,name . ,(new-qwe-lang-elt class abbrev label))))
    (qwe-lang-elt-base-ctor qwe-lang-elt)))

;;.sse.fn Context element constructor

(defun qwe-lang-new-elt-context (name abbrev label
                                      &optional
                                      keyword-based
                                      keyword-list
                                      text-list)
  (let ((qwe-lang-elt
         `(,name . ,(new-qwe-lang-elt-context 'context
                                              abbrev
                                              label
                                              keyword-based
                                              keyword-list
                                              text-list))))
    (qwe-lang-elt-base-ctor qwe-lang-elt)))

;;.sec Optional Designators
;;
;;.sse Identifiers (IDs)
;; Most {t/QWE} language elements can be accompained with an identifier (ID). IDs
;; are written between square brackets before the element name:
;;
;;.ver    .warning[warn-ID] Hey! Where's my ID?
;;
;; This is exactly the same: ;;.warning[warn-ID] Hey! Where's my ID?
;;
;; Unless you [[qwe:toggle-show-element-ids][toggle IDs visibility]], you won't see the ID.
;;
;; IDs are intended to be used by extensions, to facilitate the management
;; and identification of their own {t/QWE} elements. They must match following
;; regular expression.
;;
;;.sss.const qwe-lang-ID-regexp

(defconst qwe-lang-ID-regexp "[[:alpha:]_][[:alnum:]_-]*")

;;.sss.const qwe-lang-opt-ID-regexp

(defconst qwe-lang-opt-ID-regexp
  (concat "\\(?:\\[" qwe-lang-ID-regexp "\\]\\)?"))

;;.sse Interaction with Programming Languages (PL)
;;
;;.sss.const qwe-lang-PL-elts-list
;; This constant contains the list of all programming language elements. In
;; the future, these reserved keywords will be used to make specialized
;; indexes ({e/Index of Functions}, {e/Index of Classes}, {e/Index of Namespaces},
;; etc). Now you can use them just to indicate that a given section,
;; subsection, subsubsection or paragraph refers to a special programming
;; language element. To do it, simply append a dot and one element of the
;; list to the section tag. This is the way to indicate that a section name
;; must appear also at some special index.
;;
;; For example, this subsection is dedicated to the constant
;; {t/`qwe-lang-PL-elts-list'}. As I would like it to appear in an Index of
;; Constants, instead of simply using {t/`;;.sse'}I must use {t/`;;.sse.const'}.
;;
;; Note that in some cases there are different ways to refer to the same
;; construct.  This offers the possibility to use the complete name or simply
;; an abbreviation, e.g. both {t/`const'} and {t/`constant'} refer to constants.
;;
;; Please ask the maintainers for inclusion of new elements you need, but
;; don't ask for inclusion of concret types such as strings, lists, vectors
;; and the like.
;;
;;.todo  CUSTOM VARIABLE ?
;;       Maybe it should be a custom variable, depending on the way
;;       specialized indexes are implemented. One possibility would be a
;;       function to register the index, with parameters like the index name
;;       and the regexp to search for {e/PL}-constructs, e.g.
;;
;;.ver       (qwe-index-register "Index of Classes" "class")

(defconst qwe-lang-PL-elts-list
  (list "class" "const" "constant"
        "def" "define" "dir" "directory"
        "enum"
        "face" "file" "fn" "function"
        "iface" "import" "include" "interface"
        "label"
        "macro" "map" "member" "method" "mode"
        "namespace"
        "pkg" "package" "proc" "procedure"
        "record"
        "struct"
        "template" "type" "typedef"
        "union"
        "var" "variable"))

;;.fixme A more complete proposal ...
;; These words refer to {e/indexable thinks} one could use to arrange information
;; about the program and the system itself, not just programming language
;; elements. The better would be user or extension customizable.
;;
;;.cmt   (list "algorithm" "allocator" "API" "application" "argument" "array" "atom" "attribute"
;;.cmt         "base" "block" "bucket" "buffer" "bus"
;;.cmt         "callback" "cast" "channel" "class" "command" "compiler" "concept" "const" "constant" "conversion" "coordinate"
;;.cmt         "data" "database" "debug" "declaration" "def" "define" "device" "delegate" "dir" "directory" "dimension" "document" "domain" "dump"
;;.cmt         "element" "enum" "environment" "error" "event" "exception" "expression" "extension"
;;.cmt         "face" "feature" "frame" "framework" "file" "fn" "font" "format" "formula" "function"
;;.cmt         "global" "graph"
;;.cmt         "handler" "hash" "heuristic"
;;.cmt         "icon" "identifier" "idiom" "image" "implementation" "import" "include" "index" "input" "interface" "interpreter" "iterator"
;;.cmt         "key" "keymap" "keyword"
;;.cmt         "label" "language" "layer" "library" "link" "list" "loader" "local"
;;.cmt         "macro" "map" "member" "memory" "message" "method" "metric" "milestone" "mode" "module"
;;.cmt         "namespace" "net" "network" "number"
;;.cmt         "object" "operation" "operator" "output"
;;.cmt         "path" "pkg" "package" "parameter" "pattern" "pipe" "point" "pointer" "position" "proc" "procedure" "process" "program" "project" "property" "protocol"
;;.cmt         "query" "queue" "QWE"
;;.cmt         "record" "reference" "regexp" "register" "release" "report" "resource"
;;.cmt         "section" "signal" "script" "socket" "stack" "static" "stream" "struct" "subroutine" "subsystem" "syntax" "syscall" "system"
;;.cmt         "task" "template" "test" "thread" "tree" "type" "typedef" "typename"
;;.cmt         "union" "unit" "usecase" "user"
;;.cmt         "view"
;;.cmt         "widget"
;;.cmt         "var" "variable" "vector" "version"))

;;.sss.const qwe-lang-PL-elts-regexp
;; Regular expression used to search for programming language elements.

(defconst qwe-lang-PL-elts-regexp
  (regexp-opt qwe-lang-PL-elts-list))

;;.sss.const qwe-lang-opt-PL-elts-regexp

(defconst qwe-lang-opt-PL-elts-regexp
  (concat "\\(?:\\." qwe-lang-PL-elts-regexp "\\)?"))

;;.sse Context identifiers list
;; Extensions must use contexts to add new {t/QWE} language elements. Contexts
;; elements can include a dot-separated list of identifiers. This let
;; extensions organize their data hierarchically using just one context
;; element. To see an example of this concept, see configuration items placed
;; at the beginning of this file.

(defconst qwe-lang-context-ID-list-regexp
  (concat "\\(?:\\." qwe-lang-ID-regexp "\\)*"))


;;.sec Visual Elements
;; Visual Elements, or {i/velts} for short, are visual instances of language
;; elements used to show them on the screen. Each language element previously
;; defined has associated its visual element. The components of a visual
;; elements are:
;;
;;.Li1 A {e/face}, defined in file [[file:][qwe-faces.el]], used to show the element
;;
;;.li1 A {e/face alias}, used programatically to match and show language elements
;;
;;.li1 A {e/face specification} (also called {e/face spec}), used to show the
;;.li1 language delimiter, wich can be shown in different modes (text, label
;;.li1 or invisible) depending on te buffer.
;;
;; Visual elements for {t/QWE} elements are created later (see section [[]]). {t/QWE}
;; extensions must create their own visual elements explicitly using next
;; function.
;;
;;.sss.fn qwe-lang-new-velt

(defun qwe-lang-new-velt (elt-name)
  "Creates a new Visual Element based on `ELT-NAME'.

For a given QWE element named ELT-NAME, its corresponding visual
element is the face alias `qwe-face-ELT-ABBREV' and the varalias
`qwe-face-spec-delimiter-ELT-NAME'. The last one is
buffer-local."
  (unless (intern-soft (format "qwe-face-%S"
                               (qwe-lang-elt-get-attribute elt-name 'abbrev)))
    (intern (format "qwe-face-%S"
                    (qwe-lang-elt-get-attribute elt-name 'abbrev)))
    (unless (equal elt-name (qwe-lang-elt-get-attribute elt-name 'abbrev))
      (put (intern (format "qwe-face-%S"
                           (qwe-lang-elt-get-attribute elt-name 'abbrev)))
           'face-alias
           (intern (format "qwe-face-%S" elt-name)))))

  (set (intern (format "qwe-face-spec-delimiter-%S" elt-name)) nil)
  (make-variable-buffer-local (intern (format "qwe-face-spec-delimiter-%S" elt-name)))

  (unless (intern-soft (format "qwe-face-spec-delimiter-%S"
                               (qwe-lang-elt-get-attribute elt-name 'abbrev)))
    (defvaralias
      (intern (format "qwe-face-spec-delimiter-%S"
                      (qwe-lang-elt-get-attribute elt-name 'abbrev)))
      (intern (format "qwe-face-spec-delimiter-%S" elt-name)))))


;;.sec Functions to work with language elements
;; There are basically two, to read and write attribute values (see [[sss:][qwe-lang-elt]].
;;
;;.sse.fn qwe-lang-class-name-p

(defun qwe-lang-class-name-p (class-name)
  (not (null (memq class-name qwe-lang-class-list))))

;;.sse.fn qwe-lang-elt-name-p

(defun qwe-lang-elt-name-p (elt-name)
  (not (null (memq elt-name qwe-lang-elt-list))))

;;.sse.fn qwe-lang-elt-attribute-name-p

(defun qwe-lang-elt-attribute-name-p (prop-name)
  (not (null (memq prop-name qwe-lang-elt-attribute-list))))

;;.sse.fn qwe-lang-elt-get-attribute

(defun qwe-lang-elt-get-attribute (elt-name prop-name)
  (when (and (qwe-lang-elt-name-p elt-name)
             (qwe-lang-elt-attribute-name-p prop-name))
    (eval
     (read
      (format "(qwe-lang-elt-%s %S)"
              prop-name
              (cdr (assoc elt-name qwe-lang-elt-alist)))))))

;;.sse.fn qwe-lang-elt-set-attribute

(defun qwe-lang-elt-set-attribute (elt-name prop-name value)
  (when (and (qwe-lang-elt-name-p elt-name)
             (qwe-lang-elt-attribute-name-p prop-name))
    (eval
     (read
      (format
       "(setf (qwe-lang-elt-%s (cdr (assoc \'%s qwe-lang-elt-alist))) %s)"
       prop-name elt-name (if (stringp value)
                              (concat "\"" value "\"")
                            (concat "\'" (symbol-name value))))))))

;;.sse.fn qwe-lang-regexp

(defun qwe-lang-regexp ()
  (let ((str-list '("li1" "li2" "li3" "li4" "Li1" "Li2" "Li3" "Li4"
                    "ni1" "ni2" "ni3" "ni4" "Ni1" "Ni2" "Ni3" "Ni4"
                    "#include" "#appendix")))
    (dolist (elt qwe-lang-elt-alist)
      (add-to-list 'str-list (symbol-name (car elt)))
      (add-to-list 'str-list (symbol-name (qwe-lang-elt-abbrev (cdr elt)))))
    (regexp-opt str-list)))

;;.sse.fn qwe-lang-class-regexp

(defun qwe-lang-class-regexp (class-name &optional include-opts)
  (when (qwe-lang-class-name-p class-name)
    (let* ((class-list (intern-soft (format "qwe-lang-class-%S-list" class-name)))
           (str-list nil)
           (regexp nil))
        (dolist (elt-name (eval class-list))
          (add-to-list 'str-list (symbol-name elt-name))
          (add-to-list 'str-list (symbol-name (qwe-lang-elt-get-attribute elt-name 'abbrev))))
        (concat (regexp-opt str-list)
                (and include-opts
                     (qwe-lang--get-opts-for-regexps class-name))))))

;;.sse.fn qwe-lang-elt-regexp

(defun qwe-lang-elt-regexp (elt-name &optional include-opts)
  (when (qwe-lang-elt-name-p elt-name)
    (let* ((lang-elt (cdr (assoc elt-name qwe-lang-elt-alist)))
           (class-name (qwe-lang-elt-class lang-elt)))
      (concat (regexp-opt `(,(symbol-name elt-name)
                            ,(symbol-name (qwe-lang-elt-abbrev lang-elt))))
              (and include-opts
                   (qwe-lang--get-opts-for-regexps class-name))))))

;;.sse.fn qwe-lang-elt-re-search

(defun qwe-lang-elt-re-search (elt-name-or-abbrev
                               &optional id text-regexp doc backwards bound count)
  (let* ((elt-name (or (and (memq elt-name-or-abbrev qwe-lang-elt-list)
                            elt-name-or-abbrev)
                       (qwe-lang--get-elt-name-from-abbrev elt-name-or-abbrev)
                       (error "Invalid name or abbreviation '%S'" elt-name-or-abbrev)))
         (elt (cdr (assoc elt-name qwe-lang-elt-alist)))
         (class-name (qwe-lang-elt-class elt))
         (class (cdr (assoc class-name qwe-lang-class-alist)))
         (id-regexp (and (qwe-lang-class-opt-IDs class)
                         (concat "\\(?:\\["
                                 (or (and id
                                          (regexp-opt (list id)))
                                     ".+")
                                 "\\]\\)"
                                 (and (null id)
                                      "?"))))
         (doc (or doc
                  qwe-doc
                  (error "No QWE document specified")))
         (regexp (concat (qwe-doc-delimiter doc) "\\."
                         (qwe-lang-elt-regexp elt-name)
                         id-regexp
                         text-regexp)))
    (if backwards
        (re-search-backward regexp bound t count)
      (re-search-forward regexp bound t count))))

; (setq elt-name 'fixme)
; (setq elt (cdr (assoc elt-name qwe-lang-elt-alist)))
; (setq opt-regexps (qwe-lang--get-opts-for-regexps (qwe-lang-elt-class elt)))
; qwe-lang-elt-alist
; (setq P (read (format "(lambda (X) (string= \"%s\" (qwe-lang-elt-abbrev (cdr X))))" 'sge)))
; (car (find-if (read (format "(lambda (X) (string= \"%s\" (qwe-lang-elt-abbrev (cdr X))))" 'sse)) qwe-lang-elt-alist))

;;.cmt Private interface

(defun qwe-lang--get-elt-name-from-abbrev (abbrev)
  (car (find-if (read (format "(lambda (X) (string= \"%s\" (qwe-lang-elt-abbrev (cdr X))))" abbrev))
                qwe-lang-elt-alist)))

(defun qwe-lang--get-opts-for-regexps (class-name)
  (let* ((lang-class (cdr (assoc class-name qwe-lang-class-alist)))
         (opt-IDs (and (qwe-lang-class-opt-IDs lang-class)
                       qwe-lang-opt-ID-regexp))
         (opt-PL-elts (and (qwe-lang-class-opt-PL-elts lang-class)
                           qwe-lang-opt-PL-elts-regexp)))
    (concat opt-IDs opt-PL-elts)))


;;.sec Module Initialization
;; This code is executed only once, when the file is loaded.
;;
;;.sse Creation of language classes
;; This code is executed only once, the first time the file is loaded:

(dolist (class-def (list '(annotation    t nil)
                         '(context       t nil)
                         '(decoration    t nil)
                         '(environment nil nil)
                         '(section     nil   t)
                         '(title       nil nil)))
  (let* ((name (elt class-def 0))
         (opt-IDs (elt class-def 1))
         (opt-PL-elts (elt class-def 2)))
    (qwe-lang-new-class name opt-IDs opt-PL-elts)))

;;.sse Declaration of dynamic variables
;;
;;.sss.var qwe-lang-class-C-list

(dolist (class qwe-lang-class-list)
  (eval
   (read
    (format "(defvar qwe-lang-class-%S-list nil)" class))))

;;.sss.var qwe-lang-class-C-alist

(dolist (class qwe-lang-class-list)
  (eval
   (read
    (format "(defvar qwe-lang-class-%s-alist nil)" class))))

;;.sse Creation of language elements
;; This code is executed only once, the first time the file is loaded:
;;
;;.sss {e/Core} language elements
;;
;;.todo labels could be user-customizable?
(dolist (elt-def
         (list
          '(annotation  anchor           anc "<a>")
          '(annotation  bug              nil " B ")
          '(annotation  error            err " E ")
          '(annotation  fixme            fix " F ")
          '(annotation  note             not "(*)")
          '(annotation  todo             tod " t ")
          '(annotation  warning          war " W ")
          '(annotation  work-in-progress wip "-w-")
          ;;.cmt ---
          '(decoration  box              box "")
          '(decoration  bold-box         bbx "")
          '(decoration  button-pressed   bpr "")
          '(decoration  button-released  bre "")
          '(decoration  tip              tip "")
          ;;.cmt ---
          '(environment abstract         abs "abs ")
          '(environment comment          cmt "cmt>")
          '(environment delete           del "XXX ")
          '(environment quotation        quo "quo ")
          '(environment tabular          tab "[#] ")
          '(environment tty              tty "tty ")
          '(environment verbatim         ver "ver ")
          ;;.cmt ---
          '(title       part             prt "PRT>")
          '(title       title            ttl "TTL>")
          '(title       subtitle         stl "STL>")
          ;;.cmt ---
          '(section     chapter          chp "CHP ")
          '(section     section          sec "SEC ")
          '(section     subsection       sse "SSE ")
          '(section     subsubsection    sss "sss ")
          '(section     paragraph        par "par ")
          ))
  (let* ((class          (elt elt-def 0))
         (name           (elt elt-def 1))
         (abbrev         (elt elt-def 2))
         (label          (elt elt-def 3)))
    (qwe-lang-new-elt class name abbrev label)))

;;.sss Format text

(defvar qwe-lang-format-text-alist
  '((anchor               . a)
    (bold                 . b)
    (bold-italic          . i)
    (italic               . e)
    (fixed                . t)
    (underline            . u)
    (inverse-video        . I)
    (variable             . v)
    (keyword              . k)
    (string               . s)
    (function             . f)
    (highlight-yellow     . y)
    (highlight-green      . g)
    (highlight-orange     . o)
    (highlight-cyan       . c)
    (table-title-row      . T)
    (table-row            . R)
    (table-alternate-row  . S)
    (table-underlined-row . U)
    (table-underlined-alternate-row . V)
    (table-underlined-simple-row    . W)
    (table-box-row        . X)
    (table-bbx-row        . Y)
    (table-raised-row     . Z)
    (table-sunken-row     . z)))

(defvar qwe-lang-format-text-regexp
  (let ((ftext-list nil))
    (dolist (ftext qwe-lang-format-text-alist)
      (add-to-list 'ftext-list (symbol-name (cdr ftext)) t))
    (regexp-opt ftext-list)))

;;.sss Creation of visual elements

(progn
  (dolist (elt-name qwe-lang-elt-list)
    (qwe-lang-new-velt elt-name))

  ;;.par Special support for 'verbatim'

  (setq qwe-lang-velt--environment-verbatim nil)
  (make-variable-buffer-local 'qwe-lang-velt--environment-verbatim)

  ;;.par Processing of format text codes

  (dolist (ftext qwe-lang-format-text-alist)
    (put (intern (format "qwe-face-%S" (cdr ftext)))
         'face-alias
         (intern (format "qwe-face-%S" (car ftext))))))


;;.sec Colophon

(provide 'qwe-lang)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-lang.el ends here
