;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-toc.el}
;;.cfg.prj.file.desc      Table of Contents
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
;;.chp B. Management of ToC's
;;.cmt
;;.quo ;;.todo INSERT QUOTATION HERE
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       B. Management of ToC's
;;
;;.toc.sec             B.1. Introduction
;;
;;.toc.sec             B.2. Module dependencies
;;
;;.toc.sec             B.3. Customization
;;.toc.sse                   B.3.1. Customization Groups
;;.toc.sse                   B.3.2. Custom Variables
;;.toc.sss                         B.3.2.1. qwe-toc-indent-width
;;.toc.sss                         B.3.2.2. qwe-toc-enable-toc
;;.toc.sss                         B.3.2.3. qwe-toc-enable-lof
;;.toc.sss                         B.3.2.4. qwe-toc-enable-lot
;;.toc.sss                         B.3.2.5. qwe-toc-enable-idx
;;.toc.sss                         B.3.2.6. qwe-toc-menu-enabled
;;.toc.sss                         B.3.2.7. qwe-toc-menu-depth
;;.toc.sss                         B.3.2.8. qwe-toc-menu-size
;;.toc.sss                         B.3.2.9. qwe-toc-menu-custom-list
;;
;;.toc.sec             B.4. Data Structures
;;.toc.sse                   B.4.1. qwe-toc
;;.toc.sse                   B.4.2. Initialization
;;.toc.sss                         B.4.2.1. qwe-toc-init
;;.toc.sss                         B.4.2.2. qwe-toc-fini
;;.toc.sss                         B.4.2.3. qwe-toc-unload-function
;;.toc.sss                         B.4.2.4. Adding hooks
;;
;;.toc.sec             B.5. Processing Entries in the ToC
;;.toc.sse                   B.5.1. qwe-toc-process-list
;;
;;.toc.sec             B.6. Making the table of contents
;;.toc.sse                   B.6.1. qwe-toc-update
;;
;;.toc.sec             B.7. User Interface
;;.toc.sse                   B.7.1. Faces
;;.toc.sss                         B.7.1.1. qwe-face-toc-chp-text
;;.toc.sss                         B.7.1.2. qwe-face-toc-sec-text
;;.toc.sss                         B.7.1.3. qwe-face-toc-sse-text
;;.toc.sss                         B.7.1.4. qwe-face-toc-sss-text
;;.toc.sse                   B.7.2. QToC Elements
;;.toc.sse                   B.7.3. Links and References
;;.toc.sss                         B.7.3.1. qwe-toc-follow-link
;;
;;.toc.sec             B.8. Menus
;;.toc.sse                   B.8.1. Navigation
;;.toc.sss                         B.8.1.1. qwe-toc-menu-goto-toc
;;.toc.sss                         B.8.1.2. qwe-toc-menu-goto-section
;;.toc.sss                         B.8.1.3. qwe-toc-menu
;;.toc.sss                         B.8.1.4. qwe-toc-menu-items
;;.toc.sse                   B.8.2. Generation
;;.toc.sss                         B.8.2.1. qwe-toc-menu-rescan
;;
;;.toc.sec             B.9. Other Issues
;;
;;.toc.sec             B.10. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec B.1. Introduction
;; This {e/core extension} is the first one developed with two purposes. Firs, to
;; manage document's table of contents, list of figures and list of
;; tables. Second, to show how to expand {t/QWE} adding new functionality using
;; {t/QWE} {e/extensions}. As this extension is distributed along with {t/QWE}, we call
;; it a {e/core} extension.
;;
;; The main idea behind the problem of ToC management is that it must be a
;; completely automatized process. To do that, the user has to choose only
;; where to place it and click the right menu option.
;;
;; That seems simple, so the solution looks like this:
;;
;;.li1 The user choose the place where the ToC has to appear.
;;
;;.li1 The user choose the right menu option.
;;
;;.li1 An internal process gather all necessary information.
;;
;;.li1 An internal process formats and prints the ToC.
;;
;; The way the user choose the place for the ToC is also quite simple. She
;; has only to write the tags:
;;
;;.ver  ;;.qwe.toc.begin
;;.ver  ;;.qwe.toc.end
;;
;; and the rest of the process continues as described above. {i/ALL} lines
;; between these two contextual tags {e/pertain} to the ToC management, and will
;; be erased just before writing the ToC. The user must be warned about this
;; fact.
;;
;; To not to be so ugly, the {t/begin} line should be used o write a title whilst
;; the {t/end} line to signal the end of the ToC. See the ToC of the present
;; document.
;;
;;.sec B.2. Module dependencies

(eval-when-compile
  (require 'qwe-lang)
  (require 'qwe-cfg)
  (require 'qwe-exi))

;;.sec B.3. Customization
;;
;;.sse B.3.1. Customization Groups

(defgroup qwe-toc nil
  "Configuration of the qwe-toc extension."
  :group 'qwe)

(defgroup qwe-toc-menu nil
  "Configuration of the qwe-toc-menu entry."
 :group 'qwe-toc)

;;.sse B.3.2. Custom Variables
;;
;;.sss.var B.3.2.1. qwe-toc-indent-width

(defcustom qwe-toc-indent-width 3
  "Number of spaces added before ToC entries.

This is the number of spaces used to indent hierarchical ToC
entries. The number must be between 0 and 10, both included."
  :type 'integer
  :safe (lambda (n) (and (<= 0 n) (<= n 10)))
  :group 'qwe-toc)

;;.sss.var B.3.2.2. qwe-toc-enable-toc

(defcustom qwe-toc-enable-toc t
  "Enable Table of Contents"
  :type 'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe-toc)

;;.sss.var B.3.2.3. qwe-toc-enable-lof

(defcustom qwe-toc-enable-lof t
  "Enable List of Figures."
  :type 'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe-toc)

;;.sss.var B.3.2.4. qwe-toc-enable-lot

(defcustom qwe-toc-enable-lot t
  "Enable List of Tables."
  :type 'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe-toc)

;;.sss.var B.3.2.5. qwe-toc-enable-idx

(defcustom qwe-toc-enable-idx t
  "Enable Indexes."
  :type 'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe-toc)

;;.sss.var B.3.2.6. qwe-toc-menu-enabled

(defcustom qwe-toc-menu-enabled t
  "Indicates whether the qwe-toc-menu appears on the menu bar."
  :type 'boolean
  :safe  (lambda (b) (booleanp b))
  :group 'qwe-toc-menu)

;;.sss.var B.3.2.7. qwe-toc-menu-depth

(defcustom qwe-toc-menu-depth 0
  "Number of hierarchical levels to include in the qwe-toc-menu.

Sections appearing in the qwe-tocmenu are determined by the style
of the document (see variable `qwe-default-doc-style') You can
change the hierarchy depth manually with this variable. One means
only sections or chapters, depending on the document's style,
etc. Zero means all elements.

At the moment, this variable also has a side effect on the Table
of Contents. In the future this effect will be removed." ;;.todo Remove this side effect
  :type  'integer
  :safe  (lambda (n)
           (and (integerp n) (>= n 0)))
  :group 'qwe-toc-menu)

;;.sss.var B.3.2.8. qwe-toc-menu-size

(defcustom qwe-toc-menu-size 25
  "Number of entries per sub-menu.

If there are too many qwe-toc-menu entries, the menu can be
divided into sub-menus. This variable set the size of each
sub-menu."
  :type 'integer
  :safe  (lambda (n)
           (and (integerp n) (> n 0)))
  :group 'qwe-toc-menu)

;;.sss.var B.3.2.9. qwe-toc-menu-custom-list

(defcustom qwe-toc-menu-custom-list nil
  "List of section types to include in the qwe-toc-menu."
  :type '(set
          (const chapter)
          (const section)
          (const subsection)
          (const subsubsection)
          (const paragraph))
  :safe 'qwe-section--list-p
  :group 'qwe-toc-menu)

;;.sec B.4. Data Structures
;;
;;.sse.struct B.4.1. qwe-toc

(defstruct qwe-toc
  ;; {T/ Attrib       | Default Value | Local Variable       | Comments          }
  ;; {S/ Table of Contents and Lists of Objects and Indexes                      }
  (enable-toc        t)           ;; {R/ qwe-toc-enable-toc   } Table of Contents
  (enable-lof        nil)         ;; {R/ qwe-toc-enable-lof   } List of Figures
  (enable-lot        nil)         ;; {R/ qwe-toc-enable-lot   } List of Tables
  (enable-idx        t)           ;; {R/ qwe-toc-enable-idx   } Various Indexes
  ;; {S/ QToC menu                                                               }
  (menu-enabled      nil)         ;; {R/ qwe-toc-menu-enabled }
  (menu-items-list nil)        ;;                                   {e/         Items in QToC menu}
  )

;; If {t/qwe-toc} is supported every buffer must containt a {t/qwe-toc} buffer-local
;; variable.

(make-variable-buffer-local 'qwe-toc)

;;.sse B.4.2. Initialization
;;
;;.sss.fn B.4.2.1. qwe-toc-init

(defun qwe-toc-init ()

  ;; Structure creation and initialization
  ;;
  (setq qwe-toc (make-qwe-toc))
  (setf (qwe-toc-enable-toc qwe-toc) qwe-toc-enable-toc)
  ; (setf (qwe-toc-enable-lof qwe-toc) qwe-toc-enable-lof)
  ; (setf (qwe-toc-enable-lot qwe-toc) qwe-toc-enable-lot)
  ; (setf (qwe-toc-enable-idx qwe-toc) qwe-toc-enable-idx)

  (let ((cfg-item (qwe-cfg-get-value "toc.menu.enabled")))
    (setf (qwe-toc-menu-enabled qwe-toc)
          (if cfg-item
              (string= "yes" cfg-item)
          qwe-toc-menu-enabled)))

  ;; Enable certain menu items in {t/Preferences} sub-menu. First, a complex menu
  ;; item:
  ;;
  (qwe-exi-add-to-menu-preferences-item
   [ "Show QToC menu" (and (setf (qwe-toc-menu-enabled qwe-toc)
                                 (not (qwe-toc-menu-enabled qwe-toc)))
                           (or (qwe-toc-menu-items-list qwe-toc)
                               (qwe-toc-menu-rescan)))
     :active   t
     :style    toggle
     :selected (qwe-toc-menu-enabled qwe-toc) ])

  ;; Second, a simple callback:
  ;;
  (qwe-exi-add-to-menu-preferences "Customize qwe-toc"
                                   '(customize-group 'qwe-toc))

  (when (qwe-toc-menu-enabled qwe-toc)
    ;;.todo Create the menu entry
    (qwe-toc-menu-rescan t)))

;;.sss.fn B.4.2.2. qwe-toc-fini

(defun qwe-toc-fini ()
  (qwe-toc-unload-function))

;;.sss.fn B.4.2.3. qwe-toc-unload-function

(defun qwe-toc-unload-function ()
  (easy-menu-remove qwe-toc-menu)
  (qwe-exi-delete-from-menu-preferences "Customize qwe-toc")
  (qwe-exi-delete-from-menu-preferences "Show QToC menu")

  ;; Return {t/nil} so that unloading proceeds
  nil)

;;.sss B.4.2.4. Adding hooks

(add-hook 'qwe-mode-enable-hook 'qwe-toc-init)
(add-hook 'qwe-mode-disable-hook 'qwe-toc-fini)

;;.sec B.5. Processing Entries in the ToC
;;
;;.sse.fn B.5.1. qwe-toc-process-list

(defun qwe-toc-make-process-list (toc-list)
  (let ((section-str nil))
    (dotimes (i (length toc-list))
      (when (> i 0)
        (if (string= (elt (elt toc-list i) 0) "more...")
            (qwe-toc-make-process-list (elt toc-list i))
          (progn
            (setq section-str (elt (elt (elt (elt toc-list i) 1) 1) 1))
            (when (> (length section-str) 3)
              (setq section-str (qwe-section--get-str-from-string section-str)))

            ;; Separation line
            (when (or (string= section-str "chp")
                      (string= section-str "sec"))
              (insert (format "%s\n" (qwe-doc-delimiter qwe-doc))))

            (insert (format "%s.toc.%s" (qwe-doc-delimiter qwe-doc) section-str))
            (insert "       " (elt (elt toc-list i) 0) "\n")))))))

;;.sec B.6. Making the table of contents
;;
;;.sse.fn B.6.1. qwe-toc-update

(defun qwe-toc-update (&optional rescan-done)
  (interactive "")
  (if (not rescan-done)
      (qwe-toc-menu-rescan))
  (let ((toc-begin-point nil)
        (toc-end-point nil)
        (delimiter ;;.tip Suppor for  (qwe-doc-delimiter qwe-doc)=""
         (if (> (string-width (qwe-doc-delimiter qwe-doc)) 0)
             (qwe-doc-delimiter qwe-doc) "^"))
        (buffer-modified (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (concat delimiter "\\.toc\\.begin.*$") nil t)
        (forward-line 1)
        (setq toc-begin-point (point)))
      (when (re-search-forward (concat delimiter "\\.toc\\.end.*$") nil t)
        (goto-char (point-at-bol))
        (setq toc-end-point (point)))
      (when (and toc-begin-point toc-end-point)
        (delete-region toc-begin-point toc-end-point)
        ;;.del (qwe-toc-make-process-list qwe-toc-menu-items-list))
        (qwe-toc-make-process-list (qwe-toc-menu-items-list qwe-toc)))
      (message ""))
    (if (not buffer-modified)
        (progn
          (save-buffer)
          (message "")))))

;;.sec B.7. User Interface
;; Here is all stuff needed by the {t/qwe-toc-menu} interface.
;;
;;.sse B.7.1. Faces
;;
;;.sss.face B.7.1.1. qwe-face-toc-chp-text

(defface qwe-face-toc-chp-text
  '((t (:height  1.4
        :inherit qwe-face-toc-sec-text
        :weight  bold)))
  "Face to display chapter entries into the TOC."
 :group 'qwe-derived-faces)

;;.sss.face B.7.1.2. qwe-face-toc-sec-text

(defface qwe-face-toc-sec-text
  '((t (:height  1.2
        :inherit qwe-face-toc-sse-text
        :weight  bold)))
  "Face to display section entries into the TOC."
 :group 'qwe-derived-faces)

;;.sss.face B.7.1.3. qwe-face-toc-sse-text

(defface qwe-face-toc-sse-text
  '((t (:height 1.1
        :inherit qwe-face-toc-sss-text)))
  "Face to display subsection entries into the TOC."
 :group 'qwe-derived-faces)

;;.sss.face B.7.1.4. qwe-face-toc-sss-text

(defface qwe-face-toc-sss-text
  '((t (:inherit   qwe-face-default-text
        :slant     normal
        :underline nil
        :weight    normal)))
  "Face to display subsubsection entries into the TOC."
 :group 'qwe-derived-faces)

;;.sse B.7.2. QToC Elements
;; Elements used to construct tables of contents.

(setq qwe-face-context-toc-begin (qwe-face-spec-exi-link
                                  'qwe-face-chapter
                                  'qwe-toc-follow-link
                                  "Update ToC" 'toc 'update)
      qwe-face-context-toc-chp (qwe-face-spec-exi-link
                                'qwe-face-toc-chp-text
                                'qwe-toc-follow-link
                                "Goto chapter")
      qwe-face-context-toc-sec (qwe-face-spec-exi-link
                                'qwe-face-toc-sec-text
                                'qwe-toc-follow-link
                                "Goto section")
      qwe-face-context-toc-sse (qwe-face-spec-exi-link
                                'qwe-face-toc-sse-text
                                'qwe-toc-follow-link
                                "Goto subsection")
      qwe-face-context-toc-sss (qwe-face-spec-exi-link
                                'qwe-face-toc-sss-text
                                'qwe-toc-follow-link
                                "Goto subsubsection")
      qwe-face-context-toc-par (qwe-face-spec-exi-link
                                'qwe-face-default-text
                                'qwe-toc-follow-link
                                "Goto paragraph")
      qwe-face-context-toc-end (qwe-face-spec-exi-link
                                'qwe-face-default-text
                                'qwe-toc-follow-link
                                "Update ToC" 'toc 'update))

(qwe-exi-new-context-kb 'toc 'toc "toc "
                        '("begin" "chp" "sec" "sse" "sss" "par" "end"))


;;.sse B.7.3. Links and References
;;
;;.sss.fn B.7.3.1. qwe-toc-follow-link
;; This is the callback used when the user presses the mouse over a ToC
;; entry.

(defun qwe-toc-follow-link ()
  (interactive "")
  (if (equal 'update (get-char-property (point) 'toc))
      (qwe-toc-update) ;;.tip Link from a TOC begin or end
    (let* ((begin (next-property-change (point)))
           (end (previous-property-change (point)))
           (section-type "")
           (section-name (buffer-substring-no-properties begin end)))
      (save-excursion
        (goto-char (point-at-bol))
        (re-search-forward "\\.toc\\.\\(...\\)" nil t)
        (setq section-type (match-string-no-properties 1)))
      (qwe-link-follow--section
       (qwe-link--marker-at-point begin (current-buffer))
       section-type section-name))))

;;.sec B.8. Menus
;;
;;.sse B.8.1. Navigation
;;
;;.sss.fn B.8.1.1. qwe-toc-menu-goto-toc

(defun qwe-toc-menu-goto-toc ()
  (interactive "")
  (let ((pos nil)
        (delimiter ;;.tip Suppor for  (qwe-doc-delimiter qwe-doc)=""
         (if (> (string-width (qwe-doc-delimiter qwe-doc)) 0) (qwe-doc-delimiter qwe-doc) "^")))
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward
            (concat delimiter "\\.toc\\.begin.*$") nil t)
           (setq pos (goto-char (point-at-bol)))
           (re-search-forward
            (concat delimiter "\\.toc\\.end.*$") nil t)))
    (if pos
        (progn
          (goto-char pos)
          (recenter 3))
      (message "There is no Table of Contents."))))

;;.sss.fn B.8.1.2. qwe-toc-menu-goto-section

(defun qwe-toc-menu-goto-section (section-str section-name &optional file)
  (let ((pos nil)
        (regexp nil))
    (save-excursion
      (and (stringp file)
           (or (and (file-exists-p file)
                    (find-file file))
               (error "No such file '%s'" file)))
      (goto-char (point-min))
      (if  (> (string-width (qwe-doc-delimiter qwe-doc)) 0)  ;;.tip Support for  (qwe-doc-delimiter qwe-doc)=""
          (setq regexp (concat (qwe-doc-delimiter qwe-doc) "\\."
                               section-str ".*" (regexp-quote section-name) "$"))
        (setq regexp (concat "^\\." section-str ".*" (regexp-quote section-name) "$")))
      (when (re-search-forward regexp)
        (setq pos (point))))
    (when pos
      (and (stringp file)
           (or (and (file-exists-p file)
                    (find-file file))
               (error "No such file '%s'" file)))
      (goto-char pos)
      (goto-char (point-at-bol)))))

;;.sss.var B.8.1.3. qwe-toc-menu

(defvar qwe-toc-menu nil
  "QToC menu.")

(qwe-exi-add-menu-bar 'qwe-toc-menu
                      '("QToC"
                        :filter qwe-toc-menu-items
                        :visible (qwe-toc-menu-enabled qwe-toc))
                      "QToC menu.
This menu item shows the document sections in a hierarchy
manner.")

;;.todo Insert qwe-toc-menu into qwe-doc ??
;;
;;   Think about it to implement master document containig a complete
;;   {t/qtoc-menu} with all entries in all subdocuments. See also [[todo:includes][this todo note]].
;;
;;.sss B.8.1.4. qwe-toc-menu-items

(defun qwe-toc-menu-items (arg)
  (append (list [ "Rescan" qwe-toc-menu-rescan ]
                "---"
                [ "Table of Contents" qwe-toc-menu-goto-toc ])
          (qwe-toc-menu-items-list qwe-toc)))

;;.sse B.8.2. Generation
;; The current approach, a very basic idea, is to generate a plain list with
;; all document chapters and sections as menu items. The number of entries is
;; limited by the variable {t/qwe-toc-menu-size}. For documents with a large
;; number of entries, append a special entry to open a submenu containing a
;; sublist with more entries. Each sublist is in turn limited to contain at
;; most {t/qwe-toc-menu-size} elements.
;;
;;.todo Implement this:
;;    Another interesting approach is to generate a list of sublists, each
;;    one containing a complete sub-hierarchy of the document structure. For
;;    instance, main entries in the {t/QToC} menu can be chapters; for each one,
;;    there must be a sub-menu with all sections of that chapter; for each
;;    section, a sub-menu with all subsections ... and so on. This can give
;;    an interesting view of the document from the menu.
;;
;;.todo Define a custom variable to choose between both implementations

;;.sss.fn B.8.2.1. qwe-toc-menu-rescan

(defun qwe-toc-menu-rescan (&optional opening)

  ;;.todo[includes] Support '#include' directives to append contents of slave docs

  (interactive "")
  (setf (qwe-toc-menu-items-list qwe-toc) (list "---"))
  (let* ((items-list (qwe-toc-menu-items-list qwe-toc))
         (car-items-list items-list)
         (title nil)
         (num-items 0)
         (num-level 0)
         (regexp nil)
         (tmp-list nil)
         (section-str nil)
         (section-name nil)
         (num-level-diff 0)
         (section-str-list nil)
         (section-type-list qwe-toc-menu-custom-list))

    ;;.par Compute what section types must appear in the QToC menu
    ;; This depends on the document style.
    ;;
    (unless section-type-list
      ;; {e/List with all members}
      (setq section-type-list
            (cdr (assoc (qwe-doc-style qwe-doc) qwe-section-style-alist)))

      ;; {e/Limit the number of members}
      (when (> qwe-toc-menu-depth 0)
        (setq tmp-list nil)
        (dotimes (i qwe-toc-menu-depth)
          (when (elt section-type-list i)
            (add-to-list 'tmp-list (elt section-type-list i) t)))
        (setq section-type-list tmp-list)))

    (dolist (s section-type-list)
      (add-to-list 'section-str-list (qwe-section--get-str s) t)
      (add-to-list 'section-str-list (qwe-section--get-string s) t)
      ;;.del (when (equal s 'chapter)
      ;;.del   (add-to-list 'section-str-list (qwe-section--get-str 'appendix) t)
      ;;.del   (setq num-level-diff 1))
      )

    ;;.par Make a regexp suitable to find section types
    ;;
    (setq regexp
          (if  (> (string-width (qwe-doc-delimiter qwe-doc)) 0)
              (concat "^[ \t]*"
                      (qwe-doc-delimiter qwe-doc) "\\.\\("
                      (regexp-opt section-str-list) "\\)"
                      qwe-section-label-regexp "\\(?:\\.\\("
                      qwe-lang-PL-elts-regexp "\\)\\)? \\(.*$\\)")
            (concat "^\\.\\("
                    (regexp-opt section-str-list) "\\)"
                    qwe-section-label-regexp "\\(?:\\.\\("
                    qwe-lang-PL-elts-regexp "\\)\\)? \\(.*$\\)")))

    ;;.par Iterate over all document chapters and sections
    ;;
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (setq section-str (match-string-no-properties 1))
        ;;.tip support long section names
        (setq section-str (or (qwe-section--get-str-from-string section-str)
                              section-str))
        (setq section-name (match-string-no-properties 3))
        (setq num-level
              (max 0 (- (position section-str section-str-list :test 'string=)
                        num-level-diff)))
        (setq num-items (1+ num-items))
        (when (> num-items qwe-toc-menu-size)
          (setq num-items 1)
          (setq tmp-list (list (list "more...")))
          (setq items-list (nconc items-list tmp-list))
          (setq items-list (car tmp-list)))
        (setq title
              (format "%s%s" (make-string (* qwe-toc-indent-width num-level) 32) section-name))
        (setq items-list
              (nconc items-list
                     `([ ,title (easy-menu-make-symbol (qwe-toc-menu-goto-section
                                                        ,section-str
                                                        ,section-name))])))))
    (setf (qwe-toc-menu-items-list qwe-toc) car-items-list))

  (message "")

  (if (not opening)
      (qwe-toc-update t)))

;;.sec B.10. Colophon

(provide 'qwe-toc)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-toc.el ends here
