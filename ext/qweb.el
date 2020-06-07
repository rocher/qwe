;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qweb.el}
;;.cfg.prj.file.desc      {t/WEB}-{e/like} literate programming interface
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
;;.cfg.doc.style  article
;;.cfg.header.filename ../header.qwe
;;.cfg.header.end [[qwe:hdr-update][update]]
;;.cmt _____________________________________________________________________________
;;.cmt
;;.ttl QWEB
;;.cmt
;;.quo                                  The practitioner of literate
;;.quo                        programming can be regarded as an essayist,
;;.quo                        whose main concern is with exposition and
;;.quo                        excellence of style.
;;.cmt
;;.quo                                  {i/Donald Knuth}
;;.quo                                  Literate Programming (1984)
;;.quo                                  in Literate Programming. CSLI, 1992, pg. 99
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.sec       1. Introduction
;;.toc.sse             1.1. Module dependencies
;;
;;.toc.sec       2. Data Structures and Variables
;;.toc.sse             2.1. qweb-block
;;.toc.sse             2.2. qweb-block-alist
;;.toc.sse             2.3. qweb-enabled
;;.toc.sse             2.4. qweb-show-block-file-name
;;
;;.toc.sec       3. Language Elements
;;.toc.sse             3.1. Faces
;;.toc.sse             3.2. Contexts
;;
;;.toc.sec       4. User Interface
;;.toc.sse             4.1. Enabling and Disabling QWEB
;;.toc.sss                   4.1.1. qweb-init
;;.toc.sss                   4.1.2. qweb-fini
;;.toc.sss                   4.1.3. qweb-unload-function
;;.toc.sss                   4.1.4. Adding hooks
;;.toc.sse             4.2. Menu items
;;
;;.toc.sec       5. Links
;;.toc.sse             5.1. Variables
;;.toc.sss                   5.1.1. qweb--prev-search
;;.toc.sss                   5.1.2. qweb--last-search-failed
;;.toc.sss                   5.1.3. qweb--vars-alist
;;.toc.sss                   5.1.4. qweb--mark-boqb
;;.toc.sse             5.2. Function Callbacks
;;.toc.sss                   5.2.1. qweb-follow-extern-link
;;.toc.sss                   5.2.2. qweb-follow-link
;;.toc.sss                   5.2.3. qweb-highlight-link
;;.toc.sss                   5.2.4. qweb--search-block
;;.toc.sss                   5.2.5. qweb--re-search-block
;;
;;.toc.sec       6. Obtaining Source Code from QWEB
;;.toc.sse             6.1. qweb-make-source-code
;;
;;.toc.sec       7. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec 1. Introduction
;;
;; See the [[file:../doc/qweb-manual.txt][QWEB Manual]] for more information.

;;.sse 1.1. Module dependencies

(eval-when-compile
  (require 'cl)
  (require 'qwe-cfg)
  (require 'qwe-exi)
  (require 'qwe-lang)
  (require 'qwe-link)
  (require 'qwe-ui))


;;.sec 2. Data Structures and Variables
;;
;;.sse.struct 2.1. qweb-block
;; This data structure contains basic information of a given qweb-block.

(defstruct (qweb-block
            (:constructor nil)
            (:constructor new-qweb-block ()))
  (name "")
  (point-min nil)
  (point-max  nil))

;;.sse.var 2.2. qweb-block-alist
;; This buffer-local variable contains an association list of the form:
;;
;;.ver (("qweb-block-name_1" . (begin_1 end_1))
;;.ver  ("qweb-block-name_2" . (begin_2 end_2))
;;.ver  ("qweb-block-name_3" . (("filename_1" . (begin_3 end_3))
;;.ver                         ("filename_2" . (begin_4 end_4))))
;;.ver  ...)
;;
;; For every qweb-block, there is its name and a list of regions of the
;; buffer where the block is defined, either using
;;
;;.ver  qwe.qweb <qweb-block-name_2>=
;;
;; or
;;
;;.ver  qwe.qweb <qweb-block-name_2>+=
;;
;; in the same order these block definitions appear in the buffer. To get a
;; list of regions in which a qweb-block is defined, just use
;;
;;.ver  (cdr (assoc qweb-block-name_2 qweb-block-alist))

(defvar qweb-block-alist (list)
  "Association list describing all qweb-blocks defined in a buffer.")
(make-variable-buffer-local 'qweb-block-alist)

;;.sse.var 2.3. qweb-enabled
;; This buffer-local variable indicates whether {t/qweb} extension is being used
;; in a buffer or not. It is managed by {t/qweb-enable} and {t/qweb-disable}
;; functions, so don't assign it a value directly.

(defvar qweb-enabled nil
  "t if qweb extension is being used in a buffer.")
(make-variable-buffer-local 'qweb-enabled)

;;.sse.var 2.4. qweb-show-block-file-name
;; This buffer-local variable indicates whether filename must be shown or
;; hidden in external qweb-block references.

(defvar qweb-show-block-file-name t)
(make-variable-buffer-local 'qweb-show-block-file-name)


;;.sec 3. Language Elements
;;
;;.sse 3.1. Faces

(defface qwe-face-qweb-block-define
  '((t (:foreground "blue4"
        :inherit    qwe-face-default-keyword
        :underline  "blue4"
        :weight     bold)))
  "Face to display the qweb-block name in a qweb-block definition."
  :group 'qwe-derived-faces)

(defface qwe-face-qweb-block-reference
  '((t (:foreground "blue4"
        :inherit    qwe-face-default-keyword
        :slant      italic)))
  "Face to display theq qweb-block name in a qweb-block reference."
  :group 'qwe-derived-faces)

(defface qwe-face-qweb-block-name
  '((t (:foreground "grey50"
        :inherit    qwe-face-default-keyword
        :slant      italic)))
  "Face to display the end of a qweb-block."
  :group 'qwe-derived-faces)

(defface qwe-face-qweb-block-filename
  '((t (:inherit    qwe-face-link-text
        :underline  "blue")))
  "Face to display the filename in a qweb-block reference."
  :group 'qwe-derived-faces)

;;.sse 3.2. Contexts

(setq qwe-face-spec-qweb-define (qwe-face-spec-exi-link
                                 'qwe-face-qweb-block-define
                                 'qweb-follow-link
                                 "<prev--[ block ]--next>")
      qwe-face-spec-qweb-reference (qwe-face-spec-exi-link
                                    'qwe-face-qweb-block-reference
                                    'qweb-follow-link
                                    "<prev--[ block ]--next>")
      qwe-face-spec-qweb-begindef (qwe-face-spec-exi-link
                                   'qwe-face-qweb-block-name
                                   'qweb-highlight-link
                                   "highlight block")
      qwe-face-spec-qweb-endef (qwe-face-spec-exi-link
                                'qwe-face-qweb-block-name
                                'qweb-highlight-link
                                "highlight block"))

;;.fixme[wb-names] Support filenames as optional identifiers
;; This feature is not available neither mature, yet.
;; Might need to create optional identifiers in qwe-font-lock.el
;;.cmt (setq qwe-face-spec-qweb-id
;;.cmt       '(face qwe-face-link-followed
;;.cmt              invisible qwe--qweb-block-file
;;.cmt              front-sticky nil
;;.cmt              rear-nonsticky t))

(qwe-exi-new-context 'qweb 'qweb "qweb"
                     ;; keyword-regexp-face-alist
                     nil
                     ;;.fixme[wb-names] Support filenames as optional identifiers
                     ;;'("\\[.+\\]" . qwe-face-spec-qweb-id)
                     ;; text-regexp-face-alist
                     '(("<.+>\\+?=" . qwe-face-spec-qweb-define)
                       ("<.+>" . qwe-face-spec-qweb-reference)
                       ("\\+ .* \\+" . qwe-face-spec-qweb-begindef)
                       ("- .* -" . qwe-face-spec-qweb-endef)))


;;.sec 4. User Interface
;;
;;.sse 4.1. Enabling and Disabling QWEB
;;
;;.sss.fn 4.1.1. qweb-init
;; Since {t/qweb} extension has been loaded, it will be automatically enabled
;; on every buffer requesting its presence.

(defun qweb-init ()
  "Initializes qweb if `qweb.cfg.enable' qwe-cfg item is `yes'."
  (interactive)
  (setq qweb-enabled
            (string= "yes" (or (qwe-cfg-get-value "qweb.enable") ""))))

;;.sss.fn 4.1.2. qweb-fini

(defun qweb-fini ()
  (qweb-unload-function))

;;.sss.fn 4.1.3. qweb-unload-function

(defun qweb-unload-function ()
  (qwe-exi-delete-from-menu-preferences "Show QToC menu")
  ;; Return {t/nil} so that unloading proceeds
  nil)

;;.sss 4.1.4. Adding hooks

(add-hook 'qwe-mode-enable-hook 'qweb-init)
(add-hook 'qwe-mode-disable-hook 'qweb-fini)

;;.sse 4.2. Menu items
;; This menu entry let users toggle filename visibility on external qweb-block
;; references.

;;.fixme[wb-names] Support filenames as optional identifiers
;; This menu entry works fine as is, without further modification.
;;.cmt (qwe-exi-add-to-menu-preferences-item
;;.cmt        [ "Show qweb block filenames" (progn (setq qweb-show-block-file-name
;;.cmt                                                  (not qweb-show-block-file-name))
;;.cmt                                            (if qweb-show-block-file-name
;;.cmt                                                (remove-from-invisibility-spec 'qwe--qweb-block-file)
;;.cmt                                              (add-to-invisibility-spec 'qwe--qweb-block-file)))
;;.cmt          :active   t
;;.cmt          :style    toggle
;;.cmt          :selected qweb-show-block-file-name
;;.cmt          :visible  qweb-enabled ])

;;.todo Include qweb-blocks index
;;
;; Integrate qweb extension with ToC such that qweb blocks can appear into the
;; menu ... or make a different menu bar for qweb buffers.


;;.sec 5. Links
;; qweb-block names are also links. A click near the beginning of the name
;; will search backwards the previous instance of the qweb block currently
;; clicked. A click near the end of the name will search forwards the next
;; instance. If any of these searches fails, the next search will be wrapped.
;;
;; In exernal qweb block references is possible to click also the
;; filename. Then the link will follow the qweb block name in the given
;; (external) filename.
;;
;;.sse 5.1. Variables
;;
;;.sss.var 5.1.1. qweb--prev-search

(defvar qweb--prev-search nil
  "Contains the qweb block name of the previous search.")

;;.sss.var 5.1.2. qweb--last-search-failed

(defvar qweb--last-search-failed nil
  "If t, the last search failed.")

;;.sss.var 5.1.3. qweb--vars-alist

(defvar qweb--vars-alist nil
  "List of variables defined in a QWEB document.")

(defvar qweb--blocks-visited-list nil)

;;.sss.var 5.1.4. qweb--mark-boqb

(defvar qweb--mark-boqb t
  "In substitutions, write a mark at the beginning of QWEB blocks.

Default value is `t'. Its value can be changed with
`qweb.mark-boqb' config item.")

;;.sse 5.2. Function Callbacks
;;
;;.sss.fn 5.2.1. qweb-follow-extern-link
;; Called when users click the filename of an external qweb block
;; reference. Changes to the buffer or file and looks for the first qweb block
;; instance.

(defun qweb-follow-extern-link ()
  "Follow an external qweb block reference."
  (interactive)
  (let* ((file-name nil)
         (qweb-block-name nil))

    ;; Get file-name and block name
    (save-excursion
      (goto-char (point-at-bol))
      ;;.fixme[wb-names] Opt. filenames IDs supported in '(?1:...)'
      (re-search-forward "qweb\\[\\(?1:.*\\)\\][ \t]+\\(?2:<.*>\\)" nil t)
      (setq file-name (match-string-no-properties 1))
      (setq qweb-block-name (match-string-no-properties 2)))

    ;; Switch to other file and search for qweb-block
    (qwe-link-follow--file file-name)
    (qweb--search-block qweb-block-name nil t)))

;;.sss.fn 5.2.2. qweb-follow-link
;; This function is used as a callback when users click the qweb block name.

(defun qweb-follow-link ()
  "Follow a qweb block definition or reference.

Follow the link of a qweb-block. A click near the beginning of the
link will cause the search to be backwards, whilst a click near
the end will search forwards."
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    ;;.fixme[wb-names] Opt. filenames IDs supported in '(?:...)'
    (re-search-forward "qweb\\(?:\\[.+\\]\\)?[ \t]+\\(?1:<[ \t]*\\(?2:.+?\\)[ \t]*>\\)\\+?=?" nil t))
  (let* ((direction
          (< (- (point) (match-beginning 1))
             (- (match-end 1) (point))))
         (qweb-block-name (match-string-no-properties 2)))

    ;; Depending on the position the users clicks the link, the search will
    ;; be backwards or forwards. If the block name matches {t/'-+'}, then search
    ;; backwards the beginning of the current qweb-block or go to the next
    ;; qweb-block.

    (if (string-match-p "-+" qweb-block-name)
        (qweb--re-search-block ".+?" direction t)
      (qweb--search-block qweb-block-name direction))))

;;.sss.fn 5.2.3. qweb-highlight-link
;; This function is used as a callback when users click on qweb-block names.

(defun qweb-highlight-link ()
  (interactive)
  (save-excursion
    (goto-char (point-at-bol))
    ;;.fixme[wb-names] Opt. filenames IDs must not be supported here
    (re-search-forward "\\.qweb[ \t]+\\(-\\|\\+\\)[ \t]+\\(.*?\\)[ \t]+\\(-\\|\\+\\)" nil t))
  (let ((begin (point-at-bol))
        (end (point-at-eol))
        (name (match-string-no-properties 2)))
    (if (string= (match-string-no-properties 1) "+")
        (setq end (re-search-forward
                   (concat "^[ \t]*"
                           (qwe-doc-delimiter qwe-doc)
                           "\\.qweb[ \t]+"
                           "-[ \t]+"
                           (regexp-quote name)
                           "[ \t]+-")
                   nil t))
      (setq begin (re-search-backward
                   (concat "^[ \t]*"
                           (qwe-doc-delimiter qwe-doc)
                           "\\.qweb[ \t]+"
                           "\\+[ \t]+"
                           (regexp-quote name)
                           "[ \t]+\\+")
                   nil t)))
    (and begin end (qwe-highlight-selection begin end))))

;;.sss.fn 5.2.4. qweb--search-block

(defun qweb--search-block (qweb-block-name
                              &optional backwards qweb-block-def no-error)
  (qweb--re-search-block
   (regexp-quote qweb-block-name)
   backwards qweb-block-def no-error))

;;.sss.fn 5.2.5. qweb--re-search-block
;; Effectively searches a qweb block name in the current buffer and performs
;; an action according to the result of the last search.

(defun qweb--re-search-block (regexp
                                 &optional backwards qweb-block-def no-error)
  (let ((search-f (or (and backwards 're-search-backward)
                      're-search-forward))
        (pos-of-line-f (or (and backwards 'beginning-of-line)
                           'end-of-line))
        (match-f (or (and backwards 'match-beginning)
                     'match-end))
        (pos-of-buffer (or (and backwards (point-max)) (point-min)))
        (qweb-block-regexp
         ;;.fixme[wb-names] Opt. filenames IDs supported in '(?:...)'
         (concat "\\.qweb[ \t]+\\(?:\\[.+\\]\\)?<\\([ \t]*" regexp "[ \t]*\\)>"
                 (or (and qweb-block-def
                          "\\+?=")
                     "")))
        (found nil))

    ;; Search qweb-block
    (save-excursion
      (funcall pos-of-line-f)
      (setq found (funcall search-f qweb-block-regexp nil t)))

    ;; Act according the result of the last search.
    (cond
     (found (setq qweb--last-search-failed nil)
            (goto-char (funcall match-f 1))
            (qwe-highlight-selection (match-beginning 1) (match-end 1))
            (message (concat
                      (propertize (format "Search %s %s"
                                          (if backwards "backward" "forward")
                                          regexp)
                                  'face 'minibuffer-prompt))))
     ((and qweb--last-search-failed ;;.tip wrap search
           (string= qweb--prev-search regexp))
      (setq qweb--last-search-failed nil)
      (setq qweb--prev-search nil)
      (goto-char pos-of-buffer)
      (funcall search-f qweb-block-regexp nil t)
      (goto-char (funcall match-f 1))
      (message (propertize (format "Wrapped search %s" regexp)
                           'face 'minibuffer-prompt))
      (qwe-highlight-selection
       (match-beginning 1)
       (match-end 1)))
     (no-error
      (goto-char pos-of-buffer)
      (funcall search-f qweb-block-regexp nil t))
     (t ;;.tip signal error
      (message (concat
                (propertize (format "Failing search %s "
                                    (if backwards "backward" "forward"))
                            'face 'minibuffer-prompt)
                (propertize regexp 'face 'isearch-fail)))
      (setq qweb--prev-search regexp)
      (setq qweb--last-search-failed t)))))


;;.sec 6. Obtaining Source Code from QWEB
;; The process of creating a new buffer or file with the source code of the
;; program is not as complex as it seems. There are, however, few
;; considerations to be carefull with.

;;.sse.fn 6.1. qweb-make-source-code
;; This is the main entry point to generate the source code file. It
;; generates the source code from the current buffer, perform all needed
;; actions and finally switchs to the newly generated buffer.
;;
;; The algorithm could be depicted as follows (using a {e/qweb-like} notation):
;;
;;.ver  <Make source code>=
;;.ver     <Scan the buffer to get all qweb block definitions>
;;.ver     <Create the new buffer and make it the current one>
;;.ver     <Paste main qweb block>
;;.ver     <Recursively, for every qweb block reference, paste qweb block definition>
;;.ver     <Make final cleaning and other arrangements>

(defun qweb-make-source-code ()
  "Creates a new buffer containing source code."
  (interactive)
  (let* ((qweb-cfg-alist
          (mapcar
           (lambda (X) `(,(car X) . ,(car (last (cdr X)))))
           (qwe-cfg-get-items-alist "qweb" nil nil t)))
         (mmode major-mode)
         (qweb-src-buffer (current-buffer))
         (qweb-dst-buffer nil)
         (qweb-block-file nil)   ;;.tip The file containing  qweb-block-name
         (qweb-block-name
          (or (qwe-cfg-get-value "qweb.MAIN" qweb-cfg-alist)
              "main"))
         (qweb-dst-file (qwe-cfg-get-value "qweb.FILENAME" qweb-cfg-alist))
         (overwrite (not (string= "yes" (qwe-cfg-get-value
                                         "qweb.file.overwrite" qweb-cfg-alist))))
         (regexp (concat
                  "^[ \t]*"
                  (qwe-doc-delimiter qwe-doc)
                  ;;.fixme[wb-names] Opt. filenames IDs supported in '(?:...)'
                  "\\.qweb\\(?:\\[\\(.+\\)\\]\\)?[ \t]+"
                  "<[ \t]*\\(.+?\\)[ \t]*>[ \t]*$")))

    ;;.par Update config items
    (setq qweb--mark-boqb (not (string= "no" (qwe-cfg-get-value
                                              "qweb.mark-boqb" qweb-cfg-alist))))
    (setq qweb--mark-eoqb (not (string= "no" (qwe-cfg-get-value
                                              "qweb.mark-eoqb" qweb-cfg-alist))))

    ;;.par Update list of variables defined
    (setq qweb--vars-alist (mapcar
                            (lambda (X) `(,(car X) . ,(car (last (cdr X)))))
                            (qwe-cfg-get-items-alist "qweb" "[A-Z_-]+" t t)))

    ;;.par Scan QWEB buffer
    ;; Scan current buffer to update qweb-block definitions
    (qweb-scan-buffer) ;; {v/qweb-block-alist} contains the list of qweb-block definitions

    ;;.par Create a new buffer and make it the current one
    (setq qweb-dst-buffer (generate-new-buffer "*qweb*"))
    (set-buffer qweb-dst-buffer)

    ;;.par 'main' qweb-block
    ;; Its default name is {t/'main'}, but can be explicitly changed using the
    ;; configuration key {t/'qweb.main'}.
    (setq qweb--blocks-visited-list nil)
    (qweb--substitute-qweb-blocks qweb-src-buffer
                                  qweb-block-name
                                  qweb-dst-buffer)

    ;;.par Final cleaning
    (let ((qwe-doc-delimiter (qwe-doc-delimiter
                              (buffer-local-value 'qwe-doc qweb-src-buffer))))

      ;;.par Convert qweb-block definition into qweb-block names
      ;;.todo Use file-name in qweb-block reference
      (goto-char (point-min))
      (setq regexp (concat "^[ \t]*" qwe-doc-delimiter
                           ;;.fixme[wb-names] Opt. filenames IDs supported in '(?:...)'
                           "\\.qweb\\(?:\\[.+\\]\\)?\\(?1:[ \t]+\\)"
                           "\\(?2:<[ \t]*\\).+?\\(?3:[ \t]*>\\)"
                           "\\(?4:.*\\)$"))
      (while (re-search-forward regexp nil t)
        (if qweb--mark-boqb
            (progn (replace-match " " t t nil 1)
                   (replace-match "+ " t t nil 2)
                   (replace-match " +" t t nil 3)
                   (delete-region (match-beginning 4) (match-end 4)))
          (kill-whole-line)))

    ;;.par Arrange QWEB errors detected
      (goto-char (point-min))
      (while (re-search-forward "^qweb\\.error .*$" nil t)
        (beginning-of-line)
        (delete-char 4)
        (insert qwe-doc-delimiter))

      ;;.par Delete blank lines at end of buffer
      (goto-char (point-max))
      (delete-blank-lines)
      (goto-char (point-min))

      ;;.par Presentation according to major mode
      (when (fboundp mmode)
        (funcall mmode)

        ;;.bug This has to be done ONLY
        ;; when the generated buffer or file has a 'qwe-delimiter-tag'
        ;; different from the empty string
        (string-rectangle (point-min) (point-max) " ")
        (indent-region (point-min) (point-max))

        (setq qwe-delimiter-tag (or (and qweb-cfg-alist
                                         (qwe-cfg-get-value "qweb.qwe-delimiter-tag" qweb-cfg-alist))
                                    "qwe"))
        (qwe-mode)

        ;; Replace delimiter-tag with new delimiter-tag in all language
        ;; elements and text lines, and change {t/'qwe-doc'} accordingly.
        (when (> (string-width qwe-doc-delimiter) 0)
          (goto-char (point-min))
          (let ((regexp
                 (concat "\\("
                         (regexp-quote qwe-doc-delimiter)
                         "\\)"
                         "\\(?:\\." (qwe-lang-regexp) "\\)?")))
            (while (re-search-forward regexp nil t)
              (replace-match
               (qwe-doc-delimiter qwe-doc)
               t t nil 1)))))

      ;;.par File arrangements
      (when qweb-dst-file
        (write-file qweb-dst-file (not (null overwrite))))

      ;;.par Show source code buffer
      (goto-char (point-min))
      (switch-to-buffer qweb-dst-buffer))))

(defun qweb--substitute-qweb-blocks (qweb-src-buffer qweb-block-name qweb-dst-buffer)
  "Substitute `qweb-block-name' by its definition.

Substitution of `qweb-block-name' is complete: all QWEB blocks
references inside `qweb-block-name' are also substituted by their
definitions, taking into account recursivity and circularities.
"

  ;;.par Add {v/qweb-block-name} to the list of visited QWEB blocks
  (add-to-list 'qweb--blocks-visited-list qweb-block-name)

  ;;.par Substitute {v/qweb-block-name}
  (let ((recurse-error nil)
        (eoqb-marker (make-marker)))
    (set-marker eoqb-marker
          (qweb--insert-qweb-block-from-buffer-or-file qweb-src-buffer
                                                       qweb-block-name
                                                       qweb-dst-buffer))

    ;;.par Recursively replace all qweb-block refences used inside {v/qweb-block-name}
    (while (and (re-search-forward regexp eoqb-marker t)
                (not recurse-error))
      (setq file-name (match-string-no-properties 1))
      (setq qweb-block-name (match-string-no-properties 2))
      (kill-whole-line)
      (if (member qweb-block-name qweb--blocks-visited-list) ;;.tip Recursion detected
          (progn
            ;;.del (setq recurse-error t) ;;.tip show all errors
            (insert (format "qweb.error Recursion in block < %s > : " qweb-block-name))
            (let ((path (reverse qweb--blocks-visited-list)))
              (while (not (string= qweb-block-name (car path)))
                  (setq path (cdr path)))
              (dolist (p path)
                (insert (format "%s -> " p))))
            (insert (format "%s \n" qweb-block-name)))
        (qweb--substitute-qweb-blocks qweb-src-buffer qweb-block-name qweb-dst-buffer)))
    (setq qweb--blocks-visited-list (cdr qweb--blocks-visited-list))))

(defun qweb--insert-qweb-block-from-buffer-or-file (qweb-src-buffer
                                                    qweb-block-name
                                                    qweb-dst-buffer)
  "Substitute a QWEB block reference by its definitions.

Get `QWE-BLOCK-NAME' from `BUFFER-OR-FILE' and inserts its
contents into the current position of `QWEB-BUFFER'. Return the
position in `QWEB-BUFFER' of the end of the substituted block.
"
;;.bug bug#30900
  (let* (
         ;;.del (qweb-src-buffer (if (bufferp buffer-or-file)
         ;;.del                      buffer-or-file
         ;;.del                    (find-file-noselect buffer-or-file)))
         (file-name (file-name-nondirectory (buffer-file-name qweb-src-buffer)))
         (qweb-regexp (concat "^[ \t]*"
                              (qwe-doc-delimiter
                               (buffer-local-value 'qwe-doc qweb-src-buffer))
                              ;;.fixme[wb-names] That's Ok?
                              "\\.qweb.*<.*>"))
         (begin (point-at-bol))
         (end (make-marker))
         (region-list (cdr (assoc qweb-block-name
                                  (buffer-local-value
                                   'qweb-block-alist qweb-src-buffer)))))
    (if (> (length region-list) 0)
        (progn
          (dolist (region region-list)
            (insert-buffer-substring-no-properties qweb-src-buffer
                                                   (elt region 0)
                                                   (elt region 1))
            (insert "\n"))

            ;;.par Insert end of qweb-block mark
            (if qweb--mark-eoqb
                (insert (format "%s.qweb - %s -\n"
                                (qwe-doc-delimiter (buffer-local-value
                                                    'qwe-doc qweb-src-buffer))
                                qweb-block-name)))
            (set-marker end (point-at-eol))

            ;;.par Variable substitution
            (dolist (const qweb--vars-alist)
              (goto-char begin)
              (let ((const-regexp
                     (concat "[^\\$]\\(\\$\(" (regexp-quote (car const)) "\)\\)")))
                (while (re-search-forward const-regexp end t)
                  (replace-match (cdr const) t t nil 1))))

            ;;.par '$$' substitution
            (goto-char begin)
            (while (re-search-forward "\\$\\$" end t)
              (replace-match "$" t t))

            ;;.par Create a link to the source qweb-block
            (goto-char begin)
            (when nil;(looking-at qweb-regexp)  ;;.warning disabled
              (goto-char (point-at-bol))
              (cond
               ;;.fixme[wb-names] That's Ok?
               ((re-search-forward "\\.qweb\\(\\[.*\\]\\)" (point-at-eol) t)
                (delete-region (match-beginning 1) (match-end 1)))
               ((re-search-forward "\\.qweb[ \t]+<" (point-at-eol) t)
                (backward-char 1)))
              (insert (concat "[" file-name "]"))
              (goto-char begin))
            end)
      (insert (format "\n%s.warning qweb-block  < %s >  undefined \n"
                      (qwe-doc-delimiter (buffer-local-value
                                          'qwe-doc qweb-src-buffer))
                      qweb-block-name)))))

(defun qweb--block-add (qweb-block)
  (let ((wblock (assoc (qweb-block-name qweb-block) qweb-block-alist)))
    (add-to-list 'qweb-block-alist qweb-block t)
    (if wblock
        (nconc wblock (list `(,(qweb-block-point-min qweb-block)
                              ,(qweb-block-point-max qweb-block))))
      (add-to-list 'qweb-block-alist
                   `(,(qweb-block-name qweb-block)
                     (,(qweb-block-point-min qweb-block)
                      ,(qweb-block-point-max qweb-block))) t))))

(defun qweb-scan-buffer ()
  (interactive)
  (let ((qweb-block (new-qweb-block)))
    (setq qweb-block-alist (list))
    (save-excursion
      (goto-char (point-min))
      (while (qweb--get-next-qweb-block qweb-block)
        (qweb--block-add qweb-block)))))

;; Return begin and end points of next qweb-block starting at the point
;; position. Moves the point at the end of the block found.
;;
(defun qweb--get-next-qweb-block (qweb-block)
  (let* ((qweb-block-def-regexp
          (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                  ;;.fixme[wb-names] Opt. filenames IDs supported in '(?:...)'
                  "\\.qweb\\(?:\\[.+\\]\\)?[ \t]+"
                  "<[ \t]*\\(.+?\\)[ \t]*>\\+?="
                  "[ \t]*$"))
         (qweb-block-def-or-end-regexp
          (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                  ;;.fixme[wb-names] Opt. filenames IDs supported in '(?:...)'
                  "\\.qweb\\(?:\\[.+\\]\\)?[ \t]+"
                  "\\("  "<.+>\\+?="  "\\|"  "<-+>"  "\\)"))
         (qwe-contents
          (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)))
         (qweb-contents
          (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc) "\\.qweb"))
         (found (re-search-forward qweb-block-def-regexp nil t)))
    (cond
     (found
      (setf (qweb-block-name qweb-block)
            (match-string-no-properties 1))
      ;;(forward-line 1) ;;.todo don't store qweb-block names: customizable?
      ;;.del (beginning-of-line)
      (setf (qweb-block-point-min qweb-block) (point-at-bol))

      ;; Search the end of the qweb-block
      (forward-line 1)
      (when (re-search-forward qweb-block-def-or-end-regexp nil t)
        (let ((name (match-string-no-properties 1)))
          (forward-line -1)
          ;;.del (goto-char (point-at-bol))
          ;;.del (unless (string-match "<-+>" name)
          ;;.del   (while (and (looking-at qwe-contents)
          ;;.del               (not (looking-at qweb-contents)))
          ;;.del     (forward-line -1)))
          ))

      (setf (qweb-block-point-max qweb-block)
            (point-at-eol)))
     (t nil))))


;;.sec 7. Colophon
;; To use this extension in {t/Emacs} put
;;
;;.ver    (require 'qweb)
;;
;; in the appropriate place, usually after {t/QWE} has been loaded.

(provide 'qweb)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qweb.el ends here
