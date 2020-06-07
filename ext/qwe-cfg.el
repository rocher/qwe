;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-cfg.el}
;;.cfg.prj.file.desc      QWE configuration interface
;;.cfg.prj.doc.style      book
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
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.chp       1. Configuration Extension
;;
;;.toc.sec             1.1. Creating the context
;;.toc.sse                   1.1.1. Faces
;;.toc.sse                   1.1.2. qwe-face-config-keyword
;;.toc.sse                   1.1.3. QWE context elemets
;;
;;.toc.sec             1.2. Accesing keys
;;.toc.sse                   1.2.1. Single key access
;;.toc.sss                         1.2.1.1. qwe-cfg-get-value
;;.toc.sss                         1.2.1.2. qwe-cfg-set-value
;;.toc.sse                   1.2.2. Key-set access
;;.toc.sss                         1.2.2.1. qwe-cfg-get-items-alist
;;.toc.sse                   1.2.3. Iterative access
;;.toc.sss                         1.2.3.1. qwe-cfg-get-first-item
;;.toc.sss                         1.2.3.2. qwe-cfg-get-next-item
;;
;;.toc.sec             Accessing regions
;;.toc.sse                   Toggling config regions
;;.toc.sss                         qwe-cfg-toggle-region
;;
;;.toc.sec             1.3. Private interface
;;
;;.toc.sec             1.4. Colophon
;;.toc.end {Z/ Update ToC }
;;

(eval-when-compile
  (require 'qwe-exi))

;;.chp 1. Configuration Extension
;; This chapter deals with the configuration extension, a {e/core} {t/QWE} extension
;; aimed to provide an editor-independent way to configure {t/QWE} itself and
;; other extensions.
;;
;; All configuration items consist of a pair key-value. Values can be strings
;; or several lines of text. The formers can be specified using the same key
;; more than one time. Multi-line keys have a special syntax. Here you have
;; an example showing you both tpes of key:
;;
;;.ver    ;;.cfg.doc.name         The Title of the Document
;;.ver    ;;.cfg.doc.date         May 27 2009
;;.ver    ;;.cfg.doc.author       Fooster Barramon
;;.ver    ;;.cfg.doc.keywords     Cooking, meals, receipts,
;;.ver    ;;.cfg.doc.keywords     vitamins, proteins, minerals,
;;.ver    ;;.cfg.doc.keywords     healthy eating
;;.ver    ;;.cfg.doc.licence      GDL
;;.ver    ;;.cfg.doc.license++
;;.ver    ;;.ver
;;.ver    ;;.ver This Document can be freely distributed
;;.ver    ;;.ver under the terms of the Free Documention License
;;.ver    ;;.ver ... <complete text here>
;;.ver    ;;.ver
;;.ver    ;;.cfg.doc.license--
;;
;; In this example, the value of {t/'doc.date'} is {t/'May 27 2009'}, the value of
;; {t/'doc.keywords'} is {t/'Cooking, meals, ..., minerals, healthy eating'} (all
;; values concatenated altogether, with a space between values from different
;; lines). The value of {t/'doc.license++'} is the complete text between
;; {t/'doc.license++'} and {t/'doc.license--'} {e/except} the prefix {t/';;<dot>ver '} of
;; each line. Text after {t/'++'} and {t/'--'} indicators is complete ignored. Only
;; the first appearance of a multi-line key is valid. Both forms of key can
;; have the same name. In the example, keys {t/'doc.license'} and {t/'doc.license++'}
;; are two different keys with different values.
;;
;; Links and format text are preserved as they appear in the text, in all
;; forms of items. To remove them, use the function ;;.fixme PROVIDE A FUNC
;; NAME
;;
;; Please note that keys can be specified somehow in a hierarchical manner
;; using multiple words separated by dots. All keys sharing the same same
;; key-prefix belong to the same {e/key-set} and can be retrieved in an
;; association list with a single function call.
;;
;;.sec 1.1. Creating the context
;;
;;.sse 1.1.1. Faces
;;
;;.sse.face 1.1.2. qwe-face-config-keyword

(defface qwe-face-config-keyword
  '((t (:foreground "slate blue"
        :inherit qwe-face-default-keyword)))
  "Face to display configuration items."
  :group 'qwe-basic-faces)

;;.sse 1.1.3. QWE context elemets

(qwe-exi-new-context
 'config 'cfg ".cfg"
 `(,qwe-lang-context-ID-list-regexp . 'qwe-face-config-keyword)
 '((".*" . 'qwe-face-default-text)))

;;.sec 1.2. Accesing keys
;;
;;.sse 1.2.1. Single key access
;;
;;.sss.fn 1.2.1.1. qwe-cfg-get-value
;; This function lets you access the value associated with a key. It looks
;; for the key into the current buffer. If you have a list of items, then you
;; can use this list as a second argument to search into the list instead of
;; the buffer.

(defun qwe-cfg-get-value (cfg-key &optional cfg-items-alist as-list)
  (when (local-variable-p 'qwe-doc)
    (if cfg-items-alist
        (qwe-cfg--get-value-from-alist cfg-key cfg-items-alist)
      (progn
        (setq qwe-cfg--point nil)
        (qwe-cfg--search-value cfg-key as-list)))))

;;.sss.fn 1.2.1.2. qwe-cfg-set-value

(defun qwe-cfg-set-value (cfg-key value)
  (when (local-variable-p 'qwe-doc)
    (save-excursion
      (goto-char (point-min))
      (let* ((cfg-regexp (concat "^\\([ \t]*" (qwe-doc-delimiter qwe-doc)
                                 "\\.cfg\\.\\)" (regexp-quote cfg-key)
                                 "\\(?:\\([ \t]+\\).*\\([ \t]*\\)\\)?$"))
             (str1 nil)
             (str2 nil)
             (str3 nil))
        (when (re-search-forward cfg-regexp nil t)
          (setq str1 (match-string-no-properties 1))
          (setq str2 (match-string-no-properties 2))
          (setq str3 (match-string-no-properties 3))
          (if (listp value)
              (progn
                (delete-region (point-at-bol) (point-at-eol))
                (insert (concat str1 cfg-key str2 (car value) str3))
                (dolist (val (cdr value))  ;;.tip Insert all strings, one per line
                  (forward-line 1)
                  (if (looking-at cfg-regexp)
                      (delete-region (point-at-bol) (point-at-eol))
                    (progn (insert "\n")
                           (forward-line -1)))
                  (insert (concat str1 cfg-key str2 val str3))))
            (if (string= "++" (substring cfg-key -2))
                (progn ;;.tip Insert all lines
                  (forward-line 1)
                  (let ((begin (point))
                        (end nil)
                        (delim (qwe-doc-delimiter qwe-doc))
                        (delim-len (length (qwe-doc-delimiter qwe-doc)))
                        (cfg-key-end (concat "^[ \t]*"
                                             (qwe-doc-delimiter qwe-doc)
                                             "\\.cfg\\."
                                             (regexp-quote
                                              (substring cfg-key 0 (- (length cfg-key) 2)))
                                             "--"
                                             )))
                    (when (re-search-forward cfg-key-end nil t)
                      (delete-region begin (point-at-bol))
                      (goto-char (point-at-bol))
                      (insert (concat value "\n"))
                      (setq end (point-at-bol))
                      (when (> delim-len 0)
                        (goto-char begin)  ;;.tip make sure all lines are Ok
                        (while (< (point) end)
                          (when (not (looking-at delim))
                            (insert delim)
                            (setq end (+ end delim-len))
                            (forward-line 1)))))))
              (progn
                (delete-region (point-at-bol) (point-at-eol))
                (insert (concat str1 cfg-key str2 value str3)))))
          (forward-line 1)
          (while (looking-at cfg-regexp) ;;.tip Remove surplus lines
            (delete-region (point-at-bol) (1+ (point-at-eol)))))))))

;;.sse 1.2.2. Key-set access
;;
;;.sss.fn 1.2.2.1. qwe-cfg-get-items-alist
;; It is possible to access a {e/set} of items by specifying a prefix shared
;; among all keys. For example, using {t/'prj'} this function returns a list
;; containing {t/'prj.name'}, {t/'prj.author.email'}, etc, items (look at the
;; beginning of this file for more details). The returned list is an
;; association list, so you must use {t/'qwe-cfg-get-value'} passing the key and
;; the list as arguments to obtain the value of an existing key.

(defun qwe-cfg-get-items-alist (cfg-key-prefix
                                &optional key-regexp remove-prefix as-list)
  (let* ((cfg-items-alist nil)
         (cfg-item nil))
    (dolist (key (qwe-cfg--get-keys-matching
                  cfg-key-prefix key-regexp remove-prefix)
                 cfg-items-alist)
      (setq cfg-item (qwe-cfg--search-value
                      (if remove-prefix
                          (concat cfg-key-prefix "." key)
                        key) as-list))
      (add-to-list 'cfg-items-alist
                   `(,key . ,cfg-item) t))))

;;.sse 1.2.3. Iterative access
;; If you want to access all items sequentially, starting at the beginning of
;; the buffer, then you must access the first item with
;; {t/qwe-cfg-get-first-item}. The rest of items can be retrieved with the
;; function {t/qwe-cfg-get-next-item}, until it return a nil item. It is possible
;; to specify a key prefix to limit the earch to those items matching the
;; given regexp.
;;
;;.sss.fn 1.2.3.1. qwe-cfg-get-first-item

(defun qwe-cfg-get-first-item (&optional cfg-key-prefix)
  (setq qwe-cfg--point (point-min))
  (qwe-cfg-get-next-item cfg-key-prefix))

;;.sss.fn 1.2.3.2. qwe-cfg-get-next-item

(defun qwe-cfg-get-next-item (&optional cfg-key-prefix)
  (let ((cfg-key nil)
        (cfg-key-regexp (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                                "\\.cfg\\.\\([^ \t]+\\)[ \t]*")))
    (save-excursion
      (goto-char (or qwe-cfg--point
                     (setq qwe-cfg--point (point-min))))
      (when (re-search-forward cfg-key-regexp nil t)
        (setq cfg-key (match-string-no-properties 1))
        (goto-char (point-at-bol))
        `(,cfg-key .
         ,(qwe-cfg--search-value cfg-key))))))

;;.sec 1.3. Accessing regions
;; Config regions, or {e/sections}, are sets of contiguous lines containig zero
;; or more config keys.
;;
;; Config regions are delimited by the same key, followed by {t/.begin} and {t/.end}
;; eg:
;;
;;.ver     .hdr.begin
;;.ver        ... .hdr config keys ...
;;.ver     .hdr.end
;;
;;.sse 1.3.1. Toggling config regions
;;
;;.sss.fn 1.3.1.1. qwe-cfg-toggle-region
;; This function toggles (shows or hides) config regions

(defun qwe-cfg-toggle-region (cfg-key)
  (save-excursion
    (goto-char (point-min))
    (let* ((begin nil)
           (end nil)
           (visible nil)
           (regexp-begin (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                                 "\\.cfg\\." cfg-key "\\.begin"))
           (regexp-end (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                               "\\.cfg\\." cfg-key "\\.end")))
      (when (re-search-forward regexp-begin nil t)
        (setq begin (point-at-bol))
        (when (re-search-forward regexp-end nil t)
          (forward-line -1)
          (setq end (point-at-bol))
          (when (not (equal begin end))
            (setq visible (not (get-text-property begin 'invisible)))
            (message (format "changing region %s to %s" cfg-key visible))
            (put-text-property begin end 'invisible visible)))))))

;;.sec 1.4. Private interface
;;
;; Variable used to track point between iterative access calls. It also
;; signals, inside these functions, whether the search is iterative or not.

(defvar qwe-cfg--point nil)
(make-variable-buffer-local 'qwe-cfg--point)

(defun qwe-cfg--get-value-from-alist (cfg-key cfg-items-alist)
  (cdr (assoc cfg-key cfg-items-alist)))

(defun qwe-cfg--search-value (cfg-key &optional as-list)
  (save-excursion
    (goto-char (or qwe-cfg--point (point-min)))
    (let ((value (or (and (string-match "\\(.*\\)\\+\\+" cfg-key)
                          (qwe-cfg--search-value-multi-line
                           (match-string-no-properties 1 cfg-key)))
                     (qwe-cfg--search-value-string cfg-key as-list))))
      (if qwe-cfg--point
          (setq qwe-cfg--point (point-at-eol)))
      value)))

(defun qwe-cfg--search-value-string (cfg-key &optional as-list) ;;.note to be used only by qwe-cfg--search-value
  (let ((cfg-value nil)
        (cfg-regexp (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                            "\\.cfg\\." (regexp-quote cfg-key)
                            "[ \t]+\\(.*?\\)[ \t]*$")))
    (while (re-search-forward cfg-regexp nil t)
      (if as-list
          (if cfg-value
              (nconc cfg-value (list (match-string-no-properties 1)))
            (setq cfg-value (list (match-string-no-properties 1))))
        (if cfg-value
            (setq cfg-value (concat cfg-value " " (match-string-no-properties 1)))
          (setq cfg-value (match-string-no-properties 1)))))
    cfg-value))

(defun qwe-cfg--search-value-multi-line (cfg-key) ;;.note to be used only by qwe-cfg--search-value
  (let ((begin nil)
        (end nil)
        (cfg-value nil)
        (cfg-regexp-begin (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                                  "\\.cfg\\." (regexp-quote cfg-key) "\\+\\+"))
        (cfg-regexp-end (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                                "\\.cfg\\." (regexp-quote cfg-key) "\\-\\-")))
    (when (and (setq begin (re-search-forward cfg-regexp-begin nil t))
               (setq end (re-search-forward cfg-regexp-end nil t)))
      (goto-char begin)
      (forward-line 1)
      (setq begin (point))
      (goto-char end)
      (forward-line -1)
      (setq cfg-value (buffer-substring-no-properties begin (point-at-eol)))
      (forward-line 1)
      cfg-value))) ;;.tip Avoid getting keys like 'license--' in iterative access

(defun qwe-cfg--get-keys-matching (cfg-key-prefix
                                   &optional regexp remove-prefix)
  (let ((cfg-key-alist (list))
        (cfg-regexp (concat "^[ \t]*" (qwe-doc-delimiter qwe-doc)
                            "\\.cfg\\."
                            "\\(" (regexp-quote cfg-key-prefix) "\\)"
                            "\\.\\("
                            (or regexp "[^ \t\n]+")
                            "\\)[ \t]+")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cfg-regexp nil t)
          (unless (and (>= (length (match-string-no-properties 2)) 2)
                       (string= "--"
                                (substring (match-string-no-properties 2) -2)))
              (add-to-list 'cfg-key-alist
                           (if remove-prefix
                               (match-string-no-properties 2)
                             (concat (match-string-no-properties 1)
                                     "."
                                     (match-string-no-properties 2))) t))))
      cfg-key-alist))

;;.sec 1.5. Colophon

(provide 'qwe-cfg)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode: emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-cfg.el ends here
