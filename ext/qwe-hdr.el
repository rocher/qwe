;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-hdr.el}
;;.cfg.prj.file.desc      Header management
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
;;.ttl Header Management
;;.cmt
;;.quo                                  Modularity means your program is
;;.quo                        built like the universe. The world is made of
;;.quo                        molecules, which are made of atoms, electrons,
;;.quo                        nucleons, and quarks. Likewise good programs
;;.quo                        erect large systems from smaller ones, which
;;.quo                        are built from even smaller building
;;.quo                        blocks. And just as atoms combine in novel
;;.quo                        ways, software components should be reusable.
;;.cmt
;;.quo                                  {i/Paul Dilascia}
;;.quo                                  What Makes Good Code Good?
;;.quo                                  MSDN Magazine, July 2004
;;.cmt
;;.cmt
;;.toc.begin Table of Contents
;;
;;.toc.sec       1. Introduction
;;.toc.sse             1.1. Module dependencies
;;
;;.toc.sec       2. User Manual
;;.toc.sse             2.1. Module loading
;;.toc.sse             2.2. Updating headers
;;.toc.sse             2.3. Example
;;
;;.toc.sec       3. Data Structures and Variables
;;.toc.sse             3.1. qwe-hdr-default-filename
;;.toc.sse             3.2. qwe-hdr-default-cfg-key
;;
;;.toc.sec       4. Public Interface
;;.toc.sse             4.1. qwe-hdr-update
;;
;;.toc.sec       5. Colophon
;;.toc.end {Z/ update ToC }
;;.cmt
;;.cmt
;;.sec 1. Introduction
;; This {t/QWE} extension is used to manage and update header config items shared
;; between files of a project. For example, this file you are reading has a
;; header section with several config items. Most of them have the same value
;; in all files of this project (eg, {t/'prj.version'}). So the idea behind this
;; extension is to write their values only once and make them available to
;; other files.
;;
;;.sse 1.1. Module dependencies
;; Customizable items are inserted into the generic {t/qwe-ext} group defined in
;; {t/qwe-ext}.

(eval-when-compile
  (require 'qwe-exi))


;;.sec 2. User Manual
;;
;;.sse 2.1. Module loading
;; First at all you must load this extension using
;;
;;.ver   (require 'qwe-hdr)
;;
;; in the appropriate place, usually before {t/QWE} has been loaded.
;;
;;.sse 2.2. Updating headers
;; The process to update a set of config items in a given file follows these
;; rules:
;;
;;.li1 A set of config items is read from the header file.
;;
;;.li1 The set of config items read from the header file must share a
;;.li1 key-pefix
;;
;;.li1 Every config item read from the header file is updated in the given
;;.li1 file
;;
;;.li1 Config Items in the file not contained in the header file are not
;;.li1 affected at all
;;
;;.li1 The position in the file of the updated config items is irrelevant
;;
;;.par Header file
;; The most useful case is to have a unique header file containing all common
;; config items. The header filename can be provided in several ways, and is
;; determined in this order of priority:
;;
;;.li1 The value of key {t/'header.filename'} in the current file
;;
;;.li1 A given filename to the function {t/'qwe-header-update'} (only useful when
;;.li1 you use this function directly or in a program)
;;
;;.li1 If previous items have no value, the contents of the user customizable
;;.li1 variable {t/'qwe-default-header-filename'} is used, being its default
;;.li1 value {t/'header.qwe'}
;;
;;.par Configuration key
;; The config key to select the element in the set of config items to update
;; can be specified in several ways, and is determined in this order of
;; priority:
;;
;;.li1 The value of the key {t/'header.key'} in the current file
;;
;;.li1 A given key to the function {t/'qwe-header-update'} (only useful when you
;;.li1 use this function directly or in a program)
;;
;;.li1 If previous items have no value, the contents of the user customizable
;;.li1 variable {t/'qwe-default-header-cfg-key'} is used, being its default value
;;.li1 {t/'prj'}
;;
;;.sse 2.3. Example
;; The current file contains a complete example: the header filename used
;; comes from the variable {t/'qwe-default-header-filename'}, and unless you had
;; modified it in your cuerrent {t/Emacs} installation, its default value is
;; {t/'header.qwe'}.  The same occurs to the config key: it comes from the
;; variable {t/'qwe-default-header-cfg-key'}. So all config items prefixed with
;; {t/'prj'} contained into the {t/'header.qwe'} file are susceptible to be updated.
;;
;; Take a look into the [[file:][../header.qwe]] file. Also, please note that the
;; {t/'header.end'} mark is accompanied by a link to automatically update these
;; fields.


;;.sec 3. Data Structures and Variables
;;
;;.sse.var 3.1. qwe-hdr-default-filename

(defcustom qwe-hdr-default-filename
  "header.qwe"
  "Default filename used to share header config items."
  :group 'qwe-ext)

;;.sse.var 3.2. qwe-hdr-default-cfg-key

(defcustom qwe-hdr-default-cfg-key
  "prj"
  "Default config key used to share header config items."
  :group 'qwe-ext)


;;.sec 4. Public Interface
;;
;;.sse.fn 4.1. qwe-hdr-update

(defun qwe-hdr-update (&optional cfg-key filename)
  "Updates all config items contained in the header."
  (interactive "")
  (let* ((count 0)
         (hdr-alist nil)
         (hdr-cfg-key (or (qwe-cfg-get-value "header.key")
                             cfg-key
                             qwe-hdr-default-cfg-key))
         (hdr-file (or (qwe-cfg-get-value "header.filename")
                          filename
                          qwe-hdr-default-filename))
         (hdr-buffer (and (file-exists-p hdr-file)
                             (find-file-noselect hdr-file))))
    (when hdr-buffer
      (with-current-buffer hdr-buffer
        (setq hdr-alist
              (qwe-cfg-get-items-alist qwe-hdr-default-cfg-key nil nil t)))
      (dolist (cfg-item hdr-alist)
        (setq count (1+ count))
        (qwe-cfg-set-value (car cfg-item)
                           (cdr cfg-item))))
    (message "%d config items updated" count)))


;;.sse.fn qwe-hdr-update-files

(defun qwe-hdr-update-files (&optional cfg-key)
  "Updates headers of all files.

Given a header file with a list of files to be updated, this
function updates all headers, when necessary and appropriate, ."
  (interactive "")
  (let ((files-list (split-string (qwe-cfg-get-value "hdr.files") " " t)))
    (dolist (f files-list)
      (save-excursion
        (find-file f)
        (when qwe-mode
          (message (concat "qwe-hdr-update-files. updating file " f))
          (qwe-hdr-update))))))


;;.sec 5. Colophon
;; To use this extension in {t/Emacs} put
;;
;;.ver    (require 'qwe-hdr)
;;
;; in the appropriate place, usually after {t/QWE} has been loaded.

(provide 'qwe-hdr)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode: emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-hdr.el ends here
