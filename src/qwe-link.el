;;.cfg.header
;;.cfg.prj.name           QWE
;;.cfg.prj.brief          QWE's not WEB for Emacs
;;.cfg.prj.version        0.9.5-pre06
;;.cfg.prj.author.name    Francesc Rocher
;;.cfg.prj.homepage       [[http://github.com/rocher/qwe]]
;;.cfg.prj.keywords       {e/QWE, literate programming, literate software engineering,}
;;.cfg.prj.keywords       {e/software documentation, documentation generation,}
;;.cfg.prj.keywords       {e/lightweight markup language}
;;.cfg.prj.file           {t/qwe-link.el}
;;.cfg.prj.file.desc      Links and references
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

;;.todo Unify message output
;; Look at search functions of file [[file:][../ext/qweb.el]] to see how messages can be
;; shown in a more elegant way.

;;.todo dir:  new link type ??
;; {t/'dir:'} link type should look for {t/'index.qwe'} file. If it exists, then show
;; it, else show the dir in a dired buffer. Current file link type should to
;; this is file name is an accessible directory??

(eval-when-compile
  (require 'qwe-lang))

;;
;;
;;.toc.begin                            Table of Contents
;;
;;.toc.chp       8. Links and References
;;
;;.toc.sec             8.1. History management
;;.toc.sse                   8.1.1. Markers
;;.toc.sse                   8.1.2. Custom Variables
;;.toc.sss                         8.1.2.1. qwe-link-history-marker-filter
;;.toc.sse                   8.1.3. Private Variables
;;.toc.sss                         8.1.3.1. qwe-link-history--list
;;.toc.sss                         8.1.3.2. qwe-link-history--current
;;.toc.sss                         8.1.3.3. qwe-link-history--previous-insert
;;.toc.sse                   8.1.4. Public Interface
;;.toc.sss                         8.1.4.1. qwe-link-history-insert-marker
;;.toc.sss                         8.1.4.2. qwe-link-history-go-back
;;.toc.sss                         8.1.4.3. qwe-link-history-go-forward
;;.toc.sse                   8.1.5. Private Interface
;;.toc.sss                         8.1.5.1. qwe-link--marker-file-name
;;.toc.sss                         8.1.5.2. qwe-link--marker-position
;;.toc.sss                         8.1.5.3. qwe-link--marker-equal
;;.toc.sss                         8.1.5.4. qwe-link--marker-valid-p
;;.toc.sss                         8.1.5.5. qwe-link--marker-navigable-p
;;.toc.sss                         8.1.5.6. qwe-link--marker-at-point
;;.toc.sss                         8.1.5.7. qwe-link-history--update-current-marker
;;.toc.sss                         8.1.5.8. qwe-link-history--delete-current-marker
;;.toc.sss                         8.1.5.9. qwe-link-history--go-to
;;.toc.sse                   8.1.6. Hooks
;;.toc.sss                         8.1.6.1. qwe-link-history--find-file-hook
;;.toc.sss                         8.1.6.2. qwe-link-history--kill-buffer-hook
;;
;;.toc.sec             8.2. Links
;;.toc.sse                   8.2.1. Public Interface
;;.toc.sss                         8.2.1.1. qwe-link-follow
;;.toc.sse                   8.2.2. Private Interface
;;.toc.sss                         8.2.2.1. qwe-link--data
;;.toc.sss                         8.2.2.2. qwe-link-follow--anchor
;;.toc.sss                         8.2.2.3. qwe-link-follow--compile
;;.toc.sss                         8.2.2.4. qwe-link-follow--elisp
;;.toc.sss                         8.2.2.5. qwe-link-follow--file
;;.toc.sss                         8.2.2.6. qwe-link-follow--image
;;.toc.sss                         8.2.2.7. qwe-link-follow--info
;;.toc.sss                         8.2.2.8. qwe-link-follow--man
;;.toc.sss                         8.2.2.9. qwe-link-follow--annotation
;;.toc.sss                         8.2.2.10. qwe-link-follow--qwe
;;.toc.sss                         8.2.2.11. qwe-link-follow--regexp
;;.toc.sss                         8.2.2.12. qwe-link-follow--section
;;.toc.sss                         8.2.2.13. qwe-link-follow--shell
;;.toc.sss                         8.2.2.14. qwe-link-follow--symbol
;;.toc.sss                         8.2.2.15. qwe-link-follow--url
;;
;;.toc.sec             8.3. Colophon
;;.toc.end {R/[eot]}
;;
;;.chp 8. Links and References
;;
;;.quo                                  Many programming environments are
;;.quo                        completely controlled by specific vendors, who
;;.quo                        may well choose to switch from flat text to
;;.quo                        rich markup for their own reasons. If Microsoft
;;.quo                        had made source files XML, tens of thousands of
;;.quo                        programmers would already be putting pictures
;;.quo                        and hyperlinks in their code. Programming on
;;.quo                        the universal canvas is one revolution that
;;.quo                        can't possibly arrive too soon.
;;.quo
;;.quo                                  {i/Gregory Wilson}
;;.quo                                  XML-Based Programming Systems
;;.quo                                  Dr. Dobbs Journal, March 2003
;;
;;.sec 8.1. History management
;;
;;.sse 8.1.1. Markers
;; The following structure resembles standard {t/Emacs} {e/markers}, but with the
;; difference that these are {e/persistent}. {t/Emacs} markers point nowhere when the
;; buffer they point is deleted. {t/qwe-marker}'s store only the file name and
;; the position of a visited location.

(defstruct qwe-marker
  (file-name (buffer-file-name))
  (position  (point)))

;;.sse 8.1.2. Custom Variables
;;
;;.sss.var 8.1.2.1. qwe-link-history-marker-filter

(defcustom qwe-link-history-marker-filter 'files
  "Type of markers to be saved in the history list.

The current location is updated in the history list every time
user navigates one link backward or forward. This variable
controls whether the current location must be updated depending
on the buffer or file the point is curretnly located.

If you select 'Only files' then only locations referring to
existing files will be saved, and locations referring to buffers
without files will be ignored."

  :type '(choice :tag "Filter"
                 (const :tag "All buffers" all)
                 (const :tag "Only files" files))
  :safe (lambda (s)
          (and (symbolp s)
               (or (equal s 'all)
                   (equal s 'files))))
  :group 'qwe)

;;.sse 8.1.3. Private Variables
;;
;;.sss.var 8.1.3.1. qwe-link-history--list

(defvar qwe-link-history--list nil
  "List of visited links.
The elements of the list are of type marker or qwe-marker. To
manage the history of visited links, see
`qwe-link-history-insert-marker', `qwe-link-history-go-forward'
and `qwe-link-history-go-back'.")

;;.sss.var 8.1.3.2. qwe-link-history--current

(defvar qwe-link-history--current -1
  "Current link in `qwe-link-history--list'.")

;;.sss.var 8.1.3.3. qwe-link-history--previous-insert

(defvar qwe-link-history--previous-insert nil
  "If t, the previous operation was `qwe-link-history-insert-marker'.")

;;.sse 8.1.4. Public Interface
;;
;;.sss.fn 8.1.4.1. qwe-link-history-insert-marker

(defun qwe-link-history-insert-marker (&optional marker)
  (interactive)
  (let* ((insertion nil)
         (marker (or marker (point-marker)))
         (position (marker-position marker))
         (buffer (marker-buffer marker)))
    (when (and qwe-link-history--list
               (qwe-link--marker-valid-p marker)
               (not (qwe-link--marker-equal marker
                                            (nth qwe-link-history--current qwe-link-history--list))))
      (setq insertion t)
      (if (< (1+ qwe-link-history--current) (length qwe-link-history--list))
          (setcdr (nthcdr qwe-link-history--current qwe-link-history--list) nil))
      (setq qwe-link-history--current (1+ qwe-link-history--current))
      (nconc qwe-link-history--list (list (point-marker))))

    (unless qwe-link-history--list
      (setq insertion t)
      (setq qwe-link-history--current 0)
      (setq qwe-link-history--list (list (point-marker))))

  (when insertion
    (setq qwe-link-history--previous-insert t)
    (if marker
        (set-marker (nth qwe-link-history--current qwe-link-history--list)
                    position buffer)))))

;;.sss.fn 8.1.4.2. qwe-link-history-go-back

(defun qwe-link-history-go-back ()
  (interactive)
  (when (>= qwe-link-history--current 0)
    (if qwe-link-history--previous-insert
        (qwe-link-history-insert-marker)
      (qwe-link-history--update-current-marker))
    (setq qwe-link-history--previous-insert nil)

    ;; Look backwards for the first valid link, deleting invalid ones.
    (let ((found nil))
      (while (and (not found)
                  (> qwe-link-history--current 0))
        (setq qwe-link-history--current (1- qwe-link-history--current))
        (let ((marker (nth qwe-link-history--current qwe-link-history--list)))
          (if (setq found (qwe-link--marker-navigable-p marker))
              (qwe-link-history--go-to marker)
            (qwe-link-history--delete-current-marker t)))))))

;;.sss.fn 8.1.4.3. qwe-link-history-go-forward

(defun qwe-link-history-go-forward ()
  (interactive)
  (when (< (+ 1 qwe-link-history--current) (length qwe-link-history--list))
    (qwe-link-history--update-current-marker)
    (setq qwe-link-history--previous-insert nil)

    ;; Look forwards for the first valid link, deleting invalid ones.
    (let ((found nil))
      (while (and (not found)
                  (< (1+ qwe-link-history--current) (length qwe-link-history--list)))
        (setq qwe-link-history--current (1+ qwe-link-history--current))
        (let ((marker (nth qwe-link-history--current qwe-link-history--list)))
          (if (setq found (qwe-link--marker-navigable-p marker))
              (qwe-link-history--go-to marker)
            (qwe-link-history--delete-current-marker)))))))

;;.sse 8.1.5. Private Interface
;;
;;.sss.fn 8.1.5.1. qwe-link--marker-file-name

(defun qwe-link--marker-file-name (marker)
  (if (qwe-marker-p marker)
      (qwe-marker-file-name marker)
    (buffer-file-name (marker-buffer marker))))

;;.sss.fn 8.1.5.2. qwe-link--marker-position

(defun qwe-link--marker-position (marker)
  (if (qwe-marker-p marker)
      (qwe-marker-position marker)
    (marker-position marker)))

;;.sss.fn 8.1.5.3. qwe-link--marker-equal

(defun qwe-link--marker-equal (m1 m2)
  (and (equal (qwe-link--marker-file-name m1)
              (qwe-link--marker-file-name m2))
       (equal (qwe-link--marker-position m1)
              (qwe-link--marker-position m2))))

;;.sss.fn 8.1.5.4. qwe-link--marker-valid-p
;; Returns {t/t} if the marker refers to a buffer or file that satisfies the user
;; criteria to be included in the link history.

(defun qwe-link--marker-valid-p (marker)
  (cond
   ((equal qwe-link-history-marker-filter 'all)
    (qwe-link--marker-navigable-p marker))
   ((equal qwe-link-history-marker-filter 'files)
    (file-exists-p (qwe-link--marker-file-name marker)))))

;;.sss.fn 8.1.5.5. qwe-link--marker-navigable-p
;; Returns {t/t} if the marker is navigable, that is, if the buffer still is
;; alive or the file exists.

(defun qwe-link--marker-navigable-p (marker)
  (if (qwe-marker-p marker)
      (file-exists-p (qwe-marker-file-name marker))
    (buffer-live-p (marker-buffer marker))))

;;.sss.fn 8.1.5.6. qwe-link--marker-at-point

(defun qwe-link--marker-at-point (&optional point buffer)
  (let ((p (or point (point)))
        (b (or buffer (current-buffer)))
        (marker (make-marker)))
    (set-marker marker p buffer)
    marker))

;;.sss.fn 8.1.5.7. qwe-link-history--update-current-marker

(defun qwe-link-history--update-current-marker ()
  (let ((marker (point-marker)))
    (when (and (>= qwe-link-history--current 0)
               (qwe-link--marker-valid-p marker))
      (setcar (nthcdr qwe-link-history--current qwe-link-history--list) marker))))

;;.sss.fn 8.1.5.8. qwe-link-history--delete-current-marker

(defun qwe-link-history--delete-current-marker (&optional backwards)
  (if (> qwe-link-history--current 0)
      (progn
        (setcdr (nthcdr (1- qwe-link-history--current) qwe-link-history--list)
                (nthcdr (1+ qwe-link-history--current) qwe-link-history--list))
        (if backwards
            (setq qwe-link-history--current (1- qwe-link-history--current))
          (if (>= qwe-link-history--current (1- (length qwe-link-history--list)))
              (setq qwe-link-history--current (1- (length qwe-link-history--list))))))
    (progn
      (setq qwe-link-history--list (cdr qwe-link-history--list))
      (unless qwe-link-history--list
        (setq qwe-link-history--current -1)))))

;;.sss.fn 8.1.5.9. qwe-link-history--go-to

(defun qwe-link-history--go-to (marker)
  (find-file (qwe-link--marker-file-name marker))
  (goto-char (qwe-link--marker-position marker)))

;;.sse 8.1.6. Hooks
;;
;;.sss.fn 8.1.6.1. qwe-link-history--find-file-hook
;; Substitute all objects of type {t/qwe-marker} pointing to the file being
;; opened by markers.

(defun qwe-link-history--find-file-hook ()
  (let ((file-name (buffer-file-name))
        (buffer (current-buffer))
        (position nil)
        (n 0))
    (dolist (marker qwe-link-history--list t)
      (when (and (qwe-marker-p marker)
                 (equal file-name (qwe-marker-file-name marker)))
        (setq position (qwe-marker-position marker))
        (setcar (nthcdr n qwe-link-history--list) (make-marker))
        (set-marker (nth n qwe-link-history--list) position buffer))
      (setq n (1+ n)))))

;;.sss.fn 8.1.6.2. qwe-link-history--kill-buffer-hook
;;
;; Subtitute all markers of buffer being killed by {t/qwe-marker} objects
;; containing the same filename and position.

(defun qwe-link-history--kill-buffer-hook ()
  (let ((file-name (buffer-file-name))
        (buffer (current-buffer))
        (position nil)
        (n 0))
    (dolist (marker qwe-link-history--list t)
      (when (and (not (qwe-marker-p marker))
                 (equal buffer (marker-buffer marker)))
        (setq position (marker-position marker))
        (setcar (nthcdr n qwe-link-history--list) (make-qwe-marker))
        (setf (qwe-marker-file-name (nth n qwe-link-history--list)) file-name)
        (setf (qwe-marker-position (nth n qwe-link-history--list)) position))
      (setq n (1+ n)))))

;;.par Install hooks

(add-hook 'find-file-hook 'qwe-link-history--find-file-hook)
(add-hook 'kill-buffer-hook 'qwe-link-history--kill-buffer-hook)


;;.sec 8.2. Links
;; This section deals with links. {t/QWE} links can manage several {e/protocols},
;; {e/sections}, {e/expressions} and other {e/help systems} supported by Emacs, as {e/man}
;; and {e/info pages}, {e/symbol help} depending on the major mode, etc.
;;
;; There are three ways or syntax to write the same link: {b/simple}, {b/separated}
;; and {b/descriptive}. The difference between them is the {e/click-able} string
;; displayed to the user, offering the strinct syntax of the link or a more
;; descriptive link.
;;
;;.sse 8.2.1. Public Interface
;;
;;.sss.fn 8.2.1.1. qwe-link-follow

(defun qwe-link-follow ()
  (interactive "")
  (if (get-char-property (point) 'callback)
      (funcall (get-char-property (point) 'callback)) ;;.tip Use the callback property first
    (qwe-link-follow--link)))

(defun qwe-link-follow--link ()
  (when (get-char-property (point) 'follow-link)
    (let* ((data (qwe-link--data (point)))
           (type (elt data 0))
           (ref (elt data 1))
           (href (elt data 2))
           (file (elt data 3))
           (marker (elt data 4)))

      ;;.par switch depending on the link type
      ;;
      (cond
       ((string= type "a")                                             ;;.tip anchor
        (qwe-link-follow--anchor 'anchor marker ref nil file))
       ((string-match (regexp-opt '("app" "appendix")) type)           ;;.tip appendix
        (qwe-link-follow--section marker "chp" ref file t))
       ((string= type "compile")                                       ;;.tip compile
        (qwe-link-follow--compile marker ref))
       ((string= type "elisp")                                         ;;.tip elisp
        (qwe-link-follow--elisp marker ref file))
       ((string= type "file")                                          ;;.tip file
        (when (qwe-link-follow--file ref t)
          (qwe-link-history-insert-marker marker)))
       ((string-match "image\\(-\\(left\\|center\\|right\\)\\)?" type) ;;.tip image
        (qwe-link-follow--image begin end ref))
       ((string= type "info")                                          ;;.tip info
        (qwe-link-follow--info marker ref))
       ((string= type "man")                                           ;;.tip man
        (qwe-link-follow--man marker ref))
       ((string= type "qwe")
        (qwe-link-follow--qwe marker ref file))
       ((string-match qwe-section-regexp type)                         ;;.tip section
        (qwe-link-follow--section marker type ref file))
       ((string= type "shell")                                         ;;.tip shell
        (qwe-link-follow--shell marker ref))
       ((string= type "symbol")                                        ;;.tip symbol
        (qwe-link-follow--symbol marker ref))
       ((string= type "todo");;.todo[current-work] GENERALIZE TO ALL ANNOTATION TYPES
        (qwe-link-follow--anchor 'todo marker nil ref file))
       (t                                                              ;;.tip external links
        (qwe-link-follow--url marker type ref file))))))

(defun qwe-link--help-echo ()
    (let* ((data (qwe-link--data (point)))
           (type (elt data 0))
           (ref (elt data 1))
           (href (elt data 2))
           (file (elt data 3))
           (marker (elt data 4)))
      ))

;;.del  (defun OLD_qwe-link-follow--link (help-echo)
;;.del    (when (get-char-property (point) 'follow-link)
;;.del      (let ((begin 0)
;;.del            (middle 0)
;;.del            (end 0)
;;.del            (href nil)
;;.del            (desc nil)
;;.del            (type nil)
;;.del            (ref nil)
;;.del            (pos nil)
;;.del            (file nil)
;;.del            (marker (make-marker)))
;;.del        (save-excursion
;;.del
;;.del          ;;.par search the end of the link
;;.del          ;;.del (setq end (next-property-change (point)))
;;.del          (setq end (re-search-forward "\\]\\]"))
;;.del
;;.del          ;;.par search the begining of the link
;;.del          (goto-char (setq middle (previous-property-change (point))))
;;.del          ;;.del (goto-char (setq begin (previous-property-change (point))))
;;.del          (setq begin (re-search-backward "\\[\\["))
;;.del
;;.del          ;;.par Update the marker
;;.del          (set-marker marker begin (current-buffer))
;;.del
;;.del          (if (get-char-property (1- end) 'include)
;;.del              (setq type "file"
;;.del                    ref (buffer-substring-no-properties middle end))
;;.del            (progn
;;.del
;;.del              ;;.par obtain the link, both parts: href and description
;;.del              (when
;;.del                  (re-search-forward "\\[?\\[?\\([^][]*?\\)\\]\\(\\[\\([^][]*?\\)\\]\\)?\\]" (+ end 2) t)
;;.del                ;;.del (re-search-forward "\\[\\[\\([^][]*?\\)\\]\\(\\[\\([^][]*?\\)\\]\\)?\\]" (+ end 2) t)
;;.del                (setq href (match-string-no-properties 1))
;;.del                (setq desc (match-string-no-properties 3))
;;.del                ;;.del (if href (message "href='%s'" href)) ;;.warning debug only
;;.del                ;;.del (if desc (message "desc='%s'" desc)) ;;.war debug only
;;.del                )
;;.del
;;.del              ;;.par obtain the link type
;;.del              (when (and href
;;.del                         (string-match "\\(file:\\([^,]+\\), *\\)?\\([^:]+\\):\\(.*\\)" href))
;;.del                (setq file (match-string-no-properties 2 href))
;;.del                ;;.del (if file (message "file='%s'" file)) ;;.warning debug only
;;.del                (setq type (match-string-no-properties 3 href))
;;.del                ;;.del (if type (message "type='%s'" type)) ;;.warning debug only
;;.del                (setq ref  (match-string-no-properties 4 href))
;;.del                ;;.del (if ref (message "ref='%s'" ref)) ;;.warning debug only
;;.del                )
;;.del                (if (= (length ref) 0) (setq ref desc)))))
;;.del
;;.del        ;;.par switch depending on the link type
;;.del        ;;
;;.del        (cond
;;.del         ((string= type "a")                                             ;;.tip anchor
;;.del          (if help-echo
;;.del              (format " Go to anchor '%s' " ref)
;;.del            (qwe-link-follow--anchor 'anchor marker ref nil file)))
;;.del         ((string-match (regexp-opt '("app" "appendix")) type)           ;;.tip appendix
;;.del          (if help-echo
;;.del              (format " Visit appendix '%s' " ref)
;;.del            (qwe-link-follow--section marker "chp" ref file t)))
;;.del         ((string= type "compile")                                       ;;.tip compile
;;.del          (if help-echo
;;.del              (message (format " Compile with '%s' " ref))
;;.del            (qwe-link-follow--compile marker ref)))
;;.del         ((string= type "elisp")                                         ;;.tip elisp
;;.del          (if help-echo
;;.del              (format " Eval '%s' " ref)
;;.del            (qwe-link-follow--elisp marker ref file)))
;;.del         ((string= type "file")                                          ;;.tip file
;;.del          (if help-echo
;;.del              (format " Visit file '%s' " ref)
;;.del            (when (qwe-link-follow--file ref t)
;;.del              (qwe-link-history-insert-marker marker))))
;;.del         ((string-match "image\\(-\\(left\\|center\\|right\\)\\)?" type) ;;.tip image
;;.del          (qwe-link-follow--image begin end ref))
;;.del         ((string= type "info")                                          ;;.tip info
;;.del          (qwe-link-follow--info marker ref))
;;.del         ((string= type "man")                                           ;;.tip man
;;.del          (qwe-link-follow--man marker ref))
;;.del         ((string= type "qwe")
;;.del          (qwe-link-follow--qwe marker ref file))
;;.del         ((string-match qwe-section-regexp type)                         ;;.tip section
;;.del          (qwe-link-follow--section marker type ref file))
;;.del         ((string= type "shell")                                         ;;.tip shell
;;.del          (qwe-link-follow--shell marker ref))
;;.del         ((string= type "symbol")                                        ;;.tip symbol
;;.del          (qwe-link-follow--symbol marker ref))
;;.del         ((string= type "todo");;.todo[current-work] GENERALIZE TO ALL ANNOTATION TYPES
;;.del          (qwe-link-follow--anchor 'todo marker nil ref file))
;;.del         (t                                                              ;;.tip external links
;;.del          (qwe-link-follow--url marker type ref file))))))

;;.sse 8.2.2. Private Interface
;;
;;.sss.fn 8.2.2.1. qwe-link--data

(defun qwe-link--data (pos)
  (let ((begin 0)
        (middle 0)
        (end 0)
        (href nil)
        (desc nil)
        (type nil)
        (ref nil)
        (file nil)
        (marker (make-marker)))
    (save-excursion

      ;;.par search the end of the link
      ;;.del (setq end (next-property-change (point)))
      (goto-char pos)
      (setq end (re-search-forward "\\]\\]"))

      ;;.par search the begining of the link
      (goto-char (setq middle (previous-property-change pos)))
      ;;.del (goto-char (setq begin (previous-property-change (point))))
      (setq begin (re-search-backward "\\[\\["))

      ;;.par Update the marker
      (set-marker marker begin (current-buffer))

        (if (get-char-property (1- end) 'include)
            (setq type "file"
                  ref (buffer-substring-no-properties middle end))
          (progn

            ;;.par obtain the link, both parts: href and description
            (when
                (re-search-forward "\\[?\\[?\\([^][]*?\\)\\]\\(\\[\\([^][]*?\\)\\]\\)?\\]" (+ end 2) t)
              ;;.del (re-search-forward "\\[\\[\\([^][]*?\\)\\]\\(\\[\\([^][]*?\\)\\]\\)?\\]" (+ end 2) t)
              (setq href (match-string-no-properties 1))
              (setq desc (match-string-no-properties 3))
              ;;.del (if href (message "href='%s'" href)) ;;.warning debug only
              ;;.del (if desc (message "desc='%s'" desc)) ;;.war debug only
              )

            ;;.par obtain the link type
            (when (and href
                       (string-match "\\(file:\\([^,]+\\), *\\)?\\([^:]+\\):\\(.*\\)" href))
              (setq file (match-string-no-properties 2 href))
              ;;.del (if file (message "file='%s'" file)) ;;.warning debug only
              (setq type (match-string-no-properties 3 href))
              ;;.del (if type (message "type='%s'" type)) ;;.warning debug only
              (setq ref  (match-string-no-properties 4 href))
              ;;.del (if ref (message "ref='%s'" ref)) ;;.warning debug only
              )
              (if (= (length ref) 0) (setq ref desc)))))
    `(,type ,ref ,href ,file ,marker)))

;;.sss.fn 8.2.2.2. qwe-link-follow--anchor

(defun qwe-link-follow--anchor (type marker anchor &optional id file)
  (qwe-link-follow--file file)
  (if (or (let ((pos nil))
            (save-excursion
              (goto-char (point-min))
              (setq pos (re-search-forward
                         (regexp-quote (concat "{a/" anchor "}")) nil t)))
            (if pos (goto-char pos)))
          (qwe-link-follow--annotation type anchor id))
      (qwe-link-history-insert-marker marker)))

;;.sss.fn 8.2.2.3. qwe-link-follow--compile

(defun qwe-link-follow--compile (marker command)
  (qwe-link-history-insert-marker marker)
  (compile command))

;;.sss.fn 8.2.2.4. qwe-link-follow--elisp

(defun qwe-link-follow--elisp (marker expression &optional file)
  (qwe-link-follow--file file)
  (eval-expression (read expression)))

;;.sss.fn 8.2.2.5. qwe-link-follow--file

(defun qwe-link-follow--file (file &optional file-only)
  (if (stringp file)
      (if (file-exists-p file)
          (progn
            (find-file file)
            ;;.wip++ Skiping headers, first try
            (when (and file-only (qwe-cfg-get-value "header.skip"))
              (goto-char (point-min))
              (re-search-forward "\\.cfg\\.header\\.skip")
              (forward-line (1+ scroll-margin))
              (recenter 0))
            ;;.wip-- end
            )
        (error "No such file '%s'" file))))

;;.sss 8.2.2.6. qwe-link-follow--image
;; This function is called when the user clicks an image. It is also used to
;; show or hide all images via a menu option. In the first case the parameter
;; {t/state} is not used. In the second case {t/state} must be {t/0} or {t/1} to indicate if
;; images must be shown or hidden.

(defun qwe-link-follow--image (begin end file &optional state)
  (unless (qwe-doc-image-autoload qwe-doc)
    (let ((buffer-modified (buffer-modified-p))
          (s (if (integerp state)
                 (= state 1)
               (not (get-text-property (1- end) 'show)))))
      (set-text-properties begin end `(show ,s))
      (font-lock-fontify-region (1- end) end) ;;.tip The image will be created by font-lock
      (set-buffer-modified-p buffer-modified))))

;;.sss.fn 8.2.2.7. qwe-link-follow--info

(defun qwe-link-follow--info (marker node)
  (qwe-link-history-insert-marker marker)
  (info node))

;;.sss.fn 8.2.2.8. qwe-link-follow--man

(defun qwe-link-follow--man (marker page)
  (qwe-link-history-insert-marker marker)
  (woman page))

;;.sss.fn 8.2.2.9. qwe-link-follow--annotation

(defun qwe-link-follow--annotation (type text &optional id)
  (let ((regexp (if (not (null text))
                    (concat "[ \\t]*" text "[ \\t]*")
                  nil))
        (dest nil))
    (save-excursion
      (goto-char (point-min))
      (when (qwe-lang-elt-re-search type id regexp)
        (setq dest (point))))
    (if dest
        (goto-char dest)
      (error "%s '%s' not found" type text))))

;;.sss.fn 8.2.2.10. qwe-link-follow--qwe

;;.todo DEFINE API
;;
;; This type of link gives acces to an API that is common to ALL
;; implementations: complete this {t/API}. The API should be accessible through
;; links, configuration items, directives (maybe). It should contain methods
;; to modify and update the document contents ... etc. Extensions should be
;; able to add methods to the API.

(defun qwe-link-follow--qwe (marker expression &optional file)
  (when (fboundp (read (concat "qwe-" expression)))
    (qwe-link-follow--file file)
    (eval-expression (read (concat "(qwe-" expression ")")))))

;;.sss.fn 8.2.2.11. qwe-link-follow--regexp

(defun qwe-link-follow--regexp (type name regexp)
  (let ((dest nil))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
        (setq dest (point))))
    (if dest
        (goto-char dest)
      (error "%s '%s' not found" type name))))

;;.sss.fn 8.2.2.12. qwe-link-follow--section

(defun qwe-link-follow--section (marker section-str section-name &optional file appendix)
  (qwe-link-follow--file file)
  (let ((regexp nil)
        (delimiter (if (> (string-width (qwe-doc-delimiter qwe-doc)) 0)
                       (qwe-doc-delimiter qwe-doc) "^")) ;;.tip Support for  (qwe-doc-delimiter qwe-doc)=""
        (section-regexp (concat "\\(" section-str "\\|"
                                (qwe-section--get-string-from-str section-str) "\\)")))
    (when appendix
      (goto-char (point-min))
      (unless (re-search-forward (concat delimiter "\\.\\#appendix$") nil t)
        (error "No appendixes found")))
    (setq regexp (concat delimiter "\\." section-regexp ".*" (regexp-quote section-name) "$"))
    (if (qwe-link-follow--regexp "Section" section-name regexp)
        (qwe-link-history-insert-marker marker))))

;;.sss.fn 8.2.2.13. qwe-link-follow--shell

(defun qwe-link-follow--shell (marker command)
  (qwe-link-history-insert-marker marker)
  (shell-command command))

;;.sss.fn 8.2.2.14. qwe-link-follow--symbol

(defun qwe-link-follow--symbol (marker name-and-mode)
  (qwe-link-history-insert-marker marker)
  (string-match "\\(.+?\\),\\(.+\\)" name-and-mode)
  (let ((name (match-string-no-properties 1 name-and-mode))
        (mode (match-string-no-properties 2 name-and-mode)))
  (info-lookup-symbol name (read mode))))

;;.sss.fn 8.2.2.15. qwe-link-follow--url

(defun qwe-link-follow--url (marker type url &optional file)
  (let ((str (format "%s:%s" type url)))
    (qwe-link-history-insert-marker marker)
    (browse-url str)))


;;.sec 8.3. Colophon

(provide 'qwe-link)

;;.cmt _____________________________________________________________________________
;;.cmt
;;.cfg.footer
;;.cfg.mode Local Variables:
;;.cfg.mode qwe-delimiter-tag: ";"
;;.cfg.mode mode:emacs-lisp
;;.cfg.mode mode: qwe
;;.cfg.mode End:
;;.bbx qwe-link.el ends here
