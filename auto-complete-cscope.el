;;; auto-complete-cscope.el

;; Copyright (C) 2015 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Keywords: emacs status activity

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'tramp)
(require 'xcscope)

(defvar cscope-completion-activated t
  "t if cscope-completion is in used, nil otherwise.")

(defvar cscope-candidates (list ""))

(defvar default-cscope-mirror-path nil
    "set to a PATH where to find cscope mirror on local.")

(defface ac-cscope-candidate-face
  '((t (:inherit ac-candidate-face :foreground "DarkSlateGrey")))
  "Face for cscope candidate"
  :group 'auto-complete)

(defface ac-cscope-selection-face
  '((t (:inherit ac-selection-face)))
  "Face for the cscope selected candidate."
  :group 'auto-complete)

(defun ac-cscope-line-filter(line)
  (setq highlight-search-re (concat ac-prefix ".*"))
  (setq highlight-search-re
	(replace-regexp-in-string "\\([^\\\\]\\)\\." "\\1[a-zA-Z0-9_]"
				  (concat "\\b" highlight-search-re "\\b")))
  (let* ((case-fold-search nil)
	 (start (string-match
		 highlight-search-re
		 line)))
    (when start
      (setq cscope-candidates (append
			       (list (substring line start (match-end 0)))
			       cscope-candidates)))))

(defun ac-cscope-process-filter (process output)
  "Accept cscope process output and reformat it for human readability.
Magic text properties are added to allow the user to select lines
using the mouse."
  (let (line)
      (if cscope-process-output
	  (setq cscope-process-output (concat cscope-process-output
					      output))
	(setq cscope-process-output output))
      ;; Slice and dice it into lines.
      ;; While there are whole lines left ...
      (while (and cscope-process-output
		  (string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)"
				cscope-process-output))
	;; Get a line
	(setq line (substring cscope-process-output
			      (match-beginning 1) (match-end 1)))
	(setq cscope-process-output (substring cscope-process-output
					       (match-beginning 2)
					       (match-end 2)))
	(if (= (length cscope-process-output) 0)
	    (setq cscope-process-output nil))

	;; This should always match.
	(if (string-match
	     "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)\n"
	     line)
	    (progn
	      (setq line (substring line (match-beginning 4)
				      (match-end 4)))
		;; filter line
		(ac-cscope-line-filter line))))))

(defun ac-cscope-process-sentinel (process event)
  "Sentinel for when the cscope process dies."
  (delete-process process)
  (setq cscope-process nil))

(defun ac-cscope-candidate ()
  "search completions candidates in cscope database"
  (let ( options mirror-directory current-directory cscope-directory database-file outbuf base-database-file-name)
      (if (and cscope-initial-directory cscope-completion-activated)
	  (progn

	    ;; if cscope-initial-directory in on remote machine
	    ;; and user use default-cscope-mirror-path
	    (if (and (tramp-tramp-file-p cscope-initial-directory)
	    	     default-cscope-mirror-path)
		(progn
		  (setq mirror-directory (concat default-cscope-mirror-path
						 (car (last (split-string cscope-initial-directory "/" t)))
						 "/"))
		  (if (file-exists-p mirror-directory)
		      (setq cscope-directory mirror-directory)
		    (setq cscope-directory cscope-initial-directory)))
	      (setq cscope-directory cscope-initial-directory))

	    ;; common options
	    (setq tramp-verbose 0)
	    (setq options (list "-L" "-1" (concat ac-prefix ".*")))
	    (setq base-database-file-name "cscope.out")
	    (setq database-file (concat cscope-directory base-database-file-name)
		  cscope-searched-dirs (cons cscope-directory
					     cscope-searched-dirs))

	    ;; The database file and the directory containing the database file
	    ;; must both be writable.
	    (if (or (not (file-writable-p database-file))
		    (not (file-writable-p (file-name-directory database-file)))
		    cscope-option-do-not-update-database)
		(setq options (cons "-d" options)))

	    ;; Add the correct database file to search
	    (setq options (cons base-database-file-name options))
	    (setq options (cons "-f" options))
	    (setq current-directory default-directory)
	    (setq default-directory cscope-directory)
	    (setq cscope-process-output nil)

	    (setq cscope-process
		  ;; Communicate with a pipe. Slightly more efficient than
		  ;; a TTY
		  (let ((process-connection-type nil))
		    (apply cscope-start-file-process "cscope" outbuf
			   cscope-program
			   (append (cscope-construct-custom-options-list) options))))

	    (set-process-filter cscope-process 'ac-cscope-process-filter)
	    (set-process-sentinel cscope-process 'ac-cscope-process-sentinel)
	    (process-kill-without-query cscope-process)
	    (setq default-directory current-directory)
	    (setq tramp-verbose 3)))
      cscope-candidates
      ))

(ac-define-source cscope
  '((candidates . ac-cscope-candidate)
    (candidate-face . ac-cscope-candidate-face)
    (selection-face . ac-cscope-selection-face)
    (requires . 3)
    (symbol . "s")))

(defun toggle-cscope-completion ()
  (interactive)
  (setq cscope-completion-activated (not cscope-completion-activated)))

(defun update-cscope-mirror-database ()
  (interactive)
  (if (and (tramp-tramp-file-p cscope-initial-directory)
	   default-cscope-mirror-path)
      (progn
	(cscope-index-files cscope-initial-directory)
	(copy-directory
	 cscope-initial-directory
	 (concat default-cscope-mirror-path
		 (file-name-nondirectory (directory-file-name cscope-initial-directory)))))))

(provide 'auto-complete-cscope)
