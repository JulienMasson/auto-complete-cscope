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

(require 'xcscope)

(defface ac-cscope-candidate-face
  '((t (:inherit ac-candidate-face :foreground "coral3")))
  "Face for cscope candidate"
  :group 'auto-complete)

(defface ac-cscope-selection-face
  '((t (:inherit ac-selection-face :background "coral3")))
  "Face for the cscope selected candidate."
  :group 'auto-complete)


(defvar cscope-candidates (list "get_battery_capacity" "get_battery_level" "get_device_policy" "get_device_state" ))

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
  (let (line file function-name line-number)
    (save-excursion
      ;; Get the output thus far ...
      (if cscope-process-output
	  (setq cscope-process-output (concat cscope-process-output
					      output))
	(setq cscope-process-output output))
      ;; Slice and dice it into lines.
      ;; While there are whole lines left ...
      (while (and cscope-process-output
		  (string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)"
				cscope-process-output))
	(setq file				nil
	      glimpse-stripped-directory	nil
	      )
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
	      (let (str)
		(setq line (substring line (match-beginning 4)
				      (match-end 4)))

		;; filter line
		(ac-cscope-line-filter line)

		(setq cscope-last-file file)
		))
	  )))))

(defun ac-cscope-process-sentinel (process event)
  "Sentinel for when the cscope process dies."
  (delete-process process)
  (setq cscope-process nil))

(defun ac-cscope-candidate ()
  "Pop a database entry from cscope-search-list and do a search there."
  (let ( options cscope-directory database-file outbuf base-database-file-name)
    (save-excursion
      (setq options (list "-L" "-1" (concat ac-prefix ".*")))
      (setq cscope-directory cscope-initial-directory)
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
      (setq cscope-output-start (point))
      (setq default-directory cscope-directory)

      (setq cscope-process-output nil
	    cscope-last-file nil
	    )

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
      cscope-candidates
      )))

(ac-define-source cscope
  '((candidates . ac-cscope-candidate)
    (candidate-face . ac-cscope-candidate-face)
    (selection-face . ac-cscope-selection-face)
    (requires . 3)
    (symbol . "s")))


(provide 'auto-complete-cscope)
