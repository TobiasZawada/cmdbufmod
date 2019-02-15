;;; cmdbufmod.el --- Run command on temporarily modified buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  U-ESI-INTERNAL\TOZ

;; Author: Tobias Zawada
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See doc-string of `cmdbufmod'.

;;; Code:

(defun cmdbufmod-prepare (bufmod-list &optional start bound)
  "Prepare buffer for `cmdbufmod' with modifications BUFMOD-LIST.
See `cmdbufmod' for the format of BUFMOD-LIST.
If START is a buffer position the search for the regular expressions
in BUFMOD-LIST starts there.  Otherwise it starts at `point-min'.
Optional BOUND limits the search.
Return the list of original text sections.
Each text section is a cons of an insertion marker and the old text
that needs to be restored there."
  (unless start (setq start (point-min)))
  (let (original-list)
    (save-excursion
      (dolist (bufmod bufmod-list)
    (let ((place (car bufmod))
          (newtext (cdr bufmod)))
      (goto-char start)
      (while (if (functionp place)
               (funcall place bound)
              (re-search-forward place bound t))
        (setq original-list
          (cons (cons (set-marker (make-marker) (match-beginning 0))
                  (match-string 0))
            original-list))
        (replace-match (propertize (if (functionp newtext)
                       (funcall newtext)
                     newtext)
                       'cmdbufmod t))))))
    original-list))

(defun cmdbufmod-cleanup (original-list)
  "Restore original text sections from ORIGINAL-LIST.
See the return value of `cmdbufmod-prepare' for the structure of ORIGINAL-LIST."
  (cl-loop for interval being the intervals property 'cmdbufmod
       if (get-text-property (car interval) 'cmdbufmod)
       do (delete-region (car interval) (cdr interval)))
  (cl-loop for original in original-list do
       (goto-char (car original))
       (insert (cdr original))))

;;;###autoload
(defmacro cmdbufmod (bufmod-list fun &rest args)
  "After applying BUFMOD-LIST to current buffer run FUN with ARGS like `apply'.
BUFMOD is a list of buffer modifications.
Each buffer modification is a cons \(PLACE . NEWTEXT).
PLACE can be a regular expression or a function.
If PLACE is a function it should search for the next place to be replaced
starting at point.  It gets the search bound as an argument
and should set `match-data' like `re-search-forward',
and return non-nil if a match is found.
If PLACE is a regular expression it is treated like the function
\(lambda () (re-search-forward PLACE nil t))

NEWTEXT can be a replacement string or a function.
A function should return the string for `replace-match'."
  `(let (original-list)
     (unwind-protect
	 (progn
	   (setq original-list (cmdbufmod-prepare ,bufmod-list))
	   (apply ,fun ,@args))
       (cmdbufmod-cleanup original-list))))

(provide 'cmdbufmod)
;;; cmdbufmod.el ends here
