;;; fountain-parse.el --- Element parsing engine for Fountain Mode

;; Copyright (C) 2014 Paul Rankin

;; Author: Paul Rankin <hello@paulwrankin.com>
;; Keywords: wp, data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;; Funcation ==================================================================

(defun fountain-parse-element ()
  (cond
   ((fountain-metadata-p)
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (key (downcase (match-string-no-properties 2)))
          (value (match-string-no-properties 3)))
      (forward-line 1)
      (while (and (fountain-metadata-p)
                  (null (match-string 2)))
        (setq value (concat value (if value "\n") (match-string-no-properties 3))
              end (match-end 0))
        (forward-line 1))
      (list 'metadata
            (list :begin beg
                  :end end)
            (list key value))))
   ((fountain-section-heading-p)
    (let ((top (list 'section-heading
                     (list :beg (match-beginning 0)
                           :end (match-end 0))
                     (match-string-no-properties 3)))
          (level (funcall outline-level))
          (beg (point))
          (end (save-excursion
                 (outline-end-of-subtree)
                 (unless (eobp)
                   (forward-char 1))
                 (point))))
      (goto-char (plist-get (nth 1 top)
                            :end))
      (list 'section
            (list :begin beg
                  :end end
                  :level level)
            (list top)
            (fountain-parse-region (point) end))))
   ((fountain-scene-heading-p)
    (let ((top (list 'scene-heading
                     (list :beg (match-beginning 0)
                           :end (match-end 0))
                     (match-string-no-properties 3)))
          (beg (point))
          (end (save-excursion
                 (outline-end-of-subtree)
                 (unless (eobp)
                   (forward-char 1))
                 (point))))
      (goto-char (plist-get (nth 1 top)
                            :end))
      (list 'scene
            (list :begin beg
                  :end end
                  :num (ignore))
            (list top)
            (fountain-parse-region (point) end))))
   ((fountain-character-p)
    (let ((top (list 'character
                     (list :beg (match-beginning 0)
                           :end (match-end 0))
                     (match-string-no-properties 3)))
          (name (match-string-no-properties 4))
          (dual (cond ((stringp (match-string 5))
                       'right)
                      ((save-excursion
                         (fountain-forward-character 1 'dialog)
                         (and (fountain-character-p)
                              (stringp (match-string 5))))
                       'left)))
          (beg (point))
          (end (save-excursion
                 (if (re-search-forward "^[\s\t]*$" nil 'move)
                     (match-beginning 0)
                   (point)))))
      (goto-char (plist-get (nth 1 top)
                            :end))
      (list 'dialog
            (list :begin beg
                  :end end
                  :character name
                  :dual dual)
            (list top)
            (fountain-parse-region (point) end))))
   ((fountain-dialog-p)
    (list 'lines
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-paren-p)
    (list 'paren
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-trans-p)
    (list 'trans
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-center-p)
    (list 'center
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-synopsis-p)
    (list 'synopsis
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   ((fountain-note-p)
    (list 'note
          (list :begin (match-beginning 0)
                :end (match-end 0))
          (match-string-no-properties 3)))
   (t
    (let ((beg (car (fountain-get-block-bounds)))
          (end (cdr (fountain-get-block-bounds))))
      (forward-line 0)
      (list 'action
            (list :begin beg
                  :end end)
            (s-trim-right (buffer-substring-no-properties
                           (point) end)))))))

(defun fountain-parse-region (beg end)
  (let (list)
    (goto-char beg)
    (while (< (point) end)
      (while (looking-at "\n*\s*\n")
        (goto-char (match-end 0)))
      (if (< (point) end)
          (let ((element (fountain-parse-element)))
            (when element
              (push element list)
              (goto-char (plist-get (nth 1 element)
                                    :end))))))
    (reverse list)))

(provide 'fountain-parse)
;;; fountain-parse.el ends here
