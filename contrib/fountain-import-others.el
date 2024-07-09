;;; fountain-import-others.el --- Import other screenplay formats into fountain-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Matthew Polk

;; Author: Matthew Polk
;; Keywords: wp, lisp, extensions, files

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
;; Consider making the following available:
;; Adobe Story ".astx"
;; Celtx ".celtx"
;; Final Draft "fdx", but not the binary fdr format.
;; .txt? Maybe not since fountain already is plain text.
;; Highland ".highland"
;; .html? Maybe.
;; .rtf? Maybe.
;; .pdf? Most likely not.
;; Scrienver ".scriv"
;; FadeIn Pro ".fadein"
;; OpenScreenplay ".xml" files


;; Imports:
(require 'xml)
(require 'dom)

;;; Code:


;; FADE IN FDX
(defun xml-tree ()
  "Parse an XML tree"
  (interactive) ;;Make file selction later
  (setq buff (buffer-name))
  (setq fdx (with-temp-buffer
	;;Lets take the contents we need only and store it into a variable
	(insert-file-contents-literally "~/Downloads/Big-Fish.fdx")
	(goto-char (point-min))
	(kill-line 2)
	(search-forward "</Content>")
	(delete-region (point)(point-max))))
  (message "%s" fdx))


(provide 'fountain-import-others)
;;; fountain-import-others.el ends here
