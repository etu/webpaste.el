;;; webpaste.el --- Paste to pastebin-like services

;; Copyright (c) 2016 Elis Axelsson

;; Author: Elis "etu" Axelsson
;; URL: https://github.com/etu/webpaste.el
;; Package-Version: 0
;; Version: 0.0.1
;; Keywords: convenience, webpaste
;; Package-Requires: ((emacs "25.1") (request "0.2.0"))

;;; Commentary:

;; This mode will allow the user to paste parts or whole buffers
;; to pastebin-like services.

;;; License:

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:
(require 'request)



;; The function to paste a region
(defun webpaste-paste-region ()
  "Paste selected region to some paste service."
  (interactive)

  (let ((text (buffer-substring (mark) (point))))
    (message text)))


;; The function to paste a buffer
(defun webpaste-paste-buffer ()
  "Paste current buffer to some paste service."
  (interactive)

  (save-mark-and-excursion
   (set-mark (point-min))               ; Set mark on point-min
   (goto-char (point-max))              ; Go to point-max
   (webpaste-paste-region)))            ; Paste region

(provide 'webpaste)

;;; webpaste.el ends here
