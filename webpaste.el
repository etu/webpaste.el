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


(defcustom webpaste-provider-priority ()
  "Define provider priority of which providers to try in which order.
This variable should be a list of strings and if it isn't defined it will
default to all providers in order defined in ‘webpaste-providers’ list."
  :group 'webpaste)


(defvar webpaste-tested-providers ()
  "Variable for storing which providers to try in which order while running.
This list will be re-populated each run based on ‘webpaste-provider-priority’ or
if that variable is nil, it will use the list of names from ‘webpaste-providers’
each run.")


;;; Define providers
(defcustom webpaste-providers-alist
  '(("ix.io" .
     (lambda (text)
       "Paste TEXT to http://ix.io/."

       (let ((post-data '()))
         ;; Construct post data
         (add-to-list 'post-data (cons "f:1" text))

         ;; Use request.el to do request to ix.io to submit data
         (request "http://ix.io/"
                  :type "POST"
                  :data post-data
                  :parser 'buffer-string
                  :success (function* (lambda (&key data &allow-other-keys)
                                        (when data
                                          (webpaste-return-url data))))))
       nil))
    ("dpaste.com" .
     (lambda (text)
       "Paste TEXT to http://dpaste.com/."

       ;; Prepare post fields
       (let ((post-data '(("syntax" . "text")
                          ("title" . "")
                          ("poster" . "")
                          ("expiry_days" . "1"))))

         ;; Add TEXT as content
         (add-to-list 'post-data (cons "content" text))

         ;; Use request.el to do request to dpaste.com to submit data
         (request "http://dpaste.com/api/v2/"
                  :type "POST"
                  :data post-data
                  :parser 'buffer-string
                  :success
                  (function* (lambda (&key response &allow-other-keys)
                               (webpaste-return-url
                                (request-response-header response "Location"))))))
       nil)))
  "Define all webpaste.el providers.
Consists of provider name and lambda function to do the actuall call to the
provider.  The lamda should call ‘webpaste-return-url’ with resulting url to
return it to the user."
  :group 'webpaste)


;; Function we use to return the RETURNED-URL from the service
(defun webpaste-return-url (returned-url)
  "Return RETURNED-URL to user from the result of the paste service."

  ;; Add RETURNED-URL to killring for easy pasting
  (kill-new returned-url)

  ;; Notify user
  (message (concat "Added " returned-url " to kill ring.")))


;; Function to do pasting
(defun webpaste-paste-text (text)
  "Paste TEXT to some paste service."

  ;; Populate webpaste-provider-priority if needed
  (if (eq webpaste-provider-priority nil)
      (let ((providers-alist webpaste-providers-alist)
            (provider-names))
        ;; Loop local provider list
        (while providers-alist
          ;; Add name to list of names
          (add-to-list 'provider-names (caar providers-alist))

          ;; Depopulate list
          (setq providers-alist (cdr providers-alist)))

        ;; Set names list
        (setq-default webpaste-provider-priority (reverse provider-names))))

  ;; Populate tested providers for this request if needed
  (if (eq webpaste-tested-providers nil)
      (setq webpaste-tested-providers webpaste-provider-priority))

  ;; Get name of provider at the top of the list
  (let ((provider-name (car webpaste-tested-providers)))
    ;; Drop the name at the top of the list
    (setq webpaste-tested-providers (cdr webpaste-tested-providers))

    ;; Run pasting function
    (funcall (cdr (assoc provider-name webpaste-providers-alist)) text)))


;;;###autoload
(defun webpaste-paste-region ()
  "Paste selected region to some paste service."
  (interactive)

  (let ((text (buffer-substring (mark) (point))))
    (webpaste-paste-text text)))


;;;###autoload
(defun webpaste-paste-buffer ()
  "Paste current buffer to some paste service."
  (interactive)

  (save-mark-and-excursion
   (set-mark (point-min))               ; Set mark on point-min
   (goto-char (point-max))              ; Go to point-max
   (webpaste-paste-region)))            ; Paste region


(provide 'webpaste)

;;; webpaste.el ends here
