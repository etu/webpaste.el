;;; webpaste.el --- Paste to pastebin-like services  -*- lexical-binding: t; -*-

;; Copyright (c) 2016 Elis Axelsson

;; Author: Elis "etu" Axelsson
;; URL: https://github.com/etu/webpaste.el
;; Package-Version: 1.0.0
;; Version: 1.0.0
;; Keywords: convenience, comm, paste
;; Package-Requires: ((emacs "24.1") (request "0.2.0") (cl-lib "0.5"))

;;; Commentary:

;; This mode allows to paste whole buffers or parts of buffers to
;; pastebin-like services.  It supports more than one service and will
;; failover if one service fails.  More services can easily be added
;; over time and prefered services can easily be configured.

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
(require 'cl-lib)


(defgroup webpaste nil
  "Configuration options for webpaste.el where you can define paste providers,
provider priority for which order which provider should be tried when used."
  :group 'web)


(defcustom webpaste-provider-priority ()
  "Define provider priority of which providers to try in which order.
This variable should be a list of strings and if it isn't defined it will
default to all providers in order defined in ‘webpaste-providers’ list."
  :group 'webpaste
  :type '(repeat string))


(defvar webpaste-tested-providers ()
  "Variable for storing which providers to try in which order while running.
This list will be re-populated each run based on ‘webpaste-provider-priority’ or
if that variable is nil, it will use the list of names from ‘webpaste-providers’
each run.")


(cl-defun webpaste-provider (&key domain (type "POST") parser post-data post-field success)
  "Macro to create the lambda function for a provider.

This macro accepts the parameters :domain, :type, :parser, :post-data,
:post-field and :success.

Usage:
  (webpaste-provider
    [:keyword [option]]...)

:domain      URL that we should do the request to to paste data.
:type        HTTP Request type, defaults to POST.
:parser      Defines how request.el parses the result. Look up :parser for
             `request`.
:post-data   Default post fields sent to service. Defaults to nil.
:post-field  Name of the field to insert the code into.
:success     Callback sent to `request`, look up how to write these in the
             documentation for `request`"
  (lambda (text)
    "Paste TEXT to provider"

    ;; Local variable post-data
    (cl-pushnew (cons post-field text) post-data)

    ;; Do request
    (request domain
             :type type
             :data post-data
             :parser parser
             :success success
             :error
             (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)
                            (webpaste-paste-text text))))
    nil))


;;; Define providers
(defcustom webpaste-providers-alist
  (list (list "ix.io"
              (webpaste-provider
               :domain "http://ix.io/"
               :parser 'buffer-string
               :post-field "f:1"
               :success
               (cl-function (lambda (&key data &allow-other-keys)
                              (when data
                                (webpaste-return-url
                                 (replace-regexp-in-string "\n$" "" data)))))))

        (list "sprunge.us"
              (webpaste-provider
               :domain "http://sprunge.us/"
               :parser 'buffer-string
               :post-field "sprunge"
               :success
               (cl-function (lambda (&key data &allow-other-keys)
                              (when data
                                (webpaste-return-url
                                 (replace-regexp-in-string "\n$" "" data)))))))

        (list "dpaste.com"
              (webpaste-provider
               :domain "http://dpaste.com/api/v2/"
               :parser 'buffer-string
               :post-data '(("syntax" . "text")
                            ("title" . "")
                            ("poster" . "")
                            ("expiry_days" . 1))
               :post-field "content"
               :success
               (cl-function (lambda (&key response &allow-other-keys)
                              (webpaste-return-url
                               (request-response-header response "Location"))))))

        (list "dpaste.de"
              (webpaste-provider
               :domain "https://dpaste.de/api/"
               :parser 'buffer-string
               :post-data '(("lexer" . "text")
                            ("format" . "url")
                            ("expires" . 86400))
               :post-field "content"
               :success
               (cl-function (lambda (&key data &allow-other-keys)
                              (when data
                                (webpaste-return-url
                                 (replace-regexp-in-string "\n$" "" data))))))))

  "Define all webpaste.el providers.
Consists of provider name and lambda function to do the actuall call to the
provider.  The lamda should call ‘webpaste-return-url’ with resulting url to
return it to the user."
  :group 'webpaste
  :type  '(alist :key-type (string :tag "provider name")
                 :value-type (sexp :tag "webpaste-provider macro definition for the provider")))


;;;###autoload
(defun webpaste-return-url (returned-url)
  "Return RETURNED-URL to user from the result of the paste service."

  ;; Reset tested providers after successful paste
  (setq webpaste-tested-providers nil)

  ;; Add RETURNED-URL to killring for easy pasting
  (kill-new returned-url)

  ;; Notify user
  (message (concat "Added " returned-url " to kill ring.")))


;;;###autoload
(defun webpaste-paste-text (text)
  "Paste TEXT to some paste service.
If ‘webpaste-provider-priority’ isn't populated, it will populate it with the
default providers.

Then if ‘webpaste-tested-providers’ isn't populated it will be populated by
‘webpaste-provider-priority’.

Then it extracts the first element of ‘webpaste-tested-providers’ and drops
the first element from that list and gets the lambda for the provider and
runs the lambda to paste TEXT to the paste service.  The paste-service in turn
might call this function again with TEXT as param to retry if it failed.

When we run out of providers to try, it will restart since
‘webpaste-tested-providers’ will be empty and then populated again."

  ;; Populate webpaste-provider-priority if needed
  (if (eq webpaste-provider-priority nil)
      (let ((provider-names))
        ;; Loop provider list
        (dolist (provider webpaste-providers-alist)
          (cl-pushnew (car provider) provider-names))

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
    (funcall (cadr (assoc provider-name webpaste-providers-alist)) text)))


;;;###autoload
(defun webpaste-paste-region (point mark)
  "Paste selected region to some paste service.
Argument POINT Current point.
Argument MARK Current mark."
  (interactive "r")

  ;; Extract the buffer contents with buffer-substring and paste it
  (webpaste-paste-text (buffer-substring point mark)))


;;;###autoload
(defun webpaste-paste-buffer ()
  "Paste current buffer to some paste service."
  (interactive)

  ;; Extract the buffer contents with buffer-substring and paste it
  (webpaste-paste-text (buffer-substring (point-min) (point-max))))


(provide 'webpaste)

;;; webpaste.el ends here
