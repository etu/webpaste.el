;;; webpaste.el --- Paste to pastebin-like services  -*- lexical-binding: t; -*-

;; Copyright (c) 2016-2017 Elis Axelsson

;; Author: Elis "etu" Axelsson
;; URL: https://github.com/etu/webpaste.el
;; Package-Version: 1.3.0
;; Version: 1.3.0
;; Keywords: convenience, comm, paste
;; Package-Requires: ((emacs "24.4") (request "0.2.0") (cl-lib "0.5") (json "1.4"))

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
(require 'json)



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


(defcustom webpaste/default-lang-alist
  '((css-mode . "css")
    (fundamental-mode . "text")
    (html-mode . "html")
    (java-mode . "java")
    (js-mode . "js")
    (go-mode . "go")
    (php-mode . "php")
    (python-mode . "python")
    (yaml-mode . "yaml"))
  "Alist that maps `major-mode' names to language names."
  :type '(alist :key-type symbol :value-type string)
  :group 'webpaste)

(defcustom webpaste/paste-confirmation nil
  "Prompt for a yes/no confirmation before attempting to paste a region or
buffer"
  :group 'webpaste)


(defcustom webpaste/paste-confirmation nil
  "Prompt for a yes/no confirmation before attempting to paste a region or buffer."
  :group 'webpaste)


(defvar webpaste/tested-providers ()
  "Variable for storing which providers to try in which order while running.
This list will be re-populated each run based on ‘webpaste-provider-priority’ or
if that variable is nil, it will use the list of names from ‘webpaste-providers’
each run.")



;;; Predefined error lambda for providers
(defvar webpaste/providers-error-lambda
  (cl-function (lambda (&key error-thrown &allow-other-keys)
                 (message "Got error: %S" error-thrown)
                 (webpaste-paste-text text)))
  "Predefined error callback for providers that always does failover.")


(defvar webpaste/providers-error-lambda-no-failover
  (cl-function (lambda (&key error-thrown &allow-other-keys)
                 (message "Got error: %S" error-thrown)))
  "Predefined error callback for providers that shouldn't do failover.")


;;; Predefined success lambdas for providers
(defvar webpaste/providers-success-location-header
  (cl-function (lambda (&key response &allow-other-keys)
                 (when response
                   (webpaste-return-url
                    (request-response-header response "Location")))))
  "Predefined success callback for providers returning a Location header.")


(defvar webpaste/providers-success-returned-string
  (cl-function (lambda (&key data &allow-other-keys)
                 (when data
                   (setq data (replace-regexp-in-string "\n$" "" data))
                   (setq data (replace-regexp-in-string "\"" "" data))

                   (webpaste-return-url data))))
  "Predefined success callback for providers returning a string with URL.")


(defvar webpaste/providers-default-post-field-lambda
  (cl-function (lambda (&key text
                        post-field
                        (post-lang-field-name nil)
                        (lang-overrides nil)
                        (post-data '()))
                 (cl-pushnew (cons post-field text) post-data)

                 (when post-lang-field-name
                   ;; Get language name based on major-mode
                   (let ((language-name (cdr (assoc major-mode (webpaste/get-lang-alist-with-overrides lang-overrides)))))
                     ;; If not set correctly, get the fundamental-mode one which should be plaintext
                     (unless language-name
                       (setq language-name (cdr (assoc 'fundamental-mode (webpaste/get-lang-alist-with-overrides lang-overrides)))))

                     ;; Append language to the post-data
                     (cl-pushnew (cons post-lang-field-name language-name) post-data)))

                 post-data))
  "Predefined lambda for building post fields.")



(cl-defun webpaste-provider (&key uri
                                  post-field
                                  success-lambda
                                  (type "POST")
                                  (post-data '())
                                  (post-lang-field-name nil)
                                  (parser 'buffer-string)
                                  (lang-overrides '())
                                  (error-lambda webpaste/providers-error-lambda)
                                  (post-field-lambda webpaste/providers-default-post-field-lambda)
                                  (sync nil))
  "Function to create the lambda function for a provider.

Usage:
  (webpaste-provider
    [:keyword [option]]...)

Required params:
:uri               URI that we should do the request to to paste data.

:post-field        Name of the field to insert the code into.

:success-lambda    Callback sent to `request', look up how to write these in the
                   documentation for `request'.  Two good examples are
                   `webpaste/providers-success-location-header' and
                   `webpaste/providers-success-returned-string' as well as the
                   custom one used for the gist.github.com provider.

Optional params:
:type              HTTP Request type, defaults to POST.

:post-data         Default post fields sent to service. Defaults to nil.

:post-lang-field-name   Fieldname for defining which language your paste should
                        use to the provider.

:lang-overrides    Alist defining overides for languages for this provider. If
                   a mode is set to nil, it will use fundamental-mode's value as
                   fallback. Fundamental-mode's value can also be overridden.

:parser            Defines how request.el parses the result. Look up :parser for
                   `request'. This defaults to 'buffer-string.

:error-lambda      Callback sent to `request', look up how to write these in the
                   documentation for `request'.  The default value for this is
                   `webpaste/providers-error-lambda', but there's also
                   `webpaste/providers-error-lambda-no-failover' available if
                   you need a provider that isn't allowed to failover.

:post-field-lambda Function that builds and returns the post data that should be
                   sent to the provider.  It should accept named parameters by
                   the names TEXT, POST-FIELD and POST-DATA.  POST-DATA should
                   default to `nil' or empty list.  It also takes the option
                   LANG-OVERRIDES which is a list that enables overiding of
                   `webpaste/default-lang-alist'.

                   TEXT contains the data that should be sent.
                   POST-FIELD cointains the name of the field to be sent.
                   POST-DATA contains predefined fields that the provider needs.

:sync              Set to t to wait until request is done.  Defaults to nil.
                   This should only be used for debugging purposes."
  (lambda (text)
    "Paste TEXT to provider"

    (prog1 nil
      ;; Do request
      (request uri
               :type type
               :data (funcall post-field-lambda
                              :text text
                              :post-field post-field
                              :post-lang-field-name post-lang-field-name
                              :lang-overrides lang-overrides
                              :post-data post-data)
               :parser parser
               :success success-lambda
               :sync sync
               :error error-lambda))))



;;; Define providers
(defcustom webpaste-providers-alist
  `(("ptpb.pw"
     ,(webpaste-provider
       :uri "https://ptpb.pw/"
       :post-field "c"
       :success-lambda webpaste/providers-success-location-header))

    ("ix.io"
     ,(webpaste-provider
       :uri "http://ix.io/"
       :post-field "f:1"
       :success-lambda webpaste/providers-success-returned-string))

    ("sprunge.us"
     ,(webpaste-provider
       :uri "http://sprunge.us/"
       :post-field "sprunge"
       :success-lambda webpaste/providers-success-returned-string))

    ("dpaste.com"
     ,(webpaste-provider
       :uri "http://dpaste.com/api/v2/"
       :post-data '(("title" . "")
                    ("poster" . "")
                    ("expiry_days" . 1))
       :post-field "content"
       :post-lang-field-name "syntax"
       :lang-overrides '((emacs-lisp-mode . "clojure"))
       :success-lambda webpaste/providers-success-location-header))

    ("dpaste.de"
     ,(webpaste-provider
       :uri "https://dpaste.de/api/"
       :post-data '(("expires" . 86400))
       :post-field "content"
       :post-lang-field-name "lexer"
       :lang-overrides '((emacs-lisp-mode . "clojure"))
       :success-lambda webpaste/providers-success-returned-string))

    ("gist.github.com"
     ,(webpaste-provider
       :uri "https://api.github.com/gists"
       :post-field nil
       :post-field-lambda (cl-function (lambda (&key text &allow-other-keys)
                                         (let ((filename (or (file-name-nondirectory (buffer-file-name)) "file.txt")))
                                           (json-encode `(("description" . "Pasted from Emacs with webpaste.el")
                                                          ("public" . "false")
                                                          ("files" .
                                                           ((,filename .
                                                             (("content" . ,text))))))))))
       :success-lambda (cl-function (lambda (&key data &allow-other-keys)
                                      (when data
                                        (webpaste-return-url
                                         (cdr (assoc 'html_url (json-read-from-string data))))))))))

  "Define all webpaste.el providers.
Consists of provider name and lambda function to do the actuall call to the
provider.  The lamda should call ‘webpaste-return-url’ with resulting url to
return it to the user."
  :group 'webpaste
  :type  '(alist :key-type (string :tag "provider name")
                 :value-type (sexp :tag "webpaste-provider function definition for the provider")))



(defun webpaste/get-lang-alist-with-overrides (overrides)
  "Fetches lang-alist with OVERRIDES applied."

  ;; Copy original list to temporary list
  (let ((lang-alist webpaste/default-lang-alist))
    ;; Go through list of overrides and append them to the temporary list
    (dolist (override-element overrides)
      (cl-pushnew override-element lang-alist))

    ;; Return temporary list
    lang-alist))



(defun webpaste/get-provider-priority ()
  "Return provider priority."

  ;; Populate webpaste-provider-priority if needed
  (unless webpaste-provider-priority
    (let ((provider-names))
      ;; Loop provider list
      (dolist (provider webpaste-providers-alist)
        (cl-pushnew (car provider) provider-names))

      ;; Set names list
      (setq-default webpaste-provider-priority (reverse provider-names))))

  webpaste-provider-priority)



;;;###autoload
(defun webpaste-return-url (returned-url)
  "Return RETURNED-URL to user from the result of the paste service."

  ;; Reset tested providers after successful paste
  (setq webpaste/tested-providers nil)

  ;; Add RETURNED-URL to killring for easy pasting
  (kill-new returned-url)

  ;; Notify user
  (message "Added %S to kill ring." returned-url))



;;;###autoload
(defun webpaste-paste-text-to-provider (text provider-name)
  "Paste TEXT to specific PROVIDER-NAME.
This function sends a paste to a spacific provider.  This function is created to
make `webpaste-paste-text' do less magic things all at once."
  (funcall (cadr (assoc provider-name webpaste-providers-alist)) text))


;;;###autoload
(defun webpaste-paste-text (text)
  "Paste TEXT to some paste service.
If ‘webpaste-provider-priority’ isn't populated, it will populate it with the
default providers.

Then if ‘webpaste/tested-providers’ isn't populated it will be populated by
‘webpaste-provider-priority’.

Then it extracts the first element of ‘webpaste/tested-providers’ and drops
the first element from that list and gets the lambda for the provider and
runs the lambda to paste TEXT to the paste service.  The paste-service in turn
might call this function again with TEXT as param to retry if it failed.

When we run out of providers to try, it will restart since
‘webpaste/tested-providers’ will be empty and then populated again."

  ;; Populate tested providers for this request if needed
  (unless webpaste/tested-providers
    (setq webpaste/tested-providers (webpaste/get-provider-priority)))

  ;; Get name of provider at the top of the list
  (let ((provider-name (car webpaste/tested-providers)))
    ;; Drop the name at the top of the list
    (setq webpaste/tested-providers (cdr webpaste/tested-providers))

    ;; Run pasting function
    (webpaste-paste-text-to-provider text provider-name)))


;;;###autoload
(defun webpaste-paste-region (point mark)
  "Paste selected region to some paste service.
Argument POINT Current point.
Argument MARK Current mark."
  (interactive "r")

  ;; unless we wanted a paste confirmation and declined
  (unless (and webpaste/paste-confirmation
               (not (yes-or-no-p "paste entire region?")))
    ;; Extract the buffer contents with buffer-substring and paste it
    (webpaste-paste-text (buffer-substring point mark))))


;;;###autoload
(defun webpaste-paste-buffer ()
  "Paste current buffer to some paste service."
  (interactive)

  ;; unless we wanted a paste confirmation and declined
  (unless (and webpaste/paste-confirmation
               (not (yes-or-no-p "paste entire buffer?")))
    ;; Extract the buffer contents with buffer-substring and paste it
    (webpaste-paste-text (buffer-substring (point-min) (point-max)))))



(provide 'webpaste)

;;; webpaste.el ends here
