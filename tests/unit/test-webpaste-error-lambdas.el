;;; test-webpaste-error-lambdas.el --- Tests for error lambdas
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Create lambdas to use on failures, "

 (before-each
  (spy-on 'webpaste--paste-text)
  (spy-on 'message))

 (it
  "with fallback"

  (let ((error-lambda (webpaste--providers-error-lambda :text "my text")))
    (funcall error-lambda :error-thrown "my error")

    (expect 'message
            :to-have-been-called-with
            "Got error: %S"
            "my error")

    (expect 'webpaste--paste-text
            :to-have-been-called-with
            "my text")))

 (it
  "without fallback"
  (let ((error-lambda (webpaste--providers-error-lambda-no-failover :text "my text")))
    (funcall error-lambda :error-thrown "my error")

    (expect 'message
            :to-have-been-called-with
            "Got error: %S"
            "my error")

    (expect 'webpaste--paste-text
            :not :to-have-been-called))))


;;; test-webpaste-error-lambdas.el ends here
