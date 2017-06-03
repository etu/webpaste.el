;;; test-webpaste-paste-text.el --- Tests for webpaste
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Paste text to provider"
 :var (webpaste/tested-providers)

 (before-each
  ;; Override which fake providers exists
  (spy-on 'webpaste/get-provider-priority
          :and-return-value
          '("provider1" "provider2"))

  ;; Don't allow it to try to paste, just fake it
  (spy-on 'webpaste-paste-text-to-provider)

  ;; And let tested list be resetted for each test
  (setq webpaste/tested-providers nil))


 (it
  "can paste text to provider and try second provider if called again"

  ;; Let's paste to first provider
  (webpaste-paste-text "my test text")

  ;; And check it was to first provider
  (expect 'webpaste-paste-text-to-provider
          :to-have-been-called-with
          "my test text"
          "provider1")

  ;; Let's paste to second provider
  (webpaste-paste-text "my test text")

  ;; And check it was to second provider
  (expect 'webpaste-paste-text-to-provider
          :to-have-been-called-with
          "my test text"
          "provider2")

  ;; Check that the tested list is empty so another run would restart
  (expect webpaste/tested-providers :to-equal nil)))


(describe
 "Run provider lambda to paste text"

 (before-each
  (let ((wp-lambda (lambda (text) text)))
    (setq webpaste-providers-alist `(("provider1" ,wp-lambda)
                                     ("provider2" ,wp-lambda)))))

 (it
  "can get the lambda for the specified provider and run it"

  (expect (webpaste-paste-text-to-provider "my test text" "provider2")
          :to-equal
          "my test text")))


;;; test-webpaste-paste-text.el ends here
