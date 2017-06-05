;;; test-webpaste-default-post-field-lambda.el --- Tests for webpaste
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)

(describe
 "Default post field lambda"
 (it
  "can add language and text as post data"

  (let ((post-lambda (webpaste-providers-default-post-field-lambda))
        (major-mode 'fundamental-mode)
        (webpaste-provider-lang-alists '(("https://example.com/" . ((fundamental-mode . "text"))))))
    (let ((post-data (funcall post-lambda
                              :text "my text"
                              :post-field "content"
                              :provider-uri "https://example.com/"
                              :post-lang-field-name "lang"
                              :post-data '(("duration" . "1 day")))))
      (expect post-data
              :to-equal
              '(("lang" . "text") ("content" . "my text") ("duration" . "1 day")))))))


;;; test-webpaste-default-post-field-lambda.el ends here
