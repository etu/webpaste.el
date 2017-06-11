;;; test-webpaste-get-provider-priority.el --- Tests for webpaste
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)

(describe
 "Get provider priority"
 (it
  "can autopopulate provider priority if it's not predefined"

  (let ((webpaste-provider-priority nil)
        (webpaste-providers-alist '(("provider1" . "lambda")
                                    ("provider2" . "lambda")
                                    ("provider3" . "lambda"))))

    ;; Do test
    (expect (webpaste--get-provider-priority)
            :to-equal
            '("provider1" "provider2" "provider3"))))


 (it
  "can fetch static population of provider priority"

  (let ((webpaste-provider-priority '("provider2" "provider1" "provider3")))

    ;; Do test
    (expect (webpaste--get-provider-priority)
            :to-equal
            '("provider2" "provider1" "provider3")))))

;;; test-webpaste-get-provider-priority.el ends here
