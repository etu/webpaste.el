;;; webpaste --- Tests for webpaste
;;; Commentary:
;;; Code:
(require 'ert)
(require 'webpaste)


(ert-deftest callback-from-working-provider ()
  "This test just sends a message to a good provider that just works."

  ;; Temporal storage for result
  (let ((returned-result nil))
    ;; Reset all webpaste variables
    (setq-default webpaste-tested-providers nil)
    (setq-default webpaste-provider-priority nil)

    ;; Make a fake provider that just "returns" the paste result by setting a
    ;; variable and concatinate it with "Works: " so we can see it showing up
    (setq-default webpaste-providers-alist
                  (list (list "workingprovider"
                              (lambda (text)
                                (setq returned-result
                                      (concat "Works: " text))))))

    ;; Call webpaste
    (webpaste-paste-text "test-string")

    ;; Check that we got the expected result
    (should (string= returned-result "Works: test-string"))))


(provide 'webpaste-test)
;;; webpaste-test.el ends here
