;;; webpaste --- Tests for webpaste
;;; Commentary:
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'webpaste)



(ert-deftest webpaste-test/provider ()
  "Test creation of providers."

  (let ((used-lambda nil)
        (provider (webpaste-provider
                   :uri "http://invalid-domain-name/"
                   :post-field "data"
                   :sync t
                   :success-lambda (cl-function
                                    (lambda (&key data &allow-other-keys)
                                      (setq used-lambda "success")))
                   :error-lambda (cl-function
                                  (lambda (&key error-thrown &allow-other-keys)
                                    (setq used-lambda "error"))))))

    (funcall provider "dummy-text")

    (should (equal "error" used-lambda)))

  (let ((used-lambda nil)
        (provider (webpaste-provider
                   :uri "https://httpbin.org/status/200"
                   :post-field "data"
                   :sync t
                   :success-lambda (cl-function
                                    (lambda (&key data &allow-other-keys)
                                        (setq used-lambda "success")))
                   :error-lambda (cl-function
                                  (lambda (&key error-thrown &allow-other-keys)
                                    (setq used-lambda "error"))))))

    (funcall provider "dummy-text")

    (should (equal "success" used-lambda))))



(ert-deftest webpaste-test/paste-region-and-buffer ()
  "Test pasting of regions and buffers."

  ;; Override pasting function to just return the inserted string
  (cl-letf (((symbol-function 'webpaste-paste-text)
             (lambda (text) text)))

    ;; Set up a temporary buffer
    (with-temp-buffer
      ;; With README as content
      (insert-file-contents "README.org")

      ;; And make sure that the paste buffer function returns the same as we had
      ;; in the buffer.
      (should (equal (webpaste-paste-buffer) (buffer-string)))

      ;; Test so webpaste-paste-region selects the same part of the buffer as to
      ;; be expected.
      (should (equal (webpaste-paste-region 10 100)
                     (buffer-substring 10 100)))

      ;; Test when wanting a paste confirmation
      (let ((webpaste/paste-confirmation t))

        ;; Override yes-or-no-p to immitate "yes" response
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (text) t)))

          (should (equal (webpaste-paste-buffer) (buffer-string)))

          (should (equal (webpaste-paste-region 10 100)
                         (buffer-substring 10 100))))

        ;; Override yes-or-no-p to immitate "no" response
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (text) nil)))

          (should (not (equal (webpaste-paste-buffer) (buffer-string))))

          (should (not (equal (webpaste-paste-region 10 100)
                                 (buffer-substring 10 100)))))))))



(ert-deftest webpaste-test/return-url ()
  "Test returning of URL's to the user."

  ;; Override browse-url-generic to set a variable to t if triggered
  (cl-letf (((symbol-function 'browse-url-generic)
             (lambda (url) (setq webpaste-test/opened-in-browser t))))

    ;; Test to return a link and check that the message logged is the one we expect
    (let ((webpaste/open-in-browser nil)(webpaste-test/opened-in-browser nil))
      (should (equal
               (webpaste-return-url "https://example.com/")
               "Added \"https://example.com/\" to kill ring."))

      ;; Check so the kill ring contain the correct contents
      (should (equal (car kill-ring) "https://example.com/"))

      ;; Check so the link wasn't opened in a browser
      (should (equal webpaste-test/opened-in-browser nil)))

    ;; Test that we call browse-url-generic with the link if option to open in
    ;; browser is set
    (let ((webpaste/open-in-browser t)(webpaste-test/opened-in-browser nil))
      (webpaste-return-url "https://example.com/")

      (should (equal webpaste-test/opened-in-browser t)))))



(ert-deftest webpaste-test/get-provider-priority ()
  "Test how it populates webpaste/get-provider-priority."

  ;;; Test autopopulation of list based on providers avaliable
  ;; Prepare variables
  (setq-default webpaste-provider-priority nil)
  (setq-default webpaste-providers-alist
                `(("provider1" "lambda")
                  ("provider2" "lambda")
                  ("provider3" "lambda")))

  ;; Do test
  (should (equal (webpaste/get-provider-priority)
                 '("provider1" "provider2" "provider3")))

  ;;; Test static population of provider priority
  ;; Prepare variables
  (setq-default webpaste-provider-priority
                '("provider2" "provider1" "provider3"))
  (setq-default webpaste-providers-alist '())

  ;; Do test
  (should (equal (webpaste/get-provider-priority)
                 '("provider2" "provider1" "provider3"))))



(ert-deftest webpaste-test/callback-from-working-provider ()
  "This test just sends a message to a good provider that just works."

  ;; Temporal storage for result
  (let ((returned-result nil))
    ;; Reset all webpaste variables
    (setq-default webpaste-tested-providers nil)
    (setq-default webpaste-provider-priority nil)

    ;; Make a fake provider that just "returns" the paste result by setting a
    ;; variable and concatinate it with "Works: " so we can see it showing up
    (setq-default webpaste-providers-alist
                  `(("workingprovider"
                     ,(lambda (text)
                        (setq returned-result
                              (concat "Works: " text))))))

    ;; Call webpaste
    (webpaste-paste-text "test-string")

    ;; Check that we got the expected result
    (should (string= returned-result "Works: test-string"))))



(ert-deftest webpaste-test/callback-from-working-provider-as-fallback ()
  "This test sends a message to a bad provider that returns some error data.

Then the bad provider pastes again like it should and we check that we got the
result from the good provider only."

  ;; Temporal storage for result
  (let ((returned-result nil))
    ;; Reset all webpaste variables
    (setq-default webpaste-tested-providers nil)
    (setq-default webpaste-provider-priority nil)

    ;; Creates a "broken" provider that will call on the next provider due to a
    ;; faked failure and checks that the next provider is picked up correctly.
    (setq-default webpaste-providers-alist
                  `(("brokenprovider"
                     ,(lambda (text)
                        ;; Set return text
                        (setq returned-result
                              (concat "Broken: " text))

                        ;; Call paste again
                        (webpaste-paste-text text)))

                    ("workingprovider"
                     ,(lambda (text)
                        (setq returned-result
                              (concat "Working: " text))))))

    ;; Call webpaste
    (webpaste-paste-text "test-string")

    ;; Check that we got the expected result
    (should (string= returned-result "Working: test-string"))))



(ert-deftest webpaste-test/get-lang-alist-with-overrides ()
  "This test tests all cases that should happen when overriding langs."

  (let ((webpaste/default-lang-alist '((python-mode . "python")
                                       (php-mode . "php"))))

    ;; Test adding mode
    (should (equal (webpaste/get-lang-alist-with-overrides
                    '((emacs-lisp-mode . "lisp")))

                   '((emacs-lisp-mode . "lisp")
                     (python-mode . "python")
                     (php-mode . "php"))))

    ;; Test removing mode / clearing it's value
    (should (equal (webpaste/get-lang-alist-with-overrides
                    '((python-mode . nil)))

                   '((python-mode)
                     (python-mode . "python")
                     (php-mode . "php"))))

    ;; Test overriding mode
    (should (equal (webpaste/get-lang-alist-with-overrides
                    '((python-mode . "python3")))

                   '((python-mode . "python3")
                     (python-mode . "python")
                     (php-mode . "php"))))))


;;; webpaste-test.el ends here
