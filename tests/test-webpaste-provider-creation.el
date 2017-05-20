;;; test-webpaste-provider-creation.el --- Tests for webpaste
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Creation of providers"
 :var (broken-provider working-provider)

 (before-each
  (setq broken-provider
        (webpaste-provider
         :uri "http://invalid-domain-name/"
         :post-field "data"
         :sync t
         :success-lambda (cl-function
                          (lambda (&key data &allow-other-keys)
                            (setq used-lambda "success")))
         :error-lambda (cl-function
                        (lambda (&key error-thrown &allow-other-keys)
                          (setq used-lambda "error")))))

  (setq working-provider
        (webpaste-provider
         :uri "https://httpbin.org/status/200"
         :post-field "data"
         :sync t
         :success-lambda (cl-function
                          (lambda (&key data &allow-other-keys)
                            (setq used-lambda "success")))
         :error-lambda (cl-function
                        (lambda (&key error-thrown &allow-other-keys)
                          (setq used-lambda "error"))))))


 (it
  "can trigger the error lambda of a provider"
  (let ((used-lambda nil))
    (funcall broken-provider "my test text")

    (expect used-lambda :to-equal "error")))


 (it
  "can trigger the success lambda of a provider"
  (let ((used-lambda nil))
    (funcall broken-provider "my test text")

    (expect used-lambda :to-equal "error")))

 (it
  "can failover from a broken provider"

  (spy-on 'message)
  (spy-on 'error)

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
                                   (funcall working-provider "failover"))))))

    (funcall provider "text")

    (expect used-lambda :to-equal "success"))))


;;; test-webpaste-provider-creation.el ends here
