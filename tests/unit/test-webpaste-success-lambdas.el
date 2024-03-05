;;; test-webpaste-success-lambdas.el --- Tests for success lambdas -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Create lambdas to use on successes"

 (before-each
  (spy-on 'request-response-header :and-return-value "https://example.com/")
  (spy-on 'request-response-url :and-return-value "https://example.com/")
  (spy-on 'webpaste--return-url))

 (it
  "using a response header"

  (let ((success-lambda (webpaste--providers-success-location-header)))
    (funcall success-lambda :response "my fake response")

    (expect 'webpaste--return-url
            :to-have-been-called-with
            "https://example.com/")))


 (xit
  "when using some request.el response thingy"
  (let ((success-lambda (webpaste--providers-success-response-url)))
    (funcall success-lambda :response "my fake response")

    (expect 'webpaste--return-url
            :to-have-been-called-with
            "https://example.com/")))


 (it
  "when returning a string with an url"

  (let ((success-lambda (webpaste--providers-success-returned-string)))
    (funcall success-lambda :data "\"https://example.com/\"
")

    (expect 'webpaste--return-url
            :to-have-been-called-with
            "https://example.com/"))))


;;; test-webpaste-success-lambdas.el ends here
