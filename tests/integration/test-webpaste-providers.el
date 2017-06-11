;;; test-webpaste-providers.el --- Tests for webpaste providers
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Test provider with dummy data so it"

 (before-each
  ;; Block requests
  (spy-on 'webpaste-paste-text)
  (spy-on 'webpaste--return-url))

 (it
  "can paste with ptpb.pw"

  (let ((provider (webpaste--get-provider-by-name "ptpb.pw")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0)

    (expect (spy-calls-most-recent 'webpaste--return-url)
            :to-equal
            (make-spy-context :current-buffer (current-buffer)
                              :args '("https://ptpb.pw/gLC6")
                              :return-value nil))))


 (it
  "can paste with ix.io"

  (let ((provider (webpaste--get-provider-by-name "ix.io")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0)

    (expect (spy-calls-most-recent 'webpaste--return-url)
            :to-equal
            (make-spy-context :current-buffer (current-buffer)
                              :args '("http://ix.io/whJ")
                              :return-value nil))))


 (it
  "can paste with sprunge.us"

  (let ((provider (webpaste--get-provider-by-name "sprunge.us")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0)))


 (it
  "can paste with dpaste.com"

  (let ((provider (webpaste--get-provider-by-name "dpaste.com")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0)))


 (it
  "can paste with dpaste.de"

  (let ((provider (webpaste--get-provider-by-name "dpaste.de")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0)))


 (it
  "can paste with gist.github.com"

  ;; Override function to extract filename from a filepath, otherwise it breaks during integration tests
  (spy-on 'file-name-nondirectory :and-return-value "file.txt")

  (let ((provider (webpaste--get-provider-by-name "gist.github.com")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0)))


 (it
  "can paste with paste.pound-python.org"

  (let ((provider (webpaste--get-provider-by-name "paste.pound-python.org")))
    (funcall
     provider
     ";; This is a build artifact made from an integration test for https://github.com/etu/webpaste.el"
     :sync t)

    (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
    (expect (spy-calls-count 'webpaste-paste-text) :to-equal 0))))


;;; test-webpaste-providers.el ends here
