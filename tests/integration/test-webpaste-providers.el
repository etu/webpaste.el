;;; test-webpaste-providers.el --- Tests for webpaste providers
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Test provider with dummy data so it"
 :var (paste-message webpaste--paste-text webpaste--return-url)

 (before-each
  (setq paste-message ";; This is a build artifact created by an integration test for https://github.com/etu/webpaste.el")

  ;; Block requests to failovers and returning of URLs
  (spy-on 'webpaste--paste-text)
  (spy-on 'webpaste--return-url))


 (it
  "can paste with ix.io [ci]"

  (funcall (webpaste--get-provider-by-name "ix.io") paste-message :sync t)

  (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
  (expect (spy-calls-count 'webpaste--paste-text) :to-equal 0))


 (it
  "can paste with dpaste.com [ci]"

  (funcall (webpaste--get-provider-by-name "dpaste.com") paste-message :sync t)

  (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
  (expect (spy-calls-count 'webpaste--paste-text) :to-equal 0))


 (it
  "can paste with dpaste.org [ci]"

  (funcall (webpaste--get-provider-by-name "dpaste.org") paste-message :sync t)

  (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
  (expect (spy-calls-count 'webpaste--paste-text) :to-equal 0))

 (it
  "can paste with paste.mozilla.org [ci]"

  (funcall (webpaste--get-provider-by-name "paste.mozilla.org") paste-message :sync t)

  (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
  (expect (spy-calls-count 'webpaste--paste-text) :to-equal 0))


 (it
  "can paste with gist.github.com [local]"

  ;; Override function to extract filename from a filepath, otherwise it breaks
  ;; during integration tests
  (spy-on 'file-name-nondirectory :and-return-value "file.txt")

  (funcall (webpaste--get-provider-by-name "gist.github.com") paste-message :sync t)

  (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
  (expect (spy-calls-count 'webpaste--paste-text) :to-equal 0))


 (it
  "can paste with paste.pound-python.org [ci]"

  (funcall (webpaste--get-provider-by-name "paste.pound-python.org") paste-message :sync t)

  (expect (spy-calls-count 'webpaste--return-url) :to-equal 1)
  (expect (spy-calls-count 'webpaste--paste-text) :to-equal 0)))


;;; test-webpaste-providers.el ends here
