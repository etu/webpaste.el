;;; test-webpaste.el --- Tests for webpaste
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)

(describe "The feature"
  (it "can use bug and feature"
    (expect t
            :to-equal
            t)))

;;; test-webpaste.el ends here
