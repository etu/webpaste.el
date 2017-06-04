;;; test-webpaste-return-url.el --- Tests for webpaste
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Returning URLs to the user"

 (before-each
  (spy-on 'message)
  (spy-on 'kill-new)
  (spy-on 'browse-url-generic)
  (spy-on 'simpleclip-set-contents))

 (it
  "can put in kill-ring and message the user"

  (webpaste-return-url "https://example.com/")

  (expect 'browse-url-generic :not :to-have-been-called)

  (expect 'message
          :to-have-been-called-with
          "Added %S to kill ring." "https://example.com/")

  (expect 'kill-new
          :to-have-been-called-with
          "https://example.com/"))

 (it
  "can open an external browser with the url"

  (let ((webpaste/open-in-browser t))
    (webpaste-return-url "https://example.com/")

    (expect 'browse-url-generic
            :to-have-been-called-with
            "https://example.com/")))

 (it
  "can append language on return"

  (let ((webpaste/provider-separators
         '(("https://example.com/" . "?lang=")))
        (webpaste/provider-lang-alists
         '(("https://example.com/" . ((lisp-interaction-mode . "lisp"))))))

    (spy-calls-reset 'kill-new)

    (webpaste-return-url "https://example.com/")

    (expect 'kill-new
            :to-have-been-called-with
            "https://example.com/?lang=lisp")))

 (it
  "can put contents in clipboard using simpleclip"
  (let ((webpaste/copy-to-clipboard t)
        (webpaste/webpaste/add-to-killring nil))

    (webpaste-return-url "https://example.com/")

    (expect 'simpleclip-set-contents
            :to-have-been-called-with
            "https://example.com/")

    (expect 'message
            :to-have-been-called-with
            "URL copied to clipboard."))))



;;; test-webpaste-return-url.el ends here
