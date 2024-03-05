;;; test-webpaste-get-language.el --- Tests for webpaste -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)

(describe
 "Get language for buffer"

 ;; Setup the current buffer with a bash script
 (before-each
  (insert "#!/usr/bin/env bash")
  (newline)
  (insert "echo 'Hello'"))

 ;; Kill the contents of the current buffers bash script
 (after-each
  (kill-region (point-min) (point-max)))

 (it
  "can get the language by shebang"

  ;; Setup. Let the mode be fundamental-mode.
  (let ((major-mode 'fundamental-mode))
    ;; Do test. And expect the matched mode to be bash-mode, since the buffer
    ;; contains a bash shebang.
    (expect (webpaste--get-shebang-lang-mode)
            :to-equal
            'bash-mode)))

 (it
  "will use shebangs before major-mode"

  ;; Setup.
  (let ((major-mode 'fundamental-mode)
        (webpaste--provider-lang-alists '(("https://example.com/" .
                                           ((fundamental-mode . "text")
                                            (bash-mode . "bash"))))))

    (expect (webpaste--get-buffer-language "https://example.com/")
            :to-equal
            "bash"))))

;;; test-webpaste-get-language.el ends here
