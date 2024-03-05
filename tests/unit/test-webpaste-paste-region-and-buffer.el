;;; test-webpaste-paste-region-and-buffer.el --- Tests for webpaste -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load "tests/load-undercover.el")
(require 'webpaste)


(describe
 "Don't to paste region or buffer because of wrong answer to question"

 (before-each
  (spy-on 'webpaste--paste-text)
  (spy-on 'yes-or-no-p :and-return-value nil)
  (setq webpaste-paste-confirmation t))

 (after-each
  (setq webpaste-paste-confirmation nil))

 (it
  "can't paste because of answer to question is no"

  ;; Paste buffer
  (webpaste-paste-buffer)
  (expect 'webpaste--paste-text :not :to-have-been-called)

  ;; Paste region
  (webpaste-paste-region 10 100)
  (expect 'webpaste--paste-text :not :to-have-been-called)))


(describe
 "Paste region or buffer because of correct answer to question"

 (before-each
  (spy-on 'webpaste--paste-text)
  (spy-on 'yes-or-no-p :and-return-value t)
  (setq webpaste-paste-confirmation t))

 (after-each
  (setq webpaste-paste-confirmation nil))

 (it
  "can paste because of answer to question is yes"

  ;; Populate buffer with some content
  (with-temp-buffer
    (insert-file-contents "README.org")

    ;; Paste buffer
    (webpaste-paste-buffer)
    (expect 'webpaste--paste-text :to-have-been-called-with (buffer-string))

    ;; Paste region
    (webpaste-paste-region 10 100)
    (expect 'webpaste--paste-text
            :to-have-been-called-with
            (buffer-substring 10 100)))))


(describe
 "Paste region and buffer without question being asked"

 (before-each
  (spy-on 'webpaste--paste-text))

 (it
  "can paste entire buffers or regions"

  (with-temp-buffer
    (insert-file-contents "README.org")

    ;; Paste buffer
    (webpaste-paste-buffer)
    (expect 'webpaste--paste-text :to-have-been-called-with (buffer-string))

    ;; Paste region
    (webpaste-paste-region 10 100)
    (expect 'webpaste--paste-text
            :to-have-been-called-with
            (buffer-substring 10 100)))))


(describe
    "Paste buffer or region, but only choose one at a time depending on context"

  (before-each
    (spy-on 'webpaste-paste-buffer)
    (spy-on 'webpaste-paste-region))

  (after-each
    (deactivate-mark))

  (with-temp-buffer
    (insert-file-contents "README.org")

    (it "can paste entire buffers"
      ;; Paste buffer
      (webpaste-paste-buffer-or-region)
      (expect 'webpaste-paste-buffer :to-have-been-called-times 1)
      (expect 'webpaste-paste-region :to-have-been-called-times 0))

    (it "can paste selected region"
      ;; Mock selection of region
      (set-mark 10)
      (goto-char 100)
      (activate-mark)

      ;; Paste region
      (webpaste-paste-buffer-or-region)
      (expect 'webpaste-paste-buffer :to-have-been-called-times 0)
      (expect 'webpaste-paste-region :to-have-been-called-times 1))))


;;; test-webpaste-paste-region-and-buffer.el ends here
