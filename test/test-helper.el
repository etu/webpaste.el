;;; webpaste --- Test helper for loading webpaste in unit tests
;;; Commentary:
;;; Code:
(require 'f)


(defvar webpaste-test--test-path
  (f-parent (f-this-file)))

(defvar webpaste-test--root-path
  (f-parent webpaste-test--test-path))

(require 'ert)
(require 'webpaste (f-expand "webpaste" webpaste-test--root-path))


(provide 'test-helper)
;;; test-helper.el ends here
