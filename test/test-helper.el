;;; webpaste --- Test helper for loading webpaste in unit tests
;;; Commentary:
;;; Code:
(require 'f)
(require 'ert)
(require 'undercover)

(defvar webpaste-test/test-path
  (f-parent (f-this-file)))

(defvar webpaste-test/root-path
  (f-parent webpaste-test/test-path))

(undercover "webpaste.el")

(require 'webpaste (f-expand "webpaste" webpaste-test/root-path))

;;; test-helper.el ends here