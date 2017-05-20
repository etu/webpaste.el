;;; load-undercover --- Helper to load undercoverr
;;; Commentary:
;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"))

;;; load-undercover.el ends here
