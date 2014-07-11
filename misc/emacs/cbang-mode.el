;; First attempt to write an emacs mode for C!



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cb\\w?" . cbang-mode))

;; Woot, minimal work
(define-derived-mode cbang-mode c++-mode
  "C! mode"
  "Major mode for editing C!"
)

(provide 'cbang-mode)

;; END
