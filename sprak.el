;; sprak.el --- Using Emacs to interact with Else Heart.Break() and Sprak -*- lexical-binding: t -*-

(defun sprank-program (&optional source)
  "Yank source or the current buffer to prepare to copy into the game."
  (interactive)
  (save-excursion
    (with-current-buffer
        (or (and source (find-file-existing (concat "sprak/" source ".sprak")))
            (current-buffer))
      (kill-ring-save (point-min) (point-max)))))

(defvar sprak-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'sprank-program)
    map))

(defvar sprak-types
  '("void" "number" "string" "array" "bool" "var"))

(defvar sprak-assignment-operators
  '("=" "+=" "*=" "-=" "/="))

(defvar sprak-operators
  '("+" "-" "/" "*" "<" "<=" "==" ">=" ">" "!="))

(defvar sprak-keywords-beginning '("loop" "if"))
(defvar sprak-keywords-middle '("else" "in" "from" "to"))
(defvar sprak-keywords-ending '("end" "return" "break"))
(defvar sprak-keywords
  `(,@sprak-assignment-operators ;; these are kind of like keywords 
    ,@sprak-keywords-beginning
    ,@sprak-keywords-middle
    ,@sprak-keywords-ending))

(defvar sprak-keywords-face font-lock-builtin-face)
(defvar sprak-type-face font-lock-type-face)
(defvar sprak-operator-face font-lock-comment-delimiter-face)
(defvar sprak-constant-face font-lock-preprocessor-face)
(defvar sprak-fundef-face font-lock-function-name-face)
(defvar sprak-funarg-face font-lock-constant-face)

(defvar sprak-constants-regexp
  (rx (or "true" "false" (seq (? (any "-+")) (1+ digit) (? "." (0+ digit))))))
(defvar sprak-type-regexp
  (regexp-opt sprak-types 'words))
(defvar sprak-keyword-regexp
  (regexp-opt sprak-keywords 'symbols))
(defvar sprak-operator-regexp
  (regexp-opt sprak-operators 'symbols))

;; most likely a poorly written regexp but it seems to do the trick!
;; would be nice to not highlight commas too.
(defvar sprak-function-definition-regexp
  (rx (seq (group-n 1 (eval `(or ,@sprak-types)))
           (1+ space)
           (group-n 2 (1+ word))
           (* space)
           "("
           (group-n 3 (0+ (or word space ",")))
           ")")))

(defvar sprak-font-lock-defaults
  `(((,sprak-function-definition-regexp 
      (1 ,sprak-type-face)
      (2 ,sprak-fundef-face)
      (3 ,sprak-funarg-face))
     (,sprak-type-regexp . sprak-type-face)
     (,sprak-keyword-regexp . sprak-keywords-face)
     (,sprak-operator-regexp . sprak-operator-face)
     (,sprak-constants-regexp . sprak-constant-face))))

(defvar sprak-syntax-table
  (let ((T (make-syntax-table)))
    (modify-syntax-entry ?#  "< " T)
    (modify-syntax-entry ?\n "> " T)
    (modify-syntax-entry ?,  ". " T)
    (modify-syntax-entry ?_  "w " T)
    (modify-syntax-entry ?!  "_ " T)
    (modify-syntax-entry ?@  "_ " T)
    T)
  "Syntax table for Sprak")

(define-derived-mode sprak-mode fundamental-mode "Sprak"
  "Major mode for Sprak programs"
  :syntax-table sprak-syntax-table
  (setq font-lock-defaults sprak-font-lock-defaults)
  (use-local-map sprak-mode-map))

(add-to-list 'auto-mode-alist '("\\.sprak$" . sprak-mode))

(provide 'sprak-mode)
