;; sprak.el --- Using Emacs to interact with Else Heart.Break() and Sprak -*- lexical-binding: t -*-

;;;; GROUPS
(defgroup sprak nil
  "Major mode for Sprak programs"
  :group 'languages
  :prefix "sprak-")

(defcustom sprak-mode-hook nil
  "Hook called by `sprak-mode'"
  :type 'hook
  :group 'sprak)

;;;; FUNS
(defun sprank ()
  "Yank a Sprak program to clipboard. This makes it easy to write
programs in emacs and paste them into items in the game."
  (interactive)
  (save-excursion
    (kill-ring-save (point-min) (point-max))))

;;;; KEYMAP
(defvar sprak-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'sprank)
    map))

;;;; KEYWORDS
(defvar sprak-types
  '("void" "number" "string" "array" "bool" "var"))
(defvar sprak-assignment-operators
  '("=" "+=" "*=" "-=" "/="))
(defvar sprak-operators
  '("+" "-" "/" "*" "<" "<=" "==" ">=" ">" "!="))
(defvar sprak-control-flow
  '("loop" "if" "else" "in" "from" "to" "end" "return" "break"))
(defvar sprak-keyword
  `(,@sprak-assignment-operators ,@sprak-control-flow))

;;;; CUSTOM FACES
(defvar sprak-keyword-face
  (defface sprak-keyword-face
    '((t (:foreground "#33aaaa")))
    ;;    '((((background dark)) (:foreground "IndianRed")) (((background light)) (:foreground "Red3")))
    "Face to higlight control flow related keywords"
    :group 'sprak-faces))
(defvar sprak-type-face
  (defface sprak-type-face
    '((t  (:foreground "#00afff")))
    "Face to higlight sprak types"
    :group 'sprak-faces))
(defvar sprak-operator-face
  (defface sprak-operator-face
    '((t (:foreground "Pink")))
    "Face to higlight sprak math/infix operations"
    :group 'sprak-faces))
(defvar sprak-constant-face
  (defface sprak-constant-face
    '((t (:foreground "#33aaaa")))
    "Face to higlight sprak literals"
    :group 'sprak-faces))
(defvar sprak-fundef-face
  (defface sprak-fundef-face
    '((t (:foreground "#31ffbb")))
    "Face to higlight sprak function definitions"
    :group 'sprak-faces))
(defvar sprak-vardef-face
  (defface sprak-vardef-face
    '((t (:foreground "#31ffbb")))
    "Face to higlight sprak function definitions"
    :group 'sprak-faces))
(defvar sprak-funarg-face
  font-lock-constant-face)

;;;; REGEXPS
(defvar sprak-constants-regexp
  (rx (or "true"
          "false"
          (seq (? (any "-+"))
               (1+ digit)
               (? "." (0+ digit))))))
(defvar sprak-type-regexp
  (regexp-opt sprak-types 'words))
(defvar sprak-keyword-regexp
  (regexp-opt sprak-keyword 'symbols))
(defvar sprak-operator-regexp
  (regexp-opt sprak-operators 'symbols))

(defvar sprak-function-definition-regexp
  (rx (seq (group-n 1 (eval `(or ,@sprak-types)))
           (+ space)
           (group-n 2 (1+ word))
           (* space)
           "("
           (group-n 3 (* (or word space ",")))
           ")")))

(defvar sprak-variable-definition-regexp
  (rx (seq (group-n 1 (eval `(or ,@sprak-types)))
           (+ space)
           (group-n 2 (1+ word))
           (* space)
           (group-n 3 "="))))

;;;; FONT LOCK
(defvar sprak-font-lock-defaults
  `(((,sprak-function-definition-regexp (1 ,sprak-type-face)
                                        (2 ,sprak-fundef-face)
                                        (3 ,sprak-funarg-face))
     (,sprak-variable-definition-regexp (1 ,sprak-type-face)
                                        (2 ,sprak-vardef-face)
                                        (3 ,sprak-keyword-face))
     (,sprak-type-regexp . sprak-type-face)
     (,sprak-keyword-regexp . sprak-keyword-face)
     (,sprak-operator-regexp . sprak-operator-face)
     (,sprak-constants-regexp . sprak-constant-face))))

;;;; SYNTAX TABLE
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

;;;; MODE DEFN
(define-derived-mode sprak-mode fundamental-mode "Sprak"
  "Major mode for Sprak programs"
  :syntax-table sprak-syntax-table
  (setq font-lock-defaults sprak-font-lock-defaults)
  (use-local-map sprak-mode-keymap))

(add-to-list 'auto-mode-alist '("\\.sprak$" . sprak-mode))

(provide 'sprak-mode)
