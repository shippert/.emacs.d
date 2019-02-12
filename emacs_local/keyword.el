;; It looks like I have to build my own faces, because the regular ones are
;; too stupid, and I can't find a list of them.  So there.

(defface green-face
   '((((class color)) (:foreground  "darkgreen")))
  "It's green"
  :group 'faces)

(defvar green-face 'green-face
  "Symbol table foo")

(defface purple-face
   '((((class color)) (:foreground  "purple")))
  "It's purple"
  :group 'faces)

(defvar purple-face 'purple-face
  "Symbol table foo")

(defface red-face
   '((((class color)) (:foreground  "red")))
  "It's red"
  :group 'faces)

(defvar red-face 'red-face
  "Symbol table foo")

(defface gold-face
   '((((class color)) (:foreground  "goldenrod")))
  "It's gold"
  :group 'faces)

(defvar gold-face 'gold-face
  "Symbol table foo")


(defconst keyword-mode-keywords
  (list
   '("#.*" . font-lock-comment-face)
   '("^[\t ]*\\([A-Za-z0-9_-]+\\)[\t ]*:" 1  green-face)
   '("[\t ]*\\([A-Za-z0-9_-]+\\)[\t ]*:" 1 purple-face)
   '("[\t ]*\\([A-Za-z0-9_-]+\\)[\t ]*=" 1 red-face)
   '("\"\\(.*\\)\"" 1 gold-face)
   ))

(define-derived-mode keyword-mode fundamental-mode "Keyword"
  "Major mode for keyword:value flat files"

  ;;(setq comment-start "#")
  ;;(setq comment-start-skip "#\\W*")

  ;;register keywords
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(keyword-mode-keywords t))
  (font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.key$" . keyword-mode))
