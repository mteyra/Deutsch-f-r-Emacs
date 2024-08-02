;; Add a simple message to ensure the file is being loaded correctly:
(message "Custom German module loaded!")

;Pre-load packages.
;(require 'markdown-mode)
;(require 'google-translate)


;; File type detection for '.de' and '.deutsch' files
(add-to-list 'auto-mode-alist '("\\.de\\'" . deutsch-mode))
(add-to-list 'auto-mode-alist '("\\.deutsch\\'" . deutsch-mode))

;; 'auto-mode-alist' is a predefined list in Emacs that associates file patterns with major modes.
;; 'add-to-list' is a function that adds an element to a list if it is not already present, in this case adding to 'auto-mode-alist'.

;; Define a major mode for editing German language files, derived from markdown-mode
(define-derived-mode deutsch-mode markdown-mode "Deutsch Mode"
  "Major mode for editing German language files.")

;; 'define-derived-mode' is a macro used to define a new major mode based on an existing mode.
;; 'markdown-mode' is the base mode we are deriving from, which is a mode for editing markdown text.

;; Google Translate Configuration
(use-package google-translate
  :ensure t
  :defer t
  :config)

;; ':ensure t' ensures the package is installed if it isn't already.
;; ':defer t' defers loading the package until it's needed, improving startup time.
;; ':config' specifies configuration code to run after the package is loaded.

;; Command to translate selected text to German using google-translate-translate
(defun translate-selection-to-german ()
  "Translate selected text to German."
  (interactive)
  (require 'google-translate)
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (message "Selected text: %s" text) ;; Debugging statement
    (let ((translation (google-translate-translate "auto" "de" text)))
      (message "Translation result: %s" translation) ;; Debugging statement
      (delete-region (region-beginning) (region-end))
      (insert translation))))

;; Command to translate selected text to English using google-translate-translate
(defun translate-selection-to-english ()
  "Translate selected text to English."
  (interactive)
  (require 'google-translate)
  (let ((text (buffer-substring (region-beginning) (region-end))))
    (message "Selected text: %s" text) ;; Debugging statement
    (let ((translation (google-translate-translate "auto" "en" text)))
      (message "Translation result: %s" translation) ;; Debugging statement
      (delete-region (region-beginning) (region-end))
      (insert translation))))

;; 'defun' defines a new function.
;; '(interactive)' allows the function to interact with the user. It makes a function callable via M-x and key bindings.
;; 'let' introduces local variables with initial values. Syntax: (let ((variable1 value1) (variable2 value2) ...) (body)).
;; 'buffer-substring' returns the text between two points in the buffer. Syntax: (buffer-substring start end).


;; Adding words to the vault
(defun add-word-to-vault ()
  "Add the word at point to the word vault."
  (interactive)
  (let ((word (thing-at-point 'word 'no-properties))
        (vault-path "/media/haunted_pizza/OS/1) Linux Backup On Windows/Doom Emacs | Files and More/word-vault.txt"))
    (with-temp-file vault-path
      (insert-file-contents vault-path)
      (goto-char (point-max))
      (insert word "\n"))
    (message "Added word: %s" word)))

;; Removing words from the vault
(defun remove-word-from-vault ()
  "Remove the word at point from the word vault."
  (interactive)
  (let ((word (thing-at-point 'word 'no-properties))
        (vault-path "/media/haunted_pizza/OS/1) Linux Backup On Windows/Doom Emacs | Files and More/word-vault.txt"))
    (with-temp-file vault-path
      (insert-file-contents vault-path)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-quote word) "$") nil t)
        (replace-match ""))
      (delete-blank-lines))
    (message "Removed word: %s" word)))

;; Key bindings for Deutsch mode
; Use space as a leader key in normal mode only NOT insert mode.
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "SPC q q") 'add-word-to-vault)
  (define-key evil-visual-state-map (kbd "SPC q q") 'add-word-to-vault)

  (define-key evil-normal-state-map (kbd "SPC e e") 'remove-word-from-vault)
  (define-key evil-visual-state-map (kbd "SPC e e") 'remove-word-from-vault)

  (define-key evil-normal-state-map (kbd "SPC t d") 'translate-selection-to-german)
  (define-key evil-visual-state-map (kbd "SPC t d") 'translate-selection-to-german)

  (define-key evil-normal-state-map (kbd "SPC t e") 'translate-selection-to-english)
  (define-key evil-visual-state-map (kbd "SPC t e") 'translate-selection-to-english)

  ;; Ensure space key is not overridden in insert mode
  (define-key evil-insert-state-map (kbd "SPC") nil)
  )


;; Bind the command to a key
;; (define-key deutsch-mode-map (kbd "SPC t t") 'translate-selection)



;; Set default input method to German
(setq default-input-method "german-postfix")

;; Activate German input method in deutsch-mode
(add-hook 'deutsch-mode-hook (lambda () (activate-input-method "german-postfix")))





;; Ensure company-mode is installed and available
;(condition-case err
;    (use-package company
;      :ensure t
;      :init
;      (message "Attempting to load company mode...")
;      :config
;      (message "Company mode loaded successfully!")
;      (global-company-mode))
;  (error (message "Error loading company: %s" (error-message-string err))))

































;; Word vault suggestions
;(defvar my-word-vault-path "/media/haunted_pizza/os/1) Linux Backup On Windows/Doom Emacs | Files and More/word-vault.txt")



;; Bind the different G-Translate function for testing:
;(define-key deutsch-mode-map (kbd "SPC g 1") 'translate-selection-to-german-1)
;(define-key deutsch-mode-map (kbd "SPC g 2") 'translate-selection-to-german-2)
;(define-key deutsch-mode-map (kbd "SPC g 3") 'translate-selection-to-german-3)
;(define-key deutsch-mode-map (kbd "SPC g 4") 'translate-selection-to-german-4)



;; BASIC CODE BLOCK
;; Command to translate selected text to German
;(defun translate-selection-to-german ()
;  (interactive)
;  (require 'google-translate)
;; ------------
;  (let ((text (buffer-substring (region-beginning) (region-end))))
;    (let ((text (buffer-substring (region-beginning) (region-end))))
;      (message "Selected text: %s" text) ;; Debugging statement

;      (let ((translation (google-translate-translate "auto" "de" text)))
;        (message "Translation result: %s" translation) ;; Debugging statement
;; ------------
;        (delete-region (region-beginning) (region-end))
;        (insert translation))))
;; ------------
