# Custom German Mode for Emacs

This custom Emacs configuration provides an enhanced environment for editing German language files. It leverages markdown-mode, integrates Google Translate for translations, and offers custom keybindings for an efficient workflow. Below is a comprehensive guide to setting up and using this custom mode.

## Features

1. **File Type Detection**: Automatically detects files with `.de` and `.deutsch` extensions and activates `deutsch-mode`.
2. **Major Mode**: Defines a new major mode `deutsch-mode` derived from `markdown-mode`.
3. **Google Translate Integration**: Provides functions to translate selected text to and from German.
4. **Word Vault**: Allows adding and removing words from a custom word vault.
5. **Custom Keybindings**: Includes custom keybindings for quick access to the main features.
6. **German Input Method**: Automatically activates a German input method in `deutsch-mode`.

## Installation

1. **Pre-requisites**: Ensure you have `markdown-mode`, `google-translate`, and `use-package` installed.

    ```elisp
    (require 'markdown-mode)
    (require 'google-translate)
    ```

2. **Configuration**: Add the following configuration to your Emacs initialization file.

    ```elisp
    ;; Add a simple message to ensure the file is being loaded correctly:
    (message "Custom German module loaded!")

    ;; File type detection for '.de' and '.deutsch' files
    (add-to-list 'auto-mode-alist '("\\.de\\'" . deutsch-mode))
    (add-to-list 'auto-mode-alist '("\\.deutsch\\'" . deutsch-mode))

    ;; Define a major mode for editing German language files, derived from markdown-mode
    (define-derived-mode deutsch-mode markdown-mode "Deutsch Mode"
      "Major mode for editing German language files.")

    ;; Google Translate Configuration
    (use-package google-translate
      :ensure t
      :defer t
      :config)

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

    ;; Adding words to the vault
    (defun add-word-to-vault ()
      "Add the word at point to the word vault."
      (interactive)
      (let ((word (thing-at-point 'word 'no-properties))
            (vault-path "/path/to/your/word-vault.txt"))
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
            (vault-path "/path/to/your/word-vault.txt"))
        (with-temp-file vault-path
          (insert-file-contents vault-path)
          (goto-char (point-min))
          (while (re-search-forward (concat "^" (regexp-quote word) "$") nil t)
            (replace-match ""))
          (delete-blank-lines))
        (message "Removed word: %s" word)))

    ;; Key bindings for Deutsch mode
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

    ;; Set default input method to German
    (setq default-input-method "german-postfix")

    ;; Activate German input method in deutsch-mode
    (add-hook 'deutsch-mode-hook (lambda () (activate-input-method "german-postfix")))
    ```

## Function List

### `deutsch-mode`
- **Description**: Major mode for editing German language files, derived from `markdown-mode`.

### `translate-selection-to-german`
- **Description**: Translates the selected text to German using Google Translate.
- **Usage**: Select text and call `M-x translate-selection-to-german`.

### `translate-selection-to-english`
- **Description**: Translates the selected text to English using Google Translate.
- **Usage**: Select text and call `M-x translate-selection-to-english`.

### `add-word-to-vault`
- **Description**: Adds the word at point to the word vault.
- **Usage**: Place cursor on a word and call `M-x add-word-to-vault`.

### `remove-word-from-vault`
- **Description**: Removes the word at point from the word vault.
- **Usage**: Place cursor on a word and call `M-x remove-word-from-vault`.

## Keybindings

- `SPC q q`: Add the word at point to the word vault (Normal and Visual mode).
- `SPC e e`: Remove the word at point from the word vault (Normal and Visual mode).
- `SPC t d`: Translate the selected text to German (Normal and Visual mode).
- `SPC t e`: Translate the selected text to English (Normal and Visual mode).

## Additional Configuration

Ensure that the `word-vault.txt` path is correctly set to your desired location. Modify the path in the `add-word-to-vault` and `remove-word-from-vault` functions.

```elisp
(let ((vault-path "/path/to/your/word-vault.txt"))
```







# emacs-german-filetype
An emacs configuration for .de or .deutsch filetypes.
<br/> <br/>
Ideally, this project will grow into a module for Language learning on Emacs. <br/>
However for now it is a playground for myself, wherein I write my daily diary in german using ".de", until I've tweeked it to perfection. <br/>
<br/>
