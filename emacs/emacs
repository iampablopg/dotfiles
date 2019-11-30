;; --------------------------------------------------------------------------------------------------------------------------------
;; [ FILE ] My Emacs customization file
;; [ NAME ] .emacs
;; [ PATH ] ~/.emacs
;; [ AUTH ] Pablo Palomino Gómez (pablo.palomino1@outlook.com)

;; --------------------------------------------------------------------------------------------------------------------------------
;; PACKAGES
;; --------------------------------------------------------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; --------------------------------------------------------------------------------------------------------------------------------
;; FAVOURITE EMACS THEMES
;; --------------------------------------------------------------------------------------------------------------------------------
(load-theme 'jbeans t)
;; (load-theme 'granger t)
;; (load-theme 'material t)
;; (load-theme 'tangotango t)
;; (load-theme 'flatland t)
;; (load-theme 'busybee t)
;; (load-theme 'nimbus t)
;; (load-theme 'spacegray t)
;; (load-theme 'ample t)

;; --------------------------------------------------------------------------------------------------------------------------------
;; PERSONAL KEYMAP
;; --------------------------------------------------------------------------------------------------------------------------------
;; MOVE CURRENT LINE UP AND DOWN
(global-set-key [(control  up)]  'move-line-up)
(global-set-key [(control  down)] 'move-line-down)
;; RECURSIVE SEARCH FILENAME IN PROJECT
(global-set-key [(control t)] 'helm-projectile)
;; OPEN NEW TERM IN CURRENT BUFFER
(global-set-key [(control x) t] 'multi-term)
;; SHOW/HIDE LINE NUMBERS
(global-set-key (kbd "C-c l") 'linum-mode)
;; SHOW GIT BLAME OF CURRENT LINE IN MINIBUFFER
(global-set-key (kbd "C-c b") 'git-blame-line)
;; ACTIVATE/DEACTIVATE UNDO TREE MODE
(global-set-key (kbd "C-c u") 'undo-tree-mode)
;; MULTI CURSOR IN SELECTED LINES
(global-set-key (kbd "C-c m") 'mc/edit-lines)
;; AG SEARCH IN CURRENT PROJECT
(global-set-key (kbd "C-c a") 'ag-project)
;; SHOW/HIDE PROJECT DIRECTORY TREE
(global-set-key [f8] 'neotree-toggle)
;; LOAD INPUT THEME
(global-set-key [f7] 'load-theme)
;; GO TO ELIXIR DOC OFFICIAL SITE
;; (global-set-key (kbd "C-c e") 'elixir-mode-open-docs-stable)

;; --------------------------------------------------------------------------------------------------------------------------------
;; HOOKS
;; --------------------------------------------------------------------------------------------------------------------------------
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'term-mode-hook 'my-inhibit-global-linum-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 2)))

;; --------------------------------------------------------------------------------------------------------------------------------
;; UTILS (HIDE MENUBAR, FULLSCREEN, REQUIRES, AUTOCOMPLETE, AFTER-SAVE FORMATS AND MORE)
;; --------------------------------------------------------------------------------------------------------------------------------
(dumb-jump-mode)
(menu-bar-mode -1)
(ac-config-default)
(global-linum-mode 1)
(set-face-attribute 'default nil :height 145)

(setq ag-highlight-search t)
(setq ac-ignore-case nil)
(setq linum-format 'linum-highlight-current-line-number)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'load-path "~/.emacs.d/linum")
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-to-list 'elixir-mode-hook 'alchemist-mode)

(eval-after-load "elixir-mode"
  '(defun elixir-format--mix-executable ()
     (string-trim-right (shell-command-to-string "asdf which mix"))))

(require 'ag)
(require 'neotree)
(require 'all-the-icons)
(require 'linum-highlight-current-line-number)
(require 'auto-complete-config)

;; --------------------------------------------------------------------------------------------------------------------------------
;;  FUNCTIONS
;; --------------------------------------------------------------------------------------------------------------------------------
;; MOVE CURRENT LINE UP AND DOWN
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; CUSTOM CURRENT LINE GIT BLAME IN MINIBUFFER (LINE XX: COMMIT_ID | TITLE | AUTHOR | DATE)
(defun git-blame-line ()
  (interactive)
  (let* ((line-number (save-excursion
                        (goto-char (point-at-bol))
                        (+ 1 (count-lines 1 (point)))))
         (line-arg (format "%d,%d" line-number line-number))
         (commit-buf (generate-new-buffer "*git-blame-line-commit*")))
    (call-process "git" nil commit-buf nil
                  "blame" (buffer-file-name) "-L" line-arg)
    (let* ((commit-id (with-current-buffer commit-buf
                        (buffer-substring 1 9)))
           (log-buf (generate-new-buffer "*git-blame-line-log*")))
      (kill-new commit-id)
      (call-process "git" nil log-buf nil
                    "log" "-1" "--pretty=%h  |  %s  |  %an (%ae)  |  %ad" commit-id)
      (with-current-buffer log-buf
        (message "Line %d: %s" line-number (buffer-string)))
      (kill-buffer log-buf))
    (kill-buffer commit-buf)))

;; DISABLE LINUM MODE IN TERMS
(defun my-inhibit-global-linum-mode ()
  "Counter-act `global-linum-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))

;; ALL THE ICONS IN PROJECT TREE
(defun neo-buffer--insert-fold-symbol (name &optional file-name)
 "Custom overriding function for the fold symbol.
`NAME' decides what fold icon to use, while` FILE-NAME' decides
what file icon to use."
 (or
     (and (equal name 'leaf)  (insert (all-the-icons-icon-for-file file-name)))))
(setq ruby-insert-encoding-magic-comment nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(font-use-system-font t)
 '(inhibit-startup-screen t)
 '(linum-format " %3i ")
 '(save-place t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
