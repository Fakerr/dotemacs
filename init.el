(require 'package)

;; Set up font and font size
;; For emacs-27,use set-frame-font instead of set-default-font
(set-frame-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 105)

;; load theme
(add-hook 'after-init-hook (lambda () (load-theme 'nord)))

;; Activate linum-mode and format it
(global-linum-mode 0)
(setq linum-format " %3d ")

;; Disable linum-mode for ansi-term
(add-hook 'term-mode-hook (lambda()
                (linum-mode -1)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Activate EVIL MODE
(require 'evil)
(evil-mode 1)

;; Set cursor color
(set-cursor-color "#7FD6D6")

;; Enable column number
(column-number-mode 1)

;; Require neotree and set key bindings
(require 'neotree)
(global-set-key [f12] 'neotree-toggle)
(global-set-key [f5] 'neotree-toggle)

;; Disable evil mode when in netotree
(add-hook 'neotree-mode-hook 'evil-emacs-state)

;; Hilights matching parenthesis and customization.
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-background 'show-paren-match "#ffc04d")
(set-face-foreground 'show-paren-match "#1B2B34")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)


;; Move line up/down. TODO: extract to another file.
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-n") 'move-line-down)
;; End move line

;; Disable yasnippet when in term mode
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;; Set block selection background color
(set-face-attribute 'region nil :background "#5f6f7f")

;; Enable winner mode (fast window configuration)
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; Auto revert buffers when file changes on disk (useful for version controll)
(global-auto-revert-mode 1)

;; Set up ivy, counsel and swiper
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; M-x enhancement for Emacs.
(require 'smex) 
(smex-initialize) 

;; Save emacs session
(desktop-save-mode 1)

;; Enable smartparens (auto pairs)
(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
;;(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'html-mode-hook #'smartparens-mode)
(add-hook 'lb-datalog-mode-hook #'smartparens-mode)
(add-hook 'sh-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)

;; Set key binding to avy
(global-set-key (kbd "C-:") 'avy-goto-char)

;; Disable this annoying thing :\ No newline at end of file
(setq require-final-newline nil)

;; Enable projectile-mode (should be integrated with counsel via counsel-projectile library)
(projectile-mode 1)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Integrate projectile with counsel_projectile.
;;(counsel-projectile-mode 1)

;; Disable backup files.
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; FIXME:  Enable protobuf mode
(add-to-list 'load-path "~/.emacs.d/proto")
(require 'protobuf-mode)

;; Markdow live preview plugin
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

;; Improve cursor perform
(setq auto-window-vscroll nil)

;;;;;;;;;;;   Shell configuration   ;;;;;;;;;
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Set frames.
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;;(set-frame-parameter (selected-frame) 'alpha '(100 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (100 . 50)))

;; Enable smooth-scrolling. (Scrolling like others ides)
;;(require 'smooth-scrolling)
;;(smooth-scrolling-mode 1)
;;(setq smooth-scroll-margin 3)
;;(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
;;(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
;;(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line

;; Generated config (do not touch)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quickhelp-color-background "#4C566A")
 '(company-quickhelp-color-foreground "#D8DEE9")
 '(custom-safe-themes
   '("1d7e67fe9d8deacf470ffb2c6ccb181ac5c1af580f9edbdba90e6e0f1ba56ace" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8ac2745fb5d9dad05f42228655508e14e4ce3a5adf64c9bedaa6e570a55f60be" "7f968c172d6ec46766773a8304c7570bdff45f1220d3700008a437d9529ca3e4" default))
 '(lsp-enable-links t)
 '(lsp-go-import-shortcut "Both")
 '(lsp-go-links-in-hover t)
 '(markdown-link-space-sub-char "-")
 '(package-selected-packages
   '(ag counsel-projectile yasnippet-snippets flycheck lsp-treemacs company yasnippet dash company-quickhelp pos-tip zenburn-theme yascroll white-theme web-mode use-package twilight-bright-theme solarized-theme smooth-scrolling smooth-scroll smex smartparens scss-mode scala-mode rjsx-mode relative-line-numbers rainbow-mode prettier-js powerline oceanic-theme nord-theme nlinum nix-mode neotree monky mode-icons markdown-mode linum-relative less-css-mode lb-datalog-mode jsx-mode json-mode javap-mode go-complete go-autocomplete github-modern-theme ggtags fill-column-indicator fic-mode exec-path-from-shell evil-surround evil-mc dumb-jump dockerfile-mode avy arjen-grey-theme anything-project all-the-icons afternoon-theme abyss-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(button ((t (:background "#2E3440" :foreground "#88C0D0" :box (:line-width 1 :color "#D8DEE9" :style sunken-button)))))
 '(link ((t (:underline t))))
 '(lsp-headerline-breadcrumb-separator-face ((t (:inherit shadow :foreground "PaleVioletRed3" :height 0.8))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-doc-face :foreground "PaleVioletRed3" :weight bold))))
 '(markdown-link-face ((t (:foreground "#88C0D0")))))

;; Enable EVIL MODE multi cursor.
(require 'evil-mc)
(global-evil-mc-mode  1)

(setq evil-mc-enable-bar-cursor t)
(set-face-background 'evil-mc-cursor-default-face "#7FD6D6")

;; Enable evil surround mode.
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Company mode
(require 'company)                                   ; load company mode
;;(setq company-idle-delay 0.3)
;;(setq company-minimum-prefix-length 1)
;;(setq company-tooltip-limit 20)                      ; bigger popup window
;;(setq company-idle-delay .1)                         ; decrease delay before autocompletion popup shows
;;(setq company-echo-delay 0)                          ; remove annoying blinking
;;(setq company-begin-commands '(self-insert-command))   ; start autocompletion only after typing
;;(add-hook 'after-init-hook 'global-company-mode)

;; To display a popup with the function description
;;(company-quickhelp-mode)
;;(setq company-quickhelp-delay .1)

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;;;;;;;;;;;   Golang configuration   ;;;;;;;;;
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Add yasnippet load-path and enbale it globally
;;(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20200604.246/")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)


