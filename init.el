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

;; Add melpa package source when using package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(package-initialize)

;; Load emacs packages and activate them
;; This must come brfore configurations of installed packages.
;; Don't delete this line.
(setq package-enable-at-startup nil) (package-initialize)

;; Activate EVIL MODE
(require 'evil)
  (evil-mode 1)

;; Enable EVIL MODE multi cursor.
(require 'evil-mc)
(global-evil-mc-mode  1)

;; Set cursor color
(set-cursor-color "#7FD6D6")

;; Enable column number
(column-number-mode 1)

;; Require neotree and set key bindings
(require 'neotree)
(global-set-key [f12] 'neotree-toggle)
(global-set-key [f5] 'neotree-toggle)

;;;;;;;;;;;   Golang configuration   ;;;;;;;;;

;; Golang indentation config
;;(add-to-list 'exec-path "/home/walidberrahaal/go/bin")
;;(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 8)
            (setq indent-tabs-mode 1)))

;; Golang autocomplete
(add-to-list 'load-path (expand-file-name "~/.emacs.d/go-autocomplete.el"))
(require 'go-autocomplete)
(require 'auto-complete-config)


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

;; Add yasnippet load-path and enbale it globally
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'js-mode)))

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
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 13)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; M-x enhancement for Emacs.
(require 'smex) 
(smex-initialize) 

;; Save emacs session
(desktop-save-mode 1)

;; Enable smartparens (auto pairs)
(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'html-mode-hook #'smartparens-mode)
(add-hook 'lb-datalog-mode-hook #'smartparens-mode)
(add-hook 'sh-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)

;; Set key binding to avy
(global-set-key (kbd "M-é") 'avy-goto-char)

;; Disable this annoying thing :\ No newline at end of file
(setq require-final-newline nil)

;; Enable projectile-mode (should be integrated with counsel via counsel-projectile library)
(projectile-mode 1)

;; Integrate projectile with counsel_projectile.
(counsel-projectile-on)

;; Disable backup files.
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Markdow live preview plugin
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

;; Improve cursor perform
(setq auto-window-vscroll nil)

;;;;;;;;;;;   Shell configuration   ;;;;;;;;;
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Enable auto-complete globally
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.5) ;; Setting 0.1 not 0.0 to avoid conflict with Yasnippet.
(set-face-background 'ac-candidate-face "#4f687a")
(set-face-foreground 'ac-candidate-face "#dae3ea")
(set-face-underline 'ac-candidate-face "#536b72")
(set-face-background 'ac-selection-face "steelblue")

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
 '(custom-safe-themes
   '("7f968c172d6ec46766773a8304c7570bdff45f1220d3700008a437d9529ca3e4" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
