(require 'package)

;; Set up font and font size
(set-default-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 105)

;; Activate linum-mode and format it
(global-linum-mode t)
(setq linum-format " %3d  " )

;; Disable linum-mode for ansi-term
(add-hook 'term-mode-hook (lambda()
                (linum-mode -1)))

;; Hilights current line
(global-hl-line-mode 1)

;; Add melpa package source when using package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Load emacs packages and activate them
;; This must come brfore configurations of installed packages.
;; Don't delete this line.
(setq package-enable-at-startup nil) (package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (oceanic)))
 '(custom-safe-themes (quote ("db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "8ac2745fb5d9dad05f42228655508e14e4ce3a5adf64c9bedaa6e570a55f60be" "1d9def698a4c12a460d06c2594dde4bd46d128b2d0763324058cf4f09133c935" "71c35e02763f6a1752e9096f284ac81020f30e98891c233b5f8496a834c29b7a" "69168a8eaa2fbbe72440507efd43600018a2827326db7e04f89c79872fe0488a" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "962dacd99e5a99801ca7257f25be7be0cebc333ad07be97efd6ff59755e6148f" "66a92d07a0189facee51343718ed9add1211587295c845c47cf2febfa7568d21" "04ebeff60eb2b3e68646de93805c617d2ea9e57d9b03d6ad4b8c01d9ef3abc4a" "fd038d5fdcca50b77b0cb2f5fd55de92c749d8ecc249574b79074646b310d630" "b53e337e69739b2e5909b52b42c5ff279e85099d69b430e9a0c4141d5dafc17c" "76b9a5d0d1f4ac407cf3b7fb1f5368b06e1f78c1c0a4fb0776d5c3e2675c5ae9" "c128e5de037ae68997d74dae1630edd03285ac63d169f98d57ca2e398fd15b7e" "ecb278c933bf340a35a3472e8a31a993364a144d8a8b9923c401538c2ecae42a" "5f01eb7babe1f4adc8e27e9b601b8d080ecded1f8dd0d26fef6026fdc0e3ebf8" "6e8261fc03150ee1294c4d3e01226bf1cc063f518c0c39ad01d3076ef587e890" "ceb354e5e60a4f9f3889c59bff5c872c32c7baef6e655703a624104da17025cb" "e9d0e20fa961714e6c254466e7568090096f73b80a1c64ede8fb900cb2644ae9" "de92dd6d5e577090e01c1226f84eb41b292839b547c16746c9866885011c3aa0" "522fcd2c663938d11946b6f23c5552f126838d2f2ed44ea99946dfe0ea953213" "54609cb53cb71f158342adbf5ffdf899ba2932ce77fef82a3e2c81a2d8910a56" "a3bff9226b57f6ee827d51fad9f78ef26943067b598551ac96d1b9d18b5b39f4" "e0c1b1c551123b870689ce29ef0daf5bc65ec71932248b4f85a30e86ee59e5ce" "d8d4f544ec74f866cf2ae33acbf41dabce1bca2d2b9a7e4028a1b59dfdaeb579" "fcd04832f233dd17d903ef7b868d16581cd98d718965747b628fe0f5b8397a06" "1d7e67fe9d8deacf470ffb2c6ccb181ac5c1af580f9edbdba90e6e0f1ba56ace" "369cae4721535d842d113513a983192947ea6305c154558289454d5869132516" "6b20d669fcbcd79c6d0f3db36a71af1b88763246d3550a0c361866adecb38a9e" "12dd37432bb454355047c967db886769a6c60e638839405dad603176e2da366b" "1dbc7d9223a5292d103a480181c2edda71328f98c7d38e93cc9323f1a190ee8a" "1e264f9bbe5313b03eaec76f972b2354995f4296f4a83dcfea0a1d46ab4d9133" "d216ce6be247f256969344150fa774ccea618b4893fdad4b750ff21bd943788e" "d75bd9cc705fa30ba1727d2f22a8d22ec74debf294d74acf6321daecf172f903" "cac2d1d00e8783cbcd7c618f96526e3a7bf8db4f377539dbf60251e28167629d" "176ba0ea99d26556c99fb91fcf146922dd8206565567ba829dd5a21ae7cb9839" "c6f66646a17e4efd9f7ef094fab5ce8d6cc5a4389ef64f219584e96d45f22de6" "cab8b85d6d04db561f38ba1c1594d72ae41d5dea66298d7c1daaffccd51c1d72" "153ae92bc424bea6254c418bc819d234ef1e9832676a406d0238656d7212c0ef" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 105 :width normal))))
 '(mode-line ((t (:background "DarkOrange" :foreground "#0c0c0c" :box nil :weight normal :height 1.0))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 '(show-paren-match ((t (:background "burlywood")))))

;; Disable scrollbar mode
(scroll-bar-mode -1)

;; Enable powerline and setup theme  #EAE6E6 #80ff00 DarkOrange #3BCE7B OliveDrab3  powerline-evil-center-color-theme
(add-to-list 'load-path "~/.emacs.d/vendor/powerline")
(require 'powerline)
(powerline-evil-theme)

;; Enable column number
(column-number-mode 1)
 ;;'(powerline-evil-normal-face ((t (:foreground "#282828" :background "#33E220"))))
 ;;'(powerline-evil-insert-face ((t (:foreground "#282828" :background "#2167DF")))))

;; Require neotree and set key bindings
(require 'neotree)
(global-set-key [f12] 'neotree-toggle)
(global-set-key [f5] 'neotree-toggle)

;; Activate EVIL MODE
(require 'evil)
  (evil-mode 1)

;; Disable evil mode when in netotree
(add-hook 'neotree-mode-hook 'evil-emacs-state)


;; Hilights matching parenthesis and customization.
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-background 'show-paren-match "#ffc04d")
(set-face-foreground 'show-paren-match "#1B2B34")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)


;; Rule 80
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-column 80)
(setq fci-rule-color "#394C57")
;; Disable fci-mode for non required modes (buffers-messages, shell, ...)
(define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)

;; Disable fci-mode when in netotree
(add-hook 'neotree-mode-hook (lambda()
                (fci-mode -1)))

;; Disable fci-mode when in term mode
;;(add-hook 'term-mode-hook (lambda()
;;                (fci-mode -1)))


;; Avoid using this: Function to disdable fci-mode for all buffers
(defun disable-fci-mode-each-buffer ()
  (interactive)
  (mapc (lambda (buffer)
          (condition-case nil
              (with-current-buffer buffer
                (global-fci-mode -1))
            (buffer-read-only nil)))
	(buffer-list)))

;; Avoid using this: Function to enable fci-mode for all buffers
(defun enable-fci-mode-each-buffer ()
  (interactive)
  (mapc (lambda (buffer)
          (condition-case nil
              (with-current-buffer buffer
                (global-fci-mode 1))
            (buffer-read-only nil)))
	(buffer-list)))

;; Disable hl-line-mode when in org mode
(add-hook 'org-mode-hook (lambda ()
     (setq-local global-hl-line-mode
           nil)))

;; Disable hl-line-mode within ansi-term/eshell
(add-hook 'eshell-mode-hook (lambda ()
     (setq-local global-hl-line-mode
           nil)))
(add-hook 'term-mode-hook (lambda ()
     (setq-local global-hl-line-mode
           nil)))

;; Disable highlight line when VISUAL mode. NOTE: The snippet below needs to be fixed
;;(defvar-local was-hl-line-mode-on nil)
;;(defun hl-line-on-maybe ()  (if was-hl-line-mode-on (hl-line-mode +1)))
;;(defun hl-line-off-maybe () (if was-hl-line-mode-on (hl-line-mode -1)))
;;(add-hook 'hl-line-mode-hook 
;;  (lambda () (if hl-line-mode (setq was-hl-line-mode-on t)))))
;;
;;(add-hook 'evil-visual-state-entry-hook 'hl-line-off-maybe)
;;(add-hook 'evil-visual-state-exit-hook 'hl-line-on-maybe)

;;(add-hook 'evil-visual-state-entry-hook (lambda() (global-hl-line-mode -1)))
;;(add-hook 'evil-visual-state-exit-hook (lambda() (global-hl-line-mode +1)))


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


;; Disable yasnippet when in term mode
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;; Set the indentation for javascript files
(setq js-indent-level 2)

;; Set block selection background color
(set-face-attribute 'region nil :background "#5f6f7f")

;; Enable lb-datalog-mode in emacs
(require 'lb-datalog-mode)

;; Enable winner mode (fast window configuration)
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; Auto revert buffers when file changes on disk (useful for version controll)
(global-auto-revert-mode 1)

;; Enable evil surround mode.
(require 'evil-surround)
(global-evil-surround-mode 1)

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
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'html-mode-hook #'smartparens-mode)
(add-hook 'lb-datalog-mode-hook #'smartparens-mode)
(add-hook 'sh-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)

;; Enable flycheck mode
(add-hook 'js-mode-hook #'flycheck-mode)
;;(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
;;(global-flycheck-mode)

;; Set key binding to avy
(global-set-key (kbd "M-é") 'avy-goto-char)

;; Enable smooth-scrolling. (Scrolling like others ides)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 3)

;; Disable this annoying thing :\ No newline at end of file
(setq require-final-newline nil)

;; Enable projectile-mode (should be integrated with counsel via counsel-projectile library)
(projectile-mode 1)

;; Integrate projectile with counsel_projectile.
(counsel-projectile-on)

;; Disable backup files.
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Enable auto-complete globally
(global-auto-complete-mode t)

;; Markdow live preview plugin
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)
