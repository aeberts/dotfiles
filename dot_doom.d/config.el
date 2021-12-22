;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alexander Eberts"
      user-mail-address "alex.eberts@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono" :size 13))

;; Change local leader key to , instead of SPC m
;; we lose the evil-snipe-backwards-f but I don't use it anyway
(setq doom-localleader-key ",")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(use-package doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; (load-theme 'leuven t) ; my main light theme
  ;; (load-theme 'leuven-dark t) ; dark version of leuven (not my favourite)
  (setq doom-theme 'doom-one) ; my main dark theme
  ;; (setq doom-theme 'doom-spacegrey)
  ;; (setq doom-theme 'doom-wilmersdorf)
  ;; (setq doom-theme 'doom-palenight)
  ;; (setq doom-theme 'doom-tomorrow-day)
  ;; (setq doom-theme 'doom-one-light)
  ;; (setq doom-theme 'spacemacs-light)
  ;; (setq doom-theme 'cyberpunk-theme)
  ;; (load-theme 'adwaita t)            ; buit in light theme
  ;;
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Base Functions and Navigation

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t);

;; evil-escape allows you to type chorded key sequences to quit
(after! evil-escape
  (setq evil-escape-key-sequence "fd")
  (setq evil-escape-unordered-key-sequence t))

;; Custom Keybindings

;; Expand Region
(map!
 :nv "C-+" #'er/contract-region
 :nv "C-=" #'er/expand-region
 :nv "C-z" #'evil-undo)

;; Map C-- to expand region because I always miss it.
(map! :nv "C--" #'er/expand-region)

;; My Custom Functions

;; Adapted from
;; https://github.com/abo-abo/avy/issues/89

(defun ae/avy-goto-char-in-defun (char)
  "Jump to a character in the defun at point."
  (interactive "cchar: ")
  (let ((beg nil) (end nil))
    (save-excursion
      (beginning-of-defun)
      (setq beg (point))
      (end-of-defun)
      (setq end (point)))
    (avy--generic-jump (string char) nil beg end)))

(defun ae/avy-goto-paren-in-defun ()
  "Jump to an open paren in the defun at point."
  (interactive)
  (let ((beg nil) (end nil))
    (save-excursion
      (beginning-of-defun)
      (setq beg (point))
      (end-of-defun)
      (setq end (point)))
    (avy--generic-jump (regexp-quote "(" ) nil beg end)))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;; From https://stackoverflow.com/questions/10681766/emacs-org-mode-textual-reference-to-a-fileline
;; Copy file and position reference to the kill ring
(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
   for the current buffer's file name, and the line number at point."
  (interactive)
  (kill-new
   (format "%s:%d" (buffer-file-name) (save-restriction
                                        (widen) (line-number-at-pos)))))
;;
;; From https://stackoverflow.com/questions/2926088/how-to-indent-a-buffer-in-ess
;; Re-indent whole buffer
(defun z-indent-buffer()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun flush-blank-lines (start end)
  "This will flush blank lines and works with a region.

URL `https://masteringemacs.com/article/removing-blank-lines-buffer'
"
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

;; Set the initial frame size
(setq initial-frame-alist '((top . 1) (left . 1) (width . 157) (height . 81)))
