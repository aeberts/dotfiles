;; Emacs startup is configured with the Chemacs2 bootloader
;; https://https://github.com/plexus/chemacs2
;; 
;; Select which profile to use by editing the file ~/.emacs-profile
;; 
;; $ echo 'doom' > ~/.emacs-profile
;; $ echo 'spacemacs' > ~/.emacs-profile
;;
(
("default" . ((user-emacs-directory . "~/.doom-emacs")
	      (env . (("DOOMDIR" . "~/.doom.d")))))

("doom" . ((user-emacs-directory . "~/.doom-emacs")
	      (env . (("DOOMDIR" . "~/.doom.d")))))

("spacemacs" . ((user-emacs-directory . "~/.spacemacs-emacs")
	      (env . (("SPACEMACSDIR" . "~/.spacemacs.d")))))

 ("corgi" . ((user-emacs-directory . "~/.corgi-emacs")))

 ("vanilla" . ((user-emacs-directory . "~/.emacs.default")))
 )
