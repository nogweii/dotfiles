; toggle MPD
(xbindkey '("c:162") "mpc toggle")
(xbindkey '("c:144") "mpc prev")
(xbindkey '("c:153") "mpc next")

;(xbindkey '(Mod4 F2) "gmrun")

(xbindkey '(Mod4 Return) "urxvt.sh")

(xbindkey '("c:178") "open-browser")

;; Super-R starts keynav, a keyboard based mouse mover. Awesome, right?!?
(xbindkey '(Mod4 r) "keynav 'start, grid 2x2'")

; Start an application
(xbindkey '(Mod4 p) "dmenu_run")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start of xbindkeys guile configuration help ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This configuration is guile based.
;;   http://www.gnu.org/software/guile/guile.html
;; any functions that work in guile will work here.
;; see EXTRA FUNCTIONS:

;; Version: 1.8.2

;; If you edit this file, do not forget to uncomment any lines
;; that you change.
;; The semicolon(;) symbol may be used anywhere for comments.

;; To specify a key, you can use 'xbindkeys --key' or
;; 'xbindkeys --multikey' and put one of the two lines in this file.

;; A list of keys is in /usr/include/X11/keysym.h and in
;; /usr/include/X11/keysymdef.h
;; The XK_ is not needed.

;; List of modifier:
;;   Release, Control, Shift, Mod1 (Alt), Mod2 (NumLock),
;;   Mod3 (CapsLock), Mod4, Mod5 (Scroll).

;; xbindkeys reloads its configuration file each time it has changed.  But if
;; you want to force it reloading its configuration file, you can send a HUP
;; signal:
;;     killall -HUP xbindkeys
;; When the HUP signal is sent to xbindkeys, changes in $HOME/.xbindkeysrc only
;; take effect after moving mouse.

;; The release modifier is not a standard X modifier, but you can
;; use it if you want to catch release instead of press events

;; By defaults, xbindkeys does not pay attention to modifiers
;; NumLock, CapsLock and ScrollLock.
;; Uncomment the lines below if you want to use them.
;; To dissable them, call the functions with #f


;;;;EXTRA FUNCTIONS: Enable numlock, scrolllock or capslock usage
;;(set-numlock! #t)
;;(set-scrolllock! #t)
;;(set-capslock! #t)

;;;;; Scheme API reference
;;;;
;; Optional modifier state:
;; (set-numlock! #f or #t)
;; (set-scrolllock! #f or #t)
;; (set-capslock! #f or #t)
;; 
;; Shell command key:
;; (xbindkey key "foo-bar-command [args]")
;; (xbindkey '(modifier* key) "foo-bar-command [args]")
;; 
;; Scheme function key:
;; (xbindkey-function key function-name-or-lambda-function)
;; (xbindkey-function '(modifier* key) function-name-or-lambda-function)
;; 
;; Other functions:
;; (remove-xbindkey key)
;; (run-command "foo-bar-command [args]")
;; (grab-all-keys)
;; (ungrab-all-keys)
;; (remove-all-keys)
;; (debug)


;; Examples of commands:

;(xbindkey '(control shift q) "xbindkeys_show")

;; set directly keycode (here control + f with my keyboard)
;(xbindkey '("m:0x4" "c:41") "xterm")

;; specify a mouse button
;(xbindkey '(control "b:2") "xterm")

;;(xbindkey '(shift mod2 alt s) "xterm -geom 50x20+20+20")

;; set directly keycode (control+alt+mod2 + f with my keyboard)
;(xbindkey '(alt "m:4" mod2 "c:0x29") "xterm")

;; Control+Shift+a  release event starts rxvt
;;(xbindkey '(release control shift a) "rxvt")

;; Control + mouse button 2 release event starts rxvt
;;(xbindkey '(releace control "b:2") "rxvt")

;; Extra features
;(xbindkey-function '(control a)
;		   (lambda ()
;		     (display "Hello from Scheme!")
;		     (newline)))

;(xbindkey-function '(shift p)
;		   (lambda ()
;		     (run-command "xterm")))


;; Double click test
;(xbindkey-function '(control w)
;		   (let ((count 0))
;		     (lambda ()
;		       (set! count (+ count 1))
;		       (if (> count 1)
;			   (begin
;			    (set! count 0)
;			    (run-command "xterm"))))))

;; Time double click test:
;;  - short double click -> run an xterm
;;  - long  double click -> run an rxvt
;(xbindkey-function '(shift w)
;		   (let ((time (current-time))
;			 (count 0))
;		     (lambda ()
;		       (set! count (+ count 1))
;		       (if (> count 1)
;			   (begin
;			    (if (< (- (current-time) time) 1)
;				(run-command "xterm")
;				(run-command "rxvt"))
;			    (set! count 0)))
;		       (set! time (current-time)))))


;; Chording keys test: Start differents program if only one key is
;; pressed or another if two keys are pressed.
;; If key1 is pressed start cmd-k1
;; If key2 is pressed start cmd-k2
;; If both are pressed start cmd-k1-k2 or cmd-k2-k1 following the
;;   release order

;; Example:
;;   Shift + b:1                   start an xterm
;;   Shift + b:3                   start an rxvt
;;   Shift + b:1 then Shift + b:3  start gv
;;   Shift + b:3 then Shift + b:1  start xpdf

;(define-chord-keys '(shift "b:1") '(shift "b:3")
;  "xterm" "rxvt" "gv" "xpdf")

;; Here the release order have no importance
;; (the same program is started in both case)
;(define-chord-keys '(alt "b:1") '(alt "b:3")
;  "gv" "xpdf" "xterm" "xterm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of xbindkeys guile configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vim: set ts=8 sw=4 tw=0 syn=scheme nospell:
