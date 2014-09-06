;;; tomatinho.el --- Tomatinho

;; Author: Konrad Scorciapino <konr@konr.mobi>
;; Keywords: time, productivity, pomodoro technique
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tomatinho is a simple and beautiful [[http://www.pomodorotechnique.com/][pomodoro technique]] timer that
;; runs on Emacs and is not bloated with distractive graphics or inorganic
;; commands. Just press Enter, see time flow and do you best.

;;; Code:

(require 'cl)


;;; Customs
(defgroup tomatinho nil
  "Customs for `tomatinho' !"
  :group 'productivity)

(defcustom tomatinho-buffer-name "Tomatinho!"
  "Name of the Tomatinho buffer"
  :type 'string :group 'tomatinho)

(defvaralias 'tomatinho-buffer 'tomatinho-buffer-name)
;; ¤note: might be preferable to refactor the code using this name

(defcustom tomatinho-bar-length 25
  "Length of a podomoro bar in tubes mode"
  :type 'integer :group 'tomatinho)

(defcustom tomatinho-pomodoro-length 25
  "Time length of a Podomoro round."
  :type 'integer :group 'tomatinho)

(defvar tomatinho-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'tomatinho-interactive-kill-buffer)
    (define-key map (kbd "Q") 'tomatinho-interactive-quit)
    (define-key map (kbd "R") 'tomatinho-interactive-reset)
    (define-key map (kbd "<return>") 'tomatinho-interactive-new-pomodoro)
    (define-key map (kbd "S-<return>") 'tomatinho-interactive-deliberate-pause)
    (define-key map (kbd "<tab>") 'tomatinho-interactive-toggle-display)
    map))

;;; Pending custom
(defvar tomatinho-format "%H:%M:%S"
  "Time format for podomoro clock") ; Would be unsafe to make a custom string
(defvar tomatinho-dir (file-name-directory (or load-file-name buffer-file-name))
  "Tomatinho directory in wich sounds ar store")
;;§later: add more customization
(defvar tomatinho-sound-tick (expand-file-name (concat tomatinho-dir "tick.wav"))
  "Tick sound during a pomoro run.")
(defvar tomatinho-sound-tack (expand-file-name (concat tomatinho-dir "tack.wav"))
  "Tack sound during a break.")


;;; Faces
;; §later: classes for dark/light!
(defface tomatinho-time-face
  '(( t ( :family "DejaVu Sans" :height 6.0 :width semi-condensed))) ;; §see height was 888 §see unit!!
  ;; §note: when floating point, is relative height compared to parent
  "Tomatinho face for Clock"
  :group 'tomatinho)
;; §note: for inheritance, add :inherit tomatinho-time-face

(defface tomatinho-ok-face
  '((t (:foreground "#ff0000")))
  "Tomatinho face for valid tomatinho run"
  :group 'tomatinho)

(defface tomatinho-pause-face
  '((t (:foreground "#00ff00")))
  "Tomatinho face for paused tomatinho"
  :group 'tomatinho)

(defface tomatinho-reset-face
  '(( t (:foreground "#333333")))
  "Tomatinho face for reseted tomatinho"
  :group 'tomatinho)

(defface tomatinho-current-ok-face
  '((t (:height 2.5 :inherit tomatinho-ok-face)))
    "Tomatinho face for current tomatinho"
    :group 'tomatinho)

(defface tomatinho-current-pause-face
  '((t (:height 2.5 :inherit tomatinho-pause-face)))
    "Tomatinho face for current pause"
    :group 'tomatinho)

;;; Vars
(defvar tomatinho-timer nil
  "Tomatinho timer.")
(defvar tomatinho-events nil
  "Tomatinho event List")
(defvar tomatinho-current '(ok . 0)
  "Tomatinho current event.  'Initial value: all fine at the beggining'")
(defvar tomatinho-last 0
  "Tomatinho Last timestamp value")
(defvar tomatinho-debug nil
  "Tomatinho debugging switch")
(defvar tomatinho-display-tubes t
  "Tomatinho displaying mode, tubes rather than text.")
;; §maybe: introduce a prefered mode.


;;;;;;;;;;;;;;;;;
;; Interactive ;;
;;;;;;;;;;;;;;;;;

(defun tomatinho-interactive-deliberate-pause ()
  "Pause deliberately"
  (interactive)
  (let ((event (if (equal (car tomatinho-current) 'pause) tomatinho-current
                 (cons 'reset (cdr tomatinho-current)))))
    (tomatinho-register-event event '(pause . 0)))
  (play-sound-file-async tomatinho-sound-tick))

(defun tomatinho-interactive-kill-buffer ()
  "Kills the buffer."
  (interactive)
  (kill-current-buffer))

(defun tomatinho-interactive-new-pomodoro ()
  "Forgoes the current pomodoro or leaves a break."
  (interactive)
  (let ((event (if (equal (car tomatinho-current) 'pause) tomatinho-current
                 (cons 'reset (cdr tomatinho-current)))))
    (tomatinho-register-event event '(ok . 0)))
  (play-sound-file-async tomatinho-sound-tick))

(defun tomatinho-interactive-reset ()
  "Resets the timer."
  (interactive)
  (if (y-or-n-p "Are you sure you want to reset the timer? ")
      (progn (setq tomatinho-current '(ok . 0)
		   tomatinho-events nil
                   tomatinho-last (timestamp))
             (play-sound-file-async tomatinho-sound-tick))
    (message "Pfew! That was close!")))

(defun tomatinho-interactive-toggle-display ()
  "Toggles between display modes."
  (interactive)
  (setq tomatinho-display-tubes (not tomatinho-display-tubes))
  (tomatinho-update))

(defun tomatinho-interactive-quit ()
  "Turns off Tomatinho."
  (interactive)
  (if (y-or-n-p "Are you sure you want to turn off Tomatinho? ")
      (progn (cancel-timer tomatinho-timer)
             (kill-current-buffer)
             (tomatinho-set-events nil '(ok . 0)))
    (message "Pfew! That was close!")))


;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defun timestamp ()
  "Returns the timestamp as an integer."
  (string-to-int (format-time-string "%s")))

(defun play-sound-file-async (file)
  "Plys with some overhead, but at least doesn't freeze Emacs."
  (let ((command (car command-line-args)))
    (start-process "play-sound-file-async" nil command "-Q" "--batch" "--eval"
                   (format "(play-sound-file \"%s\")" file))))

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defmacro unlocking-buffer (&rest body)
  "Macro that allows safer manipulation of a read-only buffer."
  `(progn (toggle-read-only -1)
          ,@body
          (toggle-read-only 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display and updates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tomatinho-set-events (events new-status)
  "Sets both the event history and the current status"
  (setq tomatinho-events events
        tomatinho-current new-status
        tomatinho-last   (timestamp)))

(defun tomatinho-register-event (event new-status)
  "Appends to the event list and and sets the status"
  (tomatinho-set-events (append tomatinho-events (list event)) new-status))

(defun tomatinho-tubes-string (cons i)
  "Auxiliary function to display the tubes correctly."
  (let* ((type (car cons)) (amount (cdr cons))
         (length (ceiling (/ (* 1.0 amount tomatinho-bar-length) tomatinho-pomodoro-length)))
         (text (make-string length ?░))
         (text (if (not (equal type 'reset)) text
                 (concat text (make-string (- tomatinho-bar-length length) ?_))))
         (text (if (equal type 'pause) text (format "\n%d. %s" i text))))
    (propertize text 'font-lock-face
                (case type
                  (ok 'tomatinho-ok-face)
		  (reset 'tomatinho-reset-face)
                  (pause 'tomatinho-pause-face)
		  (t nil)))))

(defun tomatinho-display-tubes ()
  "Displays the pomodoros done so far as a series of tubes."
  (let ((i 1))
    (dolist (item (append tomatinho-events (list tomatinho-current)))
      (insert (tomatinho-tubes-string item i))
      (unless (equal (car item) 'pause)
        (when (equal (car item) 'ok)
	  (setq i (1+ i))))))
  (insert (propertize "→\n\n" 'font-lock-face '(:weight bold)))
  (loop for item in tomatinho-events
        and extra = (if (equal (car tomatinho-current) 'ok) (cdr tomatinho-current) 0)
        when (equal (car item) 'ok) sum (cdr item) into ok
        when (equal (car item) 'reset) sum (cdr item) into reset
        when (equal (car item) 'pause) sum (cdr item) into pause
        finally (insert (format "Currently using %.2f%% of your time in full pomodoros."
                                (/ (+ ok (or extra (cdr tomatinho-current))) 0.01
                                   (+ 1e-20 ok reset pause (cdr tomatinho-current)))))))

(defun tomatinho-display-history ()
  "Displays the pomodoros done so far as a history log."
  (let ((i 0))
    (dolist (item tomatinho-events)
      (when (equal (car item) 'ok) (setq i (1+ i)))
      (let* ((type (car item)) (val (cdr item))
             (number (format "%d. " i))
             (number (if (equal type 'ok) number (make-string (length number) ? )))
             (m-ok (format "Completed a pomodoro with %d minute%s\n" val (if (> val 1) "s" "")))
             (m-reset (format "Gave up after %d minute%s\n" val (if (> val 1) "s" "")))
             (m-pause (format "Had a break of %d minute%s\n" val (if (> val 1) "s" "")))
             (message (case type
                        (ok (propertize m-ok 'font-lock-face 'tomatinho-ok-face))
                        (reset (propertize m-reset 'font-lock-face 'tomatinho-reset-face))
                        (pause (propertize m-pause 'font-lock-face 'tomatinho-pause-face)))))
        (insert (concat number message)))))
  (let ((type (car tomatinho-current)) (val (cdr tomatinho-current))
        (diff (- (timestamp) tomatinho-last)))
    (insert (propertize (format "%d:%02d %s"  val diff (if (equal type 'ok) "pomodoro" "break"))
                        'font-lock-face
                        (if (equal type 'ok) 'tomatinho-current-ok-face 'tomatinho-current-pause-face)))))
;; §later: refactor to not redraw everything all the time.

(defun tomatinho-update ()
  "First updates the variables and then the buffer, if it exists."
  (let ((time (timestamp))
	(type (car tomatinho-current))
	(val (cdr tomatinho-current))
        (tick nil) ;; MXE was here. Instead of:  (tick tomatinho-sound-tick)
        (tack tomatinho-sound-tack))
    (when (>= (- time tomatinho-last) (if tomatinho-debug 0 60))
      (setq tomatinho-current (cons type (1+ val)) tomatinho-last time)
      (when (and (equal type 'ok)
		 (>= (1+ val) tomatinho-pomodoro-length))
        (setq tomatinho-events (append tomatinho-events `((ok . ,tomatinho-pomodoro-length)))
              tomatinho-current '(pause . 0)))
      (play-sound-file-async (if (equal (car tomatinho-current) 'ok) tick tack))))
  (when (get-buffer tomatinho-buffer)
    (with-current-buffer (get-buffer tomatinho-buffer)
      (unlocking-buffer
       (delete-region (point-min) (point-max))
       ;; §note: redraw buffer each time.
       (setq buffer-undo-tree nil)
       (insert (propertize (format-time-string tomatinho-format)
                           'font-lock-face 'tomatinho-time-face))
       (insert "\n")
       (if tomatinho-display-tubes
	   (tomatinho-display-tubes)
	 (tomatinho-display-history))))))

;;;;;;;;;;;;;;;;;;;
;; Main function ;;
;;;;;;;;;;;;;;;;;;;

(defun tomatinho ()
  "A simple and beautiful pomodoro technique timer."
  (interactive)
  (with-current-buffer (get-buffer-create tomatinho-buffer)
    (use-local-map tomatinho-map)
    (font-lock-mode t))
  (setq tomatinho-last (timestamp))
  (tomatinho-update)
  (when tomatinho-timer (cancel-timer tomatinho-timer))
  (setq tomatinho-timer (run-at-time nil 1 'tomatinho-update))
  (switch-to-buffer tomatinho-buffer))


(provide 'tomatinho)
;;; tomatinho.el ends here
