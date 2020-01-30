;;; amread-mode.el --- A minor mode helper user reading.

;;; Time-stamp: <2020-01-30 17:39:27 stardiviner>

;;; Commentary:

;;; Usage:
;;;
;;; 1. Launch amread-mode with command `amread-mode'.
;;; 2. Stop amread-mode by pressing [q].

;;; Code:

(defcustom amread-wps 3.0
  "Read words per second."
  :type 'float
  :safe #'floatp
  :group 'amread)

(defvar amread--running nil)
(defvar amread--overlay nil)
(defvar amread--current-position nil)

(defun amread--update ()
  "Moving amread cursor forward."
  (let* ((begin (point))
         (length (+ (skip-chars-forward "^\s\t\n—") (skip-chars-forward "—")))
         (end (point)))
    (if (eobp)
        (amread-stop)
      ;; create the overlay if does not exist
      (unless amread--overlay
        (setq amread--overlay (make-overlay begin end)))
      ;; move forward overlay
      (when amread--overlay
        ;; (delete-overlay amread--overlay)
        (move-overlay amread--overlay begin end))
      (setq amread--current-position (point))
      (overlay-put amread--overlay
                   'face '((foreground-color . "white")
                           (background-color . "dark green")))
      (skip-chars-forward "\s\t\n—"))))

(defun amread-start ()
  "Start / resume amread."
  (interactive)
  ;; resume from stopped position
  (if amread--current-position
      (goto-char amread--current-position)
    (goto-char 0))
  (setq amread--running
        (run-with-timer 0 (/ 1.0 amread-wps) #'amread--update)))

(defun amread-stop ()
  "Stop amread."
  (interactive)
  (prog1 amread--running
    (when amread--running
      (cancel-timer amread--running)
      (setq amread--running nil)
      (delete-overlay amread--overlay))))

(defun amread-pause-or-resume ()
  "Pause or resume amread."
  (interactive)
  (if amread--running
      (amread-stop)
    (amread-start)))

(defvar amread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'amread-mode)
    (define-key map (kbd "SPC") 'amread-pause-or-resume)
    (define-key map [remap keyaobrd-quit] 'amread-stop)
    map)
  "Keymap for amread-mode buffers.")

(define-minor-mode amread-mode
  "I'm reading mode."
  :init nil
  :keymap amread-mode-map
  (if amread-mode
      (amread-start)
    (amread-stop)))



(provide 'amread-mode)

;;; amread-mode.el ends here
