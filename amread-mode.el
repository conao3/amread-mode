;;; amread-mode.el --- A minor mode helper user reading.

;;; Time-stamp: <2020-01-30 16:37:07 stardiviner>

;;; Commentary:

;;; Usage:
;;;
;;; 1. Launch amread-mode with command `amread-mode'.
;;; 2. Stop amread-mode by pressing [q].

;;; Code:

(defcustom amread-wps 2
  "Read words per second."
  :type 'number
  :safe #'numberp
  :group 'amread)

(defvar amread--running nil)
(defvar amread--overlay nil)

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
      (overlay-put amread--overlay
                   'face '((foreground-color . "white")
                           (background-color . "dark green")))
      (skip-chars-forward "\s\t\n—"))))

(defun amread-start ()
  "Start / resume amread."
  (interactive)
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

(defvar amread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'amread-stop)
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
