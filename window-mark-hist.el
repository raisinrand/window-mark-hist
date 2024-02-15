(defvar window-mark-hist-max-size 128)

(defun window-mark-hist--limit-list-size (list max-size)
  "Limit the size of LIST to MAX-SIZE."
  (if (>= max-size 0)
      (if (> (length list) max-size)
          (nbutlast list (- (length list) max-size))
        list)
    (error "Invalid max-size: %s" max-size)))

(defun window-mark-hist--goto-marker (marker)
  (switch-to-buffer (marker-buffer marker) t t)
  (goto-char marker)
  (deactivate-mark))

(defun window-mark-hist-forward ()
  (interactive)
  (let ((unpop-to (pop (window-parameter nil 'mark-unpop-hist))))
    (cond
     ((null unpop-to)
      (progn
        (message "no more forward mark history")
        nil))
     ((null (marker-buffer unpop-to))
      (window-mark-hist-forward))
     (t
      (push (point-marker) (window-parameter nil 'window-mark-hist))
      (set-window-parameter
       nil 'window-mark-hist
       (window-mark-hist--limit-list-size
        (window-parameter nil 'window-mark-hist)
        window-mark-hist-max-size))
      (window-mark-hist--goto-marker unpop-to)
      (set-marker unpop-to nil)
      t))))

(defun window-mark-hist-back ()
  (interactive)
  (let ((pop-to (pop (window-parameter nil 'window-mark-hist))))
    (cond
     ((null pop-to)
      (progn
        (message "no more mark history")
        nil))
     ((null (marker-buffer pop-to))
      (window-mark-hist-back))
     (t
      (push (point-marker) (window-parameter nil 'mark-unpop-hist))
      (set-window-parameter
       nil 'mark-unpop-hist
       (window-mark-hist--limit-list-size
        (window-parameter nil 'mark-unpop-hist)
        window-mark-hist-max-size))
      (window-mark-hist--goto-marker pop-to)
      (set-marker pop-to nil)
      t))))

(defun window-mark-hist-back-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (while (and (window-mark-hist-back)
                (equal buffer (current-buffer))))))

(defun window-mark-hist-forward-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (while (and (window-mark-hist-forward)
                (equal buffer (current-buffer))))))

(defun window-mark-hist--push-mark (location)
  (let* ((window-mark-hist (window-parameter nil 'window-mark-hist))
         (last (car window-mark-hist)))
    (unless (and last
                 (equal (current-buffer) (marker-buffer last))
                 (equal (point) (marker-position last)))
      (setq marker (make-marker))
      (set-marker marker location)
      (push marker window-mark-hist)
      (set-window-parameter
       nil 'window-mark-hist
       (window-mark-hist--limit-list-size
        window-mark-hist window-mark-hist-max-size)))))

(defun window-mark-hist--push-mark-advice
    (orig-fun &optional location nomsg activate)
  (window-mark-hist--push-mark (or location (point)))
  (apply orig-fun (list location nomsg activate)))

(advice-add 'push-mark :around #'window-mark-hist--push-mark-advice)

(defun window-mark-hist--both-dirs-hist ()
  (let ((window-mark-hist (window-parameter nil 'window-mark-hist))
        (mark-unpop-hist (window-parameter nil 'mark-unpop-hist)))
    (append window-mark-hist mark-unpop-hist)))


(defun window-mark-hist-make-push-mark-advice (orig-fun &rest args)
  "Advice function to push the global mark before calling the original function."
  (push-mark)
  (apply orig-fun args))

(defun window-mark-hist-split-window-advice (orig-fun &rest args)
  (let ((window-mark-hist (window-parameter nil 'window-mark-hist))
        (mark-unpop-hist (window-parameter nil 'mark-unpop-hist)))
    (let ((result (apply orig-fun args)))
      (set-window-parameter result 'window-mark-hist window-mark-hist)
      (set-window-parameter result 'mark-unpop-hist mark-unpop-hist)
      result)))
(advice-add
 #'split-window
 :around #'window-mark-hist-split-window-advice)


(provide 'window-mark-hist)
