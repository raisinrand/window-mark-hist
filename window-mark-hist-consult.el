(require 'window-mark-hist)
(require 'consult)

(defun consult-window-mark-hist ()
  (interactive)
  (let ((all-window-mark-hist (window-mark-hist--both-dirs-hist)))
    (if all-window-mark-hist
        (consult-global-mark all-window-mark-hist))))


(provide 'window-mark-hist-consult)
