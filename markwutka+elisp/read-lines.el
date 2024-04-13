(defun read-lines (file)
  (interactive)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines '()))
      (while (re-search-forward "\\(.*\\)[\n]" nil t)
	(setq lines (cons (match-string 1) lines)))
      (reverse lines))))

