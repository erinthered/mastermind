(defvar *guess-list* nil)

(defun enumerate (length colors)
  (cond ((equal 1 length)
     (loop for color in colors
           collect (list color)))
    (t (loop for color in colors
         append (mapcar (lambda (l) (cons color l))
												(enumerate (- length 1) colors))))))

(defun nilNewts (board colors SCSA last-response)
  (declare (ignore SCSA))
	(let ((guess))
	(cond ((not last-response)
				 (setf *guess-list* (enumerate board colors)))
				(T (setf *guess-list* (rest *guess-list*))))
	(setf guess (first *guess-list*))))


;(Mastermind 4 6 'two-color-alternating)
;(play-tournament *Mastermind* 'nilNewts 'two-color-alternating 1)
