(defvar *guess-list* nil)
(defvar *last-guess* nil)

(defun enumerate (length colors)
  (cond ((equal 1 length)
         (loop for color in colors
	  collect (list color)))
        (t (loop for color in colors
	    append (mapcar (lambda (l) (cons color l))
		         (enumerate (- length 1) colors))))))

;; adaptation of "color-counter" function
(defmethod color-count (number list)
  (loop with tally = (make-array number :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

;; adaptation of "process-guess" function
(defmethod score-guess (answer guess num-colors)
  (loop with guess-color-count = (color-count num-colors guess)
     with true-color-count = (color-count num-colors answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- num-colors)
				    for guessed = (aref true-color-count i)
				    for true = (aref guess-color-count i)
				    when (<= true guessed)
				    sum true
				    else sum guessed)))))

(defun baseline-2-nilNewts (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let ((num-colors (length colors))
        (response))
    (cond ((not last-response)
	 (setf *guess-list* (enumerate board colors)))
	(t (setf response (firstn 2 last-response))
	   (setf *guess-list* (loop for guess in *guess-list*
			     for score = (score-guess *last-guess* guess num-colors)
			     when (equal score response)
			     collect guess))))
    (setf *last-guess* (first *guess-list*))
    (setf *guess-list* (rest *guess-list*))
    *last-guess*))
