(defvar *all-list* nil)
(defvar *current-list* nil)
(defvar *our-guess* nil)
(defvar *my-processed-guess* nil)

(defun enumerate (length colors)
  (cond ((equal 1 length)
     (loop for color in colors
           collect (list color)))
    (t (loop for color in colors
         append (mapcar (lambda (l) (cons color l))
	    (enumerate (- length 1) colors))))))

(defun avoids ()
  (loop for i from 0 to (- (length *all-list*) 1);loops through all permutations
       do (let (my-processed-guess (process-guess (nth i *all-list*))));get a score
	     (if (> (second my-processed-guess) 0);if score not equal 0
		 (push (nth i *all-list*) *current-list*))));push into new list
	  

(defun Baseline2 (board colors SCSA last-response)
	(declare (ignore SCSA))
	(COND
		((equal last-response nil)
		 (setf *all-list* (enumerate colors board))
		 (avoids)
		 (setf *our-guess* (first *current-list*)))))
		 
