(defvar *enumerated-list*)
(defvar *viable-list*)
(defvar *guess-list* '())
(defvar *enumerated-peg-scores* '((0 0) (0 1) (0 2) (0 3) (0 4) (1 0) (1 1) (1 2) (1 3) (2 0) (2 1) (2 2) (3 0) (4 0))) 

(defun enumerate (length colors)
  (cond ((equal 1 length)
     (loop for color in colors
           collect (list color)))
    (t (loop for color in colors
         append (mapcar (lambda (l) (cons color l))
                        (enumerate (- length 1) colors))))))

(defun generate-first-guess (board colors)
    (loop for i from 1 to board
        when(<= i (/ board 2))
            collect (first colors)
        else 
            collect (second colors)))

;counts the number of each color in a guess into an array and returns the array
(defmethod internal-color-counter (colors list)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

;scores a guess, returning a two-element list (#exact #other) where other means "right color, wrong location"
(defmethod internal-process-guess (colors answer guess)
  (loop
     with guess-color-count = (internal-color-counter colors guess)
     with true-color-count = (internal-color-counter colors answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (length colors))
					    for guessed = (aref true-color-count i)
					    for true = (aref guess-color-count i)
					    when (<= true guessed)
					    sum true
					    else sum guessed)))))

(defun eliminate-sequences (colors response)
    (loop for sequence in *viable-list*
        when (equal (internal-process-guess colors (first *guess-list*) sequence) response)
            collect (sequence)))

;; input-guess should be (first *guess-list*)
(defun knuth-function (board colors input-guess response)
    (cond ((endp input-guess) 
            (push input-guess *guess-list*)
            (print "here")(terpri)
            (setf input-guess (generate-first-guess board colors))
            (setf *enumerated-list* (enumerate board colors))
            (setf *viable-list* *enumerated-list*)
            (print input-guess)
            (print *viable-list*)
            (return-from knuth-function input-guess))
            
           (T (setf *viable-list* (eliminate-sequences colors response))
                (first *viable-list*))))

(defun nilNewts (board colors SCSA last-response)
    (cond ((null last-response) (setf *guess-list* (list))))
    (print (first *guess-list*))
    (setf guess-val (knuth-function board colors (first *guess-list*) last-response))
    (write guess-val)(terpri)
    (write *viable-list*)
    guess-val)