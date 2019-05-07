
(defvar *rao-kb* (list))
(defvar *being-fixed* nil)
(defvar *being-considered* nil)

;;;******************************************************************************
;;; Helper Functions (i) - modifying KB
;;;******************************************************************************

;;reset global variables for next game
(defun reset-rao ()
  (setf *rao-kb* (list))
  (setf *being-fixed* nil)
  (setf *being-considered* nil)
  (setf *guesses* (list)))

;;add entry of the form (color 1 2 ... board)
(defun add-kb-entry (board color)
  (let ((kb-entry (list)))
    (loop for i from board downto 1
       do (setf kb-entry (cons i kb-entry)))
    (setf *rao-kb* (append *rao-kb* (list (cons color kb-entry))))))

;;;******************************************************************************
;;; Helper Functions (ii) - getnext
;;;******************************************************************************

;;returns color tied to position pos
(defun tied (pos)
  (let ((itscolor))
    (loop for entry in *rao-kb*
       when (and (= (length entry) 2) (= (second entry) pos))
       do (setf itscolor (first entry))
       until itscolor)
    itscolor))

;;returns the next possible position to try for a color
(defun nextpos (color)
  (let ((next))
    (loop for entry in *rao-kb*
       when (and (eq (first entry) color) (> (length entry) 2)) ;not fixed
       do (setf next (second entry))
       until next)
    next))

;;returns the second unfixed color
(defun secondunfixed ()
  (let ((first-unfixed)
        (second-unfixed))
    (loop for entry in *rao-kb*
       when (and first-unfixed (> (length entry) 2))
       do (setf second-unfixed (nth 0 entry)) ;set the second unfixed color
       when (and (not first-unfixed) (> (length entry) 2))
       do (setf first-unfixed (nth 0 entry)) ;set the first unfixed color
       until second-unfixed)
    second-unfixed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions (iii) - Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;adds gain # of copies of entries with header *being-considered* to KB
(defun addlists (board gain)
  (loop for i from 1 to gain
     do (add-kb-entry board *being-considered*)))

;;fix the current *being-fixed*
(defun fix ()
  (let ((NN (1- (length *rao-kb*)))
        (entry)
        (done)) ;boolean
    (loop for n from 0 to NN
       do (setf entry (nth n *rao-kb*))
       when (and (eq (first entry) *being-fixed*)
	       (> (length entry) 2)) ;dont fix an already fixed entry
       do (setf (nth n *rao-kb*) (subseq entry 0 2))
       and do (setf done T)
       until done)
    (cleanup)))

;;fix color i in current position of color j
(defun fix-1 (i j)
  (let ((fixpos (nextpos j))
        (NN (1- (length *rao-kb*)))
        (entry)
        (done)) ;boolean
    (loop for n from 0 to NN
       do (setf entry (nth n *rao-kb*))
       when (and (eq (first entry) i)
	       (> (length entry) 2)) ;find entry for i
       do (setf (nth n *rao-kb*) (list i fixpos))
       and do (setf done T)
       until done)))

;;delete the current position of color i from KB-entry for color j
(defun del (i j)
  (let ((deletepos (nextpos i))
        (NN (1- (length *rao-kb*)))
        (done) ;boolean
        (entry))
    (loop for n from 0 to NN
       do (setf entry (nth n *rao-kb*))
       when (and (eq (first entry) j)
	       (> (length entry) 2)) ;find entry for j
       do (setf (nth n *rao-kb*) (remove deletepos entry))
       and do (setf done T)
       until done)))

;;sets the value for *being-fixed*
(defun bump ()
  (let ((NN (1- (length *rao-kb*)))
        (done) ;boolean
        (entry))
    (loop for n from 0 to NN
       do (setf entry (nth n *rao-kb*))
       when (> (length entry) 2)
       do (setf *being-fixed* (first entry))
       and do (setf done T)
       until done)))

;;set the value for *being-considered*
(defun nextcolor (board colors)
  (let ((next)
        (b-c-index))
    (if *being-considered*
        (setf b-c-index (1+ (spot *being-considered*))))
    (if (< (length *rao-kb*) board)
        (if (= (numguesses) 0)
	  (setf next 'a) ;initial value
	  (if (< b-c-index (length colors)) ;this should be redundant
	      (setf next (nth b-c-index colors))
	      (setf next *being-considered*)))
        (setf next (secondunfixed))) ;length of board
    (setf *being-considered* next)))

;;clean up KB
(defun cleanup ()
  (let ((fixedpos) ;a tied position
        (NN (1- (length *rao-kb*)))
        (fixedind) ;index of item in KB which is tied
        (entry))
    (loop for n from 0 to NN
       do (setf entry (nth n *rao-kb*))
       when (= (length entry) 2)
       do (setf fixedpos (second entry))
       and do (setf fixedind n)
       and do (loop for m from 0 to NN
	       do (setf entry (nth m *rao-kb*))
	       when (/= fixedind m)
	       do (setf (nth m *rao-kb*) (remove fixedpos entry))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions (iv) - Rao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;number of KB entries which have been fixed to a position
(defun numfix ()
  (let ((fix-count 0))
    (loop for entry in *rao-kb*
       when (= (length entry) 2)
       do (setf fix-count (1+ fix-count)))
    fix-count))

;;number of guesses made
(defun numguesses ()
  (length *guesses*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GetNext
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getnext (board)
  (let ((nextguess)
        (itscolor)
        (gi)) ;ith entry of nextguess
    (loop for i from 1 to board
       do (setf itscolor (tied i))
       when itscolor
       do (setf gi itscolor)
       else when (eq i (nextpos *being-fixed*))
       do (setf gi *being-fixed*)
       else when (= (length *rao-kb*) board)
       do (setf gi (secondunfixed))
       else
       do (setf gi *being-considered*)
       do (setf nextguess (append nextguess (list gi))))
    nextguess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-rao-kb (board colors bulls cows)
  (let ((gain 0))
    (setf gain (- (+ bulls cows) (numfix)))
    (if *being-fixed* (setf gain (1- gain)))
    (if (> (+ gain (length *rao-kb*)) board) (setf gain (1- gain)))
    (if (< (length *rao-kb*) board) (addlists board gain))
    (case cows
      (0 (fix)
         (bump))
      (1 (if *being-fixed*
	   (del *being-fixed* *being-considered*))
         (del *being-fixed* *being-fixed*))
      (2 (fix-1 *being-considered* *being-fixed*))
      (T 'error))
    (cleanup)
    (nextcolor board colors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rao-player (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let* ((bulls (first last-response))
         (cows (second last-response))
         (guess))
    (cond ((null last-response)
	 (update-total-guesses)
	 (reset-rao)
	 (setf bulls 0)
	 (setf cows 0)))
    (update-rao-kb board colors bulls cows)
    (cleanup) ;need this again for some reason
    (setf guess (getnext board))
    (update-guesses guess)
    guess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;test your own sequence of guesses and responses below
(defun run-player ()
  (let ((resplist '(nil (0 0) (3 0) (3 0))))
    (reset-rao)
				;(Mastermind 5 5 'two-color-alternating)
    (setf (answer *Mastermind*) '(b c b c b))
    (loop for resp in resplist
       do (print resp)
       do (print (funcall 'rao-player 5 '(a b c d e) nil resp))
       do (print-global)
       do (terpri))))

(defun print-global ()
  (print *rao-kb*)
  (print *being-fixed*)
  (print *being-considered*))
