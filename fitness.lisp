;compares fitness score a with fitness score b.
(defun compare (a b)
  (> (first a) (first b))

;helps fitness function claculate fitness score. Needs work.
(defun eval-diff (guess answer)
  ()

;fitness function.
(defun calculate-fitness (code)
  (loop for n from 0 to (- 1 (length *guess-list*))
       sum (eval-diff code (nth n *guess-list*))))
