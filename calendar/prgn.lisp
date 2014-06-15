(defun snd (x y)
  (declare (ignore x))
  y)

(defmacro progn* (&rest es)
  (reduce (lambda (e1 e2)
            (list 'snd e1 e2))
          es
          :initial-value nil))

(defmacro silly-progn (&rest es)
  (list 'funcall (cons 'lambda (cons '() es))))
