(defun S (f)
  (lambda (g)
    (lambda (x)
      (funcall (funcall f x) (funcall g x)))))

(defun K (x)
  (lambda (y)
    (declare (ignore y))
    x))

(defun U (f)
  (funcall (funcall f (function S)) (function K)))

(defvar I (U (function U)))

(defvar S2 (U (U (U (U (function U))))))
(defvar K2 (U (U (U (U (U (function U)))))))

(defun Y (f)
  (funcall
   (funcall
    (lambda (x)
      (funcall x x))
    (lambda (Y)
      (lambda (f)
        (lambda (x)
          (funcall
           (funcall f (funcall (funcall Y Y) f))
                    x)))))
   f))

(defun fib (x)
  (funcall
   (Y (lambda (rec) (lambda (x)
        (if (= x 0)
            1
          (* x (funcall rec (- x 1)))))))
   x))
