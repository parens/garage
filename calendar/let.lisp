(defmacro let1 (x e1 e2)
  (list (list 'lambda (list x) e2) e1))

(defmacro letn (bindings body)
  (let ((vars (mapcar (function car) bindings))
        (vals (mapcar (function cadr) bindings)))
    (cons (list 'lambda vars body) vals)))

(defmacro letn* (bindings body)
  (reduce (lambda (binding e2)
            (let ((x (car binding))
                  (e1 (cadr binding)))
              (list (list 'lambda (list x) e2) e1)))
          bindings
          :from-end t :initial-value body))
