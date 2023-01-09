(defun task (lst a x &optional (f (cdr lst)))
  (cond ((null lst) nil)
        ((eq (car lst) a) (rplacd f (cons x (cdr lst))) (task (cddr lst) a x (cdr lst)))
        (t (task (cdr lst) a x (cdr lst)))))
 
 
(setq *z* '(1 2 3 4 2 5 2))

(print *z*)
(task *z* 2 '!)

(print *z*)