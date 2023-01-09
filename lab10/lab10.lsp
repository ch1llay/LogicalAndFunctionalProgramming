(defun task (lst chooseEl newEl &optional (remainLst (cdr lst)))
  (cond ((null lst) nil)
        ((eq (car lst) chooseEl)
         (rplacd remainLst (cons newEl (cdr lst))) (task (cddr lst) chooseEl newEl (cdr lst)))
         (t (task (cdr lst) chooseEl newEl (cdr lst))))
    )
 
 
(setq *z* '(1 2 3 4 2 5 2))

(trace task)

(print *z*)
(task *z* 2 '!)


(print *z*)