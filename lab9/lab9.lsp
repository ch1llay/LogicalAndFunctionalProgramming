(defun task (lst)
 
 (cond
 
  ((null lst) nil)
 
  ((null (cdr lst)) lst)
 
  (t (list (task (cdr lst)) (car lst)))))


(print(REVL `(A B C)))