(defun REVL (lst)
 
 (cond
 
  ((null lst) nil)
 
  ((null (cdr lst)) lst)
 
  (t (list (REVL (cdr lst)) (car lst)))))


(print(REVL `(A B C)))