

(defun appendNegative (l)
    (cond 
        (
         (eq (car l) nil) 
           '())
        (
         (< (car l) 0)
            (cons  (car l) (appendNegative (cdr l))))
         (t (appendNegative (cdr l)))
         )
    )

(defun appendNotNegative(l resList)
    (cond
        (
         (eq (car l) nil) 
            resList)
        (
         (>= (car l) 0)
                (appendNotNegative (cdr l) (cons  (car l) resList) )
         )
        
          (t (appendNotNegative (cdr l) resList ))
            )
        
        )

  
(defun changeList (l)
    (append 
            (appendNegative l) 
            (appendNotNegative l `()))
 )
        
    

(trace changeList)
(print (changeList `(-1 -2 -5 3, 2, 1) ))
