

(defun appendNegative (l resList)
    (if (eq (car l) nil) 
        (reverse resList)
        (if (< (car l) 0)
                (appendNegative (cdr l) (cons  (car l) resList) )
                (appendNegative (cdr l) resList )
            )
        
        )
    )
(defun appendNotNegative(l resList)
    (if (eq (car l) nil) 
        resList
        (if (>= (car l) 0)
                (appendNotNegative (cdr l) (cons  (car l) resList) )
                (appendNotNegative (cdr l) resList )
            )
        
        )
    )
  
(defun changeList (l)
    (append 
            (appendNegative l `()) 
            (appendNotNegative l `()))
 )
        
    


(print (changeList `(-1 -2 -5 3, 2, 1) ))
