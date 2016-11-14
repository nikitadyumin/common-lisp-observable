(defun observable (executor)
 (lambda (observer) 
   (funcall executor observer)))
   
(defun ones (next) 
	(funcall next 1)
	(funcall next 2)
	(funcall next 3)) 

(funcall (observable #'ones) #'write) 


(defun dbl (x) (* x 2))
 
(defun fmap (obs fn)
	(observable 
		(lambda (next)
			(funcall obs 
				(lambda (val) 
					(funcall next
						(funcall fn val)))))))
  
(funcall 
		(fmap 
			(observable #'ones) #'dbl) 
		#'write) 