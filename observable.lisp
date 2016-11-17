(defun create-observable (executor)
 (lambda (observer) 
   (funcall executor observer)))
 
(defun fmap (obs fn)
	(create-observable 
		(lambda (next)
			(funcall obs 
				(lambda (val) 
					(funcall next
						(funcall fn val)))))))
  
		
(defun filter (obs pred)
	(create-observable 
		(lambda (next) 
			(funcall obs
				(lambda (val)
					(when 
						(funcall pred val) 
						(funcall next val)))))))

(defun even? (val)
	(eq (rem val 2) 0))

(defun dbl (x) (* x 2))

(defun nat (next) 
	(funcall next 1)
	(funcall next 2)
	(funcall next 3)
	(funcall next 4)
	(funcall next 5)) 
					
(funcall (create-observable #'nat) #'write) 
(write-line "")
(funcall 
		(fmap 
			(create-observable #'nat) #'dbl) 
		#'write) 
		
(write-line "")
(funcall 
		(filter 
			(create-observable #'nat) #'even?) 
		#'write) 
(write-line "")
(funcall 
		(fmap
			(filter 
				(create-observable #'nat) #'even?)
			#'dbl)
		#'write) 