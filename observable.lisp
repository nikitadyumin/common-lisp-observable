(defun observable (executor)
 (lambda (observer) 
  (funcall observer 
   (funcall executor))))
   
(defun one () 1) 

(funcall (observable 
 (function one)) 
  (function write))


(defun dbl (x) (+ x 2))
 
(defun fmap (obs fn)
	(observable 
		(lambda ()
			(funcall obs 
				(lambda (val) (funcall fn val))))))
  
(funcall (fmap 
	(observable 
		(function one))
	(function dbl)) 
 (function write)) 