(load "pat-match.lisp")
(defun sec (x) (/ (cos x))) (defun cot (x) (/ (tan x))) (defun csc (x) (/ (sin x)))
(defun sech (x) (/ (cosh x))) (defun coth (x) (/ (tanh x))) (defun csch (x) (/ (sinh x)))
(defun lisp-opp (x)
	"Returns whether or not x is an operation that can be evaulated by LISP"
	(member x '(+ - * / expt sin cos tan exp sinh cosh tanh sec cot csc sech coth csch)))
(defun opp (x)
	"Returns whether or not x is an operation"
	(or (lisp-opp x) (pat-match '(d ?x) x) (pat-match '(i ?x) x) (member x '(log))))
(defun unaryp (x)
	"Returns whether or not x is a unary operation"
	;;log is the natural logarithm
	(member x '(sin cos tan log exp sinh cosh tanh sec cot csc sech coth csch)))
(defun commp (x)
	"Returns whether or not x is a communative operation"
	(member x '(+ *)))
(defun inopp (x)
	"Returns whether or not x is an infix operation"
	(member x '(+ - * /)))
(defun inv-op (x)
	"Returns the inverse operation of x"
	(cdr (assoc x '((+ . -) (- . +) (/ . *) (* . /) (log . exp) (exp . log)))))
(defun absorb-args (x)
	"Reformats expressions containing lists of args (Ex. (+ (1 2 3) 4) -> (+ 1 2 3 4)"
	(if (and (consp x) (opp (car x)))
		(cons (car x) (loop for arg in (cdr x) append
			(let ((arg (absorb-args arg)))
				(if (and (listp arg) (not (opp (car arg))))
					arg (list arg)))))
		x))
(defun contains (item tree)
	"Returns true iff item appears anywhere in tree"
	(cond	((equalp item tree) t)
			((consp tree) (or (contains item (car tree)) (contains item (cdr tree))))))
(defun evaluate (expr &optional (bindings nil))
	"Evaluates all numeric computations in an expression"
	(evaluate-helper (sublis bindings expr)))
(defun evaluate-helper (expr)
	(if (and (consp expr) (not (pat-match '(?or (?x) (expt ?x) (/ ?x)) expr)))
		(let ((operands (mapcar #'evaluate-helper (cdr expr))) (op (car expr)))
			(if (and (lisp-opp op) (every #'numberp operands)) 
				(apply op operands) (cons op operands)))
		expr))
(defun compose (f g)
	(lambda (&rest args) (funcall f (apply g args))))
(defun complexity (expr)
	(cond	((consp expr) (+ (length expr) (reduce #'+ (mapcar #'complexity expr))))
			((null expr) 0)
			(t 1)))
(defun simpler? (a b)
	(< (complexity a) (complexity b)))
(defun factors (expr)
	(cond	((pat-match '(* ?*) expr) (mapcan #'factors (cdr expr)))
			((pat-match '(/ ?*) expr) (append (factors (cadr expr))
				(mapcar #'(lambda (f) `(expt ,f -1)) (mapcan #'factors (cddr expr)))))
			((pat-match '(expt ?a ?b) expr) 
				(mapcar #'(lambda (f) `(expt ,f ,(caddr expr))) (factors (cadr expr))))
			(t (list expr))))
(defun u-sub (expr)
	(when (pat-match '((i ?x) ?f) expr)
		(loop for f in (factors (cadr expr)) do
			(when (pat-match '((?op is unaryp) ?x) f)
				(let* ((x (cadar expr)) (y (cadr expr)) (u (cadr f)) (k (simplify `(/ ,y ,f ((d ,x) ,u)) :to-infix nil)))
					;(print k)
					(unless (contains x k) 
						(return-from u-sub (simplify `(* ,k ((i ,u) ,f)))))))))
	expr)
(defun ibp (expr)
	(when (pat-match '((i ?x) ?f) expr)
		(let ((facs (factors (cadr expr))))
			(loop for u in facs do
				(let* ((x (cadar expr)) (dv (list* '* (remove u facs :count 1))) (du (simplify `((d ,x) ,u) :to-infix nil :ibp nil)) (v (simplify `((i ,x) ,dv) :to-infix nil :ibp nil)) (f (simplify `(- (* ,u ,v) ((i ,x) (* ,v ,du))) :to-infix nil :ibp nil)))
					;(print f)
					(unless (contains `(i ,x) f)
						(return-from ibp f))
					(when (contains expr f)
						(let ((sol (solve `(,expr = ,f) :var expr :to-infix nil :ibp nil)))
							;(print sol)
							(when (pat-match `(,expr = ?x (?if (not (contains ',expr '?x)))) sol)
								(return-from ibp (caddr sol)))))))))
	expr)
(defparameter *simplification-rules* '(	((?x* (?if (contains 'UNDEFINED '?x*))) = UNDEFINED)
										(((?or + -)) = 0)
										(((?or * /)) = 1)
										(((?or + * /) ?x) = ?x)
										((* ?a* (* ?b*) ?c*) = (* ?a* ?b* ?c*))
										((+ ?a* (+ ?b*) ?c*) = (+ ?a* ?b* ?c*))
										((+ ?a* ?x ?b* ?x ?c*) = (+ (* 2 ?x) ?a* ?b* ?c*))
										((- ?x ?a* ?x ?b*) = (- (+ ?a* ?b*)))
										((- ?x ?a* (- ?x) ?b*) = (+ (* 2 ?x) (- (+ ?a* ?b*))))
										((* ?a* ?x ?b* ?x ?c*) = (* (expt ?x 2) ?a* ?b* ?c*))
										((* ?a* (- ?x) ?b* ?x ?c*) = (* (- (expt ?x 2)) ?a* ?b* ?c*))
										((* ?a* ?x ?b* (- ?x) ?c*) = (* (- (expt ?x 2)) ?a* ?b* ?c*))
										((/ ?x ?a* ?x ?b* (?if (not (equalp '?x 0)))) = (/ 1 ?a* ?b*))
										((/ ?x ?a* (- ?x) ?b*) = (/ -1 ?a* ?b*))
										((/ (- ?x) ?a* ?x ?b*) = (/ -1 ?a* ?b*))
										((+ ?a* 0 ?b*) = (+ ?a* ?b*))
										((- ?x ?a* 0 ?b*) = (- ?x ?a* ?b*))
										((- 0 ?x ?y*) = (- (+ ?x ?y*)))
										((- (- ?x)) = ?x)
										((- (- ?x ?a*)) = (- (+ ?a*) ?x))
										((- ?x ?a* (- ?y) ?b*) = (+ ?x ?y (- (+ ?a* ?b*))))
										((/ ?x ?a* 1 ?b*) = (/ ?x ?a* ?b*))
										((/ ?x ?a* 0 ?b*) = UNDEFINED)
										((/ 0 ?a*) = 0)
										((* ?a* 0 ?b*) = 0)
										((* ?a* 1 ?b*) = (* ?a* ?b*))
										((expt ?x 0) = 1)
										((expt ?x 1) = ?x)
										((+ ?a* ?x ?b* (* ?c* ?x ?d*) ?e*) = (+ (* (+ (* ?c* ?d*) 1) ?x) ?a* ?b* ?e*))
										((+ ?a* (* ?c* ?x ?d*) ?b* ?x ?e*) = (+ (* (+ (* ?c* ?d*) 1) ?x) ?a* ?b* ?e*))
										((+ ?a* (* ?b* ?x ?c*) ?d* (* ?e* ?x ?f*) ?g*) = (+ (* (+ (* ?b* ?c*) (* ?e* ?f*)) ?x) ?a* ?d* ?g*))
									  	((expt ?a) = 0)
									  	((* ?a* (- ?x) ?b* (- ?y) ?c*) = (* ?a* ?x ?b* ?y ?c*))
									  	((log (expt ?a ?b)) = (* ?b (log ?a)))
									  	((log E) = 1)
									  	((expt E (log ?x)) = ?x)
									  	((* ?a* ?x ?b* (/ ?c* ?x ?d*) ?e*) = (* ?a* ?b* (/ ?c* ?d*) ?e*))
									  	((* ?a* (/ ?c* ?x ?d*) ?b* ?x ?e*) = (* ?a* ?b* (/ ?c* ?d*) ?e*))
										((/ (* ?b* ?x ?c*) ?d* ?x ?e*) = (/ (* ?b* ?c*) ?d* ?e*))
										((/ ?x ?d* (* ?b* ?x ?c*) ?e*) = (/ 1 (* ?b* ?c*) ?d* ?e*))
										((/ (* ?a* ?x ?b*) ?c* (* ?d* ?x ?e*) ?f*) = (/ (* ?a* ?b*) ?c* (* ?d* ?e*) ?f*))
										((expt E ?x) = (exp ?x))
										((/ (sin ?x) ?a* (cos ?x) ?b*) = (/ (tan ?x) ?a* ?b*))
										((/ ?a ?b* (/ 1 ?c*) ?d*) = (/ (* ?a ?c*) ?b* ?d*))
										((* ?a* -1 ?b* ?c ?d*) = (* (- ?c) ?a* ?b* ?d*))
										((* ?a* ?c ?b* -1 ?d*) = (* (- ?c) ?a* ?b* ?d*))
										((+ ?a* ?x ?b* (- ?x) ?c*) = (+ ?a* ?b* ?c*))
										((+ ?a* (- ?x) ?b* ?x ?c*) = (+ ?a* ?b* ?c*))
										((- ?a ?b* (- ?c ?d*) ?e*) = (- (+ ?a ?d*) ?b* ?c ?e*))
									) "A list of rules for simplifying expressions")
(defparameter *differentiation-rules* '((((d ?x) ?y (?if (not (contains '?x '?y)))) = 0)
										(((d ?x) ?x) = 1)
										(((d ?x) (+ ?f ?g*)) = (+ ((d ?x) ?f) ((d ?x) (+ ?g*))))
									 	(((d ?x) (- ?f ?g*)) = (- ((d ?x) ?f) ((d ?x) (+ ?g*))))
									 	(((d ?x) (* ?f ?g)) = (+ (* ((d ?x) ?f) ?g) (* ((d ?x) ?g) ?f)))
									 	(((d ?x) (* ?f ?g ?h*)) = (+ (* (+ (* ((d ?x) ?f) ?g) (* ((d ?x) ?g) ?f)) ?h*) (* ((d ?x) (* ?h*)) ?f ?g)))
									 	(((d ?x) (/ 1 ?f)) = (/ (- ((d ?x) ?f)) (expt ?f 2)))
									 	(((d ?x) (/ ?f ?g)) = (/ (- (* ((d ?x) ?f) ?g) (* ((d ?x) ?g) ?f)) (expt ?g 2)))
									 	(((d ?x) (/ ?f ?g ?h*)) = (+ (/ (/ (- (* ((d ?x) ?f) ?g) (* ((d ?x) ?g) ?f)) (expt ?g 2)) ?h*) (/ (* (- ((d ?x) (* ?h*))) ?f) ?g (expt (* ?h*) 2)))) 
									 	(((d ?x) (expt ?f ?n) (?if (not (contains '?x '?n)))) = (* ((d ?x) ?f) ?n (expt ?f (- ?n 1))))
									 	(((d ?x) (expt ?n ?f) (?if (not (contains '?x '?n)))) = (* ((d ?x) ?f) (log ?n) (expt ?n ?f)))
									 	(((d ?x) (exp ?f)) = (* ((d ?x) ?f) (exp ?f)))
									 	(((d ?x) (log ?f)) = (/ ((d ?x) ?f) ?f))
									 	(((d ?x) (sin ?f)) = (* ((d ?x) ?f) (cos ?f)))
									 	(((d ?x) (cos ?f)) = (* ((d ?x) ?f) (- (sin ?f))))
									 	(((d ?x) (tan ?f)) = (* ((d ?x) ?f) (expt (sec ?f) 2)))
									 	(((d ?x) ?f (?if (not (pat-match '(expt E (log ?g)) '?f)))) = ((d ?x) (expt E (log ?f))))
									) "A list of rules for calculating derivatives")
(defparameter *integration-rules* '( 	(((i ?x) ?y (?if (not (contains '?x '?y)))) = (* ?x ?y))
										(((i ?x) ?x) = (/ (expt ?x 2) 2))
										(((i ?x) (+ ?f ?g*)) = (+ ((i ?x) ?f) ((i ?x) (+ ?g*))))
										(((i ?x) (- ?f ?g*)) = (- ((i ?x) ?f) ((i ?x) (+ ?g*))))
										(((i ?x) (* ?f* ?y ?g*) (?if (not (contains '?x '?y)))) = (* ?y ((i ?x) (* ?f* ?g*))))
										(((i ?x) (sin ?x)) = (- (cos ?x)))
										(((i ?x) (cos ?x)) = (sin ?x))
										(((i ?x) (tan ?x)) = (- (log (cos ?x))))
										(((i ?x) (exp ?x)) = (exp ?x))
										;(((i ?x) (log ?x)) = (- (* ?x (log ?x)) ?x))
										(((i ?x) (expt ?n ?x) (?if (not (contains '?x '?n)))) = (/ (expt ?n ?x) (log ?n)))
									) "A list of rules for calculating indefinite integrals")
;(setf *simplification-rules*
;	(sort *simplification-rules* (lambda (a b) (simpler? (caddr a) (caddr b)))))
;(setf *differentiation-rules*
;	(sort *differentiation-rules* (lambda (a b) (simpler? (caddr a) (caddr b)))))
(defun pre->in (expr)
	"Converts an expression from prefix to infix"
	(if (consp expr)
		(let ((temp (loop for term in expr collect (pre->in term))))
			(cond	((pat-match '((?op is inopp) ?a ?b) temp) 
						(list (cadr temp) (car temp) (caddr temp)))
					((pat-match '((?op is commp) ?x ?a*) temp)
						(list* (cadr temp) (car temp) (pre->in (list* (car temp) (cddr temp)))))
					((pat-match '((?or - /) ?a ?b ?c*) temp)
						(list (cadr temp) (car temp) 
							(pre->in (list* (inv-op (car temp)) (cddr temp)))))
					((pat-match '(expt ?a ?b) temp)
						(list (cadr temp) '^ (caddr temp)))
					(t temp)))
		expr))
(defun in->pre (expr)
	"Converts an expression from infix to prefix"
	(if (consp expr)
		(let ((temp (loop for term in expr collect (in->pre term))))
			(cond	((pat-match '(?a (?op is opp) ?b) temp)
						(list (cadr temp) (car temp) (caddr temp)))
					((pat-match '(?a (?op is commp) ?b*) temp)
						(list (cadr temp) (car temp) (in->pre (cddr temp))))
					((pat-match '(?a ^ ?b) temp)
						(list 'expt (car temp) (caddr temp)))
					(t temp)))
		expr))
(defun simplify (expr &key (to-infix t) (ibp t))
	"Simplifies an expression"
	(let ((simp (u-sub (transform (in->pre expr) 
					(append *simplification-rules* *differentiation-rules* *integration-rules*) 
					:rewrite (compose #'evaluate #'absorb-args)))))
			(when ibp (setf simp (funcall #'ibp simp)))
			(if to-infix (pre->in simp) simp)))
(defparameter *isolation-rules*	'(	( (?g = ?f (?if (not (contains '?x '?g)))) -> (?f = ?g))
									( (?f = ?g (?if (contains '?x '?f)) (?if (contains '?x '?g))) -> ((- ?f ?g) = 0) )
									( ((+ ?g* ?f ?h* (?if (not (contains '?x '?g*))) (?if (not (contains '?x '?h*)))) = ?k (?if (not (contains '?x '?k)))) -> (?f = (- ?k ?g* ?h*)) )
									( ((* ?g* ?f ?h* (?if (not (contains '?x '?g*))) (?if (not (contains '?x '?h*)))) = ?k (?if (not (contains '?x '?k)))) -> (?f = (/ ?k ?g* ?h*)) )
								 	( ((- ?f) = ?k (?if (not (contains '?x '?k)))) -> (?f = (- ?k)) )
								 	( ((- ?f ?g* (?if (not (contains '?x '?g*)))) = ?k (?if (not (contains '?x '?k)))) -> (?f = (+ ?k ?g*)) )
								 	( ((- ?g ?f ?h* (?if (not (contains '?x '?g))) (?if (not (contains '?x '?h*)))) = ?k (?if (not (contains '?x '?k)))) -> (?f = (- ?g ?k ?h*)) )
								 	( ((/ ?f ?g* (?if (not (contains '?x '?g*)))) = ?k (?if (not (contains '?x '?k)))) -> (?f = (* ?k ?g*)) ) 
								 	( ((/ ?g ?f ?h* (?if (not (contains '?x '?g))) (?if (not (contains '?x '?h*)))) = ?k (?if (not (contains '?x '?k)))) -> (?f = (/ ?g ?k ?h*)) )
								 ) "A list of rules for isolating a variable")
(defun solve (expr &key (var 'x) (to-infix t) (ibp t))
	"Solves simple equations"
	(let ((solved (transform (in->pre expr) *isolation-rules* 
		:rewrite #'(lambda (x) (simplify x :to-infix nil :ibp ibp)) 
		:bindings `((?x . ,var)))))
		(if to-infix (pre->in solved) solved)))