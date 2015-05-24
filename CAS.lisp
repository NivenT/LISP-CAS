(defun any-of (test list &key (key #'identity) (save-key nil))
	"Returns whether or not any element of list satisfies test"
	(loop for i in list do 
		(when (funcall test (funcall key i))
			(if save-key
				(return-from any-of (funcall key i))
				(return-from any-of i)))))
(defun r-find (item list)
	"Recursively searches list for item"
	(cond	((consp list)	(or (member item list) (any-of #'(lambda (x) (r-find item x)) list)))
			(t (when (equalp item list) list))))
(defun ln (x)
	"Natural log"
	(log x))
(defun variablep (x)
	"Returns whether or not x is a variable (symbol of form ?x)"
	(and (symbolp x) (equal (char (symbol-name x) 0) #\?)))
(defun num-variablep (x)
	"Returns whether or not x is a number variable (?numx represents any number)"
	(and (variablep x) (>= (length (symbol-name x)) 4) (equal (subseq (symbol-name x) 0 4) "?NUM")))
(defun const-variablep (x)
	"Returns whether or not x is a constant variable (?!x represents any expression not containing ?x)"
	(and (variablep x) (equal (char (symbol-name x) 1) #\!)))
(defun var-complement (x)
	"Returns the constant variable that cannot contain x (?x -> ?!x)"
	(read-from-string (concatenate 'string "?!" (subseq (symbol-name x) 1))))
(defun const-var-complement (x)
	"Returns the variable that x can not be associated with (?!x -> ?x)"
	(read-from-string (concatenate 'string "?" (subseq (symbol-name x) 2))))
(defun commutativep (op)
	"Returns whether or not an operation is commutative"
	(member op '(+ *)))
(defun unaryp (op)
	"Returns whether or not an operation is unary"
	(or	(member op '(sin cos tan sinh cosh tanh ln sqrt))
		(and (consp op) (matches '(d ?x) op))))
(defun get-op (expr)
	"Returns the operation in an expression"
	(cadr expr))
(defun first-operand (expr)
	"Returns the 1st operand of an expression"
	(car expr))
(defun second-operand (expr)
	"Returns the 2nd operand of an expression"
	(caddr expr))
(defun eq-expr (f g)
	"Returns whether f or g represent the same expression"
	(let ((a (simplify f)) (b (simplify g)))
		(or	(equalp a b)
			(and 	(consp a) (consp b) (eq (get-op a) (get-op b)) 
					(or (and (eq-expr (first-operand a) (first-operand b)) (eq-expr (second-operand a) (second-operand b)))
						(and (commutativep (get-op a)) (eq-expr (first-operand a) (second-operand b)) (eq-expr (second-operand a) (first-operand b))))))))
(defun variable-match (variable input bindings)
	"Returns the associated value of the variable or (T . T) if it would be associated with two values"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((r-find input (cdr (assoc (var-complement variable) bindings))) '(t . t))
			(t (cons variable input))))
(defun num-var-match (variable input bindings)
	"Returns the associated value of the number variable or (T . T) if input is invalid"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((numberp input) (cons variable input))
			((member input '(E PI)) (cons variable input))
			(t '(t . t))))
(defun const-var-match (variable input bindings)
	"Returns the associated value of the constant variable or (T . T) if input is invalid"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((not (r-find (cdr (assoc (const-var-complement variable) bindings)) input)) (cons variable input))
			(t '(t . t))))
(defun pat-match (pattern input &optional (bindings nil))
	"Returns an association list of variable bindings"
	(cond	((null pattern) bindings)
			((null input)	'((t . t)))
			((num-variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings (list (num-var-match (car pattern) (car input) bindings)))))
			((const-variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings (list (const-var-match (car pattern) (car input) bindings)))))
			((variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings (list (variable-match (car pattern) (car input) bindings)))))
			((and (consp (car pattern)) (consp (car input))) (pat-match (cdr pattern) (cdr input) (pat-match (car pattern) (car input) bindings)))
			(t (if (eq (car pattern) (car input)) (pat-match (cdr pattern) (cdr input) bindings) '((t . t))))))
(defun consistent-bindingsp (bindings)
	"Returns whether or not a list of bindings is valid (does not contain (T . T))"
	(not (member '(t . t) bindings :test #'equalp)))
(defun matches (pattern input)
	"Returns whether or not input matches pattern"
	(consistent-bindingsp (pat-match pattern input)))
(defun infix->prefix (expression)
	"Converts an infix expression to prefix"
	(cond 	((not (consp expression)) expression)
			((unaryp (get-op expression)) expression)
			((null (second-operand expression)) expression)
			((eq (get-op expression) '^)	(list 'expt (infix->prefix (first-operand expression)) (infix->prefix (second-operand expression))))
			(t (list (get-op expression) (infix->prefix (first-operand expression)) (infix->prefix (second-operand expression))))))
(defun evaluate (expression &optional (bindings nil))
	"Evaluates all numeric computations in an expression"
	(let ((f (sublis bindings expression)))
		(if (consp f)
			(cond	((matches '(d ?x) f) f)
					((and (unaryp (car f)) (numberp (cadr f))) (eval (infix->prefix f)))
					((and (unaryp (car f)) (numberp (evaluate (cadr f)))) (eval (list (car f) (evaluate (cadr f)))))
					((unaryp (car f)) (list (car f) (evaluate (cadr f))))
					((and (eq (car f) '-) (null (second-operand f))) (if (numberp (cadr f)) (eval f) (list (car f) (evaluate (cadr f)))))
					((and (numberp (first-operand f)) (numberp (second-operand f))) (eval (infix->prefix f)))
					((and (numberp (evaluate (first-operand f))) (numberp (evaluate (second-operand f))))
						(eval (infix->prefix (list (evaluate (first-operand f)) (get-op f) (evaluate (second-operand f))))))
					(t (list (evaluate (first-operand f)) (get-op f) (evaluate (second-operand f)))))
			f)))
(defparameter *simplification-rules* '(	((?x + ?x) = (2 * ?x))
										((?x - ?x) = 0)
										((?x * ?x) = (?x ^ 2))
										((?x / ?x) = 1)
										((0 + ?x) = ?x)
										((?x + 0) = ?x)
										((?x - 0) = ?x)
										((0 - ?x) = (- ?x))
										((?x / 1) = ?x)
										((0 / ?x) = 0)
										((?x * 1) = ?x)
										((1 * ?x) = ?x)
										((?x * 0) = 0)
										((0 * ?x) = 0)
										((?x ^ 0) = 1)
										((?x ^ 1) = ?x)
										((?x * (?x ^ ?n)) = (?x ^ (1 + ?n)))
										(((?x ^ ?n) * ?x) = (?x ^ (?n + 1)))
										(((?x ^ ?n) * (?x ^ ?m)) = (?x ^ (?n + ?m)))
										(((?x ^ ?n) / ?x) = (?x ^ (?n - 1)))
										((?x / (?x ^ ?n)) = (?x ^ (1 - ?n)))
										(((?x ^ ?n) / (?x ^ ?m)) = (?x ^ (?n - ?m)))
										((1 / ?x) = (?x ^ -1))
										((1 / (?x ^ ?n)) = (?x ^ (- ?n)))
										(((?x * ?a) / ?b) = (?x * (?a / ?b)))
										((ln E) = 1)
										((ln (?x ^ ?n)) = (?n * (ln ?x)))
										((log E ?x) = (ln ?x))
									  ) "A list of rules for simplifying expressions")
(defparameter *derivation-rules*     '(	(((d ?x) ?!x) = 0)
										(((d ?x) ?x) = 1)
										(((d ?x) (?f ^ ?num0)) = (((d ?x) ?f) * (?num0 * (?f ^ (?num0 - 1)))))
										(((d ?x) (sin ?f)) = (((d ?x) ?f) * (cos ?f)))
										(((d ?x) (cos ?f)) = (((d ?x) ?f) * (- (sin ?f))))
										(((d ?x) (?num0 ^ ?f)) = (((d ?x) ?f) * ((?num0 ^ ?f) * (ln ?num0))))
										(((d ?x) (log ?num0 ?f) = (((d ?x) ?f) / (?f * (ln ?num0)))))
										(((d ?x) (ln ?f)) = (((d ?x) ?f) / ?f))
										(((d ?x) (?f + ?g)) = (((d ?x) ?f) + ((d ?x) ?g)))
										(((d ?x) (?f - ?g)) = (((d ?x) ?f) - ((d ?x) ?g)))
										(((d ?x) (?f * ?g)) = ((((d ?x) ?f) * ?g) + (?f * ((d ?x) ?g))))
										(((d ?x) (?f / ?g)) = (((((d ?x) ?f) * ?g) - (?f * ((d ?x) ?g))) / (?g ^ 2)))
										(((d ?x) ?f) = ((d ?x) (E ^ (ln ?f)))) ;;If all else fails...
									  ) "A list of rules for taking derivatives of expressions")
(defun simplify-helper (expr rules)
	"Simplifies an expression, treating it as one unit"
	(let ((f (evaluate expr)))
		(when (consp f)
			(loop for rule in rules do
				(let ((bindings (pat-match (car rule) f)))
					(when (consistent-bindingsp bindings)
						(return-from simplify-helper (sublis bindings (caddr rule) :test #'eq-expr))))))
		f))
(defun simplify (expression &optional (rules (union *simplification-rules* *derivation-rules*)) (prev-expr nil))
	"Simplifies an expression"
	(let ((f (evaluate (simplify-helper expression rules))))
		(if (consp f)
			(let ((g (loop for term in f collect (simplify term rules))))
				(if (equalp prev-expr g) g (simplify g rules g)))
			f)))
			
;examples
;;the derivative of x^x: (simplify '((d x) (x ^ x)))
;;((5 * x)/(x + 9) - a) when x=4 and a=11: (evaluate '(((5 * x) / (x + 9)) - a) '((x . 4) (a . 11)))