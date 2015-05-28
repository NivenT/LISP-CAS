(defun r-find (item list)
	"Recursively searches list for item"
	(cond	((consp list)	(or (member item list) (some #'(lambda (x) (r-find item x)) list)))
			(t (when (equalp item list) list))))
(defun ln (x)
	"Natural log"
	(log x))
(defun mylength (seq)
	"returns 1 for non-sequences"
	(if (typep seq 'sequence) (length seq) 1))
(defun r-length (seq)
	"returns the recursive length of a sequence ( (1 2 (3 4) (5 (6 7) 8) 9) -> 9)"
	(if (consp seq)
		(apply #'+ (mapcar #'r-length seq))
		1))
(defun variablep (x)
	"Returns whether or not x is a variable (symbol of form ?x)"
	(and (symbolp x) (equal (char (symbol-name x) 0) #\?)))
(defun num-variablep (x)
	"Returns whether or not x is a number variable (?num represents any number)"
	(and (variablep x) (>= (length (symbol-name x)) 4) (equal (subseq (symbol-name x) 0 4) "?NUM")))
(defun const-variablep (x)
	"Returns whether or not x is a constant variable (?!x represents any expression not containing ?x)"
	(and (variablep x) (equal (char (symbol-name x) 1) #\!)))
(defun update-variablep (x)
	"Returns whether or not x is an update variable ((?u f) means replace f with its simplified value, given the bindings found thus far)"
	(and (consp x) (= (length x) 2) (eq (car x) '?u)))
(defun function-variablep (x)
	"Returns whether or not x is a function variable (?fx represents any expression containing ?x)"
	(and (variablep x) (>= (length (symbol-name x)) 3) (equal (char (symbol-name x) 1) #\F)))
(defun predicate-variablep (x)
	"Returns whether or not x is a predicate variable (?px matches input iff (x input) is not nil)"
	(and (variablep x) (>= (length (symbol-name x)) 3) (equal (char (symbol-name x) 1) #\P) (fboundp (pred-func x))))
(defun var-complement (x)
	"Returns the constant variable that cannot contain x (?x -> ?!x)"
	(read-from-string (concatenate 'string "?!" (subseq (symbol-name x) 1))))
(defun const-var-complement (x)
	"Returns the variable that x can not be associated with (?!x -> ?x)"
	(read-from-string (concatenate 'string "?" (subseq (symbol-name x) 2))))
(defun func-arg (x)
	"Returns the argument of the function variable x (?fx -> ?x)"
	(read-from-string (concatenate 'string "?" (subseq (symbol-name x) 2))))
(defun var-func (x)
	"Returns the function variable associated with x (?x -> ?fx)"
	(read-from-string (concatenate 'string "?f" (subseq (symbol-name x) 1))))
(defun pred-func (x)
	"Returns the predicate associated with x (?pevenp -> evenp)"
	(read-from-string (subseq (symbol-name x) 2)))
(defun commutativep (op)
	"Returns whether or not an operation is commutative"
	(member op '(+ * =)))
(defun unaryp (op)
	"Returns whether or not an operation is unary"
	(or	(member op '(sin cos tan sinh cosh tanh ln sqrt asin acos atan asinh acosh atanh))
		(and (consp op) (or (matches '(d ?x) op) (matches '(i ?x) op)))))
(defun get-op (expr)
	"Returns the operation in an expression"
	(cadr expr))
(defun first-operand (expr)
	"Returns the 1st operand of an expression"
	(car expr))
(defun second-operand (expr)
	"Returns the 2nd operand of an expression"
	(caddr expr))
(defun eq-expr (f g &optional (simp? t))
	"Returns whether f or g represent the same expression"
	(when (equalp f g) (return-from eq-expr t))
	(let ((a (if simp? (simplify f) f)) (b (if simp? (simplify g) g)))
		(or	(equalp a b)
			(and 	(consp a) (consp b) (eq (get-op a) (get-op b)) 
					(or (and (eq-expr (first-operand a) (first-operand b) simp?) (eq-expr (second-operand a) (second-operand b) simp?))
						(and (commutativep (get-op a)) (eq-expr (first-operand a) (second-operand b) simp?) (eq-expr (second-operand a) (first-operand b) simp?)))))))
(defun consistent-bindingsp (bindings)
	"Returns whether or not a list of bindings is valid (does not contain (T . T))"
	(not (member '(t . t) bindings :test #'equalp)))
(defun variable-match (variable input bindings)
	"Returns the associated value of the variable or (T . T) if it would be associated with two values"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((r-find input (cdr (assoc (var-complement variable) bindings))) '(t . t))
			((not (assoc (var-func variable) bindings)) (cons variable input))
			((not (r-find input (cdr (assoc (var-func variable) bindings)))) '(t . t))
			(t (cons variable input))))
(defun num-var-match (variable input bindings)
	"Returns the associated value of the number variable or (T . T) if input is invalid"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((or (numberp input) (eq input 'E) (eq input 'PI)) (cons variable input))
			((member input '(E PI)) (cons variable input))
			(t '(t . t))))
(defun const-var-match (variable input bindings)
	"Returns the associated value of the constant variable or (T . T) if input is invalid"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((not (r-find (cdr (assoc (const-var-complement variable) bindings)) input)) (cons variable input))
			(t '(t . t))))
(defun update-var (variable bindings)
	"Returns the updated representation of variable"
	(simplify (sublis bindings (cadr variable))))
(defun function-var-match (variable input bindings)
	"Returns the associated value of the function variable or (T . T) if input is invalid"
	(cond 	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((not (assoc (func-arg variable) bindings)) (cons variable input))
			((r-find (cdr (assoc (func-arg variable) bindings)) input) (cons variable input))
			(t '(t . t))))
(defun predicate-var-match (variable input bindings)
	"Returns the associated value of the predicate variable or (T . T) if input is invalid"
	(cond	((assoc variable bindings) (if (eq-expr (cdr (assoc variable bindings)) input) (assoc variable bindings) '(t . t)))
			((funcall (pred-func variable) input) (cons variable input))
			(t '(t . t))))
(defun pat-match (pattern input &optional (bindings nil))
	"Returns an association list of variable bindings"
	(cond	((not (consistent-bindingsp bindings)) bindings)
			((not (consp pattern)) bindings)
			((not (consp input))	'((t . t)))
			((num-variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings `(,(num-var-match (car pattern) (car input) bindings)))))
			((const-variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings `(,(const-var-match (car pattern) (car input) bindings)))))
			((update-variablep (car pattern)) (pat-match (cons (update-var (car pattern) bindings) (cdr pattern)) input bindings))
			((function-variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings `(,(function-var-match (car pattern) (car input) bindings)))))
			((predicate-variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings `(,(predicate-var-match (car pattern) (car input) bindings)))))
			((variablep (car pattern)) (pat-match (cdr pattern) (cdr input) (union bindings (list (variable-match (car pattern) (car input) bindings)))))
			((and (consp (car pattern)) (consp (car input))) (pat-match (cdr pattern) (cdr input) (pat-match (car pattern) (car input) bindings)))
			(t (if (eq-expr (car pattern) (car input) nil) (pat-match (cdr pattern) (cdr input) bindings) (union bindings '((t . t)))))))
(defun matches (pattern input)
	"Returns whether or not input matches pattern"
	(consistent-bindingsp (pat-match pattern input)))
(defun infix->prefix (expression)
	"Converts an infix expression to prefix"
	(cond 	((not (consp expression)) 			expression)
			((unaryp (get-op expression)) 		expression)
			((null (second-operand expression)) expression)
			((eq (get-op expression) '^)		(list 'expt (infix->prefix (first-operand expression)) (infix->prefix (second-operand expression))))
			((eq (car expression) 'log)			(list 'log (infix->prefix (caddr expression)) (infix->prefix (cadr expression))))
			(t (list (get-op expression) 		(infix->prefix (first-operand expression)) (infix->prefix (second-operand expression))))))
(defun evaluate (expression &optional (bindings nil))
	"Evaluates all numeric computations in an expression"
	(let ((f (sublis bindings expression)))
		(if (consp f)
			(cond	((matches '(d ?x) f) f)
					((matches '(i ?x) f) f)
					((matches '(d ?x) (car f)) f)
					((matches '(i ?x) (car f)) f)
					((and (unaryp (car f)) (numberp (evaluate (cadr f)))) (eval (list (car f) (evaluate (cadr f)))))
					((unaryp (car f)) (list (car f) (evaluate (cadr f))))
					((and (eq (car f) '-) (null (second-operand f))) (if (numberp (evaluate (cadr f))) (eval (list (car f) (evaluate (cadr f)))) (list (car f) (evaluate (cadr f)))))
					((and (eq (car f) 'log) (numberp (evaluate (cadr f))) (numberp (evaluate (caddr f)))) (eval (infix->prefix f)))
					((and (numberp (evaluate (first-operand f))) (numberp (evaluate (second-operand f))))
						(eval (infix->prefix (list (evaluate (first-operand f)) (get-op f) (evaluate (second-operand f))))))
					(t (list (evaluate (first-operand f)) (get-op f) (evaluate (second-operand f)))))
			f)))
(defparameter *simplification-rules* '(	((?x + ?x) = (2 * ?x))
										((?x - ?x) = 0)
										(((- ?x) - ?x) = (-2 * ?x))
										((?x * ?x) = (?x ^ 2))
										((?x / ?x) = 1)
										((0 + ?x) = ?x)
										((?x + 0) = ?x)
										((?x - 0) = ?x)
										((0 - ?x) = (- ?x))
										((- (- ?x)) = ?x)
										((?a - (- ?b)) = (?a + ?b))
										((- (?a - ?b)) = (?b - ?a))
										((?x + (?a - ?x)) = ?a)
										((?x - (?a + ?x)) = (- ?a))
										(((?a + ?x) - ?x) = ?a)
										(((?a - ?x) + ?x) = ?a)
										((?x / 1) = ?x)
										((0 / ?x) = 0)
										((?x * 1) = ?x)
										((1 * ?x) = ?x)
										((?x * 0) = 0)
										((0 * ?x) = 0)
										((?x ^ 0) = 1)
										((?x ^ 1) = ?x)
										(((?x ^ ?a) ^ ?b) = (?x ^ (?a * ?b)))
										((?x * (?x ^ ?n)) = (?x ^ (1 + ?n)))
										(((?x ^ ?n) * ?x) = (?x ^ (?n + 1)))
										(((?x ^ ?n) * (?x ^ ?m)) = (?x ^ (?n + ?m)))
										(((?x ^ ?n) / ?x) = (?x ^ (?n - 1)))
										((?x / (?x ^ ?n)) = (?x ^ (1 - ?n)))
										(((?x ^ ?n) / (?x ^ ?m)) = (?x ^ (?n - ?m)))
										((1 / ?x) = (?x ^ -1))
										((1 / (?x ^ ?n)) = (?x ^ (- ?n)))
										((?x * (?a / ?x)) = ?a)
										((?x / (?a * ?x)) = (1 / ?a))
										(((?a / ?x) * ?x) = ?a)
										(((?a * ?x) / ?x) = ?a)
										((ln E) = 1)
										((ln (?x ^ ?n)) = (?n * (ln ?x)))
										((log E ?x) = (ln ?x))
										((log ?b ?b) = 1)
										((log ?b (?x ^ ?n)) = (?n * (log ?b ?x)))
										((sin PI) = 0)
										((sin (?pintegerp * PI)) = 0)
										((sin (PI * ?pintegerp)) = 0)
										((cos PI) = -1)
										((cos (?pintegerp * PI)) = (-1 ^ ?pintegerp))
										((cos (PI * ?pintegerp)) = (-1 ^ ?pintegerp))
										(((?x * ?a) + (?x * ?b)) = ((?a + ?b) * ?x))
										(((?x * ?a) + (?b * ?x)) = ((?a + ?b) * ?x))
										(((?a * ?x) + (?x * ?b)) = ((?a + ?b) * ?x))
										(((?a * ?x) + (?b * ?x)) = ((?a + ?b) * ?x))
										((?x + (?x * ?b)) = ((1 + ?b) * ?x))
										((?x + (?b * ?x)) = ((1 + ?b) * ?x))
										(((?x * ?a) + ?x) = ((?a + 1) * ?x))
										(((?a * ?x) + ?x) = ((?a + 1) * ?x))
										(((?x * ?a) - (?x * ?b)) = ((?a - ?b) * ?x))
										(((?x * ?a) - (?b * ?x)) = ((?a - ?b) * ?x))
										(((?a * ?x) - (?x * ?b)) = ((?a - ?b) * ?x))
										(((?a * ?x) - (?b * ?x)) = ((?a - ?b) * ?x))
										((?x - (?x * ?b)) = ((1 - ?b) * ?x))
										((?x - (?b * ?x)) = ((1 - ?b) * ?x))
										(((?x * ?a) - ?x) = ((?a - 1) * ?x))
										(((?a * ?x) - ?x) = ((?a - 1) * ?x))
										(((- ?x) + ?a) = (?a - ?x))
										((?a + (- ?x)) = (?a - ?x))
									  ) "A list of rules for simplifying expressions")
(defparameter *derivation-rules*     '(	(((d ?x) ?!x) = 0)
										(((d ?x) ?x) = 1)
										(((d ?x) (?f ^ ?num0)) = (((d ?x) ?f) * (?num0 * (?f ^ (?num0 - 1)))))
										(((d ?x) (sin ?f)) = (((d ?x) ?f) * (cos ?f)))
										(((d ?x) (cos ?f)) = (((d ?x) ?f) * (- (sin ?f))))
										(((d ?x) (?num0 ^ ?f)) = (((d ?x) ?f) * ((?num0 ^ ?f) * (ln ?num0))))
										(((d ?x) (log ?num0 ?f) = (((d ?x) ?f) / (?f * (ln ?num0)))))
										(((d ?x) (ln ?f)) = (((d ?x) ?f) / ?f))
										(((d ?x) (sinh ?f)) = (((d ?x) ?f) * (cosh ?f)))
										(((d ?x) (cosh ?f)) = (((d ?x) ?f) * (sinh ?f)))
										(((d ?x) (?f + ?g)) = (((d ?x) ?f) + ((d ?x) ?g)))
										(((d ?x) (?f - ?g)) = (((d ?x) ?f) - ((d ?x) ?g)))
										(((d ?x) (- ?f)) = (- ((d ?x) ?f)))
										(((d ?x) (?f * ?g)) = ((((d ?x) ?f) * ?g) + (?f * ((d ?x) ?g))))
										(((d ?x) (?f / ?g)) = (((((d ?x) ?f) * ?g) - (?f * ((d ?x) ?g))) / (?g ^ 2)))
										(((d ?x) ((i ?x) ?f)) = ?f)
										(((d ?x) ?f) = ((d ?x) (E ^ (ln ?f)))) ;;If all else fails...
									  ) "A list of rules for taking derivatives of expressions")
(defparameter *integration-rules*	 '(	(((i ?x) ?!x) = (?x * ?!x))
										(((i ?x) ?x) = ((?x ^ 2) / 2))
										(((i ?x) (1 / ?x)) = (ln ?x))
										(((i ?x) (?x ^ -1)) = (ln ?x))
										(((i ?x) (?x ^ ?num0)) = ((?x ^ (?num0 + 1)) / (?num0 + 1)))
										(((i ?x) (sin ?x)) = (- (cos ?x)))
										(((i ?x) (cos ?x)) = (sin ?x))
										(((i ?x) (log ?num0 ?x)) = (((?x * (ln ?x)) - ?x) / (ln ?num0)))
										(((i ?x) (ln ?x)) = ((?x * (ln ?x)) - ?x))
										(((i ?x) (sinh ?x)) = (cosh ?x))
										(((i ?x) (cosh ?x)) = (sinh ?x))
										(((i ?x) (?num0 ^ ?x)) = ((?num0 ^ ?x) / (ln ?num0)))
										;u substitution
										(((i ?x) (?f * (?u ((d ?x) ?f)))) = ((?f ^ 2) / 2))
										(((i ?x) ((1 / ?f) * (?u ((d ?x) ?f)))) = (ln ?f))
										(((i ?x) ((?f ^ -1) * (?u ((d ?x) ?f)))) = (ln ?f))
										(((i ?x) ((?f ^ ?num0) * (?u ((d ?x) ?f)))) = ((?f ^ (?num0 + 1)) / (?num0 + 1)))
										(((i ?x) ((sin ?f) * (?u ((d ?x) ?f)))) = (- (cos ?f)))
										(((i ?x) ((cos ?f) * (?u ((d ?x) ?f)))) = (sin ?f))
										(((i ?x) ((log ?num0 ?f) * (?u ((d ?x) ?f)))) = (((?f * (ln ?f)) - ?f) / (ln ?num0)))
										(((i ?x) ((ln ?f) * (?u ((d ?x) ?f)))) = ((?f * (ln ?f)) - ?f))
										(((i ?x) ((sinh ?f) * (?u ((d ?x) ?f)))) = (cosh ?f))
										(((i ?x) ((cosh ?f) * (?u ((d ?x) ?f)))) = (sinh ?f))
										;integration rules
										(((i ?x) (?!x * ?f)) = (?!x * ((i ?x) ?f)))
										(((i ?x) (?f * ?!x)) = (?!x * ((i ?x) ?f)))
										(((i ?x) (?f + ?g)) = (((i ?x) ?f) + ((i ?x) ?g)))
										(((i ?x) (?f - ?g)) = (((i ?x) ?f) - ((i ?x) ?g)))
										(((i ?x) (- ?f)) = (- ((i ?x) ?f)))
										(((i ?x) ((d ?x) ?f)) = ?f)
										(((i ?x) (?f * ?g)) = ((?f * ((i ?x) ?g)) - ((i ?x) (((i ?x) ?g) * ((d ?x) ?f)))))
									  ) "A list of rules for integrating expressions")
(defparameter *rules* (append *simplification-rules* *derivation-rules* *integration-rules*) "All the rules used by simplify")
(defun simplify-helper (expr rules)
	"Simplifies an expression, treating it as one unit"
	(let ((f (evaluate expr)))
		(when (consp f)
			(loop for rule in rules do
				(let ((bindings (pat-match (car rule) f)))
					(when (consistent-bindingsp bindings)
						(return-from simplify-helper (sublis bindings (caddr rule) :test #'eq-expr))))))
		f))
(defun simplify (expression &optional (rules *rules*) (prev-expr expression))
	"Simplifies an expression"
	(if (consp expression)
		(let ((f (simplify-helper (loop for term in expression collect (simplify term rules)) rules)))
			(if (equalp prev-expr f) f (simplify f rules)))
		expression))
(defun find-zero (expression &key (accuracy .0000001) (guess 1) (var 'x))
	"Finds a zero of expression"
	(if (<= (abs (evaluate expression `((,var . ,guess) (E . ,(exp 1)) (PI . ,PI)))) accuracy)
		guess
		(find-zero expression :accuracy accuracy :var var :guess (- guess 
			(/ (evaluate expression `((,var . ,guess) (E . ,(exp 1)) (PI . ,PI))) (evaluate (simplify `((d ,var) ,expression)) `((,var . ,guess) (E . ,(exp 1)) (PI . ,PI))))))))
(defun find-extremum (expression &key (accuracy .0000001) (guess 1) (var 'x))
	"Finds an extremum point of expression"
	(let ((x (find-zero (simplify `((d ,var) ,expression)) :accuracy accuracy :guess guess :var var)))
		(list x (evaluate expression `((,var . ,x))))))
(defun terms (expression)
	"Returns a list of terms in expression"
	(cond	((not (consp expression)) `(,expression))
			((matches '(?a + ?b) expression) (append (terms (first-operand expression)) (terms (second-operand expression))))
			((matches '(?a - ?b) expression) (append (terms (first-operand expression)) (terms `(- ,(second-operand expression)))))
			((matches '(?a * ?b) expression) (loop for a in (terms (first-operand expression)) append
												(loop for b in (terms (second-operand expression))
													collect `(,a * ,b))))
			((matches '(- ?a) expression) (mapcar #'(lambda (x) (simplify `(- ,x))) (terms (cadr expression))))
			(t `(,expression))))
(defun sum (&rest terms)
	"Returns the sum of the terms"
	(case (length terms)	(0 nil)
							(1 (car terms))
							(2 `(,(car terms) + ,(cadr terms)))
							(otherwise (apply #'sum `(,(car terms) + ,(cadr terms)) (cddr terms)))))
(defun clt (expression &optional (var nil))
	"Combines numerical terms and terms that share var as a factor"
	(if (null var)
		(let ((sorted-terms (sort (terms expression) #'> :key #'(lambda (term) (if (numberp term) (abs term) 0)))))
			(simplify (apply #'sum sorted-terms))) 
		(let ((sorted-terms (sort (terms expression) #'> :key #'(lambda (term) (if (r-find var term) (mylength term) 0)))))
			(clt (simplify (apply #'sum sorted-terms))))))
(defparameter *isolation-rules*	'(	( (?!x0 = ?fx) -> (?fx = ?!x0) )
									( ((- ?fx0) = ?!x0) -> (?fx0 = (- ?!x0)) )
									( ((?fx0 + ?!x0) = ?!x1) -> (?fx0 = (?!x1 - ?!x0)) )
									( ((?!x0 + ?fx0) = ?!x1) -> (?fx0 = (?!x1 - ?!x0)) )
									( ((?fx0 - ?!x0) = ?!x1) -> (?fx0 = (?!x1 + ?!x0)) )
									( ((?!x0 - ?fx0) = ?!x1) -> (?fx0 = (?!x0 - ?!x1)) )
									( ((?fx0 * ?!x0) = ?!x1) -> (?fx0 = (?!x1 / ?!x0)) )
									( ((?!x0 * ?fx0) = ?!x1) -> (?fx0 = (?!x1 / ?!x0)) )
									( ((?fx0 / ?!x0) = ?!x1) -> (?fx0 = (?!x1 * ?!x0)) )
									( ((?!x0 / ?fx0) = ?!x1) -> (?fx0 = (?!x0 / ?!x1)) )
									( ((?fx0 ^ ?!x0) = ?!x1) -> (?fx0 = (?!x1 ^ (1 / ?!x0))) )
									( ((?!x0 ^ ?fx0) = ?!x1) -> (?fx0 = (log ?!x0 ?!x1)) )
									( ((sin ?fx0) = ?!x0) -> (?fx0 = (asin ?!x0)) )
									( ((cos ?fx0) = ?!x0) -> (?fx0 = (acos ?!x0)) )
									( ((ln ?fx0) = ?!x0) -> (?fx0 = (E ^ ?!x0)) )
									( ((log ?b ?fx0) = ?!x0) -> (?fx0 = (?b ^ ?!x0)) )
									( ((?fx0 / ?fx1) = ?!x0) -> (?fx0 = (?!x0 * ?fx1)) )
									( ((?fx0 * ?fx1) = 0) -> (?fx0 = 0) )
									( ((?fx0 ^ ?fx1) = 1) -> (?fx1 = 0) )
									( ((log ?fx0 ?fx1) = 1) -> (?fx1 = 0) )
									( (((?a / ?b) + (?c / ?d)) = ?!x0) -> ((((?a * ?d) + (?b * ?c)) / (?b * ?d)) = ?!x0) )
									( (((?a / ?b) + ?c) = ?!x0) -> (((?a + (?b * ?c)) / ?b) = ?!x0) )
									( ((?c + (?a / ?b)) = ?!x0) -> (((?a + (?b * ?c)) / ?b) = ?!x0) )
									( (?fx0 = ?fx1) -> ((?fx0 - ?fx1) = 0) )
								 ) "A list of rules for isolating a variable")
(defun clone-variable (var val &optional (n 10))
	"Creates n dummy variables that all have the same binding as val"
	(case n	(0 `((,var . ,val)))
			(otherwise (union `((,(read-from-string (concatenate 'string (symbol-name var) (write-to-string (1- n)))) . ,val)) (clone-variable var val (1- n))))))
(defun solve (equation &key (var 'x) (rules *isolation-rules*))
	"Solves simple equations"
	(when (consp equation)
		(loop for rule in rules do
			(let ((bindings (pat-match (car rule) equation (clone-variable '?x var))))
				(when (consistent-bindingsp bindings)
					(let ((eqn (sublis bindings (caddr rule))))
						(return-from solve (solve (simplify `(,(clt (car eqn) var) = ,(clt (caddr eqn) var))) :var var :rules rules)))))))
	(let ((eqn (simplify `(,(clt (car equation) var) = ,(clt (caddr equation) var)))))
		(if (equal equation eqn)
			equation
			(solve eqn :var var :rules rules))))
(defun nsolve (equation &key (accuracy .0000001) (guess 1) (var 'x))
	"Numerically solves an equation"
	(find-zero `(,(car equation) - ,(caddr equation)) :accuracy accuracy :guess guess :var var))
(defun solve-system-helper (vars equations &optional (known nil))
	(loop for eqn in equations do
		(loop for var in vars do
			(let ((sol (solve eqn :var var)))
				(if (eq (car sol) var)
					(return-from solve-system-helper 
						(solve-system-helper (remove var vars) (subst (caddr sol) (car sol) (remove eqn equations)) (append known `(,sol))))))))
	known)
(defun solve-system (vars &rest equations)
	"Solves simple systems of equations"
	(let ((sol (solve-system-helper vars equations)))
		(solve-system-helper vars (sort sol #'< :key #'r-length))))
(defun derive (expression &optional (var 'x))
	"Calculates d/dvar expression"
	(simplify `((d ,var) ,expression)))
(defun integrate (expression &key (low nil) (high nil) (var 'x))
	"Calculates the definite or indefinite integral of expression dvar"
	(if (or (null low) (null high))
		`(,(simplify `((i ,var) ,expression)) + c)
		(let ((F (simplify `((i ,var) ,expression))))
			(simplify `(,(evaluate F `((,var . ,high))) - ,(evaluate F `((,var . ,low))))))))
(defun factorial (n)
	"Computes n!"
	(if (<= n 1) 1 (* n (factorial (1- n)))))
(defun taylor (expression &key (c 0) (n 3) (var 'x))
	"Computes the Taylor series about c of expression out to n terms"
	(do (	(f expression (derive f var))
			(terms nil (append terms (list (simplify `((,(evaluate f `((,var . ,c))) / ,(factorial i)) * ((,var - ,c) ^ ,i))))))
			(i 0 (1+ i)))
		((= i n) (apply #'sum (remove 0 terms :test #'equalp)))))
	
;examples
;;integrating 2x*sin(x^2) dx: (simplify '((i x) ((sin (x ^ 2)) * (2 * x))))
;;integrating sin(ln(x))/x+sin(x)*cos(x) dx: (integrate '(((sin (ln x)) * (1 / x)) + ((sin x) * (cos x))))
;;the derivative of x^x with respect to x: (simplify '((d x) (x ^ x)))
;;the derivative of E^sin(y*x^2) with respect to y: (derive '(E ^ (sin (y * (x ^ 2)))) 'y)
;;((5 * x)/(x + 9) - a) when x=4 and a=11: (evaluate '(((5 * x) / (x + 9)) - a) '((x . 4) (a . 11)))
;;solving 4x+8=10x-7: (solve '(((4 * x) + 8) = ((10 * x) - 7)))
;;solving 2x+xy=5y: (solve '(((2 * x) + (x * y)) = (5 * y)) :var 'y)
;;solving lnx=sinx: (nsolve '((ln x) = (sin x)))
;;computing the area under xcos(x) from x=0 to x=PI: (integrate '(x * (cos x)) :low 0 :high 'PI)
;;computing the Taylor series of E^x out to the x^7 term: (taylor '(E ^ x) :n 8)
;;solving the system x+y=22 and x-y=10: (solve-system '(x y) '((x + y) = 22) '((x - y) = 10))
;;solving the system sin(x * y)=-.3 and cos(x + y)=1: (solve-system '(x y) '((sin (x * y)) = -.3) '((cos (x + y)) = 1))