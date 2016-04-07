(defun slice (lst start &optional (end (length lst)))
	(cond	((null lst) lst)
			((= end 0) nil)
			((< end 0) (slice lst start (+ end (length lst))))
			((< start 0) (slice lst (+ start (length lst)) end))
			((< end start) nil)
			((= start 0) (cons (car lst) (slice (cdr lst) 0 (1- end))))
			(t (slice (cdr lst) (1- start) (1- end)))))
(defun variablep (x)
	"Returns whether or not x is a variable (form: ?x)"
	(and (symbolp x) (equal (char (symbol-name x) 0) #\?)))
(defun star-varp (x)
	"star variables match sequences of length 0 or longer (form: ?x*)"
	(if (variablep x)
		(let ((var (symbol-name x)))
			(equal (subseq var (- (length var) 1)) "*"))))
(defun pred-varp (x)
	"predicate variables match a value if it satisfies a predicate (form: (?x is pred)"
	(and (consp x) (= (length x) 3) (eq 'is (cadr x)) (variablep (car x))
		(if (symbolp (caddr x)) (fboundp (caddr x)) (functionp (caddr x))))) 
(defun if-patp (x)
	"if patterns match iff an expression is true (form: (?if expr))"
	(and (consp x) (= (length x) 2) (eq '?if (car x))))
(defun or-patp (x)
	"or patterns match iff input matches one of a list of patterns (form: (?or pat...)"
	(and (consp x) (eq '?or (car x))))
(defun and-patp (x)
	"and pattern match iff input matches all of a list of patterns (form: (?and pat...)"
	(and (consp x) (eq '?and (car x))))
(defun variable-match (variable input bindings)
	"Returns updated bindings (nil if variable would be bound to 2 values)"
	(cond 	((assoc variable bindings) 
				(when (equalp (cdr (assoc variable bindings)) input) 
					(union bindings (list (assoc variable bindings)))))
			(t (union bindings (list (cons variable input))))))
(defun if-pat-match (pat bindings)
	(when (eval (sublis bindings (cadr pat))) (or bindings '((t . t)))))
(defun pred-var-match (var input bindings)
	(when (funcall (caddr var) input) (variable-match (car var) input bindings)))
(defun or-pat-match (pat input bindings)
	(unless (null pat) (let ((first (pat-match (car pat) input bindings)))
		(or first (or-pat-match (cdr pat) input bindings)))))
(defun and-pat-match (pat input bindings)
	(if (null pat) bindings
		(let ((first (pat-match (car pat) input bindings)))
			(when first (and-pat-match (cdr pat) input first)))))
(defun pat-match (pattern input &optional (bindings nil))
	"Returns an association list of variable bindings"
	(cond 	((variablep pattern) (variable-match pattern input bindings))
			((if-patp pattern) (when (null input) (if-pat-match pattern bindings)))
			((pred-varp pattern) (pred-var-match pattern input bindings))
			((or-patp pattern) (or-pat-match (cdr pattern) input bindings))
			((and-patp pattern) (and-pat-match (cdr pattern) input bindings))
			((equalp pattern input) (or bindings '((t . t))))
			((and (consp pattern) (listp input) (star-varp (car pattern)))
				(loop for i from (length input) downto 0 do
					(let ((first (variable-match (car pattern) (slice input 0 i) bindings)))
						(when first (let ((rest (pat-match (cdr pattern) (slice input i) first)))
							(when rest (return rest)))))))
			((and (consp pattern) (if-patp (car pattern)))
				(when (if-pat-match (car pattern) bindings) 
					(pat-match (cdr pattern) input bindings)))
			((and (consp pattern) (consp input)) 
				(let ((first (pat-match (car pattern) (car input) bindings)))
					(when first (pat-match (cdr pattern) (cdr input) first))))
			;((and (consp pattern) (consp input))
			;	(let ((rest (pat-match (cdr pattern) (cdr input) bindings)))
			;		(when rest (pat-match (car pattern) (car input) rest))))
			))
(defun transform-helper (expr rules bindings tracep)
	"Transforms an expression, treating it as one unit"
	(loop for rule in rules do
		(let ((bindings (pat-match (car rule) expr bindings)))
			(when bindings 
				(when tracep (princ (concatenate 'string
							"Used rule " (write-to-string rule) " on input " (write-to-string expr))))
				(return-from transform-helper (sublis bindings (caddr rule))))))
	expr)
(defun transform (expr rules &key (rewrite #'identity) (bindings nil) (tracep nil))
	"Transforms an expression given a set of rules"
	(if (consp expr)
		(let ((new (funcall rewrite 
				(transform-helper (loop for term in expr collect 
					(transform term rules :rewrite rewrite :bindings bindings :tracep tracep))
					rules bindings tracep))))
			(when (and tracep (not (equalp expr new))) (princ (concatenate 'string
				"Rewrote input " (write-to-string expr) " to " (write-to-string new))))
			(if (equalp expr new) new (transform new rules :rewrite rewrite :bindings bindings :tracep tracep)))
		(transform-helper expr rules bindings tracep)))