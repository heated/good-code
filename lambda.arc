; Endgame: Interpret this correctly
; (λab.a(a(a b)))(λab.a(a b))

; Press (repl) to play

(l "core")

(= arg       cadr
   body      cddr
   names     (table)
   ellipsize [+ "(" _ ")"]
   interpret prettify-λ:name-map:normalize:static-name-resolve:parse)

; Functions look like '(λ arg . body)
(def λ? (exp)
  (and cons?.exp (is car.exp 'λ)))

(def new-λ (arg body)
  (append `(λ ,arg) body))

(def λ-map (f λ)
  (new-λ arg.λ f:body.λ))

; Given a lambda expression in written notation, return tokens in a tree structure.
; Given  "(λx.λy.y) (λa.a) (λb.b)"
; Return '((λ x λ y y) (λ a a)) (λ b b))
(def parse (str)
  (read:ellipsize:multisubst '(("." " ") ("λ" "λ ")) str))

; go through the tree with a set of bound variables
; all encountered variables that are unbound become strings
; pass the hash through stuff recursively
; bind when going into a function
; unbind when jumping out of a function
; ignore inner function declarations that are the same
;
; unbinding is the hardest - how do you know when you "come back out" of a branch?
; solution: recursive, with unbinds after function calls

; a -> a | "a"
; (l r) -> recurse
; (λ ...) -> complicated shit
(def snr (exp (o bound (table)))
  (if atom.exp  (or car:bound.exp string.exp)
      no:λ?.exp (map [snr _ bound] exp)
    
      (with var   arg.exp
            index len.names
        (= names.index var)
        (push index bound.var)

        (before-returning 
          (new-λ index (snr body.exp bound))
          pop:bound.var))))

(def static-name-resolve (exp)
  (= names (table))
  snr.exp)

; Given an expression with unique function arguments and a mapping to names, replace all atoms with their corresponding names. Handle collisions by applying a unique number suffix.
; Given  '(λ 1 (λ 3  (1 (1 (1 (1 (1 (1 (1 (1 3 ))))))))))
; Return '(λ b (λ b0 (b (b (b (b (b (b (b (b b0))))))))))
(def name-map (exp (o depth (table)))
  (if atom.exp  names.exp
      no:λ?.exp (map [name-map _ depth] exp)

      (withs index arg.exp
             var   names.index
        (= names.index (symb var depth.var))
        (or= depth.var -1)
        ++:depth.var
        (before-returning 
          (new-λ names.index (name-map body.exp depth))
          --:depth.var))))

; Given an expression, simplify it to its normal form.
;              1 -> 1
; (λ var . body) -> (λ var . norm.body)
;        ((...)) -> (...)
;        ((λ) r) -> (norm:expand λ r)
;        (l . r) -> (norm.l . norm.r)
(def normalize (exp)
  (if atom.exp exp
      λ?.exp   (λ-map normalize exp)

      (let left normalize:car.exp
        (if single.exp left
            λ?.left    (normalize:cons (expand-λ left cadr.exp) cddr.exp)
                       (cons left (map normalize cdr.exp))))))

; Given a function and an expression, replace all instances of the function's argument with the expression and unwrap the function.
(def expand-λ (λ exp)
  (deep-map [if (is _ arg.λ) exp _] body.λ))

; Given an expression, return its written notation as a string.
;         a -> "a"
;   (λ x x) -> "λx.x"
; (λ x x x) -> "λx.x x"
;     (a b) -> "(a b)"
(def prettify-λ (exp)
  (if atom.exp string.exp
      λ?.exp   (string "λ" arg.exp "." (if atom:body.exp body.exp prettify-list:body.exp))
      ellipsize:prettify-list.exp))

(def prettify-list (exp)
  (if λ?.exp
    prettify-λ.exp
    (sum:intersperse " " (map prettify-λ exp))))

; Given a list of expressions, test the equality of each pair.
(def assert args
  (map [apply iso _] (tuples 2 args)))

(def run-tests ()
  (assert                           (normalize 'a)   'a
                            (normalize '(λ x . x))   '(λ x . x)
                                (interpret "λx.x")   "λx.x"
                      (interpret "λb.(λa.λb.a) b")   "λb.λb0.b"
    (interpret "(λa.λb.a(a(a b))) (λc.λd.c(c d))")   "λb.λd.b (b (b (b (b (b (b (b d)))))))"
    (interpret "(λa.λb.a(a(a b))) (λa.λb.a(a b))")   "λb.λb0.b (b (b (b (b (b (b (b b0)))))))"))


(def repl ()
  (readline)
  (prn "The format for inputted expressions is: (λa.λb.a(a(a b))) (λa.λb.a(a b))")
  (while t
    (pr "λ > ")
    (prn:interpret:readline)))
