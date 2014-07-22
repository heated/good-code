; endgame: interpret this correctly
; (λab.a(a(a b)))(λab.a(a b))

; (repl)

(l "core")

(= arg       cadr
   body      cddr
   names     (table)
   ellipsize [+ "(" _ ")"]
   interpret prettify-λ:name-map:normalize:static-name-resolve:parse)

; functions look like (λ arg . body)
(def λ? (exp)
  (and cons?.exp (is car.exp 'λ)))

(def new-λ (arg body)
  (append `(λ ,arg) body))

(def λ-map (f λ)
  (new-λ arg.λ f:body.λ))

; returns tokens in a lisp-like structure
; given  "(λx.λy.y) (λa.a) (λb.b)"
; return '((λ x λ y y) (λ a a)) (λ b b))
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
; (λ ...) -> complicated shit
; (l r) -> go into each and cons
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

; and now for remapping to names
; given  '(λ 1 (λ 3  (1 (1 (1 (1 (1 (1 (1 (1 3 ))))))))))
; return '(λ b (λ b0 (b (b (b (b (b (b (b (b b0))))))))))
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

; a -> a
; (λ var. body) -> (λ var. norm-body)
; ((λ) r) -> expand λ with r -> normalize
; (l r) -> (l.norm r) - apply the first element to the second, continue
(def normalize (exp)
  (if atom.exp exp
      λ?.exp   (λ-map normalize exp)

      (let left normalize:car.exp
        (if single.exp left
            λ?.left    (normalize:cons (expand-λ left cadr.exp) cddr.exp)
                       (cons left (map normalize cdr.exp))))))

; instead of deep-map, create a recursive function which travels through an expression replacing things inside the body, but not replacing within a function that has the same var
; a -> <expanded> || a
; (λ x body) -> (λ x (expand body))
; (l r) -> expand both
(def expand (exp token input)
  (if atom.exp (if (is exp token) input exp)
      λ?.exp   (λ-map [expand _ token input] exp)
               (map [expand _ token input] exp)))

(def expand-λ (λ exp)
  (expand body.λ arg.λ exp))

; turn exp into a pretty exp string
; a -> "a"
; (λ x x) -> "λx.x"
; (λ x x x) -> "λx.x x"
; (a b) -> "(a b)"
(def prettify-λ (exp)
  (if atom.exp string.exp
      λ?.exp   (string "λ" arg.exp "." (if atom:body.exp body.exp prettify-list:body.exp))
      ellipsize:prettify-list.exp))

(def prettify-list (exp)
  (if λ?.exp
    prettify-λ.exp
    (sum:intersperse " " (map prettify-λ exp))))


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
