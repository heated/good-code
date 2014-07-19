; endgame: interpret this correctly
; (λx.xx)(λx.xx)
; λx.xxx
; (λab.a(a(ab)))(λab.a(ab))
; abc -> (abc)

; (interpret "((λa(λb(a(a(ab))))) (λc(λd(c(cd)))))")
; λbd.b(b(b(b(b(b(b(bd)))))))

(l "core")

(= interpret normalize:parse)

; returns tokens in a lisp-like structure
; given  "(((λ x. (λ y. y)) (λ a. a)) (λ b. b))"
; output '(((λ x (λ y y)) (λ a a)) (λ b b))
(def parse (str)
  (abstract-syntax-tree:tokens:subst "" "." 
    (string:intersperse " " to-list.str)))

; given  ("(" "λ" "x" "x" ")")
; output '(λ x x)
(def abstract-syntax-tree (tokens)
  (with stack        '()
        current-list '()
    (each token tokens
      (case token
        "(" (do 
              (push current-list stack)
              (= current-list '()))
        ")" (let parent pop.stack
              (push rev.current-list parent)
              (= current-list parent))
        (push sym.token current-list)))
    car.current-list))

; functions look like (λ var exp)
(def function? (exp)
  (and cons?.exp (is car.exp 'λ)))

(def apply-body (func f)
  (list 'λ arg.func f:body.func))

(= arg cadr)

(= body caddr)

; DEPRECATED VIA (λb.((λa.(λb.a)) b))
; 
; go through the tree with a set of bound variables
; all encountered variables that are unbound become strings
; pass the hash through stuff recursively
; bind when going into a function
; unbind when jumping out of a function
; ignore inner function declarations that are the same

; unbinding is the hardest - how do you know when you "come back out" of a branch?
; solution 1: recursive, with unbinds after function calls - CERTIFIED COOLEST SHIT
; solution 2: recursive, with copies of the bindings
; base cases
; a -> a | "a"
; (λ ...) -> complicated shit
; (l r) -> go into each and cons
; (def bind-vars (exp (o bound (table)))
;   (case type.exp
;     sym (if bound.exp exp string.exp)
;     (if function?.exp
;       (let var arg.exp
;         (if bound.var
;           (apply-body exp [bind-vars _ bound])
;           ; ugly as shit
;           (do 
;             set:bound.var
;             (let result (apply-body exp [bind-vars _ bound])
;               wipe:bound.var
;               result))))
;       (map [bind-vars _ bound] exp))))

; base cases
; a -> a
; (λ var. body) -> (λ var. norm-body)
; (l r) -> (norm-λ r) 
; ((λ ...) r) -> expand λ with r -> normalize
(def normalize (exp)
  (case type.exp
    sym exp
    (if function?.exp
      (apply-body exp normalize)
      (with left  normalize:car.exp
            right          cadr.exp
        (if function?.left
          (normalize:expand-fn left right)
          (list left normalize.right))))))

(def expand-fn (f exp)
  (expand body.f arg.f exp))

; Okay so it happens to be the case you need local scoping to run lambda calculus, right?
; (λ x. ((λ y. y) x))


; possible . syntax
; (ab.a(a(ab))) (ab.a(ab))
; a.a
; ab.a
; ab.b


; could be chill if I had my own if function kinda like :
;
; (deffoo expand (exp token input)
;   (if (is exp token) input exp)
;   (if (isnt arg.exp token)
;     (apply-body exp [expand _exp token input])
;     exp)
;   (map [expand _ token input] exp))
;
; (deffoo normalize (exp)
;   exp
;   (apply-body exp normalize)
;   (with left  normalize:car.exp
;         right          cadr.exp
;     (if function?.left
;       (normalize:expand-fn left right)
;       (list left normalize.right))))


; solutions to the variable conflict problem
; 1. convert all conflicting variables to new names before running (and probably convert back)
; 2. use fucking weird beta reduction shit or something


; instead of deep-map, create a recursive function which travels through an expression replacing things inside the body, but not replacing within a function that has the same var
; base cases
; a -> <expanded> || a
; (λ x body) -> (λ x (expand body))
; (l r) -> expand both
;
; (λb.((λa.(λb.a)) b))
(def expand (exp token input)
  (case type.exp
    sym (if (is exp token) input exp)
    (if function?.exp
      (if (isnt arg.exp token)
        (apply-body exp [expand _exp token input])
        exp)
      (map [expand _ token input] exp))))

(def assert args
  (map (fn ((a b)) (iso a b)) (tuples 2 args)))

(= id      '(λ x x)
   eight   '(λ b (λ d (b (b (b (b (b (b (b (b d))))))))))
   eight2  '(λ a (λ b (a (a (a (a (a (a (a (a b)))))))))))

(def run-tests ()
  (assert                               (normalize 'a)   'a
                                        (normalize id)   id
                                     (expand-fn id id)   id
                                   (interpret "(λxx)")   id
                           (expand-fn id '(λ y (y y)))   '(λ y (y y))
                  (normalize '(λ b ((λ a (λ b a)) b)))   '(λ a (λ b a))
    (interpret "((λa(λb(a(a(ab))))) (λc(λd(c(cd)))))")   eight
    (interpret "((λa(λb(a(a(ab))))) (λa(λb(a(ab)))))")   eight2))
