; endgame: interpret this correctly
; (λx.xx)(λx.xx)
; λx.xxx
; (λab.a(a(ab)))(λab.a(ab))
; abc -> (abc)

; (interpret "((λa(λb(a(a(ab))))) (λc(λd(c(cd)))))")
; λbd.b(b(b(b(b(b(b(bd)))))))

(l "core")

(= interpret sym-map:new-name-map:normalize:static-name-resolve:parse)

; returns tokens in a lisp-like structure
; given  "(((λ x. (λ y. y)) (λ a. a)) (λ b. b))"
; return '(((λ x (λ y y)) (λ a a)) (λ b b))
(def parse (str)
  (abstract-syntax-tree:tokens:subst "" "." 
    (string:intersperse " " to-list.str)))

; given  ("(" "λ" "x" "x" ")")
; return '(λ x x)
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

(def fn-map (f func)
  (list 'λ arg.func f:body.func))

(= arg cadr)

(= body caddr)

(mac defcase (name args atom fun tuple)
  (let exp car.args
    `(def ,name ,args
       (case (type ,exp)
        cons (if (function? ,exp) ,fun ,tuple)
        ,atom))))

;   DEPRECATED VIA (λb.((λa.(λb.a)) b))
; UNDEPRECATED VIA STATIC NAME RESOLUTION
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
(= names (table))

(with bound (table)
      count 0
  (defcase bind-vars (exp)
    (or car:bound.exp string.exp)
    (let var arg.exp
      ; setup new binding
      (= names.count string.var)
      (push count bound.var)
      ++.count

      ; recurse with var bound and then unbind
      (let result (list 'λ dec.count bind-vars:body.exp)
        pop:bound.var
        result))
    (map bind-vars exp))

  (def static-name-resolve (exp)
    (= names (table)
       count 0)
    bind-vars.exp))

; and now for remapping to names
; given  '(λ 1 (λ 3  (1 (1 (1 (1 (1 (1 (1 (1 3 ))))))))))
; return '(λ b (λ b0 (b (b (b (b (b (b (b (b b0))))))))))
(let count (table)
  (defcase name-map (exp)
    names.exp
    (withs index arg.exp
           var   names.index
      (++ names.index count.var)
      (or= count.var -1)
      ++:count.var
      
      (let result (list 'λ names.index name-map:body.exp)
        --:count.var
        result))
    (map name-map exp))

  (def new-name-map (exp)
    (= count (table))
    name-map.exp))

(= sym-map [deep-map sym _])

; base cases
; a -> a
; (λ var. body) -> (λ var. norm-body)
; ((λ) r) -> expand λ with r -> normalize
; (l r) -> (l.norm r) 
(defcase normalize (exp)
  exp
  (fn-map normalize exp)
  (with left  normalize:car.exp
        right          cadr.exp
    (if function?.left
      (normalize:expand-fn left right)
      (list left normalize.right))))

(def expand-fn (f exp)
  (expand body.f arg.f exp))

; instead of deep-map, create a recursive function which travels through an expression replacing things inside the body, but not replacing within a function that has the same var
; base cases
; a -> <expanded> || a
; (λ x body) -> (λ x (expand body))
; (l r) -> expand both
; is there a clean way of passing references to a function so that it doesn't need to call itself with the same references over and over again?
(defcase expand (exp token input)
  (if (is exp token) input exp)
  (if (isnt arg.exp token)
    (fn-map [expand _ token input] exp)
    exp)
  (map [expand _ token input] exp))

(def assert args
  (map [apply iso _] (tuples 2 args)))

(= λid     '(λ x x)
   eight   '(λ b (λ d  (b (b (b (b (b (b (b (b d ))))))))))
   eight2  '(λ b (λ b0 (b (b (b (b (b (b (b (b b0)))))))))))

(def run-tests ()
  (assert                               (normalize 'a)   'a
                                       (normalize λid)   λid
                                   (expand-fn λid λid)   λid
                                   (interpret "(λxx)")   λid
                          (expand-fn λid '(λ y (y y)))   '(λ y (y y))
                 (interpret "(λ b ((λ a (λ b a)) b))")   '(λ b (λ b0 b))
    (interpret "((λa(λb(a(a(ab))))) (λc(λd(c(cd)))))")   eight
    (interpret "((λa(λb(a(a(ab))))) (λa(λb(a(ab)))))")   eight2))
