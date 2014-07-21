; endgame: interpret this correctly
; (λx.xx)(λx.xx)
; λx.xxx
; (λab.a(a(ab)))(λab.a(ab))

; (interpret "(λa.λb.a(a(a b))) (λc.λd.c(c d))")

(l "core")

(= arg       cadr
   body      cddr
   names     (table)
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
  (read:+ "(" (multisubst '(("." " ") ("λ" "λ ")) str) ")"))

; given  ("(" "λ" "x" "x" ")")
; return '(λ x x)
; (def abstract-syntax-tree (tokens)
;   (with stack        '()
;         current-list '()
;     (each token tokens
;       (case token
;         "(" (do 
;               (push current-list stack)
;               (= current-list '()))
;         ")" (let parent pop.stack
;               (push rev.current-list parent)
;               (= current-list parent))
;         (push sym.token current-list)))
;     rev.current-list))


; go through the tree with a set of bound variables
; all encountered variables that are unbound become strings
; pass the hash through stuff recursively
; bind when going into a function
; unbind when jumping out of a function
; ignore inner function declarations that are the same
;
; unbinding is the hardest - how do you know when you "come back out" of a branch?
; solution: recursive, with unbinds after function calls

; base cases
; a -> a | "a"
; (λ ...) -> complicated shit
; (l r) -> go into each and cons
(let index 0
  (def snr (exp (o bound (table)))
    (if atom.exp  (or car:bound.exp string.exp)
        no:λ?.exp (map [snr _ bound index] exp)
      
        (let var arg.exp
          (= names.index var)
          (push index bound.var)
          ++.index

          (let result (new-λ dec.index (snr body.exp bound))
            pop:bound.var
            result))))

  (def static-name-resolve (exp)
    (= index 0)
    snr.exp))

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
        (let result (new-λ names.index (name-map body.exp depth))
          --:depth.var
          result))))

; base cases
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

(def expand-λ (λ exp)
  (expand body.λ arg.λ exp))

; instead of deep-map, create a recursive function which travels through an expression replacing things inside the body, but not replacing within a function that has the same var
; base cases
; a -> <expanded> || a
; (λ x body) -> (λ x (expand body))
; (l r) -> expand both
(def expand (exp token input)
  (if atom.exp (if (is exp token) input exp)
      λ?.exp   (λ-map [expand _ token input] exp)
               (map [expand _ token input] exp)))

; turn exp into string
; put periods between functions
(def prettify-λ (exp)
  ; string.exp

  ; (string "λ" arg.exp "." prettify-λ:body.exp)

  ; (+ "(" (sum:map prettify-λ exp) ")"))
  exp)

(def assert args
  (map [apply iso _] (tuples 2 args)))

(def run-tests ()
  (assert                               (normalize 'a)   'a
                                (normalize '(λ x . x))   '(λ x . x)
                                    (interpret "λx.x")   '(λ x . x)
                          (interpret "λb.(λa.λb.a) b")   '(λ b λ b0 . b)
        (interpret "(λa.λb.a(a(a b))) (λc.λd.c(c d))")   '(λ b λ d  b (b (b (b (b (b (b (b d ))))))))
        (interpret "(λa.λb.a(a(a b))) (λa.λb.a(a b))")   '(λ b λ b0 b (b (b (b (b (b (b (b b0))))))))))
