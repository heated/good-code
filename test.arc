(l "core")
(l "sets")

(= triangle      sum:range
   ; now with efficiency
   triangle      [* _ inc._ 1/2]
   factorial     memo.product:range
   generate-some [generate 20 _]
   prime-factors [keep prime? factors._]
   digits        num->digs
   alph-pos      [- int._ 64]
   pentagonal    [* _ (dec:* 3 _) 1/2]
   hexagonal     [* _ (dec:* 2 _)]
   magnitude     floor:log
   ascending     [sort < _]
   descending    [sort > _]
   prime?        prime
   /2            [/ _ 2]
   div2          [div _ 2]
   negative      [< _ 0])

(def fibs (n)
  (with a 1 b 1
    (accum add
      (repeat n
        add.a
        (let temp b
          (= b (mod (+ b a) (expt 10 9)))
          (= a temp))))))

(def one-off-words (word list)
  (keep [is 1 
    (count
      [isnt _b._a word._a] 
      range0:len.word)]
    list))

(def unique (xs)
  (with seen (table)
    (accum add
      (on el xs
        (unless seen.el add.el)
        set:seen.el))))

; given two intervals described each by two numbers on a number line, find the intersect and the section of each interval with no overlap
; let a range be [l, r] for [left, right] 
(= valid-range? [< _.0 _.1])

(def overlap ((l1 r1) (l2 r2))
  (withs lmax      (max l1 l2)
         rmin      (min r1 r2)
         both      (list lmax rmin)
         intersect (and valid-range?.both both)
    
    (obj range1 (keep valid-range? (sub ((l1 lmax) (rmin r1))))
         range2 (keep valid-range? (sub ((l2 lmax) (rmin r2))))
         both   intersect)))

; return positions of pairs of numbers within a list that sum to zero
; cubic time, but not for vectors
(def pair-zero-sum (xs)
  (accum add
    (each-index xs i
      (for j inc.i dec:len.xs
        (if (zero:+ xs.i xs.j)
          (add:list i j))))))

; linear time
; TODO: solution is not sorted
(def pair-zero-sum (xs)
  (let seen (table)
    (accum add
      (on el xs
        (each idx seen:-.el
          (add:list idx index))
        (push index seen.el)))))

(def powerset ((x . xs))
  (if empty.x
    '(())
    (let set powerset.xs
      (+ set (map [cons x _] set)))))

(def permutations (xs)
  (if empty.xs
    '(())
    (sum:map-i
      [map 
        (fn (el) (cons _el el)) 
        (permutations:delete-at xs _i)]
      xs)))

(def bubble (xs)
  (if single.xs
    xs
    (let (a b) xs 
      (cons (min a b) (bubble:cons (max a b) cddr.xs)))))

(def bubble-sort (xs)
  (repeat len.xs
    (zap bubble xs))
  xs)

; collision with built in sort function
; (def merge (l1 l2)
;   (+ (accum add
;        (while (and l1 l2)
;          (add:if (< l1.0 l2.0) pop.l1 pop.l2)))
;      l1 l2))
;
; (def merge-sort (xs)
;   (let half (div len.xs 2)
;     (if zero.half 
;       xs
;       (merge (merge-sort:take half xs)
;              (merge-sort:drop half xs)))))

(def substr (str sub)
  (catch:for i 0 (- len.str len.sub)
    (point break
      (on chr sub
        (if (isnt (str:+ i index) chr) (break)))
      throw.i)))

(for n 1 100
  (prn:case (gcd n 15)
    1 n
    3 'Fizz
    5 'Buzz
      'FizzBuzz))

; usage: (fizzbuzz 100 3 "fizz" 5 "buzz")
(def fizzbuzz (n . schema)
  (for i 1 n
    (prn:let xs (trues [if (multiple i _.0) _.1] (tuples 2 schema))
      (if xs sum.xs i))))

; generate something like '(1 "1 " 2 "2 " 3 "3 " 4 "4 ")
(def factors-schema (n) 
  (mappend [list _ (string _ " ")] range.n))

; print all the factors of each number up to n
(def all-factors (n)
  (apply fizzbuzz (cons n factors-schema.n)))

(def palindrome? (xs)
  (let half (div len.xs 2)
    (iso 
      (take half xs) 
      (rev:drop (- len.xs half) xs))))

(def worstn-int (n f xs)
  (bestn n [< f._a f._b] xs))

(def closest (n str dictionary)
  (worstn-int n
    [diff-words str _] 
    dictionary))

(def diff-words (str1 str2)
  (let result 0
    (on el str1
      (if (isnt el str2.index)
        ++.result))
    result))

(def strictly-increasing-factors (n)
  (let last 0
    (for i 1 n
      (let facts len:factors.i
        (when (> facts last)
          (= last facts)
          (prn i " " last))))))

(def longest-occurrence (str chr)
  (with ans  0 
        temp 0
    (each el str
      (if (is el chr)
        ++.temp
        (= ans (max ans temp)
           temp 0)))
    ans))

(def shuffle (xs)
  (with v to-vec.xs
        u len.xs
    (times i u
      (swapv v i (rand i dec.u)))
    to-list.v))

(def print-depths (tree)
  (let row list.tree
    (until empty.row
      (prn:map car row)
      (= row (not-nil:mappend [list cadr._ caddr._] row)))))

; Given a set of numbers, find two distinct subsets whose sums differ by N.
(def subset-difference ((x . xs) n)
  (if zero.n t
      empty.x nil
      (some [subset-difference xs (+ n _)]
        (list 0 x -.x))))

(def sicp-1-3 xs (- (sum:map sqr xs) sqr:minimum.xs))

(def memoize (f)
  (let hash (table)
    (fn args
      (or= hash.args (apply f args)))))

; defmemo is a builtin macro, along with memo
(defmemo fib (n)
  (if (< n 2) n 
    (+ (fib:- n 1) 
       (fib:- n 2))))

; (= fib memoize.fib)

(def fib (n)
  (ref (mat-expt '((0 1) (1 1)) n) 0 1))

(def cyclic? (xs)
  (catch:with slow xs
              fast xs
    (while fast
      (zap cddr fast)
      (zap cdr slow)
      (if (is slow fast) throw.t))))

(def cycle (xs)
  (scdr last-pair.xs xs)
  t)

(defmemo ackerman (a b)
  (if zero.a          inc.b
      zero.b          (ackerman dec.a 1)
      (ackerman dec.a (ackerman a dec.b))))

(def levenshtein (str1 str2)
  (withs l1  len.str1
         row range0:inc.l1

    (on char str2
      (let next list.index
        (times i l1
          (push
            (inc:min
              (- pop.row (if (is char str1.i) 1 0))
              car.next
              car.row)
            next))
        (= row nrev.next)))
    row.l1))

(def matching-parens? (str)
  (catch:let paren-count 0
    (each char str
      (++ paren-count (case char #\( 1 #\) -1 0))
      (if (< paren-count 0) throw.nil))
    zero.paren-count))

(def partial (f . args)
  (fn rest-args
    (apply f (+ args rest-args))))

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
    rev.current-list))

; ; substr
; (def split-str (str (o match " "))
;   (accum push
;     (let idx (substr match str)
;       (while idx
;
;         (= idx (substr match str))))))


; the graph is a list of nodes and their connections
; (= a '(b c d))
; (= b '(c d))
; (= c '(d))
; (= d '())
;
; (= edges cdr)
; (= value car)
;
; (def map-nodes (f graph)
;   (map f:value graph))
;
; (def map-dir-edges (f graph)
;   (each node graph
;     (each edge edges.node
;       f.edge)))
;
; (def map-edges (f graph)
;   (let seen (table)
;     (each node graph
;       (each edge node
;         (unless seen.edge)))))


; (def ufind (x) 
;   (if (is x uf.x) 
;     x 
;     (= uf.x ufind:uf.x)))
;
; (def mix (x y) (= uf:ufind.x ufind.y))


; (def curry (f num . args)
;   (-- num len.args)
;   (let curry-fn (fn new-args
;                   (++ args new-args)
;                   (-- num len.new-args)
;                     (if (<= num 0)
;                       (apply f args)
;                       curry-fn))
;     curry-fn))


; ; ideally, allow the creation of functions in the following manner:
; (def-w/obj foo (a b c (d 1 e 2))
;   (+ a b c d e))
;
; ; which is called in the following manner:
; (foo a 2 b 3 c 4)
;
; ; it would be nice to have errors on unsatisfied variables and optional stuff have values
; (foo a 2) ; errors with "expected arguments b, c"
;
; (foo f 4) ; errors with "unexpected f; expected arguments b, c"
;
; ; it seems like a good idea to return macros that take their inputs, look for unnacceptable arguments, and use with to replace arguments and do a thing
;
; (def foo args
;   (with args (apply table args)
;     ))
;
; (mac defwithobj (name args . body)
;   `(mac ,name args
;     `(with ,@args ,,@body)))
;
; (mac foo args
;   `(with ,@args ))

; LEL
; (def expt2 (n) (if zero.n 2 (sum:map expt2 range0.n)))


