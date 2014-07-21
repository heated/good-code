(= none no:some)

(= sum [reduce + _])

(= product [reduce * _])

(= range0 [range 0 dec._])

(= not-nil [keep id _])

(def generate (n f) (map f range.n))

(= same-count [count id (map is _a _b)])

(= ignore [do _ t])

(= to-list [as cons _]) 

(= to-vec [as vec _])

(= n-of-vec to-vec:n-of)

(= xlen len)

(= ylen [len _.0])

(= iterable? [in type._ 'vec 'cons])

(mac next (xs) `(zap cdr ,xs))

(def remove (el xs)
  (rem [iso el _] xs))

(mac or= (exp val)
  `(or ,exp (= ,exp ,val)))

(def prime? (n)
  (and (isnt n 1) (none [multiple n _] (range 2 sqrt.n))))

(def delete-at (xs idx) 
  (+ (take idx xs) (drop inc.idx xs)))

(def mapcons (f pair)
  (cons f:car.pair f:cdr.pair))

(mac sub (exp) 
  (if cons?.exp
    `(cons (sub ,car.exp) (sub ,cdr.exp)) 
    exp))

(mac before-returning (x . xs)
  `(let return-value ,x
     ,@xs
     return-value))

(def map-i (f xs (o n 0))
  (if xs 
    (cons (f car.xs n) 
      (map-i f cdr.xs inc.n))))

(mac times (iter n . body)
  `(for ,iter 0 (dec ,n) ,@body))

(mac each-idx (xs iter . body)
  `(times ,iter (len ,xs) ,@body))

(def map-range0 (f xs)
  (apply mapn f (mappend [list 0 dec._] xs)))

(def mapn0 (f . xs) 
  (map-range0 f xs))

(def factors (n)
  (let under-sqrt (keep [multiple n _] range:sqrt.n)
    (dedup:+ under-sqrt (rev:map [/ n _] under-sqrt))))

(def near-count (xs1 xs2)
  (- (sum:map [min (count _ xs1) (count _ xs2)] dedup.xs1)
     (same-count xs1 xs2)))

(def join-on (xs str)
  (apply string (intersperse str xs)))

(def new-matrix ((x . dims) (o val 0))
  (if x
    (n-of x new-matrix.dims)
    val))

(def mat dims new-matrix.dims)

(def deep-copy (xs)
  (if atom.xs
    xs
    (map deep-copy xs)))

(def dims (mat)
  (if iterable?.mat
    (cons len.mat dims:mat.0)
    nil))

(def in-range (val left right)
  (and (>= val left) (<= val right)))

(def in-range0 (val max)
  (in-range val 0 dec.max))

(def reference (obj (i . xs))
  (if i
    (reference obj.i xs)
    obj))

(def ref (obj . xs)
  (reference obj xs))

(def index? (i xs)
  (and iterable?.xs (in-range0 i len.xs)))

(def referenceable? (obj (i . xs))
  (no:xor i 
    (and i
         (index? i obj)
         (referenceable? obj.i xs))))

(def ref? (obj . idxs)
  (referenceable? obj idxs))

(mac matrix-of ((n . dims) exp)
  (if n
    `(n-of ,n (matrix-of ,dims ,exp))
    exp))

(mac mat-of args
  `(matrix-of ,butlast.args ,last.args))

(def deep-to-vec (xs)
  (if iterable?.xs
    (to-vec:map deep-to-vec xs)
    xs))

(mac mat-of-vec args
  `(deep-to-vec:mat-of ,@args))

(mac each-idces (xs (i . idxs) . body)
  (if i
    `(each-idx ,xs ,i
       (each-idces (ref ,xs ,i) ,idxs
         ,@body))
    `(do ,@body)))

; given '(a b c d (e f) (g h))
; return '((a b c d) (e f) (g h))
(def wrap-tokens (xs)
  (cons 
    (accum add
      (while (and xs sym?:car.xs no:ssyntax:car.xs)
        add:pop.xs))
    xs))

(mac each-index (xs . body)
  (let body wrap-tokens.body
    `(each-idces ,xs ,@body)))


; union the keys and use that to index the tables for arguments to apply to a function
; (def map-unioned-values (f tables)
;   (table-map-values f union-tables.tables))

; (def list- (list1 list2)
;   (keep no:iso ))

; (mac deep-each (xs x . body)
;   `(if (iterable? ,xs)
;       (deep-each 
;         ,@body)
;       (let ,x ,xs 
;         ,@body)))

; (mac deep-as (type obj)
;   `(if (iterable? ,obj)
;       (as ,type (map [deep-as ,type _] ,obj))
;       ,obj))




; (times i xlen.board
;   (times j ylen.board
;     ...))

; (deep-each board spot
;   ...)

; holy items
; (macro xs xs)    - "N/A"
; (fn xs xs)       - list
; [map id _]       - list
; [apply list _]   - id
; (fn ((l . r)) l) - car
; (fn xs car.xs)   - first
; (reduce max xs)  - max
; (fn xs len.xs)   - argslen
