(l "test")

; (def triple ((c b a))
;   (is (+ sqr.a sqr.b) sqr.c))

; generate all unique sets of n numbers that sum exactly to m
(def n-over-m-partitions (n m)
  (if (is m 1)
    (sub ((n)))
    (mappend
      [map 
        (fn (partition) (cons _ partition)) 
        (n-over-m-partitions (- n _) dec.m)]
      (range (ceiling:/ n m) n))))

; (keep triple (n-over-m-partitions 3 1000))

; (def eratosthenese (n)
;   (accum new-prime
;     (let sieve (to-vec:n-of n t)
;       (for n 2 dec:len.sieve
;         (when sieve.n 
;           new-prime.n
;           (let marker sqr.n
;             (while (< marker len.sieve)
;               (= sieve.marker nil)
;               (++ marker n))))))))

(def primes<= (n)
  (accum add (for i 1 n prime&add.i)))

; (with n 0 i 0 (until (> len:factors.n 500) ++.i (++ n i)) n)

(def collatz (n)
  (if (is n 1) 1
      even.n   (inc:collatz:/ n 2)
               (inc:collatz:inc:* n 3)))

; (map collatz range.100)

; (let xs (mat-of-vec 21 21 1)
;   (last:flat:mapn
;     (fn (i j) 
;       (= xs.i.j
;         (+ (ref xs dec.i j) 
;            (ref xs i dec.j))))
;     1 20 1 20))

; (sum:digits:expt 2 1000)

(= mirror    sum:butlast:factors
   amicable? [and (is _ mirror:mirror._) (isnt _ mirror._)])

; (sum:keep amicable? range0.10000)

; (def pandigital? (n)
;   (withs xs digits.n
;          d  len.xs
;     (and (< d 10)
;          (no:pos 0 xs)
;          (is d len:dedup.xs))))

(def rotations (xs)
  (map-i
    (fn (el i) 
      (append (drop i xs) (take i xs)))
    xs))

; sum conflict with original definition
(def choose (n k)
  (let u (min k (- n k))
    (if (< u 0)
        0
        (/ (product (range (inc:- n u) n))
           factorial.u))))

; Euler Problem 
(= increasing? [sorted <= digits._]
   decreasing? [sorted >= digits._]
   bouncy?     [nor increasing?._ decreasing?._])

(def bouncy-frac (ratio)
  (with b 0
        n 1
    (until (is (/ b n) ratio)
      ++.n
      (if bouncy?.n ++.b))
    n))

; Euler Problem 42
; (let triangle? (table)
;   (for i 1 10000 set:triangle?:triangle.i)
; 
;   (count [triangle?:sum:map [- int._ 64] to-list._] words))

; Euler Problem 49
; (l "sets")
;
; (withs xs   (to-vec:keep prime? (range 1000 9999))
;        seen to-set.xs
;   (each-idx xs i
;     (for j inc.i dec:len.xs
;       (let n3 (- (* xs.j 2) xs.i) 
;         (if (and seen.n3 
;                  (apply is 
;                     (map [digs->num:sort < digits._] 
;                          (list xs.i xs.j n3))))
;           (prn xs.i " " xs.j " " n3))))))

; Euler Problem 92
; (defmemo sqr-chain (n)
;   (if (in n 1 89)
;     n
;     (sqr-chain:sum:map sqr digits.n)))
;
; (count id (mapn [is 89 sqr-chain._] 1 10000000))

; Euler Problem 58
; (with n      9
;       side   3
;       primes 3
;   (until (< (/ primes (dec:* side 2)) 1/10) 
;     (++ side 2)
;     (repeat 4
;       (if (prime?:++ n dec.side)
;         ++.primes)))
;   side)

; Euler Problem 55
; (= seen (table))
;
; (defmemo reverse-add (n)
;   (+ n digs->num:nrev:digits.n))
;
; (defmemo lychrel? (n)
;   (no:lychrel-iter reverse-add.n 50))
;
; (def lychrel-iter (n m)
;   (and positive.m
;        (or= seen.n
;             (or palindrome?:digits.n 
;                 (lychrel-iter reverse-add.n dec.m)))))
;
; (count lychrel? range0.10000)

; Euler Problem 57
; (defmemo expand (n)
;   (if zero.n
;     2
;     (+ 2 /:expand:dec.n)))
;
; (def root-2-iteration (i)
;   inc:/:expand.i)
;
; (count [> len:digits:numer._ len:digits:denom._] (mapn0 root-2-iteration 1000))

; Euler Problem 62
; (= cubes (generate 10000 [expt _ 3])
;    cube? to-set.cubes
;    perms (table))
;
; (ignore:map 
;   [let n (digs->num:sort > digits._)
;     (= perms.n (inc:or perms.n 0))
;     (if (is 5 perms.n)
;       prn.n)]
;   cubes)
;
; (keep [is 987655433210 (digs->num:sort > digits._)] cubes)

; Euler Problem 76
; write 'a' constrained by using 1 <= n <= 'b' to do it
; e.g. (summations 5 2) is 3, (summations 2 2) is 2
; (defmemo summations (a b)
;   (if (or (< a 2) (< b 2))
;     1
;     (sum:mapn [summations (- a _) _] 1 (min a b))))

; (def ways-to-sum-to (n)
;   (dec:summations n n))

; Euler Problem 65
; (+ 2 (convergent '(1 2 1 1 4 1 1 6 1 1 8 1 ...))) <- e
; (def convergent (xs)
;   (if empty.xs 0 (/:+ car.xs convergent:cdr.xs)))
;
; (def e-convergent (n)
;   (+ 2 (convergent:mapn0 e-convergent-num dec.n)))
;
; (def e-convergent-num (n)
;   (if (is (mod n 3) 1) (* 2/3 (+ n 2)) 1))
;
; sum:digits:numer:e-convergent.100

; Euler Problem 77
; write 'a' constrained by using 1 <= n <= 'b' to do it
; e.g. (summations 5 2) is 3, (summations 2 2) is 2
; (defmemo prime-summations (a b)
;   (if (is a 0) 1
;     (sum:map [prime-summations (- a _) _] (primes<=:min a b))))
;
; (def ways-to-sum-primes-to (n)
;   (- (prime-summations n n) (if prime?.n 1 0)))

; Euler Problem 85
; (defmemo grid-rects (n m)
;   (if zero.n 0
;     (+ (grid-rects dec.n m) (* n triangle.m))))
;
; (minumum:flat:mapn0 (compose [abs:- 2e6 _] grid-rects) 100 100)
; (pos 2.0 (flat:mapn0 (compose [abs:- 2e6 _] grid-rects) 100 100))

(def pandigital? (xs)
  (let d len.xs
    (and (< d 10)
         (no:pos 0 xs)
         (is d len:dedup.xs))))

(def first-n-digits (n m)
  (digits:floor:expt 10
    (+ dec.n decimal:log.m)))

; (keep [and (> _ 1e9) (pandigital?:digits:mod _ 1000000000) (pandigital?:first-n-digits 9 _)] fibs.10000)


; TODO!!
; (flat:map (fn (p) map [* p _] (eratosthenese:div (expt 10 3) p)) primes<=.1000)

(def semiprimes (n)
  (sum:map 
    [- (len:primes<=:div n _) len:primes<=:dec._]
    primes<=:floor:sqrt.n))

; (def binary-search (arr val left right)
;   (if (>= left right) left
;     (withs half  (div (+ left right) 2)
;            h-val arr.half
;       (if (is h-val val) half
;           (>  h-val val) (binary-search arr val left     dec.half)
;                          (binary-search arr val inc.half right)))))

(def b-search (arr val)
  (binary-search [< arr._ val] 0 dec:len.arr))

; (def nice-b-search (arr val)
;   (let idx (b-search arr val)
;     (and (< idx len.arr) (is arr.idx val) idx)))

; Euler Problem 17
; (def letter-count (n)
; ;                  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9
;   (if (< n 20)  ('(0 3 3 5 4 4 3 5 5 4 3 6 6 8 8 7 7 9 8 8) n)
; ;                     0 1 2 3 4 5 6 7 8 9
;       (< n 100) (+ ('(0 3 6 6 5 5 5 7 6 6) (div n 10))
;                    (letter-count:mod n 10))
;
;                 (+ (letter-count:div n 100)
;                    (let m (mod n 100)
;                      (if zero.m 7 (+ 10 letter-count.m))))))
;
; (+ (sum:mapn0 letter-count 1000) 11)

; Euler Problem 22
; (sum:map-i (fn (word i) (* inc.i (sum:map alph-pos to-list.word))) (sort < words))

; Euler Problem 19
; (= sundays 0)
;
; (with day     1
;       month   1
;       year    1900
;       weekday 1
;
; ; january february march april may june july august september october november december
; ; 31      28?      31    30    31  30   31   31     30        31      30       31
;   (def days-in-month (n)
;     (case n
;       1 31
;       2 (if (and (zero:mod year 4) (no:mod year 400)) 29 28)
;       3 31
;       4 30
;       5 31
;       6 30
;       7 31
;       8 31
;       9 30
;       10 31
;       11 30
;       12 31))
;
;   (def next-day ()
;     (= weekday (mod inc.weekday 7))
;     ++.day
;     (when (> day days-in-month.month)
;       (= day 1)
;       ++.month
;       (when (> month 12)
;         (= month 1)
;         ++.year)))
;
;   (until (is year 1901) (next-day))
;
;   (until (is year 2001)
;     (if (and (is day 1) (is weekday 0))
;       ++.sundays)
;     (next-day)))

; Euler Problem 23
; (with abundants (to-vec:keep [< (* 2 _) sum:factors._] range.28123)
;       result    (to-vec:n-of 28123 t)
;   (each-idx abundants i
;     (let n abundants.i
;       (catch:for j i dec:len.abundants
;         (let m (+ n abundants.j)
;           (if (>= m 28123) (throw))
;           (= result.m nil)))))
;  
;   (let sum 0
;     (times i 28123
;       (if result.i (++ sum i)))
;     (= yolo sum)))

; Euler Problem 27
; (maximum:flat:mapn (fn (a b) (first [prime?:+ sqr._ (* a _) b])) -999 999 -999 999)

; Euler Problem 26
; (def decimal-chain-length (n)
;   (while (multiple n 5) (= n (/ n 5)))
;   (while (multiple n 2) (= n (/ n 2)))
;   (if (is n 1)
;     0
;     (first [is 1 (mod (expt 10 _) n)])))

; Euler Problem 31
; (= coins '(1 2 5 10 20 50 100 200)
;    bigcoins rev.coins)
;
; (defmemo coin-sums (money max)
;   (if (or (< money 2) (< max 2))
;     1
;     (sum:map [coin-sums (- money _) _] (keep [<= _ (min money max)] bigcoins))))

; Euler Problem 37
; (= addable-digits '(1 2 3 5 7 9)
;    left-primes    '()
;    right-primes   '()
;    current-left   '(2 3 5 7)
;    current-right  '(2 3 5 7))
;  
; (repeat 5
;   (= current-left (keep prime?
;     (mappend
;       (fn (n)
;         (map [prn:+ (* n 10) _] addable-digits))
;       current-left)))
;
;   (= current-right (keep prime?
;     (mappend
;       (fn (n)
;         (map [prn:+ (* _ (expt 10 inc:magnitude.n)) n] addable-digits))
;       current-right)))
;
;   (++ left-primes  current-left)
;   (++ right-primes current-right))
;
; (= in-right-primes? to-set.right-primes)
;
; (= truncatable (keep in-right-primes?&id left-primes))

; Euler Problem 45
; (= triangles   (generate 1000000 triangle)
;    pentagonal? (to-set:generate 1000000 pentagonal)
;    hexagonal?  (to-set:generate 1000000 hexagonal))

; Euler Problem 39
; (def generate-triple (m n k)
;   (and (> m n) (odd:- m n) (is 1 (gcd m n))
;     (sort < (map [* k _] (list (- sqr.m sqr.n) (* 2 m n) (+ sqr.m sqr.n))))))
;
; (= triples (keep [<= sum._ 1000] (not-nil:flat1:flat1:mapn generate-triple 1 34 1 34 1 34)))
; (= freq (map sum triples))
; (maximum:map [count _ freq] range.1000)
; (keep [and (is 7 (count _ freq)) _] range.1000)
; 840

; Euler Problem 46
; (= composites (keep [and no:prime?._ odd._] range.10000))
;
; (def sqrs-under (n)
;   (map [* 2 sqr._] (range:floor:sqrt:/ dec.n 2)))
;
; (def goldbach (n)
;   (some [prime?:- n _] sqrs-under.n))
;
; (keep no:goldbach composites)

; Euler Problem 50
; find all sums of n primes in a row, for n = 100 to 550, that sum to under 1000000
; filter for primes, and order by # of primes added
; take the first one
;
; (= primes to-vec:primes<=.1000000)
;
; (catch:down chain-size 550 178
;   (= chain-sum (sum:mapn0 primes chain-size)
;      i         chain-size)
;   (until (> chain-sum 1000000)
;     (if prime?.chain-sum throw.chain-sum)
;     (++ chain-sum primes.i)
;     (-- chain-sum (primes:- i chain-size))
;     ++.i))

(def nearest-expt (base n)
  (expt base (ceiling:log n base)))

; Euler Problem 32
; xx * yyy = zzzz
; x * yyyy = zzzz
; (def pandigital? (xs)
;   (let d len.xs
;     (and (is d 9)
;          (no:pos 0 xs)
;          (is d len:dedup.xs))))
;
; (sum:dedup:accum add
;   (for i 1 99
;     (let i-digs digits.i
;       (for j inc.i 9999
;         (let k (* i j)
;           (if (pandigital?:+ i-digs digits.j digits.k) add.k))))))

; Euler Problem 52
; (def permuted? (n)
;   (with digs (sort < digits.n)
;     (all [iso digs (sort < digits._)] (mapn [* n _] 2 6))))
;
; (keep permuted? range.1000000)

; Euler Problem 33
; (def digit-canceling (n m)
;   (withs f      (/ n m)
;          n-digs digits.n
;          m-digs digits.m
;          p-n    (pos numer.f n-digs)
;          p-m    (pos denom.f m-digs)
;     (and (< n m)
;          p-n 
;          p-m
;          (is (n-digs:- 1 p-n)
;              (m-digs:- 1 p-m))
;          (no:multiple n 10))))
;
; (keep [apply digit-canceling _] (flat1:mapn list 10 99 10 99))

; Euler Problem 38
; (def pandigital-multiple (n)
;   (with total (list)
;         m     1
;     (until (>= len.total 9)
;       (++ total (digits:* n m))
;       ++.m)
;     total))
;
; (maximum:trues pandigital?&digs->num (map pandigital-multiple range.10000))

; Euler Problem 44
; (= pentagonals (generate 1000000 pentagonal)
;    pentagonal? to-set.pentagonals)
;
; (for i 1 3000
;   (for j inc.i 3000
;     (with p1 pentagonal.i
;           p2 pentagonal.j
;       (and (pentagonal?:- p2 p1)
;            (pentagonal?:+ p2 p1)
;            (prn:- p2 p1)))))

; (def totient (n)
;   (* n (product:map [- 1 /._] dedup:factor.n)))

; Euler Problem 43
; (def sub-string-divisibility (xs)
;   (let digs to-vec.xs
;     (all [multiple (digs->num:list (digs:+ _ 1) (digs:+ _ 2) (digs:+ _ 3)) primes._] range0.7)))
;
; (sum:map digs->num (keep sub-string-divisibility permutations:range0.10))

; Euler Problem 73
; (let n 0
;   (for i 1 11999
;     (for j inc.i 12000
;       (and (is 1 (gcd i j))
;            (< 1/3 (/ i j) 1/2)
;            ++.n)))
;   n)

; Euler Problem 206
; (= order (append range.9 '(0)))
;
; (def concealed-square? (n)
;   (let digs to-vec:digits.n
;     (all id (map-i (fn (d i) (is d (digs:* 2 i))) order))))
;
; (sqrt:car:find concealed-square?
;   (flat:mapn0
;     [sqr:floor:sqrt:digs->num:list 1 _a 2 _b 3 _c 4 _d 5 _e 6 0 7 0 8 0 9 0 0]
;     9 9 9 9 9))

(def substring (str i (o j 1))
  (let new-str ""
    (for iter i (min (+ i dec.j) dec:len.str)
      (++ new-str str.iter))
    new-str))

; Euler Problem 89
; (= prefix-pairs
;   '(("M"  1000)
;     ("CM"  900)
;     ("D"   500)
;     ("CD"  400)
;     ("C"   100)
;     ("XC"   90)
;     ("L"    50)
;     ("XL"   40)
;     ("X"    10)
;     ("IX"    9)
;     ("V"     5)
;     ("IV"    4)
;     ("I"     1)))
;
; (defmemo roman-numeral (n)
;   (let pair (find [>= n _.1] prefix-pairs)
;     (if pair
;       (+ pair.0 (roman-numeral:- n pair.1))
;       "")))
;
; (defmemo read-roman-numeral (str)
;   (let pair (find [in _.0 (substring str 0 2) (substring str 0 1)] prefix-pairs)
;     (if pair
;       (+ pair.1 (read-roman-numeral:substring str len:pair.0 len.str))
;       0)))
;
; (- (sum:map len numerals) (sum:map len:roman-numeral:read-roman-numeral numerals))

(def best-with-el (p f xs)
  (withs best-el  car.xs
         best     f.best-el
    (each x xs
      (let f-x f.x
        (when (p f-x best)
          (= best-el x
             best    f-x))))
    (list best-el best)))

(= minimize [best-with-el < _f _xs]
   maximize [best-with-el > _f _xs])

(def permutations? (xs1 xs2)
  (iso ascending.xs1 ascending.xs2))

; Euler Problem 70
; (def totient-permutation (n)
;   (let tot totient.n
;     (and (is magnitude.n magnitude.tot)
;          (permutations? digits.n digits.tot))))
;
; (minimize [/ _ totient._] (keep totient-permutation range.10000000))
; 500 seconds

; Euler Problem 102
; if there is one point above the origin, check that there are two lines above the origin
; if there are two points above, check that there are two below
; (def distance ((x1 y1) (x2 y2))
;   (sqrt:+ (sqr:- x2 x1)
;           (sqr:- y2 y1)))
;
; (def sidelengths-of (tri)
;   (list (distance tri.0 tri.1)
;         (distance tri.1 tri.2)
;         (distance tri.2 tri.0)))
;
; (def origin-triangle-sum (tri)
;   (sum:map area (sub (((0 0) tri.0 tri.1)
;                       ((0 0) tri.1 tri.2)
;                       ((0 0) tri.2 tri.0)))))
;
; (def area (tri)
;   (withs sides (cons 0 sidelengths-of.tri)
;          s     (/ sum.sides 2)
;     (sqrt:product:map [- s _] sides)))
;
; ; (def lines-of (pts)
; ;   (list (make-line pts.0 pts.1)
; ;         (make-line pts.1 pts.2)
; ;         (make-line pts.2 pts.0)))
;
; (def make-line ((x1 y1) (x2 y2))
;   (withs dx (- x2 x1)
;          dy (- y2 y1)
;          slope (/ dy dx)
;          y-intersect (- y1 (* slope x1))
;     (list slope y-intersect)))
;
; ; (def above-origin? (line)
; ;   positive:line.1)
;
; (def contains-origin? (tri)
;   (< (abs:- area.tri origin-triangle-sum.tri) 1e-4))
;
; ; (count contains-origin? triangles)

; 5e7
; (= sums (table))
;
; (mapn0 [set:sums:+ (expt primes._a 2)
;                    (expt primes._b 3)
;                    (expt primes._c 4)]
;    908 73 23)
;
; (count [< _ 5e7] keys.sums)

(def atkins (limit)
  (let prime? (table)
    (for x 1 (sqrt:/ limit 4)
      (catch:for y 1 sqrt.limit
        (let n (+ (* 4 sqr.x) sqr.y)
          (if (> n limit)
                  (throw)
              (in (mod n 12) 1 5)
                  toggle:prime?.n))))

    (for x 1 (sqrt:/ limit 3)
      (catch:for y 1 sqrt.limit
        (let n (+ (* 3 sqr.x) sqr.y)
          (if (> n limit)
                  (throw)
              (is 7 (mod n 12))
                  toggle:prime?.n))))

    (for y 1 (sqrt:/ limit 2)
      (catch:for x inc.y (sqrt:/ limit 2)
        (let n (- (* 3 sqr.x) sqr.y)
          (if (> n limit)
                  (throw)
              (is 11 (mod n 12))
                  toggle:prime?.n))))

    (for n 5 sqrt.limit
      (if prime?.n
        (withs n^2 sqr.n
               k   n^2
          (until (> k limit)
            wipe:prime?.k
            (++ k n^2)))))
    
    (list* 2 3 ascending:keys.prime?)))

(def first (f (o n 1))
  (until f.n ++.n)
  n)

; Euler Problem 71
; (= closest 0)
; (for i 1 1e6
;   (for j (ceiling:* i closest) (dec:ceiling:* i 3/7)
;     (if (coprime i j)
;       (= closest (/ j i)))))

; Euler Problem 104
; (def pandigital? (xs)
;   (let d len.xs
;     (and (< d 10)
;          (no:pos 0 xs)
;          (is d len:dedup.xs))))

; (def first-n-digits (n m)
;   (digits:floor:expt 10
;     (+ dec.n decimal:log.m)))

; (ignore:= mod-fibs (to-vec:cons 0 fibs.10000000))
; (ignore:= pan-fibs (keep pandigital?:digits:mod-fibs range0.1000000))
; (keep pandigital?:first-9-digit-fib pan-fibs)

(def normalized-expt (base power)
  (expt 10 (decimal:* power log.base)))

; (def first-9-digit-fib (n)
;   (first-n-digits 9
;     (/ (normalized-expt phi n) sqrt.5)))

; Euler Problem 74
; (defmemo sum-factorial-digits (n)
;   (sum:map factorial digits.n))
;
; (= SFD sum-factorial-digits
;    remaining (table))
;
; (def non-repeating (n)
;   (with unique 0
;         chain-size 1
;         thread2 n
;         seen (table)
;
;     (until (or seen.n remaining.n)
;       set:seen.n
;       (zap SFD n))
;
;     (= unique len.seen)
;
;     (if seen.n
;       (let looper SFD.n
;         (until (is n looper)
;           (zap SFD looper)
;           ++.chain-size)
;         (repeat chain-size
;           (zap SFD looper)
;           (= remaining.looper chain-size)))
;
;       (do 
;         (= chain-size remaining.n)
;         (++ unique chain-size)))
;
;     (down c unique chain-size
;       (= remaining.thread2 c)
;       (zap SFD thread2))
;     unique))
;
; (count [is 60 non-repeating._] range0.1000000)

; Euler Problem 80
; (def normalize-to-below-one (n)
;   (/ n (expt 10 inc:magnitude.n)))
;
; (def sum-100-digits-of-sqrt (n)
;   (sum:take 100 (digits:isqrt:* n (expt 10 200))))
;
; (sum:map sum-100-digits-of-sqrt (keep no:integer:sqrt range.100))

; Euler Problem 124
; (= radical product:dedup:factor)
;
; (def radical-ordering (a b)
;   (or (< a.1 b.1)
;       (and is a.1 b.1
;            (< a.0 b.0))))
;
; (time:ignore:= k (sort radical-ordering (mapn [list _ radical._] 1 100000)))

(def pent-gen (n)
  (pentagonal:/ (if odd.n inc.n -.n) 2))

(defmemo partitions (n)
  (if zero.n 1
    (with total 0 sign 1 i 1 k 1
      (while (>= (- n k) 0)
        (++ total (* sign (partitions:- n k)))
        ++.i
        (if odd.i (zap - sign))
        (= k pent-gen.i))
      total)))

; n(n-1) * 2 = m(m-1)
; n(n-1) * 2 - (n+k)(n+k+1) = 0
; 2n^2 - 2n - n^2 - 2nk - k^2 - n - k = 0
; n^2 - 2nk - k^2 - 3n - k = 0
; k ~ n * (sqrt(2) - 1)

; m(m-1) is divisible by 4

; Given the set of all integers formed by n(n-1) with an integer n, find pairs that divide each other by two
; or more specifically, start at 1e12 and start going up, looking for one that's half as big
; has to be a multiple of 4
(= off-sqr [* _ inc._])

(def in-set (n)
  (and even.n (is n off-sqr:isqrt.n)))

; (first in-set:/2:off-sqr (^ 10 12))

; How do I Diophantine Equations? I honestly have no idea.
; (with t 21 b 15
;   (while (< t 1e12)
;     (with b_temp (+ (* 3 b) (* 2 t) -2)
;           t_temp (+ (* 4 b) (* 3 t) -3)
;       (= b b_temp
;          t t_temp)))
;     b)

(def diophantine-reciprocals (n)
  (+ 2 (countn [multiple (* _ n) (- _ n)] (+ n 2) (dec:* n 2))))

(def super-composite (xs)
  (product:map [expt _a _b] (take len.xs primes) xs))

; (time:ascending:keep [> diophantine-reciprocals._ 1e3] (map super-composite (n-over-m-partitions 8 8)))

; Euler Problem 125
; (= sqr-sum-palindromes (list))
;
; (for i 1 10000
;   (with n   inc.i
;         sum sqr.i
;     (until (>= sum 1e8)
;       (++ sum sqr.n)
;       ++.n
;       (if palindrome?:digits.sum
;         (push sum sqr-sum-palindromes)))))
;
; sum:dedup.sqr-sum-palindromes

(def fast-factor-count (n)
  (let prime-factor-counts counts:factor.n
    (product:map inc:prime-factor-counts keys.prime-factor-counts)))

; (time:ignore:= factor-counts (to-vec:cons 0 (mapn fast-factor-count 1 1e7)))
;
; (countn [is factor-counts._ factor-counts:inc._] 1 dec.1e7)

(= primes (to-vec:cons 0 primes<=.1e6))

; (def euler-remainder (n)
;   (mod (+ (expt dec:primes.n n) (expt inc:primes.n n)) sqr:primes.n))
;
; (first [> euler-remainder._ 1e10])

; Euler Problem 119
; Generate the powers of all n < 60 to until > 1e7. check each power for digit sum equality and put in a list. sort list.
; (= power-digit-sums (list))
;
; (for n 2 200
;   (let power n
;     (until (> power 1e30)
;       (and (> power 9)
;            (is n sum:digits.power)
;            (push power power-digit-sums))
;       (= power (* power n)))))
;
; (= sums ascending:dedup.power-digit-sums)

; TODO: INCORRECT
; Euler Problem 122
; sum the first 200 most efficient additions, where efficient is the fewest number of additions starting from 1.
; (= efficiency (table 1 0))
; (for i 2 100
;   (= efficiency.i
;     (inc:minimum:mapn 
;       [+ (max efficiency._ (efficiency:- i _))] 1 (div i 2))))
; (sum:mapn efficiency 1 100)

; Euler Problem 59
; (def decrypt (message key)
;   (let offset 0
;     (string:map [do1 (char:bit-xor _ (key:mod offset len.key)) ++.offset] message)))
;
; (fors (a b c) 97 122
;   (let result (decrypt message (list a b c))
;     (if (posmatch "the" result) (prn a " " b " " c " " (substring result 0 15)))))
;
; '(103 111 100)

; Euler Problem 79 was solvable by hand, with all unique digits, relatively quickly.

; Euler Problem 113
; seems like a combinitorics problem: find the number of paths from one corner to the opposite corner in a rectangular grid
; should be something like n+9 choose 9
; 1 1 1 1 1 1 1 1
; 1 2 3 4 5 6 7 8
; 1 3 6 
; adding them all up is equivelent to choosing one more
;
; (def not-bouncy-for-n-digits (n)
;   (+ (choose (+ n 8) 8) (choose (+ n 9) 9) -10))
;
; (def not-bouncy-for-up-to-n-digits (n)
;   (+ (choose (+ n 9) 9) (choose (+ n 10) 10) (* -10 n) -2))

; Euler Problem 68
; (def permutation-helper (n arr f)
;   (if zero.n
;     f.arr
;     (times i n
;       (permutation-helper dec.n arr f)
;       (swap (arr:if even.n 0 i) arr:dec.n))))

; (def each-permutation (arr f)
;   (permutation-helper len.arr arr f))

; (= solutions (list))

; (def test-5-gon (arr)
;   (and (< (pos 10 arr) 5)
;        (all [< arr.0 arr._] range.4)
;        (apply is (map [+ arr._ (arr:+ _ 5) (arr:+ (mod inc._ 5) 5)] range0.5))
;        (push to-list.arr solutions)))
;
; (each-permutation to-vec:range.10 test-5-gon)

; 529
; (sum [/ (dec:expt 2.0 dec._) _] range.30)

; 329
; (= sequence '(t t t t nil nil t t t nil t t nil t nil))
;
; (defmemo prime-frog (start seq)
;   (let (expected . remaining) seq
;     (if no.seq 1
;         (* 
;           1/3
;           (if (is expected prime.start) 2 1)
;           (case start
;             1   (prime-frog inc.start remaining)
;             500 (prime-frog dec.start remaining)
;                 (/ (+ (prime-frog inc.start remaining)
;                       (prime-frog dec.start remaining))
;                    2))))))
;
; (/ (sumn [prime-frog _ sequence] 1 500) 500)


