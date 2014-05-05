; (l "test") to load within repl

(= none no:some)

(= sum [reduce + _])

(= product [reduce * _])

(= sqr-arr [map square _])

(= triangle sum:range)

; now with efficiency
(= triangle [* _ inc._ 1/2])

(= factorial product:range)

(= range0 [range 0 dec._])

(= not-nil [keep id _])

(= same-count [count id (map is _a _b)])

(def sample (xs) xs:rand:len.xs)

(def prime? (n)
  (and (isnt 1 n) (none [multiple n _] (range 2 sqrt.n))))

(def delete-at (xs idx) 
  (+ (take idx xs) (drop inc.idx xs)))

(def worstn-int (n f xs)
  (bestn n [< f._a f._b] xs))

(def map-i (f xs (o n 0))
  (and xs (cons (f car.xs n) (map-i f cdr.xs inc.n))))

(def unique (xs)
  (with seen (table)
        result nil
    (on el xs
      (unless seen.el (push el result))
      (= seen.el 't))
    rev.result))

(def factors (n)
  (let under-sqrt (keep [multiple n _] range:sqrt.n)
    (unique:+ under-sqrt (rev:map [/ n _] under-sqrt))))

(= prime-factors [keep prime? factors._])

(def near-count (xs1 xs2)
  (- (sum:map [min (count _ xs1) (count _ xs2)] unique.xs1)
     (same-count xs1 xs2)))

(def fibs (n)
  (let arr '(1 1)
    (repeat n (push (+ arr.0 arr.1) arr))
    (take n rev.arr)))

(def one-off-words (word list)
  (keep [is 1 
    (len:keep 
      [isnt _b._a word._a] 
      range0:len.word)]
    list))

; return positions of pairs of numbers within a list that sum to zero
(def pair-zero-sum (xs)
  (let result nil
    (for i 0 dec:len.xs
      (for j inc.i dec:len.xs
        (if (zero:+ xs.i xs.j)
          (push (list i j) result))))
    result))

(def powerset (xs)
  (if empty.xs
    '(())
    (with el car.xs
          set powerset:cdr.xs
      (+ set (map [cons el _] set)))))

(def permutations (xs)
  (if empty.xs
    '(())
    (sum:map-i
      [map 
        (fn (el) (cons _el el)) 
        (permutations:delete-at xs _i)]
      xs)))

(def swap (xs i j)
  (= temp xs.i
     xs.i xs.j
     xs.j temp))

(def bubblesort (arr)
  (repeat len.arr
    (for i 1 dec:len.arr
      (let j dec.i
        (if (< arr.i arr.j)
          (swap arr i j)))))
  arr)

(def merge (l1 l2)
  (= result nil)
  (while (and l1 l2)
    (push
      (if (< l1.0 l2.0) pop.l1 pop.l2)
      result))
  (+ rev.result l1 l2))

(def merge-sort (list)
  (let half (div len.list 2)
    (if 
      zero.half list
      (merge
        (merge-sort:take half list)
        (merge-sort:drop half list)))))

(def substr (str sub)
  (catch:for i 0 (- len.str len.sub)
    (point break
      (on chr sub
        (if (isnt (str:+ i index) chr) break.1))
      throw.i)))

(for n 1 100
  (prn:case (gcd n 15)
    1 n
    3 'Fizz
    5 'Buzz
      'FizzBuzz))

; schema being something like '((3 "fizz") (5 "buzz"))
(def fizzbuzz (n schema)
  (for i 1 n
    (prn:let 
      xs (trues [if (multiple i _.0) _.1] schema)
      (if xs 
        sum.xs
        i))))

; generate something like '((1 "1 ") (2 "2 ") (3 "3 "))
(def factors-schema (n) (map [list _ (string _ " ")] range.n))

; print all the factors of each number up to n
(= all-factors [fizzbuzz _ (factors-schema _)])

(def palindrome? (xs)
  (let half (div len.xs 2)
    (iso 
      (take half xs) 
      (rev:drop (- len.xs half) xs))))

(def closest (n str dictionary)
  (worstn-int 
    n
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
  (with ans 0 temp 0
    (each el str
      (if (is el chr)
        ++.temp
        (= ans (max ans temp)
           temp 0)))
    ans))

(def shuffle (xs)
  (with v ($.list->vector ($.ac-denil xs))
        u len.xs
    (for i 0 dec.u
      (swapv v i (rand i dec.u)))
    ($.vector->list v)))

(def mastermind ()
  (withs colors     '(r o y g b v)
         rand-color [sample colors]
         gen-guess  [map rand-color range.4]
         secret     (gen-guess)
         guesses    10

    (readline)
    (prn "Welcome to Mastermind. You have " guesses " guesses to diffuse the bomb.")
    (prn "The available colors are " colors)
    (prn "Example guess: \"r o y r\"")

    (catch:until zero.guesses
      (prn guesses " guesses left.")

      (withs guess  (map sym (tokens:readline))
             hits   (same-count secret guess)
             misses (near-count secret guess)

        (prn hits " correct and " misses " close misses.")
        (if (is hits 4) (throw)))

      --.guesses)

    (prn:if zero.guesses
      "I'm sorry. You died in the blast."
      "Congratulations, that's correct!")))

(def print-depths (tree)
  (with row list.tree
        unwrap [reduce append _]
    (until empty.row
      (prn:map car row)
      (= row (not-nil:unwrap:map [list cadr._ caddr._] row)))))

; Given a set of numbers, find two distinct subsets whose sums differ by N.

(def subset-difference (xs n)
  (if zero.n 't
      empty.xs nil
      (let recurse [weights cdr.xs _]
        (or recurse.n
            (recurse:- n car.xs)
            (recurse:+ n car.xs)))))

(def sicp-1-3 xs (- (sum:map sqr xs) (sqr:best < xs)))

(prn:sum:map [gcd 15 _] (range 3 117))
