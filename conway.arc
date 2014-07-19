(l "core")

(def cell-value (gen x y)
  (if (ref? gen x y) gen.x.y 0))

(def neighbor-sum (gen x y)
  (- (sum:flat:mapn 
        [cell-value gen (+ x _dx) (+ y _dy)]
        -1 1 -1 1)
    gen.x.y))

(def new-cell (gen x y)
  (case (neighbor-sum gen x y)
    3 1
    2 gen.x.y
    0))

(def next-gen (gen)
  (deep-to-vec:map-range0 [new-cell gen _i _j] dims.gen))

(def print-grid (grid)
  (each row grid
    (each cell row 
      (pr:if zero.cell "." "#"))
    (prn))
  (prn))

(def conway (grid gens)
  (let grid deep-to-vec.grid
    (repeat gens
      print-grid.grid
      (zap next-gen grid))))

(= blinker '((0 1 0)
             (0 1 0)
             (0 1 0)))
