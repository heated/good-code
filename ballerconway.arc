(l "sets")

(= moore-neighborhood (remove '(0 0) (flat1:mapn list -1 1 -1 1)))

(def translate (cells dir)
  (map [map + _cell dir] keys.cells))

(def neighborhood (cells)
  (counts:mappend [translate cells _] moore-neighborhood))

(def next-gen (cells)
  (table-map-to-values 
    (fn (key count) (case count 3 t 2 cells.key)) 
    neighborhood.cells))

(def conway (cells gens)
  (zap to-set cells)
  (repeat gens
    prn:ppr:keys.cells
    (zap next-gen cells)))

(= blinker '((1 2) (2 2) (3 2)))
(= glider '((1 0) (2 1) (0 2) (1 2) (2 2)))

; golfed - 193 chars
; (l "sets")(def c(s i)(zap to-set s)(repeat i prn:ppr:keys.s(= s(table-map-to-values[case _b 3 t 2 s._a](counts:mappend(fn(d)(map[map + _ d]keys.s))(remove'(0 0)(flat1:mapn list -1 1 -1 1)))))))
