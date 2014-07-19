(l "core")

(def to-set (xs)
  (let hash (table)
    (each x xs set:hash.x)
    hash))

(def set-of args to-set.args)

(def table-map (f hash)
  (let result (table)
    (maptable (compose (fn ((key value)) (= result.key value)) f) hash)
    result))

(def set-map (f hash)
  (table-map (fn (key value) (list f.key t)) hash))

(def table-map-to-values (f hash)
  (table-map [list _key (f _key _value)] hash))

(def table-map-values (f hash)
  (table-map-to-values (fn (key value) f.value) hash))

(def union-tables (tables)
  (let result (table)
    (each key (dedup:mappend keys tables)
      (= result.key (trues [_ key] tables)))
    result))
