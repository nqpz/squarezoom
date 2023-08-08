def log2 (n: i64): i32 = t32 (f32.log2 (f32.i64 n))

def spread_2d 't [l] (k: i64) (n: i64) (x: t) (is: [l](i64, i64)) (vs: [l]t): *[k][n]t =
  scatter_2d (replicate k (replicate n x)) is vs

def reduce_by_index_and_spread_2d 'a [n] (k: i64) (m: i64) (f: a -> a -> a) (ne: a) (is: [n](i64,i64)) (as: [n]a): *[k][m]a =
  reduce_by_index_2d (replicate k (replicate m ne)) f ne is as
