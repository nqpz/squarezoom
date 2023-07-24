def spread_2d 't [l] (k: i64) (n: i64) (x: t) (is: [l](i64, i64)) (vs: [l]t): *[k][n]t =
  scatter_2d (replicate k (replicate n x)) is vs
