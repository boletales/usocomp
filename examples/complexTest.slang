function main() -> int
{
  (int, int) $c1 = (100, 200)
  (int, int) $c2 = (300, 400)
  (int, int) $ans = main.complexProd($c1, $c2)
  return $ans.1
}

function main.complexProd((int, int) $c1, (int, int) $c2) -> (int, int)
{
  int $re1 = $c1.0
  int $im1 = $c1.1
  int $re2 = $c2.0
  int $im2 = $c2.1
  int $reAns = (($re1 * $re2) - ($im1 * $im2))
  int $imAns = (($re1 * $im2) + ($im1 * $re2))
  return ($reAns, $imAns)
}