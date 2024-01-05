function main() -> int {
  int $x = 100
  (int, int, int) $str = (200, 300, 400)
  int $y = ($x + $str.2)
  return $y
}