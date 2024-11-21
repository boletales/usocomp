function main() -> int
{
  (int, int) $0 = (100, 200)
  (int, int) $2 = (300, 400)
  int $4 = 1111111
  (int, int) $5 = main.complexProd($0, $2)
  int $7 = 2222222
  return $5.1
}

function main.complexProd((int, int) $A0, (int, int) $A1) -> (int, int)
{
  int $0 = $A0.0
  int $1 = $A0.1
  int $2 = $A1.0
  int $3 = $A1.1
  int $4 = (($0 * $2) - ($1 * $3))
  int $5 = (($0 * $3) + ($1 * $2))
  return ($4, $5)
}