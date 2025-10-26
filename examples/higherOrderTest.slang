function main() -> int
{
  int $result_4plus2 = main.x_op_2(4, &main.add)
  int $result_4minus2 = main.x_op_2(4, &main.sub)
  int $result_4times2 = main.x_op_2(4, &main.mul)
  return ((($result_4plus2 * 100) + ($result_4minus2 * 10)) + $result_4times2)
}

function main.add(int $x, int $y) -> int
{
  return ($x + $y)
}

function main.mul(int $x, int $y) -> int
{
  return ($x * $y)
}

function main.sub(int $x, int $y) -> int
{
  return ($x - $y)
}

function main.x_op_2(int $x, (int, int) -> int $op) -> int
{
  tailcall @@($op, $x, 2)
}