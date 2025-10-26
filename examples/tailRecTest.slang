function main() -> int
{
  int $fib20 = main.fibonacci(20, 0, 1)
  return $fib20
}

function main.fibonacci(int $steps, int $a, int $b) -> int
{
  if ($steps == 0)
  {
    return $b
  }
  else
  {
    tailcall main.fibonacci(($steps - 1), $b, ($a + $b))
  }

}