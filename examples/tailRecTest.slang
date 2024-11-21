function main() -> int
{
  int $0 = main.fibonacci(20, 0, 1)
  return $0
}

function main.fibonacci(int $A0, int $A1, int $A2) -> int
{
  when ($A0 == 0)
  {
    return $A2
  }
  else
  {
    tailcall main.fibonacci(($A0 - 1), $A2, ($A1 + $A2))
  }

}