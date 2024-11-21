function main() -> int
{
  return @@(&main.func1, 12345, &main.func2)
}

function main.func1(int $A0, (int, int) -> int $A1) -> int
{
  int $0 = 2345
  tailcall @@($A1, $A0, $0)
}

function main.func2(int $A0, int $A1) -> int
{
  return ($A0 - $A1)
}