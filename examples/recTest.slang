/*
selector: select examples

Compile : compile source
Step    : step by step execution
Run/Stop: run/stop execution
Reset   : reset execution
*/

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
    // change "return" to "tailcall" and see the difference!
    tailcall main.fibonacci(($steps - 1), $b, ($a + $b))
  }

}