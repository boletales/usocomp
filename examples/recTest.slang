/*
selector: select examples

Compile : compile source
Step    : step by step execution
Run/Stop: run/stop execution
Reset   : reset execution
*/

function main() -> int
{
  int $result = fibonacci(20, 0, 1)
  return $result
}

function fibonacci(int $countdown, int $fib1, int $fib2) -> int
{
  when ($countdown == 0)
  {
    return $fib2
  }
  else
  {
    // change "return" to "tailcall" and see the difference!
    return fibonacci($countdown - 1, $fib2, $fib1 + $fib2)
  }
}