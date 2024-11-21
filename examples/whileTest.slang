function main() -> int
{
  int $0 = 1
  while ($0 < 1000)
  {
    $0 = ($0 + $0)
  }

  return $0
}