function main() -> int
{
  int $i = 1
  while ($i < 1000)
  {
    $i = ($i + $i)
  }

  return $i
}