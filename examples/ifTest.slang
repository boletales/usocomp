function main() -> int
{
  int $i = 100
  int $j = 200
  int $a = 1000
  int $b = 2000
  int $c = 3000
  int $d = 4000
  if ($i > $j)
  {
    $a = 10000
  }
  else
  {
    $a = 20000
  }

  if ($i < $j)
  {
    $b = 10000
  }
  else
  {
    $b = 20000
  }

  if ($j > $i)
  {
    $c = 10000
  }
  else
  {
    $c = 20000
  }

  if ($j < $i)
  {
    $d = 10000
  }
  else
  {
    $d = 20000
  }

  return 0
}