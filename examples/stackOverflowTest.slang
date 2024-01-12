function main() -> int
{
  return loop(0)
}

function loop(int $cnt) -> int
{
  when ($cnt < 10000) 
  {
    return loop($cnt + 1)
  }
  else
  {
    return $cnt
  }
}