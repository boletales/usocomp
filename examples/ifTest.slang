
function main() -> int
{
  int $0 = 100
  int $1 = 200
  int $2 = 1000
  int $3 = 2000
  int $4 = 3000
  int $5 = 4000
  when ($0 > $1)
  {
    $2 = 10000
  }
  else
  {
    $2 = 20000
  }

  when ($0 < $1)
  {
    $3 = 10000
  }
  else
  {
    $3 = 20000
  }

  when ($1 > $0)
  {
    $4 = 10000
  }
  else
  {
    $4 = 20000
  }

  when ($1 < $0)
  {
    $5 = 10000
  }
  else
  {
    $5 = 20000
  }

  return 0
}