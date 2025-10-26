function main() -> int
{
  ((int, int, int), (int, int, int), (int, int, int)) $bigTuple = ((100, 200, 300), (1000, 2000, 3000), (10000, 20000, 30000))
  return $bigTuple.1.1
}