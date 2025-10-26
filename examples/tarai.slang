function main() -> int {
  tailcall tarai(10, 5, 0)
}

function tarai(int $x, int $y, int $z) -> int {
  if ($x > $y) {
    tailcall tarai(tarai($x-1, $y, $z), tarai($y-1, $z, $x), tarai($z-1, $y, $x))
  } else {
    return $z
  }
}