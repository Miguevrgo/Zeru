# math

```zeru,ignore
import std.math;
```

| Item | Gives |
|---|---|
| `PI`, `E`, `TAU` | the constants, as `f64` |
| `min(a, b)`, `max(a, b)` | the smaller or larger of two numbers |
| `clamp(value, low, high)` | `value` held between `low` and `high` |
| `abs(n)` | `n` without its sign |
| `pow(base, exp)` | `base` to the power `exp` (a `u32`) |
| `is_power_of_two(n)` | whether `n` is 1, 2, 4, 8... |
| `div_euclid(a, b)`, `mod_euclid(a, b)` | division and remainder that never give a negative remainder |

They work for any number type the arguments share, except
`is_power_of_two`, which takes a `u64`.

```zeru
import std.math;

println("{} {}", math::max(3, 9), math::min(2.5, 1.5)); // 9 1.5
println("{}", math::clamp(150, 0, 100)); // 100
println("{} {}", math::pow(2, 10), math::pow(1.5, 2)); // 1024 2.25
println("{} {}", -7 % 5, math::mod_euclid(-7, 5)); // -2 3
println("{}", math::PI * 2.0 == math::TAU); // true
```

`mod_euclid` is the remainder to use for wrapping around, as a position
`-1` in a ring of 5 being `4`.
