# PyF

### A QuasiQuote for string interpolation and formatting

---

# Formatting in Haskell

---

## Basic formatting

```haskell
"Hello " <> name <> ". Your age is " <> show age
  <> ".And pi = "
  <> Numeric.showFFloat (Just 2) pi "" <> ".\n"
```

- Type safe

- *No formatting*
- *Verbose*

---

## `Text.Printf`

```haskell
printf "Hello %s. Your age is %d. and pi = %.2f.\n"
        name age pi
```

- Compact
- Well known syntax (C `printf(3)` like)

- *Runtime errors*
- *Ping-Pong between format string and arguments*

---

## `Formatting`

```haskell
format ("Hello " % text % ". Your age is " % int
  % ". And pi = " % fixed 2 % ".\n") name age pi
```

- Type Safe
- Any number of formatter

- *Verbose*
- *Ping-Pong between format string and arguments*

---

### `Data.String.Interpolate`

```haskell
let formattedPi = Numeric.showFFloat (Just 2) pi
in [|Hello #{name}. Your age is #{age} and pi = #{formatedPi}.\n|]
```

- Compact
- Type safe

- *No formatting*

---

## In python ?

```python
f"Hello {name}. Your age is {age}. and pi = {pi:.2f}"
```

- Compact
- Well known syntax (extended `C printf(3)`)
- No Ping Pong

- *Runtime errors*

---

## PyF

### Python experience, with type safety

---

## PyF

```haskell
[fString|Hello {name}. Your age is {age}. and pi = {pi:.2f}|]
```

- Type Safe
- No ping pong
- Readable format string
- Well known syntax (extended C `printf(3)` like)
- Readable errors !

- *Limited number of formatters* (can be extended...)

---

## PyF - Availables QQ

- `f`: format to `Formatting` formatter
- `fString`: format to `String` (`Text` also available)
- `f'`: use type inference
- `fIO`: format and output on stdout

---

## Examples

```haskell
for_ [0..16] $ \i ->
  [fIO|{i:>2}: Binary: {i:_=#7b} Octal: {i:_=#4o} Hexa: {i:_=#4x}\n|]
```

```
 0: Binary: 0b____0 Octal: 0o_0 Hexa: 0x_0
 1: Binary: 0b____1 Octal: 0o_1 Hexa: 0x_1
 2: Binary: 0b___10 Octal: 0o_2 Hexa: 0x_2
 3: Binary: 0b___11 Octal: 0o_3 Hexa: 0x_3
 4: Binary: 0b__100 Octal: 0o_4 Hexa: 0x_4
 5: Binary: 0b__101 Octal: 0o_5 Hexa: 0x_5
 6: Binary: 0b__110 Octal: 0o_6 Hexa: 0x_6
 7: Binary: 0b__111 Octal: 0o_7 Hexa: 0x_7
 8: Binary: 0b_1000 Octal: 0o10 Hexa: 0x_8
 9: Binary: 0b_1001 Octal: 0o11 Hexa: 0x_9
10: Binary: 0b_1010 Octal: 0o12 Hexa: 0x_a
11: Binary: 0b_1011 Octal: 0o13 Hexa: 0x_b
12: Binary: 0b_1100 Octal: 0o14 Hexa: 0x_c
13: Binary: 0b_1101 Octal: 0o15 Hexa: 0x_d
14: Binary: 0b_1110 Octal: 0o16 Hexa: 0x_e
15: Binary: 0b_1111 Octal: 0o17 Hexa: 0x_f
16: Binary: 0b10000 Octal: 0o20 Hexa: 0x10
```

---

# Implementation details

---

## Details

- Template Haskell (And quasiquotes) : don't be afraid
- `Megaparsec` for parsing / error reporting
- `GADTs` for type safety of the formatting details
- `GHC.TypeLits.TypeError` for final safety

---

## Tests

```haskell
it "groups bin" $(checkExample "{123456789:_b}"
   "111_0101_1011_1100_1101_0001_0101")
```

- Format string is checked with the provided example
- *AND* with the python reference implementation
- with *Randomized* examples (using `Quickcheck`)
- Need tests for failure cases

---

## Error reporting

TODO

## Error reporting - TH Parsing

TODO

---

## Error reporting - TH Type check

TODO
---

## Error reporting - Typeclass type check

TODO
