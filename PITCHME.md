# [PyF](http://gitpitch.com/guibou/PyF/haskell-meetup-2018-04-25)

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
- Format any haskell expression

- *Limited number of formatters* (can be extended...)

---

## Availables formatters

- Floating point (Precision, fixed, exponential, generic, percent)
- Integral (Decimal, Octal, Binary, Hexa)
- Padding (left, right, around, between sign and number)
- Grouping (i.e: `123,456,789.123`)
- Sign handling

---
## Examples

Padding

```
>>> [fString|{name:|^13}|]
"||Guillaume||"
```

Grouping

```
>>> [fString|{2 ^ 32  -1:_#b}|]
"0b1111_1111_1111_1111_1111_1111_1111_1111"
```
---

## Complex formatter

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

- Crazy test suite ;)
- Template Haskell (And quasiquotes)
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

## Representing formatters : First approach

```haskell
data Format = Decimal | Binary | Alternate Format
                   | UpperCase Format | StringF | Fixed | ...
```

- Incorrect inhabitants: `Alternate StringF`, ...
- Repetitions: `Alternate (Alternate Binary)`
- Order: `UpperCase (Alternate Binary)` vs `Alternate (UpperCase Binary)`

---

## Representing formatters : GADTs

```haskell
data AltStatus = CanAlt | NoAlt
data UpperStatus = CanUpper | NoUpper
data FormatType = Fractional | Integral | Textual
```

```
data Format (k :: AltStatus) (k' :: UpperStatus) (k'' :: FormatType) where
  Decimal :: Format 'NoAlt 'NoUpper 'Integral
  Binary :: Format 'CanAlt 'NoUpper 'Integral
  Fixed :: Format 'CanAlt 'CanUpper 'Fractional
  StringF :: Format 'NoAlt 'NoUpper 'Textual

  Alternate :: Format 'CanAlt u f -> Format 'NoAlt u f
  Upper :: Format alt 'CanUpper f -> Format 'NoAlt 'NoUpper f
  ...
  ```

---

```haskell
>>> Upper Fixed
Upper Fixed
>>> Alternate Binary
Alternate Binary
>>> Upper Binary

<interactive>:5:7: error:
    • Couldn't match type ‘'NoUpper’ with ‘'CanUpper’
      Expected type: Format 'CanAlt 'CanUpper 'Integral
        Actual type: Format 'CanAlt 'NoUpper 'Integral
		...
>>> Upper (Alternate Fixed)
Upper (Alternate Fixed)
>>> Alternate (Upper Fixed)

<interactive>:7:12: error:
    • Couldn't match type ‘'NoAlt’ with ‘'CanAlt’
      Expected type: Format 'CanAlt 'NoUpper 'Fractional
        Actual type: Format 'NoAlt 'NoUpper 'Fractional
		...
```

---

## More GADTs: Alignement

```haskell
data AlignMode (k :: AlignForString) where
  AlignLeft :: AlignMode 'AlignAll
  AlignRight :: AlignMode 'AlignAll
  AlignInside :: AlignMode 'AlignNumber
  AlignCenter :: AlignMode 'AlignAll
```

```haskell
data Formatter where
    Formatter :: SingletonAlignMode ft -> AlignMode (ModeFor ft) -> Format k k' ft -> Formatter
```

---

# Type checking : 3 phases

---

## Untyped parsing

```haskell
>>> [fString|Hello {pi:Hi}|]

<interactive>:5:10: error:
    • <interactive>:1:11:
  |
1 | Hello {pi:Hi}
  |           ^
unexpected 'H'
expecting '#', '%', '+', '-', '.', '0', 'E', 'F', 'G', 'X', 'b', 'c', 'd', 'e', 'f', 'g', 'o', 's', 'x', '}', integer, or space
```

---

## *Type* checking

```haskell
>>> [f|{age:.3d}|]

<interactive>:77:4: error:
    • <interactive>:1:8:
  |
1 | {age:.3d}
  |        ^
Type `d` incompatible with precision (.3), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 's', '%'} or remove the precision field.
```

---

## Typeclass type check

- `GHC.TypeLits.TypeError` is *awesome*

```haskell
instance TypeError (
  'Text "String cannot be aligned with the \"sign-aware\" (i.e. `=`) mode"
  ) => Categorise DisableForString LText.Text where ...
```

```haskell
*PyF> [fString|Hello {"hello":=10}|]

<interactive>:10:10: error:
    • String cannot be aligned with the "sign-aware" (i.e. `=`) mode
```

---

## Final thoughts

Todos:

- Improve error reporting
- Some error case are still accepted

- https://github.com/guibou/PyF
- https://hackage.haskell.org/package/PyF

Thank you ;)
