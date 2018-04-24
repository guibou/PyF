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
  % ". And pi = " % fixed 2 % ".\n" name age pi
```

- Type Safe
- Any number of formatter

- *Verbose*
- *Ping-Pong between format string and arguments*

---

### `Data.String.Interpolate`

```haskell
let formattedPi = Numeric.showFFloat (Just 2) pi
in [|Hello #{name}. You age is ${age} and pi = #{formatedPi}.\n|]
```
---

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
[f|Hello {name}. Your age is {age}. and pi = {pi:.2f}|]
```

- Type Safe
- No ping pong
- Readable format string
- Well known syntax (extended C `printf(3)` like)

- *Limited number of formatters* (can be extended...)

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
it "groups bin" $(checkExample "{123456789:_b}" "111_0101_1011_1100_1101_0001_0101")
```

- Format string is checked with the provided example
- *AND* with the python reference implementation
- with *Randomized* examples (using `Quickcheck`)

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
