# PyF

### A QuasiQuote for string interpolation and formatting

---

## Introduction

- PyF was announced the previous week

...

---

# Formatting in Haskell

---

## Basic formatting

```haskell
"Hello " <> name <> ". Your age is " <> show age
  <> ".And pi = " <> Numeric.showFFloat (Just 2) pi "" <> ".\n"
```

- No formatting
- Incompact format string
- Type safe

---

## Advanced formatting

### `Text.Printf`

```haskell
printf "Hello %s. Your age is %d. and pi = %.2f.\n" name age pi
```

- Format string is compact
- unsafe
- Ping-Pong between format string and arguments
- Limited number of formatters (can be extended)
- Well known syntax (C printf like)

---

### `Formatting`

```haskell
format ("Hello " % text % ". Your age is " % int
  % ". And pi = " % fixed 2 % ".\n" name age pi
```

- Format string is not compact
- Type Safe
- Ping-Pong between format string and arguments
- Any number of formatter
- Well known syntax (C printf like)

---

### `Data.String.Interpolate`

```haskell
let formattedPi = Numeric.showFFloat (Just 2) pi
in [|Hello #{name}. You age is ${age} and pi = #{formatedPi}.\n|]
---

- No formatting at all
- Really readable
- Type safe

---

## In python ?

```python
f"Hello {name}. Your age is {age}. and pi = {pi:.2f}"
```

- Readable format string
- Limited number of formatters (can be extended)
- Well known syntax (extended C printf like)
- No Ping Pong
- Runtime errors

---

## Introducing PyF

```haskell
[f|Hello {name}. Your age is {age}. and pi = {pi:.2f}|]
```

- Python like syntax
- No ping pong
- Readable format string
- Limited number of formatters (can be extended*)
- Well known syntax (extended C printf like)
- Type Safe

---

# Implementation details

## Tests

---

## Error reporting - TH Parsing

---

## Error reporting - TH Type check

---

## Error reporting - Typeclass type check
