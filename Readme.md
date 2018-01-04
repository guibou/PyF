*PyF* is a Haskell library for string interpolation and formatting.

Internally, the formatting is done using the [Formatting](https://hackage.haskell.org/package/formatting) library, however *PyF* provides a quasiquotation with a mini language inspired from Python to ease the construction of format string.

# Quick Start

The following *Formatting* example:

```haskell
>>> name = "Dave"
>>> age = 54

>>> format ("Person's name is " % text % ", age is " % hex) name age
"Person's name is Dave, age is 36"
```

can be written as:

```haskell
>>> name = "Dave"
>>> age = 54
>>> [f|Person's name is {name}, age is {age:x}|]
"Person's name is Dave, age is 36"
```

This needs the `QuasiQuotes` and `OverloadedStrings` extensions enabled.

Variables are referenced by `{variableName:formatingOptions}` where `formatingOptions` follows the [Python format mini-language](https://docs.python.org/3/library/string.html#formatspec). It is recommended to read the python documentation, but the [Test file](https://github.com/guibou/PyF/blob/master/test/Spec.hs) as well as this readme contain many examples:

# padding

```haskell
>>> name = "Guillaume"
>>> [f|{name:<11}|]
"Guillaume  "
>>> name = "Guillaume"
>>> [f|{name:>11}|]
"  Guillaume"
>>> name = "Guillaume"
>>> [f|{name:|^13}|]
"||Guillaume||"
```

# Float rounding

```haskell
>>> [f|{pi:.2}|]
"3.14"
```

# Binary / Octal / Hex representation (with or without prefix)

```haskell
>>> v = 31
>>> [f|Binary: {v:#b}|]
"Binary: 0b11111"
>>> [f|Octal (no prefix): {age:o}|]
"Octal (no prefix): 37"
>>> [f|Hexa (caps and prefix): {age:#X}|]
"Hexa (caps and prefix): 0x1F"
```

# Combined

```haskell
>>> [f|{pi:~>5.2}|]
"~~3.14"
```

# Caveats

## Type inference

Type inference is broken in most cases if your variables are too polymorphic (such as simple literals). Most of the time a type annotation solve the issue.

```haskell
>>> v = 10 :: Double
>>> [f|A float: {v}|]
A float: 10
```

## Error reporting

I took great care to provide clear error reporting, this means that:

- No runtime error (or it is a bug)
- Any parsing error on the mini language results in a clear indication of the error, for example:

```haskell
>>> [f|{age:.3d}|]

<interactive>:77:4: error:
    • <interactive>:1:8:
  |
1 | {age:.3d}
  |        ^
Type incompatible with precision (.3), use any of {'e', 'E', 'f', 'F', 'g', 'G', 'n', 's', '%'} or remove the precision field.
```

- Error in variable name are also readable:

```haskell
>>> [f|{toto}|]
<interactive>:78:4: error: Variable not in scope: toto
```

- However, if the interpolated name is not of a compatible type (or
  too polymorphic), you will get an awful error.

## Output

For now, the only generated output is a lazy `Text` from `Data.Text.Lazy`. I'm still wondering if a polymorphic solution is better than a monomorphic solution with different quasiquoters name.

## Difference with the Python Syntax

### Not supported

- Number `n` formatter is not supported
- Alternative `#` forms for all floating point representations are not supported. Theses forms allow the result of the conversion to always contain a decimal-point character.
- Sign field is not supported. This adds the sign `+` in front of positives numbers.
- grouping_option field is not supported. This will allows to group digits together, such as `100,234,123.03`.
- 0 field is not supported.
- Alignment with `=` is not supported. It allows padding chars to appears between a sign and a number, such as `-     100`.
- Floating point rendering of `NaN` and `Infinity` are not supported for the moment.
- Python support sub variables in the formatting options, such as `{varname:.{precision}}`, we should too.
- Python support arbitrary python expression in the formatting options, I'm not sure we should...

### Difference

- Exponential formatters *e* and *E* formats the exponent with less digits. For example `0.2` formatted as `.1e` gives `2.0e-1` instead of `2.0e-01` in python.
- Centering puts more char on the right than on the left. For example `hello` formatted as `+10s` gives `+++hello++` instead of `++hello+++`.
- General formaters *g* and *G* behaves a bit differently. Precision influence the number of significant digits instead of the number of the magnitude at which the representation changes between fixed and exponential.

# TODO

- Fix the unsupported formatters
- Fix the small differences, the point of this library is to match the python syntax, so the differences should not exists.
- Code quality (documentation and tests, we can copy the python tests)
- Improve the error reporting with more Parsec annotation
- Improve the issue with type inference

# Build / test

Should work with `stack build; stack test`, and also with:

```shell
nix-shell
cabal new-build
cabal test
```

# Conclusion

For complex tasks, I use *Formatting* (or any great library). But for most of my simple formatting task, I was really missing a simple and non verbose library. The mini formating language of python is simple enough for a few cases.

Don't hesitate to make any suggestion, I'll be more than happy to work on it.
