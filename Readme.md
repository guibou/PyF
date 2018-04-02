*PyF* is a Haskell library for string interpolation and formatting.

*PyF* exposes a quasiquoter `f` for the [Formatting](https://hackage.haskell.org/package/formatting) library. The quasiquotation introduces string interpolation and formatting with a mini language inspired from printf / Python to ease the construction of format string.

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
>>> import PyF
>>> name = "Dave"
>>> age = 54
>>> format [f|Person's name is {name}, age is {age:x}|]
"Person's name is Dave, age is 36"
```

This needs the `QuasiQuotes` and `OverloadedStrings` extensions enabled.

Variables are referenced by `{variableName:formatingOptions}` where `formatingOptions` follows the [Python format mini-language](https://docs.python.org/3/library/string.html#formatspec). It is recommended to read the python documentation, but the [Test file](https://github.com/guibou/PyF/blob/master/test/Spec.hs) as well as this readme contain many examples:

## padding

```haskell
>>> name = "Guillaume"
>>> format [f|{name:<11}|]
"Guillaume  "
>>> name = "Guillaume"
>>> format [f|{name:>11}|]
"  Guillaume"
>>> name = "Guillaume"
>>> format [f|{name:|^13}|]
"||Guillaume||"
```

## Float rounding

```haskell
>>> format [f|{pi:.2}|]
"3.14"
```

## Binary / Octal / Hex representation (with or without prefix)

```haskell
>>> v = 31
>>> format [f|Binary: {v:#b}|]
"Binary: 0b11111"
>>> format [f|Octal (no prefix): {age:o}|]
"Octal (no prefix): 37"
>>> format [f|Hexa (caps and prefix): {age:#X}|]
"Hexa (caps and prefix): 0x1F"
```

## Subexpression

First argument inside the curly braces can be a valid haskell expression, for example:

```haskell
>>> format [f|2pi = {2* pi:.2}|]
6.28
>>> format [f|tail "hello" = {tail "hello":->6}|]
"tail \"hello\" = --ello"
```

However the expression must not contain `}` or `:` characters.

## Combined

```haskell
>>> format [f|{pi:~>5.2}|]
"~~3.14"
```

`PyF` reexport most of `Formatting` runners, such as `format`, `sformat`, `formatToString`, ...

# Other quasi quoters

*PyF* main entry point is `f` but for convenience some other quasiquoters are provided:

- `f(StrictText|LazyText|String|Builder|IO)` directly call the underlying `Formatting` runner and produce the specified type.
- `f'` use type inference to deduce the type.

For example:

```haskell
>>> [f'|hello {pi.2}|] :: String
"hello 3.14"
>>> :type [fString|hello|]
[Char]
```

# Caveats

## Type inference

Type inference with numeric literals can be unreliable if your variables are too polymorphic. A type annotation or the extension `ExtendedDefaultRules` will help.

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

## Difference with the Python Syntax

### Not supported

- Number `n` formatter is not supported
- Alternative `#` forms for all floating point representations are not supported. Theses forms allow the result of the conversion to always contain a decimal-point character.
- grouping_option field is not supported. This will allows to group digits together, such as `100,234,123.03`.
- 0 field is not supported.
- Alignment with `=` is not supported. It allows padding chars to appears between a sign and a number, such as `-     100`.
- Floating point rendering of `NaN` and `Infinity` are not supported for the moment.
- Python support sub variables in the formatting options, such as `{varname:.{precision}}`, we should too.
- Python literal integers accepts binary/octal/hexa/decimal literals, PyF only accept decimal ones

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
- Handle basic haskell string escape sequences (such as `\n` and friends)
- Improve the parser for sub-expression (handle the `:` and `}` cases if possible).

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
