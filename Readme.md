*PyF* is a Haskell library for string interpolation and formatting.

*PyF* exposes a quasiquoter `f` for the [Formatting](https://hackage.haskell.org/package/formatting) library. The quasiquotation introduces string interpolation and formatting with a mini language inspired from printf / Python to ease the construction of format string.

# Quick Start

You will need the extension `QuasiQuotes`, enable it with `{-# LANGUAGE QuasiQuotes #-}` in top of your source file or with `:set -XQuasiQuotes` in your `ghci` session.

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

Template haskell is generally known to give devellopers a lot of
frustration when it comes to error message, dumping an unreadable
piece of generated code.

However, in PyF, we took great care to provide clear error reporting, this means that:

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
  too polymorphic), you will get an awful error:

```haskell
>>*> [fString|{True:d}|]

<interactive>:80:10: error:
    • No instance for (Integral Bool)
        arising from a use of ‘PyF.Internal.QQ.formatAnyIntegral’
...
```

- There is also one class of error related to alignement which can be triggered, when using alignement inside sign (i.e. `=`) with string. This can fail in two flavors:

```haskell
>>> [fString|{"hello":=10s}|]

<interactive>:88:1: error:
    • Exception when trying to run compile-time code:
        String Cannot be aligned with the inside `=` mode
CallStack (from HasCallStack):
  error, called at src/PyF/Internal/QQ.hs:143:18 in PyF-0.4.0.0-inplace:PyF.Internal.QQ
      Code: quoteExp fString "{\"hello\":=10s}"
    • In the quasi-quotation: [fString|{"hello":=10s}|]
```

And

```haskell
*PyF PyF.Internal.QQ> [fString|{"hello":=10}|]

<interactive>:89:10: error:
    • String Cannot be aligned with the inside `=` mode
...
```

- Finally, if you make any type error inside the expression field, you are on your own:

```haskell
>>> [fString|{3 + pi + "hello":10}|]

<interactive>:99:10: error:
    • No instance for (Floating [Char]) arising from a use of ‘pi’
    ...
```

## Difference with the Python Syntax

### Not supported

- Number `n` formatter is not supported
- Alternative `#` forms for all floating point representations are not supported. Theses forms allow the result of the conversion to always contain a decimal-point character.
- 0 field is not supported.
- Python support sub variables in the formatting options, such as `{varname:.{precision}}`, we should too.
- Python literal integers accepts binary/octal/hexa/decimal literals, PyF only accept decimal ones

### Difference

- Exponential formatters *e* and *E* formats the exponent with less digits. For example `0.2` formatted as `.1e` gives `2.0e-1` instead of `2.0e-01` in python.
- General formaters *g* and *G* behaves a bit differently. Precision influence the number of significant digits instead of the number of the magnitude at which the representation changes between fixed and exponential.
- Grouping options allows grouping with an `_` for floating point, python only allows `,`.

# TODO

- Check with python that all examples are correct
- Fix the unsupported formatters
- Fix the small differences, the point of this library is to match the python syntax, so the differences should not exists.
- Code quality (documentation and tests, we can copy the python tests)
- Improve the error reporting with more Parsec annotation
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
