*PyF* is a Haskell library for string interpolation and formatting.

*PyF* exposes a quasiquoter `f` for the [Formatting](https://hackage.haskell.org/package/formatting) library. The quasiquotation introduces string interpolation and formatting with a mini language inspired from printf and Python.

# Quick Start

The following *Formatting* example:

```haskell
>>> import Formatting

>>> name = "Dave"
>>> age = 54

>>> format ("Person's name is " % text % ", age is " % hex) name age
"Person's name is Dave, age is 36"
```

can be written as:

```haskell
>>> import Formatting
>>> import PyF

>>> name = "Dave"
>>> age = 54

>>> format [f|Person's name is {name}, age is {age:x}|]
"Person's name is Dave, age is 36"
```

The formatting mini language can represent:

- Numbers with different representations (fixed point, general representation, binary, hexadecimal, octal)
- Padding, with the choice of padding char, alignment (left, right, around, between sign and number)
- Sign handling, to display or not the `+` for positive number
- Number grouping
- Floating point representation
- The interpolated value can be any Haskell expression

You will need the extension `QuasiQuotes`, enable it with `{-# LANGUAGE QuasiQuotes #-}` in top of your source file or with `:set -XQuasiQuotes` in your `ghci` session.

Expression to be formatted are referenced by `{expression:formatingOptions}` where `formatingOptions` follows the [Python format mini-language](https://docs.python.org/3/library/string.html#formatspec). It is recommended to read the python documentation, but the [Test file](https://github.com/guibou/PyF/blob/master/test/Spec.hs) as well as this readme contain many examples.

# More Examples

## Padding

Left `<` / Right `>` / Around `^` padding:

```haskell
>>> name = "Guillaume"
>>> format [f|{name:<11}|]
"Guillaume  "
>>> format [f|{name:>11}|]
"  Guillaume"
>>> format [f|{name:|^13}|]
"||Guillaume||"
```

Padding inside `=` the sign:

```haskell
>>> [fString|{-pi:=10.3}|]
"-    3.142"
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

## Grouping

Using `,` or `_`.

```haskell
>>> [fString|{10 ^ 9 - 1:,}|]
"999,999,999"
>>> [fString|{2 ^ 32  -1:_b}|]
"1111_1111_1111_1111_1111_1111_1111_1111"
```

## Sign handling

Using `+` to display the positive sign (if any) or ` ` to display a space instead:

```haskell
>>> [fString|{pi:+.3}|]
"+3.142"
>>> [fString|{pi: .3}|]
" 3.142"
```

## 0

Preceding the width with a `0` enables sign-aware zero-padding, this is equivalent to inside `=` padding with a fill char of `0`.

```haskell
>>> [fString{-10:010}|]
-000000010
```

## Sub-expressions

First argument inside the curly braces can be a valid Haskell expression, for example:

```haskell
>>> format [f|2pi = {2* pi:.2}|]
6.28
>>> format [f|tail "hello" = {tail "hello":->6}|]
"tail \"hello\" = --ello"
```

However the expression must not contain `}` or `:` characters.

## Combined

Most options can be combined. This generally leads to totally unreadable format string ;)

```haskell
>>> format [f|{pi:~>5.2}|]
"~~3.14"
```

# Other quasiquoters

*PyF* main entry point is `f` but for convenience some other quasiquoters are provided:

- `f(StrictText|LazyText|String|Builder|IO)` directly call the underlying `Formatting` runner and produce the specified type.
- `f'` use type inference to deduce the type.

`PyF` reexport most of `Formatting` runners, such as `format`, `sformat`, `formatToString`, ...

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

Template haskell is generally known to give developers a lot of
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

The implementation is unit-tested against the reference python implementation (python 3.6.4) and should match its result. However some formatters are not supported or some (minor) differences can be observed.

### Not supported
,p
- Number `n` formatter is not supported. In python this formatter can format a number and use current locale information for decimal part and thousand separator.
- Python support sub variables in the formatting options, such as `{varname:.{precision}}`, we should too.
- Python literal integers accepts binary/octal/hexa/decimal literals, PyF only accept decimal ones
- Python support adding custom formatters for new types, such as date. This may be really cool, for example `[f|{today:%Y-%M-%D}`.

### Difference

- General formatters *g* and *G* behaves a bit differently. Precision influence the number of significant digits instead of the number of the magnitude at which the representation changes between fixed and exponential.
- Grouping options allows grouping with an `_` for floating point, python only allows `,`.

# Build / test

Should work with `stack build; stack test`, and with `cabal` and (optionally) `nix`:

```shell
nix-shell # Optional, if you use nix
cabal new-build
cabal new-test
```

# TODO

- Fix the unsupported formatters
- Code quality (documentation and tests. I want automated tests ala quickcheck)
- Improve the error reporting with more Parsec annotation
- Improve the parser for sub-expression (handle the `:` and `}` cases if possible).
- Allow extension to others type / custom formatters (for date for example)

# Library note

`PyF.Formatters` exposes two functions to format numbers. They are type-safe (as much as possible) and comes with a combination of formatting options not seen in other formatting libraries:

```haskell
>>> formatIntegral Binary Plus (Just (20, AlignInside, '~')) (Just (4, ',')) 255
"+~~~~~~~~~~1111,1111"
```

# Conclusion

Don't hesitate to make any suggestion, I'll be more than happy to work on it.

