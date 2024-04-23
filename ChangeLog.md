# Revision history for PyF

##

- Support for GHC 9.10.

## 0.11.2.1 -- 2023-10-25

- Final version for GHC 9.8

## 0.11.2.0

- Fix for the neovim treesitter syntax highlighter for `fmt` and `fmtTrim` quasiquotes
- Initial support for GHC 9.8
- Version bump for new MTL

## 0.11.1.1 -- 2023-03-15

- Support for GHC 9.6. Thank you @Kleidukos for initiating the port.

## 0.11.1.0 -- 2022-09-24

- Support for OverloadedRecordsDot syntax in Meta. Thank you @Profpatsch for the report.
- In some context, the error reporting for variable not found in the quasi quote expression was incorrectly reporting existing variables as not found. See https://github.com/guibou/PyF/issues/115 for details. This is now fixed by not abusing GHC api. Thank you @michaelpj for reporting this really weird problem.

## 0.11.0.0 -- 2022-08-10

- Support for GHC 9.4. (Written with a pre-release of GHC 9.4, hopefully it won't change too much before the release).
- Error reporting now uses the native GHC API. In summary, it means that
 haskell-language-server will point to the correct location of the error, not
 the beginning of the quasi quotes.
- PyF will now correctly locate the error for variable not found in expression, even if the expression is complicated. The support for complex expression is limited, and PyF may return a false positive if you try to format a complex lambda / case expression. Please open a ticket if you need that.
- Add support for literal `[]` and `()` in haskell expression.
- Add support for overloaded labels, thank you Shimuuar.
- Support for `::` in haskell expression. Such as `[fmt| 10 :: Int:d}|]`, as a suggestion from julm (close #87).
- `Integral` padding width and precision also for formatter without type specifier.
- Extra care was used to catch all `type-defaults` warning message. PyF should
  not generate code with this kind of warning, unless the embedded Haskell
  expression are ambiguous (e.g. `[fmt|{10}|]`). You can use `::` to
  disambiguate, e.g. `[fmt|{10 :: Int}|]`.

## 0.10.1.0 -- 2021-12-05

- Padding width can now be any arbitrary Haskell expression, such as `[fmt|hello pi = {pi:<{5 * 10}}|]`.
- Precision (and now padding width) arbitrary expression can now be any `Integral` and it is not limited to `Int` anymore.
- (Meta): type expression are now parsed and hence allowed inside arbitrary Haskell expression for padding width and precision. For example, `[fmt|Hello {pi:.{3 :: Int}}|]`.

## 0.10.0.1 -- 2021-10-30

- Due to the dependencies refactor, `PyF` no have no dependencies other than the one packaged with GHC. The direct result is that `PyF` build time is reduced to 6s versus 4 minutes and 20s before.
- Remove the dependency to `megaparsec` and replaces it by `parsec`. This should have minor impact on the error messages.
- *Huge Change*. The parsing of embeded expression does not depend anymore on `haskell-src-ext` and `haskell-src-meta` and instead depends on the built-in `ghc` lib.
- Added instances for `(Lazy)ByteString` to `PyFClassify` and `PyFToString`. `ByteString` can now be integrated into format string, and will be decoded as ascii.
- Relax the constraint for floating point formatting from `RealFrac` to `Real`. As a result, a few new type can be formatted as floating point number. One drawback is that some `Integral` are `Real` too and hence it is not an error anymore to format an integral as floating point, but you still need to explicitly select a floating point formatter.
- Added instance for `(Nominal)DiffTime` to `PyFClassify`, so you can now format them without conversion.
- Introducing of the new typeclass `PyfFormatIntegral` and `PyfFormatFractional` in order to customize the formatting for numbers. An instance is derived for respectively any `Integral` and `Real` types.
- Support for `Char` formatting, as string (showing the `Char` value) or as integral, showing the `ord`.
- `Data.Ratio`.

- Introducing `fmtTrim` module. It offers the same behavior as `fmt`, but trims common indentation. Se `PyF.trimIndent` for documentation.
- Introducing `raw` for convenience. It is a multiline string without any escaping, formatting neither leading whitespace handling.
- Introducing `str` and `strTrim`. They are similar to `fmt` and `fmtTrim` but without formatting. You can see them as multiline haskell string, with special character escaping, but without formatting. For convenience, the `strTrim` version also removes indentation.


- `fmtWithDelimiters` is gone and replaced by `mkFormatter` in `PyF` which is "more" generic. 

## 0.9.0.3 -- 2021-02-06

- Test phase do not depend anymore on python (unless cabal flag `python_test` is
  set). This ease the deployment / packaging process.

## 0.9.0.2 -- 2020-09-11

- Version bump for megaparsec 9.0

## 0.9.0.1 -- 2020-03-25

- Fixs for GHC 8.10

## 0.9.0.0 -- 2019-12-29

- Any type with `Show` instance can be formatted using `:s` formatter. For example, `[fmt|hello {(True, 10):s}|]`. This breaks compatibility because previous version of PyF was generating an error when try to format to string anything which was not a string, now it accepts roughly anything (with a `Show` instance).

## 0.8.1.2 -- 2019-11-08

- Bump megaparsec bounds

## 0.8.1.1 -- 2019-10-13

- Compatibility with GHC 8.8

## 0.8.1.0 -- 2019-09-03

- Precision can now be any arbitrary haskell expression, such as `[fmt|hello pi = {pi:.{1 + 3}}|]`.

## 0.8.0.2 -- 2019-08-27

- (minor bugfix in tests): Use python3 instead of "python" to help build on environment with both python2 and python3

## 0.8.0.1 -- 2019-08-27

- Stack support

## 0.8.0.0 -- 2019-08-06

- `f` (and `fWithDelimiters`) were renamed `fmt` (`fmtWithDelimiters`). `f` was causing too much shadowing in any codebase.
- PyF now exposes the typeclass `PyFToString` and `PyFClassify` which can be extended to support any type as input for the formatters.
- PyF now uses `Data.String.IsString t` as its output type if `OverloadedString` is enabled. It means that it behaves as a real haskell string literal.
- A caveat of the previous change is that PyF does not have instances for `IO` anymore.

### bugfixes and general improvements

- An important amount of bugfixs
- Error reporting for generic formatting (i.e. formatting without a specified type) is now more robust
- Template haskell splices are simpler. This leads to more efficient / small generated code and in the event of this code appears in a GHC error message, it is more readable.
- PyF now longer emit unnecessary default typing.


## 0.7.3.0 -- 2019-02-28

- Tests: fix non reproducible tests

## 0.7.2.0 -- 2019-02-27

- Fixed: PyF now uses the same haskell extensions as the one used by the current haskell file to parse sub expressions.

## 0.7.1.0 -- 2019-02-11

- Fixed: PyF was wrongly ignoring everything located after a non-doubled closing delimiter.
- New Feature: line break can be escaped with \, thus allowing string to start on a new line ignoring the initial backspace

## 0.7.0.0 -- 2019-02-04

- Bump dependencies to megaparsec 7
- Error message are now tested
- Name in template haskell splices are stable. This increases readability of error messages
- Better error message for badly formated expression

### Formatting removal

- All monomorphic quasiquoters (`f`, `fString`, `fText`, `fIO`, `fLazyText`) are removed
- Polymophic quasiquoter `f'` is renamed `f` and is the only entry point. Monomorphic users are encouraged to use the polymorphic quasiquoter with type annotation.
- `Formatting` dependency is removed.
- Previously named `f` quasiquoters which was exporting to `Formatting.Format` is removed. User of this behavior should use `Formatting.now` instead.

## 0.6.1.0 -- 2018-08-03

- Custom delimiters, you can use whatever delimiters you want in place of `{` and `}`.

## 0.6.0.0 -- 2018-08-02

- Fix the espace parsing of `{{` and `}}` as `{` and `}`

## 0.5.0.0 -- 2018-04-16

- Support for negative zero
- Support for 0 modifier
- Exponential formatter now behaves as python
- Support for alternate floatting point represenation
- Lot of documentation
- Test are auto verified with the python reference implementation

## 0.4.0.0 -- 2018-04-13

- Support for grouping option
- Support for inner allignment
- Correct display of NaN and Infinity
- Fix a few cosmetic with python implementation
- Introduce `PyF.Formatters`, type safe generic number formatter solution
- Remove dependency to `scientific`


## 0.3.0.0 -- 2018-04-01

* Support for haskell subexpression

## 0.1.1.0  -- 2018-01-07

* Add support for the `sign` field.

## 0.1.0.0  -- 2018-01-03

* First version. Released on an unsuspecting world.
