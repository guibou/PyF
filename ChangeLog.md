# Revision history for PyF

- Remove the dependency to `megaparsec` and replaces it by `parsec`. This should have minor impact on the error messages, however it reduces the dependencies size, because `parsec` is part of the standard `ghc` distribution.
- *Huge Change*. The parsing of embeded expression does not depend anymore on `haskell-src-ext` and `haskell-src-meta` and instead depends on the built-in `ghc` lib. The direct result is that `PyF` have fewer dependencies. A `stack` build from scratch now takes 35s versus 4 minutes and 20s before.

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
