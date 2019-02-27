# Revision history for PyF

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
