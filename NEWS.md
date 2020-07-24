## syntactic 0.4.2 (2020-07-24)

### Minor changes

- Maintenance release updating minimum R dependency to 4.0.

## syntactic 0.4.1 (2020-07-09)

### Minor changes

- Added additional regular expression to catch "10uM" and "uM" edge cases in
  names that weren't sanitizing to "um" for snake case, as expected.

## syntactic 0.4.0 (2020-07-08)

Reworked the internal code and improved default handling for `makeNames`. Our
`makeNames` variant behaves a bit differently from base R `make.names`. By
default, it returns unique values and sanitizes using underscores (`_`) rather
than periods (`.`), which are easier to read and more compatible across
programming languages and in file names.

Now all syntactic naming functions, including `snakeCase`, `camelCase`, and
`dottedCase` internally pass through to `makeNames`. We have added some edge
case handlers that are now enabled by default in all of these functions,
including automatic detection of `+`/`-`, `%` (converts to percent) and some
other useful defaults for bioinformatics. These can be disabled by setting
`smart = FALSE`.

This release should be fully backward compatible and non-breaking with previous
syntactic functions, but the version has been increased to denote the change
in the internal code handling.

## syntactic 0.3.10 (2020-06-15)

### New functions

- `autopadZeros`: Migrated character method previously defined in basejump.
  Also defined support for integer class objects. Improved padding support
  of integer strings and hardened function against partial padding matches.

## syntactic 0.3.9 (2020-04-07)

### Minor changes

- Rename mode now checks for "_" prefix and will skip the file rename. This is
  particularly useful for avoiding issues with files such as `_pkgdown.yml`.

## syntactic 0.3.8 (2020-02-25)

### Minor changes

- Rename mode now includes a helpful message showing the original file name
  and modified target name. This convention matches Perl File::Rename module.

## syntactic 0.3.7 (2020-02-02)

### Minor changes

- `camelCase`: Bug fix for `strict = TRUE` not working as expected in rename
  mode (`rename = TRUE`).
- Switched license from MIT to GPL-3.
- Renamed package title to conform to CRAN requirements.

## syntactic 0.3.6 (2020-01-27)

### Major changes

- Migrated S4 generics to acidgenerics package.

### Minor changes

- Fix to perserve extension case (e.g. "R", "Rmd") for files in rename mode.

### Deprecations

- Deprecated `camel` in favor of `camelCase`.
- Deprecated `dotted` in favor of `dottedCase`.
- Deprecated `kebab` in favor of `kebabCase`.
- Deprecated `snake` in favor of `snakeCase`.
- Deprecated `upperCamel` in favor of `upperCamelCase`.

## syntactic 0.3.5 (2020-01-14)

### Minor changes

- Syntactic naming functions now check for "&" and convert to "and". This
  behavior can be disabled by setting `smart = FALSE`.
- Accented characters are now coerced to plain letters by default internally
  via `stringi::stri_trans_general`. This behavior can be disabled by setting
  `smart = FALSE`.

## syntactic 0.3.4 (2019-12-08)

### Minor changes

- Fixed expected handling of file rename mode on case insensitive file systems.
  Had to rework the internal code to loop across the files and directories,
  using an `apply` call internally.

## syntactic 0.3.3 (2019-12-05)

### Major changes

- Added recursive file rename support.

### Minor changes

- Improved handling of " - ", avoiding return of "minus" in file rename mode.
- File extension is automatically converted to lowercase when applicable.

## syntactic 0.3.2 (2019-10-22)

File rename mode support.

`camelCase`, `kebabCase`, `snakeCase`, and `upperCamelCase` now support file
rename mode (`rename = TRUE`). This works on either case-sensitive (Linux) or
case-insensitive (macOS, Windows) file systems. Files and directories are
supported, including files outside of the current working directory.

I recommend using either `kebabCase` or `snakeCase` for files.

When renaming with files beginning with a number, `prefix = TRUE` will prefix
the file with an "x", similar to the behavior in `make.names`. This can be
disabled by setting` prefix = FALSE`. Smart acronyms and other features can also
be disabled by setting `smart = FALSE`.

This code will be called internally in the upcoming [koopa][] update.

## syntactic 0.3.1 (2019-10-08)

### Minor changes

- Now allowing `prefix = FALSE` for `camelCase`, `dottedCase`, `kebabCase`,
  `snakeCase`, and `upperCamelCase`. This override allows the user to disable
  automatic "X" prefix defined by `make.names` internally for input that begins
  with a syntactically invalid character, such as a number (R-specific) or a
  non-alphanumeric. We're now allowing this override so we can pass to new
  `camel-case`, `kebab-case`, and `snake-case` shell scripts defined in
  [koopa][].
- Also now allowing `smart = FALSE`, which disables smart mode handling of
  mixed case acronyms (e.g. "RNAi", "mRNA"), numeric delims/decimals ("," "."),
  special characters that can represent words ("+"/"-"; plus/minus), etc.
  Enabled by default and strongly recommended. Added a new override mode for
  some edge cases with our new shell methods defined in koopa.
- Migrated character code coverage methods back here from basejump.
- Simplifed code coverage, removing usage of parameterized tests that depend
  on patrick package.

## syntactic 0.3.0 (2019-09-25)

### Major changes

- Migrated non-character S4 methods to basejump package, to keep syntactic
  very lightweight and fast. Defining only the character methods here allows
  the package to start up very quickly, and improves shell scripting calls
  defined in the koopa bootloader package.

## syntactic 0.2.6 (2019-09-13)

### Minor changes

- Improved handling of `"+"`, `"-"`, and `"/"` in character vectors. This update
  is particularly useful for handling examples such as `"dox+"` and
  `"vector +/- treatment"`.

## syntactic 0.2.5 (2019-09-09)

### Minor changes

- Migrated S4 generics here from bioverbs package, because they're not really
  "biological" per se. I consolidated the generics here in case we need to
  submit syntactic to CRAN rather than Bioconductor.
- Removed S4 generic methods for `mcolnames`. I'll add this into basejump
  directly instead.
- Improved documentation consistency and added timestamps.

## syntactic 0.2.4 (2019-08-27)

### Minor changes

- Updated R dependency to 3.6.

## syntactic 0.2.3 (2019-08-11)

### Minor changes

- Documentation improvements.
- Updated basejump dependencies.

## syntactic 0.2.2 (2019-07-29)

### Minor changes

- Renamed `label` to `makeLabel` and `title` to `makeTitle`, so we don't mask
  any functions in the graphics package.

## syntactic 0.2.1 (2019-07-28)

### New functions

- `makeWords`: Take a syntactic name vector and reformat to character strings
  containing words separated by spaces.
- `label`: Generates a character string suitable for plot axis labels. Uses the
  new `makeWords` function internally.
- `title`: Generate a sentence case string suitable for plot titles.

## syntactic 0.2.0 (2019-07-22)

### Major changes

- Renamed case functions. `camel` to `camelCase`; `kebab` to `kebabCase`;
  `snake` to `snakeCase`; `upperCamel` to `upperCamelCase`.

### Minor changes

- Updated naming consistency of internal S4 functions.
- Updated basejump dependencies.

## syntactic 0.1.10 (2019-07-12)

### Minor changes

- Updated R dependency to 3.5.

## syntactic 0.1.9 (2019-04-25)

### Minor changes

- S4 generic reexport documentation fixes.

## syntactic 0.1.8 (2019-03-29)

### Minor changes

- Bug fixes for `SummarizedExperiment` method support in syntactic functions.
  Added method suport for `snake`, which was missing, and forgot to import
  SummarizedExperiment as a class.
- 100% code coverage!

## syntactic 0.1.7 (2019-03-28)

### Minor changes

- Added SummarizedExperiment methods for syntactic naming functions, which
  reenables support for `rownames` and `colnames` arguments.
- Switched to using acidtest package for example test data.

## syntactic 0.1.6 (2019-03-22)

### Minor changes

- Migrated code to [Acid Genomics].

## syntactic 0.1.5 (2019-02-25)

### New functions

- `kebab`: Added character vector method support for kebab case.

### Minor changes

- Improved internal code to remove dependency on magrittr pipe.

## syntactic 0.1.4 (2019-02-11)

### Minor changes

- Converted `capitalize` to S4 method that dispatches on character vectors.

## syntactic 0.1.3 (2019-01-23)

### Minor changes

- Split out imports into `imports.R` file, to match conventions used in the
  other [basejump][] subpackages.
- Added [pkgdown][] website.

## syntactic 0.1.2 (2019-01-05)

### Minor changes

- Matrix NAMESPACE fix.
- Documentation improvements.

## syntactic 0.1.1 (2019-01-04)

### Minor changes

- Added back [Matrix][] S4 class method support.

## syntactic 0.1.0 (2019-01-01)

Initial release. Migrated syntactic name functions from [basejump][] package
here to form a more compact package that is easier to unit test.

[acid genomics]: https://acidgenomics.com/
[basejump]: https://basejump.acidgenomics.com/
[koopa]: https://koopa.acidgenomics.com/
[matrix]: https://cran.r-project.org/package=Matrix
[pkgdown]: https://pkgdown.r-lib.org/
