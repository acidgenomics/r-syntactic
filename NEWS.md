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

[Acid Genomics]: https://acidgenomics.com/
[basejump]: https://basejump.acidgenomics.com/
[Matrix]: https://cran.r-project.org/package=Matrix
[pkgdown]: https://pkgdown.r-lib.org/
