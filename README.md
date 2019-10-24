# syntactic

[![Repo status: active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Travis CI build status](https://travis-ci.com/acidgenomics/syntactic.svg?branch=master)](https://travis-ci.com/acidgenomics/syntactic)
[![AppVeyor CI build status](https://ci.appveyor.com/api/projects/status/9alj3hqmvfha9a02/branch/master?svg=true)](https://ci.appveyor.com/project/mjsteinbaugh/syntactic/branch/master)
[![Anaconda version](https://anaconda.org/bioconda/r-syntactic/badges/version.svg) ![Anaconda latest release date](https://anaconda.org/bioconda/r-syntactic/badges/latest_release_date.svg) ![Anaconda downloads](https://anaconda.org/bioconda/r-syntactic/badges/downloads.svg)](https://anaconda.org/bioconda/r-syntactic)

Make syntactically valid names out of character vectors.

## Installation

### [R][] method

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}
Sys.setenv(R_REMOTES_UPGRADE = "always")
## Set `GITHUB_PAT` in `~/.Renviron` if you get a rate limit error.
remotes::install_github("acidgenomics/syntactic")
```

### [conda][] method

Configure [conda][] to use the [bioconda][] channels.

```sh
conda install -c bioconda r-syntactic
```

## Overview

[syntactic][] improves upon the `make.names()` functionality defined in base [R][], and is designed to return syntactically valid names from biological metadata. The package exports these primary functions:

- `camelCase()` (e.g. `helloWorld`).
- `dottedCase()` (e.g. `hello.world`).
- `snakeCase()` (e.g. `hello_world`).
- `upperCamelCase()` (e.g. `HelloWorld`).

Additionally, the package exports these utility functions:

- `makeNames()`: modified variant of `make.names()` that sanitizes using underscores instead of dots.
- `capitalize()`: Capitalize the first letter of all words in a character vector.

[syntactic][] is designed to handle many common mixed case acronyms (e.g. mRNA, RNAi), as well as decimals in names.

## Related packages

If [syntactic][] doesn't work quite right for you, these popular packages also provide excellent sanitization support:

- [janitor][] by Sam Firke.
- [lettercase][] by Christopher Brown.
- [snakecase][] by Malte Grosser.

[bioconductor]: https://bioconductor.org/
[basejump]: https://basejump.acidgenomics.com/
[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[janitor]: https://cran.r-project.org/package=janitor
[lettercase]: https://cran.r-project.org/package=lettercase
[r]: https://www.r-project.org/
[snakecase]: https://cran.r-project.org/package=snakecase
[syntactic]: https://syntactic.acidgenomics.com/
