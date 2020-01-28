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

[bioconda]: https://bioconda.github.io/
[conda]: https://conda.io/
[r]: https://www.r-project.org/
