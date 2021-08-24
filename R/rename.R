#' Rename files and/or directories using a syntactic naming function
#'
#' @export
#' @note Updated 2021-08-24.
#'
#' @details
#' Intelligently deals with a case-insensitive file system, if necessary.
#' This is very useful for macOS and Windows.
#'
#' Our syntactic naming functions can result in changes that only differ in
#' case, which are problematic on case-insensitive mounts, and require movement
#' of the files into a temporary file name before the final rename.
#'
#' @param x `character`.
#'   File and/or directory paths.
#' @param fun `character(1)`.
#'   Function name.
#'
#' @return `character`.
#'   Renamed file paths.
#'
#' @examples
#' testdir <- file.path(tempdir(), "testdata")
#' unlink(testdir, recursive = TRUE)
#' dir.create(testdir)
#' from <- file.path(testdir, c("helloWorld.txt", "fooBar.R"))
#' file.create(from)
#' print(basename(from))
#' to <- syntacticRename(from)
#' print(basename(to))
syntacticRename <- function(
    path,
    recursive = FALSE,
    fun = c("kebabCase", "snakeCase", "camelCase")
) {
    assert(
        allHaveAccess(path),
        isFlag(recursive)
    )
    fun <- match.arg(fun)
    what <- get(x = fun, envir = asNamespace(.pkgName), inherits = TRUE)
    assert(is.function(what))
    ## Shared arguments passed per file to syntactic naming function.
    args <- list(
        "names" = FALSE,
        "prefix" = FALSE,
        "smart" = FALSE
    )
    if (isTRUE(grepl(pattern = "camelcase", x = fun, ignore.case = TRUE))) {
        args[["strict"]] <- TRUE
    }
    if (isTRUE(recursive)) {
        from <- .recursive(path)
        from <- c(from[["files"]], from[["dirs"]])
    } else {
        from <- realpath(path)
    }
    insensitive <- !isTRUE(isFileSystemCaseSensitive())
    to <- vapply(
        X = from,
        what = what,
        args = args,
        insensitive = insensitive,
        FUN = function(from, what, args, insensitive) {
            dir <- dirname(from)
            ext <- fileExt(from)
            stem <- basenameSansExt(from)
            if (isFALSE(grepl(pattern = "^[A-Za-z0-9]", x = stem))) {
                alertInfo(sprintf("Skipping {.file %s}.", from))
                return(from)
            }
            args[["object"]] <- stem
            stem <- do.call(what = what, args = args)
            ## Add back extension if necessary. Note that this handles both
            ## files without an extension and directories in the call.
            if (!is.na(ext)) {
                bn <- paste0(stem, ".", ext)
            } else {
                bn <- stem
            }
            to <- file.path(dir, bn)
            if (identical(from, to)) {
                return(from)
            }
            alert(sprintf("Renaming {.file %s} to {.file %s}.", from, to))
            ## nocov start
            if (isTRUE(insensitive)) {
                tmpTo <- file.path(
                    dirname(from),
                    paste0(".tmp.", basename(from))
                )
                ok <- file.rename(from = from, to = tmpTo)
                assert(file.exists(tmpTo), ok)
                ok <- file.rename(from = tmpTo, to = to)
            } else {
                ok <- file.rename(from = from, to = to)
            }
            ## nocov end
            assert(file.exists(to), ok)
            to
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    invisible(to)
}
