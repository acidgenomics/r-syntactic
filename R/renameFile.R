#' Rename files using syntactically valid conventions
#'
#' @param x `character`.
#'   File and/or directory paths.
#' @param fun `character(1)`.
#'   Function name.
#'
#' @export
#' @note Updated 2021-08-24.
#'
#' @examples
#' print("FIXME Need to rework this.")
renameFile <- function(
    x,
    fun = c(
        "kebabCase",
        "snakeCase",
        "camelCase",
        "upperCamelCase"
    )
) {
    fun <- match.arg(fun)
    fun <- get(x = fun, envir = asNamespace(.pkgName), inherits = FALSE)
    assert(is.function(fun))
    ## FIXME Migrate the '.rename' code here.


}



#' Rename files and/or directories using a syntactic naming function
#'
#' @details
#' Intelligently deals with a case-insensitive file system, if necessary.
#' This is very useful for macOS and Windows.
#'
#' Our syntactic naming functions can result in changes that only differ in
#' case, which are problematic on case-insensitive mounts, and require movement
#' of the files into a temporary file name before the final rename.
#'
#' @note Updated 2021-08-24.
#' @noRd
#'
#' @examples
#' ## > .rename(path = "sample-1.fastq.gz", fun = "snakeCase")
.rename <- function(
    path,
    recursive = FALSE,
    smart = TRUE,
    strict = TRUE,  # camelCase only.
    fun,
    ...
) {
    assert(
        allHaveAccess(path),
        isFlag(recursive),
        isFlag(smart),
        isString(fun)
    )
    if (isTRUE(grepl(pattern = "camelcase", x = fun, ignore.case = TRUE))) {
        lower <- FALSE
    } else {
        lower <- TRUE
    }
    ## Since we're reexporting the S4 generics from AcidGenerics, we must
    ## inherit here, otherwise the function will fail.
    FUN <- get(  # nolint
        x = fun,
        envir = asNamespace("syntactic"),
        inherits = TRUE
    )
    insensitive <- !isTRUE(isFileSystemCaseSensitive())
    if (isTRUE(recursive)) {
        from <- .recursive(path)
        from <- c(from[["files"]], from[["dirs"]])
    } else {
        from <- realpath(path)
    }
    to <- vapply(
        X = from,
        FUN = function(from) {
            dir <- dirname(from)
            ext <- fileExt(from)
            stem <- basenameSansExt(from)
            ## Skip on files prefixed with "_", such as `_pkgdown.yml`.
            if (isTRUE(grepl(pattern = "^_", x = stem))) {
                alertInfo(sprintf("Skipping {.file %s}.", from))
                return(from)
            }
            if (isTRUE(smart)) {
                if (isTRUE(lower)) {
                    stem <- tolower(stem)
                }
                ## Handle edge cases with file names that we want to avoid.
                stem <- gsub(
                    pattern = "'",
                    replacement = "",
                    x = stem
                )
                stem <- gsub(
                    pattern = "[[:space:]]+-[[:space:]]+",
                    replacement = "-",
                    x = stem
                )
            }
            args <- list(
                object = stem,
                rename = FALSE,
                smart = smart,
                ...
            )
            if (fun == "camelCase") {
                args[["strict"]] <- strict
            }
            stem <- do.call(what = FUN, args = args)
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
            if (isTRUE(insensitive)) {
                ## nocov start
                tmpTo <- file.path(
                    dirname(from),
                    paste0(".tmp.", basename(from))
                )
                ok <- file.rename(from = from, to = tmpTo)
                assert(file.exists(tmpTo), ok)
                ok <- file.rename(from = tmpTo, to = to)
                ## nocov end
            } else {
                ok <- file.rename(from = from, to = to)  # nocov
            }
            assert(file.exists(to), ok)
            to
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    if (isTRUE(recursive)) {
        NULL
    } else {
        to
    }
}
