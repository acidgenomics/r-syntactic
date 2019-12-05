#' Sort files and directories for recursive rename
#'
#' Note that files will be renamed first, then directories in reverse order
#' of deepest from shallowest.
#'
#' This code may be generally useful, and we may want to export in basejump.
#'
#' @note Alternatively, can use `file.info(path)[["isdir"]]` here for speed.
#' @note Updated 2019-12-05.
#' @noRd
.recursive <- function(path) {
    assert(allHaveAccess(path))
    nested <- lapply(
        X = path,
        FUN = function(path) {
            if (!isDirectory(path)) {
                return(path)
            }
            list.files(
                path = path,
                all.files = FALSE,
                full.names = TRUE,
                recursive = TRUE,
                include.dirs = TRUE
            )
        }
    )
    path <- unique(c(path, unlist(nested)))
    ## Order the deepest directories first.
    ## Note that use of `decreasing = TRUE` doesn't work the way I want here.
    dirs <- path[isDirectory(path)]
    dirs <- rev(dirs[order(fileDepth(dirs), decreasing = FALSE)])
    files <- setdiff(path, dirs)
    files <- sort(files)
    ## Rename files first, then tackle the directories.
    c(files, dirs)
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
#' @note Updated 2019-12-04.
#' @noRd
#'
#' @examples
#' ## > .rename(x = "sample-1.fastq.gz", fun = "snakeCase")
.rename <- function(x, fun, ...) {
    assert(isString(fun))
    insensitive <- !isTRUE(isFileSystemCaseSensitive())
    FUN <- get(  # nolint
        x = fun,
        envir = asNamespace("syntactic"),
        inherits = FALSE
    )
    from <- realpath(x)
    to <- vapply(
        X = x,
        FUN = function(x) {
            from <- x
            dir <- dirname(from)
            ext <- fileExt(from)
            stem <- basenameSansExt(from)
            ## Handle edge cases with file names that we want to avoid.
            stem <- gsub(
                pattern = "[[:space:]]+-[[:space:]]+",
                replacement = "-",
                x = stem
            )
            stem <- FUN(stem, rename = FALSE, ...)
            ## Add back extension if necessary. Note that this handles both
            ## files without an extension and directories in the call.
            if (!is.na(ext)) {
                bn <- tolower(paste0(stem, ".", ext))
            } else {
                bn <- stem
            }
            to <- file.path(dir, bn)
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
    if (identical(from, to)) {
        return(from)  # nocov
    }
    if (isTRUE(insensitive)) {
        ## nocov start
        tmpTo <- file.path(dirname(from), paste0(".tmp.", basename(from)))
        ok <- file.rename(from = from, to = tmpTo)
        assert(all(ok))
        ok <- file.rename(from = tmpTo, to = to)
        assert(all(ok))
        ## nocov end
    } else {
        ok <- file.rename(from = from, to = to)
        assert(all(ok))
    }
    to
}



## Updated 2019-07-19.
.sanitizeAcronyms <- function(x) {
    assert(is.atomic(x))
    x <- as.character(x)
    ## Identifier variants (e.g. "Id" to "ID").
    x <- gsub(
        pattern = "\\b(id)\\b",
        replacement = "ID",
        x = x,
        ignore.case = TRUE
    )
    ## Molarity (e.g. "10nM" to "10nm").
    ## Note that not including the front word boundary helps this work on
    ## examples such as "X10uM".
    x <- gsub(
        pattern = "([[:digit:]]+?[mnu]M)\\b",
        replacement = "\\L\\1",
        x = x,
        perl = TRUE
    )
    ## Plurarlized acronyms (e.g. "UMIs" to "UMIS").
    x <- gsub(
        pattern = "\\b([A-Z0-9]+)s\\b",
        replacement = "\\1S",
        x = x
    )
    ## Mixed case RNA types.
    x <- gsub(
        pattern = "\\b([mi|nc|pi|r]RNA)\\b",
        replacement = "\\U\\1",
        x = x,
        perl = TRUE
    )
    ## RNA interference.
    x <- gsub(
        pattern = "\\b(RNAi)\\b",
        replacement = "RNAI",
        x = x
    )
    ## Ethanol. EtOH splits into 2 words otherwise.
    ## Consider spelling out "Ethanol" instead if this is too funky.
    x <- gsub(
        pattern = "\\b(EtOH)\\b",
        replacement = "Etoh",
        x = x
    )
    ## Return.
    x
}
