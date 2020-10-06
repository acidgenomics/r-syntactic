#' Sort files and directories for recursive rename
#'
#' This function prepares files and/or directories for recursive rename by
#' ordering from deepest to shallowest, using the `fileDepth()` function.
#'
#' This code may be generally useful, and we may want to export in basejump.
#'
#' @note Alternatively, can use `file.info(path)[["isdir"]]` here for speed.
#' @note Updated 2019-12-08.
#' @noRd
.recursive <- function(path) {
    path <- realpath(path)
    nested <- unlist(lapply(
        X = path,
        FUN = function(path) {
            if (!isDirectory(path)) {
                return(path)  # nocov
            }
            list.files(
                path = path,
                all.files = FALSE,
                full.names = TRUE,
                recursive = TRUE,
                include.dirs = TRUE
            )
        }
    ))
    x <- sort(unique(realpath(c(path, nested))))
    dirs <- x[isDirectory(x)]
    dirs <- rev(dirs[order(fileDepth(dirs))])
    files <- setdiff(x, dirs)
    files <- rev(files[order(fileDepth(files))])
    list(path = path, dirs = dirs, files = files)
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
#' @note Updated 2020-04-07.
#' @noRd
#'
#' @examples
#' ## > .rename(path = "sample-1.fastq.gz", fun = "snakeCase")
.rename <- function(
    path,
    recursive = FALSE,
    smart = TRUE,
    strict = FALSE,  # camelCase only
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
                message(sprintf("Skipping '%s'.", from))
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
            message(paste0("Renaming '", from, "' to '", to, "'."))
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



## Updated 2020-07-09.
.sanitizeAcronyms <- function(x) {
    assert(is.atomic(x))
    x <- as.character(x)
    ## Note that underscores are not considered a word boundary.
    x <- gsub(pattern = "_", replacement = ".", x = x)
    ## Identifier variants (e.g. "Id" to "ID").
    x <- gsub(
        pattern = "\\b(id)\\b",
        replacement = "ID",
        x = x,
        ignore.case = TRUE
    )
    ## Molarity (e.g. "10nM" to "10nm").
    x <- gsub(
        pattern = "\\b([mnu]M)\\b",
        replacement = "\\L\\1",
        x = x,
        perl = TRUE
    )
    ## Note that not including the front word boundary helps this work on
    ## examples such as "X10uM".
    x <- gsub(
        pattern = "([[:digit:]]+[mnu]M)\\b",
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
    x <- gsub(pattern = "\\.", replacement = "_", x = x)
    x
}



## Used internally to hand off to camel, dotted, and snake case.
## Updated 2020-07-08.
.syntactic <- function(x, smart = TRUE, prefix = TRUE) {
    assert(
        is.atomic(x),
        isFlag(smart),
        isFlag(prefix)
    )
    x <- makeNames(x, smart = smart, unique = FALSE)
    if (isTRUE(smart)) {
        ## Standardize any mixed case acronyms.
        x <- .sanitizeAcronyms(x)
    }
    ## Include "X" prefix by default, but allowing manual disable, so we
    ## can pass to our shell scripts defined in koopa package.
    if (identical(prefix, FALSE)) {
        x <- gsub(
            pattern = "^X([^[:alpha:]])",
            replacement = "\\1",
            x = x,
            ignore.case = TRUE
        )
    }
    ## Establish word boundaries for camelCase acronyms
    ## (e.g. `worfdbHTMLRemap` -> `worfdb_HTML_remap`).
    ## Acronym following a word.
    x <- gsub(
        pattern = "([a-z])([A-Z])",
        replacement = "\\1_\\2",
        x = x
    )
    ## Word following an acronym.
    x <- gsub(
        pattern = "([A-Z0-9])([A-Z])([a-z])",
        replacement = "\\1_\\2\\3",
        x = x
    )
    ## Return.
    x
}
