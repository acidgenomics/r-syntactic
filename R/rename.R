#' Rename files and/or directories using a syntactic naming function
#'
#' @export
#' @note Updated 2023-02-06.
#'
#' @details
#' Intelligently deals with a case-insensitive file system, if necessary.
#' This is very useful for macOS and Windows.
#'
#' Our syntactic naming functions can result in changes that only differ in
#' case, which are problematic on case-insensitive mounts, and require movement
#' of the files into a temporary file name before the final rename.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param path `character`.
#' File and/or directory paths.
#'
#' @param fun `character(1)`.
#' Function name.
#'
#' @param recursive `logical(1)`.
#' Should the function recurse into directories?
#'
#' @param dryRun `logical(1)`.
#' Return the proposed file path modifications without modification.
#'
#' @return `list`.
#' Named list containining `from` and `to` rename operations.
#'
#' @examples
#' testdir <- AcidBase::tempdir2()
#' from <- file.path(testdir, c("helloWorld.txt", "fooBar.R"))
#' file.create(from)
#' print(basename(from))
#' output <- syntacticRename(from)
#' print(output)
#' AcidBase::unlink2(testdir)
syntacticRename <-
    function(path,
             recursive = FALSE,
             fun = c(
                 "kebabCase",
                 "snakeCase",
                 "camelCase",
                 "upperCamelCase"
             ),
             quiet = FALSE,
             dryRun = FALSE) {
        assert(
            requireNamespaces("AcidBase"),
            allHaveAccess(path),
            isFlag(recursive),
            isFlag(quiet),
            isFlag(dryRun)
        )
        if (isTRUE(dryRun)) {
            assert(isFALSE(quiet))
        }
        if (isFALSE(quiet)) {
            assert(requireNamespaces("AcidCLI"))
        }
        fun <- match.arg(fun)
        ## Shared arguments passed per file to syntactic naming function.
        whatArgs <- list(
            "prefix" = FALSE,
            "smart" = TRUE
        )
        if (isSubset(fun, c("camelCase", "upperCamelCase"))) {
            whatArgs[["strict"]] <- TRUE
            lower <- FALSE
        } else {
            lower <- TRUE
        }
        if (isTRUE(recursive)) {
            from <- .recursive(path)
            from <- c(from[["files"]], from[["dirs"]])
        } else {
            from <- AcidBase::realpath(path)
        }
        assert(allHaveAccess(from))
        toPath <- function(from, what, whatArgs, lower, quiet) {
            dn <- dirname(from)
            ext <- AcidBase::fileExt(from)
            stem <- AcidBase::basenameSansExt(from)
            if (isFALSE(grepl(pattern = "^[A-Za-z0-9]", x = stem))) {
                if (isFALSE(quiet)) {
                    AcidCLI::alertInfo(sprintf("Skipping {.file %s}.", from))
                }
                return(from)
            }
            if (isTRUE(lower)) {
                stem <- tolower(stem)
            }
            whatArgs[["object"]] <- stem
            stem <- do.call(what = what, args = whatArgs)
            if (!is.na(ext)) {
                bn <- paste0(stem, ".", ext)
            } else {
                bn <- stem
            }
            file.path(dn, bn)
        }
        what <- get(
            x = fun,
            envir = asNamespace("AcidGenerics"),
            inherits = FALSE
        )
        to <- vapply(
            X = from,
            what = what,
            whatArgs = whatArgs,
            lower = lower,
            quiet = quiet,
            FUN = toPath,
            FUN.VALUE = character(1L),
            USE.NAMES = FALSE
        )
        if (isTRUE(dryRun)) {
            dryRunPath <- function(from, to) {
                AcidCLI::alertInfo(sprintf(
                    "[%s] {.file %s} -> {.file %s}.", "dry-run", from, to
                ))
            }
            Map(f = dryRunPath, from = from, to = to, USE.NAMES = FALSE)
            return(invisible(list("from" = character(), "to" = character())))
        }
        renamer <- function(from, to, caseSensitive) {
            if (identical(from, to)) {
                return(TRUE)
            }
            if (isFALSE(quiet)) {
                AcidCLI::alert(sprintf(
                    "Renaming {.file %s} to {.file %s}.", from, to
                ))
            }
            if (isFALSE(caseSensitive)) {
                tmpTo <- file.path(
                    dirname(from),
                    paste0(".tmp.", basename(from))
                )
                ok <- file.rename(from = from, to = tmpTo)
                ok <- file.rename(from = tmpTo, to = to)
            } else {
                ok <- file.rename(from = from, to = to)
            }
            ok
        }
        ok <- unlist(Map(
            f = renamer,
            from = from,
            to = to,
            MoreArgs = list("caseSensitive" = isFileSystemCaseSensitive()),
            USE.NAMES = FALSE
        ))
        assert(all(ok))
        invisible(list("from" = from, "to" = to))
    }



#' Sort files and directories for recursive rename
#'
#' This function prepares files and/or directories for recursive rename by
#' ordering from deepest to shallowest, using the `fileDepth()` function.
#'
#' This code may be generally useful, and we may want to export in basejump.
#'
#' @note Alternatively, can use `file.info(path)[["isdir"]]` here for speed.
#' @note Updated 2022-04-29.
#' @noRd
.recursive <- function(path) {
    assert(requireNamespace("AcidBase", quietly = TRUE))
    path <- AcidBase::realpath(path)
    nested <- unlist(lapply(
        X = path,
        FUN = function(path) {
            if (!isDirectory(path)) {
                return(path) # nocov
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
    x <- sort(unique(AcidBase::realpath(c(path, nested))))
    dirs <- x[isDirectory(x)]
    dirs <- rev(dirs[order(AcidBase::fileDepth(dirs))])
    files <- setdiff(x, dirs)
    files <- rev(files[order(AcidBase::fileDepth(files))])
    list(path = path, dirs = dirs, files = files)
}
