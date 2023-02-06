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
#' @return `character`.
#' Renamed file paths.
#'
#' @examples
#' testdir <- AcidBase::tempdir2()
#' from <- file.path(testdir, c("helloWorld.txt", "fooBar.R"))
#' file.create(from)
#' print(basename(from))
#' to <- syntacticRename(from)
#' print(basename(to))
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
             quiet = FALSE) {
        assert(
            requireNamespace("AcidBase", quietly = TRUE),
            allHaveAccess(path),
            isFlag(recursive),
            isFlag(quiet)
        )
        if (isFALSE(quiet)) {
            assert(requireNamespace("AcidCLI", quietly = TRUE))
        }
        fun <- match.arg(fun)
        what <- get(
            x = fun,
            envir = asNamespace("AcidGenerics"),
            inherits = FALSE
        )
        assert(is.function(what))
        ## Shared arguments passed per file to syntactic naming function.
        args <- list(
            "prefix" = FALSE,
            "smart" = TRUE
        )
        if (isSubset(fun, c("camelCase", "upperCamelCase"))) {
            args[["strict"]] <- TRUE
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
        insensitive <- !isTRUE(isFileSystemCaseSensitive())
        to <- vapply(
            X = from,
            what = what,
            args = args,
            insensitive = insensitive,
            lower = lower,
            quiet = quiet,
            FUN = function(from,
                           what,
                           args,
                           insensitive,
                           lower,
                           quiet) {
                dir <- dirname(from)
                ext <- AcidBase::fileExt(from)
                stem <- AcidBase::basenameSansExt(from)
                if (isFALSE(grepl(pattern = "^[A-Za-z0-9]", x = stem))) {
                    if (isFALSE(quiet)) {
                        AcidCLI::alertInfo(sprintf(
                            "Skipping {.file %s}.",
                            gsub(
                                pattern = "'",
                                replacement = "",
                                x = from
                            )
                        ))
                    }
                    return(from)
                }
                if (isTRUE(lower)) {
                    stem <- tolower(stem)
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
                if (isFALSE(quiet)) {
                    AcidCLI::alert(sprintf(
                        "Renaming {.file %s} to {.file %s}.",
                        gsub(
                            pattern = "'",
                            replacement = "",
                            x = from
                        ),
                        basename(to)
                    ))
                }
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
