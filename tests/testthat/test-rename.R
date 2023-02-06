test_that("kebabCase (non-recursive)", {
    testdir <- tempdir2()
    dirs <- file.path(testdir, c("aaa_bbb", "ccc_ddd"))
    files <- file.path(
        testdir,
        c(
            "1_sample_A.fastq.gz",
            "2.sample.B.fastq.gz",
            "hello_world.txt",
            "loadSingleCell.R",
            "quality_control.Rmd",
            "_test.txt",
            "mike's notes.txt"
        )
    )
    input <- c(files, dirs)
    invisible(lapply(dirs, dir.create, recursive = TRUE))
    invisible(file.create(files))
    output <- syntacticRename(path = input, fun = "kebabCase", quiet = FALSE)
    expect_type(output, "list")
    expect_identical(
        object = output[["to"]],
        expected = file.path(
            testdir,
            c(
                "1-sample-a.fastq.gz",
                "2-sample-b.fastq.gz",
                "hello-world.txt",
                "loadsinglecell.R",
                "quality-control.Rmd",
                "_test.txt",
                "mikes-notes.txt",
                "aaa-bbb",
                "ccc-ddd"
            )
        )
    )
    unlink2(testdir)
})

test_that("kebabCase (recursive)", {
    path <- file.path(tempdir2(), "recursive")
    subdirs <- file.path(path, "level_1", "level_2")
    initDir(subdirs)
    file.create(
        file.path(path, "file_x.TXT"),
        file.path(path, "level_1", "file_x.txt"),
        file.path(path, "level_1", "level_2", "file_x.txt")
    )
    output <- syntacticRename(
        path = path,
        recursive = TRUE,
        fun = "kebabCase"
    )
    expect_type(output, "list")
    expect_identical(
        object = output[["from"]],
        expected = c(
            file.path(path, "level_1", "level_2", "file_x.txt"),
            file.path(path, "level_1", "file_x.txt"),
            file.path(path, "file_x.TXT"),
            file.path(path, "level_1", "level_2"),
            file.path(path, "level_1"),
            path
        )
    )
    expect_identical(
        object = output[["to"]],
        expected = c(
            file.path(path, "level_1", "level_2", "file-x.txt"),
            file.path(path, "level_1", "file-x.txt"),
            file.path(path, "file-x.TXT"),
            file.path(path, "level_1", "level-2"),
            file.path(path, "level-1"),
            path
        )
    )
    files <- sort(append(
        x = path,
        values = list.files(
            path = path,
            full.names = TRUE,
            recursive = TRUE,
            include.dirs = TRUE
        )
    ))
    expected <- append(
        x = path,
        values = file.path(
            path,
            c(
                "file-x.TXT",
                "level-1",
                file.path("level-1", "file-x.txt"),
                file.path("level-1", "level-2"),
                file.path("level-1", "level-2", "file-x.txt")
            )
        )
    )
    expect_identical(files, expected)
    unlink2(path)
})

test_that("snakeCase (non-recursive)", {
    testdir <- tempdir2()
    dirs <- file.path(testdir, c("aaa-bbb", "ccc-ddd"))
    files <- file.path(testdir, c("1-sample-A.fastq.gz", "hello-world.txt"))
    input <- c(files, dirs)
    invisible(lapply(dirs, dir.create, recursive = TRUE))
    invisible(file.create(files))
    output <- syntacticRename(input, fun = "snakeCase")
    expect_identical(
        object = output[["to"]],
        expected = file.path(
            testdir,
            c(
                "1_sample_a.fastq.gz",
                "hello_world.txt",
                "aaa_bbb",
                "ccc_ddd"
            )
        )
    )
    unlink2(testdir)
})

test_that("camelCase (non-recursive)", {
    testdir <- tempdir2()
    dirs <- file.path(testdir, c("aaa-bbb", "ccc-ddd"))
    files <- file.path(
        testdir,
        c(
            "1-sample-A.fastq.gz",
            "hello-world.txt",
            "helloWORLD.R"
        )
    )
    input <- c(files, dirs)
    invisible(lapply(dirs, dir.create, recursive = TRUE))
    invisible(file.create(files))
    output <- syntacticRename(input, fun = "camelCase")
    expect_identical(
        object = output[["to"]],
        expected = file.path(
            testdir,
            c(
                "1SampleA.fastq.gz",
                "helloWorld.txt",
                "helloWorld.R",
                "aaaBbb",
                "cccDdd"
            )
        )
    )
    unlink2(testdir)
})

test_that("upperCamelCase (non-recursive)", {
    testdir <- tempdir2()
    dirs <- file.path(testdir, c("aaa-bbb", "ccc-ddd"))
    files <- file.path(testdir, c("1-sample-A.fastq.gz", "hello-world.txt"))
    input <- c(files, dirs)
    invisible(lapply(dirs, dir.create, recursive = TRUE))
    invisible(file.create(files))
    output <- syntacticRename(input, fun = "upperCamelCase")
    expect_identical(
        object = output[["to"]],
        expected = file.path(
            testdir,
            c(
                "1SampleA.fastq.gz",
                "HelloWorld.txt",
                "AaaBbb",
                "CccDdd"
            )
        )
    )
    unlink2(testdir)
})
