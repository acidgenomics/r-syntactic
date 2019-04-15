rcmdcheck::rcmdcheck(args = "--no-manual")
BiocCheck::BiocCheck(`quit-with-status` = TRUE)
lintr::lint_package()
covr::report()
