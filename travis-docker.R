setwd("/syntactic")
rcmdcheck::rcmdcheck(path = ".", args = "--no-manual")
BiocCheck::BiocCheck(package = ".")
