require(devtools)
require(testthat)
load_all()

options(error = NULL)
devtools::test()
devtools::build(args = "--compact-vignettes=both") # R CMD build afex --compact-vignettes="gs+qpdf"
document()
check()

test_package("afex", filter = "aov")
test_package("afex", filter = "mixed")
test_package("afex", filter = "mixed-structure")
test_package("afex", filter = "mixed-bugs")
test_package("afex", filter = "mixed-mw")
test_package("afex", filter = "emmeans")


options(error = recover)
options(error = NULL)
options(warn = 2)
options(warn = 0)

## check for non-ASCII characters in examples:
for (f in list.files("examples/", full.names = TRUE)) {
  cat(f, "\n")
  tools::showNonASCIIfile(f)
}

## check for non-ASCII characters in R files:
for (f in list.files("R/", full.names = TRUE)) {
  cat(f, "\n")
  tools::showNonASCIIfile(f)
}

## check for non-ASCII characters in Rd files:
for (f in list.files("man/", full.names = TRUE)) {
  cat(f, "\n")
  tools::showNonASCIIfile(f)
}

## resave extdata:
rda_files <- list.files("inst/extdata/", full.names = TRUE)
for (i in rda_files) tools::resaveRdaFiles(i)

#install.packages("afex", dependencies = TRUE)
#devtools::build()
devtools::build_vignettes()
clean_vignettes(pkg = ".")

devtools::build(args = "--compact-vignettes=both")

### add packages

usethis::use_package("statmod", "Suggests")
usethis::use_package("rstanarm", "Suggests")
usethis::use_package("brms", "Suggests")
usethis::use_package("cowplot", "Suggests")
usethis::use_package("nlme", "Suggests")
#usethis::use_package("GLMMadaptive", "Suggests")
usethis::use_readme_rmd()
usethis::use_cran_badge()

### check reverse dependencies:

library(revdepcheck)  # see https://github.com/r-lib/revdepcheck
revdep_check(num_workers = 2)
revdep_summary()
revdep_details(revdep = "r2glmm")
