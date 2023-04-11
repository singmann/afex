require(devtools)
require(testthat)
library("vdiffr")
load_all()

options(error = NULL)
devtools::test()
devtools::build(args = "--compact-vignettes=both", 
                path = "development/") # R CMD build afex --compact-vignettes="gs+qpdf"
document()
check()
check_built(path = "development/afex_1.2-0.tar.gz")

## works better on windows:
devtools::test(filter = "plot", invert = TRUE)
devtools::test(filter = "plot")
devtools::test(filter = "afex_plot-basics")
devtools::test(filter = "afex_plot-vignette")


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

#####

rhub::validate_email("singmann@gmail.com")
rhub::check_for_cran("development/afex_1.2-0.tar.gz")

####

usethis::use_data(stroop, internal = FALSE, overwrite = TRUE)
usethis::use_vignette("afex_analysing_accuracy_data")
usethis::use_github_action_check_standard()

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

### add new data sets:
usethis::use_data(laptop_urry2021)

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
usethis::use_package("vdiffr", "Suggests")
#usethis::use_package("GLMMadaptive", "Suggests")
usethis::use_readme_rmd()
usethis::use_cran_badge()
usethis::use_code_of_conduct()

### check reverse dependencies:

library(revdepcheck)  # see https://github.com/r-lib/revdepcheck
revdep_check(num_workers = 2)
revdep_summary()
revdep_details(revdep = "r2glmm")
