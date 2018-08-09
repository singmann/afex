require(devtools)
require(testthat)
load_all()

options(error = NULL)
devtools::test()
devtools::build()
document()

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

#install.packages("afex", dependencies = TRUE)
#devtools::build()
devtools::build_vignettes()
clean_vignettes(pkg = ".")


### check reverse dependencies:

library(revdepcheck)  # see https://github.com/r-lib/revdepcheck
revdep_check(num_workers = 2)
revdep_summary()
revdep_details(revdep = "r2glmm")
