require(devtools)
require(testthat)
load_all()

options(error = NULL)
devtools::test()

test_package("afex", filter = "aov")
test_package("afex", filter = "mixed")
test_package("afex", filter = "mixed-structure")
test_package("afex", filter = "mixed-bugs")
test_package("afex", filter = "mixed-mw")


options(error = recover)
options(error = NULL)
options(warn = 2)
options(warn = 0)

#install.packages("afex", dependencies = TRUE)
#devtools::build()
devtools::build_vignettes()
clean_vignettes(pkg = ".")


## Complete documentation including DESCRPTION file is written using roxygen2 and wrapper roxyPackage:
require(roxyPackage)  # install.packages("roxyPackage", repo="http://R.reaktanz.de")

pkg.src.dir <- "."

## Windows
R.libs <- "C:/R/R-devel/library"
# Linux:
R.libs <- "./packages/library"

closeAllConnections()
roxy.package(
	pck.source.dir = pkg.src.dir,
	pck.version = "0.17-5",
	pck.description = data.frame(
		Package = "afex",
		Type = "Package",
		Title = "Analysis of Factorial Experiments",
		AuthorsR = "c(person(given=\"Henrik\", family=\"Singmann\", role=c(\"aut\", \"cre\"), email=\"singmann+afex@gmail.com\"),
        person(given=\"Ben\", family=\"Bolker\", role=c(\"aut\")),
        person(given=\"Jake\", family=\"Westfall\", role=c(\"aut\")),
        person(given=\"Frederik\", family=\"Aust\", role=c(\"aut\")),
        person(given=\"Søren\", family=\"Højsgaard\", role=c(\"ctb\")),
        person(given=\"John\", family=\"Fox\", role=c(\"ctb\")),
        person(given=\"Michael A.\", family=\"Lawrence\", role=c(\"ctb\")),
        person(given=\"Ulf\", family=\"Mertens\", role=c(\"ctb\")),
        person(given=\"Jonathan\", family=\"Love\", role=c(\"ctb\"))
    )",
		Depends = "R (>= 3.1.0), lme4 (>= 1.1-8), lsmeans (>= 2.17)",
		Suggests = "ascii, xtable, parallel, plyr, optimx, nloptr, knitr, rmarkdown, lattice, multcomp, testthat, mlmRev, dplyr,dfoptim, Matrix",
		Imports = "stringr, coin, pbkrtest (>= 0.4-1), lmerTest, car, reshape2, stats, methods",
		Description = "Provides convenience functions for analyzing factorial experiments using ANOVA or mixed models. aov_ez(), aov_car(), and aov_4() allow specification of between, within (i.e., repeated-measures), or mixed between-within (i.e., split-plot) ANOVAs for data in long format (i.e., one observation per row), potentially aggregating multiple observations per individual and cell of the design. mixed() fits mixed models using lme4::lmer() and computes p-values for all fixed effects using either Kenward-Roger or Satterthwaite approximation for degrees of freedom (LMM only), parametric bootstrap (LMMs and GLMMs), or likelihood ratio tests (LMMs and GLMMs). afex uses type 3 sums of squares as default (imitating commercial statistical software).",
		URL = "https://github.com/singmann/afex",
		License = "GPL (>=2)",
		Encoding = "UTF-8",
    VignetteBuilder="knitr",
		stringsAsFactors = FALSE),
		actions = c("roxy"),
		R.libs = R.libs, 
		repo.root = tempdir())
#system("rmdir pkg/afex/inst")
writeLines(iconv(readLines("DESCRIPTION"), from = "latin1", to = "UTF-8"), file("DESCRIPTION", encoding="UTF-8"))
writeLines(iconv(readLines("man/afex-package.Rd"), from = "latin1", to = "UTF-8"), file("man/afex-package.Rd", encoding="UTF-8"))
writeLines(iconv(readLines("R/afex-package.R"), from = "latin1", to = "UTF-8"), file("R/afex-package.R", encoding="UTF-8"))
closeAllConnections()


### check reverse dependencies:

revdep()
revdep_check(libpath = "../revdep", check_dir = "../revdep_checks")
revdep_check(libpath = "../revdep")
install.packages("apa", lib = "../revdep")
revdep_check_resume()
revdep_check_save_summary()
revdep_check_print_problems()
revdep_maintainers()
