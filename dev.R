require(devtools)
require(testthat)
load_all()

options(error = NULL)
devtools::test()
devtools::build()

test_package("afex", filter = "aov")
test_package("afex", filter = "mixed")
test_package("afex", filter = "mixed-structure")
test_package("afex", filter = "mixed-bugs")
test_package("afex", filter = "mixed-mw")
test_package("afex", filter = "lsmeans")


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
	pck.version = "0.20-2",
	pck.description = data.frame(
		Package = "afex",
		Type = "Package",
		Title = "Analysis of Factorial Experiments",
		AuthorsR = "c(person(given=\"Henrik\", family=\"Singmann\", role=c(\"aut\", \"cre\"), email=\"singmann+afex@gmail.com\", comment=c(ORCID=\"0000-0002-4842-3657\")),
        person(given=\"Ben\", family=\"Bolker\", role=c(\"aut\")),
        person(given=\"Jake\", family=\"Westfall\", role=c(\"aut\")),
        person(given=\"Frederik\", family=\"Aust\", role=c(\"aut\"), comment = c(ORCID = \"0000-0003-4900-788X\")),
        person(given=\"Søren\", family=\"Højsgaard\", role=c(\"ctb\")),
        person(given=\"John\", family=\"Fox\", role=c(\"ctb\")),
        person(given=\"Michael A.\", family=\"Lawrence\", role=c(\"ctb\")),
        person(given=\"Ulf\", family=\"Mertens\", role=c(\"ctb\")),
        person(given=\"Jonathan\", family=\"Love\", role=c(\"ctb\")),
        person(given=\"Russell\", family=\"Lenth\", role=c(\"ctb\")),
        person(given=\"Rune\", family=\"Haubo Bojesen Christensen\", role=c(\"ctb\"))
    )",
		Depends = "R (>= 3.1.0), lme4 (>= 1.1-8), emmeans",
		Suggests = "xtable, parallel, plyr, optimx, nloptr, knitr, rmarkdown, R.rsp, lattice, latticeExtra, multcomp, testthat, mlmRev, dplyr, tidyr, dfoptim, Matrix, psych, ggplot2, MEMSS, effects, carData",
		Imports = "stringr, coin, pbkrtest (>= 0.4-1), lmerTest, car, reshape2, stats, methods",
		Description = "Convenience functions for analyzing factorial experiments using ANOVA or mixed models. aov_ez(), aov_car(), and aov_4() allow specification of between, within (i.e., repeated-measures), or mixed between-within (i.e., split-plot) ANOVAs for data in long format (i.e., one observation per row), aggregating multiple observations per individual and cell of the design. mixed() fits mixed models using lme4::lmer() and computes p-values for all fixed effects using either Kenward-Roger or Satterthwaite approximation for degrees of freedom (LMM only), parametric bootstrap (LMMs and GLMMs), or likelihood ratio tests (LMMs and GLMMs). afex uses type 3 sums of squares as default (imitating commercial statistical software).",
		URL = "http://afex.singmann.science/, https://github.com/singmann/afex",
		BugReports = "https://github.com/singmann/afex/issues",
		License = "GPL (>=2)",
		Encoding = "UTF-8",
    VignetteBuilder="knitr, R.rsp",
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

library(revdepcheck)  # see https://github.com/r-lib/revdepcheck
revdep_check(num_workers = 2)
revdep_summary()
revdep_details(revdep = "r2glmm")
