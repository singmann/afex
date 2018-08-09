

## set default options for afex_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.afex <- list(
    afex.type = 3,
    afex.set_data_arg = FALSE,
    afex.check_contrasts = TRUE,
    afex.method_mixed = "KR",
    afex.return_aov = "afex_aov",
    afex.es_aov = "ges",
    afex.correction_aov = "GG",
    afex.factorize = TRUE,
    afex.lmer_function = "lmerTest",
    afex.sig_symbols = c(" +", " *", " **", " ***"),
    afex.emmeans_model = c("univariate")
  )
  toset <- !(names(op.afex) %in% names(op))
  if(any(toset)) options(op.afex[toset])
  
  if (requireNamespace("emmeans", quietly = TRUE)) {
        register_s3_method("emmeans", "recover_data", "mixed")
        register_s3_method("emmeans", "emm_basis", "mixed")
        register_s3_method("emmeans", "recover_data", "afex_aov")
        register_s3_method("emmeans", "emm_basis", "afex_aov")
    }
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
	#assign(".oldContrasts", options("contrasts"), envir = .GlobalEnv)
  packageStartupMessage("************\nWelcome to afex. For support visit: http://afex.singmann.science/")
  packageStartupMessage("- Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()\n- Methods for calculating p-values with mixed(): 'KR', 'S', 'LRT', and 'PB'\n- 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests\n- NEWS: library('emmeans') now needs to be called explicitly!\n- Get and set global package options with: afex_options()\n- Set orthogonal sum-to-zero contrasts globally: set_sum_contrasts()\n- For example analyses see: browseVignettes(\"afex\")\n************")
	#if (options("contrasts")[[1]][1] != "contr.sum") {
		#packageStartupMessage("Setting contrasts to effects coding: options(contrasts=c('contr.sum', 'contr.poly'))\nThis affects all functions using contrasts (e.g., lmer, lm, aov, ...).\nTo reset default settings run: options(contrasts=c('contr.treatment', 'contr.poly')) (all afex functions should be unaffected by this)\n")
    # \nPrevious contrasts saved in '.oldContrasts'.
		#options(contrasts=c('contr.sum', 'contr.poly'))
	#} else packageStartupMessage("Contrasts already set to effects coding: options(contrasts=c('contr.sum', '...'))\n")
  #packageStartupMessage("afex loads the required packages (e.g., lme4, car, pbkrtest) in an order that should not lead to problems.\nLoading any of the packages (specifically lme4) beforehand can lead to problems (especially with older versions of either).\nLoading nlme in addition to afex (before or after loading it), may especially lead to problems.\n************")
}

### Dynamic registration of S3 methods
# Code borrowed from hms pkg. I omitted some type checks etc. because
# this is only for internal use and I solemnly promise to behave myself.

register_s3_method <- function(pkg, generic, class) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
    if (isNamespaceLoaded(pkg)) {
        registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
    # Register hook in case package is later unloaded & reloaded
    setHook(
        packageEvent(pkg, "onLoad"),
        function(...) {
            registerS3method(generic, class, fun, envir = asNamespace(pkg))
        }
    )
}

