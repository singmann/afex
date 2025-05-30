

## set default options for afex_options:
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.afex <- list(
    afex.type = 3,
    afex.set_data_arg = FALSE,
    afex.check_contrasts = TRUE,
    afex.method_mixed = "S",
    afex.return_aov = "afex_aov",
    afex.es_aov = "ges",
    afex.correction_aov = "GG",
    afex.factorize = TRUE,
    afex.lmer_function = "lmerTest",
    afex.sig_symbols = c(" +", " *", " **", " ***"),
    afex.emmeans_model = c("multivariate"),
    afex.include_aov = FALSE,
    afex.round_ps = round_ps_apa
  )
  toset <- !(names(op.afex) %in% names(op))
  if(any(toset)) options(op.afex[toset])
  
  if (requireNamespace("emmeans", quietly = TRUE)) {
    emmeans::.emm_register(c("mixed", "afex_aov"), pkgname)
  }
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
	#assign(".oldContrasts", options("contrasts"), envir = .GlobalEnv)
  packageStartupMessage("************\nWelcome to afex. For support visit: http://afex.singmann.science/")
  packageStartupMessage("- Functions for ANOVAs: aov_car(), aov_ez(), and aov_4()\n- Methods for calculating p-values with mixed(): 'S', 'KR', 'LRT', and 'PB'\n- 'afex_aov' and 'mixed' objects can be passed to emmeans() for follow-up tests\n- Get and set global package options with: afex_options()\n- Set sum-to-zero contrasts globally: set_sum_contrasts()\n- For example analyses see: browseVignettes(\"afex\")\n************")
	#if (options("contrasts")[[1]][1] != "contr.sum") {
		#packageStartupMessage("Setting contrasts to effects coding: options(contrasts=c('contr.sum', 'contr.poly'))\nThis affects all functions using contrasts (e.g., lmer, lm, aov, ...).\nTo reset default settings run: options(contrasts=c('contr.treatment', 'contr.poly')) (all afex functions should be unaffected by this)\n")
    # \nPrevious contrasts saved in '.oldContrasts'.
		#options(contrasts=c('contr.sum', 'contr.poly'))
	#} else packageStartupMessage("Contrasts already set to effects coding: options(contrasts=c('contr.sum', '...'))\n")
  #packageStartupMessage("afex loads the required packages (e.g., lme4, car, pbkrtest) in an order that should not lead to problems.\nLoading any of the packages (specifically lme4) beforehand can lead to problems (especially with older versions of either).\nLoading nlme in addition to afex (before or after loading it), may especially lead to problems.\n************")
}

