

## set default options for afex.options:
.afexEnv <- new.env()
assign("type", 3, envir = .afexEnv)
assign("check.contrasts", TRUE, envir = .afexEnv)
assign("method_mixed",  "KR", envir = .afexEnv)
assign("return_aov",  "afex_aov", envir = .afexEnv)
assign("es_aov",  "ges", envir = .afexEnv)
assign("correction_aov",  "GG", envir = .afexEnv)
assign("factorize", TRUE, envir = .afexEnv)

.onAttach <- function(libname, pkgname) {
	#assign(".oldContrasts", options("contrasts"), envir = .GlobalEnv)
  packageStartupMessage("************\nWelcome to afex. Important changes in the current version:")
  packageStartupMessage("- Functions for ANOVAs have been renamed to: aov_car(), aov_ez(), and aov_4().\n- ANOVA functions return an object of class 'afex_aov' as default, see: ?aov_car\n- 'afex_aov' objects can be passed to lsmeans for contrasts and follow-up tests.\n- Reset previous (faster) behavior via: afex_options(return_aov='nice')\n- Many more arguments can now be set globally via options, see: afex_options()\n************")
	#if (options("contrasts")[[1]][1] != "contr.sum") {
		#packageStartupMessage("Setting contrasts to effects coding: options(contrasts=c('contr.sum', 'contr.poly'))\nThis affects all functions using contrasts (e.g., lmer, lm, aov, ...).\nTo reset default settings run: options(contrasts=c('contr.treatment', 'contr.poly')) (all afex functions should be unaffected by this)\n")
    # \nPrevious contrasts saved in '.oldContrasts'.
		#options(contrasts=c('contr.sum', 'contr.poly'))
	#} else packageStartupMessage("Contrasts already set to effects coding: options(contrasts=c('contr.sum', '...'))\n")
  #packageStartupMessage("afex loads the required packages (e.g., lme4, car, pbkrtest) in an order that should not lead to problems.\nLoading any of the packages (specifically lme4) beforehand can lead to problems (especially with older versions of either).\nLoading nlme in addition to afex (before or after loading it), may especially lead to problems.\n************")
}

