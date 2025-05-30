% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mixed.R
\encoding{UTF-8}
\name{mixed}
\alias{mixed}
\alias{lmer_alt}
\title{p-values for fixed effects of mixed-model via lme4::lmer()}
\usage{
mixed(
  formula,
  data,
  type = afex_options("type"),
  method = afex_options("method_mixed"),
  per_parameter = NULL,
  args_test = list(),
  test_intercept = FALSE,
  check_contrasts = afex_options("check_contrasts"),
  expand_re = FALSE,
  all_fit = FALSE,
  set_data_arg = afex_options("set_data_arg"),
  progress = interactive(),
  cl = NULL,
  return = "mixed",
  sig_symbols = afex_options("sig_symbols"),
  ...
)

lmer_alt(formula, data, check_contrasts = FALSE, ...)
}
\arguments{
\item{formula}{a formula describing the full mixed-model to be fitted. As
this formula is passed to \code{lmer}, it needs at least one random term.}

\item{data}{\code{data.frame} containing the data. Should have all the
variables present in \code{fixed}, \code{random}, and \code{dv} as columns.}

\item{type}{type of test on which effects are based. Default is to use type 3
tests, taken from \code{\link{afex_options}}.}

\item{method}{character vector indicating which methods for obtaining
p-values should be used: \code{"S"} corresponds to the Satterthwaite
approximation for degrees of freedom (via \code{\link[lmerTest]{lmerTest}},
only LMMs), \code{"KR"} corresponds to the Kenward-Roger approximation for
degrees of freedom (only LMMs), \code{"PB"} calculates p-values based on
parametric bootstrap, \code{"LRT"} calculates p-values via the likelihood
ratio tests implemented in the \code{anova} method for \code{merMod}
objects (only recommended for models with many [i.e., > 50] levels for the
random factors). The default (currently \code{"S"}) is taken from
\code{\link{afex_options}}. For historical compatibility \code{"nested-KR"}
is also supported which was the default KR-method in previous versions.}

\item{per_parameter}{\code{character} vector specifying for which variable
tests should be run for each parameter (instead for the overall effect).
Can be useful e.g., for testing ordered factors. Uses \code{\link{grep}}
for selecting parameters among the fixed effects so regular expressions
(\code{\link{regex}}) are possible. See Examples.}

\item{args_test}{\code{list} of arguments passed to the function calculating
the p-values. See Details.}

\item{test_intercept}{logical. Whether or not the intercept should also be
fitted and tested for significance. Default is \code{FALSE}. Only relevant
if \code{type = 3}.}

\item{check_contrasts}{\code{logical}. Should contrasts be checked and (if
necessary) changed to \code{"contr.sum"}? See Details. The default
(\code{"TRUE"}) is taken from \code{\link{afex_options}}.}

\item{expand_re}{logical. Should random effects terms be expanded (i.e.,
factors transformed into numerical variables) before fitting with
\code{(g)lmer}? Allows to use "||" notation with factors.}

\item{all_fit}{logical. Should \code{\link[lme4]{allFit}} be used to fit each
model with each available optimization algorithm and the results that
provided the best fit in each case be used? Warning: This can dramatically
increase the optimization time. Adds two new attributes to the returned
object designating which algorithm was selected and the log-likelihoods for
each algorithm. Note that only warnings from the initial fit are emitted
during fitting. The warnings of the chosen models are emitted when printing
the returned object.}

\item{set_data_arg}{\code{logical}. Should the data argument in the slot
\code{call} of the \code{merMod} object returned from \code{lmer} be set to
the passed data argument? If \code{FALSE} (currently the default) the name
will be \code{data}. \code{TRUE} may be helpful when fitted objects are
used afterwards (e.g., compared using \code{anova} or when using the
\code{effects} package, see examples). \pkg{emmeans} functions appear to
work better with \code{FALSE}. Default is given by
afex_options("set_data_arg").}

\item{progress}{if \code{TRUE}, shows progress with a text progress bar and
other status messages during estimation. The default is to set \code{TRUE}
for interactive usage and \code{FALSE} for non-interactive usage.}

\item{cl}{A vector identifying a cluster; used for distributing the
estimation of the different models using several cores (if seveal models
are calculated). See examples. If \code{ckeck_contrasts = TRUE}, mixed sets
the current contrasts (\code{getOption("contrasts")}) at the nodes. Note
this does \emph{not} distribute calculation of p-values (e.g., when using
\code{method = "PB"}) across the cluster. Use \code{args_test} for this.}

\item{return}{the default is to return an object of class \code{"mixed"}.
\code{return = "merMod"} will skip the calculation of all submodels and
p-values and simply return the full model estimated with \code{lmer} (note
that somewhat unintuiviely, the returned object can either be of class
\code{"lmerModLmerTest"} or of class \code{"merMod"}, depending on the
value of \code{\link{afex_options}}\code{("lmer_function")}). Can be useful
in combination with \code{expand_re = TRUE} which allows to use "||" with
factors. \code{return = "data"} will not fit any models but just return the
data that would have been used for estimating the model (note that the data
is also part of the returned object).}

\item{sig_symbols}{Character. What should be the symbols designating
significance? When entering an vector with \code{length(sig.symbol) < 4}
only those elements of the default (\code{c(" +", " *", " **", " ***")})
will be replaced. \code{sig_symbols = ""} will display the stars but not
the \code{+}, \code{sig_symbols = rep("", 4)} will display no symbols. The
default is given by \code{afex_options("sig_symbols")}.}

\item{...}{further arguments (such as \code{weights}, \code{family}, or
\code{control}) passed to
\code{\link[lme4]{lmer}}/\code{\link[lme4]{glmer}}. Note that additional
data (e.g., \code{weights}) need to be passed fully and not only
by name (e.g., \code{weights = df$weights} and not \code{weights =
weights}).}
}
\value{
An object of class \code{"mixed"} (i.e., a list) with the following
  elements:

\enumerate{
  \item \code{anova_table} a data.frame containing the statistics returned
    from \code{\link[pbkrtest]{KRmodcomp}}. The \code{stat} column in this
    data.frame gives the value of the test statistic, an F-value for
    \code{method = "KR"} and a chi-square value for the other two methods.
  \item \code{full_model} the \code{"lmerModLmerTest"} or \code{"merMod"}
    object returned from estimating the full model. Use
    \code{\link{afex_options}}\code{("lmer_function")} for setting which
    function for estimation should be used. The possible options are
    \code{"lmerTest"} (the default returning an object of class
    \code{"lmerModLmerTest"}) and \code{"lme4"} returning an object of class
    (\code{"merMod"}). Note that in case a \code{family} argument is present
    an object of class \code{"glmerMod"} is always returned.
  \item \code{restricted_models} a list of \code{"g/lmerMod"} (or 
    \code{"lmerModLmerTest"}) objects from estimating the restricted models
    (i.e., each model lacks the corresponding effect)
  \item \code{tests} a list of objects returned by the function for 
    obtaining the p-values.
  \item \code{data} The data used for estimation (i.e., after excluding 
    missing rows and applying expand_re if requested).
  \item \code{call} The matched call.
}

It also has the following attributes, \code{"type"} and \code{"method"}. And
the attributes \code{"all_fit_selected"} and \code{"all_fit_logLik"} if
\code{all_fit=TRUE}.

Two similar methods exist for objects of class \code{"mixed"}: \code{print}
and \code{anova}. They print a nice version of the \code{anova_table} element
of the returned object (which is also invisibly returned). This methods omit
some columns and nicely round the other columns. The following columns are
always printed:
\enumerate{
  \item \code{Effect} name of effect
  \item \code{p.value} estimated p-value for the effect
}

For LMMs with \code{method="KR"} or \code{method="S"} the following further
columns are returned (note: the Kenward-Roger correction does two separate
things: (1) it computes an effective number for the denominator df; (2) it
scales the statistic by a calculated amount, see also
\url{https://stackoverflow.com/a/25612960/289572}):
\enumerate{
  \item \code{F} computed F statistic
  \item \code{ndf} numerator degrees of freedom (number of parameters used
    for the effect)
  \item \code{ddf} denominator degrees of freedom (effective residual 
    degrees of freedom for testing the effect), computed from the
    Kenward-Roger correction using \code{pbkrtest::KRmodcomp}
  \item \code{F.scaling} scaling of F-statistic computing from Kenward-Roger 
    approximation (only printed if \code{method="nested-KR"})
}

For models with \code{method="LRT"} the following further columns are
returned:
\enumerate{
  \item \code{df.large} degrees of freedom (i.e., estimated paramaters) for 
    full model (i.e., model containing the corresponding effect)
  \item \code{df.small} degrees of freedom (i.e., estimated paramaters) for 
    restricted model (i.e., model without the corresponding effect)
  \item \code{chisq} 2 times the difference in likelihood (obtained with 
    \code{logLik}) between full and restricted model
  \item \code{df} difference in degrees of freedom between full and 
    restricted model (p-value is based on these df).
}

For models with \code{method="PB"} the following further column is returned:
\enumerate{
  \item \code{stat} 2 times the difference in likelihood (obtained with 
    \code{logLik}) between full and restricted model (i.e., a chi-square
    value).
}

Note that  \code{anova} can also be called with additional mixed and/or
\code{merMod} objects. In this casethe full models are passed on to
\code{anova.merMod} (with \code{refit=FALSE}, which differs from the default
of \code{anova.merMod}) which produces the known LRT tables.

The \code{summary} method for objects of class \code{mixed} simply calls
\code{\link[lme4]{summary.merMod}} on the full model.

If \code{return = "merMod"} (or when invoking \code{lmer_alt}), an object of
class \code{"lmerModLmerTest"} or of class \code{"merMod"} (depending on the
value of \code{\link{afex_options}}\code{("lmer_function")}), as returned
from \code{g/lmer}, is returned. The default behavior is to return an object
of class \code{"lmerModLmerTest"} estimated via \code{\link[lmerTest]{lmer}}.
}
\description{
Estimates mixed models with \pkg{lme4} and calculates p-values
  for all fixed effects. The default method \code{"KR"} (= Kenward-Roger) as
  well as  \code{method="S"} (Satterthwaite) support LMMs and estimate the
  model with \code{\link[lmerTest]{lmer}} and then pass it to the
  \code{\link[lmerTest]{lmerTest}} \code{anova} method (or
  \code{\link[car]{Anova}}). The other methods (\code{"LRT"} =
  likelihood-ratio tests and \code{"PB"} = parametric bootstrap) support both
  LMMs (estimated via \code{\link[lme4]{lmer}}) and GLMMs (i.e., with
  \code{family} argument which invokes estimation via
  \code{\link[lme4]{glmer}}) and estimate a full model and restricted models
  in which the parameters corresponding to one effect (i.e., model term) are
  withhold (i.e., fixed to 0). Per default tests are based on Type 3 sums of
  squares. \code{print}, \code{nice}, \code{anova}, and \code{summary}
  methods for the returned object of class \code{"mixed"} are available.
  \code{summary} invokes the default \pkg{lme4} summary method and shows
  parameters instead of effects.

  \code{lmer_alt} is simply a wrapper for mixed that only returns the
  \code{"lmerModLmerTest"} or \code{"merMod"} object and correctly uses the
  \code{||} notation for removing correlations among factors. This function
  otherwise behaves like \code{g/lmer} (as for \code{mixed}, it calls
  \code{glmer} as soon as a \code{family} argument is present). Use
  \code{\link{afex_options}}\code{("lmer_function")} to set which function
  for estimation should be used. This option determines the class of the
  returned object (i.e., \code{"lmerModLmerTest"} or \code{"merMod"}).
}
\details{
For an introduction to mixed-modeling for experimental designs see
 our chapter
 (\href{http://singmann.org/download/publications/singmann_kellen-introduction-mixed-models.pdf}{Singmann
  & Kellen, in press}) or Barr, Levy, Scheepers, & Tily (2013). Arguments for
 using the Kenward-Roger approximation for obtaining p-values are given by
 Judd, Westfall, and Kenny (2012). Further introductions to mixed-modeling
 for experimental designs are given by Baayen and colleagues (Baayen, 2008;
 Baayen, Davidson & Bates, 2008; Baayen & Milin, 2010). Specific
 recommendations on which random effects structure to specify for
 confirmatory tests can be found in Barr and colleagues (2013) and Barr
 (2013), but also see Bates et al. (2015).

 \subsection{p-value Calculations}{

 When \code{method = "KR"} (implemented via
 \code{\link[pbkrtest]{KRmodcomp}}), the Kenward-Roger approximation for
 degrees-of-freedom is calculated using \code{\link[lmerTest]{lmerTest}} (if
 \code{test_intercept=FALSE}) or \code{\link[car]{Anova}} (if
 \code{test_intercept=TRUE}), which is only applicable to linear-mixed models
 (LMMs). The test statistic in the output is an F-value (\code{F}). A similar
 method that requires less RAM is \code{method = "S"} which calculates the
 Satterthwaite approximation for degrees-of-freedom via
 \code{\link[lmerTest]{lmerTest}} and is also only applicable to LMMs.
 \code{method = "KR"} or \code{method = "S"} provide the best control for
 Type 1 errors for LMMs (Luke, 2017).

 \code{method = "PB"} calculates p-values using parametric bootstrap using
 \code{\link[pbkrtest]{PBmodcomp}}. This can be used for linear and also
 generalized linear mixed models (GLMMs) by specifying a
 \code{\link[stats]{family}} argument to \code{mixed}. Note that you should
 specify further arguments to \code{PBmodcomp} via \code{args_test},
 especially \code{nsim} (the number of simulations to form the reference
 distribution) or \code{cl} (for using multiple cores). For other arguments
 see \code{\link[pbkrtest]{PBmodcomp}}. Note that \code{REML} (argument to
 \code{[g]lmer}) will be set to \code{FALSE} if method is \code{PB}.

 \code{method = "LRT"} calculates p-values via likelihood ratio tests
 implemented in the \code{anova} method for \code{"merMod"} objects. This is
 the method recommended by Barr et al. (2013; which did not test the other
 methods implemented here). Using likelihood ratio tests is only recommended
 for models with many levels for the random effects (> 50), but can be pretty
 helpful in case the other methods fail (due to memory and/or time
 limitations). The
 \href{http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html}{lme4 faq} also
 recommends the other methods over likelihood ratio tests. }

 \subsection{Implementation Details}{

 For methods \code{"KR"} and \code{"S"} type 3 and 2 tests are implemented as
 in \code{\link[car]{Anova}}.

 For all other methods, type 3 tests are obtained by comparing a model in
 which only the tested effect is excluded with the full model (containing all
 effects). For method \code{"nested-KR"} (which was the default in previous
 versions) this corresponds to the (type 3) Wald tests given by
 \code{car::Anova} for \code{"lmerMod"} models. The submodels in which the
 tested effect is excluded are obtained by manually creating a model matrix
 which is then fitted in \code{"lme4"}.

 Type 2 tests are truly sequential. They are obtained by comparing a model in
 which the tested effect and all higher oder effect (e.g., all three-way
 interactions for testing a two-way interaction) are excluded with a model in
 which only effects up to the order of the tested effect are present and all
 higher order effects absent. In other words, there are multiple full models,
 one for each order of effects. Consequently, the results for lower order
 effects are identical of whether or not higher order effects are part of the
 model or not. This latter feature is not consistent with classical ANOVA
 type 2 tests but a consequence of the sequential tests (and
 \href{https://stat.ethz.ch/pipermail/r-sig-mixed-models/2012q3/018992.html}{I
 didn't find a better way} of implementing the Type 2 tests). This
 \strong{does not} correspond to the (type 2) Wald test reported by
 \code{car::Anova}.

 If \code{check_contrasts = TRUE}, contrasts will be set to
 \code{"contr.sum"} for all factors in the formula if default contrasts are
 not equal to \code{"contr.sum"} or \code{attrib(factor, "contrasts") !=
 "contr.sum"}. Furthermore, the current contrasts (obtained via
 \code{getOption("contrasts")}) will be set at the cluster nodes if \code{cl}
 is not \code{NULL}. }

 \subsection{Expand Random Effects}{ \code{expand_re = TRUE} allows to expand
 the random effects structure before passing it to \code{lmer}. This allows
 to disable estimation of correlation among random effects for random effects
 term containing factors using the \code{||} notation which may aid in
 achieving model convergence (see Bates et al., 2015). This is achieved by
 first creating a model matrix for each random effects term individually,
 rename and append the so created columns to the data that will be fitted,
 replace the actual random effects term with the so created variables
 (concatenated with +), and then fit the model. The variables are renamed by
 prepending all variables with rei (where i is the number of the random
 effects term) and replacing ":" with "_by_".

 \code{lmer_alt} is simply a wrapper for \code{mixed} that is intended to
 behave like \code{lmer} (or \code{glmer} if a \code{family} argument is
 present), but also allows the use of \code{||} with factors (by always using
 \code{expand_re = TRUE}). This means that \code{lmer_alt} per default does
 not enforce a specific contrast on factors and only returns the
 \code{"lmerModLmerTest"} or \code{"merMod"} object without calculating any
 additional models or p-values (this is achieved by setting \code{return =
 "merMod"}). Note that it most likely differs from \code{g/lmer} in how it
 handles missing values so it is recommended to only pass data without
 missing values to it!

 One consequence of using \code{expand_re = TRUE} is that the data that is
 fitted will not be the same as the passed data.frame which can lead to
 problems with e.g., the \code{predict} method. However, the actual data used
 for fitting is also returned as part of the \code{mixed} object so can be
 used from there. Note that the \code{set_data_arg} can be used to change
 whether the \code{data} argument in the call to \code{g/lmer} is set to
 \code{data} (the default) or the name of the data argument passed by the
 user. }
}
\note{
When \code{method = "KR"}, obtaining p-values is known to crash due too
  insufficient memory or other computational limitations (especially with
  complex random effects structures). In these cases, the other methods
  should be used. The RAM demand is a problem especially on 32 bit Windows
  which only supports up to 2 or 3GB RAM (see
  \href{https://CRAN.R-project.org/bin/windows/base/rw-FAQ.html}{R Windows
  FAQ}). Then it is probably a good idea to use methods "S", "LRT", or "PB".

  \code{"mixed"} will throw a message if numerical variables are not centered
  on 0, as main effects (of other variables then the numeric one) can be hard
  to interpret if numerical variables appear in interactions. See Dalal &
  Zickar (2012).

  Per default \code{mixed} uses \code{\link[lmerTest]{lmer}}, this can be
  changed to \code{\link[lme4]{lmer}} by calling:
  \code{afex_options(lmer_function = "lme4")}

  Formulas longer than 500 characters will most likely fail due to the use of
  \code{\link{deparse}}.

  Please report bugs or unexpected behavior by opening a guthub issue:
  \url{https://github.com/singmann/afex/issues}
}
\examples{

##################################
## Simple Examples (from MEMSS) ##
##################################

if (requireNamespace("MEMSS")) {
data("Machines", package = "MEMSS") 

# simple model with random-slopes for repeated-measures factor
m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines)
m1

# suppress correlations among random effect parameters with || and expand_re = TRUE
m2 <- mixed(score ~ Machine + (Machine||Worker), data=Machines, expand_re = TRUE)
m2

## compare:
summary(m1)$varcor
summary(m2)$varcor
# for wrong solution see: 
# summary(lmer(score ~ Machine + (Machine||Worker), data=Machines))$varcor

if (requireNamespace("emmeans")) {
# follow-up tests
library("emmeans")  # package emmeans needs to be attached for follow-up tests.
(emm1 <- emmeans(m1, "Machine"))
pairs(emm1, adjust = "holm") # all pairwise comparisons
con1 <- list(
  c1 = c(1, -0.5, -0.5), # 1 versus other 2
  c2 = c(0.5, -1, 0.5) # 1 and 3 versus  2
)
contrast(emm1, con1, adjust = "holm")

if (requireNamespace("ggplot2")) {
# plotting 
afex_plot(m1, "Machine") ## default uses model-based CIs
## within-subjects CIs somewhat more in line with pairwirse comparisons:
afex_plot(m1, "Machine", error = "within") 

## less differences between CIs for model without correlations:
afex_plot(m2, "Machine")
afex_plot(m2, "Machine", error = "within")
}}}

\dontrun{
#######################
### Further Options ###
#######################

## Multicore:

require(parallel)
(nc <- detectCores()) # number of cores
cl <- makeCluster(rep("localhost", nc)) # make cluster
# to keep track of what the function is doindg redirect output to outfile:
# cl <- makeCluster(rep("localhost", nc), outfile = "cl.log.txt")

data("Machines", package = "MEMSS") 
## There are two ways to use multicore:

# 1. Obtain fits with multicore (e.g. for likelihood ratio tests, LRT):
mixed(score ~ Machine + (Machine|Worker), data=Machines, cl = cl, 
      method = "LRT")

# 2. Obtain PB samples via multicore: 
mixed(score ~ Machine + (Machine|Worker), data=Machines,
 method = "PB", args_test = list(nsim = 50, cl = cl)) # better use 500 or 1000 

## Both ways can be combined:
# 2. Obtain PB samples via multicore: 
mixed(score ~ Machine + (Machine|Worker), data=Machines, cl = cl,
 method = "PB", args_test = list(nsim = 50, cl = cl))

#### use all_fit = TRUE and expand_re = TRUE:
data("sk2011.2") # data described in more detail below
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

require(optimx) # uses two more algorithms
sk2_aff_b <- mixed(response ~ instruction*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, method = "LRT")
attr(sk2_aff_b, "all_fit_selected")
attr(sk2_aff_b, "all_fit_logLik")

# considerably faster with multicore:
clusterEvalQ(cl, library(optimx)) # need to load optimx in cluster
sk2_aff_b2 <- mixed(response ~ instruction*type+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, cl=cl, method = "LRT")
attr(sk2_aff_b2, "all_fit_selected")
attr(sk2_aff_b2, "all_fit_logLik")


stopCluster(cl)

}

###################################################
## Replicating Maxwell & Delaney (2004) Examples ##
###################################################
\dontrun{

### replicate results from Table 15.4 (Maxwell & Delaney, 2004, p. 789)
data(md_15.1)
# random intercept plus random slope
(t15.4a <- mixed(iq ~ timecat + (1+time|id),data=md_15.1))

# to also replicate exact parameters use treatment.contrasts and the last level as base level:
contrasts(md_15.1$timecat) <- contr.treatment(4, base = 4)
(t15.4b <- mixed(iq ~ timecat + (1+time|id),data=md_15.1, check_contrasts=FALSE))
summary(t15.4a)  # gives "wrong" parameters extimates
summary(t15.4b)  # identical parameters estimates

# for more examples from chapter 15 see ?md_15.1

### replicate results from Table 16.3 (Maxwell & Delaney, 2004, p. 837)
data(md_16.1)

# original results need treatment contrasts:
(mixed1_orig <- mixed(severity ~ sex + (1|id), md_16.1, check_contrasts=FALSE))
summary(mixed1_orig$full_model)

# p-value stays the same with afex default contrasts (contr.sum),
# but estimates and t-values for the fixed effects parameters change.
(mixed1 <- mixed(severity ~ sex + (1|id), md_16.1))
summary(mixed1$full_model)


# data for next examples (Maxwell & Delaney, Table 16.4)
data(md_16.4)
str(md_16.4)

### replicate results from Table 16.6 (Maxwell & Delaney, 2004, p. 845)
# Note that (1|room:cond) is needed because room is nested within cond.
# p-value (almost) holds.
(mixed2 <- mixed(induct ~ cond + (1|room:cond), md_16.4))
# (differences are dut to the use of Kenward-Roger approximation here,
# whereas M&W's p-values are based on uncorrected df.)

# again, to obtain identical parameter and t-values, use treatment contrasts:
summary(mixed2) # not identical

# prepare new data.frame with contrasts:
md_16.4b <- within(md_16.4, cond <- C(cond, contr.treatment, base = 2))
str(md_16.4b)

# p-value stays identical:
(mixed2_orig <- mixed(induct ~ cond + (1|room:cond), md_16.4b, 
                      check_contrasts=FALSE))
summary(mixed2_orig$full_model) # replicates parameters


### replicate results from Table 16.7 (Maxwell & Delaney, 2004, p. 851)
# F-values (almost) hold, p-values (especially for skill) are off
(mixed3 <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4))

# however, parameters are perfectly recovered when using the original contrasts:
mixed3_orig <- mixed(induct ~ cond + skill + (1|room:cond), md_16.4b, 
                     check_contrasts=FALSE)
summary(mixed3_orig)


### replicate results from Table 16.10 (Maxwell & Delaney, 2004, p. 862)
# for this we need to center cog:
md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)

# F-values and p-values are relatively off:
(mixed4 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b))
# contrast has a relatively important influence on cog
(mixed4_orig <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, 
                      check_contrasts=FALSE))

# parameters are again almost perfectly recovered:
summary(mixed4_orig)
}

###########################
## Full Analysis Example ##
###########################

\dontrun{
### split-plot experiment (Singmann & Klauer, 2011, Exp. 2)
## between-factor: instruction
## within-factor: inference & type
## hypothesis: three-way interaction
data("sk2011.2")

# use only affirmation problems (S&K also splitted the data like this)
sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])

# set up model with maximal by-participant random slopes 
sk_m1 <- mixed(response ~ instruction*inference*type+(inference*type|id), sk2_aff)

sk_m1 # prints ANOVA table with nicely rounded numbers (i.e., as characters)
nice(sk_m1)  # returns the same but without printing potential warnings
anova(sk_m1) # returns and prints numeric ANOVA table (i.e., not-rounded)
summary(sk_m1) # lmer summary of full model

# same model but using Kenward-Roger approximation of df
# very similar results but slower
sk_m1b <- mixed(response ~ instruction*inference*type+(inference*type|id), 
                sk2_aff, method="KR")
nice(sk_m1b)
# identical results as:
anova(sk_m1$full_model)

# suppressing correlation among random slopes: very similar results, but
# significantly faster and often less convergence warnings.
sk_m2 <- mixed(response ~ instruction*inference*type+(inference*type||id), sk2_aff,
               expand_re = TRUE)
sk_m2

## mixed objects can be passed to emmeans
library("emmeans")  # however, package emmeans needs to be attached first

# emmeans also approximate df which takes time with default Kenward-Roger
emm_options(lmer.df = "Kenward-Roger") # default setting, slow
emm_options(lmer.df = "Satterthwaite") # faster setting, preferrable
emm_options(lmer.df = "asymptotic") # the fastest, df = infinity


# recreates basically Figure 4 (S&K, 2011, upper panel)
# only the 4th and 6th x-axis position are flipped
afex_plot(sk_m1, x = c("type", "inference"), trace = "instruction")

# set up reference grid for custom contrasts:
(rg1 <- emmeans(sk_m1, c("instruction", "type", "inference")))

# set up contrasts on reference grid:
contr_sk2 <- list(
  ded_validity_effect = c(rep(0, 4), 1, rep(0, 5), -1, 0),
  ind_validity_effect = c(rep(0, 5), 1, rep(0, 5), -1),
  counter_MP = c(rep(0, 4), 1, -1, rep(0, 6)),
  counter_AC = c(rep(0, 10), 1, -1)
)

# test the main double dissociation (see S&K, p. 268)
contrast(rg1, contr_sk2, adjust = "holm")
# all effects are significant.
}

####################
## Other Examples ##
####################

\dontrun{

# use the obk.long data (not reasonable, no random slopes)
data(obk.long)
mixed(value ~ treatment * phase + (1|id), obk.long)

# Examples for using the per.parameter argument 
# note, require method = "nested-KR", "LRT", or "PB" 
# also we use custom contrasts
data(obk.long, package = "afex")
obk.long$hour <- ordered(obk.long$hour)
contrasts(obk.long$phase) <- "contr.sum"
contrasts(obk.long$treatment) <- "contr.sum" 

# tests only the main effect parameters of hour individually per parameter.
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = "^hour$", 
      data = obk.long, method = "nested-KR", check_contrasts = FALSE)

# tests all parameters including hour individually
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = "hour", 
      data = obk.long, method = "nested-KR", check_contrasts = FALSE)

# tests all parameters individually
mixed(value ~ treatment*phase*hour +(1|id), per_parameter = ".", 
      data = obk.long, method = "nested-KR", check_contrasts = FALSE)

# example data from package languageR: Lexical decision latencies elicited from
# 21 subjects for 79 English concrete nouns, with variables linked to subject or
# word.
data(lexdec, package = "languageR")

# using the simplest model
m1 <- mixed(RT ~ Correct + Trial + PrevType * meanWeight + 
    Frequency + NativeLanguage * Length + (1|Subject) + (1|Word), data = lexdec)
m1
# Mixed Model Anova Table (Type 3 tests, S-method)
# 
# Model: RT ~ Correct + Trial + PrevType * meanWeight + Frequency + NativeLanguage * 
# Model:     Length + (1 | Subject) + (1 | Word)
# Data: lexdec
#                  Effect         df         F p.value
# 1               Correct 1, 1627.67   8.16 **    .004
# 2                 Trial 1, 1591.92   7.58 **    .006
# 3              PrevType 1, 1605.05      0.17    .680
# 4            meanWeight   1, 74.37 14.85 ***   <.001
# 5             Frequency   1, 75.06 56.54 ***   <.001
# 6        NativeLanguage   1, 27.12      0.70    .412
# 7                Length   1, 74.80   8.70 **    .004
# 8   PrevType:meanWeight 1, 1600.79    6.19 *    .013
# 9 NativeLanguage:Length 1, 1554.49 14.24 ***   <.001
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

# Fitting a GLMM using parametric bootstrap:
require("mlmRev") # for the data, see ?Contraception

gm1 <- mixed(use ~ age + I(age^2) + urban + livch + (1 | district), method = "PB",
 family = binomial, data = Contraception, args_test = list(nsim = 10))
## note that nsim = 10 is way too low for all real examples!

}

\dontrun{
#####################################
## Interplay with effects packages ##
#####################################

data("Machines", package = "MEMSS") 
# simple model with random-slopes for repeated-measures factor
m1 <- mixed(score ~ Machine + (Machine|Worker), data=Machines, 
            set_data_arg = TRUE) ## necessary for it to work!
  
library("effects")

Effect("Machine", m1$full_model) # not correct:
#  Machine effect
# Machine
#        A        B        C 
# 59.65000 52.35556 60.32222 

# compare:
emmeans::emmeans(m1, "Machine")
 # Machine   emmean       SE  df asymp.LCL asymp.UCL
 # A       52.35556 1.680711 Inf  49.06142  55.64969
 # B       60.32222 3.528546 Inf  53.40640  67.23804
 # C       66.27222 1.806273 Inf  62.73199  69.81245

## necessary to set contr.sum globally:
set_sum_contrasts()
Effect("Machine", m1$full_model)
#  Machine effect
# Machine
#        A        B        C 
# 52.35556 60.32222 66.27222 

plot(Effect("Machine", m1$full_model))
}
}
\references{
Baayen, R. H. (2008). \emph{Analyzing linguistic data: a
  practical introduction to statistics using R}. Cambridge, UK; New York:
  Cambridge University Press.

  Baayen, R. H., Davidson, D. J., & Bates, D. M. (2008). Mixed-effects
  modeling with crossed random effects for subjects and items. \emph{Journal
  of Memory and Language}, 59(4), 390-412. \doi{10.1016/j.jml.2007.12.005}

  Baayen, R. H., & Milin, P. (2010). Analyzing Reaction Times.
  \emph{International Journal of Psychological Research}, 3(2), 12-28.

  Barr, D. J. (2013). Random effects structure for testing interactions in
  linear mixed-effects models. \emph{Frontiers in Quantitative Psychology and
  Measurement}, 328. \doi{10.3389/fpsyg.2013.00328}

  Barr, D. J., Levy, R., Scheepers, C., & Tily, H. J. (2013). Random effects
  structure for confirmatory hypothesis testing: Keep it maximal.
  \emph{Journal of Memory and Language}, 68(3), 255-278.
  \doi{10.1016/j.jml.2012.11.001}

  Bates, D., Kliegl, R., Vasishth, S., & Baayen, H. (2015).
  \emph{Parsimonious Mixed Models}. arXiv:1506.04967 [stat]. Retrieved from
  \url{https://arxiv.org/abs/1506.04967}

  Dalal, D. K., & Zickar, M. J. (2012). Some Common Myths About Centering
  Predictor Variables in Moderated Multiple Regression and Polynomial
  Regression. \emph{Organizational Research Methods}, 15(3), 339-362.
  \doi{10.1177/1094428111430540}

  Judd, C. M., Westfall, J., & Kenny, D. A. (2012). Treating stimuli as a
  random factor in social psychology: A new and comprehensive solution to a
  pervasive but largely ignored problem. \emph{Journal of Personality and
  Social Psychology}, 103(1), 54-69. \doi{10.1037/a0028347}

  Luke, S. (2017). Evaluating significance in linear mixed-effects models in
  R. \emph{Behavior Research Methods}.
  \doi{10.3758/s13428-016-0809-y}

  Maxwell, S. E., & Delaney, H. D. (2004). \emph{Designing experiments and
  analyzing data: a model-comparisons perspective.} Mahwah, N.J.: Lawrence
  Erlbaum Associates.
}
\seealso{
\code{\link{aov_ez}} and \code{\link{aov_car}} for convenience
  functions to analyze experimental desIgns with classical ANOVA or ANCOVA
  wrapping \code{\link[car]{Anova}}.

  see the following for the data sets from Maxwell and Delaney (2004) used
  and more examples: \code{\link{md_15.1}}, \code{\link{md_16.1}}, and
  \code{\link{md_16.4}}.
}
\author{
Henrik Singmann with contributions from
  \href{https://stackoverflow.com/q/11335923/289572}{Ben Bolker and Joshua
  Wiley}.
}
