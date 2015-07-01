afex: Analysis of Factorial EXperiments
====

The two main functionalities of `afex` are (1) to provide a coherent and intuitive interface to run standard ANOVAs with any number of within- or between-subjects variables (the relevant functions are now called `aov_car`, `aov_ez`, and `aov_4`) and (2) to provide *p*-values for fixed effects in mixed models using `lme4` via function `mixed`. The default outputs of those functions can be directly passed to `lsmeans` for post-hoc tests or contrasts. 


## Installation

- `afex` is available from CRAN so the current stable version can be installed directly via: 
  `install.packages("afex")`

- To install the latest development version you will need the `devtools` package: 
  `devtools::install_github("singmann/afex@master")`


## What is New

As of version `0.14` several changes to the interface were introduced:

- ANOVA functions renamed to `aov_car`, `aov_ez`, and `aov_4`. Old ANOVA functions are now deprecated.

- new default return argument for ANOVA functions `afex_aov`, an S3 object containing the following:
  1. ANOVA table of class `"anova"`
  2. ANOVA fitted with base R's `aov` (can be passed to `lsmeans` for post-hoc tests)
  3. output from `car::Anova` (for tests of effects), ANOVA table 1. is based on this model
  4. `lm` object passed to `car::Anova`
  5. data used for estimating 2. and 4.
        
-  added support for `lsmeans`: objects of class `afex_aov` can be passed to `lsmeans` directly. `afex` now depends on `lsmeans`.

- `nice.anova` was renamed to `nice` (and now also works with `mixed` objects).

- Returned objects of `mixed` and the ANOVA functions (i.e., of class `afex_aov`) are similar:
  - Both have a numeric Anova table as first element called `anova_table` (which is of class `c("anova", "data.frame")`).
  - calling `nice` on either returns a nicely rounded Anova table (i.e., numbers converted to characters). This table is also per default printed.
  - calling `anova` on either will return the numeric Anova table (for which print methods exist as well).
    
- added `afex_options()` functionality for setting options globally.
    
- added vignette showing how to calculate contrasts after ANOVA.
    
- `afex` does not depend on `car` package anymore, it is only imported.

- first element in mixed object renamed to `anova_table`.
    
- `summary` method for `mixed` objects now calls `summary.merMod` on full model.
