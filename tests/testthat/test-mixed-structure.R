
context("Mixed: structural tests")

# note: all calls with type 2 are wrapped in suppressWarnings()!

test_that("mixed: Maxell & Delaney (2004), Table 16.4, p. 842: Type 2", {
  data(md_16.4)
  md_16.4b <- md_16.4
  md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)
  contrasts(md_16.4b$cond) <- "contr.sum"
  mixed4_2 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, type = 2, 
                    progress=FALSE, method = "nested-KR")
  lmer4_full <- lmer(induct ~ cond*cog + (cog|room:cond), md_16.4b)
  lmer4_small <- lmer(induct ~ cond+cog + (cog|room:cond), md_16.4b)
  expect_that(fixef(mixed4_2$full_model[[2]]), equals(fixef(lmer4_full)))
  expect_that(fixef(mixed4_2$full_model[[1]]), is_equivalent_to(fixef(lmer4_small)))  
})

test_that("mixed: Maxell & Delaney (2004), Table 16.4, p. 842: Type 3", {
  data(md_16.4)
  md_16.4b <- md_16.4
  md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)
  contrasts(md_16.4b$cond) <- "contr.sum"
  mixed4_2 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, type = 3, 
                    progress=FALSE, method = "nested-KR")
  lmer4_full <- lmer(induct ~ cond*cog + (cog|room:cond), md_16.4b)
  lmer4_small <- lmer(induct ~ cond+cog + (cog|room:cond), md_16.4b)
  expect_that(fixef(mixed4_2$full_model), equals(fixef(lmer4_full)))
  expect_that(mixed4_2$full_model, is_equivalent_to(lmer4_full))
  expect_that(fixef(mixed4_2$restricted_models$`cond:cog`), 
              is_equivalent_to(fixef(lmer4_small)))  
})

test_that("mixed, obk.long: type 2 and LRTs", {
  data(obk.long, package = "afex")
  contrasts(obk.long$treatment) <- "contr.sum"
  contrasts(obk.long$phase) <- "contr.sum"
  t2 <- mixed(value ~ treatment*phase +(1|id), data = obk.long, method = "LRT", 
              type = 2, progress=FALSE)
  expect_output(print(t2), "treatment")
  a2.f <- lmer(value ~ treatment*phase +(1|id), data = obk.long, REML=FALSE)
  a2.h <- lmer(value ~ treatment+phase +(1|id), data = obk.long, REML=FALSE)
  a2.t <- lmer(value ~ treatment +(1|id), data = obk.long, REML=FALSE)
  a2.p <- lmer(value ~ phase +(1|id), data = obk.long, REML=FALSE)
  if (packageVersion("lme4") <= "1.1.21") {
    extract_anova <- function(anova) unlist(anova)[c("Df1", "Chisq2", "Chi Df2", "Pr(>Chisq)2" )]
  } else {
    extract_anova <- function(anova) unlist(anova)[c("npar1", "Chisq2", "Df2", "Pr(>Chisq)2" )]
  }
  
  expect_that(
    unlist(t2$anova_table[3,])
    , is_equivalent_to(
      extract_anova(anova(a2.h, a2.f))
    ))
  expect_that(
    unlist(t2$anova_table[2,])
    , is_equivalent_to(
      extract_anova(anova(a2.t, a2.h))
    ))
  expect_that(
    unlist(t2$anova_table[1,])
    , is_equivalent_to(
      extract_anova(anova(a2.p, a2.h))
    ))
})

test_that("mixed, mlmRev: type 3 and 2 LRTs for GLMMs", {
  skip_if_not_installed("mlmRev")
  if (require("mlmRev")) {
    suppressWarnings(gm1 <- mixed(use ~ age*urban + (1 | district), 
                                  family = binomial, data = Contraception, 
                                  method = "LRT", progress=FALSE))
    suppressWarnings(gm2 <- mixed(use ~ age*urban + (1 | district), 
                                  family = binomial, data = Contraception, 
                                  method = "LRT", type = 2, progress=FALSE))
    expect_that(gm1, is_a("mixed"))
    expect_that(gm1, is_a("mixed"))  
  }
})

test_that("mixed, obk.long: LMM with method = PB", {
  expect_that(mixed(value ~ treatment+phase*hour +(1|id), data = obk.long, 
                    method = "PB", args_test = list(nsim = 10), progress=FALSE),
              is_a("mixed"))
})

test_that("mixed, obk.long: multicore loads lme4 and produces the same results", {
  #if (packageVersion("testthat") >= "0.9") {
  if (FALSE) {  # that never seems to run...
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    data(obk.long, package = "afex")
    require(parallel)
    cl <- makeCluster(rep("localhost", 2)) # make cluster
    # 1. Obtain fits with multicore:
    m_mc1 <- mixed(value ~ treatment +(phase|id), data = obk.long, 
                   method = "LRT", cl = cl, control = 
                     lmerControl(optCtrl=list(maxfun = 100000)), progress=FALSE)
    cl_search <- clusterEvalQ(cl, search())
    stopCluster(cl)  
    m_mc2 <- mixed(value ~ treatment +(phase|id), data = obk.long, 
                   method = "LRT", control = 
                     lmerControl(optCtrl=list(maxfun = 100000)), progress=FALSE)
    expect_that(all(vapply(cl_search, function(x) any(grepl("^package:lme4$", x)), NA)), is_true())
    expect_that(m_mc1, equals(m_mc2, check.attributes = FALSE))
  }
})

test_that("print(mixed) works: only 1 or 2 fixed effects with all methods", {
  data(obk.long, package = "afex")
  expect_that(print(mixed(value ~ treatment+(1|id), data = obk.long, 
                          progress=FALSE)), is_a("data.frame"))
  expect_that(print(mixed(value ~ treatment+phase+(1|id), data = obk.long, 
                          progress=FALSE)), is_a("data.frame"))
  expect_that(print(mixed(value ~ treatment+(1|id), data = obk.long, 
                          method = "LRT", progress=FALSE)), is_a("data.frame"))
  expect_that(print(mixed(value ~ treatment+phase+(1|id), data = obk.long, 
                          method = "LRT", progress=FALSE)), is_a("data.frame"))
  skip_if_not_installed("mlmRev")
  require("mlmRev") # for the data, see ?Contraception
  expect_that(print(mixed(use ~ urban + (1 | district), method = "PB",
                          family = binomial, data = Contraception, 
                          args_test=list(nsim=2), progress=FALSE)), 
              is_a("data.frame"))
  expect_that(print(mixed(use ~ urban + livch + (1 | district), method = "PB", 
                          family = binomial, data = Contraception, 
                          args_test=list(nsim=2), progress=FALSE)), 
              is_a("data.frame"))  
})

# test_that("mixed, Maxell & Delaney (2004), Table 16.4, p. 842: bobyqa not fitting well", {
#   data(md_16.4)
#   # F-values and p-values are relatively off:
#   expect_that(mixed(induct ~ cond*cog + (cog|room:cond), md_16.4, control=lmerControl(optimizer="bobyqa")), gives_warning("better fit"))
#   expect_that(mixed(induct ~ cond*cog + (cog|room:cond), md_16.4, type=2, control=lmerControl(optimizer="bobyqa")), gives_warning("better fit"))
# })

test_that("mixed: set.data.arg", {
  data(obk.long, package = "afex")
  suppressWarnings(m1 <- mixed(value ~ treatment*phase +(1|id), obk.long, 
                               method = "LRT", progress=FALSE, 
                               set_data_arg = TRUE))
  suppressWarnings(m2 <- mixed(value ~ treatment*phase +(1|id), obk.long, 
                               method = "LRT", progress=FALSE, 
                               set_data_arg = FALSE))
  expect_that(m1$full_model@call[["data"]], 
              is_identical_to(as.name("obk.long")))
  expect_that(m2$full_model@call[["data"]], is_identical_to(as.name("data")))
})

test_that("mixed: anova with multiple mixed objexts", {
  data("sk2011.2")
  data("ks2013.3")
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk_m1 <- mixed(response ~ instruction+(1|id), sk2_aff, method = "LRT", 
                 progress = FALSE, set_data_arg = TRUE)
  sk_m2 <- mixed(response ~ instruction+(1|id)+(1|content), sk2_aff, 
                 method = "LRT", progress = FALSE, set_data_arg = TRUE)
  sk_m3 <- lmer(response ~ instruction+(1|id)+(validity|content), sk2_aff, 
                REML = FALSE)
  sk_m4 <- lmer(response ~ instruction+(1|id)+(validity|content), sk2_aff, 
                REML = TRUE)
  t <- anova(sk_m1, sk_m2, sk_m3)
  xx <- anova(sk_m1$full_model, sk_m2$full_model, sk_m3, 
              model.names = c("sk_m1", "sk_m2", "sk_m3"))
  expect_identical(rownames(xx), rownames(t))
  expect_identical(rownames(xx), c("sk_m1", "sk_m2", "sk_m3"))
  expect_is(t, c("anova", "data.frame"))
  expect_is(anova(sk_m1, object = sk_m2, sk_m3), c("anova", "data.frame"))
  expect_is(anova(sk_m1, object = sk_m2, sk_m3, ks2013.3), c("anova", "data.frame"))
  expect_warning(anova(sk_m1, object = sk_m2, sk_m3, sk_m4), 
                 "some models fit with REML = TRUE, some not")
})

context("Mixed: Expand random effects")


test_that("mixed: expand_re argument, return = 'merMod'", {
  data("ks2013.3")
  set_default_contrasts()
  m2 <- mixed(response ~ validity + (believability||id), ks2013.3, 
              expand_re = TRUE, method = "LRT", progress=FALSE)
  m3 <- mixed(response ~ validity + (believability|id), ks2013.3, 
              method = "LRT", progress=FALSE)
  expect_identical(length(unlist(summary(m2)$varcor)), 
                   nrow(summary(m3)$varcor$id))
  expect_true(all.equal(unlist(summary(m2)$varcor), diag(summary(m3)$varcor$id),
                        tolerance = 0.03, check.attributes = FALSE))
  l2 <- mixed(response ~ validity + (believability||id), ks2013.3, 
              expand_re = TRUE, return = "merMod", progress=FALSE)
  expect_is(l2, "merMod")
  expect_equivalent(m2$full_model, l2)
  l3 <- lmer_alt(response ~ validity + (believability||id), ks2013.3)
  l4 <- lmer_alt(response ~ validity + (believability||id), ks2013.3, 
                 control = lmerControl(optimizer = "Nelder_Mead"))
  expect_equivalent(l2, l3) 
  expect_equal(l3, l4, check.attributes = FALSE)
  l5 <- lmer_alt(response ~ validity + (believability||id), ks2013.3, 
                 control = lmerControl(optimizer = "Nelder_Mead"), 
                 check_contrasts = TRUE)
  expect_equal(l2, l5, check.attributes = FALSE )
  # parameter names need to be identical (same contrasts):
  expect_identical(names(coef(l2)$id), names(coef(l5)$id))
  # parameter names need to be different (different contrasts):
  expect_false(all(names(coef(l2)$id) == names(coef(l3)$id)))  
  l7 <- lmer_alt(response ~ validity + (1|id) + (0+validity*condition||content), 
                 ks2013.3, control = lmerControl(optCtrl = list(maxfun=1e6)))
  expect_is(l7, "merMod")
  expect_error(lmer_alt(response ~ validity + (0|id) + 
                          (0+validity*condition||content), ks2013.3), 
               "Invalid random effects term")
  expect_is(lmer_alt(response ~ validity + (validity||id) + (validity|content), 
                     ks2013.3), "merMod")
})

test_that("mixed: expand_re argument (longer)", {
  if (packageVersion("testthat") >= "0.9") {
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    data("ks2013.3")
    m4 <- mixed(response ~ validity + (believability*validity||id) + 
                  (validity*condition|content), ks2013.3, expand_re = TRUE, 
                method = "LRT", control = 
                  lmerControl(optCtrl = list(maxfun=1e6)), progress=FALSE)
    m5 <- suppressWarnings(mixed(response ~ validity + 
                                   (believability*validity|id) + 
                                   (validity*condition||content), ks2013.3, 
                                 method = "LRT", control = 
                                   lmerControl(optCtrl = list(maxfun=1e6)), 
                                 expand_re = TRUE, progress=FALSE))
    expect_identical(length(unlist(summary(m4)$varcor[-7])), 
                     nrow(summary(m5)$varcor$id))
    expect_identical(length(unlist(summary(m5)$varcor[-1])), 
                     nrow(summary(m4)$varcor$content))
    expect_equal(attr(summary(m5)$varcor, "sc"), attr(summary(m4)$varcor, "sc"),
                 tolerance = 0.02)
  }
})


test_that("mixed: return=data, expand_re argument, and allFit", {
  #if (packageVersion("testthat") >= "0.9") {
  #testthat::skip_on_travis()
  testthat::skip_if_not_installed("optimx")
  testthat::skip_on_cran()
  skip_on_os("windows")
  require(optimx)
  data("ks2013.3")
  ks2013.3_tmp <- ks2013.3
  m6 <- mixed(response ~ validity + (believability*validity||id), ks2013.3_tmp, 
              expand_re = TRUE, method = "LRT", 
              control = lmerControl(optCtrl = list(maxfun=1e6)), progress=FALSE,
              return = "merMod")
  m6_all_1 <- all_fit(m6, verbose = FALSE, data = ks2013.3_tmp)
  expect_output(print(m6_all_1$`bobyqa.`), 
                "object 're1.believability1' not found")
  ks2013.3_tmp <- mixed(response ~ validity + (believability*validity||id), 
                        ks2013.3_tmp, expand_re = TRUE, method = "LRT", 
                        control = lmerControl(optCtrl = list(maxfun=1e6)), 
                        progress=FALSE, return = "data")
  m6_all_2 <- suppressWarnings(all_fit(m6, verbose = FALSE, data = ks2013.3_tmp))
  expect_is(m6_all_2$`bobyqa.`, "merMod")
  expect_is(m6_all_2$`Nelder_Mead.`, "merMod") 
  expect_is(m6_all_2$`nmkbw.`, "merMod")
  expect_is(m6_all_2$optimx.nlminb, "merMod")
  expect_is(m6_all_2$`optimx.L-BFGS-B`, "merMod")
  expect_is(m6_all_2$nloptwrap.NLOPT_LN_NELDERMEAD, "merMod")
})

test_that("mixed with all_fit = TRUE", {
  testthat::skip_if_not_installed("optimx")
  testthat::skip_if_not_installed("MEMSS")
  testthat::skip_if_not_installed("dfoptim")
  testthat::skip_on_cran()
  skip_on_os("windows")
  require(optimx)
  data("Machines", package = "MEMSS") 
  aop <- afex_options()
  afex_options(lmer_function = "lmerTest")
  m1 <- mixed(score ~ Machine + (Machine||Worker), data=Machines, 
              expand_re = TRUE, all_fit = TRUE, progress = FALSE)
  afex_options(lmer_function = "lme4")
  m2 <- mixed(score ~ Machine + (Machine||Worker), data=Machines, 
              expand_re = TRUE, all_fit = TRUE, method = "LRT", 
              progress = FALSE)
  afex_options(aop)
  
  all_loglik1 <- attr(m1, "all_fit_logLik")
  all_loglik2 <- attr(m2, "all_fit_logLik")
  expect_false(any(is.na(all_loglik1[,1])))
  expect_false(any(is.na(all_loglik2[,1])))
  expect_equivalent(rep(all_loglik1[1,1], nrow(all_loglik1)), 
                    all_loglik1[,1])
  expect_equivalent(rep(all_loglik2[1,1], nrow(all_loglik2)), 
                    all_loglik2[,1])
})


test_that("mixed: return=data works", {
  data("ks2013.3")
  ks2013.3_tmp <- ks2013.3
  ks2013.3_tmp <- mixed(response ~ validity + (believability*validity||id), 
                        ks2013.3_tmp, expand_re = TRUE, method = "LRT", 
                        control = lmerControl(optCtrl = list(maxfun=1e6)), 
                        progress=FALSE, return = "data")
  expect_is(ks2013.3_tmp, "data.frame")
  if (packageVersion("testthat") >= "0.11.0.9000") 
    expect_gt(ncol(ks2013.3_tmp), ncol(ks2013.3))
  expect_output(print(colnames(ks2013.3_tmp)), "re1.believability1_by_validity1")
})


test_that("mixed with all available methods", {
  data("sk2011.2") # see example("mixed")
  testthat::skip_on_travis()
  testthat::skip_on_cran()
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  for (i in c(2, 3)) {
    sk2_aff_kr <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
                        expand_re = TRUE, all_fit = FALSE, method = "KR", 
                        progress=FALSE, type = i)
    sk2_aff_s <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
                       expand_re = TRUE, all_fit = FALSE, method = "S", 
                       progress=FALSE, type = i)
    sk2_aff_nkr <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
                         progress = FALSE, type = i,
                         expand_re = TRUE, all_fit = FALSE, method = "nested-KR")
    sk2_aff_lrt <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
                         progress = FALSE, type = i,
                         expand_re = TRUE, all_fit = FALSE, method = "LRT")
    sk2_aff_pb <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
                        progress = FALSE, type = i, args_test = list(nsim = 10),
                        expand_re = TRUE, all_fit = FALSE, method = "PB")
    expect_is(sk2_aff_kr, "mixed")
    expect_is(sk2_aff_s, "mixed")
    expect_is(sk2_aff_nkr, "mixed")
    expect_is(sk2_aff_lrt, "mixed")
    expect_is(sk2_aff_pb, "mixed")
    expect_is(anova(sk2_aff_kr), "anova")
    expect_is(anova(sk2_aff_s), "anova")
    expect_is(anova(sk2_aff_nkr), "anova")
    expect_is(anova(sk2_aff_lrt), "anova")
    expect_is(anova(sk2_aff_pb), "anova")
    expect_output(print(sk2_aff_kr), "Effect")
    expect_output(print(sk2_aff_kr), "F")
    expect_output(print(sk2_aff_s), "Effect")
    expect_output(print(sk2_aff_s), "F")
    expect_output(print(sk2_aff_nkr), "Effect")
    expect_output(print(sk2_aff_nkr), "F")
    expect_output(print(sk2_aff_lrt), "Effect")
    expect_output(print(sk2_aff_lrt), "Chisq")
    expect_output(print(sk2_aff_pb), "Effect")
    expect_output(print(sk2_aff_pb), "Chisq")
  }
})


test_that("mixed all_fit = TRUE works with old methods", {
  data("sk2011.2") # see example("mixed")
  testthat::skip_on_cran()
  skip_on_os("windows")
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk2_aff_b <- mixed(response ~ instruction+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, method = "nested-KR", 
               progress = FALSE)
  sk2_aff_b2 <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
               type = 2, expand_re = TRUE, all_fit = TRUE, method = "nested-KR",
               progress = FALSE)
  expect_is(sk2_aff_b, "mixed")
  expect_length(attr(sk2_aff_b, "all_fit_selected"), 2)
  expect_length(attr(sk2_aff_b, "all_fit_logLik"), 2)
  expect_is(sk2_aff_b2, "mixed")
  expect_length(attr(sk2_aff_b2, "all_fit_selected"), 5)
  expect_length(attr(sk2_aff_b2, "all_fit_logLik"), 5)
})



test_that("mixed all_fit = TRUE works with new (KR) methods", {
  skip_on_os("windows")
  data("sk2011.2") # see example("mixed")
  testthat::skip_on_cran()
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk2_aff_b <- mixed(response ~ instruction+(inference*type||id), sk2_aff,
               expand_re = TRUE, all_fit = TRUE, method = "KR",
               progress = FALSE)
  sk2_aff_b2 <- mixed(response ~ instruction*type+(inference||id), sk2_aff,
               type = 2, expand_re = TRUE, all_fit = TRUE, method = "KR",
               progress = FALSE)
  expect_is(sk2_aff_b, "mixed")
  expect_named(attr(sk2_aff_b, "all_fit_selected"), "full_model")
  expect_false(is.null(attr(sk2_aff_b, "all_fit_logLik")))
  expect_is(sk2_aff_b2, "mixed")
  expect_named(attr(sk2_aff_b2, "all_fit_selected"), "full_model")
  expect_false(is.null(attr(sk2_aff_b2, "all_fit_logLik")))
})


test_that("anova_table attributes", {
  data(obk.long)
  symbol_test <- mixed(value ~ treatment * phase + (1|id), obk.long, 
                       sig_symbols = c("", "a", "aa", "aaa"), progress = FALSE, 
                       return = "nice")
  expect_output(print(symbol_test), "aaa")
  
  symbol_test <- mixed(value ~ treatment * phase + (1|id), obk.long, 
                       sig_symbols = c("", "a", "aa", "aaa"), progress = FALSE)
  expect_output(print(symbol_test), "aaa")
  expect_output(print(nice(symbol_test, sig_symbols = c("", "b", "bb", "bbb"))), 
                "bbb")
  
  new_symbols <- c(" ", " b", " bb", " bbb")
  symbol_test <- anova(symbol_test, sig_symbols = c(" ", " b", " bb", " bbb"))
  expect_identical(attr(symbol_test, "sig_symbols"), new_symbols)
  expect_output(print(nice(symbol_test)), "bbb")
  
  # Test support for old afex objects
  old_afex_object <- default_options <- mixed(value ~ treatment * phase + 
                                                (1|id), obk.long, 
                                              progress = FALSE)
  attr(old_afex_object$anova_table, "sig_symbols") <- NULL
  expect_that(nice(old_afex_object), is_identical_to(nice(default_options)))
})
