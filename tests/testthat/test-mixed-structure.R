
context("Mixed: structural tests")

# note: all calls with type 2 are wrapped in suppressWarnings()!

test_that("mixed: Maxell & Delaney (2004), Table 16.4, p. 842: Type 2", {
  data(md_16.4)
  md_16.4b <- md_16.4
  md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)
  contrasts(md_16.4b$cond) <- "contr.sum"
  suppressWarnings(mixed4_2 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, type = 2, progress=FALSE))
  lmer4_full <- lmer(induct ~ cond*cog + (cog|room:cond), md_16.4b)
  lmer4_small <- lmer(induct ~ cond+cog + (cog|room:cond), md_16.4b)
  expect_that(fixef(mixed4_2$full.model[[2]]), equals(fixef(lmer4_full)))
  expect_that(fixef(mixed4_2$full.model[[1]]), is_equivalent_to(fixef(lmer4_small)))  
})

test_that("mixed: Maxell & Delaney (2004), Table 16.4, p. 842: Type 3", {
  data(md_16.4)
  md_16.4b <- md_16.4
  md_16.4b$cog <- scale(md_16.4b$cog, scale=FALSE)
  contrasts(md_16.4b$cond) <- "contr.sum"
  suppressWarnings(mixed4_2 <- mixed(induct ~ cond*cog + (cog|room:cond), md_16.4b, type = 3, progress=FALSE))
  lmer4_full <- lmer(induct ~ cond*cog + (cog|room:cond), md_16.4b)
  lmer4_small <- lmer(induct ~ cond+cog + (cog|room:cond), md_16.4b)
  expect_that(fixef(mixed4_2$full.model), equals(fixef(lmer4_full)))
  expect_that(mixed4_2$full.model, is_equivalent_to(lmer4_full))
  expect_that(fixef(mixed4_2$restricted.models$`cond:cog`), is_equivalent_to(fixef(lmer4_small)))  
})

test_that("mixed, obk.long: type 2 and LRTs", {
  data(obk.long, package = "afex")
  contrasts(obk.long$treatment) <- "contr.sum"
  contrasts(obk.long$phase) <- "contr.sum"
  suppressWarnings(t2 <- mixed(value ~ treatment*phase +(1|id), data = obk.long, method = "LRT", type = 2, progress=FALSE))
  a2.f <- lmer(value ~ treatment*phase +(1|id), data = obk.long, REML=FALSE)
  a2.h <- lmer(value ~ treatment+phase +(1|id), data = obk.long, REML=FALSE)
  a2.t <- lmer(value ~ treatment +(1|id), data = obk.long, REML=FALSE)
  a2.p <- lmer(value ~ phase +(1|id), data = obk.long, REML=FALSE)
  extract_anova <- function(anova) unlist(anova)[c("Df1", "Chisq2", "Chi Df2", "Pr(>Chisq)2" )]
  
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
  require("mlmRev")
  suppressWarnings(gm1 <- mixed(use ~ age*urban + (1 | district), family = binomial, data = Contraception, method = "LRT", progress=FALSE))
  suppressWarnings(gm2 <- mixed(use ~ age*urban + (1 | district), family = binomial, data = Contraception, method = "LRT", type = 2, progress=FALSE))
  expect_that(gm1, is_a("mixed"))
  expect_that(gm1, is_a("mixed"))
})

test_that("mixed, obk.long: LMM with method = PB", {
  expect_that(mixed(value ~ treatment+phase*hour +(1|id), data = obk.long, method = "PB", args.test = list(nsim = 10), progress=FALSE), is_a("mixed"))
})

test_that("mixed, obk.long: multicore loads lme4 and produces the same results", {
  skip_on_cran()
  data(obk.long, package = "afex")
  require(parallel)
  cl <- makeCluster(rep("localhost", 2)) # make cluster
  # 1. Obtain fits with multicore:
  m_mc1 <- mixed(value ~ treatment +(phase|id), data = obk.long, method = "LRT", cl = cl, control = lmerControl(optCtrl=list(maxfun = 100000)), progress=FALSE)
  cl_search <- clusterEvalQ(cl, search())
  stopCluster(cl)  
  m_mc2 <- mixed(value ~ treatment +(phase|id), data = obk.long, method = "LRT", control = lmerControl(optCtrl=list(maxfun = 100000)), progress=FALSE)
  expect_that(all(vapply(cl_search, function(x) any(grepl("^package:lme4$", x)), NA)), is_true())
  expect_that(m_mc1, equals(m_mc2, check.attributes = FALSE))
})

test_that("print(mixed) works: only 1 or 2 fixed effects with all methods", {
  data(obk.long, package = "afex")
  expect_that(print(mixed(value ~ treatment+(1|id), data = obk.long)), is_a("data.frame"))
  expect_that(print(mixed(value ~ treatment+phase+(1|id), data = obk.long)), is_a("data.frame"))
  expect_that(print(mixed(value ~ treatment+(1|id), data = obk.long, method = "LRT")), is_a("data.frame"))
  expect_that(print(mixed(value ~ treatment+phase+(1|id), data = obk.long, method = "LRT")), is_a("data.frame"))
  require("mlmRev") # for the data, see ?Contraception
  expect_that(print(mixed(use ~ urban + (1 | district), method = "PB", family = binomial, data = Contraception, args.test=list(nsim=2))), is_a("data.frame"))
  expect_that(print(mixed(use ~ urban + livch + (1 | district), method = "PB", family = binomial, data = Contraception, args.test=list(nsim=2))), is_a("data.frame"))  
})

# test_that("mixed, Maxell & Delaney (2004), Table 16.4, p. 842: bobyqa not fitting well", {
#   data(md_16.4)
#   # F-values and p-values are relatively off:
#   expect_that(mixed(induct ~ cond*cog + (cog|room:cond), md_16.4, control=lmerControl(optimizer="bobyqa")), gives_warning("better fit"))
#   expect_that(mixed(induct ~ cond*cog + (cog|room:cond), md_16.4, type=2, control=lmerControl(optimizer="bobyqa")), gives_warning("better fit"))
# })

test_that("mixed: set.data.arg", {
  data(obk.long, package = "afex")
  suppressWarnings(m1 <- mixed(value ~ treatment*phase +(1|id), obk.long, method = "LRT", progress=FALSE))
  suppressWarnings(m2 <- mixed(value ~ treatment*phase +(1|id), obk.long, method = "LRT", progress=FALSE, set.data.arg = FALSE))
  expect_that(m1$full.model@call[["data"]], is_identical_to(as.name("obk.long")))
  expect_that(m2$full.model@call[["data"]], is_identical_to(as.name("data")))
})

test_that("mixed: anova with multiple mixed objexts", {
  data("sk2011.2")
  data("ks2013.3")
  sk2_aff <- droplevels(sk2011.2[sk2011.2$what == "affirmation",])
  sk_m1 <- mixed(response ~ instruction+(1|id), sk2_aff, method = "LRT", progress = FALSE)
  sk_m2 <- mixed(response ~ instruction+(1|id)+(1|content), sk2_aff, method = "LRT", progress = FALSE)
  sk_m3 <- lmer(response ~ instruction+(1|id)+(validity|content), sk2_aff, REML = FALSE)
  sk_m4 <- lmer(response ~ instruction+(1|id)+(validity|content), sk2_aff, REML = TRUE)
  t <- anova(sk_m1, sk_m2, sk_m3)
  expect_is(t, c("anova", "data.frame"))
  expect_is(anova(sk_m1, object = sk_m2, sk_m3), c("anova", "data.frame"))
  expect_is(anova(sk_m1, object = sk_m2, sk_m3, ks2013.3), c("anova", "data.frame"))
  expect_warning(anova(sk_m1, object = sk_m2, sk_m3, sk_m4), "some models fit with REML = TRUE, some not")
})

context("Mixed: Expand random effects")


test_that("mixed: expand_re argument, return = 'merMod'", {
  data("ks2013.3")
  m2 <- mixed(response ~ validity + (believability||id), ks2013.3, expand_re = TRUE, method = "LRT", progress=FALSE)
  m3 <- mixed(response ~ validity + (believability|id), ks2013.3, method = "LRT", progress=FALSE)
  expect_identical(length(unlist(summary(m2)$varcor)), nrow(summary(m3)$varcor$id))
  expect_true(all.equal(unlist(summary(m2)$varcor), diag(summary(m3)$varcor$id), tolerance = 0.03, check.attributes = FALSE))
  l2 <- mixed(response ~ validity + (believability||id), ks2013.3, expand_re = TRUE, return = "merMod")
  expect_is(l2, "merMod")
  expect_equivalent(m2$full.model, l2)
  l3 <- lmer_alt(response ~ validity + (believability||id), ks2013.3)
  l4 <- lmer_alt(response ~ validity + (believability||id), ks2013.3, control = lmerControl(optimizer = "Nelder_Mead"))
  expect_equivalent(l2, l3) 
  expect_equal(l3, l4, check.attributes = FALSE)
  l5 <- lmer_alt(response ~ validity + (believability||id), ks2013.3, control = lmerControl(optimizer = "Nelder_Mead"), check.contrasts = TRUE)
  expect_equal(l2, l5, check.attributes = FALSE )
  expect_identical(names(coef(l2)$id), names(coef(l5)$id))  # parameter names need to be identical (same contrasts)
  expect_false(all(names(coef(l2)$id) == names(coef(l3)$id)))  # parameter names need to be different (different contrasts)
  l7 <- lmer_alt(response ~ validity + (1|id) + (0+validity*condition||content), ks2013.3, control = lmerControl(optCtrl = list(maxfun=1e6)))
  expect_is(l7, "merMod")
  expect_error(lmer_alt(response ~ validity + (0|id) + (0+validity*condition||content), ks2013.3), "Invalid random effects term")
  expect_is(lmer_alt(response ~ validity + (validity||id) + (validity|content), ks2013.3), "merMod")
})

test_that("mixed: expand_re argument (longer)", {
  testthat::skip_on_cran()
  data("ks2013.3")
  m4 <- mixed(response ~ validity + (believability*validity||id) + (validity*condition|content), ks2013.3, expand_re = TRUE, method = "LRT", control = lmerControl(optCtrl = list(maxfun=1e6)), progress=FALSE)
  m5 <- mixed(response ~ validity + (believability*validity|id) + (validity*condition||content), ks2013.3, method = "LRT", control = lmerControl(optCtrl = list(maxfun=1e6)), expand_re = TRUE, progress=FALSE)
  expect_identical(length(unlist(summary(m4)$varcor[-7])), nrow(summary(m5)$varcor$id))
  expect_identical(length(unlist(summary(m5)$varcor[-1])), nrow(summary(m4)$varcor$content))
  expect_equal(attr(summary(m5)$varcor, "sc"), attr(summary(m4)$varcor, "sc"), tolerance = 0.02)
})


