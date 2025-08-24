context("ANOVAs: replicating published results")

test_that("purely within ANOVA, return='univ': Maxell & Delaney (2004), Table 12.5 and 12.6, p. 578", {
  ### replicate results from Table 12.6
  data(md_12.1)
  # valus from table:
  f <- c(40.72, 33.77, 45.31)
  ss_num <- c(289920, 285660, 105120)
  ss_error <- c(64080, 76140, 20880)
  num_df <- c(2, 1, 2)
  den_df <- c(18, 9, 18)

  md_ez_r <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))
  md_car_r <- aov_car(rt ~ 1 + Error(id / angle * noise), md_12.1)
  md_aov_4_r <- aov_4(rt ~ 1 + (angle * noise | id), md_12.1)

  expect_that(md_ez_r, is_equivalent_to(md_car_r))
  expect_that(md_ez_r, is_equivalent_to(md_aov_4_r))
  expect_that(round(md_ez_r$anova_table[, "F"], 2), is_equivalent_to(f))
  tmp_univ <- suppressWarnings(summary(md_ez_r$Anova)$univariate.tests)
  if ("Sum Sq" %in% colnames(tmp_univ)) {
    # to allow both car 3.0 and earlier versions
    ss_test <- tmp_univ[, "Sum Sq"][-1]
  } else {
    ss_test <- tmp_univ[, "SS"][-1]
  }
  expect_that(ss_test, is_equivalent_to(ss_num))
  expect_that(tmp_univ[, "Error SS"][-1], is_equivalent_to(ss_error))
  expect_that(
    anova(md_ez_r, correction = "none")[, "num Df"],
    is_equivalent_to(num_df)
  )
  expect_that(
    anova(md_ez_r, correction = "none")[, "den Df"],
    is_equivalent_to(den_df)
  )
})

test_that("Analysis of Singmann & Klauer (2011, Exp. 1)", {
  data(sk2011.1, package = "afex")

  out1 <- aov_ez(
    "id",
    "response",
    sk2011.1[sk2011.1$what == "affirmation", ],
    within = c("inference", "type"),
    between = "instruction",
    anova_table = (es = "pes"),
    fun_aggregate = mean,
    return = "afex_aov"
  )

  df_num <- rep(1, 7)
  df_den <- rep(38, 7)
  MSE <- c(1072.42, 1007.21, 1007.21, 187.9, 187.9, 498.48, 498.48)
  F <- c(0.13, 13.01, 12.44, 0.06, 3.09, 29.62, 10.73)
  pes <- c(0, 0.26, 0.25, 0, 0.08, 0.44, 0.22)
  p <- c(0.72, 0.0009, 0.001, 0.81, 0.09, 0.001, 0.002)

  expect_that(out1$anova_table[["num Df"]], is_equivalent_to(df_num))
  expect_that(out1$anova_table[["den Df"]], is_equivalent_to(df_den))
  expect_that(out1$anova_table[["MSE"]], equals(MSE, tolerance = 0.001))
  expect_that(out1$anova_table[["F"]], equals(F, tolerance = 0.001))
  expect_that(out1$anova_table[["pes"]], equals(pes, tolerance = 0.02))
  expect_that(out1$anova_table[["Pr(>F)"]], equals(p, tolerance = 0.01))
})


test_that("Data from O'Brien & Kaiser replicates their paper (p. 328, Table 8, column 'average'", {
  data(obk.long, package = "afex")
  out1 <- aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long,
    observed = "gender",
    return = "afex_aov",
    anova_table = list(correction = "none")
  )

  expect_that(
    unname(unlist(out1[["anova_table"]][
      "treatment",
      c("num Df", "den Df", "F")
    ])),
    equals(c(2, 10, 3.94), tolerance = 0.001)
  )
  expect_that(
    unname(unlist(out1[["anova_table"]]["gender", c("num Df", "den Df", "F")])),
    equals(c(1, 10, 3.66), tolerance = 0.001)
  )
  expect_that(
    round(
      unname(unlist(out1[["anova_table"]][
        "treatment:gender",
        c("num Df", "den Df", "F")
      ])),
      2
    ),
    equals(c(2, 10, 2.86), tolerance = 0.001)
  )

  ## check against own results:
  anova_tab <- structure(
    list(
      `num Df` = c(2, 1, 2, 2, 4, 2, 4, 4, 8, 4, 8, 8, 16, 8, 16),
      `den Df` = c(10, 10, 10, 20, 20, 20, 20, 40, 40, 40, 40, 80, 80, 80, 80),
      MSE = c(
        22.8055555555555,
        22.8055555555555,
        22.8055555555555,
        4.01388888888889,
        4.01388888888889,
        4.01388888888889,
        4.01388888888889,
        1.5625,
        1.5625,
        1.5625,
        1.5625,
        1.20208333333333,
        1.20208333333333,
        1.20208333333333,
        1.20208333333333
      ),
      F = c(
        3.940494501098,
        3.65912050065102,
        2.85547267441343,
        16.1329196993199,
        4.85098375975551,
        0.282782484190432,
        0.636602429722426,
        16.6856704980843,
        0.0933333333333336,
        0.450268199233716,
        0.620437956204379,
        1.17990398215104,
        0.345292160558641,
        0.931293452060798,
        0.735935938468544
      ),
      ges = c(
        0.198248507309966,
        0.114806410630587,
        0.179183259116394,
        0.151232705544895,
        0.0967823866181358,
        0.00312317714869712,
        0.0140618480455475,
        0.12547183572154,
        0.00160250371109459,
        0.0038716854273722,
        0.010669821220833,
        0.0153706689696344,
        0.00905399063368842,
        0.012321395080303,
        0.0194734697889242
      ),
      `Pr(>F)` = c(
        0.0547069269265198,
        0.0848002538616402,
        0.104469234023772,
        6.73163655770545e-05,
        0.00672273209545241,
        0.756647338927411,
        0.642369488905348,
        4.02664339633774e-08,
        0.999244623719389,
        0.771559070589063,
        0.755484449904079,
        0.32158661418337,
        0.990124565656718,
        0.495611922963992,
        0.749561639456282
      )
    ),
    .Names = c("num Df", "den Df", "MSE", "F", "ges", "Pr(>F)"),
    heading = c("Anova Table (Type 3 tests)\n", "Response: value"),
    row.names = c(
      "treatment",
      "gender",
      "treatment:gender",
      "phase",
      "treatment:phase",
      "gender:phase",
      "treatment:gender:phase",
      "hour",
      "treatment:hour",
      "gender:hour",
      "treatment:gender:hour",
      "phase:hour",
      "treatment:phase:hour",
      "gender:phase:hour",
      "treatment:gender:phase:hour"
    ),
    class = c("data.frame")
  )

  expect_equal(out1[["anova_table"]], anova_tab, check.attributes = FALSE)
})

test_that("Data from O'Brien & Kaiser adjusted for familywise error rate (p. 328, Table 8, column 'average'", {
  data(obk.long, package = "afex")
  out1 <- aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long,
    observed = "gender",
    return = "afex_aov",
    anova_table = list(correction = "none", p_adjust_method = "bonferroni")
  )

  expect_that(
    unname(unlist(out1[["anova_table"]][
      "treatment",
      c("num Df", "den Df", "F")
    ])),
    equals(c(2, 10, 3.94), tolerance = 0.001)
  )
  expect_that(
    unname(unlist(out1[["anova_table"]]["gender", c("num Df", "den Df", "F")])),
    equals(c(1, 10, 3.66), tolerance = 0.001)
  )
  expect_that(
    round(
      unname(unlist(out1[["anova_table"]][
        "treatment:gender",
        c("num Df", "den Df", "F")
      ])),
      2
    ),
    equals(c(2, 10, 2.86), tolerance = 0.001)
  )

  ## check against own results:
  anova_tab <- structure(
    list(
      `num Df` = c(2, 1, 2, 2, 4, 2, 4, 4, 8, 4, 8, 8, 16, 8, 16),
      `den Df` = c(10, 10, 10, 20, 20, 20, 20, 40, 40, 40, 40, 80, 80, 80, 80),
      MSE = c(
        22.8055555555555,
        22.8055555555555,
        22.8055555555555,
        4.01388888888889,
        4.01388888888889,
        4.01388888888889,
        4.01388888888889,
        1.5625,
        1.5625,
        1.5625,
        1.5625,
        1.20208333333333,
        1.20208333333333,
        1.20208333333333,
        1.20208333333333
      ),
      F = c(
        3.940494501098,
        3.65912050065102,
        2.85547267441343,
        16.1329196993199,
        4.85098375975551,
        0.282782484190432,
        0.636602429722426,
        16.6856704980843,
        0.0933333333333336,
        0.450268199233716,
        0.620437956204379,
        1.17990398215104,
        0.345292160558641,
        0.931293452060798,
        0.735935938468544
      ),
      ges = c(
        0.198248507309966,
        0.114806410630587,
        0.179183259116394,
        0.151232705544895,
        0.0967823866181358,
        0.00312317714869712,
        0.0140618480455475,
        0.12547183572154,
        0.00160250371109459,
        0.0038716854273722,
        0.010669821220833,
        0.0153706689696344,
        0.00905399063368842,
        0.012321395080303,
        0.0194734697889242
      ),
      `Pr(>F)` = c(
        0.0547069269265198,
        0.0848002538616402,
        0.104469234023772,
        6.73163655770545e-05,
        0.00672273209545241,
        0.756647338927411,
        0.642369488905348,
        4.02664339633774e-08,
        0.999244623719389,
        0.771559070589063,
        0.755484449904079,
        0.32158661418337,
        0.990124565656718,
        0.495611922963992,
        0.749561639456282
      )
    ),
    .Names = c("num Df", "den Df", "MSE", "F", "ges", "Pr(>F)"),
    heading = c("Anova Table (Type 3 tests)\n", "Response: value"),
    row.names = c(
      "treatment",
      "gender",
      "treatment:gender",
      "phase",
      "treatment:phase",
      "gender:phase",
      "treatment:gender:phase",
      "hour",
      "treatment:hour",
      "gender:hour",
      "treatment:gender:hour",
      "phase:hour",
      "treatment:phase:hour",
      "gender:phase:hour",
      "treatment:gender:phase:hour"
    ),
    class = c("data.frame")
  )
  anova_tab$`Pr(>F)` <- p.adjust(anova_tab$`Pr(>F)`, method = "bonferroni")

  expect_equal(out1[["anova_table"]], anova_tab, check.attributes = FALSE)
})

test_that("afex_aov printing", {
  data(sk2011.1, package = "afex")
  out_new <- aov_ez(
    "id",
    "response",
    sk2011.1[sk2011.1$what == "affirmation", ],
    within = c("inference", "type"),
    between = "instruction",
    anova_table = (es = "pes"),
    fun_aggregate = mean,
    return = "afex_aov"
  )

  expect_output(print(out_new), "Signif. codes")
  expect_output(print(anova(out_new)), "Signif. codes")
  expect_output(print(nice(out_new)), "Anova")

  load("afex_aov_16_1.rda")
  expect_output(print(out1), "Anova")
  skip_if_not(packageVersion("car") >= '3.0.13')
  ## only throws error with car version '3.0.13' or larger, the
  expect_error(
    anova(out1),
    regexp = "summary.Anova.mlm() failed for 'afex_aov' object.",
    fixed = TRUE
  )
  expect_error(
    nice(out1),
    regexp = "summary.Anova.mlm() failed for 'afex_aov' object.",
    fixed = TRUE
  )
})
