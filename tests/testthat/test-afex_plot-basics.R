data(obk.long, package = "afex")
# estimate mixed ANOVA on the full design:
a1 <- aov_car(
  value ~ treatment * gender + Error(id / (phase * hour)),
  data = obk.long,
  observed = "gender"
)
data(md_12.1)
aw <- aov_ez("id", "rt", md_12.1, within = c("angle", "noise"))

test_that("ANOVA plots are produced", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_on_cran() ## uses only expect_doppelganger
  expect_doppelganger("one-way within", afex_plot(a1, "hour", error = "within"))
  expect_doppelganger(
    "two-way",
    afex_plot(a1, c("phase", "hour"), trace = "treatment", error = "none")
  )
  expect_doppelganger(
    "x-trace-panel",
    afex_plot(
      a1,
      "phase",
      trace = "hour",
      panel = "treatment",
      error = "within"
    )
  )
})

test_that("all input type works and warnings are correct", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  expect_warning(
    em1 <- afex_plot(a1, ~ phase * hour, ~ treatment + gender, return = "data"),
    "mixed within-between-design"
  )
  expect_warning(
    em2 <- afex_plot(
      a1,
      c("phase", "hour"),
      ~ treatment + gender,
      return = "data"
    ),
    "mixed within-between-design"
  )
  expect_warning(
    em3 <- afex_plot(
      a1,
      ~ phase * hour,
      c("treatment", "gender"),
      return = "data"
    ),
    "mixed within-between-design"
  )
  expect_warning(
    em4 <- afex_plot(
      a1,
      c("phase", "hour"),
      c("treatment", "gender"),
      return = "data"
    ),
    "mixed within-between-design"
  )

  expect_equal(em1, em2)
  expect_equal(em1, em3)
  expect_equal(em1, em4)

  expect_warning(
    em5 <- afex_plot(a1, c("phase", "hour"), return = "data"),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_warning(
    em6 <- afex_plot(a1, ~ phase * hour, return = "data"),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_equal(em5, em6)

  expect_warning(
    em7 <- afex_plot(
      a1,
      c("treatment", "gender"),
      panel = "phase",
      return = "data",
      error = "within"
    ),
    "between-subjects factors, but within-subjects error bars"
  )
  expect_warning(
    em8 <- afex_plot(
      a1,
      ~ treatment * gender,
      panel = "phase",
      return = "data",
      error = "within"
    ),
    "between-subjects factors, but within-subjects error bars"
  )
  expect_equal(em7, em8)
})


test_that("mixed plots are produced", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_on_cran() ## uses only expect_doppelganger
  data("fhch2010") # load
  fhch <- droplevels(fhch2010[fhch2010$correct, ]) # remove errors
  ### reduced data.frame length
  # fhch <- fhch[unlist(lapply(which(as.numeric(fhch$id) !=
  #                                    c(NA, fhch$id[-length(fhch$id)])),
  #                            function(x) 0:29 + x)),]
  mrt <- mixed(
    log_rt ~ task * stimulus * frequency + (1 | id),
    fhch,
    method = "S",
    progress = FALSE
  )

  p1 <- afex_plot(mrt, "task", id = "id")
  expect_doppelganger("mixed model 1", p1)

  p2 <- afex_plot(mrt, x = "stimulus", panel = "task", id = "id")
  expect_doppelganger("mixed model 2", p2)

  p3 <- afex_plot(mrt, x = "stimulus", trace = "task", id = "id")
  expect_doppelganger("mixed model 3", p3)

  p4 <- afex_plot(
    mrt,
    x = "stimulus",
    trace = "frequency",
    panel = "task",
    id = "id"
  )
  expect_doppelganger("mixed model 4", p4)
})

test_that("lme4::merMod plots are produced", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  Oats <- nlme::Oats
  Oats$VarBlock <- Oats$Variety:Oats$Block
  Oats.lmer <- lmer(
    yield ~ Variety * factor(nitro) + (1 | VarBlock) + (1 | Block),
    data = Oats
  )
  p1 <- afex_plot(Oats.lmer, "nitro", id = "VarBlock")
  expect_doppelganger("lme4::merMod plot 1", p1)

  p2 <- afex_plot(Oats.lmer, "nitro", "Variety", id = "VarBlock")
  expect_doppelganger("lme4::merMod plot 2", p2)
  p3 <- afex_plot(Oats.lmer, "nitro", panel = "Variety", id = "VarBlock")
  expect_doppelganger("lme4::merMod plot 3", p3)

  ## check that id argument works:
  d1 <- afex_plot(Oats.lmer, "nitro", id = "VarBlock", return = "data")
  d2 <- afex_plot(Oats.lmer, "nitro", id = "Block", return = "data")
  d3 <- afex_plot(
    Oats.lmer,
    "nitro",
    id = c("Block", "VarBlock"),
    return = "data"
  )
  expect_lt(nrow(d2$data), nrow(d1$data))
  expect_lt(nrow(d2$data), nrow(d3$data))
  expect_identical(nrow(d1$data), nrow(d3$data))
})

test_that("afex_plot works with various geoms (from examples)", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("ggpol")
  testthat::skip_if_not_installed("ggbeeswarm")
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_on_cran() ## uses only expect_doppelganger
  set.seed(1)

  ### There are several ways to deal with overlapping points in the background besides alpha
  # Using the default data geom and ggplot2::position_jitterdodge
  g1 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    dodge = 0.3,
    data_arg = list(
      position = ggplot2::position_jitterdodge(
        jitter.width = 0,
        jitter.height = 5,
        dodge.width = 0.3 ## needs to be same as dodge
      )
    )
  )
  expect_doppelganger("geoms work: jitterdodge", g1)

  # Overlapping points are shown as larger points using geom_count
  g2 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    dodge = 0.5,
    data_geom = ggplot2::geom_count
  )
  expect_doppelganger("geoms work: geom_count", g2)

  # Using ggbeeswarm::geom_quasirandom (overlapping points shown in violin shape)
  g3 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    dodge = 0.5,
    data_geom = ggbeeswarm::geom_quasirandom,
    data_arg = list(
      dodge.width = 0.5, ## needs to be same as dodge
      cex = 0.8,
      width = 0.05 ## choose small value so data points are not overlapping
    )
  )
  expect_doppelganger("geoms work: ggbeeswarm::geom_quasirandom", g3)

  # Using ggbeeswarm::geom_beeswarm (overlapping points are adjacent on y-axis)
  g4 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    dodge = 0.5,
    data_geom = ggbeeswarm::geom_beeswarm,
    data_arg = list(
      dodge.width = 0.5, ## needs to be same as dodge
      cex = 0.8
    )
  )
  expect_doppelganger("geoms work: ggbeeswarm::geom_beeswarm", g4)

  # Do not display points, but use a violinplot: ggplot2::geom_violin
  g5 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    data_geom = ggplot2::geom_violin,
    data_arg = list(width = 0.5)
  )
  expect_doppelganger("geoms work: violin", g5)

  # violinplots with color: ggplot2::geom_violin
  g6 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    mapping = c("linetype", "shape", "fill"),
    data_geom = ggplot2::geom_violin,
    data_arg = list(width = 0.5)
  )
  expect_doppelganger("geoms work: violin with colour", g6)

  # do not display points, but use a boxplot: ggplot2::geom_boxplot
  g7 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    data_geom = ggplot2::geom_boxplot,
    data_arg = list(width = 0.3)
  )
  expect_doppelganger("geoms work: box plot", g7)

  ## disabled due to boxjitter failing with ggplot 4.0.0 (August 2025)
  # # combine points with boxplot: ggpol::geom_boxjitter
  # g8 <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
  #           data_geom = ggpol::geom_boxjitter,
  #           data_arg = list(width = 0.3))
  # expect_doppelganger("geoms work: boxjitter 1", g8)
  # ## hides error bars!

  # nicer variant of ggpol::geom_boxjitter
  # g9 <- afex_plot(aw, x = "noise", trace = "angle", error = "within",
  #           mapping = c("shape", "fill"),
  #           data_geom = ggpol::geom_boxjitter,
  #           data_arg = list(
  #             width = 0.3,
  #             jitter.params = list(width = 0, height = 10),
  #             outlier.intersect = TRUE),
  #           point_arg = list(size = 2.5),
  #           error_arg = list(linewidth = 1.5, width = 0))
  # expect_doppelganger("geoms work: boxjitter 2", g9)

  # nicer variant of ggpol::geom_boxjitter without lines
  # g10 <- afex_plot(aw, x = "noise", trace = "angle", error = "within", dodge = 0.7,
  #           mapping = c("shape", "fill"),
  #           data_geom = ggpol::geom_boxjitter,
  #           data_arg = list(
  #             width = 0.5,
  #             jitter.params = list(width = 0, height = 10),
  #             outlier.intersect = TRUE),
  #           point_arg = list(size = 2.5),
  #           line_arg = list(linetype = 0),
  #           error_arg = list(linewidth = 1.5, width = 0))
  # expect_doppelganger("geoms work: boxjitter 3", g10)

  ### we can also use multiple geoms for the background by passing a list of geoms
  g11 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    data_geom = list(
      ggplot2::geom_violin,
      ggplot2::geom_point
    )
  )
  expect_doppelganger("multiple geoms work 1", g11)

  ## with separate extra arguments:

  g12 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    dodge = 0.5,
    data_geom = list(
      ggplot2::geom_violin,
      ggplot2::geom_point
    ),
    data_arg = list(
      list(width = 0.4),
      list(
        position = ggplot2::position_jitterdodge(
          jitter.width = 0,
          jitter.height = 5,
          dodge.width = 0.5 ## needs to be same as dodge
        )
      )
    )
  )
  expect_doppelganger("multiple geoms work 2", g12)
})

test_that("relabeling of factors and legend works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")

  ## relabel factor levels via new_levels
  p1 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    factor_levels = list(
      angle = c("0", "4", "8"),
      noise = c("Absent", "Present")
    )
  )
  expect_equal(levels(p1$data$noise), c("Absent", "Present"))
  expect_equal(levels(p1$data$angle), c("0", "4", "8"))

  p2 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    factor_levels = list(
      angle = c(X8 = "8", X4 = "4", X0 = "0"),
      noise = c(present = "Present")
    )
  )
  expect_equal(levels(p2$data$angle), rev(c("0", "4", "8")))
  expect_equal(levels(p2$data$noise), c("absent", "Present"))

  p1d <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    factor_levels = list(
      angle = c("0", "4", "8"),
      noise = c("Absent", "Present")
    ),
    return = "data"
  )
  p2d <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    factor_levels = list(
      angle = c(X8 = "8", X4 = "4", X0 = "0"),
      noise = c(present = "Present")
    ),
    return = "data"
  )
  expect_equal(p1d$means$lower, p2d$means$lower)

  expect_warning(
    p3 <- afex_plot(
      aw,
      x = "noise",
      trace = "angle",
      error = "mean",
      factor_levels = list(
        angle = c(X8 = "8", X4 = "4", X0 = "0"),
        noise = c(present = "Present")
      )
    ),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_equal(levels(p3$data$angle), rev(c("0", "4", "8")))

  expect_warning(
    p3d <- afex_plot(
      aw,
      x = "noise",
      trace = "angle",
      error = "mean",
      factor_levels = list(
        angle = c(X8 = "8", X4 = "4", X0 = "0"),
        noise = c(present = "Present")
      ),
      return = "data"
    ),
    "show within-subjects factors, but not within-subjects error bars"
  )

  expect_warning(
    p3nd <- afex_plot(
      aw,
      x = "noise",
      trace = "angle",
      error = "mean",
      factor_levels = list(
        angle = c("0", "4", "8"),
        noise = c("Absent", "Present")
      ),
      return = "data"
    ),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_equal(p3d$means$lower, p3nd$means$lower)

  expect_warning(
    p4d <- afex_plot(
      aw,
      x = "noise",
      trace = "angle",
      error = "between",
      factor_levels = list(
        angle = c(X8 = "8", X4 = "4", X0 = "0"),
        noise = c(present = "Present")
      ),
      return = "data"
    ),
    "show within-subjects factors, but not within-subjects error bars"
  )

  expect_warning(
    p4nd <- afex_plot(
      aw,
      x = "noise",
      trace = "angle",
      error = "between",
      factor_levels = list(
        angle = c("0", "4", "8"),
        noise = c("Absent", "Present")
      ),
      return = "data"
    ),
    "show within-subjects factors, but not within-subjects error bars"
  )
  expect_equal(p4d$means$lower, p4nd$means$lower)

  expect_error(
    afex_plot(
      aw,
      x = "noise",
      trace = "angle",
      error = "within",
      factor_levels = list(angle = c("0", "4"), noise = c("Absent", "Present"))
    ),
    "length of new factor_levels for 'angle' != length of factor levels"
  )

  p2 <- afex_plot(
    aw,
    x = "noise",
    trace = "angle",
    error = "within",
    legend_title = "Noise Condition"
  )
  if (inherits(p2$guides, "Guides")) {
    expect_equal(p2$guides$guides$shape$params$title, "Noise Condition")
    expect_equal(p2$guides$guides$linetype$params$title, "Noise Condition")
  } else {
    expect_equal(p2$guides$shape$title, "Noise Condition")
    expect_equal(p2$guides$linetype$title, "Noise Condition")
  }
})

test_that("connecting individual points works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_on_cran() ## uses only expect_doppelganger

  p_con <- afex_plot(
    aw,
    x = "angle",
    error = "within",
    data_geom = list(ggplot2::geom_count, ggplot2::geom_line),
    data_arg = list(list(), list(mapping = ggplot2::aes(group = id))),
    point_arg = list(size = 2.5),
    error_arg = list(width = 0, linewidth = 1.5)
  ) +
    ggplot2::geom_line(ggplot2::aes(group = 1), linewidth = 1.5)
  expect_doppelganger("afex_plot connecting individual points works", p_con)
})

test_that("plot_first works", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_on_cran() ## uses only expect_doppelganger

  p_ref_line1 <- afex_plot(
    aw,
    x = "angle",
    trace = "noise",
    error = "within",
    plot_first = list(
      ggplot2::geom_hline(yintercept = 550),
      ggplot2::geom_vline(xintercept = 1.5)
    )
  )
  p_ref_line2 <- afex_plot(
    aw,
    x = "angle",
    trace = "noise",
    error = "within",
    plot_first = ggplot2::geom_hline(yintercept = 550)
  )
  p_ref_line3 <- afex_plot(
    aw,
    x = "angle",
    error = "within",
    plot_first = list(
      ggplot2::geom_hline(yintercept = 550),
      ggplot2::geom_vline(xintercept = 1.5)
    )
  )
  p_ref_line4 <- afex_plot(
    aw,
    x = "angle",
    trace = "noise",
    error = "within",
    plot_first = ggplot2::geom_hline(yintercept = 550)
  )
  expect_doppelganger("plot_first for afex_plot works 1", p_ref_line1)
  expect_doppelganger("plot_first for afex_plot works 2", p_ref_line2)
  expect_doppelganger("plot_first for afex_plot works 3", p_ref_line3)
  expect_doppelganger("plot_first for afex_plot works 4", p_ref_line4)
})

test_that("labels are correct in case variables are of lenth > 1", {
  testthat::skip_if_not_installed("emmeans")
  testthat::skip_if_not_installed("ggplot2")
  data(obk.long, package = "afex")
  # estimate mixed ANOVA on the full design:
  a1 <- aov_car(
    value ~ treatment * gender + Error(id / (phase * hour)),
    data = obk.long,
    observed = "gender"
  )

  p1 <- afex_plot(
    a1,
    c("phase", "hour"),
    c("treatment", "gender"),
    error = "none"
  )
  p2 <- afex_plot(a1, c("phase", "hour"), error = "none")
  expect_match(p1$labels$x, "phase")
  expect_match(p1$labels$x, "hour")
  if (inherits(p1$guides, "Guides")) {
    expect_match(p1$guides$guides$shape$params$title, "treatment")
    expect_match(p1$guides$guides$shape$params$title, "gender")
  } else {
    expect_match(p1$guides$shape$title, "treatment")
    expect_match(p1$guides$shape$title, "gender")
  }
  expect_match(p2$labels$x, "phase")
  expect_match(p2$labels$x, "hour")
})
