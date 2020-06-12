dataset <- readRDS("development/dataset.RDS")
model <- tryCatch(afex::mixed(
  formula         = as.formula("JaspColumn_.3._Encoded~JaspColumn_.1._Encoded*JaspColumn_.10._Encoded+(JaspColumn_.1._Encoded*JaspColumn_.10._Encoded|JaspColumn_.12._Encoded)"),
  data            = dataset,
  type            = "3",
  method          = "LRT",
  test_intercept  = FALSE,
  args_test       = list(nsim = 500),
  check_contrasts = TRUE,
  #start           = start,
  family          = eval(call("binomial","logit"))
), error = function(e)return(e))

str(dataset)

p <- afex::afex_plot(
  model,
  #dv          = "JaspColumn_.3._Encoded",
  x           = "JaspColumn_.10._Encoded",
  id          = "JaspColumn_.12._Encoded",
  data_geom   = getFromNamespace("geom_violin", "ggplot2"),
  mapping     = c("shape","linetype","fill" ),
  error       = "model",
  error_level = .95,
  data_alpha  = .7,
  data_arg    = list(width = 1, color = "darkgrey"),
  error_arg   = list(width = 0, size  = .5 * 1),
  point_arg   = list(size = 1.5 * 1),
  line_arg    = list(size = .5 * 1),
  legend_title= "",
  dodge       = .3
)
p

