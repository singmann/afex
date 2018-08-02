
nested_model_matrix <- function(fixed_formula, data, 
                                random_formula,
                                type = 3,
                                per_parameter) {
  
  ## to avoid problems with tibbles:
  data <- as.data.frame(data)
  
  ## get informations
  #browser()
  
  mc <- match.call()
  formula.f <- as.formula(fixed_formula)
  if (!inherits(fixed_formula, "formula")) 
    message("Formula (the first argument) converted to formula.")
  dv <- as.character(formula.f)[[2]]
  all.terms <- attr(terms(formula.f), "term.labels")
  effect.order <- attr(terms(formula.f), "order")
  max.effect.order <- max(effect.order)
  random <- str_c(str_c("(", all.terms[grepl("\\|", all.terms)], ")"), 
                  collapse = " + ")
  rh2 <- nobars(formula.f)
  rh2[[2]] <- NULL
  m.matrix <- model.matrix(rh2, data = data)
  fixed.effects <- attr(terms(rh2, data = data), "term.labels")
  mapping <- attr(m.matrix, "assign")
  fixed.vars <- all.vars(rh2)
  # check for missing values in variables used:
  if (nrow(m.matrix) != nrow(data)) {
    data <- model.frame(
      as.formula(str_c(vars.to.check[1], 
                       "~", 
                       str_c(vars.to.check[-1], collapse = "+"))), 
      data = data)
    m.matrix <- model.matrix(rh2, data = data)
    warning(str_c("Due to missing values, reduced number of observations to ", 
                  nrow(data)))
    if(set_data_arg) {
      warning("Due to missing values, set_data_arg set to FALSE.")
      set_data_arg <- FALSE
    }
  }
  
  if (attr(terms(rh2, data = data), "intercept") == 1) 
    fixed.effects <- c("(Intercept)", fixed.effects)
  
  formulas <- vector("list", length(fixed.effects) + 1)
  formulas[[1]] <- mf[["formula"]]
  for (i in seq_along(fixed.effects)) {
    tmp.columns <- str_c(deparse(-which(mapping == (i-1))), collapse = "")
    formulas[[i+1]] <- 
      as.formula(str_c(dv, "~ 0 + m.matrix[,", tmp.columns, "] +", random))
  }
  names(formulas) <- c("full_model", fixed.effects)
  if (!test_intercept && fixed.effects[1] == "(Intercept)") {
    fixed.effects <- fixed.effects[-1]
    formulas[["(Intercept)"]] <- NULL
  }

  
}

