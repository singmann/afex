
nested_model_matrix <- function(fixed_formula, data, 
                                random_formula,
                                type = 3,
                                test_intercept = FALSE,
                                per_parameter,
                                na.action) {
  
  ## to avoid problems with tibbles:
  data <- as.data.frame(data)
  
  
  #mc <- match.call()
  formula <- as.formula(fixed_formula)
  if (!inherits(fixed_formula, "formula")) 
    message("Formula (the first argument) converted to formula.")
  dv <- as.character(formula)[[2]]

  full_formula <- reformulate(unique(c(all.vars(formula[[3]]), 
                                       all.vars(random_formula))), 
              response = formula[[2]])
  new_data <- model.frame(full_formula, data = data, na.action = na.action)
  
  all_terms <- attr(terms(formula), "term.labels")
  effect_order <- attr(terms(formula), "order")
  max_effect_order <- max(effect_order)
  
  m_matrix <- model.matrix(formula, data = new_data)

  fixed_effects <- attr(terms(formula, data = data), "term.labels")
  mapping <- attr(m_matrix, "assign")
  fixed_vars <- all.vars(formula)[-1]
  
  out_data <- data.frame(new_data, m_matrix)
  tmp_colnames <- colnames(out_data)[-seq_len(ncol(new_data))]
  
  if (attr(terms(formula, data = data), "intercept") == 1) {
    fixed_effects <- c("(Intercept)", fixed_effects)
  }
  
  formulas <- vector("list", length(fixed_effects) + 1)
  formulas[[1]] <- formula
  for (i in seq_along(fixed_effects)) {
    formulas[[i+1]] <- reformulate(tmp_colnames[!(mapping == (i-1))], 
                response = dv, intercept = FALSE)
  }
  names(formulas) <- c("full_model", fixed_effects)
  if (!test_intercept && fixed_effects[1] == "(Intercept)") {
    fixed_effects <- fixed_effects[-1]
    formulas[["(Intercept)"]] <- NULL
  }
  
  full_formulas <- lapply(formulas, function(x)
    as.formula(paste(deparse(x), deparse(random_formula[[2]]))))
  
  out <- list(
    formulas = full_formulas,
    data = out_data,
    fixed_formulas = formulas
  )
  
  return(out)
  
}

