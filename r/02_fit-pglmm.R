source("r/header.R")

list2env(readr::read_rds("objects/prepared-data.rds"), envir = globalenv())

safe_pglmm <- safely(pglmm)

plan(multisession, workers = 4)

set.seed(105497041)

taxa <- traits$taxa

# Define set of response and predictor variables
df_vars <- crossing(
  nesting(
    response = c(
      "achene_length",
      "length_log",
      "achene_width",
      "width_log",
      "mass_per_achene",
      "mass_log",
      "awn_presence",
      "setose_glabrous",
      "achene_shape",
      "wing_state"
    ),
    family = c(rep("gaussian", 6), rep("binomial", 4))
  ),
  predictor = c(
    "scaled_elev_min",
    "scaled_elev_midpoint",
    "scaled_elev_max",
    "scaled_elev_range",
    "costal_upland",
    "montane_subalpine",
    "younglava"
  )
)

# Map over trees
results <- future_map_dfr(seq_along(trees),
                          \(i) {
                            tree <- trees[[i]]
                            
                            re1 <- list(1, taxa__ = factor(taxa), covar = vcv.phylo(tree)[taxa, taxa])
                            
                            df_vars |>
                              mutate(
                                tree = list(tree),
                                tree_n = i,
                                form0 = glue("{response} ~ 1"),
                                form1 = glue("{response} ~ {predictor}"),
                                fit0 = map2(form0, family, ~ {
                                  pglmm(
                                    formula = .x,
                                    family = .y,
                                    data = traits,
                                    random.effects = list(taxa = re1),
                                    REML = FALSE
                                  )
                                }),
                                fit1 = map2(form1, family, ~ {
                                  pglmm(
                                    formula = .x,
                                    family = .y,
                                    data = traits,
                                    random.effects = list(taxa = re1),
                                    REML = FALSE
                                  )
                                }),
                                p_value = map2_dbl(fit1, predictor, ~ {
                                  .x$B.pvalue[.y, 1]
                                }),
                                delta_AIC = map2_dbl(fit0, fit1, ~ {
                                  .x$AIC - .y$AIC
                                })
                              ) |>
                              mutate(sim = map(fit1, ~ {
                                simulate(.x, nsim = 1e2)
                              }),
                              boot_pred = map2(fit1, sim, \(.x, .y) {
                                form <- formula(.x) |>
                                  as.formula()
                                response_var <- all.vars(form)[1]
                                predictor_var <- all.vars(form)[2]
                                fam <- family(.x)$family
                                re <- .x$random.effects
                                new_data <- select(.x$data, taxa, matches(predictor_var))
                                
                                map_dbl(seq_len(ncol(.y)), \(.z) {
                                  new_data[[response_var]] <- .y[, .z]
                                  fit_sim <- safe_pglmm(
                                    formula = form,
                                    family = fam,
                                    data = new_data,
                                    random.effects = re,
                                    REML = FALSE
                                  )
                                  if (is.null(fit_sim$result))
                                    NA_real_
                                  else
                                    fit_sim$result$B[predictor_var, 1]
                                })
                              }))
                            
                          },
                          .progress = TRUE,
                          .options = furrr_options(seed = TRUE)) |>
  split(. ~ response + predictor) |>
  map_dfr(\(.x) {
    tibble(
      response = first(.x$response),
      predictor = first(.x$predictor),
      boot_pred = list(unlist(.x$boot_pred, use.names = FALSE)),
      p_value = list(.x$p_value),
      delta_AIC = list(.x$delta_AIC)
    )
  })

write_rds(results, "objects/pglmm-results.rds")
