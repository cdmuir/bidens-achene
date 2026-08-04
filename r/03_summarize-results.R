source("r/header.R")

pglmm_results <- read_rds("objects/pglmm-results.rds")

# |>
#   mutate(
#     lowerCI = map_dbl(boot_pred, ~ {quantile(.x, probs = 0.025, na.rm = TRUE)}),
#     upperCI = map_dbl(boot_pred, ~ {quantile(.x, probs = 0.975, na.rm = TRUE)}),
#     prob_direction = map_dbl(boot_pred, ~ {pd(.x)$pd}),
#     boot_p_value = pd_to_p(prob_direction),
#     r2 = map_dbl(fit1, R2_pred)
#   )
