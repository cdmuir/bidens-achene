source("r/header.R")

pglmm_results <- read_rds("objects/pglmm-results.rds")
pglmm_results_multi_single <- read_rds("objects/pglmm-results-multi_single.rds")

tab <- pglmm_results |>
  mutate(
    delta_AIC = map_dbl(delta_AIC, mean),
    r2_pred = map_dbl(r2_pred, mean),
    lowerCI = map_dbl(boot_pred, ~ {quantile(.x, probs = 0.025, na.rm = TRUE)}),
    upperCI = map_dbl(boot_pred, ~ {quantile(.x, probs = 0.975, na.rm = TRUE)}),
    prob_direction = map_dbl(boot_pred, ~ {pd(.x)$pd}),
    boot_p_value = pd_to_p(prob_direction)
  ) |>
  arrange(boot_p_value) |>
  select(response, predictor, delta_AIC, r2_pred, lowerCI, upperCI, prob_direction, boot_p_value)

# Table S2-S3
table_s2s3 <- tab |>
  mutate(
    boot_p_value = sprintf("%.3f", boot_p_value),
    delta_AIC = sprintf("%.3f", delta_AIC),
    r2_pred = sprintf("%.3f", r2_pred),
    Trait = case_when(
      response == "awn_presence" ~ "Awn Presence",
      response == "achene_length" ~ "Achene Length (mm)",
      response == "setose_glabrous" ~ "Setose vs. Glabrous",
      response == "length_log" ~ "Achene Length (mm)",
      response == "achene_shape" ~ "Achene Shape",
      response == "mass_log" ~ "Achene Mass (g)",
      response == "mass_per_achene" ~ NA,
      response == "width_log" ~ "Achene Width (mm)",
      response == "wing_state" ~ "Wing State",
      response == "achene_width" ~ NA
    ),
    Predictor = case_when(
      predictor == "younglava" ~ "Young lava",
      predictor == "costal_upland" ~ "Coastal/Upland",
      predictor == "scaled_elev_min" ~ "Elevation min (m)",
      predictor == "scaled_elev_max" ~ "Elevation max (m)",
      predictor == "scaled_elev_range" ~ "Elevation range (m)",
      predictor == "scaled_elev_midpoint" ~ "Elevation mid (m)",
      predictor == "montane_subalpine" ~ "Montane/Subalpine"
    ),
    table = case_when(
      response %in% c("length_log", "mass_log", "width_log") ~ "s2",
      response %in% c(
        "achene_shape",
        "awn_presence",
        "setose_glabrous",
        "wing_state"
      ) ~ "s3"
    )
  ) |>
  filter(!is.na(Trait)) |>
  arrange(table, Trait, Predictor, boot_p_value, delta_AIC) |>
  select(table,
         Trait,
         Predictor,
         `Bootstrapped *P*-value` = boot_p_value,
         `ΔAIC` = delta_AIC,
         `R2_pred` = r2_pred)

# Table S4: predictors of multi_single (whether a taxon occurs on
# multiple islands vs. a single island)
table_s4 <- pglmm_results_multi_single |>
  mutate(
    delta_AIC = map_dbl(delta_AIC, mean),
    r2_pred = map_dbl(r2_pred, mean),
    lowerCI = map_dbl(boot_pred, ~ {quantile(.x, probs = 0.025, na.rm = TRUE)}),
    upperCI = map_dbl(boot_pred, ~ {quantile(.x, probs = 0.975, na.rm = TRUE)}),
    prob_direction = map_dbl(boot_pred, ~ {pd(.x)$pd}),
    boot_p_value = pd_to_p(prob_direction)
  ) |>
  mutate(
    boot_p_value = sprintf("%.3f", boot_p_value),
    delta_AIC = sprintf("%.3f", delta_AIC),
    r2_pred = sprintf("%.3f", r2_pred),
    Predictor = case_when(
      predictor == "awn_presence" ~ "Awn Presence",
      predictor == "setose_glabrous" ~ "Setose vs. Glabrous",
      predictor == "achene_shape" ~ "Achene Shape",
      predictor == "wing_state" ~ "Wing State",
      predictor == "length_log" ~ "Achene Length (mm)",
      predictor == "width_log" ~ "Achene Width (mm)",
      predictor == "mass_log" ~ "Achene Mass (g)"
    )
  ) |>
  arrange(boot_p_value, delta_AIC) |>
  select(Predictor,
         `Bootstrapped *P*-value` = boot_p_value,
         `ΔAIC` = delta_AIC,
         `R2_pred` = r2_pred)

# Create a Word document
doc <- read_docx() |>
  body_add_par("Tables S2-S4", style = "heading 1")

# Convert the dataframe to a flextable and add it to the document
ft2 <- table_s2s3 |>
  filter(table == "s2") |>
  select(-table) |>
  flextable() |>
  autofit() |>
  set_caption("Table S2")

ft3 <- table_s2s3 |>
  filter(table == "s3") |>
  select(-table) |>
  flextable() |>
  autofit() |>
  set_caption("Table S3")

ft4 <- table_s4 |>
  flextable() |>
  autofit() |>
  set_caption("Table S4")

doc <- doc |>
  body_add_flextable(ft2) |>
  body_add_break() |>
  body_add_flextable(ft3) |>
  body_add_break() |>
  body_add_flextable(ft4)

# Save the document
print(doc, target = "Table_S2-S4.docx")

# Write tab for figure
write_rds(tab, "objects/pglmm-summary.rds")
