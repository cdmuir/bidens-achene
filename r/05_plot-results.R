source("r/header.R")

pglmm_results <- read_rds("objects/pglmm-summary.rds") |>
  mutate(
    `Environmental predictor` = case_when(
      predictor == "costal_upland" ~ "Coastal/Upland",
      predictor == "montane_subalpine" ~ "Montane/Subalpine",
      predictor == "scaled_elev_max" ~ "Elevation (max)",
      predictor == "scaled_elev_midpoint" ~ "Elevation (mid)",
      predictor == "scaled_elev_min" ~ "Elevation (min)",
      predictor == "scaled_elev_range" ~ "Elevation (range)",
      predictor == "younglava" ~ "Young Lava"
    ),
    `Trait response` = case_when(
      response == "length_log" ~ "Achene Length (mm)",
      response == "width_log" ~ "Achene Width (mm)",
      response == "awn_presence" ~ "Awn Presence",
      response == "mass_log" ~ "Achene Mass (g)",
      response == "achene_shape" ~ "Achene Shape",
      response == "setose_glabrous" ~ "Setose vs Glabrous",
      response == "wing_state" ~ "Wing State",
      TRUE ~ NA_character_
    ),
    Significance = case_when(boot_p_value <= 0.05 ~ "italic(P) <= 0.05", boot_p_value > 0.05 ~ "italic(P) > 0.05")
  ) |>
  filter(!is.na(`Trait response`))

ggplot(
  pglmm_results,
  aes(
    x = `Environmental predictor`,
    y = `Trait response`,
    color = Significance,
    size = delta_AIC
  )
) +
  geom_point() +
  scale_color_manual(
    values = c("tomato", "grey"),
    labels = function(x)
      parse(text = x)
  ) +
  scale_size_continuous(
    name = expression(Delta * AIC),
    breaks = c(-2, 0, 2),
    limits = c(-2.005, 3)
  ) +
  labs(title = "PGLMM AIC and statistical significance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

# Figure 3
ggsave(
  "pglmm_results.png",
  width = 6,
  height = 4.5,
  dpi = 300
)
