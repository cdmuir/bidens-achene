source("r/header.R")

outgroup_taxa <- c("Bidens_pilosa", "Bidens_alba_var_radiata")

# Read in tree and trait data (add log values for continuous response vars)
trees1 <- read.nexus("data/sampled_100_trees.nex")
trees2 <- vector("list", length = length(trees1))

for (i in seq_along(trees1)) {
  trees2[[i]] = trees1[[i]] |>
    drop.tip(outgroup_taxa)
}

trait_data <- read_csv("data/bidens_achene.csv", show_col_types = FALSE) |>
  filter(!taxa %in% outgroup_taxa) |>
  mutate(
    across(achene_length:mass_per_achene, log10, .names = "{.col}_log"),
    across(starts_with("elev_"), ~ {
      scale(.x)[, 1]
    }, .names = "scaled_{.col}")
  ) |>
  rename(length_log = achene_length_log,
         width_log = achene_width_log,
         mass_log = mass_per_achene_log)
