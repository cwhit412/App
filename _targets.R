library(targets)

# Make all functions (add_hitter_rates, compute_woba_*, make_team_perf_base, etc.)
# available to the pipeline
source("functions.R")

tar_option_set(
  packages = c("dplyr", "readr", "purrr", "stringr", "fs", "tibble")
)

list(
  tar_target(
    perf_files,
    fs::dir_ls("../raw_data", regexp = "\\.csv$") |>
      purrr::discard(~ stringr::str_detect(basename(.x), "^tourn_details\\.csv$")),
    format = "file",
    cue = tar_cue(mode = "always")
  ),
  
  tar_target(
    tourn_details_file,
    "tournament_data/tourn_details.csv",
    format = "file",
    cue = tar_cue(mode = "always")
  ),
  
  tar_target(
    tourn_details,
    if (!fs::file_exists(tourn_details_file)) tibble::tibble()
    else readr::read_csv(tourn_details_file, show_col_types = FALSE)
  ),
  
  tar_target(
    all_perf_raw,
    if (length(perf_files) == 0) tibble::tibble()
    else purrr::map_dfr(perf_files, read_tourney_csv) %>%
      dplyr::left_join(tourn_details, by = c("tourney_type" = "varname"))
  ),
  
  tar_target(
    team_perf_base,
    if (nrow(all_perf_raw) == 0) tibble::tibble()
    else make_team_perf_base(all_perf_raw)
  ),
  
  tar_target(
    team_perf_base_rds,
    {
      out_path <- file.path("tournament_data", "team_perf_base.rds")
      saveRDS(team_perf_base, out_path)
      out_path
    },
    format = "file"
  ),
  
  tar_target(
    all_perf_raw_rds,
    {
      out_path <- file.path("tournament_data", "all_perf_raw.rds")
      saveRDS(all_perf_raw, out_path)
      out_path
    },
    format = "file"
  ),
  
  # --- wOBA weights by tournament_type ---------------------------------------
  tar_target(
    woba_weights,
    if (nrow(all_perf_raw) == 0) {
      tibble::tibble()
    } else {
      compute_woba_by_tourney(all_perf_raw)
    }
  ),
  
  tar_target(
    woba_weights_rds,
    {
      out_path <- file.path("tournament_data", "woba_weights.rds")
      saveRDS(woba_weights, out_path)
      out_path
    },
    format = "file"
  )
)