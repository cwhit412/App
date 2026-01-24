library(targets)

tar_option_set(
  packages = c("dplyr", "readr", "purrr", "stringr", "fs", "tibble")
)

read_tourney_csv <- function(path) {
  tourney_type <- stringr::str_replace(basename(path), "\\.csv$", "")
  
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::mutate(
      tourney_type = tourney_type,
      source_file  = basename(path)
    )
}

make_team_perf_base <- function(df) {
  # Expect at least: ORG, exptime, Title, W, L
  needed <- c("ORG", "exptime", "Title", "W", "L", "tourney_type")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns for Team Performance: ", paste(missing, collapse = ", "))
  }
  
  df %>%
    dplyr::mutate(
      tourney = .data$Title,   # TEMP: tournament display name
      Wins    = suppressWarnings(as.numeric(.data$W)),
      Losses  = suppressWarnings(as.numeric(.data$L))
    ) %>%
    dplyr::group_by(.data$tourney_type, .data$tourney, .data$exptime, .data$ORG) %>%
    dplyr::summarise(
      Wins   = sum(.data$Wins, na.rm = TRUE),
      Losses = sum(.data$Losses, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # determine who "won" each tournament instance:
    # winner = max Wins within each (tourney_type, tourney, exptime)
    dplyr::group_by(.data$tourney_type, .data$tourney, .data$exptime) %>%
    dplyr::mutate(
      tourneys_won = as.integer(.data$Wins == max(.data$Wins, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
}

list(
  tar_target(
    perf_files,
    fs::dir_ls("tournament_data", regexp = "\\.csv$"),
    format = "file"
  ),
  
  tar_target(
    all_perf_raw,
    if (length(perf_files) == 0) tibble::tibble() else purrr::map_dfr(perf_files, read_tourney_csv)
  ),
  
  tar_target(
    team_perf_base,
    if (nrow(all_perf_raw) == 0) tibble::tibble() else make_team_perf_base(all_perf_raw)
  ),
  
  tar_target(
    team_perf_base_rds,
    {
      out_path <- file.path("tournament_data", "team_perf_base.rds")
      saveRDS(team_perf_base, out_path)
      out_path
    },
    format = "file"
  )
)