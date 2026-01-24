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

# Create a nice display label from the details sheet (safe defaults)
make_tourney_label <- function(tourney_type, name = NA_character_, type = NA_character_) {
  # Choose what you want in the UI:
  # 1) just name if present
  # 2) name + type if present
  if (!is.na(name) && nzchar(name)) {
    if (!is.na(type) && nzchar(type)) {
      return(paste0(name, " (", type, ")"))
    } else {
      return(name)
    }
  }
  tourney_type
}

make_team_perf_base <- function(df) {
  needed <- c("ORG", "exptime", "W", "L", "tourney_type")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns for Team Performance: ", paste(missing, collapse = ", "))
  }
  
  df %>%
    dplyr::mutate(
      Wins   = suppressWarnings(as.numeric(.data$W)),
      Losses = suppressWarnings(as.numeric(.data$L)),
      # label from details sheet; fallback to tourney_type
      tourney = dplyr::coalesce(.data$name, .data$tourney_type)
    ) %>%
    dplyr::group_by(.data$tourney_type, .data$tourney, .data$exptime, .data$ORG) %>%
    dplyr::summarise(
      Wins   = sum(.data$Wins, na.rm = TRUE),
      Losses = sum(.data$Losses, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(.data$tourney_type, .data$tourney, .data$exptime) %>%
    dplyr::mutate(
      tourneys_won = as.integer(.data$Wins == max(.data$Wins, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
}

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
  ),
  
  tar_target(
    all_perf_raw_rds,
    {
      out_path <- file.path("tournament_data", "all_perf_raw.rds")
      saveRDS(all_perf_raw, out_path)
      out_path
    },
    format = "file"
  )
)