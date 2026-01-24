library(dplyr)

# Minimum column contract for Team Performance:
# - tourney (tournament title shown to users)
# - ORG (team/org identifier)
# - exptime (date or datetime per tournament instance)
# - Wins, Losses (per ORG x exptime)
# - tourneys_won (0/1 or FALSE/TRUE per ORG x exptime)

team_perf_base <- function(df, tourney_title, tourney_type = "All", start_date = NULL) {
  stopifnot(!missing(df), !missing(tourney_title))
  
  if (nrow(df) == 0) return(df)
  
  out <- df %>% filter(tourney == tourney_title)
  
  if (!is.null(tourney_type) &&
      tourney_type != "All" &&
      "tourney_type" %in% names(out)) {
    out <- out %>% filter(.data$tourney_type == tourney_type)
  }
  
  # Optional start-date filter (prep for future date ranges)
  ok_date <- !is.null(start_date) &&
    length(start_date) == 1 &&
    !is.na(start_date) &&
    (inherits(start_date, "Date") || is.character(start_date))
  
  if (ok_date && "exptime" %in% names(out)) {
    out <- out %>%
      filter(!is.na(exptime) & as.Date(exptime) >= as.Date(start_date))
  }
  
  out
}

team_perf_summary <- function(df, mintourn = 1) {
  if (nrow(df) == 0) return(df)
  
  df %>%
    group_by(ORG) %>%
    summarise(
      Wins   = sum(Wins, na.rm = TRUE),
      Losses = sum(Losses, na.rm = TRUE),
      WinPct = ifelse((Wins + Losses) > 0, round(Wins / (Wins + Losses), 3), NA_real_),
      n_tourneys = n(),
      tourneys_won = sum(tourneys_won, na.rm = TRUE),
      tourneys_win_per = ifelse(n_tourneys > 0, round(tourneys_won / n_tourneys, 2), NA_real_),
      .groups = "drop"
    ) %>%
    filter(n_tourneys >= mintourn) %>%
    arrange(desc(WinPct))
}

team_perf_date_span <- function(df) {
  if (!"exptime" %in% names(df)) return(NULL)
  d <- as.Date(df$exptime)
  d <- d[!is.na(d)]
  if (!length(d)) return(NULL)
  list(start = min(d), end = max(d))
}

add_hitter_rates <- function(df) {
  df %>%
    dplyr::mutate(
      BBpct   = dplyr::if_else(PA > 0, round(100 * BB / PA, 1), NA_real_),
      SOpct   = dplyr::if_else(AB > 0, round(100 * SO / AB, 1), NA_real_),
      HRpct   = dplyr::if_else(AB > 0, round(100 * HR / AB, 1), NA_real_),
      BABIP   = dplyr::if_else((AB - SO - HR) > 0, round((H - HR) / (AB - SO - HR), 3), NA_real_),
      XBH     = X2B + X3B + HR,
      XBH_pct = dplyr::if_else((H - HR) > 0, round(100 * (X2B + X3B) / (H - HR), 1), NA_real_)
    )
}

add_pitcher_rates <- function(df) {
  df %>%
    dplyr::mutate(
      GSper     = dplyr::if_else(G > 0, round(GS / G, 3), NA_real_),
      IPpergame = dplyr::if_else(G > 0, round(IP / G, 2), NA_real_),
      
      RA9 = dplyr::if_else(IP > 0, round(9 * R / IP, 2), NA_real_),
      ERA = dplyr::if_else(IP > 0, round(9 * ER / IP, 2), NA_real_),
      
      K9  = dplyr::if_else(IP > 0, round(9 * K / IP, 2), NA_real_),
      BB9 = dplyr::if_else(IP > 0, round(9 * BB / IP, 2), NA_real_),
      HR9 = dplyr::if_else(IP > 0, round(9 * HR / IP, 2), NA_real_),
      
      GBper = {
        denom <- GB + FB
        dplyr::if_else(denom > 0, round(GB / denom, 3), NA_real_)
      },
      
      BABIP = {
        denom <- AB - HR - K + SF
        numer <- X1B + X2B + X3B
        dplyr::if_else(denom > 0, round(numer / denom, 3), NA_real_)
      }
    )
}