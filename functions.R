library(dplyr)

# Minimum column contract for Team Performance:
# - tourney (tournament title shown to users)
# - ORG (team/org identifier)
# - exptime (date or datetime per tournament instance)
# - Wins, Losses (per ORG x exptime)
# - tourneys_won (0/1 or FALSE/TRUE per ORG x exptime)

# Read a single raw tournament CSV and tag with tourney_type + source_file
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
  if (!is.na(name) && nzchar(name)) {
    if (!is.na(type) && nzchar(type)) {
      paste0(name, " (", type, ")")
    } else {
      name
    }
  } else {
    tourney_type
  }
}

# Build the team_perf_base table from all_perf_raw
make_team_perf_base <- function(df) {
  needed <- c("ORG", "exptime", "W", "L", "tourney_type")
  missing <- setdiff(needed, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns for Team Performance: ",
         paste(missing, collapse = ", "))
  }
  
  df %>%
    dplyr::mutate(
      Wins   = suppressWarnings(as.numeric(.data$W)),
      Losses = suppressWarnings(as.numeric(.data$L)),
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
 
#compute wOBA weights function
compute_woba_weights <- function(df) {
  totals <- df %>%
    summarise(
      PAsum     = sum(PA, na.rm=TRUE),
      ABsum     = sum(AB, na.rm=TRUE),
      singlesum = sum(X1B.1, na.rm=TRUE),
      doublesum = sum(X2B.1, na.rm=TRUE),
      triplesum = sum(X3B.1, na.rm=TRUE),
      HRsum     = sum(HR, na.rm=TRUE),
      BBsum     = sum(BB, na.rm=TRUE),
      IBBsum    = sum(IBB, na.rm=TRUE),
      HPsum     = sum(HP, na.rm=TRUE),
      SFsum     = sum(SF, na.rm=TRUE),
      SHsum     = sum(SH, na.rm=TRUE),
      SBsum     = sum(SB, na.rm=TRUE),
      CSsum     = sum(CS, na.rm=TRUE),
      outsum    = sum(IP, na.rm=TRUE)*3,
      runssum   = sum(R, na.rm=TRUE)
    )
  
  with(as.list(totals), {
    triplerate <- ifelse((triplesum + doublesum) > 0, triplesum / (triplesum + doublesum), 0)
    
    RperPA   <- runssum / PAsum
    rperout  <- runssum / outsum
    rbb      <- rperout + 0.14
    rhbp     <- rbb + 0.025
    r1b      <- rbb + 0.155
    r2b      <- r1b + 0.3
    r3b      <- r2b + 0.27
    rhr      <- 1.4
    rsb      <- 0.2
    rcs      <- 2*rperout + 0.075
    runval   <- r1b*singlesum + r2b*doublesum + r3b*triplesum + HRsum*rhr +
      (BBsum-IBBsum)*rbb + rhbp*HPsum + rsb*SBsum - rcs*CSsum
    
    unproouts <- ABsum - (singlesum + doublesum + triplesum + HRsum) + SFsum
    proouts   <- (BBsum-IBBsum+HPsum+singlesum+doublesum+triplesum+HRsum)
    runminus  <- runval / unproouts
    runplus   <- runval / proouts
    wobascale <- 1 / (runminus + runplus)
    
    wobabb   <- (rbb   + runminus) * wobascale
    wobaHBP  <- (rhbp  + runminus) * wobascale
    woba1B   <- (r1b   + runminus) * wobascale
    woba2B   <- (r2b   + runminus) * wobascale
    woba3B   <- (r3b   + runminus) * wobascale
    wobaHR   <- (rhr   + runminus) * wobascale
    wobaSB   <- rsb    * wobascale
    wobaCS   <- rcs    * wobascale
    
    tibble(
      wBB    = wobabb,      # walk (non-intentional)
      wHBP   = wobaHBP,
      w1B    = woba1B,
      w2B    = woba2B,
      w3B    = woba3B,
      wHR    = wobaHR,
      wSB    = wobaSB,
      wCS    = wobaCS,
      scale  = wobascale,
      runenv = runssum / PAsum
    )
  })
}

# Compute wOBA weights for each tournament_type
compute_woba_by_tourney <- function(df) {
  # df is expected to be all_perf_raw
  
  df %>%
    dplyr::group_by(tourney_type) %>%
    dplyr::group_modify(~ compute_woba_weights(.x)) %>%
    dplyr::ungroup()
}