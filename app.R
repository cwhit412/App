library(shiny)
library(dplyr)
library(DT)
library(tibble)

source("functions.R")


# Root directory of the *deployed* app bundle.
# On shinyapps.io, getwd() will be the app directory.
APP_DIR <- getwd()

TEAM_RDS <- file.path(APP_DIR, "tournament_data", "team_perf_base.rds")
RAW_RDS  <- file.path(APP_DIR, "tournament_data", "all_perf_raw.rds")
W_RDS    <- file.path(APP_DIR, "tournament_data", "woba_weights.rds")

# Helpful checks so the app fails with a clear message instead of just dying
if (!file.exists(TEAM_RDS)) {
  stop("Missing RDS file: ", TEAM_RDS)
}
if (!file.exists(RAW_RDS)) {
  stop("Missing RDS file: ", RAW_RDS)
}

team_perf_base_static <- readRDS(TEAM_RDS)
raw_static            <- readRDS(RAW_RDS)
woba_weights_static   <- if (file.exists(W_RDS)) readRDS(W_RDS) else tibble()

 
HITTER_DISPLAY_COLS <- c(
  "Name", "VAL", "VLvl", "Hand",
  "PA",
  "AVG", "OBP", "SLG", "wOBA",
  "BBpct", "SOpct", "HRper600", "BABIP"
)

PITCHER_DISPLAY_COLS <- c(
  "CID",            # <--- keep the card ID
  "Name", 'VLvl', "IP", "GSper", "IPpergame", "RA9", "ERA",
  "K9", "BB9", "HR9", "BABIP", "GBper"
)


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .modal-dialog {
      width: 90vw !important;
      max-width: 95vw !important;
    }
    .modal-content {
      width: 100%;
    }
    .modal-body {
      max-height: 80vh;
      overflow-y: auto;
    }
  "))
  ),

  sidebarLayout(
    sidebarPanel(
      uiOutput("data_status"),
      
      uiOutput("tourney_type_ui"),
      uiOutput("tourney_ui"),
      
      dateInput("start_date", "Start date (optional)", value = NA),
      
      # Teams-only slider
      uiOutput("mintourn_ui"),
      
      # Hitters tab filters
      conditionalPanel(
        condition = "input.main_tabs == 'Hitters'",
        uiOutput("hitter_pos_ui"),
        uiOutput("hitter_min_pa_ui")
      ),
      
      # Pitchers tab filters
      conditionalPanel(
        condition = "input.main_tabs == 'Pitchers'",
        uiOutput("pitcher_role_ui"),   # <-- NEW
        uiOutput("pitcher_min_ip_ui")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel(
          "Teams",
          uiOutput("team_data_span"),
          DTOutput("team_perf_tbl")
        ),
        
        tabPanel(
          "Hitters",
          div(class = "small text-muted", uiOutput("hitter_data_span")),
          uiOutput("tourney_summary_ui"),   # <--- new unified summary
          DTOutput("hitter_summary_tbl")
        ),
        
        tabPanel(
          "Pitchers",
          div(class = "small text-muted", uiOutput("pitcher_data_span")),
          uiOutput("pitcher_tourney_summary_ui"),   # <--- new
          DTOutput("pitcher_summary_tbl")
        )
      )
    )
  )
)

  server <- function(input, output, session) {
    
    modal_level <- reactiveVal("team")
    
    team_perf_base_df <- reactive({
      team_perf_base_static
    })
    
    raw_df <- reactive({
      df <- raw_static
      
      # Ensure CID column exists so group_by/filter on CID never explodes
      if (!"CID" %in% names(df)) {
        df$CID <- NA_integer_
      }
      
      df
    })
    
    all_perf <- reactive({
      df <- team_perf_base_df()
      if (is.null(df)) tibble::tibble() else df
    })
    # wOBA weights for the app (by tournament_type)
    woba_weights <- reactive({
      woba_weights_static
    })
  
    # existing:
    selected_org        <- reactiveVal(NULL)
    team_raw_rv         <- reactiveVal(NULL)
    by_tourney_rv       <- reactiveVal(NULL)
    instances_rv        <- reactiveVal(NULL)
    
    selected_tourney        <- reactiveVal(NULL)
    selected_tourney_label  <- reactiveVal(NULL)
    tourney_instances_rv    <- reactiveVal(NULL)
    selected_exptime        <- reactiveVal(NULL)
    
    # NEW: player-level state
    selected_player_cid   <- reactiveVal(NULL)
    selected_player_name  <- reactiveVal(NULL)
    
    # NEW: store last rosters so row-clicks can see them
    tourney_bat_rv <- reactiveVal(NULL)
    tourney_pit_rv <- reactiveVal(NULL)
    inst_bat_rv    <- reactiveVal(NULL)
    inst_pit_rv    <- reactiveVal(NULL)
  
  # helper: recompute and show TEAM modal from ORG + team_universe()
  show_team_modal <- function() {
    org <- selected_org()
    req(org)
    
    raw <- team_universe() %>% dplyr::filter(ORG == org)
    req(nrow(raw) > 0)
    
    # by tournament (with key + label, stored but not shown)
    by_tourney <- raw %>%
      dplyr::group_by(tourney_type, tourney) %>%
      dplyr::summarise(
        Tournaments  = dplyr::n(),
        Wins         = sum(Wins, na.rm = TRUE),
        Losses       = sum(Losses, na.rm = TRUE),
        WinPct       = ifelse(
          (Wins + Losses) > 0,
          round(Wins / (Wins + Losses), 3),
          NA_real_
        ),
        TourneysWon  = sum(tourneys_won, na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(Tournaments))
    
    # display-only version
    by_tourney_show <- by_tourney %>%
      dplyr::select(-tourney_type) %>%
      dplyr::rename(Tournament = tourney)
    
    # instance table (with key stored but not shown)
    instances <- raw %>%
      dplyr::arrange(dplyr::desc(exptime)) %>%
      dplyr::transmute(
        tourney      = tourney,
        tourney_type = tourney_type,
        exptime      = as.character(exptime),
        Wins         = Wins,
        Losses       = Losses,
        WinPct       = ifelse(
          (Wins + Losses) > 0,
          round(Wins / (Wins + Losses), 3),
          NA_real_
        ),
        Won          = tourneys_won
      )
    
    instances_show <- instances %>%
      dplyr::select(-tourney_type)
    
    # overall row
    overall <- raw %>%
      dplyr::summarise(
        Team        = org,
        Tournaments = dplyr::n(),
        Wins        = sum(Wins, na.rm = TRUE),
        Losses      = sum(Losses, na.rm = TRUE),
        WinPct      = ifelse(
          (Wins + Losses) > 0,
          round(Wins / (Wins + Losses), 3),
          NA_real_
        ),
        TourneysWon = sum(tourneys_won, na.rm = TRUE),
        .groups     = "drop"
      )
    
    # store for downstream clicks
    team_raw_rv(raw)
    by_tourney_rv(by_tourney)
    instances_rv(instances)
    
    modal_level("team")
    showModal(render_drilldown_modal("team"))
    
    # wire outputs
    output$team_modal_overall <- renderDT({
      datatable(overall, rownames = FALSE, options = list(dom = "t"))
    })
    
    output$team_modal_by_tourney <- renderDT({
      datatable(
        by_tourney_show,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 10, scrollX = TRUE)
      ) %>%
        formatRound(columns = "WinPct", digits = 3)
    })
    
    output$team_modal_instances <- renderDT({
      datatable(
        instances_show,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 10, scrollX = TRUE)
      ) %>%
        formatRound(columns = "WinPct", digits = 3)
    })
  }
  
  build_player_modal_outputs <- function() {
    cid <- selected_player_cid()
    req(cid)
    
    raw <- raw_df()
    req(!is.null(raw), nrow(raw) > 0)
    
    raw_card <- raw %>% dplyr::filter(CID == cid)
    req(nrow(raw_card) > 0)
    
## ----------------- BATTING: same layout as hitters tab -----------------
has_bat_cols <- all(c(
  "PA", "AB", "H", "HR", "BB", "SO", "X2B.1", "X3B.1"
) %in% names(raw_card))

if (has_bat_cols) {
  # Ensure HP (HBP) + IBB exist so downstream calcs never choke
  if (!"HP"  %in% names(raw_card)) raw_card$HP  <- 0
  if (!"IBB" %in% names(raw_card)) raw_card$IBB <- 0
  
  bat_by_type <- raw_card %>%
    dplyr::group_by(tourney_type) %>%
    dplyr::summarise(
      PA  = sum(PA,     na.rm = TRUE),
      AB  = sum(AB,     na.rm = TRUE),
      H   = sum(H,      na.rm = TRUE),
      HR  = sum(HR,     na.rm = TRUE),
      BB  = sum(BB,     na.rm = TRUE),
      IBB = sum(IBB,    na.rm = TRUE),
      SO  = sum(SO,     na.rm = TRUE),
      HP  = sum(HP,     na.rm = TRUE),
      X2B = sum(X2B.1,  na.rm = TRUE),
      X3B = sum(X3B.1,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    add_hitter_rates()
  
  # Attach wOBA weights for each tournament_type (similar to hitter_universe)
  ww_all <- woba_weights()
  if (!is.null(ww_all) && nrow(ww_all) > 0 && "tourney_type" %in% names(bat_by_type)) {
    bat_by_type <- bat_by_type %>%
      dplyr::left_join(ww_all, by = "tourney_type") %>%
      dplyr::mutate(
        singles = pmax(H - X2B - X3B - HR, 0),
        BB_nib  = pmax(BB - IBB, 0),
        woba_num = wBB  * BB_nib +
          wHBP * HP +
          w1B  * singles +
          w2B  * X2B +
          w3B  * X3B +
          wHR  * HR,
        wOBA = dplyr::if_else(PA > 0, woba_num / PA, NA_real_)
      )
  } else {
    # If no weights, at least have a wOBA column filled with NA
    if (!"wOBA" %in% names(bat_by_type)) {
      bat_by_type$wOBA <- NA_real_
    }
  }
  
  # Final display: align with HITTER_DISPLAY_COLS, but grouped by tournament type
  bat_by_type <- bat_by_type %>%
    dplyr::select(
      `Tournament type` = tourney_type,
      PA,
      AVG, OBP, SLG, wOBA,
      BBpct, SOpct, HRper600, BABIP
    ) %>%
    dplyr::arrange(dplyr::desc(PA))
  
  output$player_bat_by_type <- DT::renderDT({
    DT::datatable(
      bat_by_type,
      rownames = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE)
    ) %>%
      DT::formatRound(
        columns = c("AVG", "OBP", "SLG", "wOBA",
                    "BBpct", "SOpct", "HRper600", "BABIP"),
        digits  = 3
      )
  })
    
    ## ----------------- PITCHING: same rates as other pages -----------------
    has_pit_cols <- all(c(
      "G.1", "GS.1", "IP", "R.1", "ER",
      "BB.1", "IBB.1", "HP.1",
      "AB.1", "SF.1",
      "K", "HR.1",
      "GB", "FB.1",
      "X1B.2", "X2B.2", "X3B.2"
    ) %in% names(raw_card))
    
    if (has_pit_cols) {
      pit_by_type <- raw_card %>%
        dplyr::group_by(tourney_type) %>%
        dplyr::summarise(
          G   = sum(G.1,    na.rm = TRUE),
          GS  = sum(GS.1,   na.rm = TRUE),
          IP  = round(sum(IP, na.rm = TRUE), 1),
          R   = sum(R.1,    na.rm = TRUE),
          ER  = sum(ER,     na.rm = TRUE),
          BB  = sum(BB.1,   na.rm = TRUE),
          IBB = sum(IBB.1,  na.rm = TRUE),
          HBP = sum(HP.1,   na.rm = TRUE),
          AB  = sum(AB.1,   na.rm = TRUE),
          SF  = sum(SF.1,   na.rm = TRUE),
          K   = sum(K,      na.rm = TRUE),
          HR  = sum(HR.1,   na.rm = TRUE),
          GB  = sum(GB,     na.rm = TRUE),
          FB  = sum(FB.1,   na.rm = TRUE),
          X1B = sum(X1B.2,  na.rm = TRUE),
          X2B = sum(X2B.2,  na.rm = TRUE),
          X3B = sum(X3B.2,  na.rm = TRUE),
          .groups = "drop"
        ) %>%
        add_pitcher_rates() %>%
        # match PITCHER_DISPLAY_COLS:
        # IP, GSper, IPpergame, RA9, ERA, K9, BB9, HR9, BABIP, GBper
        dplyr::select(
          `Tournament type` = tourney_type,
          IP,
          GSper, IPpergame, RA9, ERA,
          K9, BB9, HR9, BABIP, GBper
        ) %>%
        dplyr::arrange(dplyr::desc(IP))
      
      output$player_pit_by_type <- DT::renderDT({
        DT::datatable(
          pit_by_type,
          rownames = FALSE,
          options  = list(pageLength = 15, scrollX = TRUE)
        ) %>%
          DT::formatRound(
            columns = c("GSper", "IPpergame", "RA9", "ERA",
                        "K9", "BB9", "HR9", "BABIP", "GBper"),
            digits  = 3
          )
      })
    } else {
      output$player_pit_by_type <- DT::renderDT({
        DT::datatable(
          data.frame(Message = "No pitching stats for this card."),
          rownames = FALSE,
          options  = list(dom = "t")
        )
      })
    }
}
}
  output$data_status <- renderUI({
    df <- all_perf()
    
    if (!file.exists(TEAM_RDS)) {
      return(tags$div(
        style = "color: #b00;",
        "Missing file: tournament_data/team_perf_base.rds. Run targets::tar_make() to generate it."
      ))
    }
    if (nrow(df) == 0) {
      return(tags$div(
        style = "color: #b00;",
        "No rows found. Add one or more CSVs to tournament_data/ and re-run tar_make()."
      ))
    }
    tags$div(
      class = "small text-muted",
      paste0("Loaded ", format(nrow(df), big.mark = ","), " rows from team_perf_base.rds")
    )
  })
  
  output$tourney_type_ui <- renderUI({
    df <- raw_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    if (!"type" %in% names(df)) {
      return(tags$div(
        style = "color:#b00;",
        "Column 'type' not found in all_perf_raw. Make sure tourn_details.csv has 'varname' and 'type', and rerun tar_make()."
      ))
    }
    
    cats <- df$type
    cats <- cats[!is.na(cats) & cats != ""]
    cats <- sort(unique(cats))
    
    selectInput(
      "tourney_cat",
      "Tournament type",
      choices = cats,
      selected = cats[[1]]
    )
  })
  
  output$tourney_ui <- renderUI({
    df <- raw_df()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    needed <- c("tourney_type", "name")
    miss <- setdiff(needed, names(df))
    if (length(miss) > 0) {
      return(tags$div(
        style = "color:#b00;",
        paste("Missing column(s) in all_perf_raw:", paste(miss, collapse = ", "))
      ))
    }
    
    # Filter by category if selected
    if (!is.null(input$tourney_cat) && "type" %in% names(df)) {
      df <- df %>% filter(type == input$tourney_cat)
    }
    
    # Build label = name (fallback to tourney_type)
    dd <- df %>%
      dplyr::mutate(label = dplyr::coalesce(name, tourney_type)) %>%
      dplyr::distinct(tourney_type, label) %>%
      dplyr::arrange(label)
    
    if (nrow(dd) == 0) {
      return(tags$div(style = "color:#b00;", "No tournaments match this tournament type."))
    }
    
    choices <- stats::setNames(dd$tourney_type, dd$label)
    
    selectInput(
      "tourney_key",
      "Tournament",
      choices = choices,
      selected = dd$tourney_type[[1]]
    )
  })
  
  team_base <- reactive({
    df <- all_perf()    # still team_perf_base_df
    req(nrow(df) > 0)
    req("tourney_type" %in% names(df), "ORG" %in% names(df))
    
    req(input$tourney_key)
    
    # Filter team_perf_base to the selected tournament key
    df <- df %>% dplyr::filter(tourney_type == input$tourney_key)
    
    # Now df is per-team, per-instance for only that tournament_type
    df
  })
  
  team_universe <- reactive({
    df <- all_perf()
    req(nrow(df) > 0)
    
    # Optional: keep file filter (if you want "dataset" to mean "current file selection")
    if (!is.null(input$tourney_cat) && "type" %in% names(df)) {
      df <- df %>% filter(type == input$tourney_cat)
    }
    
    # Optional: keep the start_date filter
    sdate <- input$start_date
    ok_date <- !is.null(sdate) && length(sdate) == 1 && !is.na(sdate) &&
      (inherits(sdate, "Date") || (is.character(sdate) && sdate != ""))
    if (ok_date && "exptime" %in% names(df)) {
      df <- df %>% dplyr::filter(!is.na(exptime) & as.Date(exptime) >= as.Date(sdate))
    }
    
    df
  })
  tourney_raw <- reactive({
    df <- raw_df()
    req(!is.null(df), nrow(df) > 0)
    
    # Filter by category if selected (same as in output$tourney_type_ui)
    if (!is.null(input$tourney_cat) && "type" %in% names(df)) {
      df <- df %>% dplyr::filter(type == input$tourney_cat)
    }
    
    # Filter to selected tournament key if present
    if (!is.null(input$tourney_key) &&
        "tourney_type" %in% names(df)) {
      df <- df %>% dplyr::filter(tourney_type == input$tourney_key)
    }
    
    # Optional start date filter
    sdate <- input$start_date
    ok_date <- !is.null(sdate) && length(sdate) == 1 && !is.na(sdate) &&
      (inherits(sdate, "Date") || (is.character(sdate) && sdate != ""))
    if (ok_date && "exptime" %in% names(df)) {
      df <- df %>%
        dplyr::filter(!is.na(exptime) & as.Date(exptime) >= as.Date(sdate))
    }
    
    df <- ensure_cid(df)   # <--- add this
    
    df
  })
  
  # Run environment / per-PA summary for the currently selected tournament
  env_summary <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    
    sums <- df %>%
      dplyr::summarise(
        PAsum = sum(PA, na.rm = TRUE),
        ABsum = sum(AB, na.rm = TRUE),
        Hsum  = sum(H,  na.rm = TRUE),
        HRsum = sum(HR, na.rm = TRUE),
        BBsum = sum(BB, na.rm = TRUE),
        SOsum = sum(SO, na.rm = TRUE),
        Rsum  = sum(R,  na.rm = TRUE),
        IPsum = sum(IP, na.rm = TRUE),
        .groups = "drop"
      )
    
    with(sums, tibble::tibble(
      R_per_PA  = ifelse(PAsum > 0, Rsum  / PAsum, NA_real_),
      HR_per_PA = ifelse(PAsum > 0, HRsum / PAsum, NA_real_),
      BB_per_PA = ifelse(PAsum > 0, BBsum / PAsum, NA_real_),
      SO_per_PA = ifelse(PAsum > 0, SOsum / PAsum, NA_real_),
      AVG       = ifelse(ABsum > 0, Hsum  / ABsum, NA_real_),
      R_per_9   = ifelse(IPsum > 0, 9 * Rsum / IPsum, NA_real_)
    ))
  })
  
  output$env_summary_ui <- renderUI({
    env <- env_summary()
    if (nrow(env) == 0) return(NULL)
    
    fmt <- function(x) ifelse(is.na(x), "–", sprintf("%.3f", x))
    
    tags$div(
      class = "small",
      tags$strong("Run environment (current tournament):"),
      tags$table(
        class = "table table-condensed table-sm",
        tags$thead(
          tags$tr(
            tags$th("R/PA"),
            tags$th("HR/PA"),
            tags$th("BB/PA"),
            tags$th("SO/PA"),
            tags$th("AVG"),
            tags$th("R/9")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(fmt(env$R_per_PA)),
            tags$td(fmt(env$HR_per_PA)),
            tags$td(fmt(env$BB_per_PA)),
            tags$td(fmt(env$SO_per_PA)),
            tags$td(fmt(env$AVG)),
            tags$td(fmt(env$R_per_9))
          )
        )
      )
    )
  })
  output$woba_weights_ui <- renderUI({
    ww_all <- woba_weights()
    if (is.null(ww_all) || nrow(ww_all) == 0) return(NULL)
    if (is.null(input$tourney_key)) return(NULL)
    
    if (!"tourney_type" %in% names(ww_all)) return(NULL)
    
    ww <- ww_all %>%
      dplyr::filter(tourney_type == input$tourney_key)
    
    if (nrow(ww) == 0) return(NULL)
    ww <- ww[1, , drop = FALSE]
    
    cols <- c("wBB", "wHBP", "w1B", "w2B", "w3B", "wHR", "scale", "runenv")
    cols <- intersect(cols, names(ww))
    
    fmt <- function(x) ifelse(is.na(x), "–", sprintf("%.3f", as.numeric(x)))
    
    tags$div(
      class = "small",
      tags$strong("wOBA weights (current tournament):"),
      tags$table(
        class = "table table-condensed table-sm",
        tags$thead(tags$tr(lapply(cols, tags$th))),
        tags$tbody(
          tags$tr(lapply(ww[1, cols], function(v) tags$td(fmt(v))))
        )
      )
    )
  })
  # ---- Hitter rating + wOBA weight summary for current tournament ---------
  # ---- Hitter environment summary (match old league_env_summaries) --------
  hitter_env_summary <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    if (!"PA" %in% names(df)) return(tibble::tibble())
    
    total_pa <- sum(df$PA, na.rm = TRUE)
    if (!is.finite(total_pa) || total_pa <= 0) return(tibble::tibble())
    
    ## --- pitcher handedness from T/IP (righty_pct_p, lefty_pct_p) ---
    if (all(c("T", "IP") %in% names(df))) {
      ph <- df %>%
        dplyr::group_by(T) %>%
        dplyr::summarise(IP = sum(IP, na.rm = TRUE), .groups = "drop")
      
      total_ip <- sum(ph$IP, na.rm = TRUE)
      
      left_p  <- ph$IP[ph$T == "L"] / total_ip
      right_p <- ph$IP[ph$T == "R"] / total_ip
      
      left_p  <- ifelse(is.na(left_p),  0, left_p)
      right_p <- ifelse(is.na(right_p), 0, right_p)
    } else {
      left_p  <- NA_real_
      right_p <- NA_real_
    }
    
    ## --- average hitting ratings per PA (POW/EYE/K.s/BABIP/GAP) ---
    rating_names_b <- c("POW", "EYE", "K.s", "BABIP", "GAP")
    
    rating_avgs_b <- df %>%
      dplyr::filter(PA > 0) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(rating_names_b),
          ~ sum(.x * PA, na.rm = TRUE) / sum(PA, na.rm = TRUE),
          .names = "{.col}_pa_avg"
        )
      )
    
    # start with pitcher-handedness columns, then join rating averages
    tibble::tibble(
      righty_pct_p = right_p,
      lefty_pct_p  = left_p
    ) %>%
      dplyr::bind_cols(rating_avgs_b)
  })
  
  
  
  # ---- Tournament-level stat summary: slash line, wOBA, %s ----
  tourney_stat_summary <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    
    # basic totals
    sums <- df %>%
      dplyr::summarise(
        PA   = sum(PA,   na.rm = TRUE),
        AB   = sum(AB,   na.rm = TRUE),
        H    = sum(H,    na.rm = TRUE),
        BB   = sum(BB,   na.rm = TRUE),
        IBB  = sum(IBB,  na.rm = TRUE),
        HBP  = sum(HP,   na.rm = TRUE),
        SO   = sum(SO,   na.rm = TRUE),
        HR   = sum(HR,   na.rm = TRUE),
        X1B  = dplyr::if_else("X1B.1" %in% names(df),
                              sum(X1B.1, na.rm = TRUE),
                              sum(H, na.rm = TRUE) -
                                sum(X2B.1, na.rm = TRUE) -
                                sum(X3B.1, na.rm = TRUE) -
                                sum(HR,    na.rm = TRUE)),
        X2B  = sum(X2B.1, na.rm = TRUE),
        X3B  = sum(X3B.1, na.rm = TRUE),
        SF   = if ("SF" %in% names(df)) sum(SF, na.rm = TRUE) else 0,
        .groups = "drop"
      )
    
    PA  <- sums$PA
    AB  <- sums$AB
    H   <- sums$H
    BB  <- sums$BB
    IBB <- sums$IBB
    HBP <- sums$HBP
    SO  <- sums$SO
    HR  <- sums$HR
    X1B <- sums$X1B
    X2B <- sums$X2B
    X3B <- sums$X3B
    SF  <- sums$SF
    
    # slash line
    AVG <- ifelse(AB > 0, H / AB, NA_real_)
    OBP <- {
      denom <- AB + BB + HBP + SF
      num   <- H + BB + HBP
      ifelse(denom > 0, num / denom, NA_real_)
    }
    SLG <- {
      tb <- 1 * X1B + 2 * X2B + 3 * X3B + 4 * HR
      ifelse(AB > 0, tb / AB, NA_real_)
    }
    
    # BABIP
    BABIP <- {
      denom <- AB - SO - HR + SF
      num   <- H - HR
      ifelse(denom > 0, num / denom, NA_real_)
    }
    
    # XBH%
    XBH_pct <- ifelse(PA > 0, (X2B + X3B + HR) / PA, NA_real_)
    
    # BB%, SO%, HR%
    BB_pct <- ifelse(PA > 0, BB / PA, NA_real_)
    SO_pct <- ifelse(PA > 0, SO / PA, NA_real_)
    HR_pct <- ifelse(PA > 0, HR / PA, NA_real_)
    
    # wOBA using league weights, if available
    wt_all <- woba_weights()
    woba_val <- NA_real_
    if (!is.null(wt_all) &&
        nrow(wt_all) > 0 &&
        "tourney_type" %in% names(df)) {
      
      tt_idx <- which(!is.na(df$tourney_type))[1]
      if (length(tt_idx) == 1 && !is.na(tt_idx)) {
        tt <- df$tourney_type[tt_idx]
        wt <- wt_all %>% dplyr::filter(tourney_type == tt)
        
        if (nrow(wt) == 1 && PA > 0) {
          singles <- X1B
          BB_nib  <- pmax(BB - IBB, 0)
          num <- wt$wBB  * BB_nib +
            wt$wHBP * HBP    +
            wt$w1B  * singles +
            wt$w2B  * X2B    +
            wt$w3B  * X3B    +
            wt$wHR  * HR
          woba_val <- num / PA
        }
      }
    }
    
    tibble::tibble(
      AVG     = AVG,
      OBP     = OBP,
      SLG     = SLG,
      wOBA    = woba_val,
      BB_pct  = BB_pct,
      SO_pct  = SO_pct,
      HR_pct  = HR_pct,
      BABIP   = BABIP,
      XBH_pct = XBH_pct
    )
  })
  
  # ---- Tournament-level stat summary by batter handedness (L/R/S) ----
  tourney_stat_by_hand <- reactive({
    df <- tourney_raw()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    if (!"B" %in% names(df)) return(NULL)   # need batter handedness
    
    # Only rows with valid B and PA
    df <- df %>%
      dplyr::filter(!is.na(B), B %in% c("L", "R", "S"), PA > 0)
    if (nrow(df) == 0) return(NULL)
    
    # Get wOBA weights for this tournament_type (same logic as overall)
    wt_all <- woba_weights()
    wt <- NULL
    if (!is.null(wt_all) &&
        nrow(wt_all) > 0 &&
        "tourney_type" %in% names(df)) {
      tt_idx <- which(!is.na(df$tourney_type))[1]
      if (length(tt_idx) == 1 && !is.na(tt_idx)) {
        tt <- df$tourney_type[tt_idx]
        tmp <- wt_all %>% dplyr::filter(tourney_type == tt)
        if (nrow(tmp) == 1) wt <- tmp
      }
    }
    
    # helper to compute all stats for a subset (one handedness)
    compute_stats <- function(sub) {
      if (is.null(sub) || nrow(sub) == 0) {
        return(list(
          AVG = NA_real_, OBP = NA_real_, SLG = NA_real_, wOBA = NA_real_,
          BB_pct = NA_real_, SO_pct = NA_real_, HR_pct = NA_real_,
          BABIP = NA_real_, XBH_pct = NA_real_
        ))
      }
      
      sums <- sub %>%
        dplyr::summarise(
          PA   = sum(PA,   na.rm = TRUE),
          AB   = sum(AB,   na.rm = TRUE),
          H    = sum(H,    na.rm = TRUE),
          BB   = sum(BB,   na.rm = TRUE),
          IBB  = sum(IBB,  na.rm = TRUE),
          HBP  = sum(HP,   na.rm = TRUE),
          SO   = sum(SO,   na.rm = TRUE),
          HR   = sum(HR,   na.rm = TRUE),
          X1B  = dplyr::if_else("X1B.1" %in% names(sub),
                                sum(X1B.1, na.rm = TRUE),
                                sum(H, na.rm = TRUE) -
                                  sum(X2B.1, na.rm = TRUE) -
                                  sum(X3B.1, na.rm = TRUE) -
                                  sum(HR,    na.rm = TRUE)),
          X2B  = sum(X2B.1, na.rm = TRUE),
          X3B  = sum(X3B.1, na.rm = TRUE),
          SF   = if ("SF" %in% names(sub)) sum(SF, na.rm = TRUE) else 0,
          .groups = "drop"
        )
      
      PA  <- sums$PA
      AB  <- sums$AB
      H   <- sums$H
      BB  <- sums$BB
      IBB <- sums$IBB
      HBP <- sums$HBP
      SO  <- sums$SO
      HR  <- sums$HR
      X1B <- sums$X1B
      X2B <- sums$X2B
      X3B <- sums$X3B
      SF  <- sums$SF
      
      AVG <- ifelse(AB > 0, H / AB, NA_real_)
      OBP <- {
        denom <- AB + BB + HBP + SF
        num   <- H + BB + HBP
        ifelse(denom > 0, num / denom, NA_real_)
      }
      SLG <- {
        tb <- 1 * X1B + 2 * X2B + 3 * X3B + 4 * HR
        ifelse(AB > 0, tb / AB, NA_real_)
      }
      BABIP <- {
        denom <- AB - SO - HR + SF
        num   <- H - HR
        ifelse(denom > 0, num / denom, NA_real_)
      }
      XBH_pct <- ifelse(PA > 0, (X2B + X3B + HR) / PA, NA_real_)
      BB_pct  <- ifelse(PA > 0, BB / PA, NA_real_)
      SO_pct  <- ifelse(PA > 0, SO / PA, NA_real_)
      HR_pct  <- ifelse(PA > 0, HR / PA, NA_real_)
      
      woba_val <- NA_real_
      if (!is.null(wt) && PA > 0) {
        singles <- X1B
        BB_nib  <- pmax(BB - IBB, 0)
        num <- wt$wBB  * BB_nib +
          wt$wHBP * HBP    +
          wt$w1B  * singles +
          wt$w2B  * X2B    +
          wt$w3B  * X3B    +
          wt$wHR  * HR
        woba_val <- num / PA
      }
      
      list(
        AVG     = AVG,
        OBP     = OBP,
        SLG     = SLG,
        wOBA    = woba_val,
        BB_pct  = BB_pct,
        SO_pct  = SO_pct,
        HR_pct  = HR_pct,
        BABIP   = BABIP,
        XBH_pct = XBH_pct
      )
    }
    
    # compute for each handedness
    res_L <- compute_stats(df %>% dplyr::filter(B == "L"))
    res_R <- compute_stats(df %>% dplyr::filter(B == "R"))
    res_S <- compute_stats(df %>% dplyr::filter(B == "S"))
    
    metrics <- c("AVG", "OBP", "SLG", "wOBA",
                 "BB_pct", "SO_pct", "HR_pct", "BABIP", "XBH_pct")
    
    out <- list()
    for (m in metrics) {
      out[[m]] <- c(
        L = res_L[[m]],
        R = res_R[[m]],
        S = res_S[[m]]
      )
    }
    
    out
  })
  
  # ---- Pitcher vR/vL rating summary (IP-weighted, with R/L breakdown) ----
  pitcher_tourney_rating_summary <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    if (!"IP" %in% names(df)) return(NULL)
    
    df <- df %>% dplyr::filter(IP > 0)
    if (nrow(df) == 0) return(NULL)
    
    # Candidate pitcher rating columns (adjust as needed to match your data)
    rating_vR <- c("STU.vR", "CON.vR.1", "HRR.vR", "PBABIP.vR")
    rating_vL <- c("STU.vL", "CON.vL.1", "HRR.vL", "PBABIP.vL")
    
    cols_present <- intersect(c(rating_vR, rating_vL), names(df))
    if (length(cols_present) == 0) {
      return(NULL)
    }
    
    # Overall IP-weighted averages for each rating column
    base <- df %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(cols_present),
          ~ sum(.x * IP, na.rm = TRUE) / sum(IP, na.rm = TRUE),
          .names = "{.col}"
        )
      )
    
    # Interleave vL/vR pairs in a nice order
    pair_order <- c(
      "STU.vL","STU.vR",
      "CON.vL.1","CON.vR.1",
      "HRR.vL","HRR.vR",
      "PBABIP.vL","PBABIP.vR"
    )
    keep <- intersect(pair_order, names(base))
    ratings_row <- base[, keep, drop = FALSE]
    
    # ---- Breakout for tooltips: R vs L pitchers (IP-weighted) ----
    # Need pitcher handedness column T
    ratings_breakdown <- NULL
    if (ncol(ratings_row) > 0 && "T" %in% names(df)) {
      cols <- names(ratings_row)
      ratings_breakdown <- lapply(cols, function(cc) {
        # Right-handed pitchers
        df_R <- df %>% dplyr::filter(IP > 0, T == "R")
        # Left-handed pitchers
        df_L <- df %>% dplyr::filter(IP > 0, T == "L")
        
        avg_R <- if (nrow(df_R) > 0) {
          sum(df_R[[cc]] * df_R$IP, na.rm = TRUE) / sum(df_R$IP, na.rm = TRUE)
        } else NA_real_
        
        avg_L <- if (nrow(df_L) > 0) {
          sum(df_L[[cc]] * df_L$IP, na.rm = TRUE) / sum(df_L$IP, na.rm = TRUE)
        } else NA_real_
        
        c(R = avg_R, L = avg_L)
      })
      names(ratings_breakdown) <- cols
    }
    
    list(
      ratings_row       = ratings_row,
      ratings_breakdown = ratings_breakdown
    )
  })
  
  output$pitcher_tourney_summary_ui <- renderUI({
    stats      <- tourney_stat_summary()   # reuse the same environment stats
    stat_hand  <- tourney_stat_by_hand()   # same L/R/S breakdown as hitters
    pit_rates  <- pitcher_tourney_rating_summary()
    
    if (nrow(stats) == 0) return(NULL)
    
    fmt3 <- function(x) ifelse(is.na(x), "–", sprintf("%.3f", as.numeric(x)))
    fmt_pct <- function(x) ifelse(is.na(x), "–", sprintf("%.1f%%", 100 * as.numeric(x)))
    
    # helper: build stat cell with optional L/R/S tooltip (same idea as hitters)
    make_pitcher_stat_cell <- function(metric, is_pct = FALSE) {
      main_val <- stats[[metric]]
      main_txt <- if (is_pct) fmt_pct(main_val) else fmt3(main_val)
      
      if (is.null(stat_hand) || is.null(stat_hand[[metric]])) {
        return(tags$td(main_txt))
      }
      
      vals <- stat_hand[[metric]]
      L <- vals[["L"]]
      R <- vals[["R"]]
      S <- vals[["S"]]
      
      fmt_val <- if (is_pct) fmt_pct else fmt3
      
      tooltip <- paste0(
        "L: ", fmt_val(L),
        " | R: ", fmt_val(R),
        " | S: ", fmt_val(S)
      )
      
      tags$td(main_txt, title = tooltip)
    }
    
    # Line 1: same stat bar as hitters, but independent code path
    line1 <- tags$table(
      class = "table table-condensed table-sm",
      tags$thead(
        tags$tr(
          tags$th("AVG"),
          tags$th("OBP"),
          tags$th("SLG"),
          tags$th("wOBA"),
          tags$th("BB%"),
          tags$th("SO%"),
          tags$th("HR%"),
          tags$th("BABIP"),
          tags$th("XBH%")
        )
      ),
      tags$tbody(
        tags$tr(
          make_pitcher_stat_cell("AVG",     is_pct = FALSE),
          make_pitcher_stat_cell("OBP",     is_pct = FALSE),
          make_pitcher_stat_cell("SLG",     is_pct = FALSE),
          make_pitcher_stat_cell("wOBA",    is_pct = FALSE),
          make_pitcher_stat_cell("BB_pct",  is_pct = TRUE),
          make_pitcher_stat_cell("SO_pct",  is_pct = TRUE),
          make_pitcher_stat_cell("HR_pct",  is_pct = TRUE),
          make_pitcher_stat_cell("BABIP",   is_pct = FALSE),
          make_pitcher_stat_cell("XBH_pct", is_pct = TRUE)
        )
      )
    )
    
    # Line 2: pitcher vR/vL rating bar (IP-weighted, with hover breakdown)
    ratings_tbl <- NULL
    if (!is.null(pit_rates) &&
        !is.null(pit_rates$ratings_row) &&
        ncol(pit_rates$ratings_row) > 0) {
      
      rr     <- pit_rates$ratings_row
      bd_all <- pit_rates$ratings_breakdown
      
      # Map actual column names -> pretty labels
      nice_names <- c(
        "STU.vL"    = "STU vL",
        "STU.vR"    = "STU vR",
        "CON.vL.1"  = "CON vL",
        "CON.vR.1"  = "CON vR",
        "HRR.vL"     = "HRA vL",
        "HRR.vR"     = "HRA vR",
        "PBABIP.vL" = "PBABIP vL",
        "PBABIP.vR" = "PBABIP vR"
      )
      
      headers <- sapply(names(rr), function(nm) {
        val <- nice_names[nm]
        if (length(val) == 0 || is.na(val)) nm else val
      })
      
      vals_fmt <- lapply(rr[1, , drop = TRUE], fmt3)
      
      # helper for tooltips: R vs L pitchers
      fmt1 <- function(x) ifelse(is.na(x), "–", sprintf("%.1f", as.numeric(x)))
      
      cells <- lapply(seq_along(vals_fmt), function(i) {
        colname  <- names(rr)[i]
        cell_txt <- as.character(vals_fmt[[i]])
        
        bd <- NULL
        if (!is.null(bd_all) && !is.null(bd_all[[colname]])) {
          bd <- bd_all[[colname]]
        }
        
        if (is.null(bd)) {
          tags$td(cell_txt)
        } else {
          tooltip <- paste0(
            "R pitchers: ", fmt1(bd["R"]),
            " | L pitchers: ", fmt1(bd["L"])
          )
          tags$td(cell_txt, title = tooltip)
        }
      })
      
      ratings_tbl <- tags$table(
        class = "table table-condensed table-sm",
        tags$thead(
          tags$tr(lapply(headers, tags$th))
        ),
        tags$tbody(
          tags$tr(cells)
        )
      )
    }
    
    tags$div(
      class = "small",
      tags$strong(paste0("Pitching summary — ", input$tourney_key %||% "")),
      tags$br(),
      line1,
      if (!is.null(ratings_tbl)) {
        tagList(
          tags$strong("Pitcher vR / vL rating averages (IP-weighted):"),
          ratings_tbl
        )
      } else {
        NULL
      },
      tags$hr()
    )
  })
  
  output$pitcher_role_ui <- renderUI({
    radioButtons(
      inputId = "pitcher_role",
      label   = "Pitcher role",
      choices = c("All", "Starters", "Relievers"),
      selected = "All",
      inline  = TRUE
    )
  })
  # ---- Tournament-level handedness + vR/vL rating summary ----
  tourney_rating_summary <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    if (!"PA" %in% names(df)) return(tibble::tibble())
    
    total_pa <- sum(df$PA, na.rm = TRUE)
    if (!is.finite(total_pa) || total_pa <= 0) return(tibble::tibble())
    
    ## ---- Batter handedness (by PA) ----
    if ("B" %in% names(df)) {
      bh <- df %>%
        dplyr::group_by(B) %>%
        dplyr::summarise(PA = sum(PA, na.rm = TRUE), .groups = "drop")
      
      total_pa_b <- sum(bh$PA, na.rm = TRUE)
      left_b   <- bh$PA[bh$B == "L"] / total_pa_b
      right_b  <- bh$PA[bh$B == "R"] / total_pa_b
      switch_b <- bh$PA[bh$B == "S"] / total_pa_b
      
      left_b   <- ifelse(is.na(left_b),   0, left_b)
      right_b  <- ifelse(is.na(right_b),  0, right_b)
      switch_b <- ifelse(is.na(switch_b), 0, switch_b)
    } else {
      left_b <- right_b <- switch_b <- NA_real_
    }
    
    ## ---- Pitcher handedness – starts (GS) and innings (IP) ----
    left_p_start  <- right_p_start <- NA_real_
    left_p_ip     <- right_p_ip    <- NA_real_
    
    # Starts %
    if (all(c("T", "GS.1") %in% names(df))) {
      ps <- df %>%
        dplyr::group_by(T) %>%
        dplyr::summarise(GS = sum(GS.1, na.rm = TRUE), .groups = "drop")
      
      total_gs <- sum(ps$GS, na.rm = TRUE)
      if (total_gs > 0) {
        left_p_start  <- ps$GS[ps$T == "L"] / total_gs
        right_p_start <- ps$GS[ps$T == "R"] / total_gs
      }
    }
    
    # Innings %
    if (all(c("T", "IP") %in% names(df))) {
      ph <- df %>%
        dplyr::group_by(T) %>%
        dplyr::summarise(IP = sum(IP, na.rm = TRUE), .groups = "drop")
      
      total_ip <- sum(ph$IP, na.rm = TRUE)
      if (total_ip > 0) {
        left_p_ip  <- ph$IP[ph$T == "L"] / total_ip
        right_p_ip <- ph$IP[ph$T == "R"] / total_ip
      }
    }
    
    left_p_start  <- ifelse(is.na(left_p_start),  0, left_p_start)
    right_p_start <- ifelse(is.na(right_p_start), 0, right_p_start)
    left_p_ip     <- ifelse(is.na(left_p_ip),     0, left_p_ip)
    right_p_ip    <- ifelse(is.na(right_p_ip),    0, right_p_ip)
    
    ## ---- PA-weighted vR / vL ratings (overall bar) ----
    rating_vR <- c("POW.vR",  "EYE.vR",  "K.vR",   "BA.vR", "GAP.vR")
    rating_vL <- c("POW.vL",  "EYE.vL",  "K.vL",   "BA.vL", "GAP.vL")
    
    cols_present <- intersect(c(rating_vR, rating_vL), names(df))
    ratings_row  <- tibble::tibble()
    
    if (length(cols_present) > 0) {
      base <- df %>%
        dplyr::filter(PA > 0) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(cols_present),
            ~ sum(.x * PA, na.rm = TRUE) / sum(PA, na.rm = TRUE),
            .names = "{.col}"
          )
        )
      
      # Interleave vL / vR pairs in bar order
      pair_order <- c(
        "POW.vL",  "POW.vR",
        "EYE.vL",  "EYE.vR",
        "K.vL",   "K.vR",
        "BA.vL","BA.vR",
        "GAP.vL",  "GAP.vR"
      )
      keep <- intersect(pair_order, names(base))
      ratings_row <- base[, keep, drop = FALSE]
    }
    
    ## ---- Breakout for tooltips: right-side vs left-side averages ----
    # For vR columns:
    #   right-side = R-only; left-side = L + S (switch counts as lefty vs R)
    # For vL columns:
    #   right-side = R + S; left-side = L-only (switch counts as righty vs L)
    ratings_breakdown <- NULL
    if (ncol(ratings_row) > 0 && "B" %in% names(df)) {
      cols <- names(ratings_row)
      ratings_breakdown <- lapply(cols, function(cc) {
        is_vR <- grepl("vR$", cc)
        is_vL <- grepl("vL$", cc)
        if (!(is_vR || is_vL)) return(NULL)
        
        if (is_vR) {
          df_R <- df %>% dplyr::filter(PA > 0, B == "R")
          df_L <- df %>% dplyr::filter(PA > 0, B %in% c("L", "S"))
        } else { # vL
          df_R <- df %>% dplyr::filter(PA > 0, B %in% c("R", "S"))
          df_L <- df %>% dplyr::filter(PA > 0, B == "L")
        }
        
        avg_R <- if (nrow(df_R) > 0) sum(df_R[[cc]] * df_R$PA, na.rm = TRUE) / sum(df_R$PA, na.rm = TRUE) else NA_real_
        avg_L <- if (nrow(df_L) > 0) sum(df_L[[cc]] * df_L$PA, na.rm = TRUE) / sum(df_L$PA, na.rm = TRUE) else NA_real_
        
        c(right_side = avg_R, left_side = avg_L)
      })
      names(ratings_breakdown) <- cols
    }
    
    list(
      lefty_pct_b         = left_b,
      righty_pct_b        = right_b,
      switch_pct_b        = switch_b,
      lefty_start_pct_p   = left_p_start,
      righty_start_pct_p  = right_p_start,
      lefty_ip_pct_p      = left_p_ip,
      righty_ip_pct_p     = right_p_ip,
      ratings_row         = ratings_row,
      ratings_breakdown   = ratings_breakdown
    )
  })
  
  output$tourney_summary_ui <- renderUI({
    stats    <- tourney_stat_summary()
    rat      <- tourney_rating_summary()
    stat_hand <- tourney_stat_by_hand()
    
    if (nrow(stats) == 0) return(NULL)
    
    fmt3 <- function(x) ifelse(is.na(x), "–", sprintf("%.3f", as.numeric(x)))
    fmt_pct <- function(x) ifelse(is.na(x), "–", sprintf("%.1f%%", 100 * as.numeric(x)))
    
    # helper to build a stat cell with optional L/R/S breakdown tooltip
    make_stat_cell <- function(metric, is_pct = FALSE) {
      main_val <- stats[[metric]]
      main_txt <- if (is_pct) fmt_pct(main_val) else fmt3(main_val)
      
      # if no breakdown info, just return plain cell
      if (is.null(stat_hand) || is.null(stat_hand[[metric]])) {
        return(tags$td(main_txt))
      }
      
      vals <- stat_hand[[metric]]
      # ensure we have L/R/S names
      L <- vals[["L"]]; R <- vals[["R"]]; S <- vals[["S"]]
      fmt_val <- if (is_pct) fmt_pct else fmt3
      
      tooltip <- paste0(
        "L: ", fmt_val(L),
        " | R: ", fmt_val(R),
        " | S: ", fmt_val(S)
      )
      
      tags$td(main_txt, title = tooltip)
    }
    # Line 1: slash line + wOBA + %s
    # Line 1: slash line + wOBA + %s
    line1 <- tags$table(
      class = "table table-condensed table-sm",
      tags$thead(
        tags$tr(
          tags$th("AVG"),
          tags$th("OBP"),
          tags$th("SLG"),
          tags$th("wOBA"),
          tags$th("BB%"),
          tags$th("SO%"),
          tags$th("HR%"),
          tags$th("BABIP"),
          tags$th("XBH%")
        )
      ),
      tags$tbody(
        tags$tr(
          make_stat_cell("AVG",     is_pct = FALSE),
          make_stat_cell("OBP",     is_pct = FALSE),
          make_stat_cell("SLG",     is_pct = FALSE),
          make_stat_cell("wOBA",    is_pct = FALSE),
          make_stat_cell("BB_pct",  is_pct = TRUE),
          make_stat_cell("SO_pct",  is_pct = TRUE),
          make_stat_cell("HR_pct",  is_pct = TRUE),
          make_stat_cell("BABIP",   is_pct = FALSE),
          make_stat_cell("XBH_pct", is_pct = TRUE)
        )
      )
    )
    
    # Line 2: handedness + vR/vL ratings
    handedness_row <- tags$tr(
      tags$td(
        tags$strong("Batters: "),
        sprintf("L %s, R %s, S %s",
                fmt_pct(rat$lefty_pct_b),
                fmt_pct(rat$righty_pct_b),
                fmt_pct(rat$switch_pct_b))
      ),
      tags$td(
        tags$strong("Pitchers: "),
        sprintf(
          "Starts – L %s, R %s; IP – L %s, R %s",
          fmt_pct(rat$lefty_start_pct_p),
          fmt_pct(rat$righty_start_pct_p),
          fmt_pct(rat$lefty_ip_pct_p),
          fmt_pct(rat$righty_ip_pct_p)
        )
      )
    )
    
    ratings_tbl <- NULL
    if (!is.null(rat$ratings_row) && ncol(rat$ratings_row) > 0) {
      rr <- rat$ratings_row
      
      # Pretty column labels
      nice_names <- c(
        "POW.vL"   = "POW vL",
        "POW.vR"   = "POW vR",
        "EYE.vL"   = "EYE vL",
        "EYE.vR"   = "EYE vR",
        "K.vL"  = "K vL",
        "K.vR"  = "K vR",
        "BA.vL" = "BABIP vL",
        "BA.vR" = "BABIP vR",
        "GAP.vL"   = "GAP vL",
        "GAP.vR"   = "GAP vR"
      )
      headers <- sapply(names(rr), function(nm) nice_names[[nm]] %||% nm)
      
      vals_fmt <- lapply(rr[1, , drop = TRUE], fmt3)
      
      cells <- lapply(seq_along(vals_fmt), function(i) {
        colname  <- names(rr)[i]
        cell_txt <- as.character(vals_fmt[[i]])
        
        bd <- NULL
        if (!is.null(rat$ratings_breakdown) &&
            !is.null(rat$ratings_breakdown[[colname]])) {
          bd <- rat$ratings_breakdown[[colname]]
        }
        
        if (is.null(bd)) {
          tags$td(cell_txt)
        } else {
          fmt1 <- function(x) ifelse(is.na(x), "–", sprintf("%.1f", as.numeric(x)))
          tooltip <- paste0(
            "Right-handed side: ", fmt1(bd["right_side"]),
            " | Left-handed side: ", fmt1(bd["left_side"]),
            "\n(Switch hitters counted as lefty for vR, righty for vL.)"
          )
          tags$td(cell_txt, title = tooltip)
        }
      })
      
      ratings_tbl <- tags$table(
        class = "table table-condensed table-sm",
        tags$thead(
          tags$tr(lapply(headers, tags$th))
        ),
        tags$tbody(
          tags$tr(cells)
        )
      )
    }
      
    tags$div(
      class = "small",
      tags$strong(paste0("Tournament summary — ", input$tourney_key %||% "")),
      tags$br(),
      line1,
      tags$table(
        class = "table table-condensed table-sm",
        tags$tbody(handedness_row)
      ),
      if (!is.null(ratings_tbl)) {
        tagList(
          tags$strong("vR / vL rating averages:"),
          ratings_tbl
        )
      } else {
        NULL
      },
      tags$hr()
    )
  })
  
  # Canonical hitter position order + mapping to Pot columns
  HITTER_POS_MAP <- tibble::tibble(
    label = c("C", "1B", "2B", "3B", "SS", "LF", "CF", "RF"),
    pot   = c(
      "C.Pot",
      "X1B.Pot",
      "X2B.Pot",
      "X3B.Pot",
      "SS.Pot",
      "LF.Pot",
      "CF.Pot",
      "RF.Pot"
    )
  )
  
  output$hitter_pos_ui <- renderUI({
    df <- tourney_raw()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Keep only positions where at least one card has Pot > 0
    available <- HITTER_POS_MAP %>%
      dplyr::filter(pot %in% names(df)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        has_any = any(!is.na(df[[pot]]) & df[[pot]] > 0)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(has_any)
    
    if (nrow(available) == 0) return(NULL)
    
    checkboxGroupInput(
      inputId = "hitter_pos",
      label   = "Hitter positions",
      choices = available$label,
      selected = character(0),
      inline  = TRUE
    )
  })
  
  output$hitter_min_pa_ui <- renderUI({
    df <- tourney_raw()
    if (!"PA" %in% names(df) || nrow(df) == 0) return(NULL)
    
    max_pa <- max(df$PA, na.rm = TRUE)
    max_pa <- ifelse(is.finite(max_pa), max_pa, 0)
    
    numericInput(
      "hitter_min_pa",
      "Minimum PA",
      value = 30,
      min   = 0,
      max   = max(0, max_pa),
      step  = 10
    )
  })
 
  output$pitcher_min_ip_ui <- renderUI({
    df <- tourney_raw()
    if (!"IP" %in% names(df) || nrow(df) == 0) return(NULL)
    
    max_ip <- max(df$IP, na.rm = TRUE)
    max_ip <- ifelse(is.finite(max_ip), max_ip, 0)
    
    numericInput(
      "pitcher_min_ip",
      "Minimum IP",
      value = 10,
      min   = 0,
      max   = max(0, max_ip),
      step  = 5
    )
  })
  # store for click-to-player
  overall_bat_rv <- reactiveVal(NULL)
  overall_pit_rv <- reactiveVal(NULL)
  hitter_universe <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    
    # Only rows with any PA
    df <- df %>% dplyr::filter(!is.na(PA) & PA > 0)
    req(nrow(df) > 0)
    
    # Position filter using potential columns (multi-position support)
    # Position filter using multiple potential columns
    if (!is.null(input$hitter_pos) && length(input$hitter_pos) > 0) {
      
      # Map selected labels -> pot columns
      pots <- HITTER_POS_MAP %>%
        dplyr::filter(label %in% input$hitter_pos) %>%
        dplyr::pull(pot)
      
      pots <- intersect(pots, names(df))
      
      if (length(pots) > 0) {
        df <- df %>%
          dplyr::filter(
            rowSums(
              dplyr::across(
                dplyr::all_of(pots),
                ~ !is.na(.x) & .x > 0
              )
            ) > 0
          )
      }
    }
    
    req(nrow(df) > 0)
    
    # Aggregate to card level, keeping counts needed for wOBA
    df_sum <- df %>%
      dplyr::group_by(Name, POS, CID, VAL, VLvl, Title, B) %>%   # add B for Hand column
      dplyr::summarise(
        PA   = sum(PA,     na.rm = TRUE),
        AB   = sum(AB,     na.rm = TRUE),
        H    = sum(H,      na.rm = TRUE),
        HR   = sum(HR,     na.rm = TRUE),
        BB   = sum(BB,     na.rm = TRUE),
        IBB  = sum(IBB,    na.rm = TRUE),
        HP   = sum(HP,     na.rm = TRUE),
        SO   = sum(SO,     na.rm = TRUE),
        X1B  = sum(X1B.1,  na.rm = TRUE),
        X2B  = sum(X2B.1,  na.rm = TRUE),
        X3B  = sum(X3B.1,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_hitter_rates()
    
    # Attach wOBA weights for this tournament_type (if available)
    wt_all <- woba_weights()
    if (nrow(wt_all) > 0 && "tourney_type" %in% names(df)) {
      # tourney_raw() is already filtered to one tourney_type
      tt_idx <- which(!is.na(df$tourney_type))[1]
      if (length(tt_idx) == 1 && !is.na(tt_idx)) {
        tt <- df$tourney_type[tt_idx]
        wt <- wt_all %>% dplyr::filter(tourney_type == tt)
        
        if (nrow(wt) == 1) {
          df_sum <- df_sum %>%
            dplyr::mutate(
              singles = pmax(X1B, 0),
              BB_nib  = pmax(BB - IBB, 0),
              woba_num = wt$wBB  * BB_nib +
                wt$wHBP * HP +
                wt$w1B  * singles +
                wt$w2B  * X2B +
                wt$w3B  * X3B +
                wt$wHR  * HR,
              wOBA = dplyr::if_else(PA > 0, woba_num / PA, NA_real_)
            )
        } else {
          df_sum$wOBA <- NA_real_
        }
      } else {
        df_sum$wOBA <- NA_real_
      }
    } else {
      df_sum$wOBA <- NA_real_
    }
    
    # Minimum PA filter at *card* level
    min_pa <- input$hitter_min_pa %||% 0
    df_sum <- df_sum %>%
      dplyr::filter(PA >= min_pa)
    
    # Standardized display (card_title, card_value, VLvl, Hand, etc.)
    df_show <- make_hitter_display(df_sum)
    
    df_show
  })
  render_drilldown_modal <- function(level = modal_level()) {
    if (level == "team") {
      modalDialog(
        title = paste0("Team page: ", selected_org()),
        size = "l",
        easyClose = TRUE,
        tags$h4("Overall"),
        DTOutput("team_modal_overall"),
        tags$hr(),
        tags$h4("By tournament"),
        DTOutput("team_modal_by_tourney"),
        tags$hr(),
        tags$h4("Tournament instances"),
        DTOutput("team_modal_instances")
      )
      
    } else if (level == "tourney") {
      modalDialog(
        title = paste0("Team: ", selected_org(),
                       " — Tournament: ", selected_tourney_label()),
        size = "l",
        easyClose = TRUE,
        actionButton("back_to_team", "⬅ Back to Team"),
        tags$hr(),
        tabsetPanel(
          tabPanel("Batting", DTOutput("tourney_bat_tbl")),
          tabPanel("Pitching", DTOutput("tourney_pit_tbl"))
        ),
        tags$hr(),
        tags$h4("Instances"),
        DTOutput("tourney_instances_tbl")
      )
      
    } else if (level == "instance") {
      modalDialog(
        title = paste0(
          "Team: ", selected_org(),
          " — ", selected_tourney_label(),
          " — Instance"
        ),
        size = "l",
        easyClose = TRUE,
        fluidRow(
          column(6, actionButton("back_to_tourney", "⬅ Back to Tournament")),
          column(6, actionButton("back_to_team", "⬅⬅ Back to Team"))
        ),
        tags$hr(),
        tabsetPanel(
          tabPanel("Batting", DTOutput("inst_bat_tbl")),
          tabPanel("Pitching", DTOutput("inst_pit_tbl"))
        )
      )
      
    } else if (level == "player") {
      modalDialog(
        title = paste0("Player: ", selected_player_name()),
        size = "l",
        easyClose = TRUE,
        fluidRow(
          column(6, actionButton("player_back_to_instance", "⬅ Back to Instance")),
          column(6, actionButton("player_back_to_tourney",  "⬅⬅ Back to Tournament"))
        ),
        tags$hr(),
        tabsetPanel(
          tabPanel("Batting by tournament type",  DTOutput("player_bat_by_type")),
          tabPanel("Pitching by tournament type", DTOutput("player_pit_by_type"))
        )
      )
    }
  }
  output$mintourn_ui <- renderUI({
    df <- team_base()
    if (nrow(df) == 0) return(NULL)
    
    counts <- df %>% group_by(ORG) %>% summarise(n = n(), .groups = "drop")
    max_n <- if (nrow(counts)) max(counts$n, na.rm = TRUE) else 5
    med_n <- if (nrow(counts)) median(counts$n, na.rm = TRUE) else 5
    
    sliderInput(
      "mintourn",
      "Minimum tournaments",
      min = 1,
      max = max(5, max_n),
      value = max(1, floor(med_n)),
      step = 1
    )
  })
  
  team_summary <- reactive({
    df <- team_base()
    req(input$mintourn)
    
    # Column checks (keep this tab self-contained)
    needed <- c("Wins", "Losses", "tourneys_won")
    missing <- setdiff(needed, names(df))
    validate(need(length(missing) == 0,
                  paste("Missing required columns:", paste(missing, collapse = ", "))))
    
    team_perf_summary(df, mintourn = input$mintourn)
  })
  
  output$team_data_span <- renderUI({
    df <- team_base()
    if (nrow(df) == 0) return(NULL)
    
    span <- team_perf_date_span(df)
    if (is.null(span)) return(NULL)
    
    tags$div(
      class = "small text-muted",
      paste0("Data coverage: ", format(span$start), " to ", format(span$end))
    )
  })
  
  output$team_perf_tbl <- renderDT({
    df <- team_summary()
    validate(need(nrow(df) > 0, "No teams match the selected filters."))
    
    show <- df %>%
      rename(
        Team = ORG,
        `Win %` = WinPct,
        `Tourneys` = n_tourneys,
        `Tourneys Won` = tourneys_won,
        `Win Tourney %` = tourneys_win_per
      )
    
    datatable(
      show,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE)
    ) %>%
      formatRound(columns = c("Win %", "Win Tourney %"), digits = 3)
  })
  
  observeEvent(input$team_perf_tbl_rows_selected, {
    sel <- input$team_perf_tbl_rows_selected
    req(sel)
    
    sumdf <- team_summary()
    org   <- sumdf$ORG[sel]
    req(org)
    
    selected_org(org)
    show_team_modal()
  })
  observeEvent(input$team_modal_by_tourney_rows_selected, {
    sel <- input$team_modal_by_tourney_rows_selected
    req(sel)
    
    org <- selected_org()
    bt  <- by_tourney_rv()
    req(org, bt)
    req(nrow(bt) >= sel)
    
    tourney_type_sel <- bt$tourney_type[sel]   # KEY
    tourney_label    <- bt$tourney[sel]        # LABEL
    
    selected_tourney(tourney_type_sel)         # store key
    selected_tourney_label(tourney_label)      # store pretty name
    
    raw <- raw_df()
    req(nrow(raw) > 0)
    
    raw_tt <- raw %>%
      dplyr::filter(ORG == org, tourney_type == tourney_type_sel)
    
    validate(need(nrow(raw_tt) > 0, "No raw rows found for this team/tournament."))
    raw_tt <- ensure_cid(raw_tt)   # <--- add this
    # Instance list from the *team-level* data (fast + clean W/L)
    inst_tbl <- team_raw_rv() %>%
      dplyr::filter(tourney_type == tourney_type_sel) %>%   # key filter
      dplyr::arrange(dplyr::desc(exptime)) %>%
      dplyr::mutate(exptime_chr = as.character(exptime)) %>%
      dplyr::transmute(
        exptime = exptime_chr,
        Wins, Losses,
        WinPct = ifelse((Wins + Losses) > 0, round(Wins/(Wins+Losses), 3), NA_real_),
        Won = tourneys_won
      )
    
    tourney_instances_rv(inst_tbl)
    
    # Batting roster (all instances combined)
    bat <- raw_tt %>%
      dplyr::group_by(Name, POS, CID, VAL, VLvl, Title, B) %>%
      dplyr::summarise(
        PA  = sum(PA, na.rm = TRUE),
        AB  = sum(AB, na.rm = TRUE),
        H   = sum(H,  na.rm = TRUE),
        HR  = sum(HR, na.rm = TRUE),
        BB  = sum(BB, na.rm = TRUE),
        SO  = sum(SO, na.rm = TRUE),
        HP  = sum(HP, na.rm = TRUE),
        X2B = sum(X2B.1, na.rm = TRUE),
        X3B = sum(X3B.1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_hitter_rates() %>%
      dplyr::arrange(dplyr::desc(PA))
    
    # Pitching roster (all instances combined) — uses your raw column names
    pit <- raw_tt %>%
      dplyr::group_by(Name, POS, CID, VLvl, Title) %>%
      dplyr::summarise(
        G   = sum(G.1,  na.rm = TRUE),
        GS  = sum(GS.1, na.rm = TRUE),
        IP  = round(sum(IP, na.rm = TRUE), 1),
        
        R   = sum(R.1,   na.rm = TRUE),
        ER  = sum(ER,    na.rm = TRUE),
        BB  = sum(BB.1,  na.rm = TRUE),
        IBB = sum(IBB.1, na.rm = TRUE),
        HBP = sum(HP.1,  na.rm = TRUE),
        AB  = sum(AB.1,  na.rm = TRUE),
        SF  = sum(SF.1,  na.rm = TRUE),
        
        K   = sum(K,     na.rm = TRUE),
        HR  = sum(HR.1,  na.rm = TRUE),
        GB  = sum(GB,    na.rm = TRUE),
        FB  = sum(FB.1,  na.rm = TRUE),
        
        X1B = sum(X1B.2, na.rm = TRUE),
        X2B = sum(X2B.2, na.rm = TRUE),
        X3B = sum(X3B.2, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      add_pitcher_rates() %>%
      dplyr::select(dplyr::all_of(PITCHER_DISPLAY_COLS)) %>%
      dplyr::arrange(dplyr::desc(IP))
    
    # ... after pit is created
    tourney_bat_rv(bat)
    tourney_pit_rv(pit)
    
    modal_level("tourney")
    showModal(render_drilldown_modal())
    
    output$tourney_bat_tbl <- DT::renderDT({
      bat_full <- tourney_bat_rv()
      req(bat_full)
      
      bat_display <- bat_full %>%
        dplyr::select(dplyr::any_of(HITTER_DISPLAY_COLS))
      
      DT::datatable(
        bat_display,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 15, scrollX = TRUE)
      ) %>%
        DT::formatRound(
          columns = intersect(
            c("AVG", "OBP", "SLG", "wOBA",
              "BBpct", "SOpct", "HRper600", "BABIP"),
            names(bat_display)
          ),
          digits = 3
        )
    })
    output$tourney_pit_tbl <- DT::renderDT({
      pit_full <- tourney_pit_rv()
      req(pit_full)
      
      pit_display <- pit_full %>%
        dplyr::select(-dplyr::any_of(c("CID", "GBper")))
      
      DT::datatable(
        pit_display,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 15, scrollX = TRUE)
      ) %>%
        DT::formatRound(
          columns = intersect(
            c("GSper", "IPpergame", "RA9", "ERA",
              "K9", "BB9", "HR9", "BABIP", "GBper"),
            names(pit_display)
          ),
          digits = 3
        )
    })
    output$tourney_instances_tbl <- DT::renderDT({
      DT::datatable(inst_tbl, rownames = FALSE, selection = "single",
                    options = list(pageLength = 10, scrollX = TRUE)) %>%
        DT::formatRound(columns = "WinPct", digits = 3)
    })
  })
  observeEvent(input$tourney_instances_tbl_rows_selected, {
    sel <- input$tourney_instances_tbl_rows_selected
    req(sel)
    
    org          <- selected_org()
    tourney_key  <- selected_tourney()       # KEY retrieved from earlier click
    inst_tbl     <- tourney_instances_rv()
    req(org, tourney_key, inst_tbl)
    req(nrow(inst_tbl) >= sel)
    
    exptime_sel <- inst_tbl$exptime[sel]
    req(exptime_sel)
    
    raw <- raw_df()
    req(nrow(raw) > 0)
    
    raw_inst <- raw %>%
      dplyr::filter(ORG == org, tourney_type == tourney_key) %>%
      dplyr::mutate(exptime_chr = as.character(exptime)) %>%
      dplyr::filter(exptime_chr == exptime_sel)
    
    raw_inst <- ensure_cid(raw_inst)   # <--- add this
    
    validate(need(nrow(raw_inst) > 0, "No raw rows found for this instance."))
    bat_i <- raw_inst %>%
      dplyr::group_by(Name, POS, CID, VAL, VLvl, Title, B) %>%
      dplyr::summarise(
        PA  = sum(PA, na.rm = TRUE),
        AB  = sum(AB, na.rm = TRUE),
        H   = sum(H,  na.rm = TRUE),
        HR  = sum(HR, na.rm = TRUE),
        BB  = sum(BB, na.rm = TRUE),
        SO  = sum(SO, na.rm = TRUE),
        HP  = sum(HP, na.rm = TRUE),
        X2B = sum(X2B.1, na.rm = TRUE),
        X3B = sum(X3B.1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_hitter_rates() %>%
      dplyr::arrange(dplyr::desc(PA))
    
    pit_i <- raw_inst %>%
      dplyr::group_by(Name, POS, CID, VLvl, Title) %>%
      dplyr::summarise(
        G   = sum(G.1,  na.rm = TRUE),
        GS  = sum(GS.1, na.rm = TRUE),
        IP  = round(sum(IP, na.rm = TRUE), 1),
        R   = sum(R.1,   na.rm = TRUE),
        ER  = sum(ER,    na.rm = TRUE),
        BB  = sum(BB.1,  na.rm = TRUE),
        IBB = sum(IBB.1, na.rm = TRUE),
        HBP = sum(HP.1,  na.rm = TRUE),
        AB  = sum(AB.1,  na.rm = TRUE),
        SF  = sum(SF.1,  na.rm = TRUE),
        K   = sum(K,     na.rm = TRUE),
        HR  = sum(HR.1,  na.rm = TRUE),
        GB  = sum(GB,    na.rm = TRUE),
        FB  = sum(FB.1,  na.rm = TRUE),
        X1B = sum(X1B.2, na.rm = TRUE),
        X2B = sum(X2B.2, na.rm = TRUE),
        X3B = sum(X3B.2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_pitcher_rates() %>%
      dplyr::select(dplyr::all_of(PITCHER_DISPLAY_COLS)) %>%
      dplyr::arrange(dplyr::desc(IP))
    
    inst_bat_rv(bat_i)
    inst_pit_rv(pit_i)
    
    selected_exptime(exptime_sel)
    modal_level("instance")
    showModal(render_drilldown_modal())
    
    output$inst_bat_tbl <- DT::renderDT({
      bat_full <- inst_bat_rv()
      req(bat_full)
      
      bat_display <- bat_full %>%
        dplyr::select(dplyr::any_of(HITTER_DISPLAY_COLS))
      
      DT::datatable(
        bat_display,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 20, scrollX = TRUE)
      ) %>%
        DT::formatRound(
          columns = intersect(
            c("AVG", "OBP", "SLG", "wOBA",
              "BBpct", "SOpct", "HRper600", "BABIP"),
            names(bat_display)
          ),
          digits = 3
        )
    })
    output$inst_pit_tbl <- DT::renderDT({
      DT::datatable(
        pit_i,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 20, scrollX = TRUE)
      )
    })
  })
  observeEvent(input$team_modal_instances_rows_selected, {
    sel <- input$team_modal_instances_rows_selected
    req(sel)
    
    org <- selected_org()
    inst_tbl <- instances_rv()
    req(org, inst_tbl)
    req(nrow(inst_tbl) >= sel)
    
    exptime_sel <- inst_tbl$exptime[sel]
    tourney_type_sel <- inst_tbl$tourney_type[sel]  # stable key
    req(exptime_sel, tourney_type_sel)
    
    # set navigation state so Back buttons know where to go
    selected_tourney(tourney_type_sel)
    selected_exptime(exptime_sel)
    
    raw <- raw_df()
    req(nrow(raw) > 0)
    
    raw_inst <- raw %>%
      dplyr::filter(ORG == org, tourney_type == tourney_type_sel) %>%
      dplyr::mutate(exptime_chr = as.character(exptime)) %>%
      dplyr::filter(exptime_chr == exptime_sel)
    
    raw_inst <- ensure_cid(raw_inst)   # <--- add this
    
    validate(need(nrow(raw_inst) > 0, "No raw rows found for this team/tournament instance."))
    
    bat_i <- raw_inst %>%
      dplyr::group_by(Name, POS, CID, VAL, VLvl, Title, B) %>%
      dplyr::summarise(
        PA  = sum(PA, na.rm = TRUE),
        AB  = sum(AB, na.rm = TRUE),
        H   = sum(H,  na.rm = TRUE),
        HR  = sum(HR, na.rm = TRUE),
        BB  = sum(BB, na.rm = TRUE),
        SO  = sum(SO, na.rm = TRUE),
        HP  = sum(HP, na.rm = TRUE),
        X2B = sum(X2B.1, na.rm = TRUE),
        X3B = sum(X3B.1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_hitter_rates() %>%
      dplyr::arrange(dplyr::desc(PA))
    
    pit_i <- raw_inst %>%
      dplyr::group_by(Name, POS, CID, VLvl, Title) %>%
      dplyr::summarise(
        G   = sum(G.1,  na.rm = TRUE),
        GS  = sum(GS.1, na.rm = TRUE),
        IP  = round(sum(IP, na.rm = TRUE), 1),
        
        R   = sum(R.1,   na.rm = TRUE),
        ER  = sum(ER,    na.rm = TRUE),
        BB  = sum(BB.1,  na.rm = TRUE),
        IBB = sum(IBB.1, na.rm = TRUE),
        HBP = sum(HP.1,  na.rm = TRUE),
        AB  = sum(AB.1,  na.rm = TRUE),
        SF  = sum(SF.1,  na.rm = TRUE),
        
        K   = sum(K,     na.rm = TRUE),
        HR  = sum(HR.1,  na.rm = TRUE),
        GB  = sum(GB,    na.rm = TRUE),
        FB  = sum(FB.1,  na.rm = TRUE),
        
        X1B = sum(X1B.2, na.rm = TRUE),
        X2B = sum(X2B.2, na.rm = TRUE),
        X3B = sum(X3B.2, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      add_pitcher_rates() %>%
      dplyr::select(dplyr::all_of(PITCHER_DISPLAY_COLS)) %>%
      dplyr::arrange(dplyr::desc(IP))
    
    inst_bat_rv(bat_i)
    inst_pit_rv(pit_i)
    
    modal_level("instance")
    showModal(render_drilldown_modal())
    
    output$inst_bat_tbl <- DT::renderDT({
      bat_full <- inst_bat_rv()
      req(bat_full)
      
      bat_display <- bat_full %>%
        dplyr::select(dplyr::any_of(HITTER_DISPLAY_COLS))
      
      DT::datatable(
        bat_display,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 20, scrollX = TRUE)
      ) %>%
        DT::formatRound(
          columns = intersect(
            c("AVG", "OBP", "SLG", "wOBA",
              "BBpct", "SOpct", "HRper600", "BABIP"),
            names(bat_display)
          ),
          digits = 3
        )
    })
    output$inst_pit_tbl <- DT::renderDT({
      DT::datatable(
        pit_i,
        rownames  = FALSE,
        selection = "single",
        options   = list(pageLength = 20, scrollX = TRUE)
      )
    })
  })
  output$hitter_summary_tbl <- DT::renderDT({
    df_full <- hitter_universe()     # includes CID etc.
    overall_bat_rv(df_full)          # cache full data for click → player
    
    df_display <- df_full %>%
      dplyr::select(dplyr::any_of(HITTER_DISPLAY_COLS))
    
    DT::datatable(
      df_display,
      rownames  = FALSE,
      selection = "single",
      options   = list(pageLength = 25, scrollX = TRUE)
    ) %>%
      DT::formatRound(
        columns = intersect(
          c("AVG", "OBP", "SLG", "wOBA",
            "BBpct", "SOpct", "HRper600", "BABIP"),
          names(df_display)
        ),
        digits = 3
      )
  })
  
  output$hitter_data_span <- renderUI({
    df <- tourney_raw()
    if (!"exptime" %in% names(df) || nrow(df) == 0) return(NULL)
    
    rng <- range(as.Date(df$exptime), na.rm = TRUE)
    if (any(!is.finite(rng))) return(NULL)
    
    tags$span(
      paste0("Data coverage (hitters): ",
             format(rng[1]), " to ", format(rng[2]))
    )
  })
  
  pitcher_universe <- reactive({
    df <- tourney_raw()
    req(nrow(df) > 0)
    
    # Only rows with any IP
    df <- df %>% dplyr::filter(!is.na(IP) & IP > 0)
    req(nrow(df) > 0)
    
    # --- NEW: pitcher role filter (row-level) ---
    if (!is.null(input$pitcher_role) && input$pitcher_role != "All") {
      # Need GS.1 and G.1 to classify roles
      if (all(c("GS.1", "G.1") %in% names(df))) {
        if (input$pitcher_role == "Starters") {
          # Starters: rows where GS.1 == G.1 (all games started)
          df <- df %>%
            dplyr::filter(
              !is.na(GS.1), !is.na(G.1),
              G.1 > 0,
              GS.1 == G.1
            )
        } else if (input$pitcher_role == "Relievers") {
          # Relievers: rows where GS.1 == 0
          df <- df %>%
            dplyr::filter(
              !is.na(GS.1),
              GS.1 == 0
            )
        }
      } else {
        # If the columns aren't present, just leave df as-is for safety
      }
    }
    req(nrow(df) > 0)
    
    # Position filter (POS column: SP/RP/CL/etc.)
    if (!is.null(input$pitcher_pos) &&
        input$pitcher_pos != "All" &&
        "POS" %in% names(df)) {
      df <- df %>% dplyr::filter(POS == input$pitcher_pos)
    }
    
    req(nrow(df) > 0)
    
    # Aggregate to card level
    df_sum <- df %>%
      dplyr::group_by(Name, POS, CID, VLvl, Title) %>%
      dplyr::summarise(
        G   = sum(G.1,    na.rm = TRUE),
        GS  = sum(GS.1,   na.rm = TRUE),
        IP  = round(sum(IP, na.rm = TRUE), 1),
        R   = sum(R.1,    na.rm = TRUE),
        ER  = sum(ER,     na.rm = TRUE),
        BB  = sum(BB.1,   na.rm = TRUE),
        IBB = sum(IBB.1,  na.rm = TRUE),
        HBP = sum(HP.1,   na.rm = TRUE),
        AB  = sum(AB.1,   na.rm = TRUE),
        SF  = sum(SF.1,   na.rm = TRUE),
        K   = sum(K,      na.rm = TRUE),
        HR  = sum(HR.1,   na.rm = TRUE),
        GB  = sum(GB,     na.rm = TRUE),
        FB  = sum(FB.1,   na.rm = TRUE),
        X1B = sum(X1B.2,  na.rm = TRUE),
        X2B = sum(X2B.2,  na.rm = TRUE),
        X3B = sum(X3B.2,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_pitcher_rates()
    
    # Minimum IP filter at *card* level
    min_ip <- input$pitcher_min_ip %||% 0
    df_sum <- df_sum %>%
      dplyr::filter(IP >= min_ip) %>%
      dplyr::select(dplyr::all_of(PITCHER_DISPLAY_COLS)) %>%
      dplyr::arrange(dplyr::desc(IP))
    
    df_sum
  })
  output$pitcher_summary_tbl <- DT::renderDT({
    df_full <- pitcher_universe()
    overall_pit_rv(df_full)   # keep full data (with CID) for click → player
    
    # Drop CID and GBper from the *displayed* table
    df_display <- df_full %>%
      dplyr::select(-dplyr::any_of(c("CID", "GBper")))
    
    DT::datatable(
      df_display,
      rownames  = FALSE,
      selection = "single",
      options   = list(pageLength = 25, scrollX = TRUE)
    ) %>%
      DT::formatRound(
        columns = intersect(
          c("GSper", "IPpergame", "RA9", "ERA",
            "K9", "BB9", "HR9", "BABIP", "GBper"),
          names(df_display)
        ),
        digits = 3
      )
  })
  
  output$pitcher_data_span <- renderUI({
    df <- tourney_raw()
    if (!"exptime" %in% names(df) || nrow(df) == 0) return(NULL)
    
    rng <- range(as.Date(df$exptime), na.rm = TRUE)
    if (any(!is.finite(rng))) return(NULL)
    
    tags$span(
      paste0("Data coverage (pitchers): ",
             format(rng[1]), " to ", format(rng[2]))
    )
  })
  # --- Click in tournament-level batting table -> player page ---
  observeEvent(input$tourney_bat_tbl_rows_selected, {
    sel <- input$tourney_bat_tbl_rows_selected
    df  <- tourney_bat_rv()
    req(df, length(sel) == 1, sel <= nrow(df))
    
    selected_player_cid(df$CID[sel])
    selected_player_name(df$Name[sel])
    
    modal_level("player")
    showModal(render_drilldown_modal("player"))
    build_player_modal_outputs()
  })
  
  # --- Click in tournament-level pitching table -> player page ---
  observeEvent(input$tourney_pit_tbl_rows_selected, {
    sel <- input$tourney_pit_tbl_rows_selected
    df  <- tourney_pit_rv()
    req(df, length(sel) == 1, sel <= nrow(df))
    
    selected_player_cid(df$CID[sel])
    selected_player_name(df$Name[sel])
    
    modal_level("player")
    showModal(render_drilldown_modal("player"))
    build_player_modal_outputs()
  })
  
  # --- Click in instance-level batting table -> player page ---
  observeEvent(input$inst_bat_tbl_rows_selected, {
    sel <- input$inst_bat_tbl_rows_selected
    df  <- inst_bat_rv()
    req(df, length(sel) == 1, sel <= nrow(df))
    
    selected_player_cid(df$CID[sel])
    selected_player_name(df$Name[sel])
    
    modal_level("player")
    showModal(render_drilldown_modal("player"))
    build_player_modal_outputs()
  })
  
  # --- Click in instance-level pitching table -> player page ---
  observeEvent(input$inst_pit_tbl_rows_selected, {
    sel <- input$inst_pit_tbl_rows_selected
    df  <- inst_pit_rv()
    req(df, length(sel) == 1, sel <= nrow(df))
    
    selected_player_cid(df$CID[sel])
    selected_player_name(df$Name[sel])
    
    modal_level("player")
    showModal(render_drilldown_modal("player"))
    build_player_modal_outputs()
  })

  observeEvent(input$back_to_team, {
    # ORG is already stored in selected_org()
    removeModal()
    show_team_modal()
  })
  
  observeEvent(input$back_to_tourney, {
    modal_level("tourney")
    removeModal()
    showModal(render_drilldown_modal("tourney"))  # <-- explicit
  })
  observeEvent(input$player_back_to_instance, {
    # If we never came from an instance, just go back to tournament instead
    if (is.null(selected_exptime())) {
      modal_level("tourney")
      removeModal()
      showModal(render_drilldown_modal("tourney"))
      return(NULL)
    }
    
    # Normal case: instance context exists, just re-show the instance modal
    modal_level("instance")
    removeModal()
    showModal(render_drilldown_modal("instance"))
  })
  
  observeEvent(input$player_back_to_tourney, {
    modal_level("tourney")
    removeModal()
    showModal(render_drilldown_modal("tourney"))
  })
  # --- Click in overall hitters tab -> player page ---
  observeEvent(input$hitter_summary_tbl_rows_selected, {
    sel <- input$hitter_summary_tbl_rows_selected
    df  <- overall_bat_rv()
    req(df, length(sel) == 1, sel <= nrow(df))
    
    selected_player_cid(df$CID[sel])
    selected_player_name(df$Name[sel])
    
    modal_level("player")
    showModal(render_drilldown_modal("player"))
    build_player_modal_outputs()
  })
  
  # --- Click in overall pitchers tab -> player page ---
  observeEvent(input$pitcher_summary_tbl_rows_selected, {
    sel <- input$pitcher_summary_tbl_rows_selected
    df  <- overall_pit_rv()
    req(df, length(sel) == 1, sel <= nrow(df))
    
    selected_player_cid(df$CID[sel])
    selected_player_name(df$Name[sel])
    
    modal_level("player")
    showModal(render_drilldown_modal("player"))
    build_player_modal_outputs()
  })
}
  ensure_cid <- function(df) {
    if (!"CID" %in% names(df)) {
      df$CID <- NA_integer_
    }
    df
  }
  
# tiny helper (keeps the app self-contained)
`%||%` <- function(a, b) if (!is.null(a)) a else b

shinyApp(ui, server)