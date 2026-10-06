# ============================================================================
# ABA SCOUTING DASHBOARD - MULTI-TEAM VERSION
# Login: username = first word of team name, password = last word
#   e.g. "Ohio Kings"  ->  user: Ohio  /  pass: Kings
# Cache: aba_data_cache.rds saved locally - only scrapes once
# ============================================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(lubridate)
library(plotly)
library(DT)
library(future)
library(furrr)
conflictRules('dplyr', exclude = 'filter')

n_cores <- max(1, min(4, parallelly::availableCores()))
plan(multisession, workers = n_cores)

# ============================================================================
# ABA BASE COLORS  (login page / pre-login chrome)
# ============================================================================
aba_primary   <- "#1B3A6B"
aba_secondary <- "#C8102E"
aba_accent    <- "#F5A623"

# ============================================================================
# PER-TEAM COLOR OVERRIDES
# Teams not listed here get auto-assigned from color_pool
# ============================================================================
team_color_overrides <- list(
  "Ohio Kings" = list(primary = "#C8102E", secondary = "#041E42", accent = "#FDBB30")
)

color_pool <- list(
  list(primary = "#007A33", secondary = "#FFFFFF", accent = "#BA9653"),
  list(primary = "#552583", secondary = "#FDB927", accent = "#FFFFFF"),
  list(primary = "#1D428A", secondary = "#FFC72C", accent = "#FFFFFF"),
  list(primary = "#CE1141", secondary = "#000000", accent = "#C4CED4"),
  list(primary = "#006BB6", secondary = "#F58426", accent = "#FFFFFF"),
  list(primary = "#860038", secondary = "#041E42", accent = "#FDBB30"),
  list(primary = "#00471B", secondary = "#EEE1C6", accent = "#FFFFFF"),
  list(primary = "#5D76A9", secondary = "#12173F", accent = "#F5B112"),
  list(primary = "#B4975A", secondary = "#061922", accent = "#FFFFFF"),
  list(primary = "#E56020", secondary = "#1D1160", accent = "#FFFFFF"),
  list(primary = "#007AC1", secondary = "#EF3B24", accent = "#002D62"),
  list(primary = "#98002E", secondary = "#00A9E0", accent = "#FFFFFF")
)

get_team_colors <- function(team_name) {
  if (is.null(team_name) || is.na(team_name) || team_name == "")
    return(list(primary = aba_primary, secondary = aba_secondary, accent = aba_accent))
  if (team_name %in% names(team_color_overrides))
    return(team_color_overrides[[team_name]])
  idx <- (sum(utf8ToInt(substr(team_name, 1, min(8, nchar(team_name))))) %% length(color_pool)) + 1
  return(color_pool[[idx]])
}

# ============================================================================
# CREDENTIAL BUILDER
# username = first word of team name | password = last word of team name
# ============================================================================
build_team_credentials <- function(team_names) {
  creds <- list()
  for (tn in team_names) {
    tn <- trimws(tn)
    if (nchar(tn) == 0) next
    parts    <- strsplit(tn, "\\s+")[[1]]
    username <- parts[1]
    password <- tail(parts, 1)
    key      <- username
    if (key %in% names(creds)) key <- paste0(username, "_2")
    creds[[key]] <- list(password = password, team_name = tn)
  }
  return(creds)
}

# ============================================================================
# SCRAPING FUNCTIONS  (identical to original)
# ============================================================================

get_team_lookup <- function() {
  url  <- "https://www.mystatsonline.com/basket/visitor/league/home/home_basket.aspx?IDLeague=64225"
  page <- read_html(url, timeout = 10)
  team_links <- page %>% html_nodes("a[href*='IDTeam']")
  team_lookup <- data.frame(
    team_id   = team_links %>% html_attr("href") %>% str_extract("IDTeam=\\d+") %>% str_remove("IDTeam="),
    team_name = team_links %>% html_text() %>% str_trim(),
    stringsAsFactors = FALSE
  ) %>%
    distinct() %>%
    filter(!is.na(team_id), team_name != "", !str_detect(team_name, "^\\s*$")) %>%
    mutate(
      team_name = case_when(
        str_detect(team_name, "^\\d") ~ {
          code <- str_extract(team_name, "^\\d{3}")
          name <- str_extract(team_name, "(?<=\\d\\s).*")
          paste(code, name)
        },
        str_detect(team_name, "^[A-Z]{3}[A-Z]") ~ str_remove(team_name, "^[A-Z]{3}"),
        TRUE ~ team_name
      )
    )
  
  team_lookup$logo_url <- sapply(1:nrow(team_lookup), function(i) {
    team_id   <- team_lookup$team_id[i]
    team_name <- team_lookup$team_name[i]
    tryCatch({
      team_page_url <- paste0(
        "https://www.mystatsonline.com/basket/visitor/league/stats/team_basket.aspx?IDLeague=64225&IDSeason=104460&IDTeam=",
        team_id)
      Sys.sleep(0.1)
      team_page  <- read_html(team_page_url)
      all_logos  <- team_page %>% html_nodes("img[src*='teamlogo']") %>% html_attr("src")
      correct_logo <- NULL
      for (logo_url in all_logos) {
        if (str_detect(logo_url, paste0("/", team_id, "[./]"))) {
          correct_logo <- logo_url; break
        }
      }
      if (!is.null(correct_logo) && !is.na(correct_logo)) {
        full_url <- if (str_detect(correct_logo, "^https?://")) correct_logo
        else if (str_detect(correct_logo, "^/")) paste0("https://www.mystatsonline.com", correct_logo)
        else paste0("https://www.mystatsonline.com/", correct_logo)
        clean_url <- str_remove(full_url, "\\?cache=.*$")
        message(paste0("OK ", team_name))
        return(clean_url)
      } else {
        message(paste0("No logo: ", team_name))
        return(NA_character_)
      }
    }, error = function(e) {
      message(paste0("Error: ", team_name, " - ", e$message))
      return(NA_character_)
    })
  })
  
  team_lookup %>% arrange(team_name)
}

get_team_player_stats <- function(team_id) {
  url <- paste0(
    "https://www.mystatsonline.com/basket/visitor/league/stats/team_basket.aspx?IDLeague=64225&IDSeason=104460&IDTeam=",
    team_id)
  tryCatch({
    page   <- read_html(url)
    tables <- page %>% html_table(fill = TRUE)
    if (length(tables) < 2 || nrow(tables[[2]]) < 2) return(NULL)
    tables[[2]] %>%
      mutate(
        PLAYERS = str_remove(PLAYERS, "#\\d+") %>% str_trim(),
        PLAYERS = str_replace(PLAYERS, "^(.*),\\s*(.*)$", "\\2 \\1"),
        PLAYERS = str_to_title(PLAYERS)
      ) %>%
      select(-matches("^#$|^OTHER$")) %>%
      rename(Player = "PLAYERS") %>%
      filter(Player != "", !is.na(Player), Player != "Players")
  }, error = function(e) NULL)
}

get_player_lookup <- function() {
  url  <- "https://www.mystatsonline.com/basket/visitor/league/stats/player_basket.aspx?IDLeague=64225"
  page <- read_html(url)
  player_links <- page %>% html_nodes("a[href*='player_details_basket']")
  data.frame(
    player_id   = player_links %>% html_attr("href") %>% str_extract("\\d+"),
    player_name = player_links %>% html_text() %>% str_trim(),
    stringsAsFactors = FALSE
  ) %>%
    distinct() %>%
    mutate(
      player_name = case_when(
        str_detect(player_name, ",") ~ {
          parts      <- str_split(player_name, ",", simplify = TRUE)
          last_name  <- str_trim(parts[, 1])
          first_name <- str_trim(parts[, 2]) %>% str_remove("#\\d+") %>% str_trim()
          paste(first_name, last_name)
        },
        TRUE ~ player_name
      )
    ) %>%
    select(player_id, player_name) %>%
    mutate(player_name = str_to_title(player_name)) %>%
    arrange(player_name)
}

get_indiv_stats <- function(player_id) {
  url <- paste0(
    "https://www.mystatsonline.com/basket/visitor/league/card/card_basket.aspx?IDLeague=64225&IDPlayer=",
    player_id)
  tryCatch({
    page   <- read_html(url)
    tables <- page %>% html_table(fill = TRUE)
    if (length(tables) < 2) return(NULL)
    stat_table <- tables[[2]]
    if (nrow(stat_table) < 2) return(NULL)
    
    player_name <- page %>%
      html_node("h3") %>% html_text() %>%
      str_extract(".*(?=\\|)") %>% str_trim() %>% str_to_title()
    if (is.na(player_name) || player_name == "") return(NULL)
    
    player_position <- stat_table %>%
      filter(!str_detect(GAME, "TOTAL")) %>% pull(POS) %>% first()
    if (is.na(player_position)) player_position <- "G"
    
    stat_table <- stat_table %>%
      mutate(
        player_name = player_name,
        position    = player_position,
        date        = mdy(str_extract(GAME, "\\d{1,2}/\\d{1,2}/\\d{4}")),
        time        = str_extract(GAME, "\\d{1,2}:\\d{2} \\w{2}")
      )
    
    teams <- stat_table %>%
      pull(GAME) %>%
      map(~str_extract_all(.x, "[A-Z]{3,4}(?= \\d+)")[[1]]) %>%
      unlist()
    if (length(teams) == 0) return(NULL)
    player_team <- names(sort(table(teams), decreasing = TRUE))[1]
    
    stat_table <- stat_table %>%
      mutate(
        opponent = map_chr(GAME, function(game) {
          t   <- str_extract_all(game, "[A-Z]{3,4}(?= \\d+)")[[1]]
          opp <- setdiff(t, player_team)
          if (length(opp) > 0) opp[1] else NA_character_
        })
      ) %>%
      filter(!str_detect(GAME, "TOTAL")) %>%
      select(-matches("GAME|PTS/G|REB/G|AST/G|STL/G|BLK/G"))
    if (nrow(stat_table) == 0) return(NULL)
    
    per_game_table <- stat_table %>%
      mutate(
        ft_pct    = suppressWarnings(as.numeric(str_remove(`FT%`, "%"))),
        two_pct   = suppressWarnings(as.numeric(str_remove(`2P%`, "%"))),
        three_pct = suppressWarnings(as.numeric(str_remove(`3P%`, "%")))
      ) %>%
      select(-matches("FT%|2P%|3P%")) %>%
      group_by(player_name, position) %>%
      summarize(
        PPG     = round(mean(as.numeric(PTS), na.rm = TRUE), 2),
        RPG     = round(mean(as.numeric(REB), na.rm = TRUE), 2),
        APG     = round(mean(as.numeric(AST), na.rm = TRUE), 2),
        SPG     = round(mean(as.numeric(STL), na.rm = TRUE), 2),
        BPG     = round(mean(as.numeric(BLK), na.rm = TRUE), 2),
        FT_pct  = round(mean(ft_pct,    na.rm = TRUE), 2),
        FG2_pct = round(mean(two_pct,   na.rm = TRUE), 2),
        FG3_pct = round(mean(three_pct, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA_real_, .)))
    if (nrow(per_game_table) == 0) return(NULL)
    
    list(game_log = stat_table, per_game = per_game_table,
         position = player_position, team = player_team)
  }, error = function(e) NULL)
}

get_division_standings <- function() {
  url  <- "https://www.mystatsonline.com/basket/visitor/league/home/home_basket.aspx?IDLeague=64225"
  page <- read_html(url)
  clean_team_names <- function(teams) {
    teams %>% mutate(
      Team = case_when(
        str_detect(Team, "^\\d") ~ paste(str_extract(Team, "^\\d{3}"), str_extract(Team, "(?<=\\d\\s).*")),
        str_detect(Team, "^[A-Z]{3}[A-Z]") ~ str_remove(Team, "^[A-Z]{3}"),
        TRUE ~ Team
      )
    )
  }
  bind_rows(
    page %>% html_table() %>% .[[2]] %>% clean_team_names() %>% mutate(Division = "Alpha Blue"),
    page %>% html_table() %>% .[[3]] %>% clean_team_names() %>% mutate(Division = "Alpha Red"),
    page %>% html_table() %>% .[[4]] %>% clean_team_names() %>% mutate(Division = "Beta Black"),
    page %>% html_table() %>% .[[5]] %>% clean_team_names() %>% mutate(Division = "Beta White")
  ) %>% arrange(desc(W))
}

get_accurate_team_ppg <- function(team_name, aba_data) {
  if (!is.null(aba_data$team_schedules) && team_name %in% names(aba_data$team_schedules)) {
    sched      <- aba_data$team_schedules[[team_name]]
    real_games <- sched %>% filter(!is_forfeit, team_score > 0)
    if (nrow(real_games) > 0) return(round(mean(real_games$team_score, na.rm = TRUE), 1))
  }
  roster <- aba_data$all_team_stats[[team_name]]
  if (!is.null(roster)) {
    total_row <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
    tr  <- aba_data$league_data %>% filter(Team == team_name)
    gp  <- if (nrow(tr) > 0) tr$W[1] + tr$L[1] else 15
    if (nrow(total_row) > 0 && "PTS" %in% names(total_row))
      return(round(as.numeric(total_row$PTS[1]) / gp, 1))
  }
  return(0)
}

get_accurate_team_rpg <- function(team_name, aba_data) {
  roster <- aba_data$all_team_stats[[team_name]]
  if (!is.null(roster)) {
    total_row <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
    tr  <- aba_data$league_data %>% filter(Team == team_name)
    gp  <- if (nrow(tr) > 0) tr$W[1] + tr$L[1] else 15
    if (nrow(total_row) > 0 && "REB" %in% names(total_row))
      return(round(as.numeric(total_row$REB[1]) / gp, 1))
  }
  return(0)
}

get_accurate_team_apg <- function(team_name, aba_data) {
  roster <- aba_data$all_team_stats[[team_name]]
  if (!is.null(roster)) {
    total_row <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
    tr  <- aba_data$league_data %>% filter(Team == team_name)
    gp  <- if (nrow(tr) > 0) tr$W[1] + tr$L[1] else 15
    if (nrow(total_row) > 0 && "AST" %in% names(total_row))
      return(round(as.numeric(total_row$AST[1]) / gp, 1))
  }
  return(0)
}

# ============================================================================
# DATA LOADING  — persistent local cache (aba_data_cache.rds)
# First run scrapes everything (~15-20 min). Every run after loads in seconds.
# ============================================================================

load_aba_data <- function(force_refresh = FALSE) {
  cache_file <- "aba_data_cache.rds"
  
  # ── INCREMENTAL UPDATE (cache < 24 h old) ────────────────────────────────
  if (!force_refresh && file.exists(cache_file)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file)$mtime, units = "hours")
    if (cache_age < 24) {
      message("Loading from cache and checking for updates...")
      cached_data <- readRDS(cache_file)
      
      current_team_lookup <- get_team_lookup()
      current_team_stats  <- list()
      for (i in 1:nrow(current_team_lookup)) {
        roster <- get_team_player_stats(current_team_lookup$team_id[i])
        if (!is.null(roster) && nrow(roster) > 0)
          current_team_stats[[current_team_lookup$team_name[i]]] <- roster
      }
      
      new_players <- setdiff(
        unique(unlist(lapply(current_team_stats, function(r) r$Player))),
        names(cached_data$all_player_stats))
      
      if (length(new_players) > 0) {
        message(paste("Found", length(new_players), "new players..."))
        player_lookup  <- get_player_lookup()
        new_player_ids <- player_lookup %>% filter(player_name %in% new_players)
        new_results    <- future_map2(
          new_player_ids$player_id, new_player_ids$player_name,
          ~{ stats <- get_indiv_stats(.x)
          if (!is.null(stats) && !is.null(stats$per_game) && nrow(stats$per_game) > 0)
            list(name = .y, stats = stats) else NULL },
          .options = furrr_options(seed = TRUE))
        for (r in new_results)
          if (!is.null(r)) cached_data$all_player_stats[[r$name]] <- r$stats
      } else {
        message("No new players - data up to date")
      }
      
      # Refresh schedule
      cached_data$team_schedules <- .scrape_schedule(current_team_lookup)
      cached_data$team_lookup    <- current_team_lookup
      cached_data$all_team_stats <- current_team_stats
      cached_data$league_data    <- get_division_standings()
      cached_data$last_updated   <- Sys.time()
      saveRDS(cached_data, cache_file)
      return(cached_data)
    }
  }
  
  # ── FULL FRESH SCRAPE ────────────────────────────────────────────────────
  message("Full scrape starting (~15-20 min)...")
  data               <- list()
  data$team_lookup   <- get_team_lookup()
  data$player_lookup <- get_player_lookup()
  data$league_data   <- get_division_standings()
  
  data$all_team_stats <- list()
  for (i in 1:nrow(data$team_lookup)) {
    roster <- get_team_player_stats(data$team_lookup$team_id[i])
    if (!is.null(roster) && nrow(roster) > 0)
      data$all_team_stats[[data$team_lookup$team_name[i]]] <- roster
  }
  
  all_roster_players <- unique(unlist(lapply(data$all_team_stats, function(r) r$Player)))
  players_to_scrape  <- data$player_lookup %>% filter(player_name %in% all_roster_players)
  batch_size  <- 25
  num_batches <- ceiling(nrow(players_to_scrape) / batch_size)
  data$all_player_stats <- list()
  
  for (batch_num in 1:num_batches) {
    si    <- (batch_num - 1) * batch_size + 1
    ei    <- min(batch_num * batch_size, nrow(players_to_scrape))
    batch <- players_to_scrape[si:ei, ]
    results <- future_map2(
      batch$player_id, batch$player_name,
      ~{ stats <- get_indiv_stats(.x)
      if (!is.null(stats) && !is.null(stats$per_game) && nrow(stats$per_game) > 0)
        list(name = .y, stats = stats) else NULL },
      .options = furrr_options(seed = TRUE))
    for (r in results) if (!is.null(r)) data$all_player_stats[[r$name]] <- r$stats
    message(paste("Batch", batch_num, "of", num_batches, "done"))
  }
  
  data$team_schedules <- .scrape_schedule(data$team_lookup)
  data$last_updated   <- Sys.time()
  saveRDS(data, cache_file)
  message("Cache saved: ", normalizePath(cache_file))
  return(data)
}

# Internal helper — scrape the league schedule once and assign to all teams
.scrape_schedule <- function(team_lookup) {
  schedules <- list()
  league_url <- "https://www.mystatsonline.com/basket/visitor/league/schedule_scores/schedule.aspx?IDLeague=64225"
  tryCatch({
    page   <- read_html(league_url)
    tables <- page %>% html_table(fill = TRUE)
    if (length(tables) < 2) return(schedules)
    
    sched_raw <- tables[[2]]
    names(sched_raw) <- c("DateTime","AwayTeam","AwayScore","Col4","HomeScore","HomeTeam","Col7","Status")
    
    all_games    <- list()
    current_date <- NA
    
    for (i in 1:nrow(sched_raw)) {
      row <- sched_raw[i, ]
      dtc <- as.character(row$DateTime)
      if (grepl("googletag|div-gpt", dtc, ignore.case = TRUE)) next
      if (grepl("(Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).*\\d{4}", dtc) &&
          !grepl("^\\d{1,2}:\\d{2}", dtc)) {
        current_date <- tryCatch(mdy(dtc), error = function(e) NA); next
      }
      if (!grepl("^\\d{1,2}:\\d{2}\\s*(AM|PM)", dtc)) next
      away_team  <- str_trim(as.character(row$AwayTeam))
      home_team  <- str_trim(as.character(row$HomeTeam))
      away_score <- suppressWarnings(as.numeric(str_trim(row$AwayScore)))
      home_score <- suppressWarnings(as.numeric(str_trim(row$HomeScore)))
      if (is.na(current_date)) next
      is_future  <- is.na(away_score) || is.na(home_score)
      all_games[[length(all_games) + 1]] <- data.frame(
        date       = current_date,
        away_team  = away_team, home_team  = home_team,
        away_score = if (is_future) NA else away_score,
        home_score = if (is_future) NA else home_score,
        is_forfeit = if (is_future) FALSE else (away_score == 0 && home_score == 0),
        stringsAsFactors = FALSE)
    }
    
    if (length(all_games) == 0) return(schedules)
    all_games_df <- bind_rows(all_games)
    
    for (i in 1:nrow(team_lookup)) {
      tn    <- team_lookup$team_name[i]
      tn_cl <- str_remove(tn, "^\\d{3}\\s+") %>% str_trim()
      tg <- all_games_df %>%
        filter(grepl(tn_cl, away_team, ignore.case=TRUE) | grepl(tn_cl, home_team, ignore.case=TRUE) |
                 grepl(tn,    away_team, ignore.case=TRUE) | grepl(tn,    home_team, ignore.case=TRUE)) %>%
        rowwise() %>%
        mutate(is_home = grepl(tn_cl, home_team, ignore.case=TRUE) ||
                 grepl(tn, home_team, ignore.case=TRUE)) %>%
        ungroup() %>%
        rowwise() %>%
        mutate(
          team_score = if_else(is_home, home_score, away_score),
          opp_score  = if_else(is_home, away_score, home_score),
          opponent   = if_else(is_home, away_team,  home_team),
          result     = if_else(team_score > opp_score, "W", "L"),
          home_away  = if_else(is_home, "Home", "Away")
        ) %>%
        ungroup() %>%
        select(date, team_score, opp_score, opponent, result, home_away, is_forfeit) %>%
        arrange(date)
      if (nrow(tg) > 0) schedules[[tn]] <- tg
    }
  }, error = function(e) message("Schedule scrape error: ", e$message))
  return(schedules)
}

# ============================================================================
# SCHEDULE / DIFFICULTY HELPERS  (identical to original)
# ============================================================================

calculate_game_difficulty <- function(opponent_name, aba_data) {
  opp_record <- aba_data$league_data %>% filter(Team == opponent_name)
  if (nrow(opp_record) == 0) return(5)
  opp_gp    <- opp_record$W[1] + opp_record$L[1]
  opp_wpct  <- if (opp_gp > 0) opp_record$W[1] / opp_gp else 0.5
  opp_ppg   <- get_accurate_team_ppg(opponent_name, aba_data)
  wpct_score <- opp_wpct * 10
  ppg_score  <- max(1, min(10, 5 + (opp_ppg - 95) / 20))
  kings_div  <- aba_data$league_data %>% filter(Team == "Ohio Kings") %>% pull(Division)
  is_rival   <- if (length(kings_div) > 0 && opp_record$Division[1] == kings_div[1]) 2 else 0
  difficulty <- round(wpct_score * 0.4 + ppg_score * 0.3 + is_rival * 1.5, 1)
  max(1, min(10, difficulty))
}

get_difficulty_label <- function(difficulty) {
  if (difficulty >= 8.5) return(list(label = "\u2694\ufe0f War",         color = "#7f1d1d", bg = "#fef2f2"))
  if (difficulty >= 7.0) return(list(label = "\U0001f525 Battle",        color = "#b91c1c", bg = "#fee2e2"))
  if (difficulty >= 5.5) return(list(label = "\U0001f4aa Competitive",   color = "#d97706", bg = "#fffbeb"))
  if (difficulty >= 4.0) return(list(label = "\u2696\ufe0f Even",        color = "#65a30d", bg = "#f7fee7"))
  return(list(label = "\U0001f60c Routine", color = "#15803d", bg = "#f0fdf4"))
}

# ============================================================================
# UNIFIED WIN PROBABILITY  (original formula, my_team_name parameter added)
# ============================================================================

calculate_unified_win_probability <- function(kings_data, opponent_name, aba_data,
                                              include_schedule = TRUE,
                                              my_team_name = NULL) {
  if (is.null(my_team_name)) my_team_name <- "Ohio Kings"
  
  opp_roster   <- aba_data$all_team_stats[[opponent_name]]
  kings_roster <- aba_data$all_team_stats[[my_team_name]]
  has_full_data <- !is.null(kings_roster) && !is.null(opp_roster) &&
    nrow(kings_roster) > 0 && nrow(opp_roster) > 0
  
  if (!has_full_data) {
    kings_record <- aba_data$league_data %>% filter(Team == my_team_name)
    opp_record   <- aba_data$league_data %>% filter(Team == opponent_name)
    if (nrow(kings_record) > 0 && nrow(opp_record) > 0) {
      k_gp   <- kings_record$W[1] + kings_record$L[1]
      o_gp   <- opp_record$W[1]   + opp_record$L[1]
      k_wpct <- if (k_gp > 0) kings_record$W[1] / k_gp else 0.5
      o_wpct <- if (o_gp > 0) opp_record$W[1]   / o_gp else 0.5
      prob   <- round(100 / (1 + exp(-4 * (k_wpct - o_wpct))), 0)
      return(list(prob = prob, has_full_data = FALSE, message = "Limited data - based on records only"))
    }
    return(list(prob = 50, has_full_data = FALSE, message = "Insufficient data"))
  }
  
  kings_record <- aba_data$league_data %>% filter(Team == my_team_name)
  opp_record   <- aba_data$league_data %>% filter(Team == opponent_name)
  if (nrow(kings_record) == 0 || nrow(opp_record) == 0)
    return(list(prob = 50, has_full_data = FALSE, message = "No record data"))
  
  k_gp   <- kings_record$W[1] + kings_record$L[1]
  o_gp   <- opp_record$W[1]   + opp_record$L[1]
  k_wpct <- if (k_gp > 0) kings_record$W[1] / k_gp else 0.5
  o_wpct <- if (o_gp > 0) opp_record$W[1]   / o_gp else 0.5
  
  k_ppg <- get_accurate_team_ppg(my_team_name,  aba_data)
  o_ppg <- get_accurate_team_ppg(opponent_name, aba_data)
  
  get_stat <- function(roster, col) {
    tr <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
    if (nrow(tr) > 0 && col %in% names(tr)) return(as.numeric(tr[[col]][1]))
    pr <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
    if (col %in% names(pr)) return(sum(as.numeric(pr[[col]]), na.rm = TRUE))
    return(0)
  }
  
  k_2pm <- get_stat(kings_roster,"2PM"); k_2pa <- get_stat(kings_roster,"2PA")
  k_3pm <- get_stat(kings_roster,"3PM"); k_3pa <- get_stat(kings_roster,"3PA")
  k_fga <- k_2pa + k_3pa; k_fgm <- k_2pm + k_3pm
  k_efg <- if (k_fga > 0) (k_fgm + 0.5 * k_3pm) / k_fga * 100 else 50
  
  o_2pm <- get_stat(opp_roster,"2PM"); o_2pa <- get_stat(opp_roster,"2PA")
  o_3pm <- get_stat(opp_roster,"3PM"); o_3pa <- get_stat(opp_roster,"3PA")
  o_fga <- o_2pa + o_3pa; o_fgm <- o_2pm + o_3pm
  o_efg <- if (o_fga > 0) (o_fgm + 0.5 * o_3pm) / o_fga * 100 else 50
  
  ppg_advantage    <- 100 / (1 + exp(-0.12 * (k_ppg - o_ppg)))
  efg_advantage    <- 100 / (1 + exp(-0.15 * (k_efg - o_efg)))
  record_advantage <- 100 / (1 + exp(-4   * (k_wpct - o_wpct)))
  matchup_advantage <- 50
  schedule_advantage <- 50
  
  if (include_schedule &&
      my_team_name  %in% names(aba_data$team_schedules) &&
      opponent_name %in% names(aba_data$team_schedules)) {
    k_sched  <- aba_data$team_schedules[[my_team_name]]
    o_sched  <- aba_data$team_schedules[[opponent_name]]
    k_recent <- k_sched %>% filter(date >= Sys.Date() - 14, date < Sys.Date(), !is_forfeit) %>% nrow()
    o_recent <- o_sched %>% filter(date >= Sys.Date() - 14, date < Sys.Date(), !is_forfeit) %>% nrow()
    if      (k_recent > o_recent + 1) schedule_advantage <- 45
    else if (o_recent > k_recent + 1) schedule_advantage <- 55
  }
  
  win_prob <- round(
    ppg_advantage     * 0.30 +
      efg_advantage     * 0.20 +
      record_advantage  * 0.20 +
      matchup_advantage * 0.25 +
      schedule_advantage * 0.05, 1)
  
  list(prob = max(15, min(85, win_prob)), has_full_data = TRUE, message = NULL)
}

# ============================================================================
# UI
# ============================================================================

ui <- conditionalPanel(
  condition = "output.show_dashboard == true",
  
  dashboardPage(
    skin = "red",
    
    dashboardHeader(
      title      = uiOutput("header_title"),
      titleWidth = 300,
      
      tags$li(class = "dropdown",
              tags$head(
                tags$link(
                  href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&display=swap",
                  rel  = "stylesheet"
                )
              ),
              uiOutput("dynamic_css")
      ),
      
      tags$li(class = "dropdown", style = "padding: 15px;",
              actionLink("dark_mode_btn",
                         HTML("\U0001f319 Dark Mode"),
                         style = "color: white; font-weight: bold; margin-right: 15px;")),
      
      tags$li(class = "dropdown", style = "padding: 15px;",
              actionLink("logout_btn",
                         HTML("<i class='fa fa-sign-out'></i> Logout"),
                         style = "color: white; font-weight: bold;"))
    ),
    
    dashboardSidebar(
      div(
        style = "text-align: center; padding: 15px 10px 10px 10px;",
        uiOutput("sidebar_logo"),
        uiOutput("sidebar_team_name")
      ),
      hr(style = "border-color: #4a4a4a; margin: 5px 15px;"),
      sidebarMenu(
        id = "tabs",
        menuItem("League Overview", tabName = "league",   icon = icon("trophy")),
        menuItem("Division",        tabName = "division",  icon = icon("users")),
        menuItem("Team",            tabName = "team",      icon = icon("basketball-ball")),
        menuItem("Player",          tabName = "player",    icon = icon("user")),
        menuItem("Coaching",        tabName = "coaching",  icon = icon("clipboard")),
        menuItem("Schedule",        tabName = "schedule",  icon = icon("calendar")),
        menuItem("Scouting Report", tabName = "scouting",  icon = icon("file-text"))
      )
    ),
    
    dashboardBody(
      
      tags$head(
        tags$script(HTML("
          Shiny.addCustomMessageHandler('toggleDarkMode', function(msg) {
            document.body.classList.toggle('dark-mode');
            var btn = document.querySelector('#dark_mode_btn');
            if (document.body.classList.contains('dark-mode')) {
              btn.innerHTML = '\u2600\ufe0f Light Mode';
            } else {
              btn.innerHTML = '\U0001f319 Dark Mode';
            }
          });

          $(document).ready(function() {
            $(document).on('mousedown focusin', '.selectize-input', function() {
              var $input = $(this);
              setTimeout(function() {
                var $dropdown = $input.closest('.selectize-control').find('.selectize-dropdown');
                if ($dropdown.length) {
                  var inputRect = $input[0].getBoundingClientRect();
                  $dropdown.css({
                    'position': 'fixed',
                    'top':      (inputRect.bottom + 2) + 'px',
                    'left':     inputRect.left + 'px',
                    'width':    inputRect.width + 'px',
                    'z-index':  '999999'
                  });
                }
              }, 10);
            });
            $(window).on('scroll resize', function() {
              $('.selectize-control').each(function() {
                var $input    = $(this).find('.selectize-input');
                var $dropdown = $(this).find('.selectize-dropdown');
                if ($dropdown.is(':visible')) {
                  var inputRect = $input[0].getBoundingClientRect();
                  $dropdown.css({
                    'top':   (inputRect.bottom + 2) + 'px',
                    'left':  inputRect.left + 'px',
                    'width': inputRect.width + 'px'
                  });
                }
              });
            });
          });
        "))
      ),
      
      tabItems(
        
        # ── LEAGUE OVERVIEW ──────────────────────────────────────────────────
        tabItem(
          tabName = "league",
          fluidRow(
            column(6, h2("League Overview", style = "margin-top: 15px;")),
            column(3,
                   div(style = "margin-top: 15px;",
                       textInput("quick_search", NULL,
                                 placeholder = "\U0001f50d Search players/teams...",
                                 width = "100%"))),
            column(3,
                   div(style = "text-align: right; padding-top: 15px;",
                       actionButton("refresh_data_btn", "\U0001f504 Refresh Data",
                                    class = "btn-warning",
                                    style = "font-weight: bold;")))
          ),
          fluidRow(column(12, uiOutput("data_update_info"))),
          fluidRow(
            box(title = "League Standings", status = "primary", solidHeader = TRUE, width = 7,
                DTOutput("league_standings")),
            box(title = "Standings Race",   status = "primary", solidHeader = TRUE, width = 5,
                plotlyOutput("standings_race", height = "500px"))
          ),
          fluidRow(
            box(title = "Top Players", status = "info", solidHeader = TRUE, width = 6,
                selectInput("player_stat_select", "Stat:",
                            choices = c("PPG","RPG","APG","SPG","BPG"), selected = "PPG"),
                DTOutput("top_players")),
            box(title = "Top Teams", status = "info", solidHeader = TRUE, width = 6,
                selectInput("team_stat_select", "Stat:",
                            choices = c("PPG","RPG","APG"), selected = "PPG"),
                DTOutput("top_teams"))
          ),
          fluidRow(
            box(title = "Playoff Bracket - Division Leaders", status = "success",
                solidHeader = TRUE, width = 12,
                uiOutput("playoff_bracket"))
          ),
          fluidRow(
            box(title = "League Performance Trends", status = "warning",
                solidHeader = TRUE, width = 12,
                plotlyOutput("league_trends", height = "400px"))
          )
        ),
        
        # ── DIVISION ─────────────────────────────────────────────────────────
        tabItem(
          tabName = "division",
          h2("Division View"),
          fluidRow(
            box(width = 12,
                selectInput("division_select", "Select Division:",
                            choices  = c("Alpha Blue","Alpha Red","Beta Black","Beta White"),
                            selected = "Alpha Blue"))
          ),
          fluidRow(
            box(title = "Division Snapshot", status = "success", solidHeader = TRUE, width = 12,
                uiOutput("division_snapshot"))
          ),
          fluidRow(
            box(title = "Division Standings", status = "primary", solidHeader = TRUE, width = 12,
                DTOutput("division_standings"))
          ),
          fluidRow(
            box(title = "Division Performance Analysis", status = "info",
                solidHeader = TRUE, width = 12,
                plotlyOutput("division_comparison", height = "400px"))
          )
        ),
        
        # ── TEAM ─────────────────────────────────────────────────────────────
        tabItem(
          tabName = "team",
          h2("Team Analysis"),
          fluidRow(box(width = 12, selectInput("team_select", "Select Team:", choices = NULL))),
          fluidRow(uiOutput("team_stats_boxes")),
          fluidRow(
            box(title = "Team Roster", status = "primary", solidHeader = TRUE, width = 12,
                DTOutput("team_roster"))
          ),
          fluidRow(
            box(title = "Team Performance Analysis", status = "info",
                solidHeader = TRUE, width = 8,
                plotlyOutput("team_performance", height = "400px")),
            box(title = "Points Distribution", status = "info",
                solidHeader = TRUE, width = 4,
                plotlyOutput("team_scoring_breakdown", height = "400px"))
          ),
          fluidRow(
            box(title = "\U0001f4ca Advanced Team Metrics", status = "warning",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                uiOutput("team_advanced_metrics"))
          ),
          fluidRow(
            box(title = "\u2696\ufe0f Compare Teams", status = "info",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                fluidRow(
                  column(4, selectInput("compare_team_1", "Team 1:", choices = NULL)),
                  column(4, selectInput("compare_team_2", "Team 2:", choices = NULL)),
                  column(4, selectInput("compare_team_3", "Team 3 (Optional):", choices = NULL))
                ),
                uiOutput("team_comparison"))
          )
        ),
        
        # ── PLAYER ───────────────────────────────────────────────────────────
        tabItem(
          tabName = "player",
          h2("Player Analysis"),
          fluidRow(
            box(width = 8, selectInput("player_select", "Select Player:", choices = NULL)),
            box(width = 4,
                selectInput("game_range", "Game Range:",
                            choices  = c("Last 5 Games" = 5, "Last 10 Games" = 10, "Last 15 Games" = 15),
                            selected = 5))
          ),
          fluidRow(uiOutput("player_stats_boxes")),
          fluidRow(
            box(title = "Recent Game Performance", status = "primary",
                solidHeader = TRUE, width = 12,
                plotlyOutput("player_recent_games", height = "400px"))
          ),
          fluidRow(
            box(title = "Game Log",          status = "info",    solidHeader = TRUE, width = 8,
                DTOutput("player_game_log")),
            box(title = "Scoring Breakdown", status = "primary", solidHeader = TRUE, width = 4,
                plotlyOutput("player_scoring_donut", height = "350px"))
          ),
          fluidRow(
            box(title = "\U0001f4ca Advanced Player Metrics", status = "warning",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                uiOutput("player_advanced_metrics"))
          ),
          fluidRow(
            box(title = "\U0001f4c8 Player Impact Rating", status = "success",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                uiOutput("player_impact_rating"))
          ),
          fluidRow(
            box(title = "\u2696\ufe0f Compare Players", status = "info",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                fluidRow(
                  column(4, selectInput("compare_player_1", "Player 1:", choices = NULL)),
                  column(4, selectInput("compare_player_2", "Player 2:", choices = NULL)),
                  column(4, selectInput("compare_player_3", "Player 3 (Optional):", choices = NULL))
                ),
                uiOutput("player_comparison"))
          )
        ),
        
        # ── COACHING ─────────────────────────────────────────────────────────
        tabItem(
          tabName = "coaching",
          h2("Coaching Tools - Lineup Optimizer"),
          fluidRow(
            box(width = 6,
                selectInput("situation_select", "Select Situation:",
                            choices = c(
                              "Choose..."                 = "",
                              "Defense - Last Possession" = "def_last",
                              "Offense - Down 2"          = "off_down2",
                              "Offense - Down 3"          = "off_down3",
                              "Need a Stop"               = "need_stop",
                              "Free Throw Situation"      = "ft_situation",
                              "Pure Scoring"              = "scoring"
                            ))),
            box(width = 6,
                selectInput("opponent_select", "Opponent Team (Optional):", choices = NULL))
          ),
          fluidRow(
            box(title = "\U0001f3c0 Court View - Lineup Matchups", status = "primary",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                uiOutput("court_visualization"))
          ),
          fluidRow(
            box(title = "Ohio Kings Recommended Lineup", status = "primary",
                solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
                uiOutput("kings_lineup")),
            box(title = "Opponent Expected Lineup", status = "danger",
                solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
                uiOutput("opponent_lineup"))
          ),
          fluidRow(
            box(title = "\U0001f3af Situational Success Prediction", status = "success",
                solidHeader = TRUE, width = 12,
                uiOutput("situation_success"))
          ),
          fluidRow(
            box(title = "Four Factors Analysis - Ohio Kings", status = "success",
                solidHeader = TRUE, width = 12,
                uiOutput("four_factors"))
          ),
          fluidRow(
            box(title = "\U0001f3b2 Win Probability Analysis", status = "success",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                uiOutput("win_probability"))
          ),
          fluidRow(
            box(title = "\U0001f3c0 Player Matchup Predictions (Overall Performance)", status = "warning",
                solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                uiOutput("player_matchups"))
          )
        ),
        
        # ── SCHEDULE ─────────────────────────────────────────────────────────
        tabItem(
          tabName = "schedule",
          fluidRow(
            column(8, h2("Season Schedule", style = "margin-top: 15px;")),
            column(4,
                   div(style = "margin-top: 15px;",
                       selectInput("schedule_month", "Month:",
                                   choices  = c("October","November","December","January",
                                                "February","March","April","May"),
                                   selected = "February", width = "100%")))
          ),
          fluidRow(
            box(title = "\U0001f4c5 Monthly Calendar", status = "primary",
                solidHeader = TRUE, width = 12,
                uiOutput("calendar_view"))
          ),
          fluidRow(
            box(title = "\U0001f4ca Schedule Insights",   status = "success",
                solidHeader = TRUE, width = 6,
                uiOutput("schedule_insights")),
            box(title = "\u26a1 Upcoming Challenge",      status = "warning",
                solidHeader = TRUE, width = 6,
                uiOutput("next_games_preview"))
          ),
          fluidRow(
            box(title = "\U0001f3af Matchup Breakdown",         status = "info",
                solidHeader = TRUE, width = 6,
                uiOutput("schedule_matchup_breakdown")),
            box(title = "\U0001f4c8 Schedule Difficulty Curve",  status = "warning",
                solidHeader = TRUE, width = 6,
                plotlyOutput("difficulty_timeline", height = "300px"))
          ),
          fluidRow(
            box(title = "\U0001f451 Division Games Tracker", status = "success",
                solidHeader = TRUE, width = 6,
                uiOutput("division_games_tracker")),
            box(title = "\U0001f525 Key Games to Watch",    status = "danger",
                solidHeader = TRUE, width = 6,
                uiOutput("key_games_watch"))
          ),
          fluidRow(
            box(title = "\U0001f4ca Win-Loss Prediction", status = "primary",
                solidHeader = TRUE, width = 12, collapsible = TRUE,
                uiOutput("schedule_win_loss_prediction"))
          )
        ),
        
        # ── SCOUTING REPORT ──────────────────────────────────────────────────
        tabItem(
          tabName = "scouting",
          fluidRow(
            column(8, h2("Scouting Report", style = "margin-top: 15px;")),
            column(4,
                   div(style = "margin-top: 15px;",
                       selectInput("scout_team", "Select Opponent:",
                                   choices = NULL, width = "100%")))
          ),
          fluidRow(column(12, uiOutput("scouting_report")))
        )
      ) # end tabItems
    )   # end dashboardBody
  )     # end dashboardPage
)       # end conditionalPanel

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  message("Server started")
  
  # ── Reactive state ─────────────────────────────────────────────────────────
  rv <- reactiveValues(
    logged_in    = FALSE,
    aba_data     = NULL,
    current_user = NULL,
    my_team      = NULL,
    team_cols    = list(
      primary   = aba_primary,
      secondary = aba_secondary,
      accent    = aba_accent
    ),
    credentials  = list()
  )
  
  rv_custom_lineup <- reactiveValues(
    selected_slot = NULL,
    swaps         = list()
  )
  
  # ── show_dashboard controls the conditionalPanel ───────────────────────────
  output$show_dashboard <- reactive({ rv$logged_in })
  outputOptions(output, "show_dashboard", suspendWhenHidden = FALSE)
  
  # ── LINEUP SWAP HANDLERS ──────────────────────────────────────────────────
  observeEvent(input$lineup_slot_clicked, {
    rv_custom_lineup$selected_slot <- input$lineup_slot_clicked
  })
  observeEvent(input$swap_player, {
    slot   <- input$swap_player$slot
    player <- input$swap_player$player
    rv_custom_lineup$swaps[[as.character(slot)]] <- player
    rv_custom_lineup$selected_slot <- NULL
  })
  observeEvent(input$situation_select, {
    rv_custom_lineup$swaps         <- list()
    rv_custom_lineup$selected_slot <- NULL
  })
  
  # ============================================================================
  # LOGIN MODAL
  # Shows on startup and after logout.
  # Team list is populated from cache if available, otherwise placeholder.
  # No credential hint shown.
  # ============================================================================
  
  show_login_modal <- function(team_choices = character(0)) {
    
    # Build the dropdown choices — blank first entry so nothing is pre-selected
    if (length(team_choices) == 0) {
      dropdown_choices <- c("Loading teams..." = "")
    } else {
      dropdown_choices <- c("Select your team..." = "", team_choices)
    }
    
    showModal(modalDialog(
      title     = NULL,
      size      = "m",
      footer    = NULL,
      easyClose = FALSE,
      
      # ── Modal shell styling ───────────────────────────────────────────────
      tags$head(tags$style(HTML(paste0("
        .modal-backdrop {
          background: linear-gradient(135deg, #041E42 0%, #1a0a0a 50%, #1B3A6B 100%) !important;
          opacity: 0.97 !important;
        }
        .modal-content {
          border-radius: 20px !important;
          box-shadow: 0 25px 60px rgba(0,0,0,0.5) !important;
          border: 1px solid rgba(255,255,255,0.08) !important;
          overflow: hidden;
        }
        .modal-body { padding: 35px !important; }
        @keyframes pulse {
          0%, 100% { transform: scale(1); box-shadow: 0 4px 15px rgba(27,58,107,0.35); }
          50%       { transform: scale(1.05); box-shadow: 0 8px 25px rgba(27,58,107,0.5); }
        }
        @keyframes fadeInDown {
          from { opacity: 0; transform: translateY(-20px); }
          to   { opacity: 1; transform: translateY(0); }
        }
        .login-container { animation: fadeInDown 0.4s ease; }
        .login-team-select .selectize-input {
          border-radius: 10px !important;
          border: 2px solid #e2e8f0 !important;
          padding: 10px 14px !important;
          font-size: 0.95em !important;
          min-height: 44px !important;
        }
        .login-team-select .selectize-input.focus {
          border-color: ", aba_primary, " !important;
          box-shadow: 0 0 0 3px rgba(27,58,107,0.15) !important;
        }
        #login_btn {
          background: linear-gradient(135deg, ", aba_primary, ", ", aba_secondary, ") !important;
          border: none !important;
          border-radius: 10px !important;
          font-weight: 700 !important;
          font-size: 1em !important;
          padding: 13px !important;
          transition: all 0.2s !important;
          box-shadow: 0 4px 15px rgba(27,58,107,0.3);
        }
        #login_btn:hover {
          transform: translateY(-1px);
          box-shadow: 0 6px 20px rgba(27,58,107,0.4) !important;
        }
      ")))),
      
      div(
        class = "login-container",
        style = "text-align: center;",
        
        # ── Animated logo ─────────────────────────────────────────────────
        div(
          style = paste0(
            "width: 110px; height: 110px; margin: 0 auto 24px; ",
            "background: linear-gradient(135deg, ", aba_primary, " 0%, ", aba_secondary, " 100%); ",
            "border-radius: 50%; display: flex; align-items: center; justify-content: center; ",
            "animation: pulse 2.5s infinite;"
          ),
          icon("basketball-ball", style = "font-size: 52px; color: white;")
        ),
        
        # ── Title ─────────────────────────────────────────────────────────
        h1("ABA Scouting Portal",
           style = "margin: 0 0 6px 0; color: #1e293b; font-weight: 900; font-size: 1.9em; letter-spacing: -0.5px;"),
        p("Sign in to access your team dashboard",
          style = "color: #64748b; margin: 0 0 28px 0; font-size: 0.95em;"),
        
        # ── Divider ───────────────────────────────────────────────────────
        div(style = "height: 1px; background: linear-gradient(90deg, transparent, #e2e8f0, transparent); margin-bottom: 24px;"),
        
        # ── Team selector ─────────────────────────────────────────────────
        div(
          style = "text-align: left; margin-bottom: 16px;",
          tags$label("Your Team",
                     style = "font-size: 0.85em; font-weight: 700; color: #475569;
                              display: block; margin-bottom: 6px; letter-spacing: 0.3px;"),
          div(class = "login-team-select",
              selectInput("login_team", label = NULL,
                          choices  = dropdown_choices,
                          selected = "",
                          width    = "100%"))
        ),
        
        # ── Username ──────────────────────────────────────────────────────
        div(
          style = "text-align: left; margin-bottom: 14px;",
          tags$label("Username",
                     style = "font-size: 0.85em; font-weight: 700; color: #475569;
                              display: block; margin-bottom: 6px; letter-spacing: 0.3px;"),
          textInput("username", label = NULL,
                    placeholder = "Enter your username", width = "100%")
        ),
        
        # ── Password ──────────────────────────────────────────────────────
        div(
          style = "text-align: left; margin-bottom: 24px;",
          tags$label("Password",
                     style = "font-size: 0.85em; font-weight: 700; color: #475569;
                              display: block; margin-bottom: 6px; letter-spacing: 0.3px;"),
          passwordInput("password", label = NULL,
                        placeholder = "Enter your password", width = "100%")
        ),
        
        # ── Login button ──────────────────────────────────────────────────
        actionButton("login_btn", "Sign In to Dashboard",
                     class = "btn-primary btn-block",
                     style = "width: 100%;"),
        
        # ── Error message ─────────────────────────────────────────────────
        uiOutput("login_message"),
        
        # ── Footer ────────────────────────────────────────────────────────
        div(style = "margin-top: 28px; padding-top: 20px;
                     border-top: 1px solid #f1f5f9;",
            p("American Basketball Association",
              style = "margin: 0; color: #94a3b8; font-size: 0.8em; font-weight: 600;"),
            p("Central Region Scouting System",
              style = "margin: 4px 0 0 0; color: #cbd5e1; font-size: 0.75em;"))
      )
    ))
  }
  
  # ── ON STARTUP: load cache silently, then show login ──────────────────────
  observeEvent(session$clientData$url_hostname, {
    isolate({
      cache_file <- "aba_data_cache.rds"
      if (file.exists(cache_file)) {
        tryCatch({
          rv$aba_data <- readRDS(cache_file)
          rv$aba_data$last_updated <- file.info(cache_file)$mtime
          message("Cache loaded on startup")
        }, error = function(e) message("Cache read error: ", e$message))
      }
    })
    
    team_choices <- if (!is.null(rv$aba_data) && !is.null(rv$aba_data$team_lookup))
      sort(rv$aba_data$team_lookup$team_name)
    else
      character(0)
    
    show_login_modal(team_choices)
    
  }, once = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE)
  
  # ── LOGIN HANDLER ─────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    
    # If no cache at all, do a full scrape first (first-ever run)
    if (is.null(rv$aba_data)) {
      withProgress(message = "First-time setup: scraping all data (~20 min)...", value = 0.1, {
        rv$aba_data <- load_aba_data(force_refresh = FALSE)
        incProgress(1, detail = "Done!")
      })
    }
    
    # Build credentials from the full team list
    team_names     <- rv$aba_data$team_lookup$team_name
    rv$credentials <- build_team_credentials(team_names)
    
    username <- trimws(input$username)
    password <- trimws(input$password)
    
    matched_team <- NULL
    
    # Primary check: match typed username/password against credential map
    for (key in names(rv$credentials)) {
      cred <- rv$credentials[[key]]
      if (tolower(key) == tolower(username) &&
          tolower(cred$password) == tolower(password)) {
        matched_team <- cred$team_name
        break
      }
    }
    
    # Fallback: also validate against the dropdown selection
    # (handles edge cases where team name has unusual spacing)
    if (is.null(matched_team) && !is.null(input$login_team) && input$login_team != "") {
      selected_team <- input$login_team
      parts         <- strsplit(trimws(selected_team), "\\s+")[[1]]
      expected_user <- parts[1]
      expected_pass <- tail(parts, 1)
      if (tolower(username) == tolower(expected_user) &&
          tolower(password) == tolower(expected_pass)) {
        matched_team <- selected_team
      }
    }
    
    if (!is.null(matched_team)) {
      
      # ── Successful login ──────────────────────────────────────────────────
      rv$logged_in    <- TRUE
      rv$current_user <- username
      rv$my_team      <- matched_team
      rv$team_cols    <- get_team_colors(matched_team)
      
      removeModal()
      
      # Populate all dropdowns with full league data
      all_teams   <- rv$aba_data$team_lookup$team_name
      all_players <- rv$aba_data$player_lookup$player_name
      
      updateSelectInput(session, "team_select",
                        choices  = all_teams,
                        selected = matched_team)
      updateSelectInput(session, "opponent_select",
                        choices  = c("Select opponent..." = "", all_teams))
      updateSelectInput(session, "scout_team",
                        choices  = c("Select a team..." = "", all_teams))
      updateSelectizeInput(session, "player_select",
                           choices = all_players, server = TRUE)
      updateSelectizeInput(session, "compare_player_1",
                           choices = c("Select player..." = "", all_players), server = TRUE)
      updateSelectizeInput(session, "compare_player_2",
                           choices = c("Select player..." = "", all_players), server = TRUE)
      updateSelectizeInput(session, "compare_player_3",
                           choices = c("Select player..." = "", all_players), server = TRUE)
      updateSelectInput(session, "compare_team_1",
                        choices  = c("Select team..." = "", all_teams))
      updateSelectInput(session, "compare_team_2",
                        choices  = c("Select team..." = "", all_teams))
      updateSelectInput(session, "compare_team_3",
                        choices  = c("Select team..." = "", all_teams))
      
    } else {
      
      # ── Failed login ──────────────────────────────────────────────────────
      output$login_message <- renderUI({
        div(
          style = "margin-top: 16px; padding: 12px 16px;
                   background: #fef2f2; border: 1px solid #fecaca;
                   border-radius: 10px; display: flex; align-items: center; gap: 10px;",
          icon("exclamation-circle", style = "color: #ef4444; font-size: 1.1em;"),
          span("Invalid username or password. Please try again.",
               style = "color: #991b1b; font-weight: 600; font-size: 0.9em;")
        )
      })
    }
  })
  
  # ── LOGOUT — resets state and re-shows login modal ─────────────────────────
  observeEvent(input$logout_btn, {
    rv$logged_in    <- FALSE
    rv$current_user <- NULL
    rv$my_team      <- NULL
    rv$team_cols    <- list(
      primary   = aba_primary,
      secondary = aba_secondary,
      accent    = aba_accent
    )
    # Clear the error message so it doesn't persist on re-login
    output$login_message <- renderUI({ NULL })
    
    team_choices <- if (!is.null(rv$aba_data) && !is.null(rv$aba_data$team_lookup))
      sort(rv$aba_data$team_lookup$team_name)
    else
      character(0)
    
    show_login_modal(team_choices)
  })
  
  # ── DARK MODE ─────────────────────────────────────────────────────────────
  observeEvent(input$dark_mode_btn, {
    session$sendCustomMessage("toggleDarkMode", list())
  })
  
  # ── REFRESH DATA ──────────────────────────────────────────────────────────
  observeEvent(input$refresh_data_btn, {
    withProgress(message = "Refreshing ABA data...", value = 0, {
      incProgress(0.1, detail = "Scraping fresh data...")
      rv$aba_data <- load_aba_data(force_refresh = TRUE)
      incProgress(1, detail = "Done!")
    })
    showNotification("Data refreshed successfully!", type = "message", duration = 3)
  })
  
  # ── QUICK SEARCH ──────────────────────────────────────────────────────────
  observeEvent(input$quick_search, {
    req(rv$logged_in, rv$aba_data)
    search_term <- tolower(trimws(input$quick_search))
    if (nchar(search_term) < 2) return()
    
    team_match <- rv$aba_data$team_lookup %>%
      filter(str_detect(tolower(team_name), search_term))
    if (nrow(team_match) > 0) {
      updateSelectInput(session, "team_select", selected = team_match$team_name[1])
      updateTabItems(session, "tabs", "team")
      return()
    }
    
    player_match <- rv$aba_data$player_lookup %>%
      filter(str_detect(tolower(player_name), search_term))
    if (nrow(player_match) > 0) {
      updateSelectInput(session, "player_select", selected = player_match$player_name[1])
      updateTabItems(session, "tabs", "player")
      return()
    }
    
    showNotification(paste0("No results for '", input$quick_search, "'"),
                     type = "warning", duration = 3)
  })
  
  # ── LAZY LOAD INDIVIDUAL PLAYER DATA ──────────────────────────────────────
  current_player_data <- reactive({
    req(rv$logged_in, input$player_select, rv$aba_data)
    player_name <- input$player_select
    
    if (player_name %in% names(rv$aba_data$all_player_stats)) {
      pd <- rv$aba_data$all_player_stats[[player_name]]
      if (!is.null(pd) && !is.null(pd$per_game)) return(pd)
    }
    
    player_id <- rv$aba_data$player_lookup %>%
      filter(player_name == !!player_name) %>%
      pull(player_id)
    
    if (length(player_id) > 0 && !is.na(player_id[1])) {
      withProgress(message = paste("Loading", player_name, "..."), value = 0.5, {
        stats <- tryCatch(get_indiv_stats(player_id[1]), error = function(e) NULL)
        if (!is.null(stats) && !is.null(stats$per_game)) {
          rv$aba_data$all_player_stats[[player_name]] <- stats
          return(stats)
        }
      })
    }
    
    list(
      per_game = data.frame(PPG = 0, RPG = 0, APG = 0, SPG = 0, BPG = 0,
                            FG2_pct = 0, FG3_pct = 0, FT_pct = 0),
      game_log = data.frame()
    )
  })
  
  # ============================================================================
  # DYNAMIC CSS — injects team colors after login, ABA colors before login
  # Full original CSS block with every selector, dark mode, skeleton, scrollbar
  # ============================================================================
  
  output$dynamic_css <- renderUI({
    p <- rv$team_cols$primary
    s <- rv$team_cols$secondary
    a <- rv$team_cols$accent
    
    light_accents <- c("#FDBB30","#FDB927","#FFC72C","#F5A623","#BA9653",
                       "#EEE1C6","#FFFFFF","#C4CED4","#fca5a5","#F5B112",
                       "#EEE1C6","#FFC72C","#FDB927")
    accent_text <- if (a %in% light_accents) "#1a1a2e" else "white"
    
    tags$style(HTML(paste0("

    /* ── Font ── */
    body, h1, h2, h3, h4, h5, p, span, div, td, th, label, input, select, button {
      font-family: 'Inter', sans-serif !important;
    }

    /* ── Background ── */
    body, html { margin: 0; padding: 0; background: #0f172a !important; }
    .content-wrapper { background: #f0f4f8 !important; }

    /* ── Header ── */
    .skin-red .main-header .navbar {
      background: linear-gradient(90deg, ", p, " 0%, ", s, " 100%) !important;
      border-bottom: 3px solid ", a, " !important;
    }
    .skin-red .main-header .logo {
      background: ", s, " !important;
      border-bottom: 3px solid ", a, " !important;
      font-weight: 800 !important;
      color: white !important;
    }
    .skin-red .main-header .logo:hover { background: ", p, " !important; }

    /* ── Sidebar ── */
    .main-sidebar {
      background: linear-gradient(180deg, ", s, " 0%, #020f21 100%) !important;
      box-shadow: 4px 0 20px rgba(0,0,0,0.4);
    }
    .sidebar-menu > li > a {
      color: #94a3b8 !important;
      font-weight: 500;
      font-size: 0.92em;
      border-left: 3px solid transparent;
      transition: all 0.2s ease;
      padding: 12px 15px 12px 20px !important;
      display: flex !important;
      align-items: center !important;
    }
    .sidebar-menu > li > a:hover {
      color: white !important;
      background: rgba(255,255,255,0.07) !important;
      border-left: 3px solid ", a, " !important;
    }
    .sidebar-menu > li.active > a,
    .skin-red .sidebar-menu > li.active > a,
    .skin-red .sidebar-menu > li.active > a:hover {
      color: white !important;
      background: linear-gradient(90deg, rgba(255,255,255,0.15), rgba(255,255,255,0.03)) !important;
      border-left: 3px solid ", a, " !important;
      font-weight: 700;
    }
    .sidebar-menu > li > a > .fa,
    .sidebar-menu > li > a > .glyphicon,
    .sidebar-menu > li > a > i {
      color: ", a, " !important;
      width: 20px !important;
      text-align: center !important;
      margin-right: 10px !important;
      font-size: 1em !important;
      flex-shrink: 0;
    }
    .sidebar-menu > li.active > a > .fa,
    .sidebar-menu > li.active > a > .glyphicon,
    .sidebar-menu > li.active > a > i { color: white !important; }
    .sidebar-menu > li > a > .pull-right-container { margin-left: auto; }

    /* ── Boxes ── */
    .box {
      border-radius: 12px !important;
      box-shadow: 0 2px 12px rgba(0,0,0,0.06) !important;
      border: none !important;
      overflow: visible !important;
      transition: box-shadow 0.2s ease, transform 0.2s ease;
    }
    .box-body { overflow: visible !important; background: white; padding: 18px !important; }
    .box:hover { box-shadow: 0 8px 24px rgba(0,0,0,0.1) !important; }

    .box.box-primary > .box-header {
      background: linear-gradient(135deg, ", p, " 0%, ", s, " 100%) !important;
      color: white !important; border-bottom: none; padding: 14px 18px;
    }
    .box.box-info > .box-header {
      background: linear-gradient(135deg, ", s, " 0%, #062a5e 100%) !important;
      color: white !important; border-bottom: none; padding: 14px 18px;
    }
    .box.box-success > .box-header {
      background: linear-gradient(135deg, #15803d 0%, #22c55e 100%) !important;
      color: white !important; border-bottom: none; padding: 14px 18px;
    }
    .box.box-warning > .box-header {
      background: linear-gradient(135deg, #d97706 0%, ", a, " 100%) !important;
      color: ", accent_text, " !important; border-bottom: none; padding: 14px 18px;
    }
    .box.box-danger > .box-header {
      background: linear-gradient(135deg, #b91c1c 0%, #ef4444 100%) !important;
      color: white !important; border-bottom: none; padding: 14px 18px;
    }
    .box-header .box-title { font-weight: 700 !important; font-size: 0.95em !important; letter-spacing: 0.3px; }

    /* ── DataTables ── */
    table.dataTable thead th {
      background: ", s, " !important;
      color: white !important;
      font-weight: 700 !important;
      font-size: 0.82em !important;
      letter-spacing: 0.5px !important;
      text-transform: uppercase;
      border-bottom: 2px solid ", p, " !important;
      padding: 12px 10px !important;
    }
    table.dataTable tbody tr:nth-child(odd)  { background: #f8fafc !important; }
    table.dataTable tbody tr:nth-child(even) { background: white !important; }
    table.dataTable tbody tr:hover           { background: #e0f2fe !important; }
    table.dataTable tbody td {
      font-size: 0.88em;
      padding: 10px !important;
      border-bottom: 1px solid #f1f5f9 !important;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background: ", p, " !important; color: white !important;
      border-radius: 6px; border: none !important;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background: ", s, " !important; color: white !important; border-radius: 6px;
    }

    /* ── Selectize ── */
    .selectize-input {
      border-radius: 8px !important;
      border: 1.5px solid #e2e8f0 !important;
      font-size: 0.9em !important;
      font-weight: 500;
      min-height: 38px !important;
      box-shadow: 0 1px 3px rgba(0,0,0,0.05) !important;
    }
    .selectize-input.focus {
      border-color: ", p, " !important;
      box-shadow: 0 0 0 3px rgba(27,58,107,0.1) !important;
    }
    .selectize-input > input { font-size: 0.9em !important; min-width: 80px !important; }
    .selectize-dropdown {
      border-radius: 8px !important;
      border: 1.5px solid #e2e8f0 !important;
      box-shadow: 0 8px 24px rgba(0,0,0,0.12) !important;
      max-height: 300px !important;
      overflow-y: auto !important;
      z-index: 99999 !important;
      position: absolute !important;
    }
    .selectize-dropdown-content { max-height: 280px !important; overflow-y: auto !important; }
    .selectize-dropdown .option { padding: 8px 12px !important; font-size: 0.9em; }
    .selectize-dropdown .option:hover,
    .selectize-dropdown .option.selected {
      background: #dbeafe !important; color: ", p, " !important;
    }
    .form-control {
      border-radius: 8px !important;
      border: 1.5px solid #e2e8f0 !important;
      font-size: 0.9em !important;
      height: 38px !important;
    }
    .form-control:focus {
      border-color: ", p, " !important;
      box-shadow: 0 0 0 3px rgba(27,58,107,0.1) !important;
    }

    /* ── Buttons ── */
    .btn-primary {
      background: linear-gradient(135deg, ", p, ", ", s, ") !important;
      border: none !important;
      border-radius: 8px !important;
      font-weight: 700 !important;
      color: white !important;
      box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      transition: all 0.2s;
    }
    .btn-primary:hover { box-shadow: 0 4px 14px rgba(0,0,0,0.3) !important; transform: translateY(-1px); }
    .btn-warning {
      background: linear-gradient(135deg, ", a, ", #e6a800) !important;
      border: none !important;
      border-radius: 8px !important;
      color: ", accent_text, " !important;
      font-weight: 700 !important;
      box-shadow: 0 2px 8px rgba(0,0,0,0.15);
      transition: all 0.2s;
    }
    .btn-warning:hover { box-shadow: 0 4px 14px rgba(0,0,0,0.25) !important; }

    /* ── Refresh bar ── */
    .refresh-info {
      background: white;
      border-left: 4px solid ", p, ";
      padding: 10px 16px;
      margin: 10px 0;
      border-radius: 8px;
      font-size: 0.88em;
      color: #475569;
      box-shadow: 0 1px 4px rgba(0,0,0,0.05);
    }

    /* ── Scrollbar ── */
    ::-webkit-scrollbar       { width: 6px; height: 6px; }
    ::-webkit-scrollbar-track { background: #f1f5f9; }
    ::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 3px; }
    ::-webkit-scrollbar-thumb:hover { background: ", p, "; }

    /* ── Skeleton loader ── */
    .skeleton {
      background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
      background-size: 200% 100%;
      animation: loading 1.5s infinite;
      border-radius: 8px;
    }
    @keyframes loading {
      0%   { background-position: 200% 0; }
      100% { background-position: -200% 0; }
    }
    .skeleton-text  { height: 16px; margin: 8px 0; }
    .skeleton-title { height: 24px; width: 60%; margin: 12px 0; }
    .skeleton-stat  { height: 80px; border-radius: 12px; }

    /* ── Dark mode ── */
    body.dark-mode .content-wrapper { background: #0f172a !important; color: #e2e8f0 !important; }
    body.dark-mode .box             { background: #1e293b !important; }
    body.dark-mode .box-body        { background: #1e293b !important; color: #cbd5e1 !important; }
    body.dark-mode table.dataTable tbody tr:nth-child(odd)  { background: #1e293b !important; color: #cbd5e1 !important; }
    body.dark-mode table.dataTable tbody tr:nth-child(even) { background: #243044 !important; color: #cbd5e1 !important; }
    body.dark-mode table.dataTable tbody tr:hover           { background: #334155 !important; }
    body.dark-mode table.dataTable thead th                 { background: #0f172a !important; color: #94a3b8 !important; }
    body.dark-mode .refresh-info    { background: #1e293b !important; color: #94a3b8 !important; }
    body.dark-mode h1, body.dark-mode h2,
    body.dark-mode h3, body.dark-mode h4 { color: #e2e8f0 !important; }
    body.dark-mode .selectize-input    { background: #334155 !important; color: #e2e8f0 !important; border-color: #475569 !important; }
    body.dark-mode .selectize-dropdown { background: #1e293b !important; color: #e2e8f0 !important; }
    body.dark-mode .selectize-dropdown .option:hover { background: #334155 !important; }
    body.dark-mode .form-control       { background: #334155 !important; color: #e2e8f0 !important; border-color: #475569 !important; }
    body.dark-mode .main-sidebar       { background: linear-gradient(180deg, #0f172a 0%, #020812 100%) !important; }

    ")))
  })
  
  # ── HEADER TITLE ──────────────────────────────────────────────────────────
  output$header_title <- renderUI({
    if (!is.null(rv$my_team) && rv$logged_in)
      span(paste(rv$my_team, "Scouting"), style = "font-weight: 800;")
    else
      span("ABA Scouting Portal", style = "font-weight: 800;")
  })
  
  # ── SIDEBAR LOGO ──────────────────────────────────────────────────────────
  output$sidebar_logo <- renderUI({
    p <- rv$team_cols$primary
    s <- rv$team_cols$secondary
    
    if (!rv$logged_in || is.null(rv$aba_data) || is.null(rv$my_team)) {
      return(div(
        style = paste0(
          "width: 80px; height: 80px; margin: 0 auto; ",
          "background: linear-gradient(135deg, ", aba_primary, ", ", aba_secondary, "); ",
          "border-radius: 50%; display: flex; align-items: center; justify-content: center;"
        ),
        icon("basketball-ball", style = "font-size: 35px; color: white;")
      ))
    }
    
    logo_url <- NA_character_
    if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
      lr <- rv$aba_data$team_lookup %>%
        filter(team_name == rv$my_team) %>%
        pull(logo_url)
      if (length(lr) > 0 && !is.na(lr[1])) logo_url <- lr[1]
    }
    
    if (!is.na(logo_url)) {
      tags$img(src   = logo_url,
               style = paste0(
                 "width: 80px; height: 80px; border-radius: 50%; ",
                 "border: 3px solid ", p, "; object-fit: cover; background: white;"
               ))
    } else {
      div(
        style = paste0(
          "width: 80px; height: 80px; margin: 0 auto; ",
          "background: linear-gradient(135deg, ", p, ", ", s, "); ",
          "border-radius: 50%; display: flex; align-items: center; justify-content: center;"
        ),
        icon("basketball-ball", style = "font-size: 35px; color: white;")
      )
    }
  })
  
  # ── SIDEBAR TEAM NAME ─────────────────────────────────────────────────────
  output$sidebar_team_name <- renderUI({
    if (!is.null(rv$my_team) && rv$logged_in)
      p(rv$my_team,
        style = "color: #adb5bd; font-size: 0.8em; margin: 5px 0 0 0; font-weight: 600;")
    else
      p("Scouting Dashboard",
        style = "color: #adb5bd; font-size: 0.8em; margin: 5px 0 0 0;")
  })
  
  # ── DATA STATUS BAR ───────────────────────────────────────────────────────
  output$data_update_info <- renderUI({
    req(rv$logged_in, rv$aba_data)
    div(class = "refresh-info",
        HTML(paste0(
          "<strong>\U0001f4ca Data Status:</strong> Last updated ",
          format(rv$aba_data$last_updated, "%B %d, %Y at %I:%M %p"),
          " | ", length(rv$aba_data$all_team_stats),  " teams loaded",
          " | ", length(rv$aba_data$all_player_stats), " players loaded",
          " | Logged in as: <strong>", rv$my_team, "</strong>"
        )))
  })
  
  # ==========================================================================
  # LEAGUE OVERVIEW OUTPUTS
  # ==========================================================================
  
  output$standings_race <- renderPlotly({
    req(rv$logged_in, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    standings <- rv$aba_data$league_data %>%
      select(Team, Division, W, L) %>%
      mutate(
        GP      = W + L,
        Win_Pct = ifelse(GP > 0, round(W / GP * 100, 1), 0)
      ) %>%
      arrange(desc(Win_Pct), desc(W)) %>%
      head(30) %>%
      mutate(
        Team_Short = sapply(Team, function(t) {
          parts <- strsplit(t, " ")[[1]]
          if (length(parts) > 1) tail(parts, 1) else t
        }),
        bar_color = ifelse(Team == rv$my_team, ohio_kings_primary,
                           ifelse(Division == "Alpha Blue",  "#3b82f6",
                                  ifelse(Division == "Alpha Red",   "#ef4444",
                                         ifelse(Division == "Beta Black",  "#1e293b", "#94a3b8")))),
        is_my_team = Team == rv$my_team
      ) %>%
      arrange(Win_Pct)
    
    plot_ly(standings,
            y = ~reorder(Team_Short, Win_Pct),
            x = ~Win_Pct,
            type = 'bar',
            orientation = 'h',
            marker = list(
              color = ~bar_color,
              line  = list(
                color = ifelse(standings$is_my_team, ohio_kings_accent, 'rgba(0,0,0,0)'),
                width = ifelse(standings$is_my_team, 3, 0)
              )
            ),
            text         = ~paste0(W, "-", L, " (", Win_Pct, "%)"),
            textposition = 'auto',
            textfont     = list(size = 10),
            hoverinfo    = 'text',
            hovertext    = ~paste0(Team, "\n", Division, "\n", W, "-", L,
                                   "\nW PCT: ", Win_Pct, "%")
    ) %>%
      layout(
        xaxis      = list(title = "W PCT (%)", range = c(0, 105),
                          showgrid = FALSE, showticklabels = FALSE),
        yaxis      = list(title = ""),
        margin     = list(l = 120),
        showlegend = FALSE,
        height     = max(500, nrow(standings) * 25)
      )
  })
  
  output$league_standings <- renderDT({
    req(rv$logged_in, rv$aba_data)
    ohio_kings_primary <- rv$team_cols$primary
    
    standings <- rv$aba_data$league_data %>%
      select(Team, Division, W, L) %>%
      arrange(desc(as.numeric(W) / (as.numeric(W) + as.numeric(L)))) %>%
      mutate(
        W       = as.numeric(W),
        L       = as.numeric(L),
        'W PCT' = round(W / (W + L), 3),
        GB      = ((first(W) - W) + (L - first(L))) / 2,
        GB      = ifelse(is.na(GB) | GB == 0, "-", as.character(GB))
      )
    
    if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
      standings <- standings %>%
        left_join(rv$aba_data$team_lookup %>% select(team_name, logo_url),
                  by = c("Team" = "team_name")) %>%
        mutate(
          Team = paste0(
            '<div style="display:flex;align-items:center;gap:10px;">',
            '<img src="', ifelse(is.na(logo_url), '', logo_url),
            '" style="width:28px;height:28px;border-radius:50%;object-fit:cover;background:#f0f0f0;"',
            ' onerror="this.style.display=\'none\'">',
            '<span>', Team, '</span></div>'
          )
        ) %>%
        select(-logo_url)
    }
    
    datatable(
      standings,
      options   = list(pageLength = 50, dom = 't',
                       scrollY = "500px", scrollCollapse = TRUE),
      selection = 'none',
      rownames  = FALSE,
      escape    = FALSE,
      class     = 'cell-border stripe hover'
    ) %>%
      formatStyle('Team', cursor = 'pointer', color = ohio_kings_primary)
  })
  
  observeEvent(input$league_standings_cell_clicked, {
    info <- input$league_standings_cell_clicked
    if (!is.null(info$value) && info$col == 0) {
      updateSelectInput(session, "team_select", selected = info$value)
      updateTabItems(session, "tabs", "team")
    }
  })
  
  output$top_players <- renderDT({
    req(rv$logged_in, rv$aba_data)
    ohio_kings_primary <- rv$team_cols$primary
    stat_col           <- toupper(input$player_stat_select)
    
    player_stats <- lapply(names(rv$aba_data$all_player_stats), function(pname) {
      stats <- rv$aba_data$all_player_stats[[pname]]$per_game
      if (!is.null(stats) && nrow(stats) > 0 && !is.na(stats[[stat_col]][1])) {
        player_team <- "Unknown"
        for (tn in names(rv$aba_data$all_team_stats)) {
          if (pname %in% rv$aba_data$all_team_stats[[tn]]$Player) {
            player_team <- tn; break
          }
        }
        games_played <- if (!is.null(rv$aba_data$all_player_stats[[pname]]$game_log))
          nrow(rv$aba_data$all_player_stats[[pname]]$game_log) else 0
        data.frame(Player = pname, Team = player_team,
                   Stat   = round(stats[[stat_col]][1], 2),
                   Games  = games_played, stringsAsFactors = FALSE)
      }
    })
    
    player_df <- bind_rows(player_stats) %>%
      filter(!is.na(Stat), Games >= 3) %>%
      arrange(desc(Stat)) %>%
      head(5) %>%
      select(-Games)
    
    colnames(player_df) <- c("Player", "Team", input$player_stat_select)
    
    datatable(player_df,
              options  = list(pageLength = 5, dom = 't'),
              rownames = FALSE, escape = FALSE) %>%
      formatStyle('Player', cursor = 'pointer', color = ohio_kings_primary,
                  textDecoration = 'underline') %>%
      formatStyle('Team', cursor = 'pointer', color = ohio_kings_primary,
                  textDecoration = 'underline')
  })
  
  observeEvent(input$top_players_cell_clicked, {
    info <- input$top_players_cell_clicked
    if (!is.null(info$value)) {
      if (info$col == 0) {
        updateSelectInput(session, "player_select", selected = info$value)
        updateTabItems(session, "tabs", "player")
      } else if (info$col == 1) {
        updateSelectInput(session, "team_select", selected = info$value)
        updateTabItems(session, "tabs", "team")
      }
    }
  })
  
  output$top_teams <- renderDT({
    req(rv$logged_in, rv$aba_data)
    ohio_kings_primary <- rv$team_cols$primary
    stat_col           <- input$team_stat_select
    
    team_stats_list <- lapply(names(rv$aba_data$all_team_stats), function(team_name) {
      roster      <- rv$aba_data$all_team_stats[[team_name]]
      if (is.null(roster) || nrow(roster) == 0) return(NULL)
      total_row   <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
      team_record <- rv$aba_data$league_data %>% filter(Team == team_name)
      gp          <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
      
      stat_value <- if (stat_col == "PPG") {
        v <- get_accurate_team_ppg(team_name, rv$aba_data)
        if (v == 0 && nrow(total_row) > 0 && "PTS" %in% names(total_row))
          as.numeric(total_row$PTS[1]) / gp else v
      } else if (stat_col == "RPG" && nrow(total_row) > 0 && "REB" %in% names(total_row)) {
        as.numeric(total_row$REB[1]) / gp
      } else if (stat_col == "APG" && nrow(total_row) > 0 && "AST" %in% names(total_row)) {
        as.numeric(total_row$AST[1]) / gp
      } else { 0 }
      
      data.frame(Team = team_name, Stat = round(stat_value, 2), stringsAsFactors = FALSE)
    })
    
    team_df <- bind_rows(team_stats_list) %>%
      filter(!is.na(Stat), Stat > 0) %>%
      arrange(desc(Stat)) %>%
      head(5)
    
    colnames(team_df) <- c("Team", stat_col)
    
    datatable(team_df,
              options  = list(pageLength = 5, dom = 't'),
              rownames = FALSE, escape = FALSE) %>%
      formatStyle('Team', cursor = 'pointer', color = ohio_kings_primary,
                  textDecoration = 'underline')
  })
  
  observeEvent(input$top_teams_cell_clicked, {
    info <- input$top_teams_cell_clicked
    if (!is.null(info$value) && info$col == 0) {
      updateSelectInput(session, "team_select", selected = info$value)
      updateTabItems(session, "tabs", "team")
    }
  })
  
  output$playoff_bracket <- renderUI({
    req(rv$logged_in, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    my_team              <- rv$my_team
    
    lp <- function(wpa, wpb) as.integer(round(100 / (1 + exp(-4 * (wpa - wpb))), 0))
    
    sim_4team_bracket <- function(teams_df, n_sim = 10000) {
      t <- teams_df$Team; wp <- teams_df$WP
      wins <- setNames(rep(0L, 4), t)
      for (i in seq_len(n_sim)) {
        p14 <- lp(wp[1], wp[4]) / 100; p23 <- lp(wp[2], wp[3]) / 100
        w1  <- if (runif(1) < p14) t[1] else t[4]
        w2  <- if (runif(1) < p23) t[2] else t[3]
        wp_w1 <- wp[which(t == w1)]; wp_w2 <- wp[which(t == w2)]
        champ <- if (runif(1) < lp(wp_w1, wp_w2) / 100) w1 else w2
        wins[champ] <- wins[champ] + 1L
      }
      round(wins / n_sim * 100, 0)
    }
    
    set.seed(42)
    
    std <- rv$aba_data$league_data %>%
      mutate(W = as.numeric(W), L = as.numeric(L),
             GP = W + L, WP = ifelse(GP > 0, W / GP, 0))
    
    divisions <- unique(std$Division)
    
    div_seeded <- std %>%
      group_by(Division) %>%
      arrange(desc(WP), desc(W), .by_group = TRUE) %>%
      mutate(div_seed = row_number(), div_size = n()) %>%
      ungroup()
    
    process_division <- function(div_name) {
      teams <- div_seeded %>% filter(Division == div_name) %>% arrange(div_seed)
      n     <- nrow(teams)
      
      if (n <= 4) {
        bracket_4    <- teams %>% head(4)
        playin_games <- list()
      } else {
        n_playin_games <- n - 4
        auto_seeds     <- 4 - n_playin_games
        auto_teams     <- teams %>% filter(div_seed <= auto_seeds)
        playin_teams   <- teams %>% filter(div_seed > auto_seeds) %>% arrange(div_seed)
        playin_games   <- lapply(seq_len(n_playin_games), function(i) {
          high <- playin_teams[i, ]
          low  <- playin_teams[nrow(playin_teams) - i + 1, ]
          prob <- lp(high$WP, low$WP)
          list(high = high, low = low, win_high_prob = prob,
               predicted_winner = if (prob >= 50) high$Team else low$Team,
               predicted_wp     = if (prob >= 50) high$WP   else low$WP,
               predicted_seed   = auto_seeds + i)
        })
        playin_winners <- lapply(playin_games, function(pg) {
          tibble(Team = pg$predicted_winner, WP = pg$predicted_wp,
                 div_seed = pg$predicted_seed, div_size = n,
                 Division = div_name, W = 0, L = 0, GP = 0)
        })
        bracket_4 <- bind_rows(auto_teams, bind_rows(playin_winners)) %>%
          arrange(div_seed) %>% head(4)
      }
      
      if (nrow(bracket_4) < 2) {
        champ_probs <- setNames(c(100), bracket_4$Team[1])
      } else if (nrow(bracket_4) < 4) {
        prob_val    <- lp(bracket_4$WP[1], bracket_4$WP[min(2, nrow(bracket_4))])
        champ_probs <- setNames(c(prob_val, 100 - prob_val), bracket_4$Team[1:2])
      } else {
        champ_probs <- sim_4team_bracket(bracket_4)
      }
      
      wp <- bracket_4$WP; t <- bracket_4$Team
      semi1_p   <- if (length(t) >= 4) lp(wp[1], wp[4]) else 80L
      semi2_p   <- if (length(t) >= 3) lp(wp[2], wp[3]) else 80L
      semi1_win <- if (semi1_p >= 50) t[1] else t[min(4, length(t))]
      semi2_win <- if (semi2_p >= 50) t[2] else t[min(3, length(t))]
      final_wp1 <- wp[which(t == semi1_win)][1]
      final_wp2 <- wp[which(t == semi2_win)][1]
      final_p   <- lp(final_wp1, final_wp2)
      div_champ <- if (final_p >= 50) semi1_win else semi2_win
      
      list(name = div_name, all_teams = teams, bracket_4 = bracket_4,
           playin_games = playin_games, champ_probs = champ_probs,
           div_champ = div_champ,
           semi1 = list(t1 = t[1], t4 = t[min(4,length(t))],
                        p1 = semi1_p, winner = semi1_win),
           semi2 = list(t2 = t[2], t3 = t[min(3,length(t))],
                        p2 = semi2_p, winner = semi2_win),
           final_p = final_p)
    }
    
    div_data <- lapply(divisions, process_division)
    names(div_data) <- divisions
    
    kings     <- div_seeded %>% filter(Team == my_team)
    kings_div <- if (nrow(kings) > 0) kings$Division[1] else NA
    kings_prob <- if (!is.na(kings_div) && kings_div %in% names(div_data)) {
      probs <- div_data[[kings_div]]$champ_probs
      if (my_team %in% names(probs)) as.integer(probs[my_team]) else 0L
    } else 0L
    
    kings_status <- if (nrow(kings) == 0) {
      "No data found"
    } else {
      n    <- kings$div_size[1]; seed <- kings$div_seed[1]
      auto_seeds_k <- max(1, 4 - max(0, n - 4))
      if (n <= 4 || seed <= auto_seeds_k)
        paste0("AUTO — #", seed, " seed in ", kings$Division[1],
               " | Direct to Division Champ bracket")
      else
        paste0("PLAY-IN (Mar 6) — #", seed, " of ", n, " in ", kings$Division[1],
               " | Must win to reach Division Champ bracket")
    }
    
    prob_col <- function(p) {
      if (p >= 70) "#15803d" else if (p >= 45) "#65a30d"
      else if (p >= 25) "#d97706" else "#b91c1c"
    }
    
    seed_badge <- function(n_val, color) {
      div(style = paste0(
        "width:22px;height:22px;border-radius:50%;background:", color,
        ";color:white;display:flex;align-items:center;justify-content:center;",
        "font-weight:800;font-size:0.72em;flex-shrink:0;"), n_val)
    }
    
    team_row_ui <- function(tr, is_playin = FALSE, champ_prob = NULL) {
      is_k  <- tr$Team == my_team
      gp    <- tr$W + tr$L
      wlbl  <- if (gp > 0) paste0(round(tr$WP * 100, 1), "%") else "-"
      bg    <- if (is_k) "#fef3c7" else if (is_playin) "#fef2f2" else "#f8fafc"
      bdr   <- if (is_k) ohio_kings_accent else if (is_playin) "#fca5a5" else "#e2e8f0"
      tcol  <- if (is_k) ohio_kings_primary else if (is_playin) "#991b1b" else "#1e293b"
      bcol  <- if (is_playin) "#ef4444" else ohio_kings_secondary
      
      div(style = paste0(
        "background:", bg, ";border:1.5px solid ", bdr,
        ";border-radius:8px;padding:8px 10px;display:flex;align-items:center;gap:8px;"),
        seed_badge(tr$div_seed, bcol),
        div(style = "flex:1;",
            div(tr$Team, style = paste0("font-weight:700;font-size:0.82em;color:", tcol, ";")),
            div(paste0(tr$W, "-", tr$L, "  |  W PCT: ", wlbl),
                style = "font-size:0.62em;color:#64748b;margin-top:2px;")),
        if (!is.null(champ_prob)) {
          div(style = "text-align:right;",
              div(paste0(champ_prob, "%"),
                  style = paste0("font-size:0.85em;font-weight:800;color:",
                                 prob_col(as.integer(champ_prob)), ";")),
              div("div win", style = "font-size:0.55em;color:#94a3b8;"))
        }
      )
    }
    
    div(
      # ── My team banner ───────────────────────────────────────────────────
      div(style = paste0(
        "background:linear-gradient(135deg,", prob_col(kings_prob), ",",
        prob_col(kings_prob), "cc);color:white;border-radius:14px;",
        "padding:18px 24px;margin-bottom:20px;display:flex;",
        "align-items:center;gap:20px;flex-wrap:wrap;",
        "box-shadow:0 4px 16px rgba(0,0,0,0.12);"),
        div(style = "font-size:3em;font-weight:900;line-height:1;",
            paste0(kings_prob, "%")),
        div(
          div(paste0(toupper(my_team), " — EST. DIVISION WIN PROBABILITY"),
              style = "font-size:0.72em;font-weight:700;opacity:0.75;letter-spacing:1px;"),
          div(kings_status, style = "font-size:0.95em;font-weight:700;margin-top:5px;"),
          if (nrow(kings) > 0) {
            div(paste0("Record: ", kings$W[1], "-", kings$L[1],
                       "  |  W PCT: ", round(kings$WP[1] * 100, 1), "%",
                       "  |  Seed #", kings$div_seed[1],
                       " of ", kings$div_size[1], " in ", kings$Division[1]),
                style = "font-size:0.78em;opacity:0.85;margin-top:5px;")
          }
        )
      ),
      
      # ── Tournament calendar ──────────────────────────────────────────────
      div(style = "display:flex;gap:8px;flex-wrap:wrap;margin-bottom:22px;",
          lapply(list(
            list("Mar 6",     "Play-In Games",     "Only divisions with 5+ teams",    "#475569"),
            list("Mar 13",    "Division Champs",   "4-team bracket: semis + final",   ohio_kings_secondary),
            list("Mar 20",    "Conference Champs", "Division winners compete",        "#6d28d9"),
            list("Mar 27",    "Regional Champs",   "Conference winners → Final Four", "#b45309"),
            list("Apr 10-12", "Final Four + Title","Atlanta \U0001f3c6",              ohio_kings_primary)
          ), function(rd) {
            div(style = paste0(
              "flex:1;min-width:120px;background:", rd[[4]], "15;",
              "border:2px solid ", rd[[4]], ";border-radius:10px;padding:10px 12px;"),
              div(rd[[1]], style = paste0("font-size:0.68em;font-weight:800;color:",
                                          rd[[4]], ";letter-spacing:0.5px;")),
              div(rd[[2]], style = "font-size:0.82em;font-weight:700;color:#1e293b;margin-top:3px;"),
              div(rd[[3]], style = "font-size:0.62em;color:#64748b;margin-top:3px;"))
          })
      ),
      
      h4("Division Standings & Playoff Picture",
         style = "margin:0 0 14px;color:#334155;font-weight:800;"),
      
      # ── Division cards ────────────────────────────────────────────────────
      div(style = "display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:16px;",
          lapply(div_data, function(ds) {
            teams     <- ds$all_teams
            bracket_4 <- ds$bracket_4
            is_my_div <- my_team %in% teams$Team
            n_teams   <- nrow(teams)
            needs_pi  <- n_teams > 4
            
            div(style = paste0(
              "background:white;border:2px solid ",
              if (is_my_div) ohio_kings_primary else "#e2e8f0",
              ";border-radius:12px;overflow:hidden;",
              "box-shadow:0 2px 10px rgba(0,0,0,0.06);"),
              
              # Header
              div(style = paste0(
                "background:linear-gradient(135deg,",
                if (is_my_div) paste0(ohio_kings_primary, ",", ohio_kings_secondary)
                else "#475569,#64748b",
                ");color:white;padding:12px 16px;"),
                div(style = "display:flex;justify-content:space-between;align-items:center;",
                    div(
                      div(ds$name, style = "font-weight:800;font-size:0.95em;"),
                      div(paste0(n_teams, " teams",
                                 if (needs_pi) paste0(" | ", n_teams - 4,
                                                      " play-in game",
                                                      if (n_teams - 4 > 1) "s") else " | No play-in"),
                          style = "font-size:0.65em;opacity:0.75;margin-top:2px;")),
                    div(style = "text-align:right;",
                        div("\U0001f3c6 Predicted:", style = "font-size:0.6em;opacity:0.7;"),
                        div(ds$div_champ, style = "font-size:0.78em;font-weight:700;"),
                        div(paste0(ds$champ_probs[ds$div_champ], "% prob"),
                            style = "font-size:0.6em;opacity:0.75;"))
                )
              ),
              
              div(style = "padding:10px;display:flex;flex-direction:column;gap:5px;",
                  
                  # Play-in section
                  if (needs_pi && length(ds$playin_games) > 0) {
                    tagList(
                      div(style = "font-size:0.65em;font-weight:800;color:#ef4444;
                                     letter-spacing:0.5px;padding:4px 2px;",
                          paste0("\u26a0\ufe0f PLAY-IN GAMES (Mar 6) — ",
                                 length(ds$playin_games), " game",
                                 if (length(ds$playin_games) > 1) "s" else "")),
                      lapply(ds$playin_games, function(pg) {
                        div(style = "background:#fef2f2;border:1.5px dashed #fca5a5;
                                       border-radius:8px;padding:8px 10px;margin-bottom:2px;",
                            div(style = "display:flex;justify-content:space-between;align-items:center;",
                                div(style = "display:flex;align-items:center;gap:6px;",
                                    seed_badge(pg$high$div_seed, "#ef4444"),
                                    div(div(pg$high$Team,
                                            style = paste0("font-weight:700;font-size:0.8em;color:",
                                                           if(pg$high$Team==my_team) ohio_kings_primary else "#991b1b",";")),
                                        div(paste0(pg$high$W,"-",pg$high$L," | W PCT: ",
                                                   round(pg$high$WP*100,1),"%"),
                                            style="font-size:0.6em;color:#64748b;"))),
                                div(paste0(pg$win_high_prob,"%"),
                                    style=paste0("font-size:0.8em;font-weight:800;color:",
                                                 if(pg$win_high_prob>=50)"#15803d" else "#b91c1c",";"))
                            ),
                            div(style="text-align:center;font-size:0.65em;color:#94a3b8;padding:2px 0;","vs"),
                            div(style = "display:flex;justify-content:space-between;align-items:center;",
                                div(style = "display:flex;align-items:center;gap:6px;",
                                    seed_badge(pg$low$div_seed, "#ef4444"),
                                    div(div(pg$low$Team,
                                            style = paste0("font-weight:700;font-size:0.8em;color:",
                                                           if(pg$low$Team==my_team) ohio_kings_primary else "#991b1b",";")),
                                        div(paste0(pg$low$W,"-",pg$low$L," | W PCT: ",
                                                   round(pg$low$WP*100,1),"%"),
                                            style="font-size:0.6em;color:#64748b;"))),
                                div(paste0(100L - pg$win_high_prob,"%"),
                                    style=paste0("font-size:0.8em;font-weight:800;color:",
                                                 if((100L-pg$win_high_prob)>=50)"#15803d" else "#b91c1c",";"))
                            ),
                            div(style="margin-top:5px;font-size:0.65em;color:#166534;font-weight:600;
                                         background:#f0fdf4;border-radius:4px;padding:3px 6px;",
                                paste0("\u25b6 Predicted: ", pg$predicted_winner,
                                       " (", pg$win_high_prob, "%) advances to bracket"))
                        )
                      })
                    )
                  },
                  
                  if (needs_pi) {
                    div(style = "display:flex;align-items:center;gap:6px;margin:4px 0;",
                        div(style = "flex:1;height:1px;background:#e2e8f0;"),
                        span("DIVISION CHAMP BRACKET (Mar 13)",
                             style = paste0("font-size:0.6em;font-weight:800;color:",
                                            ohio_kings_secondary, ";white-space:nowrap;")),
                        div(style = "flex:1;height:1px;background:#e2e8f0;"))
                  },
                  
                  div(style = "font-size:0.62em;font-weight:700;color:#94a3b8;padding:3px 2px;", "SEMIS"),
                  {
                    s1 <- ds$semi1; s2 <- ds$semi2
                    tagList(
                      div(style = "background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;
                                     padding:7px 10px;margin-bottom:3px;",
                          div(style = "display:flex;justify-content:space-between;align-items:center;",
                              div(style = "font-size:0.75em;font-weight:600;color:#334155;",
                                  paste0("#1 ", s1$t1)),
                              div(paste0(s1$p1,"%"),
                                  style=paste0("font-size:0.75em;font-weight:800;color:",
                                               if(s1$p1>=50)"#15803d" else "#b91c1c",";"))),
                          div(style="text-align:center;font-size:0.6em;color:#94a3b8;","vs"),
                          div(style = "display:flex;justify-content:space-between;align-items:center;",
                              div(style = "font-size:0.75em;font-weight:600;color:#334155;",
                                  paste0("#4 ", s1$t4)),
                              div(paste0(100L - s1$p1,"%"),
                                  style=paste0("font-size:0.75em;font-weight:800;color:",
                                               if((100L-s1$p1)>=50)"#15803d" else "#b91c1c",";")))
                      ),
                      div(style = "background:#f8fafc;border:1px solid #e2e8f0;border-radius:8px;
                                     padding:7px 10px;margin-bottom:6px;",
                          div(style = "display:flex;justify-content:space-between;align-items:center;",
                              div(style = "font-size:0.75em;font-weight:600;color:#334155;",
                                  paste0("#2 ", s2$t2)),
                              div(paste0(s2$p2,"%"),
                                  style=paste0("font-size:0.75em;font-weight:800;color:",
                                               if(s2$p2>=50)"#15803d" else "#b91c1c",";"))),
                          div(style="text-align:center;font-size:0.6em;color:#94a3b8;","vs"),
                          div(style = "display:flex;justify-content:space-between;align-items:center;",
                              div(style = "font-size:0.75em;font-weight:600;color:#334155;",
                                  paste0("#3 ", s2$t3)),
                              div(paste0(100L - s2$p2,"%"),
                                  style=paste0("font-size:0.75em;font-weight:800;color:",
                                               if((100L-s2$p2)>=50)"#15803d" else "#b91c1c",";")))
                      ),
                      div(style="font-size:0.62em;font-weight:700;color:#94a3b8;padding:3px 2px;",
                          "DIVISION FINAL"),
                      div(style=paste0("background:linear-gradient(135deg,",
                                       ohio_kings_primary,",",ohio_kings_secondary,");",
                                       "color:white;border-radius:8px;padding:8px 12px;"),
                          div(style="display:flex;justify-content:space-between;align-items:center;",
                              div(div("\U0001f3c6 Predicted Champion",
                                      style="font-size:0.62em;opacity:0.75;font-weight:600;"),
                                  div(ds$div_champ,
                                      style="font-size:0.88em;font-weight:800;margin-top:2px;")),
                              div(style="text-align:right;",
                                  div(paste0(ds$final_p,"%"),
                                      style="font-size:1.2em;font-weight:900;"),
                                  div("final win prob",style="font-size:0.55em;opacity:0.75;"))
                          )
                      ),
                      div(style="margin-top:8px;",
                          div(style="font-size:0.6em;font-weight:700;color:#94a3b8;
                                       padding:3px 2px;margin-bottom:4px;",
                              "DIVISION WIN PROBABILITY (all teams, sums to 100%)"),
                          lapply(teams %>% arrange(div_seed) %>%
                                   split(1:nrow(teams %>% arrange(div_seed))),
                                 function(tr) {
                                   cp <- if (tr$Team %in% names(ds$champ_probs))
                                     ds$champ_probs[tr$Team] else 0
                                   team_row_ui(tr,
                                               is_playin = any(sapply(ds$playin_games, function(pg)
                                                 tr$Team %in% c(pg$high$Team, pg$low$Team))),
                                               champ_prob = cp)
                                 })
                      )
                    )
                  }
              )
            )
          })
      ),
      
      # ── Format note ────────────────────────────────────────────────────────
      div(style = "margin-top:22px;background:#f8fafc;border-radius:10px;
                   padding:14px 18px;border:1px solid #e2e8f0;",
          div(style = "font-size:0.8em;color:#475569;line-height:1.9;",
              HTML(paste0(
                "<strong>How it works:</strong> Divisions with 4 or fewer teams go straight to the ",
                "<strong>Division Championship bracket (Mar 13)</strong>. Divisions with 5+ teams ",
                "hold <strong>play-in games on Mar 6</strong>. ",
                "Division Champ bracket: semis (1v4, 2v3) → final. ",
                "Division winners advance to <strong>Conference (Mar 20)</strong> → ",
                "<strong>Regional (Mar 27)</strong> → ",
                "<strong>Final Four & Championship in Atlanta (Apr 10-12)</strong>. ",
                "Win probabilities simulated 10,000 times and sum to 100% per division."
              ))
          )
      )
    )
  })
  
  output$league_trends <- renderPlotly({
    req(rv$logged_in, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    get_team_stat_safe <- function(roster, col) {
      total_row <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
      if (nrow(total_row) > 0 && col %in% names(total_row))
        return(as.numeric(total_row[[col]][1]))
      pr <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      if (col %in% names(pr)) return(sum(as.numeric(pr[[col]]), na.rm = TRUE))
      return(0)
    }
    
    div_stats <- rv$aba_data$league_data %>%
      rowwise() %>%
      mutate(GP = W + L, roster = list(rv$aba_data$all_team_stats[[Team]])) %>%
      filter(!is.null(roster)) %>%
      mutate(
        PPG = get_accurate_team_ppg(Team, rv$aba_data),
        RPG = if (!is.null(roster) && GP > 0) round(get_team_stat_safe(roster,"REB") / GP, 1) else 0,
        APG = if (!is.null(roster) && GP > 0) round(get_team_stat_safe(roster,"AST") / GP, 1) else 0
      ) %>%
      ungroup() %>%
      select(Team, Division, W, L, GP, PPG, RPG, APG)
    
    div_summary <- div_stats %>%
      group_by(Division) %>%
      summarize(Avg_PPG = round(mean(PPG,na.rm=TRUE),1),
                Avg_RPG = round(mean(RPG,na.rm=TRUE),1),
                Avg_APG = round(mean(APG,na.rm=TRUE),1),
                .groups = "drop")
    
    my_div <- rv$aba_data$league_data %>%
      filter(Team == rv$my_team) %>% pull(Division)
    div_summary$outline_color <- ifelse(div_summary$Division %in% my_div,
                                        ohio_kings_accent, "black")
    
    plot_ly(div_summary, x = ~Division) %>%
      add_trace(y = ~Avg_PPG, name = 'Avg PPG', type = 'bar',
                marker = list(color = ohio_kings_primary,
                              line  = list(color = ~outline_color, width = 3.5)),
                text = ~Avg_PPG, textposition = 'auto') %>%
      add_trace(y = ~Avg_RPG, name = 'Avg RPG', type = 'bar',
                marker = list(color = ohio_kings_secondary,
                              line  = list(color = ~outline_color, width = 3.5)),
                text = ~Avg_RPG, textposition = 'auto', visible = 'legendonly') %>%
      add_trace(y = ~Avg_APG, name = 'Avg APG', type = 'bar',
                marker = list(color = ohio_kings_accent,
                              line  = list(color = ~outline_color, width = 3.5)),
                text = ~Avg_APG, textposition = 'auto', visible = 'legendonly') %>%
      layout(
        title   = paste0("<b>Division Statistical Averages</b>"),
        yaxis   = list(title = ""), xaxis = list(title = ""),
        barmode = 'group', hovermode = "x unified",
        legend  = list(orientation = 'h', y = -0.2),
        margin  = list(t = 80, b = 40),
        annotations = list(list(
          text = "*Your division outlined in your team color",
          x = .35, y = -0.65, xref = 'paper', yref = 'paper',
          xanchor = 'left', yanchor = 'top', showarrow = FALSE,
          font = list(size = 12, color = 'gray')))
      )
  })
  
  # ==========================================================================
  # DIVISION TAB OUTPUTS
  # ==========================================================================
  
  output$division_snapshot <- renderUI({
    req(rv$logged_in, rv$aba_data, input$division_select)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    
    div_teams <- rv$aba_data$league_data %>%
      filter(Division == input$division_select) %>%
      arrange(desc(W))
    if (nrow(div_teams) == 0) return(p("No data", style = "color: #999;"))
    
    leader     <- div_teams[1, ]
    second     <- if (nrow(div_teams) >= 2) div_teams[2, ] else NULL
    games_back <- if (!is.null(second))
      ((leader$W - second$W) + (second$L - leader$L)) / 2 else 0
    
    get_team_ppg_div <- function(tname) {
      ppg <- get_accurate_team_ppg(tname, rv$aba_data)
      if (ppg > 0) return(ppg)
      r <- rv$aba_data$all_team_stats[[tname]]
      if (is.null(r)) return(0)
      tr  <- rv$aba_data$league_data %>% filter(Team == tname)
      gp  <- if (nrow(tr) > 0) tr$W[1] + tr$L[1] else 15
      total_row <- r %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
      if (nrow(total_row) > 0 && "PTS" %in% names(total_row))
        round(as.numeric(total_row$PTS[1]) / gp, 1) else 0
    }
    
    div_ppg           <- sapply(div_teams$Team, get_team_ppg_div)
    best_offense_idx  <- which.max(div_ppg)
    best_offense_team <- div_teams$Team[best_offense_idx]
    best_offense_ppg  <- div_ppg[best_offense_idx]
    avg_wins          <- round(mean(div_teams$W), 1)
    
    div(style = "display:flex;gap:12px;flex-wrap:wrap;",
        div(style = "flex:1;min-width:180px;background:linear-gradient(135deg,#15803d,#22c55e);
                     color:white;padding:16px;border-radius:12px;text-align:center;",
            div("\U0001f3c6", style = "font-size:1.8em;"),
            div("Division Leader", style = "font-size:0.75em;opacity:0.8;font-weight:600;margin-top:4px;"),
            div(leader$Team, style = "font-size:1em;font-weight:800;margin-top:6px;"),
            div(paste0(leader$W,"-",leader$L), style="font-size:1.4em;font-weight:800;margin-top:4px;")),
        div(style = "flex:1;min-width:180px;background:linear-gradient(135deg,#d97706,#fbbf24);
                     color:#451a03;padding:16px;border-radius:12px;text-align:center;",
            div("\U0001f525", style = "font-size:1.8em;"),
            div("2nd Place Gap", style = "font-size:0.75em;opacity:0.8;font-weight:600;margin-top:4px;"),
            div(paste0(games_back," GB"), style="font-size:1.4em;font-weight:800;margin-top:6px;"),
            div(paste0(if(!is.null(second)) second$Team else "N/A", " trails"),
                style="font-size:0.8em;font-weight:600;margin-top:4px;opacity:0.9;")),
        div(style = paste0("flex:1;min-width:180px;background:linear-gradient(135deg,",
                           ohio_kings_primary,",",ohio_kings_secondary,");",
                           "color:white;padding:16px;border-radius:12px;text-align:center;"),
            div("\U0001f3af", style = "font-size:1.8em;"),
            div("Best Offense", style = "font-size:0.75em;opacity:0.8;font-weight:600;margin-top:4px;"),
            div(best_offense_team, style = "font-size:1em;font-weight:800;margin-top:6px;"),
            div(paste0(best_offense_ppg," PPG"), style="font-size:1.4em;font-weight:800;margin-top:4px;")),
        div(style = "flex:1;min-width:180px;background:linear-gradient(135deg,#475569,#64748b);
                     color:white;padding:16px;border-radius:12px;text-align:center;",
            div("\U0001f4ca", style = "font-size:1.8em;"),
            div("Division Average", style="font-size:0.75em;opacity:0.8;font-weight:600;margin-top:4px;"),
            div(paste0(avg_wins," wins"), style="font-size:1em;font-weight:800;margin-top:6px;"),
            div(paste0(nrow(div_teams)," teams"), style="font-size:1.4em;font-weight:800;margin-top:4px;"))
    )
  })
  
  output$division_standings <- renderDT({
    req(rv$logged_in, rv$aba_data, input$division_select)
    ohio_kings_primary <- rv$team_cols$primary
    
    standings <- rv$aba_data$league_data %>%
      filter(Division == input$division_select) %>%
      select(Team, W, L) %>%
      arrange(desc(as.numeric(W) / (as.numeric(W) + as.numeric(L)))) %>%
      mutate(
        W = as.numeric(W), L = as.numeric(L),
        'W PCT' = round(W / (W + L), 3),
        GB = ((first(W) - W) + (L - first(L))) / 2,
        GB = ifelse(is.na(GB) | GB == 0, "-", as.character(GB))
      )
    
    datatable(standings,
              options  = list(pageLength = 10, dom = 't'),
              rownames = FALSE, escape = FALSE,
              class    = 'cell-border stripe hover') %>%
      formatStyle('Team', cursor = 'pointer', color = ohio_kings_primary,
                  textDecoration = 'underline')
  })
  
  observeEvent(input$division_standings_cell_clicked, {
    info <- input$division_standings_cell_clicked
    if (!is.null(info$value) && info$col == 0) {
      updateSelectInput(session, "team_select", selected = info$value)
      updateTabItems(session, "tabs", "team")
    }
  })
  
  output$division_comparison <- renderPlotly({
    req(rv$logged_in, rv$aba_data, input$division_select)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    get_team_stat_div <- function(roster, col) {
      total_row <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
      if (nrow(total_row) > 0 && col %in% names(total_row))
        return(as.numeric(total_row[[col]][1]))
      pr <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      if (col %in% names(pr)) return(sum(as.numeric(pr[[col]]), na.rm = TRUE))
      return(0)
    }
    
    div_data <- rv$aba_data$league_data %>%
      filter(Division == input$division_select) %>%
      mutate(GP = W + L) %>%
      rowwise() %>%
      mutate(
        roster = list(rv$aba_data$all_team_stats[[Team]]),
        PPG    = get_accurate_team_ppg(Team, rv$aba_data),
        RPG    = if (!is.null(roster) && GP > 0) round(get_team_stat_div(roster,"REB")/GP,1) else 0,
        APG    = if (!is.null(roster) && GP > 0) round(get_team_stat_div(roster,"AST")/GP,1) else 0
      ) %>%
      ungroup() %>%
      select(Team, W, L, GP, PPG, RPG, APG) %>%
      mutate(
        PPG_border = ifelse(PPG == max(PPG,na.rm=TRUE), '#60a5fa', 'black'),
        RPG_border = ifelse(RPG == max(RPG,na.rm=TRUE), '#60a5fa', 'black'),
        APG_border = ifelse(APG == max(APG,na.rm=TRUE), '#60a5fa', 'black'),
        PPG_width  = ifelse(PPG == max(PPG,na.rm=TRUE), 3.5, 1),
        RPG_width  = ifelse(RPG == max(RPG,na.rm=TRUE), 3.5, 1),
        APG_width  = ifelse(APG == max(APG,na.rm=TRUE), 3.5, 1),
        Team       = paste0("<b>", Team, "</b>")
      )
    
    plot_ly(div_data) %>%
      add_trace(x=~Team, y=~PPG, type='bar', name='PPG',
                marker=list(color=ohio_kings_primary,
                            line=list(color=~PPG_border,width=~PPG_width)),
                text=~PPG, textposition='auto') %>%
      add_trace(x=~Team, y=~RPG, type='bar', name='RPG',
                marker=list(color=ohio_kings_secondary,
                            line=list(color=~RPG_border,width=~RPG_width)),
                text=~RPG, textposition='auto') %>%
      add_trace(x=~Team, y=~APG, type='bar', name='APG',
                marker=list(color=ohio_kings_accent,
                            line=list(color=~APG_border,width=~APG_width)),
                text=~APG, textposition='auto') %>%
      layout(
        barmode='group',
        title=paste0("<b>",input$division_select," - Total Team Per Game Statistics</b>"),
        xaxis=list(title="",tickangle=-30), yaxis=list(title=""),
        legend=list(orientation='h',y=-0.6), margin=list(t=80,b=60),
        annotations=list(list(
          text="*Blue border indicates highest mark in division",
          x=.35,y=-0.65,xref='paper',yref='paper',
          xanchor='left',yanchor='top',showarrow=FALSE,
          font=list(size=12,color='gray')))
      )
  })
  
  # ==========================================================================
  # TEAM TAB OUTPUTS
  # ==========================================================================
  
  output$team_stats_boxes <- renderUI({
    req(rv$logged_in, input$team_select, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    team_data <- rv$aba_data$league_data %>% filter(Team == input$team_select)
    if (nrow(team_data) == 0) return(NULL)
    
    team_logo <- ""
    if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
      logo_result <- rv$aba_data$team_lookup %>%
        filter(team_name == input$team_select) %>% pull(logo_url)
      if (length(logo_result) > 0 && !is.na(logo_result[1])) team_logo <- logo_result[1]
    }
    
    roster        <- rv$aba_data$all_team_stats[[input$team_select]]
    games_played  <- team_data$W[1] + team_data$L[1]
    total_row     <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
    player_roster <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
    
    ppg <- get_accurate_team_ppg(input$team_select, rv$aba_data)
    if (ppg == 0 && nrow(total_row) > 0 && "PTS" %in% names(total_row))
      ppg <- round(as.numeric(total_row$PTS[1]) / games_played, 2)
    
    rpg <- if (nrow(total_row) > 0 && "REB" %in% names(total_row))
      round(as.numeric(total_row$REB[1]) / games_played, 2)
    else if ("REB" %in% names(player_roster))
      round(sum(as.numeric(player_roster$REB), na.rm=TRUE) / games_played, 2)
    else 0
    
    apg <- if (nrow(total_row) > 0 && "AST" %in% names(total_row))
      round(as.numeric(total_row$AST[1]) / games_played, 2)
    else if ("AST" %in% names(player_roster))
      round(sum(as.numeric(player_roster$AST), na.rm=TRUE) / games_played, 2)
    else 0
    
    # League percentile helpers
    all_team_ppg <- sapply(names(rv$aba_data$all_team_stats),
                           function(tn) get_accurate_team_ppg(tn, rv$aba_data))
    pct_rank <- function(val, all_vals) {
      all_vals <- all_vals[!is.na(all_vals) & all_vals > 0]
      if (length(all_vals) == 0) return(50)
      round(sum(all_vals <= val) / length(all_vals) * 100, 0)
    }
    ppg_pct    <- pct_rank(ppg, all_team_ppg)
    wins       <- as.numeric(team_data$W[1])
    losses     <- as.numeric(team_data$L[1])
    gp         <- wins + losses
    win_pct    <- if (gp > 0) wins / gp else 0.5
    all_wpts   <- rv$aba_data$league_data %>%
      mutate(wp = as.numeric(W) / (as.numeric(W) + as.numeric(L))) %>% pull(wp)
    record_pct <- pct_rank(win_pct, all_wpts)
    
    make_team_box <- function(value, label, percentile) {
      if      (percentile >= 75) {
        bg <- "background: linear-gradient(135deg, #15803d 0%, #22c55e 100%);"
        text_col <- "color: white;"
        tag <- "Top 25%"; tag_bg <- "background: rgba(255,255,255,0.25);"
      } else if (percentile >= 50) {
        bg <- "background: linear-gradient(135deg, #65a30d 0%, #a3e635 100%);"
        text_col <- "color: #1a2e05;"
        tag <- "Above Avg"; tag_bg <- "background: rgba(0,0,0,0.1);"
      } else if (percentile >= 25) {
        bg <- "background: linear-gradient(135deg, #d97706 0%, #fbbf24 100%);"
        text_col <- "color: #451a03;"
        tag <- "Below Avg"; tag_bg <- "background: rgba(0,0,0,0.1);"
      } else {
        bg <- "background: linear-gradient(135deg, #b91c1c 0%, #f87171 100%);"
        text_col <- "color: white;"
        tag <- "Bottom 25%"; tag_bg <- "background: rgba(255,255,255,0.2);"
      }
      div(
        style = paste0(bg, text_col,
                       " padding: 18px 15px; border-radius: 14px; margin: 10px 0;
                        text-align: center; position: relative;
                        box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                        border: 1px solid rgba(255,255,255,0.15);"),
        span(paste0(tag, " \u00b7 ", percentile, "th"),
             style = paste0(tag_bg,
                            " position: absolute; top: 8px; right: 8px;
                             font-size: 0.6em; font-weight: 700;
                             padding: 2px 8px; border-radius: 20px; letter-spacing: 0.3px;")),
        h3(value, style = "margin: 8px 0 0 0; font-size: 2.3em; font-weight: 800; letter-spacing: -0.5px;"),
        p(label,  style = "margin: 6px 0 0 0; opacity: 0.85; font-weight: 600; font-size: 0.9em; letter-spacing: 0.5px;")
      )
    }
    
    tagList(
      if (team_logo != "") {
        fluidRow(column(12,
                        div(style = "display: flex; align-items: center; gap: 20px; margin-bottom: 15px;
                       padding: 15px;
                       background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
                       border-radius: 12px;",
                            tags$img(src = team_logo,
                                     style = paste0("width: 70px; height: 70px; border-radius: 50%;
                                       border: 3px solid ", ohio_kings_primary,
                                                    "; object-fit: cover; background: white;")),
                            div(h2(input$team_select,
                                   style = paste0("margin: 0; color: ", ohio_kings_secondary,
                                                  "; font-weight: 800;")),
                                p(paste0(team_data$Division[1], " Division | ",
                                         wins, "-", losses, " Record"),
                                  style = "margin: 5px 0 0 0; color: #64748b;")))
        ))
      },
      fluidRow(
        column(3, make_team_box(paste0(wins, "-", losses), "Record", record_pct)),
        column(3, make_team_box(ppg, "PPG", ppg_pct)),
        column(3, make_team_box(rpg, "RPG", 50)),
        column(3, make_team_box(apg, "APG", 50))
      )
    )
  })
  
  output$team_roster <- renderDT({
    req(rv$logged_in, input$team_select, rv$aba_data)
    ohio_kings_primary <- rv$team_cols$primary
    roster <- rv$aba_data$all_team_stats[[input$team_select]]
    if (is.null(roster)) return(NULL)
    
    roster$FGM    <- suppressWarnings(as.numeric(roster$`2PM`)) +
      suppressWarnings(as.numeric(roster$`3PM`))
    roster$FGA    <- suppressWarnings(as.numeric(roster$`2PA`)) +
      suppressWarnings(as.numeric(roster$`3PA`))
    roster$`eFG%` <- ifelse(roster$FGA > 0,
                            paste0(round(((roster$FGM + 0.5 * suppressWarnings(as.numeric(roster$`3PM`))) /
                                            roster$FGA) * 100), "%"), "0%")
    
    three_pct_index <- which(names(roster) == "3P%")
    new_cols        <- c("FGA","FGM","eFG%")
    other_cols      <- setdiff(names(roster), new_cols)
    roster <- roster[, c(other_cols[1:three_pct_index], new_cols,
                         other_cols[(three_pct_index+1):length(other_cols)])]
    
    datatable(roster,
              options   = list(pageLength = 15, scrollX = TRUE),
              rownames  = FALSE, selection = 'none', escape = FALSE) %>%
      formatStyle('Player', cursor = 'pointer', color = ohio_kings_primary,
                  textDecoration = 'underline')
  })
  
  observeEvent(input$team_roster_cell_clicked, {
    info <- input$team_roster_cell_clicked
    if (!is.null(info$value) && info$col == 0) {
      updateSelectInput(session, "player_select", selected = info$value)
      updateTabItems(session, "tabs", "player")
    }
  })
  
  output$team_performance <- renderPlotly({
    req(rv$logged_in, input$team_select, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (!is.null(rv$aba_data$team_schedules) &&
        input$team_select %in% names(rv$aba_data$team_schedules)) {
      sched <- rv$aba_data$team_schedules[[input$team_select]]
      if (!is.null(sched) && nrow(sched) > 0) {
        team_games <- sched %>%
          filter(!is_forfeit, team_score > 0) %>%
          arrange(date) %>%
          mutate(
            Game           = row_number(),
            PTS            = team_score,
            Cumulative_PPG = round(cumsum(team_score) / Game, 1),
            marker_color   = ifelse(result == "W", "#22c55e", "#ef4444"),
            hover_text     = paste0("Game ", Game, " (", format(date, "%b %d"), ")\n",
                                    PTS, " pts vs ", opponent, " (", result, ")\n",
                                    opp_score, " allowed")
          )
        if (nrow(team_games) > 0) {
          w_avg <- if (any(team_games$result == "W", na.rm=TRUE))
            mean(team_games$PTS[team_games$result == "W"], na.rm=TRUE)
          else mean(team_games$PTS, na.rm=TRUE)
          l_avg <- if (any(team_games$result == "L", na.rm=TRUE))
            mean(team_games$PTS[team_games$result == "L"], na.rm=TRUE)
          else mean(team_games$PTS, na.rm=TRUE)
          threshold <- round((w_avg + l_avg) / 2, 0)
          
          return(
            plot_ly(team_games, x = ~Game) %>%
              add_trace(y = ~PTS, name = 'Points Scored', type = 'scatter',
                        mode = 'lines+markers',
                        line   = list(color = ohio_kings_primary, width = 3),
                        marker = list(size = 12, color = ~marker_color,
                                      line = list(color = 'white', width = 2)),
                        text = ~hover_text, hoverinfo = 'text') %>%
              add_trace(y = ~Cumulative_PPG, name = 'Season PPG Avg',
                        type = 'scatter', mode = 'lines',
                        line = list(color = '#6366f1', width = 2, dash = 'dash')) %>%
              layout(
                title  = paste0(input$team_select, " - Game by Game Performance (",
                                nrow(team_games), " games)"),
                xaxis  = list(title = "Game Number"),
                yaxis  = list(title = "Points"),
                hovermode = "closest",
                legend = list(orientation = 'h', y = -0.2),
                shapes = list(list(type = "line",
                                   x0 = 0.5, x1 = nrow(team_games) + 0.5,
                                   y0 = threshold, y1 = threshold,
                                   line = list(color = "rgba(100,100,100,0.4)",
                                               width = 1, dash = "dot"))),
                annotations = list(list(
                  x = nrow(team_games) + 0.3, y = threshold,
                  text = paste0("Win line ~", threshold),
                  showarrow = TRUE, arrowhead = 0, arrowsize = 0.5,
                  ax = 40, ay = -20,
                  font = list(size = 11, color = "#64748b", weight = "bold")))
              )
          )
        }
      }
    }
    plotly_empty() %>% layout(title = "No schedule data available")
  })
  
  output$team_scoring_breakdown <- renderPlotly({
    req(rv$logged_in, input$team_select, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    roster <- rv$aba_data$all_team_stats[[input$team_select]]
    if (is.null(roster)) return(plotly_empty())
    
    get_stat_brk <- function(r, col) {
      tr <- r %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
      if (nrow(tr) > 0 && col %in% names(tr)) return(as.numeric(tr[[col]][1]))
      pr <- r %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      if (col %in% names(pr)) return(sum(as.numeric(pr[[col]]), na.rm=TRUE))
      return(0)
    }
    
    total_2pm <- get_stat_brk(roster,"2PM"); total_2pa <- get_stat_brk(roster,"2PA")
    total_3pm <- get_stat_brk(roster,"3PM"); total_3pa <- get_stat_brk(roster,"3PA")
    total_ftm <- get_stat_brk(roster,"FTM"); total_fta <- get_stat_brk(roster,"FTA")
    miss_2    <- pmax(total_2pa - total_2pm, 0)
    miss_3    <- pmax(total_3pa - total_3pm, 0)
    miss_ft   <- pmax(total_fta - total_ftm, 0)
    categories <- c("2-Pointers","3-Pointers","Free Throws")
    made_vals  <- c(total_2pm, total_3pm, total_ftm)
    att_vals   <- c(total_2pa, total_3pa, total_fta)
    miss_vals  <- c(miss_2, miss_3, miss_ft)
    if (sum(att_vals) == 0) return(plotly_empty())
    pct_labels <- ifelse(att_vals > 0,
                         paste0(round(made_vals/att_vals*100,1),"%"), "")
    
    plot_ly() %>%
      add_trace(x=categories, y=made_vals, type='bar', name='Made',
                marker=list(color=c(ohio_kings_primary,ohio_kings_secondary,ohio_kings_accent),
                            line=list(color='#ffffff',width=1.5)),
                hovertemplate='%{x}: %{y} made<extra></extra>') %>%
      add_trace(x=categories, y=miss_vals, type='bar', name='Missed',
                text=pct_labels, textposition='outside', cliponaxis=FALSE,
                textfont=list(color='#334155',size=13,family='Arial Black'),
                marker=list(color='rgba(148,163,184,0.4)',
                            line=list(color='#ffffff',width=1.5)),
                hovertemplate='%{x}: %{y} missed<extra></extra>') %>%
      layout(barmode='stack',
             xaxis=list(title='',tickfont=list(size=13,color='#334155')),
             yaxis=list(title='Attempts',titlefont=list(size=12,color='#334155'),
                        tickfont=list(size=11),range=c(0,max(att_vals)*1.2)),
             legend=list(orientation='h',y=-0.15,x=0.5,xanchor='center',
                         font=list(size=11)),
             margin=list(l=40,r=20,t=20,b=60),
             plot_bgcolor='rgba(0,0,0,0)', paper_bgcolor='rgba(0,0,0,0)')
  })
  
  output$team_advanced_metrics <- renderUI({
    req(rv$logged_in, input$team_select, rv$aba_data)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    
    team_roster <- rv$aba_data$all_team_stats[[input$team_select]]
    if (is.null(team_roster) || nrow(team_roster) == 0)
      return(p("Select a team to view advanced metrics",
               style = "text-align: center; color: #999; padding: 20px;"))
    
    get_stat <- function(roster, col) {
      total_row <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
      if (nrow(total_row) > 0 && col %in% names(total_row))
        return(as.numeric(total_row[[col]][1]))
      player_rows <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      if (col %in% names(player_rows))
        return(sum(as.numeric(player_rows[[col]]), na.rm = TRUE))
      return(0)
    }
    
    team_record  <- rv$aba_data$league_data %>% filter(Team == input$team_select)
    games_played <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
    
    total_2pm <- get_stat(team_roster,"2PM"); total_2pa <- get_stat(team_roster,"2PA")
    total_3pm <- get_stat(team_roster,"3PM"); total_3pa <- get_stat(team_roster,"3PA")
    total_fgm <- total_2pm + total_3pm;       total_fga <- total_2pa + total_3pa
    total_fta <- get_stat(team_roster,"FTA")
    total_oreb <- get_stat(team_roster,"OREB"); total_dreb <- get_stat(team_roster,"DREB")
    
    efg <- if (total_fga > 0)
      round(min((total_fgm + 0.5 * total_3pm) / total_fga * 100, 100), 1) else 0
    ft_rate  <- if (total_fga > 0) round(total_fta / total_fga, 2) else 0
    oreb_pct <- if ((total_oreb + total_dreb) > 0)
      round(total_oreb / (total_oreb + total_dreb) * 100, 1) else 0
    
    ppg_from_schedule <- get_accurate_team_ppg(input$team_select, rv$aba_data)
    est_poss     <- total_fga + 0.44 * total_fta
    poss_per_game <- if (games_played > 0) round(est_poss / games_played, 1) else 0
    off_rating <- if (est_poss > 0 && ppg_from_schedule > 0)
      round((ppg_from_schedule / poss_per_game) * 100, 2) else 0
    
    def_rating <- 0
    if (!is.null(rv$aba_data$team_schedules) &&
        input$team_select %in% names(rv$aba_data$team_schedules)) {
      sched      <- rv$aba_data$team_schedules[[input$team_select]]
      real_games <- sched %>% filter(!is_forfeit, team_score > 0)
      if (nrow(real_games) > 0 && poss_per_game > 0) {
        actual_opp_ppg <- mean(real_games$opp_score, na.rm=TRUE)
        def_rating <- round((actual_opp_ppg / poss_per_game) * 100, 2)
      }
    }
    net_rating <- round(off_rating - def_rating, 2)
    
    player_roster <- team_roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
    usage_dist <- player_roster %>%
      mutate(
        fga       = suppressWarnings(as.numeric(`2PA`)) + suppressWarnings(as.numeric(`3PA`)),
        usage_pct = if (total_fga > 0) round((fga / total_fga) * 100, 1) else 0
      ) %>%
      filter(!is.na(usage_pct)) %>%
      arrange(desc(usage_pct)) %>%
      head(5) %>%
      select(Player, usage_pct)
    
    all_team_off_ratings <- sapply(names(rv$aba_data$all_team_stats), function(tname) {
      tryCatch({
        r    <- rv$aba_data$all_team_stats[[tname]]
        tr   <- rv$aba_data$league_data %>% filter(Team == tname)
        gp   <- if (nrow(tr) > 0) tr$W[1] + tr$L[1] else 15
        t_fga <- get_stat(r,"2PA") + get_stat(r,"3PA")
        t_fta <- get_stat(r,"FTA")
        t_poss <- t_fga + 0.44 * t_fta
        ppg_t  <- get_accurate_team_ppg(tname, rv$aba_data)
        poss_pg <- if (gp > 0) t_poss / gp else 0
        if (t_poss > 0 && poss_pg > 0) (ppg_t / poss_pg) * 100 else NA
      }, error = function(e) NA)
    })
    
    adv_pct_rank <- function(val, all_vals) {
      all_vals <- all_vals[!is.na(all_vals) & all_vals > 0]
      if (length(all_vals) == 0) return(50)
      round(sum(all_vals <= val) / length(all_vals) * 100, 0)
    }
    
    off_pct  <- adv_pct_rank(off_rating, all_team_off_ratings)
    def_pct  <- 100 - adv_pct_rank(def_rating, all_team_off_ratings)
    net_pct  <- if (net_rating > 5) 85 else if (net_rating > 0) 65
    else if (net_rating > -5) 35 else 15
    pace_pct <- 50
    
    adv_box <- function(value, label, sublabel, percentile) {
      if      (percentile >= 75) {
        bg <- "background: linear-gradient(135deg, #15803d 0%, #22c55e 100%);"; tc <- "color: white;"
        tag <- "Elite"; tbg <- "background: rgba(255,255,255,0.25);"
      } else if (percentile >= 50) {
        bg <- "background: linear-gradient(135deg, #65a30d 0%, #a3e635 100%);"; tc <- "color: #1a2e05;"
        tag <- "Above Avg"; tbg <- "background: rgba(0,0,0,0.1);"
      } else if (percentile >= 25) {
        bg <- "background: linear-gradient(135deg, #d97706 0%, #fbbf24 100%);"; tc <- "color: #451a03;"
        tag <- "Below Avg"; tbg <- "background: rgba(0,0,0,0.1);"
      } else {
        bg <- "background: linear-gradient(135deg, #b91c1c 0%, #f87171 100%);"; tc <- "color: white;"
        tag <- "Poor"; tbg <- "background: rgba(255,255,255,0.2);"
      }
      div(style = paste0(bg, tc,
                         " padding: 18px 15px; border-radius: 14px; text-align: center;
                          margin: 10px 0; position: relative;
                          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                          border: 1px solid rgba(255,255,255,0.15);"),
          span(paste0(tag, " \u00b7 ", percentile, "th"),
               style = paste0(tbg,
                              " position: absolute; top: 8px; right: 8px;
                               font-size: 0.6em; font-weight: 700;
                               padding: 2px 8px; border-radius: 20px;")),
          h3(value,    style = "margin: 8px 0 0 0; font-size: 2.3em; font-weight: 800;"),
          p(label,     style = "margin: 5px 0 0 0; opacity: 0.9; font-weight: 600;"),
          p(sublabel,  style = "font-size: 0.8em; opacity: 0.75; margin: 4px 0 0 0;"))
    }
    
    bar_colors <- c(
      "linear-gradient(90deg, #15803d, #22c55e)",
      "linear-gradient(90deg, #65a30d, #a3e635)",
      "linear-gradient(90deg, #d97706, #fbbf24)",
      "linear-gradient(90deg, #9a3412, #f97316)",
      "linear-gradient(90deg, #64748b, #94a3b8)"
    )
    
    div(
      fluidRow(
        column(3, adv_box(off_rating, "Offensive Rating", "Pts per 100 poss", off_pct)),
        column(3, adv_box(def_rating, "Defensive Rating", "Est. pts allowed/100", def_pct)),
        column(3, adv_box(poss_per_game, "Pace", "Poss per game", pace_pct)),
        column(3, adv_box(paste0(ifelse(net_rating > 0, "+", ""), net_rating),
                          "Net Rating", "Off - Def", net_pct))
      ),
      fluidRow(
        column(12,
               div(style = "background: linear-gradient(180deg, #ffffff 0%, #f8fafc 100%);
                        border-radius: 14px; padding: 24px; margin: 10px 0;
                        box-shadow: 0 4px 12px rgba(0,0,0,0.06); border: 1px solid #e2e8f0;",
                   h4("Usage Distribution - Who Takes the Shots?",
                      style = "margin: 0 0 18px 0; color: #1e293b; font-weight: 700;"),
                   if (nrow(usage_dist) > 0) {
                     lapply(1:nrow(usage_dist), function(i) {
                       player <- usage_dist[i, ]
                       bar_bg <- bar_colors[min(i, length(bar_colors))]
                       div(style = "margin-bottom: 12px;",
                           div(style = "display: flex; justify-content: space-between;
                                   margin-bottom: 6px; align-items: center;",
                               div(style = "display: flex; align-items: center; gap: 8px;",
                                   span(paste0("#", i),
                                        style = "font-weight: 800; color: #94a3b8;
                                            font-size: 0.85em; min-width: 24px;"),
                                   span(player$Player,
                                        style = "font-weight: 700; color: #1e293b; font-size: 0.95em;")),
                               span(paste0(player$usage_pct, "%"),
                                    style = "font-weight: 700; color: #475569; font-size: 0.95em;")),
                           div(style = "background: #e2e8f0; border-radius: 12px;
                                   height: 26px; overflow: hidden;",
                               div(style = paste0("width: ", min(player$usage_pct * 2.5, 100),
                                                  "%; background: ", bar_bg,
                                                  "; height: 100%; border-radius: 12px;"))))
                     })
                   } else {
                     p("No usage data available", style = "color: #94a3b8; text-align: center;")
                   }
               )
        )
      )
    )
  })
  
  output$team_comparison <- renderUI({
    req(rv$logged_in, input$compare_team_1, rv$aba_data)
    ohio_kings_primary <- rv$team_cols$primary
    
    teams_to_compare <- c(input$compare_team_1, input$compare_team_2, input$compare_team_3)
    teams_to_compare <- teams_to_compare[teams_to_compare != "" & !is.na(teams_to_compare)]
    if (length(teams_to_compare) < 2)
      return(p("Select at least 2 teams to compare",
               style = "text-align: center; color: #999; padding: 20px;"))
    
    comparison_data <- lapply(teams_to_compare, function(tname) {
      roster      <- rv$aba_data$all_team_stats[[tname]]
      team_record <- rv$aba_data$league_data %>% filter(Team == tname)
      if (is.null(roster) || nrow(team_record) == 0) return(NULL)
      roster <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      gp     <- team_record$W[1] + team_record$L[1]
      data.frame(
        Team   = tname,
        Record = paste0(team_record$W[1], "-", team_record$L[1]),
        PPG    = get_accurate_team_ppg(tname, rv$aba_data),
        RPG    = round(sum(as.numeric(roster$REB), na.rm=TRUE) / gp, 2),
        APG    = round(sum(as.numeric(roster$AST), na.rm=TRUE) / gp, 2),
        FG_pct = round((sum(as.numeric(roster$`2PM`) + as.numeric(roster$`3PM`), na.rm=TRUE) /
                          sum(as.numeric(roster$`2PA`) + as.numeric(roster$`3PA`), na.rm=TRUE)) * 100, 2)
      )
    })
    
    comp_df <- bind_rows(comparison_data)
    if (nrow(comp_df) == 0)
      return(p("No data", style = "text-align: center; color: #999;"))
    
    div(style = "overflow-x: auto;",
        tags$table(class = "table table-striped table-hover", style = "width: 100%;",
                   tags$thead(tags$tr(
                     tags$th("Metric", style = "background: #f8fafc; padding: 12px; font-weight: 600;"),
                     lapply(comp_df$Team, function(t)
                       tags$th(t, style = paste0("background: ", ohio_kings_primary,
                                                 "; color: white; padding: 12px; text-align: center;")))
                   )),
                   tags$tbody(lapply(list(
                     list("Record", comp_df$Record),
                     list("PPG",    comp_df$PPG),
                     list("RPG",    comp_df$RPG),
                     list("APG",    comp_df$APG),
                     list("FG%",    paste0(comp_df$FG_pct, "%"))
                   ), function(row) {
                     tags$tr(
                       tags$td(row[[1]], style = "padding: 10px; font-weight: 600;"),
                       lapply(row[[2]], function(v)
                         tags$td(v, style = "text-align: center; padding: 10px;"))
                     )
                   }))
        )
    )
  })
  
  # ==========================================================================
  # PLAYER TAB OUTPUTS
  # ==========================================================================
  
  output$player_stats_boxes <- renderUI({
    req(rv$logged_in, input$player_select)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    player_stats <- current_player_data()
    
    if (!is.null(player_stats) && !is.null(player_stats$per_game) &&
        !is.null(player_stats$game_log) && nrow(player_stats$game_log) > 0) {
      
      stats    <- player_stats$per_game
      game_log <- player_stats$game_log
      
      total_pts <- sum(as.numeric(game_log$PTS), na.rm = TRUE)
      total_2pa <- sum(as.numeric(game_log$`2PA`), na.rm = TRUE)
      total_3pa <- sum(as.numeric(game_log$`3PA`), na.rm = TRUE)
      total_fga <- total_2pa + total_3pa
      total_2pm <- sum(as.numeric(game_log$`2PM`), na.rm = TRUE)
      total_3pm <- sum(as.numeric(game_log$`3PM`), na.rm = TRUE)
      total_fgm <- total_2pm + total_3pm
      total_fta <- sum(as.numeric(game_log$FTA), na.rm = TRUE)
      
      efg_pct <- if (total_fga > 0)
        round((total_fgm + 0.5 * total_3pm) / total_fga * 100, 2) else 0
      ts_denominator <- 2 * (total_fga + 0.44 * total_fta)
      ts_pct <- if (ts_denominator > 0) round(100 * total_pts / ts_denominator, 2) else 0
      ts_pct  <- min(ts_pct, 150)
      efg_pct <- min(efg_pct, 100)
      total_games <- nrow(game_log)
      
      all_ppg <- sapply(rv$aba_data$all_player_stats, function(p) {
        if (!is.null(p$per_game) && nrow(p$per_game) > 0) p$per_game$PPG[1] else NA
      })
      all_rpg <- sapply(rv$aba_data$all_player_stats, function(p) {
        if (!is.null(p$per_game) && nrow(p$per_game) > 0) p$per_game$RPG[1] else NA
      })
      all_apg <- sapply(rv$aba_data$all_player_stats, function(p) {
        if (!is.null(p$per_game) && nrow(p$per_game) > 0) p$per_game$APG[1] else NA
      })
      
      pct_rank <- function(val, all_vals) {
        all_vals <- all_vals[!is.na(all_vals) & all_vals > 0]
        if (length(all_vals) == 0) return(50)
        round(sum(all_vals <= val) / length(all_vals) * 100, 0)
      }
      
      ppg_pct      <- pct_rank(stats$PPG, all_ppg)
      rpg_pct      <- pct_rank(stats$RPG, all_rpg)
      apg_pct      <- pct_rank(stats$APG, all_apg)
      efg_pct_rank <- if (efg_pct > 0)
        pct_rank(efg_pct, c(40,45,48,50,52,55,58,60)) else 50
      ts_pct_rank  <- if (ts_pct > 0)
        pct_rank(ts_pct, c(45,48,50,52,55,58,60,65)) else 50
      
      make_stat_box <- function(value, label, percentile) {
        if      (percentile >= 75) {
          bg <- "background: linear-gradient(135deg, #15803d 0%, #22c55e 100%);"; tc <- "color: white;"
          tag <- "Elite"; tbg <- "background: rgba(255,255,255,0.25);"
        } else if (percentile >= 50) {
          bg <- "background: linear-gradient(135deg, #65a30d 0%, #a3e635 100%);"; tc <- "color: #1a2e05;"
          tag <- "Above Avg"; tbg <- "background: rgba(0,0,0,0.1);"
        } else if (percentile >= 25) {
          bg <- "background: linear-gradient(135deg, #d97706 0%, #fbbf24 100%);"; tc <- "color: #451a03;"
          tag <- "Below Avg"; tbg <- "background: rgba(0,0,0,0.1);"
        } else {
          bg <- "background: linear-gradient(135deg, #b91c1c 0%, #f87171 100%);"; tc <- "color: white;"
          tag <- "Poor"; tbg <- "background: rgba(255,255,255,0.2);"
        }
        div(style = paste0(bg, tc,
                           " padding: 18px 15px; border-radius: 14px; margin: 10px 0;
                            text-align: center; position: relative;
                            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                            backdrop-filter: blur(10px);"),
            span(paste0(tag, " \u00b7 ", percentile, "th"),
                 style = paste0(tbg,
                                " position: absolute; top: 8px; right: 8px;
                                 font-size: 0.6em; font-weight: 700;
                                 padding: 2px 8px; border-radius: 20px; letter-spacing: 0.3px;")),
            h3(value, style = "margin: 8px 0 0 0; font-size: 2.3em; font-weight: 800; letter-spacing: -0.5px;"),
            p(label,  style = "margin: 6px 0 0 0; opacity: 0.85; font-weight: 600; font-size: 0.9em; letter-spacing: 0.5px;"))
      }
      
      efg_display <- if (!is.na(efg_pct) && efg_pct > 0) paste0(efg_pct, "%") else "N/A"
      
      fluidRow(
        column(2, make_stat_box(round(stats$PPG, 2), "PPG", ppg_pct)),
        column(2, make_stat_box(round(stats$RPG, 2), "RPG", rpg_pct)),
        column(2, make_stat_box(round(stats$APG, 2), "APG", apg_pct)),
        column(2, make_stat_box(efg_display,          "eFG%", efg_pct_rank)),
        column(2, make_stat_box(paste0(ts_pct, "%"),  "TS%",  ts_pct_rank)),
        column(2, div(
          style = paste0("background: linear-gradient(135deg, #475569 0%, #64748b 100%);
                          color: white; padding: 20px; border-radius: 10px;
                          margin: 10px 0; text-align: center;"),
          h3(total_games, style = "margin: 0; font-size: 2.2em;"),
          p("Games", style = "margin: 5px 0 0 0; opacity: 0.9;")))
      )
    } else {
      fluidRow(
        column(2, div(class="skeleton skeleton-stat")),
        column(2, div(class="skeleton skeleton-stat")),
        column(2, div(class="skeleton skeleton-stat")),
        column(2, div(class="skeleton skeleton-stat")),
        column(2, div(class="skeleton skeleton-stat")),
        column(2, div(class="skeleton skeleton-stat"))
      )
    }
  })
  
  output$player_recent_games <- renderPlotly({
    req(rv$logged_in, input$player_select)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    player_stats <- current_player_data()
    if (is.null(player_stats) || is.null(player_stats$game_log) ||
        nrow(player_stats$game_log) == 0) return(plotly_empty())
    
    game_log <- player_stats$game_log %>%
      arrange(desc(date)) %>%
      head(as.numeric(input$game_range)) %>%
      mutate(PTS = as.numeric(PTS), REB = as.numeric(REB),
             AST = as.numeric(AST), game_num = row_number())
    
    season_avgs <- player_stats$per_game
    ppg_avg <- if (!is.null(season_avgs)) season_avgs$PPG[1] else 0
    rpg_avg <- if (!is.null(season_avgs)) season_avgs$RPG[1] else 0
    apg_avg <- if (!is.null(season_avgs)) season_avgs$APG[1] else 0
    
    plot_ly(game_log, x = ~game_num) %>%
      add_trace(y = ~PTS, name = 'Points', type = 'scatter', mode = 'lines+markers',
                line   = list(color = ohio_kings_primary, width = 3),
                marker = list(size = 10, color = "black")) %>%
      add_trace(y = ~REB, name = 'Rebounds', type = 'scatter', mode = 'lines+markers',
                line   = list(color = ohio_kings_secondary, width = 3),
                marker = list(size = 8, color = "black")) %>%
      add_trace(y = ~AST, name = 'Assists', type = 'scatter', mode = 'lines+markers',
                line   = list(color = ohio_kings_accent, width = 3),
                marker = list(size = 8, color = "black")) %>%
      layout(
        title     = paste(input$player_select, "- Last", input$game_range, "Games"),
        xaxis     = list(title = "Game (Most Recent \u2192 Oldest)", autorange = "reversed"),
        yaxis     = list(title = "Value"),
        hovermode = "x unified",
        legend    = list(orientation = 'h', y = -0.2),
        margin    = list(r = 80),
        shapes    = list(
          list(type="line", x0=0.5, x1=nrow(game_log)+0.5,
               y0=ppg_avg, y1=ppg_avg,
               line=list(color=ohio_kings_primary, width=1.5, dash="dash")),
          list(type="line", x0=0.5, x1=nrow(game_log)+0.5,
               y0=rpg_avg, y1=rpg_avg,
               line=list(color=ohio_kings_secondary, width=1, dash="dot")),
          list(type="line", x0=0.5, x1=nrow(game_log)+0.5,
               y0=apg_avg, y1=apg_avg,
               line=list(color=ohio_kings_accent, width=1, dash="dot"))
        ),
        annotations = list(
          list(x=nrow(game_log)+0.3, y=ppg_avg+1,
               text=paste0("PPG: ",ppg_avg), showarrow=TRUE,
               arrowhead=0, arrowsize=0.5, ax=40, ay=-15,
               font=list(size=10, color=ohio_kings_primary, weight="bold")),
          list(x=nrow(game_log)+0.3, y=rpg_avg+1,
               text=paste0("RPG: ",rpg_avg), showarrow=TRUE,
               arrowhead=0, arrowsize=0.5, ax=40, ay=-15,
               font=list(size=10, color=ohio_kings_secondary, weight="bold")),
          list(x=nrow(game_log)+0.3, y=apg_avg-1,
               text=paste0("APG: ",apg_avg), showarrow=TRUE,
               arrowhead=0, arrowsize=0.5, ax=40, ay=15,
               font=list(size=10, color=ohio_kings_accent, weight="bold"))
        )
      )
  })
  
  output$player_game_log <- renderDT({
    req(rv$logged_in, input$player_select)
    ohio_kings_primary <- rv$team_cols$primary
    
    player_stats <- current_player_data()
    if (is.null(player_stats) || is.null(player_stats$game_log) ||
        nrow(player_stats$game_log) == 0) return(NULL)
    
    season_avg <- player_stats$per_game
    game_log   <- player_stats$game_log %>%
      arrange(desc(date)) %>%
      head(as.numeric(input$game_range)) %>%
      mutate(PTS_num = suppressWarnings(as.numeric(PTS)),
             REB_num = suppressWarnings(as.numeric(REB)),
             AST_num = suppressWarnings(as.numeric(AST)))
    
    if (!is.null(season_avg) && nrow(season_avg) > 0) {
      game_log <- game_log %>%
        mutate(
          PTS_trend = case_when(
            PTS_num >= season_avg$PPG[1]*1.2 ~ paste0("\U0001f525 ", PTS),
            PTS_num >= season_avg$PPG[1]     ~ paste0("\u25b2 ", PTS),
            PTS_num >= season_avg$PPG[1]*0.8 ~ paste0("\u25bc ", PTS),
            TRUE                             ~ paste0("\u2b07 ", PTS)
          ),
          REB_trend = case_when(
            REB_num >= season_avg$RPG[1]*1.2 ~ paste0("\U0001f525 ", REB),
            REB_num >= season_avg$RPG[1]     ~ paste0("\u25b2 ", REB),
            TRUE                             ~ paste0("\u25bc ", REB)
          ),
          AST_trend = case_when(
            AST_num >= season_avg$APG[1]*1.2 ~ paste0("\U0001f525 ", AST),
            AST_num >= season_avg$APG[1]     ~ paste0("\u25b2 ", AST),
            TRUE                             ~ paste0("\u25bc ", AST)
          )
        )
      game_log$PTS <- game_log$PTS_trend
      game_log$REB <- game_log$REB_trend
      game_log$AST <- game_log$AST_trend
    }
    
    display_cols <- game_log %>%
      select(-matches("PTS_num|REB_num|AST_num|PTS_trend|REB_trend|AST_trend"))
    
    dt <- datatable(
      display_cols,
      options  = list(pageLength=15, scrollX=TRUE,
                      columnDefs=list(list(className='dt-center',targets='_all'))),
      rownames = FALSE, escape = FALSE
    )
    
    if ("PTS" %in% names(display_cols) && !is.null(season_avg) && nrow(season_avg) > 0) {
      dt <- dt %>% formatStyle(
        'PTS',
        backgroundColor = styleInterval(
          c(season_avg$PPG[1]*0.8, season_avg$PPG[1], season_avg$PPG[1]*1.2),
          c('#fee2e2','#fef3c7','#d1fae5','#bbf7d0')
        )
      )
    }
    dt
  })
  
  output$player_scoring_donut <- renderPlotly({
    req(rv$logged_in, input$player_select)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    player_stats <- current_player_data()
    if (is.null(player_stats) || is.null(player_stats$game_log) ||
        nrow(player_stats$game_log) == 0) return(plotly_empty())
    
    gl        <- player_stats$game_log
    total_2pm <- sum(suppressWarnings(as.numeric(gl$`2PM`)), na.rm=TRUE)
    total_2pa <- sum(suppressWarnings(as.numeric(gl$`2PA`)), na.rm=TRUE)
    total_3pm <- sum(suppressWarnings(as.numeric(gl$`3PM`)), na.rm=TRUE)
    total_3pa <- sum(suppressWarnings(as.numeric(gl$`3PA`)), na.rm=TRUE)
    total_ftm <- sum(suppressWarnings(as.numeric(gl$FTM)),   na.rm=TRUE)
    total_fta <- sum(suppressWarnings(as.numeric(gl$FTA)),   na.rm=TRUE)
    miss_2    <- pmax(total_2pa - total_2pm, 0)
    miss_3    <- pmax(total_3pa - total_3pm, 0)
    miss_ft   <- pmax(total_fta - total_ftm, 0)
    categories <- c("2-Pointers","3-Pointers","Free Throws")
    made_vals  <- c(total_2pm, total_3pm, total_ftm)
    att_vals   <- c(total_2pa, total_3pa, total_fta)
    miss_vals  <- c(miss_2, miss_3, miss_ft)
    pct_labels <- ifelse(att_vals > 0,
                         paste0(round(made_vals/att_vals*100,1),"%"), "")
    
    plot_ly() %>%
      add_trace(x=categories, y=made_vals, type='bar', name='Made',
                marker=list(color=c(ohio_kings_primary,ohio_kings_secondary,ohio_kings_accent),
                            line=list(color='#ffffff',width=1.5)),
                hovertemplate='%{x}: %{y} made<extra></extra>') %>%
      add_trace(x=categories, y=miss_vals, type='bar', name='Missed',
                text=pct_labels, textposition='outside', cliponaxis=FALSE,
                textfont=list(color='#334155',size=13,family='Arial Black'),
                marker=list(color='rgba(148,163,184,0.4)',
                            line=list(color='#ffffff',width=1.5)),
                hovertemplate='%{x}: %{y} missed<extra></extra>') %>%
      layout(barmode='stack',
             xaxis=list(title='',tickfont=list(size=13,color='#334155')),
             yaxis=list(title='Attempts',range=c(0,max(att_vals)*1.2)),
             legend=list(orientation='h',y=-0.15,x=0.5,xanchor='center'),
             margin=list(l=40,r=20,t=20,b=60),
             plot_bgcolor='rgba(0,0,0,0)',paper_bgcolor='rgba(0,0,0,0)')
  })
  
  output$player_advanced_metrics <- renderUI({
    req(rv$logged_in, input$player_select)
    ohio_kings_primary <- rv$team_cols$primary
    
    player_stats <- current_player_data()
    if (is.null(player_stats) || is.null(player_stats$per_game) ||
        is.null(player_stats$game_log) || nrow(player_stats$game_log) == 0)
      return(p("Select a player to view advanced metrics",
               style = "text-align: center; color: #999; padding: 20px;"))
    
    tryCatch({
      stats    <- player_stats$per_game
      game_log <- player_stats$game_log
      
      pts_vector <- suppressWarnings(as.numeric(game_log$PTS))
      pts_sd     <- sd(pts_vector, na.rm=TRUE); if (is.na(pts_sd)) pts_sd <- 10
      consistency_score   <- max(0, 100 - pts_sd * 5)
      consistency_rating  <- if (pts_sd < 5) "Very Consistent" else if (pts_sd < 8) "Consistent"
      else if (pts_sd < 12) "Moderate" else "Inconsistent"
      
      recent     <- game_log %>% arrange(desc(date)) %>% head(3)
      recent_avg <- mean(suppressWarnings(as.numeric(recent$PTS)), na.rm=TRUE)
      season_avg_ppg <- stats$PPG[1]
      streak_diff <- if (!is.na(recent_avg) && !is.na(season_avg_ppg) && season_avg_ppg > 0)
        round(((recent_avg - season_avg_ppg) / season_avg_ppg) * 100, 1) else 0
      streak_status <- if (streak_diff > 15) "\U0001f525 Hot"
      else if (streak_diff < -15) "\u2744\ufe0f Cold" else "\u27a1\ufe0f Steady"
      streak_border <- if (streak_diff > 15) "#15803d" else if (streak_diff > 0) "#65a30d"
      else if (streak_diff > -15) "#d97706" else "#b91c1c"
      streak_bg     <- if (streak_diff > 15) "#f0fdf4" else if (streak_diff > 0) "#f7fee7"
      else if (streak_diff > -15) "#fffbeb" else "#fef2f2"
      
      est_mpg    <- if (stats$PPG[1] > 15) 32 else 20
      per_36_ppg <- round((stats$PPG[1] / est_mpg) * 36, 2)
      per_36_rpg <- round((stats$RPG[1] / est_mpg) * 36, 2)
      per_36_apg <- round((stats$APG[1] / est_mpg) * 36, 2)
      
      avg_game_score <- tryCatch({
        gs_data <- game_log %>%
          mutate(
            fgm  = suppressWarnings(as.numeric(`2PM`)) + suppressWarnings(as.numeric(`3PM`)),
            fga  = suppressWarnings(as.numeric(`2PA`)) + suppressWarnings(as.numeric(`3PA`)),
            pts  = suppressWarnings(as.numeric(PTS)),
            oreb = if ("OREB" %in% names(.)) suppressWarnings(as.numeric(OREB)) else 0,
            dreb = if ("DREB" %in% names(.)) suppressWarnings(as.numeric(DREB)) else 0,
            ast  = suppressWarnings(as.numeric(AST)),
            stl  = suppressWarnings(as.numeric(STL)),
            blk  = suppressWarnings(as.numeric(BLK)),
            pf   = if ("PF" %in% names(.)) suppressWarnings(as.numeric(PF)) else 0,
            to   = if ("TO" %in% names(.)) suppressWarnings(as.numeric(TO)) else 0,
            game_score = pts + 0.4*fgm - 0.7*fga + 0.7*oreb + 0.3*dreb +
              stl + 0.7*ast + 0.7*blk - 0.4*pf - to
          )
        round(mean(gs_data$game_score, na.rm=TRUE), 2)
      }, error = function(e) 0)
      if (is.na(avg_game_score)) avg_game_score <- 0
      
      gs_pct      <- if (avg_game_score >= 15) 85 else if (avg_game_score >= 10) 65
      else if (avg_game_score >= 5) 40 else 15
      consist_pct <- if (pts_sd < 5) 85 else if (pts_sd < 8) 65
      else if (pts_sd < 12) 40 else 15
      
      player_adv_box <- function(value, label, sublabel, percentile) {
        if      (percentile >= 75) {
          bg <- "background: linear-gradient(135deg, #15803d 0%, #22c55e 100%);"; tc <- "color: white;"
          tag <- "Elite"; tbg <- "background: rgba(255,255,255,0.25);"
        } else if (percentile >= 50) {
          bg <- "background: linear-gradient(135deg, #65a30d 0%, #a3e635 100%);"; tc <- "color: #1a2e05;"
          tag <- "Above Avg"; tbg <- "background: rgba(0,0,0,0.1);"
        } else if (percentile >= 25) {
          bg <- "background: linear-gradient(135deg, #d97706 0%, #fbbf24 100%);"; tc <- "color: #451a03;"
          tag <- "Below Avg"; tbg <- "background: rgba(0,0,0,0.1);"
        } else {
          bg <- "background: linear-gradient(135deg, #b91c1c 0%, #f87171 100%);"; tc <- "color: white;"
          tag <- "Poor"; tbg <- "background: rgba(255,255,255,0.2);"
        }
        div(style = paste0(bg, tc,
                           " padding: 18px 15px; border-radius: 14px; text-align: center;
                            margin: 10px 0; position: relative;
                            box-shadow: 0 4px 12px rgba(0,0,0,0.08);
                            border: 1px solid rgba(255,255,255,0.15);"),
            span(paste0(tag, " \u00b7 ", percentile, "th"),
                 style = paste0(tbg,
                                " position: absolute; top: 8px; right: 8px;
                                 font-size: 0.6em; font-weight: 700;
                                 padding: 2px 8px; border-radius: 20px;")),
            h3(value,   style = "margin: 8px 0 0 0; font-size: 2.3em; font-weight: 800;"),
            p(label,    style = "margin: 5px 0 0 0; opacity: 0.9; font-weight: 600;"),
            p(sublabel, style = "font-size: 0.8em; opacity: 0.75; margin: 4px 0 0 0;"))
      }
      
      div(
        fluidRow(
          column(4, player_adv_box(avg_game_score, "Game Score", "Overall impact", gs_pct)),
          column(4, player_adv_box(round(pts_sd,2), "Consistency (SD)", consistency_rating, consist_pct)),
          column(4, player_adv_box(paste0(round(stats$PPG[1],1),"/",
                                          round(stats$RPG[1],1),"/",
                                          round(stats$APG[1],1)),
                                   "Season Line", "PPG/RPG/APG", 50))
        ),
        fluidRow(
          column(6,
                 div(style = paste0("background:", streak_bg,
                                    "; border-left: 5px solid ", streak_border,
                                    "; padding: 20px; border-radius: 12px; margin: 10px 0;
                                box-shadow: 0 2px 8px rgba(0,0,0,0.04);"),
                     h4(streak_status, style = "margin: 0; color: #1e293b; font-size: 1.2em;"),
                     p("Last 3 Games Trend", style = "margin: 5px 0; color: #64748b; font-size: 0.9em;"),
                     p(paste0(ifelse(streak_diff > 0, "+", ""), streak_diff, "% vs season avg"),
                       style = paste0("margin: 5px 0 0 0; font-size: 1.15em; font-weight: 700; color: ",
                                      streak_border, ";")))
          ),
          column(6,
                 div(style = "background: #f8fafc; border-left: 5px solid #6366f1;
                          padding: 20px; border-radius: 12px; margin: 10px 0;
                          box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
                     h4("\U0001f4ca Per-36 Minutes",
                        style = "margin: 0; color: #1e293b; font-size: 1.2em;"),
                     p("Normalized stats", style = "margin: 5px 0; color: #64748b; font-size: 0.9em;"),
                     div(style = "display: flex; gap: 20px; margin-top: 8px;",
                         div(style = "text-align: center;",
                             div(per_36_ppg, style="font-size:1.4em;font-weight:800;color:#1e293b;"),
                             div("PPG", style="font-size:0.75em;color:#64748b;font-weight:600;")),
                         div(style = "text-align: center;",
                             div(per_36_rpg, style="font-size:1.4em;font-weight:800;color:#1e293b;"),
                             div("RPG", style="font-size:0.75em;color:#64748b;font-weight:600;")),
                         div(style = "text-align: center;",
                             div(per_36_apg, style="font-size:1.4em;font-weight:800;color:#1e293b;"),
                             div("APG", style="font-size:0.75em;color:#64748b;font-weight:600;"))))
          )
        )
      )
    }, error = function(e) {
      div(style="background:#fee2e2;border-left:4px solid #ef4444;padding:15px;border-radius:8px;margin:10px;",
          p(paste("Error calculating advanced metrics:", e$message),
            style="color:#991b1b;margin:0;"))
    })
  })
  
  output$player_impact_rating <- renderUI({
    req(rv$logged_in, input$player_select)
    ohio_kings_primary <- rv$team_cols$primary
    
    player_stats <- current_player_data()
    if (is.null(player_stats) || is.null(player_stats$per_game) ||
        is.null(player_stats$game_log) || nrow(player_stats$game_log) == 0)
      return(p("Select a player to view impact rating",
               style = "text-align: center; color: #999; padding: 20px;"))
    
    tryCatch({
      stats  <- player_stats$per_game
      gl     <- player_stats$game_log
      ppg    <- stats$PPG[1]; rpg <- stats$RPG[1]; apg <- stats$APG[1]
      spg    <- stats$SPG[1]; bpg <- stats$BPG[1]
      fg2    <- if (!is.na(stats$FG2_pct[1])) stats$FG2_pct[1] else 0
      fg3    <- if (!is.na(stats$FG3_pct[1])) stats$FG3_pct[1] else 0
      ft_p   <- if (!is.na(stats$FT_pct[1]))  stats$FT_pct[1]  else 0
      games  <- nrow(gl)
      
      pts_sd <- sd(suppressWarnings(as.numeric(gl$PTS)), na.rm=TRUE)
      if (is.na(pts_sd)) pts_sd <- 10
      consistency_score <- max(0, 100 - pts_sd * 5)
      efficiency_score  <- (fg2 * 0.5 + fg3 * 0.3 + ft_p * 0.2)
      availability      <- min(100, games / 10 * 100)
      versatility_score <- min(100, (
        min(ppg/20,1)*30 + min(rpg/8,1)*25 +
          min(apg/6,1)*25 + min((spg+bpg)/3,1)*20
      ))
      
      all_ppg_pir <- sapply(rv$aba_data$all_player_stats, function(pl) {
        if (!is.null(pl$per_game) && nrow(pl$per_game) > 0) pl$per_game$PPG[1] else NA
      })
      all_ppg_pir    <- all_ppg_pir[!is.na(all_ppg_pir) & all_ppg_pir > 0]
      ppg_percentile <- sum(all_ppg_pir <= ppg) / length(all_ppg_pir) * 100
      
      pir <- round(
        ppg_percentile    * 0.30 +
          efficiency_score  * 0.25 +
          versatility_score * 0.20 +
          consistency_score * 0.15 +
          availability      * 0.10, 1)
      
      grade     <- if (pir >= 70) "Elite" else if (pir >= 55) "Starter"
      else if (pir >= 40) "Rotation" else "Bench"
      grade_col <- if (pir >= 70) "#15803d" else if (pir >= 55) "#65a30d"
      else if (pir >= 40) "#d97706" else "#b91c1c"
      grade_bg  <- if (pir >= 70) "linear-gradient(135deg,#15803d,#22c55e)"
      else if (pir >= 55) "linear-gradient(135deg,#65a30d,#a3e635)"
      else if (pir >= 40) "linear-gradient(135deg,#d97706,#fbbf24)"
      else "linear-gradient(135deg,#b91c1c,#f87171)"
      
      # League rank
      all_pirs <- sapply(names(rv$aba_data$all_player_stats), function(pname) {
        tryCatch({
          pl <- rv$aba_data$all_player_stats[[pname]]
          if (is.null(pl$per_game) || nrow(pl$per_game)==0 ||
              is.null(pl$game_log)  || nrow(pl$game_log)<3) return(NA)
          ps     <- pl$per_game
          p_ppg  <- ps$PPG[1]; p_rpg <- ps$RPG[1]; p_apg <- ps$APG[1]
          p_spg  <- ps$SPG[1]; p_bpg <- ps$BPG[1]
          p_fg2  <- if (!is.na(ps$FG2_pct[1])) ps$FG2_pct[1] else 0
          p_fg3  <- if (!is.na(ps$FG3_pct[1])) ps$FG3_pct[1] else 0
          p_ft   <- if (!is.na(ps$FT_pct[1]))  ps$FT_pct[1]  else 0
          p_sd   <- sd(suppressWarnings(as.numeric(pl$game_log$PTS)), na.rm=TRUE)
          if (is.na(p_sd)) p_sd <- 10
          p_eff  <- p_fg2*0.5 + p_fg3*0.3 + p_ft*0.2
          p_ppct <- sum(all_ppg_pir <= p_ppg) / length(all_ppg_pir) * 100
          p_vers <- min(100,(min(p_ppg/20,1)*30+min(p_rpg/8,1)*25+
                               min(p_apg/6,1)*25+min((p_spg+p_bpg)/3,1)*20))
          p_cons <- max(0,100-p_sd*5)
          p_avail <- min(100,nrow(pl$game_log)/10*100)
          round(p_ppct*0.30+p_eff*0.25+p_vers*0.20+p_cons*0.15+p_avail*0.10,1)
        }, error=function(e) NA)
      })
      all_pirs    <- all_pirs[!is.na(all_pirs)]
      league_rank <- sum(all_pirs > pir) + 1
      league_pct  <- round(sum(all_pirs <= pir) / length(all_pirs) * 100, 0)
      
      div(
        div(style="display:flex;gap:20px;flex-wrap:wrap;",
            # Left big number
            div(style=paste0("flex:0 0 200px;background:",grade_bg,
                             ";color:white;border-radius:14px;padding:24px;",
                             "text-align:center;box-shadow:0 4px 16px rgba(0,0,0,0.1);"),
                div("PLAYER IMPACT RATING",
                    style="font-size:0.7em;font-weight:700;opacity:0.8;letter-spacing:1px;"),
                h1(pir, style="margin:10px 0 5px 0;font-size:3.5em;font-weight:800;"),
                div("/ 100",style="font-size:0.9em;opacity:0.7;"),
                div(grade,
                    style=paste0("margin-top:8px;font-weight:700;font-size:1.1em;",
                                 "background:rgba(255,255,255,0.2);padding:4px 16px;",
                                 "border-radius:20px;display:inline-block;")),
                div(paste0("#",league_rank," in league (",league_pct,"th %ile)"),
                    style="margin-top:8px;font-size:0.75em;opacity:0.8;")
            ),
            # Right components
            div(style="flex:1;min-width:300px;",
                h4("Rating Components",
                   style="margin:0 0 12px 0;color:#334155;font-weight:700;"),
                lapply(list(
                  list("Scoring Volume (30%)",   ppg_percentile,    paste0(ppg," PPG")),
                  list("Efficiency (25%)",        efficiency_score,  paste0("2P:",round(fg2,0),"% 3P:",round(fg3,0),"% FT:",round(ft_p,0),"%")),
                  list("Versatility (20%)",       versatility_score, paste0(ppg,"/",rpg,"/",apg,"/",round(spg+bpg,1))),
                  list("Consistency (15%)",       consistency_score, paste0("\u00b1",round(pts_sd,1)," pts SD")),
                  list("Availability (10%)",      availability,      paste0(games," games played"))
                ), function(comp) {
                  cv   <- round(comp[[2]],1)
                  ccol <- if (cv>=70) "#15803d" else if (cv>=50) "#65a30d"
                  else if (cv>=30) "#d97706" else "#b91c1c"
                  div(style="margin-bottom:10px;",
                      div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;",
                          span(comp[[1]], style="font-size:0.82em;font-weight:600;color:#475569;"),
                          div(style="display:flex;align-items:center;gap:6px;",
                              span(comp[[3]], style="font-size:0.72em;color:#94a3b8;"),
                              span(cv, style=paste0("font-size:0.82em;font-weight:700;color:",ccol,";")))),
                      div(style="height:8px;background:#e2e8f0;border-radius:4px;overflow:hidden;",
                          div(style=paste0("width:",min(cv,100),"%;height:100%;background:",
                                           ccol,";border-radius:4px;"))))
                })
            )
        )
      )
    }, error = function(e) {
      div(style="background:#fee2e2;border-left:4px solid #ef4444;padding:15px;border-radius:8px;",
          p(paste("Error calculating impact rating:", e$message),
            style="color:#991b1b;margin:0;"))
    })
  })
  
  output$player_comparison <- renderUI({
    req(rv$logged_in, input$compare_player_1, rv$aba_data)
    ohio_kings_primary <- rv$team_cols$primary
    
    players_to_compare <- c(input$compare_player_1, input$compare_player_2, input$compare_player_3)
    players_to_compare <- players_to_compare[players_to_compare != "" & !is.na(players_to_compare)]
    if (length(players_to_compare) < 2)
      return(p("Select at least 2 players to compare",
               style="text-align:center;color:#999;padding:20px;"))
    
    comparison_data <- lapply(players_to_compare, function(pname) {
      if (pname %in% names(rv$aba_data$all_player_stats)) {
        pd <- rv$aba_data$all_player_stats[[pname]]
        if (!is.null(pd$per_game)) {
          stats <- pd$per_game
          data.frame(Player  = pname, PPG = stats$PPG[1], RPG = stats$RPG[1],
                     APG = stats$APG[1], SPG = stats$SPG[1], BPG = stats$BPG[1],
                     FG2_pct = stats$FG2_pct[1], FG3_pct = stats$FG3_pct[1],
                     FT_pct  = stats$FT_pct[1])
        }
      }
    })
    comp_df <- bind_rows(comparison_data)
    if (nrow(comp_df) == 0)
      return(p("Player data not available",
               style="text-align:center;color:#999;padding:20px;"))
    
    div(
      div(style="overflow-x:auto;margin-bottom:20px;",
          tags$table(class="table table-striped table-hover",style="width:100%;",
                     tags$thead(tags$tr(
                       tags$th("Stat", style="background:#f8fafc;padding:12px;font-weight:600;"),
                       lapply(comp_df$Player, function(pl)
                         tags$th(pl, style=paste0("background:",ohio_kings_primary,
                                                  ";color:white;padding:12px;text-align:center;")))
                     )),
                     tags$tbody(lapply(list(
                       list("PPG",  comp_df$PPG),
                       list("RPG",  comp_df$RPG),
                       list("APG",  comp_df$APG),
                       list("2P%",  paste0(comp_df$FG2_pct,"%")),
                       list("3P%",  paste0(comp_df$FG3_pct,"%")),
                       list("FT%",  paste0(comp_df$FT_pct,"%"))
                     ), function(row) {
                       tags$tr(
                         tags$td(row[[1]], style="padding:10px;font-weight:600;"),
                         lapply(row[[2]], function(v)
                           tags$td(v, style="text-align:center;padding:10px;"))
                       )
                     }))
          )
      ),
      div(h4("Performance Comparison",
             style="margin-top:20px;color:#334155;"),
          plotlyOutput("player_comparison_radar", height="400px"))
    )
  })
  
  output$player_comparison_radar <- renderPlotly({
    req(rv$logged_in, input$compare_player_1)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    players_to_compare <- c(input$compare_player_1, input$compare_player_2, input$compare_player_3)
    players_to_compare <- players_to_compare[players_to_compare != "" & !is.na(players_to_compare)]
    if (length(players_to_compare) < 2) return(plotly_empty())
    
    raw_data <- lapply(players_to_compare, function(pname) {
      if (pname %in% names(rv$aba_data$all_player_stats)) {
        pd <- rv$aba_data$all_player_stats[[pname]]
        if (!is.null(pd$per_game)) {
          stats <- pd$per_game
          data.frame(Player=pname, PPG=stats$PPG[1], RPG=stats$RPG[1],
                     APG=stats$APG[1], Defense=stats$SPG[1]+stats$BPG[1],
                     Shooting=stats$FG2_pct[1], stringsAsFactors=FALSE)
        }
      }
    })
    raw_df <- bind_rows(raw_data)
    if (nrow(raw_df) < 2) return(plotly_empty())
    
    all_stats <- lapply(names(rv$aba_data$all_player_stats), function(pname) {
      pd <- rv$aba_data$all_player_stats[[pname]]
      if (!is.null(pd$per_game) && nrow(pd$per_game)>0 &&
          !is.null(pd$game_log)  && nrow(pd$game_log)>=3) {
        stats <- pd$per_game
        data.frame(PPG=stats$PPG[1], RPG=stats$RPG[1], APG=stats$APG[1],
                   Defense=stats$SPG[1]+stats$BPG[1],
                   Shooting=stats$FG2_pct[1], stringsAsFactors=FALSE)
      }
    })
    league_df <- bind_rows(all_stats)
    
    calc_pct <- function(value, all_values) {
      all_values <- all_values[!is.na(all_values) & all_values > 0]
      if (length(all_values) == 0) return(50)
      round(sum(all_values <= value) / length(all_values) * 100, 1)
    }
    
    radar_df <- raw_df %>%
      mutate(
        Scoring    = sapply(PPG,      calc_pct, league_df$PPG),
        Rebounding = sapply(RPG,      calc_pct, league_df$RPG),
        Assists    = sapply(APG,      calc_pct, league_df$APG),
        Def        = sapply(Defense,  calc_pct, league_df$Defense),
        Shot       = sapply(Shooting, calc_pct, league_df$Shooting)
      )
    
    colors     <- c(ohio_kings_primary, ohio_kings_secondary, ohio_kings_accent)
    categories <- c('Scoring','Rebounding','Assists','Defense','Shooting','Scoring')
    
    p <- plot_ly(type='scatterpolar', mode='lines', fill='toself')
    for (i in 1:nrow(radar_df)) {
      vals  <- c(radar_df$Scoring[i], radar_df$Rebounding[i], radar_df$Assists[i],
                 radar_df$Def[i],     radar_df$Shot[i],       radar_df$Scoring[i])
      hover <- paste0(
        c('Scoring','Rebounding','Assists','Defense','Shooting'), ': ',
        round(c(raw_df$PPG[i],raw_df$RPG[i],raw_df$APG[i],
                raw_df$Defense[i],raw_df$Shooting[i]),1),
        ' (', round(c(radar_df$Scoring[i],radar_df$Rebounding[i],radar_df$Assists[i],
                      radar_df$Def[i],radar_df$Shot[i]),0), 'th %ile)')
      p <- p %>% add_trace(
        r = vals, theta = categories, name = radar_df$Player[i],
        line      = list(color = colors[min(i,length(colors))], width = 3),
        fillcolor = paste0(colors[min(i,length(colors))], '30'),
        hovertext = c(hover, hover[1]), hoverinfo = 'text+name'
      )
    }
    p %>% layout(
      polar      = list(radialaxis=list(visible=TRUE, range=c(0,100),
                                        tickvals=c(25,50,75,100),
                                        ticktext=c("25th","50th","75th","100th"))),
      showlegend = TRUE, title = "League Percentile Comparison"
    )
  })
  
  # ==========================================================================
  # COACHING TAB — build_lineup and predict_player_game helpers
  # ==========================================================================
  
  predict_player_game <- function(player_name, opponent_team_name) {
    if (!player_name %in% names(rv$aba_data$all_player_stats))
      return(list(pts=NA,reb=NA,ast=NA))
    p_data <- rv$aba_data$all_player_stats[[player_name]]
    if (is.null(p_data$per_game)||nrow(p_data$per_game)==0||
        is.null(p_data$game_log)||nrow(p_data$game_log)<3)
      return(list(pts=NA,reb=NA,ast=NA))
    
    pg  <- p_data$per_game; gl <- p_data$game_log
    pts_rolling <- pg$PPG[1]; reb_rolling <- pg$RPG[1]; ast_rolling <- pg$APG[1]
    fg2 <- if (!is.na(pg$FG2_pct[1])) pg$FG2_pct[1] else 45
    fg3 <- if (!is.na(pg$FG3_pct[1])) pg$FG3_pct[1] else 30
    ft  <- if (!is.na(pg$FT_pct[1]))  pg$FT_pct[1]  else 70
    sh_pct    <- (fg2 + fg3) / 2
    two_att   <- mean(suppressWarnings(as.numeric(gl$`2PA`)),na.rm=TRUE)
    three_att <- mean(suppressWarnings(as.numeric(gl$`3PA`)),na.rm=TRUE)
    ft_att    <- mean(suppressWarnings(as.numeric(gl$FTA)),  na.rm=TRUE)
    if (is.na(two_att))   two_att   <- 5
    if (is.na(three_att)) three_att <- 3
    if (is.na(ft_att))    ft_att    <- 3
    
    recent  <- gl %>% arrange(desc(date)) %>% head(3)
    pts_l3  <- mean(suppressWarnings(as.numeric(recent$PTS)),na.rm=TRUE)
    reb_l3  <- mean(suppressWarnings(as.numeric(recent$REB)),na.rm=TRUE)
    ast_l3  <- mean(suppressWarnings(as.numeric(recent$AST)),na.rm=TRUE)
    if (is.na(pts_l3)) pts_l3 <- pts_rolling
    if (is.na(reb_l3)) reb_l3 <- reb_rolling
    if (is.na(ast_l3)) ast_l3 <- ast_rolling
    
    avg_pts_for <- get_accurate_team_ppg(opponent_team_name, rv$aba_data)
    if (avg_pts_for == 0) avg_pts_for <- 95
    avg_pts_against <- 95
    if (!is.null(rv$aba_data$team_schedules) &&
        opponent_team_name %in% names(rv$aba_data$team_schedules)) {
      opp_sched <- rv$aba_data$team_schedules[[opponent_team_name]]
      rg <- opp_sched %>% filter(!is_forfeit, team_score > 0)
      if (nrow(rg) > 0) avg_pts_against <- mean(rg$opp_score, na.rm=TRUE)
    }
    
    predicted_pts <- max(0, round(
      -2.432641170 +
        0.802291414 * pts_rolling  + 0.002380790 * reb_rolling +
        0.034955651 * sh_pct       + 0.537215789 * two_att     +
        0.280946978 * three_att    + 0.018800949 * ft          +
        -0.049714341 * ft_att       + 0.177013889 * pts_l3      +
        0.219779695 * reb_l3      + -0.048098736 * avg_pts_for +
        0.054106630 * avg_pts_against + 1.106096512 * 0.5      +
        -0.017647906 * (sh_pct * two_att)   +
        -0.008515466 * (sh_pct * three_att) +
        -0.001245315 * (ft * ft_att), 1))
    
    opp_pace       <- avg_pts_for / 95
    predicted_reb  <- max(0, round(reb_rolling*0.70 + reb_l3*0.20 + reb_rolling*opp_pace*0.10, 1))
    predicted_ast  <- max(0, round(ast_rolling*0.70 + ast_l3*0.25 + ast_rolling*0.05, 1))
    list(pts=predicted_pts, reb=predicted_reb, ast=predicted_ast)
  }
  
  build_lineup <- function(team_name, criteria, aba_data = NULL) {
    data_source <- if (!is.null(aba_data)) aba_data else rv$aba_data
    if (is.null(data_source)) return(NULL)
    roster <- data_source$all_team_stats[[team_name]]
    if (is.null(roster) || nrow(roster) == 0) return(NULL)
    
    roster <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
    team_record    <- data_source$league_data %>% filter(Team == team_name)
    games_played_t <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
    
    roster <- roster %>%
      mutate(
        Position = sapply(Player, function(pl) {
          if (pl %in% names(data_source$all_player_stats)) {
            pos <- data_source$all_player_stats[[pl]]$position
            if (!is.null(pos) && !is.na(pos) && pos != "") return(pos)
          }
          return("G")
        }),
        GP_individual = sapply(Player, function(pl) {
          if (pl %in% names(data_source$all_player_stats)) {
            gl <- data_source$all_player_stats[[pl]]$game_log
            if (!is.null(gl)) return(nrow(gl))
          }
          return(0)
        })
      ) %>%
      filter(GP_individual >= 3)
    
    if (nrow(roster) == 0) return(NULL)
    
    roster <- roster %>%
      mutate(
        PTS = as.numeric(PTS), REB = as.numeric(REB), AST = as.numeric(AST),
        STL = if ("STL" %in% names(.)) as.numeric(STL) else 0,
        BLK = if ("BLK" %in% names(.)) as.numeric(BLK) else 0,
        `2PM` = if ("2PM" %in% names(.)) as.numeric(`2PM`) else 0,
        `2PA` = if ("2PA" %in% names(.)) as.numeric(`2PA`) else 1,
        `3PM` = if ("3PM" %in% names(.)) as.numeric(`3PM`) else 0,
        `3PA` = if ("3PA" %in% names(.)) as.numeric(`3PA`) else 1,
        FTM   = if ("FTM" %in% names(.)) as.numeric(FTM)   else 0,
        FTA   = if ("FTA" %in% names(.)) as.numeric(FTA)   else 1
      ) %>%
      mutate(
        PPG = sapply(Player, function(pl) {
          if (pl %in% names(data_source$all_player_stats)) {
            pg <- data_source$all_player_stats[[pl]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$PPG[1])) return(pg$PPG[1])
          }
          round(PTS / games_played_t, 2)
        }),
        RPG = sapply(Player, function(pl) {
          if (pl %in% names(data_source$all_player_stats)) {
            pg <- data_source$all_player_stats[[pl]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$RPG[1])) return(pg$RPG[1])
          }
          round(REB / games_played_t, 2)
        }),
        APG = sapply(Player, function(pl) {
          if (pl %in% names(data_source$all_player_stats)) {
            pg <- data_source$all_player_stats[[pl]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$APG[1])) return(pg$APG[1])
          }
          round(AST / games_played_t, 2)
        }),
        SPG     = round(STL / games_played_t, 2),
        BPG     = round(BLK / games_played_t, 2),
        FG2_pct = if_else(`2PA` > 0, round(100 * `2PM` / `2PA`, 2), 0),
        FG3_pct = if_else(`3PA` > 0, round(100 * `3PM` / `3PA`, 2), 0),
        FT_pct  = if_else(FTA   > 0, round(100 * FTM   / FTA,   2), 0)
      ) %>%
      mutate(across(where(is.numeric), ~ifelse(is.nan(.)|is.infinite(.), 0, .))) %>%
      mutate(
        situation_score = case_when(
          criteria == "scoring"      ~ PPG,
          criteria == "off_down2"    ~ (FG2_pct/100) * PPG,
          criteria == "off_down3"    ~ (FG3_pct/100) * (PPG*0.5 + `3PM`/GP_individual*5),
          criteria == "ft_situation" ~ (FT_pct/100)  * PPG,
          criteria == "def_last"     ~ RPG*2 + SPG*0.5 + BPG*0.5,
          criteria == "need_stop"    ~ SPG*2 + BPG*2 + RPG,
          TRUE ~ PPG
        )
      ) %>%
      arrange(desc(situation_score))
    
    can_play_pos <- function(player_pos, target_pos) {
      flex <- list(
        "PG"=c("PG","SG"), "SG"=c("PG","SG","SF"), "G"=c("PG","SG","SF"),
        "SF"=c("SG","SF","PF"), "F"=c("SF","PF","C"), "PF"=c("SF","PF","C"), "C"=c("PF","C")
      )
      if (is.null(player_pos)||is.na(player_pos)||player_pos=="") return(TRUE)
      if (player_pos %in% names(flex)) return(target_pos %in% flex[[player_pos]])
      return(TRUE)
    }
    
    target_positions <- c("C","PF","SF","SG","PG")
    final_lineup     <- data.frame()
    used_players     <- c()
    
    for (target_pos in target_positions) {
      eligible         <- roster %>% filter(!Player %in% used_players)
      eligible_for_pos <- eligible %>%
        filter(sapply(Position, function(pos) can_play_pos(pos, target_pos)))
      
      if (nrow(eligible_for_pos) > 0) {
        best_player  <- eligible_for_pos %>% arrange(desc(situation_score)) %>% head(1) %>%
          mutate(lineup_position = target_pos)
        final_lineup <- bind_rows(final_lineup, best_player)
        used_players <- c(used_players, best_player$Player)
      } else {
        fallback_order <- list(
          "C"=c("PF","SF","SG","PG"), "PF"=c("C","SF","SG","PG"),
          "SF"=c("PF","SG","C","PG"), "SG"=c("SF","PG","PF","C"), "PG"=c("SG","SF","PF","C")
        )
        for (fb_pos in fallback_order[[target_pos]]) {
          available_fb <- eligible %>% filter(Position == fb_pos) %>%
            arrange(desc(situation_score)) %>% head(1)
          if (nrow(available_fb) > 0) {
            available_fb <- available_fb %>% mutate(lineup_position = target_pos)
            final_lineup <- bind_rows(final_lineup, available_fb)
            used_players <- c(used_players, available_fb$Player)
            break
          }
        }
      }
    }
    
    final_lineup <- final_lineup %>%
      mutate(position_order = case_when(
        lineup_position=="PG"~1, lineup_position=="SG"~2, lineup_position=="SF"~3,
        lineup_position=="PF"~4, lineup_position=="C"~5, TRUE~6)) %>%
      arrange(position_order)
    
    all_scores <- roster$situation_score
    final_lineup <- final_lineup %>%
      mutate(
        team_rank       = sapply(situation_score, function(sc) sum(all_scores > sc) + 1),
        team_percentile = sapply(situation_score, function(sc)
          round(sum(all_scores <= sc) / length(all_scores) * 100, 0))
      ) %>%
      rowwise() %>%
      mutate(
        league_percentile = {
          pos_players <- lapply(names(data_source$all_player_stats), function(pname) {
            pd <- data_source$all_player_stats[[pname]]
            if (!is.null(pd$position) && !is.na(pd$position) && pd$position==Position &&
                !is.null(pd$per_game) && nrow(pd$per_game)>0 &&
                !is.null(pd$game_log)  && nrow(pd$game_log)>=3) {
              ps <- pd$per_game
              p_score <- case_when(
                criteria=="scoring"      ~ ps$PPG[1],
                criteria=="off_down2"    ~ (ps$FG2_pct[1]/100)*ps$PPG[1],
                criteria=="off_down3"    ~ (ps$FG3_pct[1]/100)*(ps$PPG[1]*0.5),
                criteria=="ft_situation" ~ (ps$FT_pct[1]/100)*ps$PPG[1],
                criteria=="def_last"     ~ ps$RPG[1]*2+ps$SPG[1]*0.5+ps$BPG[1]*0.5,
                criteria=="need_stop"    ~ ps$SPG[1]*2+ps$BPG[1]*2+ps$RPG[1],
                TRUE ~ ps$PPG[1]
              )
              if (!is.na(p_score) && p_score > 0) data.frame(score=p_score) else NULL
            }
          })
          pos_scores <- bind_rows(pos_players) %>% filter(!is.na(score), score > 0)
          if (nrow(pos_scores) > 0)
            round(sum(pos_scores$score <= situation_score) / nrow(pos_scores) * 100, 0)
          else 50
        }
      ) %>%
      ungroup()
    
    return(final_lineup)
  }
  
  # ==========================================================================
  # COACHING TAB OUTPUTS
  # ==========================================================================
  
  output$kings_lineup <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$situation_select == "")
      return(p("Select a situation to see recommended lineup",
               style="text-align:center;color:#999;padding:20px;"))
    
    lineup <- build_lineup(rv$my_team, input$situation_select, rv$aba_data)
    
    # Apply custom swaps
    if (length(rv_custom_lineup$swaps) > 0 && !is.null(lineup) && nrow(lineup) > 0) {
      kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
      team_record  <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      gp           <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
      for (slot_str in names(rv_custom_lineup$swaps)) {
        slot_num  <- as.integer(slot_str)
        swap_name <- rv_custom_lineup$swaps[[slot_str]]
        if (slot_num <= nrow(lineup) && !is.null(swap_name)) {
          old_pos  <- lineup$lineup_position[slot_num]
          swap_row <- kings_roster %>%
            filter(Player == swap_name, !str_detect(toupper(Player), "TOTAL|TEAM")) %>% head(1)
          if (nrow(swap_row) > 0) {
            swap_row <- swap_row %>%
              mutate(
                Position = { pos <- if (Player %in% names(rv$aba_data$all_player_stats))
                  rv$aba_data$all_player_stats[[Player]]$position else "G"
                if (is.null(pos)||is.na(pos)||pos=="") "G" else pos },
                GP_individual = { if (Player %in% names(rv$aba_data$all_player_stats)) {
                  gl <- rv$aba_data$all_player_stats[[Player]]$game_log
                  if (!is.null(gl)) nrow(gl) else 0 } else 0 },
                PTS=as.numeric(PTS), REB=as.numeric(REB), AST=as.numeric(AST),
                STL=if("STL"%in%names(.)) as.numeric(STL) else 0,
                BLK=if("BLK"%in%names(.)) as.numeric(BLK) else 0,
                PPG=round(as.numeric(PTS)/gp,2), RPG=round(as.numeric(REB)/gp,2),
                APG=round(as.numeric(AST)/gp,2), SPG=0, BPG=0,
                lineup_position=old_pos, situation_score=0,
                team_rank=0, team_percentile=50, league_percentile=50,
                position_order=lineup$position_order[slot_num]
              )
            common_cols <- intersect(names(lineup), names(swap_row))
            lineup[slot_num, common_cols] <- swap_row[1, common_cols]
          }
        }
      }
    }
    
    if (!is.null(lineup) && nrow(lineup) > 0) {
      div(lapply(1:nrow(lineup), function(i) {
        player    <- lineup[i, ]
        tc_color  <- if (player$team_percentile >= 75) "#10b981"
        else if (player$team_percentile >= 50) "#f59e0b" else "#ef4444"
        lc_color  <- if (player$league_percentile >= 75) "#10b981"
        else if (player$league_percentile >= 50) "#f59e0b" else "#ef4444"
        div(
          id      = paste0("kings_player_", i),
          onclick = paste0("Shiny.setInputValue('lineup_slot_clicked',", i, ",{priority:'event'})"),
          style   = paste0("background:#f8fafc;border-left:4px solid ", ohio_kings_primary, ";",
                           "padding:15px;margin:10px 0;border-radius:5px;",
                           "box-shadow:0 2px 4px rgba(0,0,0,0.1);cursor:pointer;transition:all 0.2s;"),
          onmouseover = "this.style.background='#e0f2fe';this.style.boxShadow='0 4px 8px rgba(0,0,0,0.15)';",
          onmouseout  = "this.style.background='#f8fafc';this.style.boxShadow='0 2px 4px rgba(0,0,0,0.1)';",
          div(strong(player$Player, style=paste0("font-size:1.1em;color:",ohio_kings_secondary,";")),
              span(paste0(" (", player$lineup_position, ")"), style="color:#666;margin-left:8px;"),
              span(paste0(" \u2022 ", player$GP_individual, " GP"),
                   style="color:#999;margin-left:8px;font-size:0.9em;")),
          div(style="margin-top:8px;color:#334155;font-size:0.95em;",
              paste("PPG:", player$PPG, "| RPG:", player$RPG, "| APG:", player$APG)),
          div(style=paste0("margin-top:10px;color:", ohio_kings_primary, ";font-weight:600;"),
              paste("\u2b50 Situation Score:", round(player$situation_score, 2))),
          div(style="margin-top:8px;",
              div(style="display:flex;align-items:center;margin-bottom:5px;",
                  span("Team:", style="width:60px;font-size:0.85em;color:#64748b;"),
                  div(style="flex-grow:1;background:#e2e8f0;border-radius:10px;height:20px;overflow:hidden;position:relative;",
                      div(style=paste0("width:",player$team_percentile,"%;background:",tc_color,";height:100%;")),
                      span(paste0(player$team_percentile,"% (#",player$team_rank," on team)"),
                           style="position:absolute;right:5px;top:50%;transform:translateY(-50%);font-size:0.75em;font-weight:bold;color:#1e293b;"))),
              div(style="display:flex;align-items:center;",
                  span("League:", style="width:60px;font-size:0.85em;color:#64748b;"),
                  div(style="flex-grow:1;background:#e2e8f0;border-radius:10px;height:20px;overflow:hidden;position:relative;",
                      div(style=paste0("width:",player$league_percentile,"%;background:",lc_color,";height:100%;")),
                      span(paste0(player$league_percentile,"% (",player$Position,")"),
                           style="position:absolute;right:5px;top:50%;transform:translateY(-50%);font-size:0.75em;font-weight:bold;color:#1e293b;"))))
        )
      }))
    } else {
      p("No lineup data available", style="text-align:center;padding:20px;")
    }
  })
  
  output$opponent_lineup <- renderUI({
    req(rv$logged_in, input$situation_select, input$opponent_select)
    if (input$opponent_select == "")
      return(p("Select an opponent to see their expected lineup",
               style="text-align:center;color:#999;padding:20px;"))
    opp_criteria <- switch(input$situation_select,
                           "off_down3"="need_stop","off_down2"="need_stop","need_stop"="scoring",
                           "def_last"="scoring","scoring"="need_stop","ft_situation"="ft_situation",
                           input$situation_select)
    lineup <- build_lineup(input$opponent_select, opp_criteria, rv$aba_data)
    if (!is.null(lineup) && nrow(lineup) > 0) {
      div(lapply(1:nrow(lineup), function(i) {
        player <- lineup[i, ]
        tc_c   <- if (player$team_percentile>=75)"#10b981" else if(player$team_percentile>=50)"#f59e0b" else "#ef4444"
        lc_c   <- if (player$league_percentile>=75)"#10b981" else if(player$league_percentile>=50)"#f59e0b" else "#ef4444"
        div(style="background:#fef2f2;border-left:4px solid #ef4444;padding:15px;margin:10px 0;border-radius:5px;box-shadow:0 2px 4px rgba(0,0,0,0.1);",
            div(strong(player$Player, style="font-size:1.1em;color:#991b1b;"),
                span(paste0(" (",player$lineup_position,")"),style="color:#666;margin-left:8px;"),
                span(paste0(" \u2022 ",player$GP_individual," GP"),style="color:#999;margin-left:8px;font-size:0.9em;")),
            div(style="margin-top:8px;color:#334155;font-size:0.95em;",
                paste("PPG:",player$PPG,"| RPG:",player$RPG,"| APG:",player$APG)),
            div(style="margin-top:10px;color:#ef4444;font-weight:600;",
                paste("\u2b50 Situation Score:",round(player$situation_score,2))),
            div(style="margin-top:8px;",
                div(style="display:flex;align-items:center;margin-bottom:5px;",
                    span("Team:",style="width:60px;font-size:0.85em;color:#64748b;"),
                    div(style="flex-grow:1;background:#e2e8f0;border-radius:10px;height:20px;overflow:hidden;position:relative;",
                        div(style=paste0("width:",player$team_percentile,"%;background:",tc_c,";height:100%;")),
                        span(paste0(player$team_percentile,"% (#",player$team_rank," on team)"),
                             style="position:absolute;right:5px;top:50%;transform:translateY(-50%);font-size:0.75em;font-weight:bold;color:#1e293b;"))),
                div(style="display:flex;align-items:center;",
                    span("League:",style="width:60px;font-size:0.85em;color:#64748b;"),
                    div(style="flex-grow:1;background:#e2e8f0;border-radius:10px;height:20px;overflow:hidden;position:relative;",
                        div(style=paste0("width:",player$league_percentile,"%;background:",lc_c,";height:100%;")),
                        span(paste0(player$league_percentile,"% (",player$Position,")"),
                             style="position:absolute;right:5px;top:50%;transform:translateY(-50%);font-size:0.75em;font-weight:bold;color:#1e293b;"))))
        )
      }))
    } else {
      p("No lineup data available for opponent", style="text-align:center;padding:20px;")
    }
  })
  
  output$court_visualization <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$situation_select == "")
      return(p("Select a situation to see court visualization",
               style="text-align:center;color:#999;padding:40px;"))
    
    kings_lineup <- build_lineup(rv$my_team, input$situation_select, rv$aba_data)
    opp_lineup   <- NULL
    has_opp      <- !is.null(input$opponent_select) && input$opponent_select != ""
    if (has_opp) {
      opp_criteria <- switch(input$situation_select,
                             "off_down3"="need_stop","off_down2"="need_stop","need_stop"="scoring",
                             "def_last"="scoring","scoring"="need_stop","ft_situation"="ft_situation",
                             input$situation_select)
      opp_lineup <- build_lineup(input$opponent_select, opp_criteria, rv$aba_data)
    }
    
    if (is.null(kings_lineup) || nrow(kings_lineup) == 0)
      return(p("No lineup data available",style="text-align:center;color:#999;padding:40px;"))
    
    # Apply custom swaps to court view
    if (length(rv_custom_lineup$swaps) > 0) {
      kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
      team_record  <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      gp           <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
      for (slot_str in names(rv_custom_lineup$swaps)) {
        slot_num  <- as.integer(slot_str)
        swap_name <- rv_custom_lineup$swaps[[slot_str]]
        if (slot_num <= nrow(kings_lineup) && !is.null(swap_name)) {
          old_pos  <- kings_lineup$lineup_position[slot_num]
          swap_row <- kings_roster %>%
            filter(Player==swap_name, !str_detect(toupper(Player),"TOTAL|TEAM")) %>% head(1)
          if (nrow(swap_row) > 0) {
            real_ppg <- 0; real_rpg <- 0; real_apg <- 0; real_spg <- 0; real_bpg <- 0
            if (swap_name %in% names(rv$aba_data$all_player_stats)) {
              pg <- rv$aba_data$all_player_stats[[swap_name]]$per_game
              if (!is.null(pg) && nrow(pg) > 0) {
                real_ppg <- if(!is.na(pg$PPG[1])) pg$PPG[1] else 0
                real_rpg <- if(!is.na(pg$RPG[1])) pg$RPG[1] else 0
                real_apg <- if(!is.na(pg$APG[1])) pg$APG[1] else 0
                real_spg <- if(!is.na(pg$SPG[1])) pg$SPG[1] else 0
                real_bpg <- if(!is.na(pg$BPG[1])) pg$BPG[1] else 0
              }
            }
            if (real_ppg == 0) real_ppg <- round(as.numeric(swap_row$PTS)/gp,2)
            if (real_rpg == 0) real_rpg <- round(as.numeric(swap_row$REB)/gp,2)
            if (real_apg == 0) real_apg <- round(as.numeric(swap_row$AST)/gp,2)
            kings_lineup$Player[slot_num]           <- swap_name
            kings_lineup$PPG[slot_num]              <- real_ppg
            kings_lineup$RPG[slot_num]              <- real_rpg
            kings_lineup$APG[slot_num]              <- real_apg
            kings_lineup$SPG[slot_num]              <- real_spg
            kings_lineup$BPG[slot_num]              <- real_bpg
            kings_lineup$lineup_position[slot_num]  <- old_pos
            kings_lineup$team_percentile[slot_num]  <- 50
            kings_lineup$league_percentile[slot_num] <- 50
          }
        }
      }
    }
    
    pos_kings <- list("PG"=list(x=22,y=78),"SG"=list(x=8,y=46),"SF"=list(x=28,y=14),
                      "PF"=list(x=38,y=58),"C"=list(x=34,y=36))
    pos_opp   <- list("PG"=list(x=78,y=78),"SG"=list(x=92,y=46),"SF"=list(x=72,y=14),
                      "PF"=list(x=62,y=58),"C"=list(x=66,y=36))
    
    shorten_name <- function(name) {
      parts <- strsplit(name," ")[[1]]
      if (length(parts)>=2) paste0(substr(parts[1],1,1),". ",paste(parts[-1],collapse=" ")) else name
    }
    pct_dot_color <- function(pct) {
      if (pct>=75)"#22c55e" else if(pct>=50)"#a3e635" else if(pct>=25)"#fbbf24" else "#f87171"
    }
    
    make_court_card <- function(player, coords, is_kings=TRUE, slot_index=NULL) {
      accent_c  <- if (is_kings) ohio_kings_primary else "#991b1b"
      card_bg   <- if (is_kings) "rgba(255,255,255,0.92)" else "rgba(255,240,240,0.92)"
      border_c  <- if (is_kings) ohio_kings_primary else "#dc2626"
      is_sel    <- is_kings && !is.null(rv_custom_lineup$selected_slot) &&
        !is.null(slot_index) && rv_custom_lineup$selected_slot == slot_index
      onclick_a <- if (is_kings && !is.null(slot_index))
        paste0("Shiny.setInputValue('lineup_slot_clicked',",slot_index,",{priority:'event'})") else ""
      
      # Get streak for this player
      p_streak <- 0
      if (player$Player %in% names(rv$aba_data$all_player_stats)) {
        p_data <- rv$aba_data$all_player_stats[[player$Player]]
        if (!is.null(p_data$game_log) && nrow(p_data$game_log)>=3 && !is.null(p_data$per_game)) {
          recent     <- p_data$game_log %>% arrange(desc(date)) %>% head(3)
          recent_avg <- mean(suppressWarnings(as.numeric(recent$PTS)),na.rm=TRUE)
          szn_avg    <- p_data$per_game$PPG[1]
          if (!is.na(recent_avg)&&!is.na(szn_avg)&&szn_avg>0)
            p_streak <- round(((recent_avg-szn_avg)/szn_avg)*100,1)
        }
      }
      
      div(style=paste0("position:absolute;left:",coords$x,"%;top:",coords$y,
                       "%;transform:translate(-50%,-50%);z-index:10;width:130px;"),
          div(style=paste0("background:",card_bg,";border:",
                           if(is_sel)"3px" else "2px"," solid ",
                           if(is_sel)"#f59e0b" else border_c,
                           ";border-radius:12px;padding:8px 6px;text-align:center;",
                           "box-shadow:0 4px 15px rgba(0,0,0,0.25);cursor:",
                           if(is_kings)"pointer" else "default",";"),
              onclick=onclick_a,
              div(style=paste0("display:inline-block;background:",accent_c,
                               ";color:white;font-weight:800;font-size:0.8em;",
                               "padding:2px 10px;border-radius:8px;margin-bottom:4px;"),
                  player$lineup_position),
              div(style="font-weight:700;font-size:0.75em;color:#1e293b;margin:3px 0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                  shorten_name(player$Player)),
              div(style="display:flex;justify-content:center;gap:4px;margin:4px 0;",
                  span(paste0(player$PPG),
                       style=paste0("font-size:0.65em;font-weight:700;color:",ohio_kings_primary,
                                    ";background:#fee2e2;padding:1px 4px;border-radius:4px;")),
                  span(paste0(player$RPG),
                       style="font-size:0.65em;font-weight:700;color:#1e40af;background:#dbeafe;padding:1px 4px;border-radius:4px;"),
                  span(paste0(player$APG),
                       style="font-size:0.65em;font-weight:700;color:#b45309;background:#fef3c7;padding:1px 4px;border-radius:4px;")),
              div(style="display:flex;justify-content:center;gap:6px;",
                  span("PPG",style="font-size:0.5em;color:#94a3b8;font-weight:600;"),
                  span("RPG",style="font-size:0.5em;color:#94a3b8;font-weight:600;"),
                  span("APG",style="font-size:0.5em;color:#94a3b8;font-weight:600;")),
              div(style="margin-top:4px;display:flex;justify-content:center;gap:6px;align-items:center;",
                  span(paste0("\u2b50 ",round(player$situation_score,1)),
                       style="font-size:0.6em;color:#475569;font-weight:600;"),
                  if (p_streak > 15)
                    span("\U0001f525",style="font-size:0.7em;",title=paste0("+",p_streak,"% vs avg"))
                  else if (p_streak < -15)
                    span("\u2744\ufe0f",style="font-size:0.7em;",title=paste0(p_streak,"% vs avg"))
              ),
              div(style="margin-top:4px;",
                  div(style="display:flex;align-items:center;gap:2px;margin-bottom:2px;",
                      span("T",style="font-size:0.5em;color:#94a3b8;font-weight:700;width:10px;"),
                      div(style="flex:1;height:6px;background:#e2e8f0;border-radius:3px;overflow:hidden;",
                          div(style=paste0("width:",player$team_percentile,"%;height:100%;background:",
                                           pct_dot_color(player$team_percentile),";border-radius:3px;"))),
                      span(paste0(player$team_percentile),
                           style="font-size:0.45em;color:#64748b;font-weight:700;width:16px;text-align:right;")),
                  div(style="display:flex;align-items:center;gap:2px;",
                      span("L",style="font-size:0.5em;color:#94a3b8;font-weight:700;width:10px;"),
                      div(style="flex:1;height:6px;background:#e2e8f0;border-radius:3px;overflow:hidden;",
                          div(style=paste0("width:",player$league_percentile,"%;height:100%;background:",
                                           pct_dot_color(player$league_percentile),";border-radius:3px;"))),
                      span(paste0(player$league_percentile),
                           style="font-size:0.45em;color:#64748b;font-weight:700;width:16px;text-align:right;"))
              ),
              if (is_sel) div(style=paste0("margin-top:4px;font-size:0.55em;font-weight:700;color:#f59e0b;"),
                              "\u2193 Select from bench")
          )
      )
    }
    
    # Build bench
    kings_roster_all <- rv$aba_data$all_team_stats[[rv$my_team]]
    bench_players    <- data.frame()
    if (!is.null(kings_roster_all)) {
      team_record  <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      gp_team      <- if (nrow(team_record)>0) team_record$W[1]+team_record$L[1] else 15
      bench_players <- kings_roster_all %>%
        filter(!str_detect(toupper(Player),"TOTAL|TEAM")) %>%
        filter(!Player %in% kings_lineup$Player) %>%
        mutate(
          GP_individual = sapply(Player, function(pl) {
            if (pl %in% names(rv$aba_data$all_player_stats)) {
              gl <- rv$aba_data$all_player_stats[[pl]]$game_log
              if (!is.null(gl)) return(nrow(gl))
            }; return(0)
          }),
          Position = sapply(Player, function(pl) {
            if (pl %in% names(rv$aba_data$all_player_stats)) {
              pos <- rv$aba_data$all_player_stats[[pl]]$position
              if (!is.null(pos)&&!is.na(pos)&&pos!="") return(pos)
            }; return("G")
          }),
          PPG = sapply(Player, function(pl) {
            if (pl %in% names(rv$aba_data$all_player_stats)) {
              pg <- rv$aba_data$all_player_stats[[pl]]$per_game
              if (!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) return(pg$PPG[1])
            }; return(round(as.numeric(PTS)/gp_team,2))
          }),
          RPG = sapply(Player, function(pl) {
            if (pl %in% names(rv$aba_data$all_player_stats)) {
              pg <- rv$aba_data$all_player_stats[[pl]]$per_game
              if (!is.null(pg)&&nrow(pg)>0&&!is.na(pg$RPG[1])) return(pg$RPG[1])
            }; return(round(as.numeric(REB)/gp_team,2))
          }),
          APG = sapply(Player, function(pl) {
            if (pl %in% names(rv$aba_data$all_player_stats)) {
              pg <- rv$aba_data$all_player_stats[[pl]]$per_game
              if (!is.null(pg)&&nrow(pg)>0&&!is.na(pg$APG[1])) return(pg$APG[1])
            }; return(round(as.numeric(AST)/gp_team,2))
          })
        ) %>%
        filter(GP_individual >= 3) %>%
        mutate(
          situation_score = case_when(
            input$situation_select=="scoring"      ~ PPG,
            input$situation_select=="off_down2"    ~ (as.numeric(`2PM`)/pmax(as.numeric(`2PA`),1))*PPG,
            input$situation_select=="off_down3"    ~ (as.numeric(`3PM`)/pmax(as.numeric(`3PA`),1))*PPG,
            input$situation_select=="ft_situation" ~ (as.numeric(FTM)/pmax(as.numeric(FTA),1))*PPG,
            input$situation_select=="def_last"     ~ RPG*2,
            input$situation_select=="need_stop"    ~ RPG,
            TRUE ~ PPG
          )
        ) %>%
        arrange(desc(situation_score)) %>%
        head(8)
    }
    
    is_slot_selected <- !is.null(rv_custom_lineup$selected_slot)
    
    div(
      # Court
      div(style=paste0("position:relative;width:100%;padding-bottom:52%;",
                       "background:linear-gradient(180deg,#1a5632 0%,#2d6a4f 50%,#1a5632 100%);",
                       "border-radius:14px;overflow:hidden;",
                       "box-shadow:0 8px 32px rgba(0,0,0,0.2),inset 0 0 60px rgba(0,0,0,0.15);"),
          HTML('<svg viewBox="0 0 1000 520" style="position:absolute;top:0;left:0;width:100%;height:100%;" preserveAspectRatio="xMidYMid meet">
            <rect x="15" y="15" width="970" height="490" fill="none" stroke="rgba(255,255,255,0.35)" stroke-width="2.5" rx="4"/>
            <line x1="500" y1="15" x2="500" y2="505" stroke="rgba(255,255,255,0.35)" stroke-width="2.5"/>
            <circle cx="500" cy="260" r="55" fill="none" stroke="rgba(255,255,255,0.35)" stroke-width="2.5"/>
            <circle cx="500" cy="260" r="4" fill="rgba(255,255,255,0.35)"/>
            <rect x="15" y="155" width="175" height="210" fill="rgba(200,16,46,0.08)" stroke="rgba(255,255,255,0.25)" stroke-width="2"/>
            <circle cx="190" cy="260" r="55" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5" stroke-dasharray="6,6"/>
            <path d="M 15 120 Q 260 260 15 400" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5"/>
            <circle cx="50" cy="260" r="10" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
            <rect x="15" y="250" width="25" height="20" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
            <rect x="810" y="155" width="175" height="210" fill="rgba(153,27,27,0.08)" stroke="rgba(255,255,255,0.25)" stroke-width="2"/>
            <circle cx="810" cy="260" r="55" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5" stroke-dasharray="6,6"/>
            <path d="M 985 120 Q 740 260 985 400" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5"/>
            <circle cx="950" cy="260" r="10" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
            <rect x="960" y="250" width="25" height="20" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
          </svg>'),
          div(style=paste0("position:absolute;top:10px;left:20px;color:rgba(255,255,255,0.6);",
                           "font-size:1.4em;font-weight:800;letter-spacing:2px;"),
              toupper(rv$my_team)),
          if (has_opp)
            div(style="position:absolute;top:10px;right:20px;color:rgba(255,255,255,0.4);font-size:1.4em;font-weight:800;letter-spacing:2px;text-align:right;",
                toupper(input$opponent_select)),
          lapply(1:nrow(kings_lineup), function(i) {
            player <- kings_lineup[i,]
            coords <- pos_kings[[player$lineup_position]]
            if (is.null(coords)) coords <- list(x=25,y=50)
            make_court_card(player, coords, is_kings=TRUE, slot_index=i)
          }),
          if (!is.null(opp_lineup) && nrow(opp_lineup)>0) {
            lapply(1:nrow(opp_lineup), function(i) {
              player <- opp_lineup[i,]
              coords <- pos_opp[[player$lineup_position]]
              if (is.null(coords)) coords <- list(x=75,y=50)
              make_court_card(player, coords, is_kings=FALSE)
            })
          }
      ),
      
      # Legend
      div(style="display:flex;justify-content:center;gap:20px;margin-top:10px;padding:8px;",
          span("T = Team Rank",style="font-size:0.75em;color:#64748b;font-weight:600;"),
          span("L = League Rank",style="font-size:0.75em;color:#64748b;font-weight:600;"),
          span("\u2b50 = Situation Score",style="font-size:0.75em;color:#64748b;font-weight:600;"),
          div(style="display:flex;align-items:center;gap:4px;",
              div(style="width:10px;height:10px;border-radius:50%;background:#22c55e;"),
              span("75+",style="font-size:0.65em;color:#64748b;"),
              div(style="width:10px;height:10px;border-radius:50%;background:#a3e635;"),
              span("50+",style="font-size:0.65em;color:#64748b;"),
              div(style="width:10px;height:10px;border-radius:50%;background:#fbbf24;"),
              span("25+",style="font-size:0.65em;color:#64748b;"),
              div(style="width:10px;height:10px;border-radius:50%;background:#f87171;"),
              span("<25",style="font-size:0.65em;color:#64748b;"))
      ),
      
      # Bench
      div(style="margin-top:15px;background:linear-gradient(180deg,#f8fafc,#f1f5f9);border-radius:12px;padding:16px;border:1px solid #e2e8f0;",
          div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
              h4(paste0("\U0001f4ba ",rv$my_team," Bench"),
                 style="margin:0;color:#334155;font-weight:700;"),
              if (is_slot_selected) {
                span(paste0("\u2b05 Swapping slot ",rv_custom_lineup$selected_slot,
                            " \u2014 click a bench player"),
                     style=paste0("font-size:0.85em;font-weight:700;color:",ohio_kings_primary,";"))
              } else {
                span("Click a court player first, then click a bench player to swap",
                     style="font-size:0.8em;color:#94a3b8;font-style:italic;")
              }
          ),
          if (nrow(bench_players) > 0) {
            div(style="display:flex;gap:10px;overflow-x:auto;padding-bottom:8px;",
                lapply(1:nrow(bench_players), function(j) {
                  bp <- bench_players[j,]
                  div(
                    style=paste0("min-width:130px;background:white;border:2px solid ",
                                 if(is_slot_selected) ohio_kings_primary else "#e2e8f0",
                                 ";border-radius:10px;padding:10px 8px;text-align:center;cursor:",
                                 if(is_slot_selected)"pointer" else "default",
                                 ";transition:all 0.2s;flex-shrink:0;",
                                 if(is_slot_selected) paste0(" box-shadow:0 0 0 2px rgba(200,16,46,0.2);") else ""),
                    onmouseover=if(is_slot_selected) paste0("this.style.borderColor='",ohio_kings_primary,"';this.style.transform='translateY(-2px)';") else "",
                    onmouseout =if(is_slot_selected) paste0("this.style.borderColor='",ohio_kings_primary,"';this.style.transform='translateY(0)';") else "",
                    onclick    =if(is_slot_selected)
                      paste0("Shiny.setInputValue('swap_player',{slot:",rv_custom_lineup$selected_slot,
                             ",player:'",gsub("'","\\\\'",bp$Player),"'},{priority:'event'})") else "",
                    span(bp$Position,
                         style=paste0("display:inline-block;background:",ohio_kings_secondary,
                                      ";color:white;font-size:0.65em;font-weight:700;padding:1px 6px;border-radius:6px;")),
                    div(style="font-weight:700;font-size:0.75em;color:#1e293b;margin:5px 0 3px 0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                        shorten_name(bp$Player)),
                    div(style="display:flex;justify-content:center;gap:3px;margin:3px 0;",
                        span(paste0(bp$PPG),
                             style=paste0("font-size:0.6em;font-weight:700;color:",ohio_kings_primary,";background:#fee2e2;padding:1px 3px;border-radius:3px;")),
                        span(paste0(bp$RPG),
                             style="font-size:0.6em;font-weight:700;color:#1e40af;background:#dbeafe;padding:1px 3px;border-radius:3px;"),
                        span(paste0(bp$APG),
                             style="font-size:0.6em;font-weight:700;color:#b45309;background:#fef3c7;padding:1px 3px;border-radius:3px;")),
                    div(style="font-size:0.55em;color:#94a3b8;margin-top:2px;",
                        paste0(bp$GP_individual," GP")),
                    if (is_slot_selected)
                      div(style=paste0("margin-top:5px;font-size:0.65em;font-weight:700;color:",ohio_kings_primary,";"),
                          "\u21bb Tap to swap")
                  )
                })
            )
          } else {
            p("No bench players available",style="color:#94a3b8;text-align:center;")
          }
      )
    )
  })
  
  output$four_factors <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
    if (is.null(kings_roster) || nrow(kings_roster) == 0)
      return(p("No roster data available",style="text-align:center;color:#999;padding:20px;"))
    
    get_stat <- function(roster, col) {
      total_row <- roster %>% filter(str_detect(toupper(Player),"TOTAL|TEAM"))
      if (nrow(total_row)>0 && col %in% names(total_row)) return(as.numeric(total_row[[col]][1]))
      player_rows <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
      if (col %in% names(player_rows)) return(sum(as.numeric(player_rows[[col]]),na.rm=TRUE))
      return(0)
    }
    
    total_2pm  <- get_stat(kings_roster,"2PM"); total_2pa <- get_stat(kings_roster,"2PA")
    total_3pm  <- get_stat(kings_roster,"3PM"); total_3pa <- get_stat(kings_roster,"3PA")
    total_fgm  <- total_2pm + total_3pm;         total_fga <- total_2pa + total_3pa
    total_fta  <- get_stat(kings_roster,"FTA");   total_ftm <- get_stat(kings_roster,"FTM")
    total_oreb <- get_stat(kings_roster,"OREB");  total_dreb <- get_stat(kings_roster,"DREB")
    
    efg      <- if (total_fga>0) round(min((total_fgm+0.5*total_3pm)/total_fga*100,100),1) else 0
    tov_pct  <- 12  # estimated without full TO data
    oreb_pct <- if ((total_oreb+total_dreb)>0) round(total_oreb/(total_oreb+total_dreb)*100,1) else 0
    ft_rate  <- if (total_fga>0) round(total_fta/total_fga,2) else 0
    
    efg_grade  <- if(efg>=55)"A" else if(efg>=50)"B" else if(efg>=45)"C" else "D"
    tov_grade  <- if(tov_pct<=12)"A" else if(tov_pct<=15)"B" else if(tov_pct<=18)"C" else "D"
    oreb_grade <- if(oreb_pct>=35)"A" else if(oreb_pct>=30)"B" else if(oreb_pct>=25)"C" else "D"
    ftr_grade  <- if(ft_rate>=0.35)"A" else if(ft_rate>=0.25)"B" else if(ft_rate>=0.18)"C" else "D"
    
    grade_color <- function(grade) switch(grade,"A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c")
    grade_bg    <- function(grade) switch(grade,
                                          "A"="linear-gradient(135deg,#15803d,#22c55e)",
                                          "B"="linear-gradient(135deg,#65a30d,#a3e635)",
                                          "C"="linear-gradient(135deg,#d97706,#fbbf24)",
                                          "D"="linear-gradient(135deg,#b91c1c,#f87171)")
    
    make_gauge <- function(value, label, grade, description, fill_pct) {
      fill_pct <- max(0, min(100, fill_pct))
      cx <- 60; cy <- 60; r <- 45
      to_rad <- function(deg) deg * pi / 180
      start_angle <- 135; total_sweep <- 270
      fill_angle  <- start_angle + (fill_pct/100) * total_sweep
      bg_x1 <- cx + r * cos(to_rad(start_angle)); bg_y1 <- cy + r * sin(to_rad(start_angle))
      bg_x2 <- cx + r * cos(to_rad(start_angle+total_sweep)); bg_y2 <- cy + r*sin(to_rad(start_angle+total_sweep))
      fill_x2 <- cx + r * cos(to_rad(fill_angle)); fill_y2 <- cy + r * sin(to_rad(fill_angle))
      bg_large   <- 1
      fill_large <- if ((fill_pct/100)*total_sweep > 180) 1 else 0
      
      div(style=paste0("background:linear-gradient(180deg,#ffffff,#fafafa);border-radius:16px;",
                       "padding:20px;text-align:center;",
                       "box-shadow:0 4px 16px rgba(0,0,0,0.06),0 1px 3px rgba(0,0,0,0.04);",
                       "border:2px solid ",grade_color(grade),"25;position:relative;",
                       "transition:transform 0.2s,box-shadow 0.2s;"),
          onmouseover="this.style.transform='translateY(-2px)';this.style.boxShadow='0 8px 24px rgba(0,0,0,0.1)';",
          onmouseout ="this.style.transform='translateY(0)';this.style.boxShadow='0 4px 16px rgba(0,0,0,0.06)';",
          div(style=paste0("position:absolute;top:10px;right:10px;width:36px;height:36px;",
                           "background:",grade_bg(grade),";border-radius:50%;display:flex;",
                           "align-items:center;justify-content:center;font-weight:800;font-size:0.95em;",
                           "box-shadow:0 2px 8px rgba(0,0,0,0.15);color:",
                           if(grade %in% c("B","C")) "#1a1a2e" else "white",";"),
              grade),
          HTML(paste0('<svg width="120" height="100" viewBox="0 0 120 100" style="margin:0 auto;display:block;">
            <path d="M ',round(bg_x1,2),' ',round(bg_y1,2),' A ',r,' ',r,' 0 ',bg_large,' 1 ',round(bg_x2,2),' ',round(bg_y2,2),'"
                  fill="none" stroke="#e2e8f0" stroke-width="10" stroke-linecap="round"/>',
                      if (fill_pct > 1) paste0(
                        '<path d="M ',round(bg_x1,2),' ',round(bg_y1,2),' A ',r,' ',r,' 0 ',fill_large,' 1 ',round(fill_x2,2),' ',round(fill_y2,2),'"
                    fill="none" stroke="',grade_color(grade),'" stroke-width="10" stroke-linecap="round"/>') else '',
                      '</svg>')),
          h3(value,       style=paste0("margin:5px 0 0 0;font-size:1.8em;font-weight:800;color:",grade_color(grade),";")),
          p(label,        style="margin:5px 0 0 0;color:#475569;font-weight:600;font-size:0.95em;"),
          p(description,  style="margin:4px 0 0 0;color:#94a3b8;font-size:0.78em;"),
          p(paste0(round(fill_pct,0),"th %ile league-wide"),
            style=paste0("margin:6px 0 0 0;font-size:0.72em;font-weight:600;color:",grade_color(grade),";opacity:0.8;"))
      )
    }
    
    efg_fill  <- min(100,max(0,(efg-35)/(65-35)*100))
    tov_fill  <- min(100,max(0,(25-tov_pct)/(25-5)*100))
    oreb_fill <- min(100,max(0,(oreb_pct-15)/(45-15)*100))
    ftr_fill  <- min(100,max(0,(ft_rate-0.10)/(0.45-0.10)*100))
    
    fluidRow(
      column(3, make_gauge(paste0(efg,"%"),    "eFG%",     "Shooting efficiency", efg_grade,  efg_fill)),
      column(3, make_gauge(paste0(tov_pct,"%"),"TOV%",     "Lower is better",     tov_grade,  tov_fill)),
      column(3, make_gauge(paste0(oreb_pct,"%"),"OREB%",   "Offensive boards",    oreb_grade, oreb_fill)),
      column(3, make_gauge(ft_rate,             "FT Rate",  "Getting to the line", ftr_grade,  ftr_fill))
    )
  })
  
  output$situation_success <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$situation_select == "")
      return(p("Select a situation to see success prediction",
               style="text-align:center;color:#999;padding:20px;"))
    
    kings_lineup <- build_lineup(rv$my_team, input$situation_select, rv$aba_data)
    if (is.null(kings_lineup)||nrow(kings_lineup)==0)
      return(p("No lineup data available",style="text-align:center;color:#999;padding:20px;"))
    
    # Apply swaps
    if (length(rv_custom_lineup$swaps)>0) {
      for (slot_str in names(rv_custom_lineup$swaps)) {
        slot_num  <- as.integer(slot_str)
        swap_name <- rv_custom_lineup$swaps[[slot_str]]
        if (slot_num<=nrow(kings_lineup) && !is.null(swap_name) &&
            swap_name %in% names(rv$aba_data$all_player_stats)) {
          pg <- rv$aba_data$all_player_stats[[swap_name]]$per_game
          if (!is.null(pg) && nrow(pg)>0) {
            kings_lineup$Player[slot_num]  <- swap_name
            kings_lineup$PPG[slot_num]     <- pg$PPG[1]
            kings_lineup$RPG[slot_num]     <- pg$RPG[1]
            kings_lineup$APG[slot_num]     <- pg$APG[1]
            kings_lineup$SPG[slot_num]     <- pg$SPG[1]
            kings_lineup$BPG[slot_num]     <- pg$BPG[1]
            if (!is.na(pg$FG2_pct[1])) kings_lineup$FG2_pct[slot_num] <- pg$FG2_pct[1]
            if (!is.na(pg$FG3_pct[1])) kings_lineup$FG3_pct[slot_num] <- pg$FG3_pct[1]
            if (!is.na(pg$FT_pct[1]))  kings_lineup$FT_pct[slot_num]  <- pg$FT_pct[1]
          }
        }
      }
    }
    
    opp_lineup_ss <- NULL
    has_opp_ss    <- !is.null(input$opponent_select) && input$opponent_select != ""
    if (has_opp_ss) {
      opp_crit <- switch(input$situation_select,
                         "off_down3"="need_stop","off_down2"="need_stop","need_stop"="scoring",
                         "def_last"="scoring","scoring"="need_stop","ft_situation"="ft_situation",
                         input$situation_select)
      opp_lineup_ss <- tryCatch(build_lineup(input$opponent_select,opp_crit,rv$aba_data),
                                error=function(e) NULL)
    }
    
    # Compute shot weights and key stats
    ppgs         <- kings_lineup$PPG;   ppgs[is.na(ppgs)]  <- 0
    total_ppg_l  <- sum(ppgs)
    shot_weights <- if (total_ppg_l>0) ppgs/total_ppg_l else rep(0.2,length(ppgs))
    primary_idx  <- which.max(shot_weights)
    primary_pl   <- kings_lineup$Player[primary_idx]
    primary_wt   <- round(shot_weights[primary_idx]*100,0)
    fg2s <- kings_lineup$FG2_pct; fg2s[is.na(fg2s)] <- 0
    fg3s <- kings_lineup$FG3_pct; fg3s[is.na(fg3s)] <- 0
    fts  <- kings_lineup$FT_pct;  fts[is.na(fts)]   <- 0
    spgs <- kings_lineup$SPG;     spgs[is.na(spgs)]  <- 0
    bpgs <- kings_lineup$BPG;     bpgs[is.na(bpgs)]  <- 0
    rpgs <- kings_lineup$RPG;     rpgs[is.na(rpgs)]  <- 0
    
    # Opponent defensive impact
    opp_def_factor <- 1.0
    if (!is.null(opp_lineup_ss) && nrow(opp_lineup_ss)>0) {
      opp_spg        <- sum(opp_lineup_ss$SPG,na.rm=TRUE)
      opp_bpg        <- sum(opp_lineup_ss$BPG,na.rm=TRUE)
      opp_def_plays  <- opp_spg + opp_bpg
      opp_def_factor <- max(0.85, 1-(opp_def_plays-3)*0.025)
    }
    
    criteria <- input$situation_select
    situation_data <- switch(criteria,
                             "off_down3" = {
                               three_attempts <- sapply(kings_lineup$Player, function(pl) {
                                 if (pl %in% names(rv$aba_data$all_player_stats)) {
                                   gl <- rv$aba_data$all_player_stats[[pl]]$game_log
                                   if (!is.null(gl)&&"3PA"%in%names(gl))
                                     return(sum(suppressWarnings(as.numeric(gl$`3PA`)),na.rm=TRUE))
                                 }; return(0)
                               })
                               min_3pa <- 15; league_avg_3 <- 32
                               adj_3pcts <- ifelse(
                                 three_attempts >= min_3pa, fg3s,
                                 ifelse(three_attempts==0, 0,
                                        (fg3s*three_attempts+league_avg_3*min_3pa)/(three_attempts+min_3pa)))
                               raw_pct <- sum(shot_weights*adj_3pcts)
                               single_poss_pct <- round(raw_pct*opp_def_factor,1)
                               reliable_idx <- which(three_attempts>=min_3pa & fg3s>0)
                               best_3_idx <- if(length(reliable_idx)>0) reliable_idx[which.max(fg3s[reliable_idx])] else which.max(three_attempts)
                               best_3_pl  <- kings_lineup$Player[best_3_idx]; best_3_pct <- fg3s[best_3_idx]
                               optimal_pct <- round(best_3_pct,1); improvement <- round(optimal_pct-single_poss_pct,1)
                               list(title="Down 3 \u2014 One Possession to Hit a Three",
                                    primary_stat=paste0(single_poss_pct,"%"),
                                    primary_label="Probability of hitting a 3 (weighted by shot distribution)",
                                    secondary_stat=paste0(optimal_pct,"%"),
                                    secondary_label=paste0("If drawn for ",best_3_pl," (",round(best_3_pct,1),"% 3P%)"),
                                    detail=paste0("\U0001f3af Primary option: ",primary_pl," (~",primary_wt,"% shot share)"),
                                    detail2=if(improvement>2) paste0("\u2b06\ufe0f +",improvement,"% boost if you go to ",best_3_pl) else NULL,
                                    vs_league=paste0("League avg 3P%: ~32%"), vs_diff=single_poss_pct-32,
                                    gauge_pct=min(100,max(0,(single_poss_pct-15)/(50-15)*100)))
                             },
                             "off_down2" = {
                               raw_pct <- sum(shot_weights*fg2s)
                               single_poss_pct <- round(raw_pct*opp_def_factor,1)
                               best_2_idx <- which.max(fg2s); best_2_pl <- kings_lineup$Player[best_2_idx]
                               optimal_pct <- round(fg2s[best_2_idx],1); improvement <- round(optimal_pct-single_poss_pct,1)
                               list(title="Down 2 \u2014 One Possession to Tie or Take the Lead",
                                    primary_stat=paste0(single_poss_pct,"%"),
                                    primary_label="Probability of hitting a 2 (weighted by shot distribution)",
                                    secondary_stat=paste0(optimal_pct,"%"),
                                    secondary_label=paste0("If play drawn for ",best_2_pl," (best 2P shooter)"),
                                    detail=paste0("\U0001f3af Primary option: ",primary_pl,
                                                  " (",round(fg2s[primary_idx],1),"% from 2, ~",primary_wt,"% share)"),
                                    detail2=if(improvement>2) paste0("\u2b06\ufe0f +",improvement,"% boost to ",best_2_pl) else NULL,
                                    vs_league="League avg 2P%: ~47%", vs_diff=single_poss_pct-47,
                                    gauge_pct=min(100,max(0,(single_poss_pct-30)/(65-30)*100)))
                             },
                             "ft_situation" = {
                               fouled_idx  <- primary_idx; fouled_pl <- kings_lineup$Player[fouled_idx]
                               fouled_ft   <- fts[fouled_idx]
                               both_fts    <- round((fouled_ft/100)^2*100,1)
                               at_least_one <- round((1-((1-fouled_ft/100)^2))*100,1)
                               best_ft_idx <- which.max(fts); best_ft_pl <- kings_lineup$Player[best_ft_idx]
                               best_ft_pct <- fts[best_ft_idx]; best_both <- round((best_ft_pct/100)^2*100,1)
                               worst_nonzero <- which(fts > 0); worst_ft_idx <- if(length(worst_nonzero)>0) worst_nonzero[which.min(fts[worst_nonzero])] else 1
                               worst_ft_pl <- kings_lineup$Player[worst_ft_idx]; worst_ft_pct <- fts[worst_ft_idx]
                               list(title="Free Throw Situation \u2014 Most Likely Player Fouled",
                                    primary_stat=paste0(both_fts,"%"),
                                    primary_label=paste0("Prob ",fouled_pl," makes BOTH FTs (",fouled_ft,"% shooter)"),
                                    secondary_stat=paste0(at_least_one,"%"),
                                    secondary_label="Prob makes at least 1 of 2",
                                    detail=paste0("\U0001f3af Best FT option: ",best_ft_pl," (",best_ft_pct,"% \u2192 ",best_both,"% both)"),
                                    detail2=paste0("\u26a0\ufe0f Protect ",worst_ft_pl," (",worst_ft_pct,"% FT) \u2014 don't let them get fouled"),
                                    vs_league="Good FT%: 75%+", vs_diff=fouled_ft-75,
                                    gauge_pct=min(100,max(0,(fouled_ft-40)/(90-40)*100)))
                             },
                             "need_stop" = {
                               def_plays_pg  <- sum(spgs)+sum(bpgs)
                               est_poss      <- 70
                               stop_prob     <- round(min(95,(def_plays_pg/est_poss)*100),1)
                               base_miss     <- 55; def_boost <- min(10,def_plays_pg*1.5)
                               opp_penalty   <- 0
                               if (!is.null(opp_lineup_ss)&&nrow(opp_lineup_ss)>0) {
                                 opp_total_ppg <- sum(opp_lineup_ss$PPG,na.rm=TRUE)
                                 opp_penalty   <- max(-8,min(0,(80-opp_total_ppg)*0.15))
                               }
                               adj_miss <- round(base_miss+def_boost+opp_penalty,1)
                               best_def_idx <- which.max(spgs+bpgs); best_def_pl <- kings_lineup$Player[best_def_idx]
                               best_def_val <- round(spgs[best_def_idx]+bpgs[best_def_idx],1)
                               list(title="Need a Stop \u2014 One Defensive Possession",
                                    primary_stat=paste0(adj_miss,"%"),
                                    primary_label="Estimated probability opponent misses this possession",
                                    secondary_stat=paste0(stop_prob,"%"),
                                    secondary_label="Chance of forced turnover or block",
                                    detail=paste0("\U0001f6e1\ufe0f Defensive anchor: ",best_def_pl," (",best_def_val," STL+BLK per game)"),
                                    detail2=paste0("\U0001f3c0 Combined ",round(sum(rpgs),1)," RPG \u2014 board presence if they miss"),
                                    vs_league=paste0(round(def_plays_pg,1)," STL+BLK per game"),
                                    vs_diff=def_plays_pg-4,
                                    gauge_pct=min(100,max(0,(adj_miss-45)/(75-45)*100)))
                             },
                             "def_last" = {
                               total_rpg_l  <- sum(rpgs); total_bpg_l <- sum(bpgs)
                               reb_share    <- round(min(90,total_rpg_l/45*100),1)
                               contest_rate <- round(min(40,total_bpg_l/0.7*10),1)
                               best_reb_idx <- which.max(rpgs); best_rebounder <- kings_lineup$Player[best_reb_idx]
                               list(title="Defense \u2014 Last Possession",
                                    primary_stat=paste0(reb_share,"%"),
                                    primary_label="Estimated rebound probability if opponent misses",
                                    secondary_stat=paste0(contest_rate,"%"),
                                    secondary_label="Shot contest/block probability",
                                    detail=paste0("\U0001f3c0 Key rebounder: ",best_rebounder," (",rpgs[best_reb_idx]," RPG)"),
                                    detail2=paste0("Combined ",round(total_bpg_l,1)," BPG \u2014 ",
                                                   if(total_bpg_l>=2)"strong rim protection" else "limited shot blocking"),
                                    vs_league=paste0("Combined ",round(total_rpg_l,1)," RPG"),
                                    vs_diff=total_rpg_l-35,
                                    gauge_pct=min(100,max(0,(reb_share-40)/(90-40)*100)))
                             },
                             "scoring" = {
                               weighted_fg  <- round(sum(shot_weights*((fg2s+fg3s)/2)),1)
                               expected_pps <- round(sum(shot_weights*(fg2s/100*2+fg3s/100*3*0.3)),2)
                               list(title="Pure Scoring \u2014 Maximum Offensive Output",
                                    primary_stat=round(total_ppg_l,1),
                                    primary_label="Combined lineup PPG",
                                    secondary_stat=paste0(expected_pps," pts"),
                                    secondary_label="Expected points per possession",
                                    detail=paste0("\U0001f525 Primary scorer: ",primary_pl," (",ppgs[primary_idx]," PPG, ~",primary_wt,"% usage)"),
                                    detail2=paste0("Lineup weighted FG%: ",weighted_fg,"%"),
                                    vs_league="Good offense: 1.0+ pts/poss", vs_diff=expected_pps-1.0,
                                    gauge_pct=min(100,max(0,(total_ppg_l-50)/(120-50)*100)))
                             }
    )
    
    if (is.null(situation_data))
      return(p("Select a situation",style="text-align:center;color:#999;padding:20px;"))
    
    gauge_pct <- situation_data$gauge_pct
    grade     <- if(gauge_pct>=75)"A" else if(gauge_pct>=50)"B" else if(gauge_pct>=25)"C" else "D"
    grade_col <- switch(grade,"A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c")
    grade_bg_s <- switch(grade,
                         "A"="linear-gradient(135deg,#15803d,#22c55e)",
                         "B"="linear-gradient(135deg,#65a30d,#a3e635)",
                         "C"="linear-gradient(135deg,#d97706,#fbbf24)",
                         "D"="linear-gradient(135deg,#b91c1c,#f87171)")
    vs_color <- if(situation_data$vs_diff>0)"#15803d" else if(situation_data$vs_diff<0)"#b91c1c" else "#64748b"
    
    # Momentum
    get_streak_pct <- function(player_name) {
      if (player_name %in% names(rv$aba_data$all_player_stats)) {
        pd <- rv$aba_data$all_player_stats[[player_name]]
        if (!is.null(pd$game_log)&&nrow(pd$game_log)>=3&&!is.null(pd$per_game)) {
          rec <- pd$game_log %>% arrange(desc(date)) %>% head(3)
          rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)),na.rm=TRUE)
          szn_avg <- pd$per_game$PPG[1]
          if (!is.na(rec_avg)&&!is.na(szn_avg)&&szn_avg>0)
            return(round(((rec_avg-szn_avg)/szn_avg)*100,1))
        }
      }; return(0)
    }
    streaks    <- sapply(kings_lineup$Player, get_streak_pct)
    hot_count  <- sum(streaks>15); cold_count <- sum(streaks<-15)
    avg_streak <- round(mean(streaks),1)
    mom_label  <- if(avg_streak>10)"\U0001f525 On Fire" else if(avg_streak>0)"\u2b06\ufe0f Trending Up"
    else if(avg_streak>-10)"\u27a1\ufe0f Steady" else "\u2744\ufe0f Cold Stretch"
    mom_color  <- if(avg_streak>10)"#15803d" else if(avg_streak>0)"#65a30d"
    else if(avg_streak>-10)"#d97706" else "#b91c1c"
    
    div(
      div(style="display:flex;gap:20px;flex-wrap:wrap;",
          # Left: primary metric card
          div(style=paste0("flex:1;min-width:280px;background:white;border:2px solid ",grade_col,"25;",
                           "border-radius:14px;padding:24px;text-align:center;position:relative;",
                           "box-shadow:0 4px 16px rgba(0,0,0,0.06);"),
              div(style=paste0("position:absolute;top:12px;right:12px;width:40px;height:40px;",
                               "background:",grade_bg_s,";border-radius:50%;display:flex;",
                               "align-items:center;justify-content:center;font-weight:800;",
                               "font-size:1.1em;box-shadow:0 2px 8px rgba(0,0,0,0.15);color:",
                               if(grade%in%c("B","C"))"#1a1a2e;" else "white;"),
                  grade),
              h4(situation_data$title,style="margin:0 0 15px 0;color:#334155;font-weight:700;"),
              h2(situation_data$primary_stat,
                 style=paste0("margin:0;font-size:3.5em;font-weight:800;color:",grade_col,";")),
              p(situation_data$primary_label,style="margin:5px 0 0 0;color:#64748b;font-weight:600;"),
              div(style="margin:15px auto;max-width:250px;",
                  div(style="height:10px;background:#e2e8f0;border-radius:5px;overflow:hidden;",
                      div(style=paste0("width:",gauge_pct,"%;height:100%;background:",grade_col,
                                       ";border-radius:5px;"))),
                  div(style="display:flex;justify-content:space-between;margin-top:4px;",
                      span("Poor",style="font-size:0.65em;color:#94a3b8;"),
                      span("Elite",style="font-size:0.65em;color:#94a3b8;"))),
              div(style="margin-top:10px;",
                  span(situation_data$vs_league,style="font-size:0.85em;color:#64748b;"),
                  span(paste0(" (",ifelse(situation_data$vs_diff>0,"+",""),
                              round(situation_data$vs_diff,1),")"),
                       style=paste0("font-size:0.85em;font-weight:700;color:",vs_color,";margin-left:5px;")))
          ),
          # Right: secondary detail
          div(style="flex:1;min-width:280px;display:flex;flex-direction:column;gap:12px;",
              div(style="background:#f8fafc;border-radius:12px;padding:18px;border:1px solid #e2e8f0;",
                  h3(situation_data$secondary_stat,
                     style=paste0("margin:0;font-size:1.8em;font-weight:800;color:",ohio_kings_secondary,";")),
                  p(situation_data$secondary_label,style="margin:4px 0 0 0;color:#64748b;font-size:0.85em;")),
              div(style=paste0("background:",grade_col,"10;border-left:4px solid ",grade_col,
                               ";border-radius:8px;padding:14px;"),
                  p(situation_data$detail,
                    style="margin:0;color:#334155;font-weight:600;font-size:0.95em;"),
                  if (!is.null(situation_data$detail2))
                    p(situation_data$detail2,
                      style="margin:8px 0 0 0;color:#475569;font-weight:600;font-size:0.88em;")),
              if (has_opp_ss && !is.null(opp_lineup_ss) && nrow(opp_lineup_ss)>0) {
                opp_ppg_total <- sum(opp_lineup_ss$PPG,na.rm=TRUE)
                kings_on_off  <- criteria %in% c("off_down3","off_down2","scoring","ft_situation")
                opp_label     <- if(kings_on_off) "Defending (defensive lineup)" else "Attacking (offensive lineup)"
                div(style="background:#fef2f2;border:1px solid #fecaca;border-radius:10px;padding:14px;",
                    div(style="display:flex;justify-content:space-between;align-items:center;",
                        div(div(style="display:flex;align-items:center;gap:6px;",
                                span("\u26a0\ufe0f",style="font-size:1.2em;"),
                                span(paste0("vs ",input$opponent_select),
                                     style="font-weight:700;color:#991b1b;font-size:0.95em;")),
                            div(opp_label,style="font-size:0.8em;color:#64748b;margin-top:2px;")),
                        div(paste0(round(opp_ppg_total,1)," combined PPG"),
                            style="font-weight:700;color:#991b1b;font-size:0.9em;"))
                )
              },
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:14px;",
                  div(style="display:flex;justify-content:space-between;align-items:center;",
                      div(span("Lineup Momentum",style="font-weight:600;color:#334155;font-size:0.9em;"),
                          div(paste0(hot_count," hot / ",cold_count," cold / ",
                                     5-hot_count-cold_count," steady"),
                              style="margin-top:4px;font-size:0.8em;color:#64748b;")),
                      div(style="text-align:right;",
                          div(mom_label,style=paste0("font-weight:700;font-size:1.1em;color:",mom_color,";")),
                          div(paste0(ifelse(avg_streak>0,"+",""),avg_streak,"% avg"),
                              style=paste0("font-size:0.8em;font-weight:600;color:",mom_color,";"))))
              )
          )
      )
    )
  })
  
  output$win_probability <- renderUI({
    req(rv$logged_in, input$opponent_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$opponent_select == "")
      return(p("Select an opponent to see win probability analysis",
               style="text-align:center;color:#999;padding:20px;"))
    
    if (!rv$my_team %in% names(rv$aba_data$all_team_stats) ||
        !input$opponent_select %in% names(rv$aba_data$all_team_stats))
      return(p("Team data not available",
               style="text-align:center;color:#999;padding:20px;"))
    
    win_result_full <- calculate_unified_win_probability(
      NULL, input$opponent_select, rv$aba_data,
      include_schedule=TRUE, my_team_name=rv$my_team)
    
    kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
    opp_roster   <- rv$aba_data$all_team_stats[[input$opponent_select]]
    if (is.null(kings_roster)||is.null(opp_roster)||
        nrow(kings_roster)==0||nrow(opp_roster)==0)
      return(p("Unable to calculate - missing team data",
               style="text-align:center;color:#999;padding:20px;"))
    
    kings_roster_clean <- kings_roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    opp_roster_clean   <- opp_roster   %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    
    kings_record <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
    opp_record   <- rv$aba_data$league_data %>% filter(Team == input$opponent_select)
    k_gp <- if(nrow(kings_record)>0) kings_record$W[1]+kings_record$L[1] else 15
    o_gp <- if(nrow(opp_record)>0)   opp_record$W[1]+opp_record$L[1]     else 15
    k_wpct <- if(k_gp>0) kings_record$W[1]/k_gp else 0.5
    o_wpct <- if(o_gp>0) opp_record$W[1]/o_gp   else 0.5
    
    k_ppg <- get_accurate_team_ppg(rv$my_team,           rv$aba_data)
    o_ppg <- get_accurate_team_ppg(input$opponent_select, rv$aba_data)
    
    get_stat <- function(roster, col) {
      tr <- roster %>% filter(str_detect(toupper(Player),"TOTAL|TEAM"))
      if (nrow(tr)>0&&col%in%names(tr)) return(as.numeric(tr[[col]][1]))
      pr <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
      if (col%in%names(pr)) return(sum(as.numeric(pr[[col]]),na.rm=TRUE))
      return(0)
    }
    
    k_2pm<-get_stat(kings_roster,"2PM"); k_2pa<-get_stat(kings_roster,"2PA")
    k_3pm<-get_stat(kings_roster,"3PM"); k_3pa<-get_stat(kings_roster,"3PA")
    k_fga<-k_2pa+k_3pa; k_fgm<-k_2pm+k_3pm
    k_efg<-if(k_fga>0)(k_fgm+0.5*k_3pm)/k_fga*100 else 50
    k_fta<-get_stat(kings_roster,"FTA"); k_ftm<-get_stat(kings_roster,"FTM")
    k_oreb<-get_stat(kings_roster,"OREB"); k_dreb<-get_stat(kings_roster,"DREB")
    
    o_2pm<-get_stat(opp_roster,"2PM"); o_2pa<-get_stat(opp_roster,"2PA")
    o_3pm<-get_stat(opp_roster,"3PM"); o_3pa<-get_stat(opp_roster,"3PA")
    o_fga<-o_2pa+o_3pa; o_fgm<-o_2pm+o_3pm
    o_efg<-if(o_fga>0)(o_fgm+0.5*o_3pm)/o_fga*100 else 50
    o_fta<-get_stat(opp_roster,"FTA"); o_ftm<-get_stat(opp_roster,"FTM")
    o_oreb<-get_stat(opp_roster,"OREB"); o_dreb<-get_stat(opp_roster,"DREB")
    
    k_efg_val <- round(k_efg,1); o_efg_val <- round(o_efg,1)
    k_oreb_pct <- if((k_oreb+k_dreb)>0) round(k_oreb/(k_oreb+k_dreb)*100,1) else 0
    o_oreb_pct <- if((o_oreb+o_dreb)>0) round(o_oreb/(o_oreb+o_dreb)*100,1) else 0
    k_ftr <- if(k_fga>0) round(k_fta/k_fga,2) else 0
    o_ftr <- if(o_fga>0) round(o_fta/o_fga,2) else 0
    
    win_prob     <- win_result_full$prob
    opp_win_prob <- 100 - win_prob
    
    avg_total    <- (k_ppg + o_ppg) / 2
    team_spread  <- (win_prob-50)/50*12
    pred_kings   <- round(avg_total + team_spread/2, 0)
    pred_opp     <- round(avg_total - team_spread/2, 0)
    if (win_prob>50 && pred_kings<=pred_opp) pred_kings <- pred_opp+1
    if (win_prob<50 && pred_opp<=pred_kings) pred_opp   <- pred_kings+1
    
    efg_diff  <- k_efg_val - o_efg_val
    reb_diff  <- k_oreb_pct - o_oreb_pct
    ftr_diff  <- k_ftr - o_ftr
    
    adj_factor <- function(k_v, o_v) 100/(1+exp(-0.15*(k_v-o_v)))
    matchup_adv <- tryCatch({
      kl <- build_lineup(rv$my_team,           "scoring", rv$aba_data)
      ol <- build_lineup(input$opponent_select, "scoring", rv$aba_data)
      if (!is.null(kl)&&!is.null(ol)&&nrow(kl)>0&&nrow(ol)>0) {
        n_mu  <- min(nrow(kl),nrow(ol))
        edges <- sapply(1:n_mu,function(i) {
          k<-kl[i,]; o<-ol[i,]
          adj_factor(k$PPG,o$PPG)*0.40+adj_factor(k$RPG,o$RPG)*0.25+
            adj_factor(k$APG,o$APG)*0.20+adj_factor(k$SPG+k$BPG,o$SPG+o$BPG)*0.15
        })
        mean(edges)
      } else 50
    },error=function(e) 50)
    
    matchups_won <- tryCatch({
      kl <- build_lineup(rv$my_team,"scoring",rv$aba_data)
      ol <- build_lineup(input$opponent_select,"scoring",rv$aba_data)
      if(!is.null(kl)&&!is.null(ol)&&nrow(kl)>0&&nrow(ol)>0) {
        n_mu <- min(nrow(kl),nrow(ol))
        sum(sapply(1:n_mu,function(i){
          k<-kl[i,];o<-ol[i,]
          overall<-adj_factor(k$PPG,o$PPG)*0.40+adj_factor(k$RPG,o$RPG)*0.25+
            adj_factor(k$APG,o$APG)*0.20+adj_factor(k$SPG+k$BPG,o$SPG+o$BPG)*0.15
          overall>55
        }))
      } else 0
    },error=function(e) 0)
    
    n_mu_total <- tryCatch({
      kl<-build_lineup(rv$my_team,"scoring",rv$aba_data)
      ol<-build_lineup(input$opponent_select,"scoring",rv$aba_data)
      if(!is.null(kl)&&!is.null(ol)) min(nrow(kl),nrow(ol)) else 5
    },error=function(e) 5)
    
    div(
      if (!win_result_full$has_full_data) {
        div(style="background:#fffbeb;border:2px solid #f59e0b;border-radius:10px;padding:14px;margin-bottom:15px;",
            div(style="display:flex;align-items:center;gap:10px;",
                span("\u26a0\ufe0f",style="font-size:1.5em;"),
                div(strong("Limited Data Available",style="color:#92400e;font-size:1em;"),
                    p(win_result_full$message,
                      style="margin:4px 0 0 0;color:#78350f;font-size:0.85em;"))))
      },
      
      div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:15px;",
          div(h3(rv$my_team,style=paste0("margin:0;color:",ohio_kings_primary,";font-weight:800;")),
              h2(paste0(win_prob,"%"),
                 style=paste0("margin:5px 0 0 0;color:",ohio_kings_primary,";font-size:3em;"))),
          div(style="text-align:center;padding:0 20px;",
              p("WIN PROBABILITY",style="margin:0;color:#64748b;font-size:0.9em;font-weight:600;"),
              h1("VS",style="margin:10px 0;color:#1e293b;font-size:2.5em;")),
          div(style="text-align:right;",
              h3(input$opponent_select,style="margin:0;color:#ef4444;font-weight:800;"),
              h2(paste0(opp_win_prob,"%"),
                 style="margin:5px 0 0 0;color:#ef4444;font-size:3em;"))
      ),
      
      div(style="height:40px;background:#e2e8f0;border-radius:20px;overflow:hidden;position:relative;box-shadow:inset 0 2px 4px rgba(0,0,0,0.1);",
          div(style=paste0("width:",win_prob,"%;height:100%;",
                           "background:linear-gradient(90deg,",ohio_kings_primary,",",ohio_kings_secondary,");",
                           "display:flex;align-items:center;justify-content:flex-end;padding-right:15px;"),
              span(paste0(win_prob,"%"),style="color:white;font-weight:bold;font-size:1.1em;")),
          div(style="position:absolute;right:15px;top:50%;transform:translateY(-50%);color:#1e293b;font-weight:bold;font-size:1.1em;",
              paste0(opp_win_prob,"%"))),
      
      # Key advantages
      h4("Key Factors:",style="margin:20px 0 15px 0;color:#334155;"),
      fluidRow(
        column(6, div(style=paste0("background:",if(efg_diff>0)"#d1fae5" else "#fee2e2",
                                   ";border-left:4px solid ",if(efg_diff>0)"#10b981" else "#ef4444",
                                   ";padding:15px;border-radius:8px;"),
                      div(style="display:flex;justify-content:space-between;align-items:center;",
                          span("\U0001f3af Shooting Efficiency (eFG%)",style="font-weight:600;color:#1e293b;"),
                          span(paste0(if(efg_diff>=0)"\u2713 " else "\u2717 ",
                                      ifelse(efg_diff>=0,"+",""),round(efg_diff,1),"%"),
                               style=paste0("font-weight:700;font-size:1.2em;color:",
                                            if(efg_diff>=0)"#10b981" else "#ef4444",";"))),
                      p(paste0(rv$my_team,": ",k_efg_val,"% | Opp: ",o_efg_val,"%"),
                        style="margin:8px 0 0 0;color:#64748b;font-size:0.9em;"))),
        column(6, div(style=paste0("background:",if(reb_diff>=0)"#d1fae5" else "#fee2e2",
                                   ";border-left:4px solid ",if(reb_diff>=0)"#10b981" else "#ef4444",
                                   ";padding:15px;border-radius:8px;"),
                      div(style="display:flex;justify-content:space-between;align-items:center;",
                          span("\U0001f3c0 Rebounding (OREB%)",style="font-weight:600;color:#1e293b;"),
                          span(paste0(if(reb_diff>=0)"\u2713 " else "\u2717 ",
                                      ifelse(reb_diff>=0,"+",""),round(reb_diff,1),"%"),
                               style=paste0("font-weight:700;font-size:1.2em;color:",
                                            if(reb_diff>=0)"#10b981" else "#ef4444",";"))),
                      p(paste0(rv$my_team,": ",k_oreb_pct,"% | Opp: ",o_oreb_pct,"%"),
                        style="margin:8px 0 0 0;color:#64748b;font-size:0.9em;")))
      ),
      fluidRow(
        column(6, div(style=paste0("background:",if(ftr_diff>=0)"#d1fae5" else "#fee2e2",
                                   ";border-left:4px solid ",if(ftr_diff>=0)"#10b981" else "#ef4444",
                                   ";padding:15px;border-radius:8px;margin-top:10px;"),
                      div(style="display:flex;justify-content:space-between;align-items:center;",
                          span("\U0001f4ca Free Throw Rate",style="font-weight:600;color:#1e293b;"),
                          span(paste0(if(ftr_diff>=0)"\u2713 " else "\u2717 ",
                                      ifelse(ftr_diff>=0,"+",""),round(ftr_diff,2)),
                               style=paste0("font-weight:700;font-size:1.2em;color:",
                                            if(ftr_diff>=0)"#10b981" else "#ef4444",";"))),
                      p(paste0(rv$my_team,": ",k_ftr," | Opp: ",o_ftr),
                        style="margin:8px 0 0 0;color:#64748b;font-size:0.9em;"))),
        column(6, div(style=paste0("background:",if(matchup_adv>55)"#d1fae5" else if(matchup_adv<45)"#fee2e2" else "#fffbeb",
                                   ";border-left:4px solid ",if(matchup_adv>55)"#10b981" else if(matchup_adv<45)"#ef4444" else "#f59e0b",
                                   ";padding:15px;border-radius:8px;margin-top:10px;"),
                      div(style="display:flex;justify-content:space-between;align-items:center;",
                          span("\U0001f91c Player Matchups",style="font-weight:600;color:#1e293b;"),
                          span(paste0(matchups_won,"/",n_mu_total," won"),
                               style=paste0("font-weight:700;font-size:1.2em;color:",
                                            if(matchup_adv>55)"#10b981" else if(matchup_adv<45)"#ef4444" else "#f59e0b",";"))),
                      p(paste0("Overall matchup: ",round(matchup_adv,1),"% ",rv$my_team," edge"),
                        style="margin:8px 0 0 0;color:#64748b;font-size:0.9em;")))
      ),
      
      # Predicted scores
      div(style="display:flex;gap:15px;margin-top:20px;",
          div(style=paste0("flex:1;background:linear-gradient(135deg,",ohio_kings_primary,",",ohio_kings_secondary,");",
                           "color:white;padding:20px;border-radius:12px;text-align:center;"),
              h4("PREDICTED SCORE",style="margin:0 0 10px 0;opacity:0.8;font-size:0.85em;letter-spacing:1px;"),
              div(style="display:flex;justify-content:center;align-items:center;gap:20px;",
                  div(div(rv$my_team,style="font-size:0.8em;opacity:0.8;"),
                      div(pred_kings,style="font-size:3em;font-weight:800;line-height:1;")),
                  div("-",style="font-size:2em;opacity:0.5;"),
                  div(div(input$opponent_select,style="font-size:0.8em;opacity:0.8;"),
                      div(pred_opp,style="font-size:3em;font-weight:800;line-height:1;"))),
              p(paste0("Based on PPG + four factors + record"),
                style="margin:10px 0 0 0;opacity:0.7;font-size:0.8em;")),
          div(style="flex:1;background:linear-gradient(135deg,#1e293b,#334155);color:white;padding:20px;border-radius:12px;",
              div(paste0(rv$my_team," W-L: ",if(nrow(kings_record)>0) paste0(kings_record$W[1],"-",kings_record$L[1]) else "N/A",
                         " (",round(k_wpct*100,0),"%)"),
                  style="font-size:0.85em;opacity:0.8;margin-bottom:6px;"),
              div(paste0(input$opponent_select," W-L: ",
                         if(nrow(opp_record)>0) paste0(opp_record$W[1],"-",opp_record$L[1]) else "N/A",
                         " (",round(o_wpct*100,0),"%)"),
                  style="font-size:0.85em;opacity:0.8;margin-bottom:6px;"),
              div(paste0("PPG: ",k_ppg," vs ",o_ppg),
                  style="font-size:0.85em;opacity:0.8;"))
      )
    )
  })
  
  output$player_matchups <- renderUI({
    req(rv$logged_in, input$situation_select, input$opponent_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$situation_select==""||input$opponent_select=="")
      return(p("Select a situation and opponent to see player matchup predictions",
               style="text-align:center;color:#999;padding:20px;"))
    
    kings_lineup <- build_lineup(rv$my_team,            "scoring", rv$aba_data)
    opp_lineup   <- build_lineup(input$opponent_select, "scoring", rv$aba_data)
    
    if (is.null(kings_lineup)||is.null(opp_lineup)||nrow(kings_lineup)==0||nrow(opp_lineup)==0)
      return(p("Lineup data not available for matchup analysis",
               style="text-align:center;color:#999;padding:20px;"))
    
    n_matchups <- min(nrow(kings_lineup),nrow(opp_lineup))
    
    all_edges <- sapply(1:n_matchups,function(i){
      k<-kings_lineup[i,]; o<-opp_lineup[i,]
      100/(1+exp(-0.15*(k$PPG-o$PPG)))*0.40 +
        100/(1+exp(-0.3*(k$RPG-o$RPG)))*0.25  +
        100/(1+exp(-0.3*(k$APG-o$APG)))*0.20  +
        100/(1+exp(-0.5*((k$SPG+k$BPG)-(o$SPG+o$BPG))))*0.15
    })
    best_idx  <- which.max(all_edges); worst_idx <- which.min(all_edges)
    
    div(
      # Exploit/watch callout
      div(style="display:flex;gap:12px;margin-bottom:15px;",
          div(style="flex:1;background:#f0fdf4;border:1px solid #bbf7d0;border-radius:10px;padding:14px;",
              div(style="display:flex;align-items:center;gap:8px;",
                  span("\u2705",style="font-size:1.3em;"),
                  div(div("EXPLOIT THIS",style="font-weight:800;font-size:0.75em;color:#15803d;letter-spacing:0.5px;"),
                      div(paste0(kings_lineup$lineup_position[best_idx],": ",
                                 kings_lineup$Player[best_idx]," has ",
                                 round(all_edges[best_idx],1),"% edge vs ",
                                 opp_lineup$Player[best_idx]),
                          style="font-weight:600;font-size:0.9em;color:#166534;margin-top:2px;")))),
          div(style="flex:1;background:#fef2f2;border:1px solid #fecaca;border-radius:10px;padding:14px;",
              div(style="display:flex;align-items:center;gap:8px;",
                  span("\u26a0\ufe0f",style="font-size:1.3em;"),
                  div(div("WATCH OUT",style="font-weight:800;font-size:0.75em;color:#b91c1c;letter-spacing:0.5px;"),
                      div(paste0(kings_lineup$lineup_position[worst_idx],": ",
                                 opp_lineup$Player[worst_idx]," has ",
                                 round(100-all_edges[worst_idx],1),"% edge vs ",
                                 kings_lineup$Player[worst_idx]),
                          style="font-weight:600;font-size:0.9em;color:#991b1b;margin-top:2px;")))
          )
      ),
      
      lapply(1:n_matchups, function(i) {
        k <- kings_lineup[i,]; o <- opp_lineup[i,]
        sc_adv <- 100/(1+exp(-0.15*(k$PPG-o$PPG)))
        rb_adv <- 100/(1+exp(-0.3*(k$RPG-o$RPG)))
        as_adv <- 100/(1+exp(-0.3*(k$APG-o$APG)))
        df_adv <- 100/(1+exp(-0.5*((k$SPG+k$BPG)-(o$SPG+o$BPG))))
        overall <- round(sc_adv*0.40+rb_adv*0.25+as_adv*0.20+df_adv*0.15,1)
        opp_adv <- round(100-overall,1)
        winner  <- if(overall>55)"kings" else if(overall<45)"opp" else "even"
        
        k_pred <- predict_player_game(k$Player, input$opponent_select)
        o_pred <- predict_player_game(o$Player, rv$my_team)
        k_pred_pts <- if(!is.na(k_pred$pts)) k_pred$pts else k$PPG
        k_pred_reb <- if(!is.na(k_pred$reb)) k_pred$reb else k$RPG
        k_pred_ast <- if(!is.na(k_pred$ast)) k_pred$ast else k$APG
        o_pred_pts <- if(!is.na(o_pred$pts)) o_pred$pts else o$PPG
        o_pred_reb <- if(!is.na(o_pred$reb)) o_pred$reb else o$RPG
        o_pred_ast <- if(!is.na(o_pred$ast)) o_pred$ast else o$APG
        
        border_c <- if(winner=="kings")"#10b981" else if(winner=="opp")"#ef4444" else "#f59e0b"
        bg_c     <- if(winner=="kings")"#f0fdf4" else if(winner=="opp")"#fef2f2" else "#fffbeb"
        
        div(style=paste0("background:",bg_c,";border-left:5px solid ",border_c,
                         ";padding:18px;margin:12px 0;border-radius:8px;",
                         "box-shadow:0 2px 6px rgba(0,0,0,0.08);"),
            div(style="text-align:center;margin-bottom:12px;",
                span(paste0("— ",k$lineup_position," MATCHUP —"),
                     style="font-weight:700;color:#64748b;font-size:0.85em;letter-spacing:1px;")),
            div(style="display:flex;justify-content:space-between;align-items:center;",
                # Kings player
                div(style="flex:1;text-align:left;",
                    div(strong(k$Player,style=paste0("font-size:1.1em;color:",ohio_kings_primary,";")),
                        span(paste0(" (",k$Position,")"),style="color:#64748b;font-size:0.9em;")),
                    div(style="font-size:0.85em;color:#64748b;margin-top:4px;",
                        paste0("Season: ",k$PPG," / ",k$RPG," / ",k$APG)),
                    div(style="display:flex;gap:4px;margin-top:4px;",
                        span(paste0(k_pred_pts," PTS"),
                             style=paste0("font-size:0.75em;font-weight:700;color:white;background:",ohio_kings_primary,";padding:2px 6px;border-radius:4px;")),
                        span(paste0(k_pred_reb," REB"),
                             style=paste0("font-size:0.75em;font-weight:700;color:white;background:",ohio_kings_secondary,";padding:2px 6px;border-radius:4px;")),
                        span(paste0(k_pred_ast," AST"),
                             style=paste0("font-size:0.75em;font-weight:700;color:#1e293b;background:",ohio_kings_accent,";padding:2px 6px;border-radius:4px;"))),
                    div(style="font-size:0.7em;color:#94a3b8;margin-top:2px;font-style:italic;",
                        "\u2191 Predicted for this game")),
                # Edge indicator
                div(style="flex:0 0 120px;text-align:center;",
                    div(if(winner=="kings") paste0(overall,"%") else if(winner=="opp") paste0(opp_adv,"%") else "EVEN",
                        style=paste0("font-size:1.8em;font-weight:800;color:",border_c,";")),
                    div(if(winner=="kings") paste0(rv$my_team," Edge") else if(winner=="opp") "Opp Edge" else "Toss-Up",
                        style=paste0("font-size:0.75em;font-weight:600;color:",border_c,";"))),
                # Opp player
                div(style="flex:1;text-align:right;",
                    div(strong(o$Player,style="font-size:1.1em;color:#991b1b;"),
                        span(paste0(" (",o$Position,")"),style="color:#64748b;font-size:0.9em;")),
                    div(style="font-size:0.85em;color:#64748b;margin-top:4px;",
                        paste0("Season: ",o$PPG," / ",o$RPG," / ",o$APG)),
                    div(style="display:flex;gap:4px;margin-top:4px;justify-content:flex-end;",
                        span(paste0(o_pred_pts," PTS"),style="font-size:0.75em;font-weight:700;color:white;background:#991b1b;padding:2px 6px;border-radius:4px;"),
                        span(paste0(o_pred_reb," REB"),style="font-size:0.75em;font-weight:700;color:white;background:#7f1d1d;padding:2px 6px;border-radius:4px;"),
                        span(paste0(o_pred_ast," AST"),style="font-size:0.75em;font-weight:700;color:#1e293b;background:#fca5a5;padding:2px 6px;border-radius:4px;")),
                    div(style="font-size:0.7em;color:#94a3b8;margin-top:2px;font-style:italic;text-align:right;",
                        "\u2191 Predicted for this game"))
            ),
            div(style="margin-top:12px;",
                div(style="display:flex;align-items:center;gap:8px;",
                    span(rv$my_team,style=paste0("font-size:0.75em;font-weight:600;color:",ohio_kings_primary,";width:80px;")),
                    div(style=paste0("flex-grow:1;height:14px;background:#fee2e2;border-radius:7px;overflow:hidden;"),
                        div(style=paste0("width:",overall,"%;height:100%;",
                                         "background:linear-gradient(90deg,",ohio_kings_primary,",",ohio_kings_secondary,");",
                                         "border-radius:7px;"))),
                    span("Opp",style="font-size:0.75em;font-weight:600;color:#991b1b;width:30px;text-align:right;"))),
            div(style="display:flex;gap:15px;margin-top:10px;justify-content:center;",
                span(paste0("\U0001f3af Scoring: ",round(sc_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(sc_adv>55)"#10b981" else if(sc_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f3c0 Boards: ",round(rb_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(rb_adv>55)"#10b981" else if(rb_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f3af Playmaking: ",round(as_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(as_adv>55)"#10b981" else if(as_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f6e1\ufe0f Defense: ",round(df_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(df_adv>55)"#10b981" else if(df_adv<45)"#ef4444" else "#64748b",";")))
        )
      })
    )
  })
  
  # ==========================================================================
  # SCHEDULE TAB OUTPUTS
  # ==========================================================================
  
  output$calendar_view <- renderUI({
    req(rv$logged_in, rv$aba_data, input$schedule_month, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data available",
               style = "text-align:center;color:#999;padding:40px;"))
    
    kings_schedule <- rv$aba_data$team_schedules[[rv$my_team]]
    month_num      <- match(input$schedule_month, month.name)
    year_for_month <- if (month_num >= 10) 2025 else 2026
    
    schedule_filtered <- kings_schedule %>%
      filter(month(date) == month_num, year(date) == year_for_month) %>%
      filter(!is.na(opponent), opponent != "") %>%
      mutate(
        difficulty = sapply(opponent, function(opp) {
          tryCatch(calculate_game_difficulty(opp, rv$aba_data), error = function(e) 5)
        }),
        difficulty = ifelse(is.na(difficulty) | is.nan(difficulty), 5, difficulty),
        diff_info  = lapply(difficulty, function(d) {
          tryCatch(get_difficulty_label(d), error = function(e)
            list(label = "\u2696\ufe0f Even", color = "#65a30d", bg = "#f7fee7"))
        })
      )
    
    if (nrow(schedule_filtered) == 0)
      return(p(paste("No games scheduled in", input$schedule_month),
               style = "text-align:center;color:#999;padding:40px;"))
    
    first_day      <- as.Date(paste0(year_for_month, "-", sprintf("%02d", month_num), "-01"))
    last_day       <- ceiling_date(first_day, "month") - days(1)
    start_weekday  <- wday(first_day, week_start = 1)
    if (is.na(start_weekday)) start_weekday <- 1
    padding_before <- max(0, start_weekday - 1)
    calendar_days  <- seq(first_day, last_day, by = "day")
    
    safe_str <- function(x, fallback = "") {
      if (is.null(x) || length(x) == 0 || is.na(x)) fallback else as.character(x)
    }
    safe_num <- function(x, fallback = 0) {
      v <- suppressWarnings(as.numeric(x))
      if (is.null(v) || length(v) == 0 || is.na(v)) fallback else v
    }
    
    div(
      style = "background:linear-gradient(180deg,#f8fafc,#ffffff);border-radius:12px;padding:20px;",
      
      div(style = "text-align:center;margin-bottom:20px;",
          h3(paste(input$schedule_month, year_for_month),
             style = "margin:0;color:#1e293b;font-weight:800;")),
      
      div(style = "display:grid;grid-template-columns:repeat(7,1fr);gap:8px;margin-bottom:8px;",
          lapply(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), function(d)
            div(d, style = "text-align:center;font-weight:700;color:#64748b;font-size:0.85em;padding:8px;"))),
      
      div(style = "display:grid;grid-template-columns:repeat(7,1fr);gap:8px;",
          
          if (padding_before > 0)
            lapply(seq_len(padding_before), function(i)
              div(style = "min-height:100px;background:#f8fafc;border-radius:8px;")),
          
          lapply(calendar_days, function(day) {
            day_num     <- day(day)
            games_today <- schedule_filtered %>% filter(date == day)
            
            if (nrow(games_today) > 0) {
              game      <- games_today[1, ]
              opp       <- safe_str(game$opponent, "Unknown")
              t_score   <- safe_num(game$team_score)
              o_score   <- safe_num(game$opp_score)
              result    <- safe_str(game$result)
              ha        <- safe_str(game$home_away, "Home")
              is_past   <- !is.na(game$date) && game$date < Sys.Date()
              diff_info <- tryCatch({
                di <- game$diff_info[[1]]
                if (is.null(di) || !is.list(di))
                  list(label="\u2696\ufe0f Even",color="#65a30d",bg="#f7fee7") else di
              }, error = function(e)
                list(label="\u2696\ufe0f Even",color="#65a30d",bg="#f7fee7"))
              
              opp_logo <- ""
              if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
                lr <- rv$aba_data$team_lookup %>% filter(team_name==opp) %>% pull(logo_url)
                if (length(lr) > 0 && !is.na(lr[1])) opp_logo <- lr[1]
              }
              opp_short <- tryCatch({
                parts <- strsplit(opp," ")[[1]]
                if (length(parts)>1) tail(parts,1) else opp
              }, error=function(e) opp)
              
              div(
                style = paste0(
                  "min-height:100px;background:", safe_str(diff_info$bg,"#f7fee7"),
                  ";border:2px solid ", safe_str(diff_info$color,"#65a30d"),
                  ";border-radius:10px;padding:10px;cursor:pointer;position:relative;",
                  "transition:all 0.2s;box-shadow:0 2px 8px rgba(0,0,0,0.06);"
                ),
                onclick     = paste0("Shiny.setInputValue('selected_game_date','",
                                     as.character(day),"',{priority:'event'})"),
                onmouseover = "this.style.transform='translateY(-3px)';this.style.boxShadow='0 6px 16px rgba(0,0,0,0.12)';",
                onmouseout  = "this.style.transform='translateY(0)';this.style.boxShadow='0 2px 8px rgba(0,0,0,0.06)';",
                
                div(day_num, style="font-weight:800;font-size:1.1em;color:#1e293b;margin-bottom:6px;"),
                
                if (opp_logo != "")
                  div(style="text-align:center;margin:6px 0;",
                      tags$img(src=opp_logo,
                               style="width:32px;height:32px;border-radius:50%;border:1px solid rgba(0,0,0,0.1);")),
                
                div(style="font-size:0.7em;font-weight:600;color:#475569;text-align:center;margin:4px 0;",
                    opp_short),
                
                if (is_past && result != "") {
                  div(style="text-align:center;margin-top:6px;",
                      div(style=paste0(
                        "display:inline-block;background:",
                        if(result=="W")"#15803d" else "#b91c1c",
                        ";color:white;padding:3px 8px;border-radius:6px;font-weight:700;font-size:0.75em;"),
                        paste0(result," ",t_score,"-",o_score)))
                } else {
                  div(style="text-align:center;margin-top:6px;",
                      div(safe_str(diff_info$label,"\u2696\ufe0f Even"),
                          style=paste0("font-size:0.65em;font-weight:700;color:",
                                       safe_str(diff_info$color,"#65a30d"),";")))
                },
                
                div(style="position:absolute;top:6px;right:6px;font-size:0.6em;
                           background:rgba(0,0,0,0.05);padding:2px 6px;border-radius:4px;
                           font-weight:600;color:#64748b;",
                    if (ha=="Home") "\U0001f3e0" else "\u2708\ufe0f")
              )
            } else {
              div(style="min-height:100px;background:white;border:1px solid #e2e8f0;border-radius:8px;padding:10px;",
                  div(day_num,style="font-weight:600;font-size:0.95em;color:#cbd5e1;"))
            }
          })
      ),
      
      div(style="display:flex;justify-content:center;gap:20px;margin-top:20px;padding:15px;
                 background:white;border-radius:8px;border:1px solid #e2e8f0;flex-wrap:wrap;",
          lapply(list(
            list("\U0001f60c Routine",    "#15803d"),
            list("\u2696\ufe0f Even",     "#65a30d"),
            list("\U0001f4aa Competitive","#d97706"),
            list("\U0001f525 Battle",     "#b91c1c"),
            list("\u2694\ufe0f War",      "#7f1d1d")
          ), function(item)
            div(style="display:flex;align-items:center;gap:6px;",
                div(style=paste0("width:12px;height:12px;border-radius:3px;background:",item[[2]],";")),
                span(item[[1]],style="font-size:0.8em;font-weight:600;color:#64748b;"))),
          div(style="display:flex;align-items:center;gap:8px;margin-left:20px;padding-left:20px;border-left:2px solid #e2e8f0;",
              span("\U0001f3e0 Home",style="font-size:0.75em;color:#64748b;font-weight:600;"),
              span("\u2708\ufe0f Away",  style="font-size:0.75em;color:#64748b;font-weight:600;"))
      )
    )
  })
  
  output$schedule_insights <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    full_schedule <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      mutate(
        difficulty = sapply(opponent, function(opp) {
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        is_past = date < Sys.Date()
      )
    
    past_games   <- full_schedule %>% filter(is_past==TRUE,  !is_forfeit)
    future_games <- full_schedule %>% filter(is_past==FALSE)
    
    streak_label <- "Season Start"; streak_icon <- "\U0001f3c0"; streak_color <- "#64748b"
    if (nrow(past_games) > 0) {
      recent <- past_games %>% filter(!is.na(result)) %>% arrange(desc(date))
      if (nrow(recent) > 0) {
        streak_count <- 0; streak_type <- recent$result[1]
        for (i in 1:nrow(recent)) {
          if (recent$result[i]==streak_type) streak_count <- streak_count+1 else break
        }
        if (streak_count > 0) {
          streak_label <- paste0(streak_count, streak_type)
          streak_icon  <- if(streak_type=="W") "\U0001f525" else "\u2744\ufe0f"
          streak_color <- if(streak_type=="W") "#15803d" else "#b91c1c"
        }
      }
    }
    
    upcoming_label <- "Season End"; upcoming_color <- "#64748b"; next_3_diff <- 5
    if (nrow(future_games) >= 3) {
      next_3_diff <- mean(future_games$difficulty[1:3],na.rm=TRUE)
      if (!is.na(next_3_diff)) {
        upcoming_label <- if(next_3_diff>=7)"Brutal Stretch"
        else if(next_3_diff>=5.5)"Tough Schedule"
        else if(next_3_diff>=4)"Balanced" else "Easy Run"
        upcoming_color <- if(next_3_diff>=7)"#b91c1c"
        else if(next_3_diff>=5.5)"#d97706"
        else if(next_3_diff>=4)"#65a30d" else "#15803d"
      }
    }
    
    avg_difficulty <- round(mean(full_schedule$difficulty,na.rm=TRUE),1)
    if (is.na(avg_difficulty)) avg_difficulty <- 5
    sos_label <- if(avg_difficulty>=6.5)"Brutal" else if(avg_difficulty>=5)"Tough"
    else if(avg_difficulty>=4)"Average" else "Easy"
    
    div(
      div(style=paste0("background:linear-gradient(135deg,",streak_color,",",streak_color,
                       "cc);color:white;padding:20px;border-radius:12px;text-align:center;margin-bottom:12px;"),
          div(streak_icon,style="font-size:2em;"),
          h3(streak_label,style="margin:8px 0 0 0;font-size:2em;font-weight:800;"),
          p("Current Streak",style="margin:5px 0 0 0;opacity:0.8;font-size:0.9em;")),
      div(style=paste0("background:",upcoming_color,"15;border:2px solid ",upcoming_color,
                       ";padding:16px;border-radius:10px;margin-bottom:12px;"),
          div(style="display:flex;justify-content:space-between;align-items:center;",
              div(div("Next 3 Games",style="font-size:0.75em;color:#64748b;font-weight:600;"),
                  div(upcoming_label,
                      style=paste0("font-size:1.2em;font-weight:800;color:",upcoming_color,";margin-top:2px;"))),
              div(round(next_3_diff,1),
                  style=paste0("font-size:2em;font-weight:800;color:",upcoming_color,";"))),
          div(style="margin-top:10px;font-size:0.8em;color:#475569;",
              "Avg difficulty rating (1-10 scale)")),
      div(style="background:white;border:1px solid #e2e8f0;padding:16px;border-radius:10px;",
          div(style="display:flex;justify-content:space-between;align-items:center;",
              span("Season SOS",style="font-size:0.85em;font-weight:600;color:#64748b;"),
              div(span(sos_label,style="font-weight:700;color:#334155;margin-right:8px;"),
                  span(avg_difficulty,style=paste0("font-weight:800;font-size:1.3em;color:",ohio_kings_primary,";")))))
    )
  })
  
  output$next_games_preview <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    upcoming <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(date >= Sys.Date()) %>%
      arrange(date) %>%
      head(5) %>%
      rowwise() %>%
      mutate(
        difficulty = calculate_game_difficulty(opponent,rv$aba_data),
        diff_info  = list(get_difficulty_label(difficulty))
      ) %>%
      ungroup()
    
    if (nrow(upcoming) == 0)
      return(div(style="text-align:center;padding:40px;",
                 div("\U0001f3c1",style="font-size:3em;opacity:0.3;"),
                 p("Season Complete",style="color:#94a3b8;margin-top:10px;")))
    
    div(lapply(1:nrow(upcoming), function(i) {
      game      <- upcoming[i,]
      diff_info <- game$diff_info[[1]]
      win_result <- tryCatch(
        calculate_unified_win_probability(NULL, game$opponent, rv$aba_data,
                                          include_schedule=TRUE, my_team_name=rv$my_team),
        error=function(e) list(prob=50,has_full_data=FALSE))
      win_prob <- if (is.list(win_result)) win_result$prob else 50
      
      div(style=paste0("background:white;border-left:4px solid ",diff_info$color,
                       ";padding:12px;margin-bottom:10px;border-radius:8px;",
                       "cursor:pointer;transition:all 0.2s;"),
          onclick     = paste0("Shiny.setInputValue('selected_game_date','",
                               as.character(game$date),"',{priority:'event'})"),
          onmouseover = "this.style.background='#f8fafc';",
          onmouseout  = "this.style.background='white';",
          div(style="display:flex;justify-content:space-between;align-items:center;",
              div(div(format(game$date,"%b %d"),style="font-size:0.75em;color:#64748b;font-weight:600;"),
                  div(paste0("vs ",game$opponent),
                      style="font-weight:700;color:#1e293b;font-size:0.95em;margin-top:2px;"),
                  div(style="display:flex;gap:8px;margin-top:4px;",
                      span(diff_info$label,
                           style=paste0("font-size:0.65em;font-weight:700;color:",diff_info$color,";")),
                      span(if(game$home_away=="Home")"\U0001f3e0 Home" else "\u2708\ufe0f Away",
                           style="font-size:0.65em;color:#94a3b8;font-weight:600;"))),
              div(style="text-align:right;",
                  div(paste0(win_prob,"%"),
                      style=paste0("font-size:1.5em;font-weight:800;color:",
                                   if(win_prob>=60)"#15803d" else if(win_prob>=40)"#d97706" else "#b91c1c",";")),
                  div("Win Prob",style="font-size:0.65em;color:#94a3b8;font-weight:600;")))
      )
    }))
  })
  
  output$schedule_matchup_breakdown <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    full_schedule <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      mutate(
        difficulty = sapply(opponent,function(opp){
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        is_past = date < Sys.Date()
      )
    
    categorize_diff <- function(d) {
      if(d>=8.5)"War" else if(d>=7)"Battle"
      else if(d>=5.5)"Competitive" else if(d>=4)"Even" else "Routine"
    }
    cat_color <- function(cat) switch(cat,"War"="#7f1d1d","Battle"="#b91c1c",
                                      "Competitive"="#d97706","Even"="#65a30d",
                                      "Routine"="#15803d","#64748b")
    cat_icon  <- function(cat) switch(cat,"War"="\u2694\ufe0f","Battle"="\U0001f525",
                                      "Competitive"="\U0001f4aa","Even"="\u2696\ufe0f",
                                      "Routine"="\U0001f60c","\U0001f3c0")
    
    past   <- full_schedule %>% filter(is_past==TRUE,  !is_forfeit)
    future <- full_schedule %>% filter(is_past==FALSE)
    
    past_breakdown <- if (nrow(past)>0) {
      past %>%
        mutate(category=sapply(difficulty,categorize_diff)) %>%
        group_by(category) %>%
        summarize(games=n(), wins=sum(result=="W",na.rm=TRUE),
                  avg_diff=round(mean(difficulty,na.rm=TRUE),1), .groups="drop") %>%
        arrange(desc(avg_diff))
    } else data.frame()
    
    future_breakdown <- if (nrow(future)>0) {
      future %>% mutate(category=sapply(difficulty,categorize_diff)) %>%
        count(category,name="games")
    } else data.frame()
    
    div(
      h4("By Opponent Strength",style="margin:0 0 16px 0;color:#334155;font-weight:700;"),
      if (nrow(past_breakdown)>0) tagList(
        div("Past Games Performance",
            style="font-size:0.75em;color:#64748b;font-weight:600;margin-bottom:10px;"),
        lapply(1:nrow(past_breakdown),function(i) {
          cat <- past_breakdown[i,]
          cc  <- cat_color(cat$category); ci <- cat_icon(cat$category)
          div(style="margin-bottom:10px;",
              div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;",
                  span(paste(ci,cat$category),style="font-size:0.85em;font-weight:600;color:#475569;"),
                  div(span(paste0(cat$games," games"),style="font-weight:600;color:#64748b;margin-right:6px;font-size:0.8em;"),
                      span(paste0(cat$wins,"-",cat$games-cat$wins),
                           style=paste0("font-weight:700;color:",cc,";")))),
              div(style=paste0("height:24px;background:",cc,"15;border-radius:6px;",
                               "padding:4px 10px;margin-top:4px;"),
                  span(paste0("Avg Difficulty: ",cat$avg_diff,"/10"),
                       style=paste0("font-size:0.75em;font-weight:700;color:",cc,";"))))
        })
      ),
      if (nrow(future_breakdown)>0) tagList(
        div("Remaining Schedule",
            style="font-size:0.75em;color:#64748b;font-weight:600;margin:20px 0 10px;"),
        div(style="display:flex;gap:8px;flex-wrap:wrap;",
            lapply(1:nrow(future_breakdown),function(i) {
              cat <- future_breakdown[i,]; cc <- cat_color(cat$category)
              div(style=paste0("background:",cc,"10;border:1px solid ",cc,
                               ";padding:8px 12px;border-radius:8px;"),
                  span(paste0(cat$games," ",cat$category),
                       style=paste0("font-size:0.8em;font-weight:700;color:",cc,";")))
            }))
      )
    )
  })
  
  output$difficulty_timeline <- renderPlotly({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(plotly_empty())
    
    timeline_data <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      mutate(
        difficulty  = sapply(opponent,function(opp){
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        game_num    = row_number(),
        is_past     = date < Sys.Date(),
        point_color = case_when(
          is_past & result=="W" ~ "#22c55e",
          is_past & result=="L" ~ "#ef4444",
          TRUE                  ~ "#94a3b8"
        ),
        hover_text = paste0(
          "Game ",game_num,": ",opponent,"\n",
          format(date,"%b %d")," | ",
          if_else(home_away=="Home","\U0001f3e0 Home","\u2708\ufe0f Away"),"\n",
          if_else(is_past,paste0(result," ",team_score,"-",opp_score),"Upcoming"),"\n",
          "Difficulty: ",round(difficulty,1),"/10"
        )
      )
    
    plot_ly(timeline_data, x=~game_num, y=~difficulty) %>%
      add_trace(type="scatter",mode="lines+markers",
                line   = list(color=ohio_kings_primary,width=2),
                marker = list(size=10,color=~point_color,line=list(color="white",width=2)),
                text=~hover_text,hoverinfo="text",name="Difficulty") %>%
      add_trace(y=7,type="scatter",mode="lines",
                line=list(color="#b91c1c",width=1,dash="dash"),
                name="Tough Threshold",hoverinfo="skip",showlegend=FALSE) %>%
      add_trace(y=4,type="scatter",mode="lines",
                line=list(color="#65a30d",width=1,dash="dash"),
                name="Easy Threshold",hoverinfo="skip",showlegend=FALSE) %>%
      layout(xaxis=list(title="Game Number"),
             yaxis=list(title="Difficulty (1-10)",range=c(0,10)),
             hovermode="closest",showlegend=FALSE,margin=list(t=20,b=40))
  })
  
  output$division_games_tracker <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    ohio_kings_accent  <- rv$team_cols$accent
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    kings_div <- rv$aba_data$league_data %>% filter(Team==rv$my_team) %>% pull(Division)
    if (length(kings_div)==0) return(p("Division info not available",style="color:#999;"))
    
    div_teams <- rv$aba_data$league_data %>%
      filter(Division==kings_div[1], Team!=rv$my_team) %>% pull(Team)
    
    full_schedule <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      mutate(is_division=opponent %in% div_teams, is_past=date<Sys.Date())
    
    div_past   <- full_schedule %>% filter(is_division, is_past,  !is_forfeit)
    div_future <- full_schedule %>% filter(is_division, !is_past)
    
    div_wins   <- if(nrow(div_past)>0) sum(div_past$result=="W",na.rm=TRUE) else 0
    div_losses <- if(nrow(div_past)>0) sum(div_past$result=="L",na.rm=TRUE) else 0
    div_wpct   <- if((div_wins+div_losses)>0) round(div_wins/(div_wins+div_losses)*100,0) else 0
    
    div(
      div(style=paste0("background:linear-gradient(135deg,",ohio_kings_accent,",#e6a800);",
                       "color:#451a03;padding:20px;border-radius:12px;",
                       "text-align:center;margin-bottom:16px;"),
          div("\U0001f451",style="font-size:2em;"),
          h3(paste0(div_wins,"-",div_losses),
             style="margin:8px 0 0 0;font-size:2.5em;font-weight:800;"),
          p(paste0("Division Record (",div_wpct,"%)"),
            style="margin:5px 0 0 0;opacity:0.9;font-size:0.9em;font-weight:600;")),
      if (nrow(div_future)>0) tagList(
        div(paste0(nrow(div_future)," Division Games Remaining"),
            style="font-size:0.8em;font-weight:700;color:#64748b;margin-bottom:10px;text-transform:uppercase;"),
        lapply(1:min(5,nrow(div_future)),function(i) {
          game      <- div_future[i,]
          diff      <- tryCatch(calculate_game_difficulty(game$opponent,rv$aba_data),error=function(e) 5)
          diff_info <- get_difficulty_label(diff)
          div(style=paste0("background:white;border-left:3px solid ",diff_info$color,
                           ";padding:10px;margin-bottom:8px;border-radius:6px;"),
              div(style="display:flex;justify-content:space-between;align-items:center;",
                  div(div(format(game$date,"%b %d"),style="font-size:0.7em;color:#94a3b8;"),
                      div(game$opponent,style="font-weight:700;color:#1e293b;font-size:0.9em;margin-top:2px;")),
                  span(diff_info$label,
                       style=paste0("font-size:0.7em;font-weight:700;color:",diff_info$color,";"))))
        })
      ) else p("No upcoming division games",
               style="text-align:center;color:#94a3b8;padding:20px;font-style:italic;")
    )
  })
  
  output$key_games_watch <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    upcoming <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(date >= Sys.Date()) %>% arrange(date) %>%
      mutate(difficulty=sapply(opponent,function(opp){
        tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
      }))
    
    if (nrow(upcoming)==0)
      return(p("Season complete",style="text-align:center;color:#94a3b8;padding:20px;"))
    
    kings_div <- rv$aba_data$league_data %>% filter(Team==rv$my_team) %>% pull(Division)
    div_teams <- if(length(kings_div)>0) {
      rv$aba_data$league_data %>% filter(Division==kings_div[1],Team!=rv$my_team) %>% pull(Team)
    } else c()
    
    key_games <- upcoming %>%
      head(10) %>%
      mutate(
        importance=case_when(
          opponent %in% div_teams & difficulty>=7 ~ "\U0001f525 MUST-WIN Division Battle",
          difficulty>=8.5                          ~ "\u2694\ufe0f Season-Defining Game",
          opponent %in% div_teams                  ~ "\U0001f451 Division Game",
          difficulty>=7                            ~ "\U0001f4aa Tough Test",
          difficulty<=3.5                          ~ "\u2705 Winnable Game",
          TRUE                                     ~ "\U0001f4ca Regular Game"
        ),
        imp_rank=case_when(
          opponent %in% div_teams & difficulty>=7 ~ 1,
          difficulty>=8.5                          ~ 2,
          opponent %in% div_teams                  ~ 3,
          difficulty>=7                            ~ 4,
          TRUE                                     ~ 5
        )
      ) %>%
      arrange(imp_rank,desc(difficulty)) %>%
      head(5)
    
    div(lapply(1:nrow(key_games),function(i) {
      game      <- key_games[i,]
      diff_info <- get_difficulty_label(game$difficulty)
      bc <- switch(as.character(game$imp_rank),
                   "1"="#b91c1c","2"="#7f1d1d","3"="#f59e0b","4"="#d97706","#64748b")
      div(style=paste0("background:white;border:2px solid ",bc,
                       ";padding:14px;margin-bottom:10px;border-radius:10px;",
                       "cursor:pointer;transition:all 0.2s;"),
          onclick     = paste0("Shiny.setInputValue('selected_game_date','",
                               as.character(game$date),"',{priority:'event'})"),
          onmouseover = "this.style.transform='translateY(-2px)';this.style.boxShadow='0 4px 12px rgba(0,0,0,0.1)';",
          onmouseout  = "this.style.transform='translateY(0)';this.style.boxShadow='none';",
          div(game$importance,
              style=paste0("font-size:0.75em;font-weight:700;color:",bc,";margin-bottom:6px;letter-spacing:0.3px;")),
          div(style="display:flex;justify-content:space-between;align-items:center;",
              div(div(format(game$date,"%A, %B %d"),
                      style="font-size:0.75em;color:#64748b;font-weight:600;"),
                  div(paste0("vs ",game$opponent),
                      style="font-weight:700;color:#1e293b;font-size:1em;margin-top:2px;"),
                  div(style="margin-top:4px;",
                      span(diff_info$label,
                           style=paste0("font-size:0.7em;font-weight:600;color:",diff_info$color,";margin-right:8px;")),
                      span(if(game$home_away=="Home")"\U0001f3e0 Home" else "\u2708\ufe0f Away",
                           style="font-size:0.7em;color:#94a3b8;"))),
              div(style="text-align:right;",
                  div(round(game$difficulty,1),
                      style=paste0("font-size:2em;font-weight:800;color:",ohio_kings_primary,";")),
                  div("/10",style="font-size:0.7em;color:#94a3b8;")))
      )
    }))
  })
  
  output$schedule_win_loss_prediction <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    upcoming <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(date >= Sys.Date()) %>% arrange(date) %>%
      mutate(
        difficulty = sapply(opponent,function(opp){
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        win_prob = sapply(opponent,function(opp){
          tryCatch({
            res <- calculate_unified_win_probability(NULL,opp,rv$aba_data,
                                                     include_schedule=TRUE,
                                                     my_team_name=rv$my_team)
            if(is.list(res)) res$prob else as.numeric(res)
          },error=function(e) 50)
        })
      )
    
    if (nrow(upcoming)==0)
      return(p("Season complete - no games remaining",
               style="text-align:center;color:#94a3b8;padding:20px;"))
    
    expected_wins  <- round(sum(upcoming$win_prob)/100,1)
    k_rec          <- rv$aba_data$league_data %>% filter(Team==rv$my_team)
    current_wins   <- if(nrow(k_rec)>0) k_rec$W[1] else 0
    current_losses <- if(nrow(k_rec)>0) k_rec$L[1] else 0
    current_total  <- current_wins + current_losses
    proj_wins      <- current_wins + round(expected_wins)
    proj_losses    <- current_total + nrow(upcoming) - proj_wins
    proj_wpct      <- round(proj_wins/(proj_wins+proj_losses)*100,0)
    
    div(
      div(style="display:flex;gap:20px;margin-bottom:20px;flex-wrap:wrap;",
          div(style="flex:1;min-width:200px;background:linear-gradient(135deg,#475569,#64748b);
                     color:white;padding:20px;border-radius:12px;text-align:center;",
              div("CURRENT",style="font-size:0.7em;opacity:0.6;letter-spacing:1px;margin-bottom:8px;"),
              h2(paste0(current_wins,"-",current_losses),
                 style="margin:0;font-size:2.5em;font-weight:800;"),
              p(paste0(if(current_total>0) round(current_wins/current_total*100,0) else 0,"% win rate"),
                style="margin:8px 0 0 0;opacity:0.8;font-size:0.85em;")),
          div(style="flex:1;min-width:200px;background:linear-gradient(135deg,#3b82f6,#2563eb);
                     color:white;padding:20px;border-radius:12px;text-align:center;",
              div("EXPECTED WINS",style="font-size:0.7em;opacity:0.6;letter-spacing:1px;margin-bottom:8px;"),
              h2(expected_wins,style="margin:0;font-size:2.5em;font-weight:800;"),
              p(paste0("From next ",nrow(upcoming)," games"),
                style="margin:8px 0 0 0;opacity:0.8;font-size:0.85em;")),
          div(style=paste0("flex:1;min-width:200px;background:linear-gradient(135deg,",
                           ohio_kings_primary,",",ohio_kings_secondary,");",
                           "color:white;padding:20px;border-radius:12px;text-align:center;"),
              div("PROJECTED FINAL",style="font-size:0.7em;opacity:0.6;letter-spacing:1px;margin-bottom:8px;"),
              h2(paste0(proj_wins,"-",proj_losses),style="margin:0;font-size:2.5em;font-weight:800;"),
              p(paste0(proj_wpct,"% win rate"),
                style="margin:8px 0 0 0;opacity:0.8;font-size:0.85em;font-weight:700;"))
      ),
      div(style="background:#f8fafc;border-radius:10px;padding:16px;",
          h4("Remaining Games",style="margin:0 0 12px 0;color:#334155;font-weight:700;"),
          div(style="max-height:300px;overflow-y:auto;",
              lapply(1:nrow(upcoming),function(i) {
                game <- upcoming[i,]
                div(style="background:white;border:1px solid #e2e8f0;padding:10px;margin-bottom:8px;border-radius:6px;",
                    div(style="display:flex;justify-content:space-between;align-items:center;",
                        div(div(format(game$date,"%b %d"),style="font-size:0.7em;color:#94a3b8;font-weight:600;"),
                            div(game$opponent,style="font-weight:700;color:#1e293b;font-size:0.85em;margin-top:2px;")),
                        div(style="text-align:right;",
                            div(paste0(game$win_prob,"%"),
                                style=paste0("font-size:1.2em;font-weight:800;color:",
                                             if(game$win_prob>=60)"#15803d" else if(game$win_prob>=40)"#d97706" else "#b91c1c",";")),
                            div("win prob",style="font-size:0.6em;color:#94a3b8;"))))
              })))
    )
  })
  
  observeEvent(input$selected_game_date, {
    req(rv$aba_data, rv$my_team)
    game_date <- as.Date(input$selected_game_date)
    if (!rv$my_team %in% names(rv$aba_data$team_schedules)) return()
    game_info <- rv$aba_data$team_schedules[[rv$my_team]] %>% filter(date == game_date)
    if (nrow(game_info) == 0) return()
    game <- game_info[1,]
    updateSelectInput(session, "scout_team",      selected = game$opponent)
    updateSelectInput(session, "scout_game_select",selected = as.character(game_date))
    updateTabItems(session, "tabs", "scouting")
  })
  
  # ==========================================================================
  # SCOUTING REPORT TAB
  # ==========================================================================
  
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$scout_team == "")
      return(div(style="text-align:center;padding:60px 20px;",
                 div("\U0001f50d",style="font-size:4em;opacity:0.3;"),
                 h3("Select an opponent to generate scouting report",
                    style="color:#94a3b8;margin-top:15px;"),
                 p("Choose a team from the dropdown above",style="color:#cbd5e1;")))
    
    opp_name   <- input$scout_team
    opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
    opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
    if (is.null(opp_roster)||nrow(opp_record)==0)
      return(p("No data available for this team",
               style="text-align:center;color:#999;padding:40px;"))
    
    opp_gp   <- opp_record$W[1] + opp_record$L[1]
    opp_wpct <- if(opp_gp>0) round(opp_record$W[1]/opp_gp*100,0) else 50
    
    get_stat <- function(roster,col) {
      tr <- roster %>% filter(str_detect(toupper(Player),"TOTAL|TEAM"))
      if(nrow(tr)>0&&col%in%names(tr)) return(as.numeric(tr[[col]][1]))
      pr <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
      if(col%in%names(pr)) return(sum(as.numeric(pr[[col]]),na.rm=TRUE))
      return(0)
    }
    
    opp_pts <- get_stat(opp_roster,"PTS"); opp_reb <- get_stat(opp_roster,"REB")
    opp_ast <- get_stat(opp_roster,"AST")
    opp_2pm <- get_stat(opp_roster,"2PM"); opp_2pa <- get_stat(opp_roster,"2PA")
    opp_3pm <- get_stat(opp_roster,"3PM"); opp_3pa <- get_stat(opp_roster,"3PA")
    opp_ftm <- get_stat(opp_roster,"FTM"); opp_fta <- get_stat(opp_roster,"FTA")
    
    opp_ppg <- get_accurate_team_ppg(opp_name,rv$aba_data)
    if(opp_ppg==0) opp_ppg <- round(opp_pts/opp_gp,1)
    opp_rpg <- round(opp_reb/opp_gp,1)
    opp_apg <- round(opp_ast/opp_gp,1)
    
    fg2_pct    <- if(opp_2pa>0) round(opp_2pm/opp_2pa*100,1) else 0
    fg3_pct    <- if(opp_3pa>0) round(opp_3pm/opp_3pa*100,1) else 0
    ft_pct     <- if(opp_fta>0) round(opp_ftm/opp_fta*100,1) else 0
    three_rate <- if((opp_2pa+opp_3pa)>0) round(opp_3pa/(opp_2pa+opp_3pa)*100,0) else 0
    play_style <- if(three_rate>=40)"Perimeter-Heavy" else if(three_rate>=25)"Balanced" else "Interior-Focused"
    
    pts_from_2 <- opp_2pm*2; pts_from_3 <- opp_3pm*3; pts_from_ft <- opp_ftm
    total_scored <- pts_from_2+pts_from_3+pts_from_ft
    if(total_scored<opp_pts&&total_scored>0){
      diff <- opp_pts-total_scored
      pts_from_2 <- pts_from_2+round(diff*pts_from_2/total_scored)
      pts_from_3 <- pts_from_3+round(diff*pts_from_3/total_scored)
      pts_from_ft <- opp_pts-pts_from_2-pts_from_3
    }
    pct_2  <- if(opp_pts>0) round(pts_from_2/opp_pts*100,0) else 0
    pct_3  <- if(opp_pts>0) round(pts_from_3/opp_pts*100,0) else 0
    pct_ft <- if(opp_pts>0) round(pts_from_ft/opp_pts*100,0) else 0
    
    player_roster <- opp_roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    
    key_players <- player_roster %>%
      mutate(
        player_ppg = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pg<-rv$aba_data$all_player_stats[[pl]]$per_game
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) return(pg$PPG[1])
          }; return(0)
        }),
        player_rpg = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pg<-rv$aba_data$all_player_stats[[pl]]$per_game
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$RPG[1])) return(pg$RPG[1])
          }; return(0)
        }),
        player_apg = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pg<-rv$aba_data$all_player_stats[[pl]]$per_game
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$APG[1])) return(pg$APG[1])
          }; return(0)
        }),
        player_pos = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pos<-rv$aba_data$all_player_stats[[pl]]$position
            if(!is.null(pos)&&!is.na(pos)) return(pos)
          }; return("G")
        }),
        gp = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            gl<-rv$aba_data$all_player_stats[[pl]]$game_log
            if(!is.null(gl)) return(nrow(gl))
          }; return(0)
        }),
        streak = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pd<-rv$aba_data$all_player_stats[[pl]]
            if(!is.null(pd$game_log)&&nrow(pd$game_log)>=3&&!is.null(pd$per_game)){
              rec<-pd$game_log%>%arrange(desc(date))%>%head(3)
              rec_avg<-mean(suppressWarnings(as.numeric(rec$PTS)),na.rm=TRUE)
              szn_avg<-pd$per_game$PPG[1]
              if(!is.na(rec_avg)&&!is.na(szn_avg)&&szn_avg>0)
                return(round(((rec_avg-szn_avg)/szn_avg)*100,1))
            }
          }; return(0)
        })
      ) %>%
      filter(gp>=3,player_ppg>0) %>%
      arrange(desc(player_ppg)) %>%
      head(5)
    
    top_scorer_share <- if(opp_ppg>0&&nrow(key_players)>0)
      round(key_players$player_ppg[1]/opp_ppg*100,0) else 0
    top2_share <- if(opp_ppg>0&&nrow(key_players)>=2)
      round(sum(key_players$player_ppg[1:2])/opp_ppg*100,0) else top_scorer_share
    concentration <- if(top_scorer_share>=30)"Star-Dependent"
    else if(top2_share>=50)"Top-Heavy" else "Balanced Scoring"
    
    hot_players  <- key_players %>% filter(streak>15)
    cold_players <- key_players %>% filter(streak<-15)
    
    threat       <- if(opp_wpct>=70)"HIGH" else if(opp_wpct>=50)"MODERATE" else "LOW"
    threat_color <- if(threat=="HIGH")"#b91c1c" else if(threat=="MODERATE")"#d97706" else "#15803d"
    
    kings_record <- rv$aba_data$league_data %>% filter(Team==rv$my_team)
    kings_ppg    <- get_accurate_team_ppg(rv$my_team,rv$aba_data)
    kings_rpg    <- get_accurate_team_rpg(rv$my_team,rv$aba_data)
    kings_apg    <- get_accurate_team_apg(rv$my_team,rv$aba_data)
    kings_gp     <- if(nrow(kings_record)>0) kings_record$W[1]+kings_record$L[1] else 0
    kings_wpct   <- if(kings_gp>0) round(kings_record$W[1]/kings_gp*100,0) else 50
    
    opp_logo <- ""
    if("logo_url"%in%names(rv$aba_data$team_lookup)){
      lr<-rv$aba_data$team_lookup%>%filter(team_name==opp_name)%>%pull(logo_url)
      if(length(lr)>0&&!is.na(lr[1])) opp_logo<-lr[1]
    }
    
    # Strategy generation
    strategy_points <- c()
    if(nrow(key_players)>=1){
      significant <- key_players %>% filter(player_ppg>=opp_ppg*0.15)
      n_threats   <- nrow(significant)
      if(n_threats==1){
        strategy_points<-c(strategy_points,paste0(
          "Contain ",significant$Player[1]," (",significant$player_ppg[1]," PPG, ",
          top_scorer_share,"% of scoring) — team has one dominant option. ",
          "Double aggressively and force role players to create."))
      } else if(n_threats==2){
        p2_share<-if(opp_ppg>0) round(significant$player_ppg[2]/opp_ppg*100,0) else 0
        strategy_points<-c(strategy_points,paste0(
          "Two-headed attack: ",significant$Player[1]," (",significant$player_ppg[1]," PPG) and ",
          significant$Player[2]," (",significant$player_ppg[2]," PPG, ",p2_share,
          "%) combine for ",top2_share,"% of scoring — can't focus on just one."))
      } else if(n_threats>=3){
        strategy_points<-c(strategy_points,paste0(
          "Deep scoring team with ",n_threats," players averaging 15%+ of team scoring — ",
          "prioritize team defense and communication over individual assignments."))
      }
    }
    if(three_rate>=35&&fg3_pct>=35){
      strategy_points<-c(strategy_points,paste0(
        "DANGER from 3: They shoot ",three_rate,"% of shots from deep at ",fg3_pct,
        "% — close out hard, no open looks. Contest every three-point attempt."))
    } else if(three_rate>=35&&fg3_pct<35){
      strategy_points<-c(strategy_points,paste0(
        "They attempt a lot of 3s (",three_rate,"% of shots) but shoot poorly (",fg3_pct,
        "%) — let them settle for contested threes, don't over-help."))
    } else if(three_rate<25&&fg2_pct>=50){
      strategy_points<-c(strategy_points,paste0(
        "Interior-focused team shooting ",fg2_pct,
        "% from 2 — pack the paint, wall up on drives, force perimeter shots."))
    } else {
      strategy_points<-c(strategy_points,paste0(
        "Balanced attack: ",three_rate,"% from 3 (",fg3_pct,"%), ",fg2_pct,
        "% on 2s — play honest defense, don't overcommit either way."))
    }
    if(opp_rpg>kings_rpg+3){
      strategy_points<-c(strategy_points,paste0(
        "REBOUNDING MISMATCH: They average ",opp_rpg," RPG vs our ",kings_rpg,
        " — must crash boards harder, consider a bigger lineup."))
    } else if(opp_rpg>kings_rpg){
      strategy_points<-c(strategy_points,paste0(
        "Slight rebounding edge to them (",opp_rpg," vs our ",kings_rpg,
        " RPG) — box out consistently, don't give up second chances."))
    } else if(kings_rpg>opp_rpg+3){
      strategy_points<-c(strategy_points,paste0(
        "We dominate the glass (",kings_rpg," vs their ",opp_rpg,
        " RPG) — push tempo off defensive boards, attack offensive glass."))
    }
    if(opp_apg>18){
      strategy_points<-c(strategy_points,paste0(
        "High assist team (",opp_apg," APG) — they move the ball well. ",
        "Disrupt passing lanes, pressure the ball handler."))
    } else if(opp_apg<10){
      strategy_points<-c(strategy_points,paste0(
        "Low assist team (",opp_apg," APG) — isolation-heavy offense. ",
        "Stay in front of your man, limit dribble penetration."))
    }
    if(ft_pct>=75){
      strategy_points<-c(strategy_points,paste0(
        "Strong FT shooting (",ft_pct,"%) — avoid putting them on the line in close games."))
    } else if(ft_pct<60){
      strategy_points<-c(strategy_points,paste0(
        "Poor FT shooting (",ft_pct,"%) — consider strategic fouling in late-game situations."))
    }
    if(nrow(hot_players)>0){
      hot_details<-sapply(1:nrow(hot_players),function(i)
        paste0(hot_players$Player[i]," (+",hot_players$streak[i],"% above avg)"))
      strategy_points<-c(strategy_points,paste0(
        "\U0001f525 HOT ALERT: ",paste(hot_details,collapse=", "),
        " — extra attention needed, playing well above season average."))
    }
    if(nrow(cold_players)>0){
      cold_details<-sapply(1:nrow(cold_players),function(i)
        paste0(cold_players$Player[i]," (",cold_players$streak[i],"% below avg)"))
      strategy_points<-c(strategy_points,paste0(
        "\u2744\ufe0f Cold stretch: ",paste(cold_details,collapse=", "),
        " — still dangerous based on season averages, but may be less aggressive."))
    }
    if(kings_ppg>opp_ppg+10){
      strategy_points<-c(strategy_points,paste0(
        "\U0001f4aa We outscore them by ",round(kings_ppg-opp_ppg,0),
        " PPG — push the pace, get into transition, make this a track meet."))
    } else if(opp_ppg>kings_ppg+10){
      strategy_points<-c(strategy_points,paste0(
        "\u26a0\ufe0f They outscore us by ",round(opp_ppg-kings_ppg,0),
        " PPG — slow the game down, limit possessions, make every trip count."))
    }
    
    # Auto-generated narrative
    narrative_parts <- c()
    narrative_parts<-c(narrative_parts,paste0(
      "The ",opp_name," enter at ",opp_record$W[1],"-",opp_record$L[1],
      " (",opp_wpct,"%) from the ",opp_record$Division[1]," Division, ",
      "averaging ",opp_ppg," points per game."))
    narrative_parts<-c(narrative_parts,paste0(
      "Their offense is ",tolower(play_style),
      if(play_style=="Perimeter-Heavy") paste0(", launching ",three_rate,
                                               "% of their shots from deep",
                                               if(fg3_pct>=38) paste0(" and converting at a lethal ",fg3_pct,"%.") else paste0(" at a ",fg3_pct,"% clip."))
      else if(play_style=="Interior-Focused") paste0(", preferring to work inside where they shoot ",fg2_pct,"% from two.")
      else paste0(" with ",three_rate,"% from 3 (",fg3_pct,"%) and ",fg2_pct,"% from inside.")))
    if(nrow(key_players)>=1){
      p1<-key_players[1,]
      if(concentration=="Star-Dependent"){
        narrative_parts<-c(narrative_parts,paste0(
          p1$Player," carries ",top_scorer_share,"% of the scoring at ",p1$player_ppg,
          " PPG. When contained, this team struggles to find offense elsewhere."))
      } else if(top2_share>=45&&nrow(key_players)>=2){
        p2<-key_players[2,]
        p2_share<-if(opp_ppg>0) round(p2$player_ppg/opp_ppg*100,0) else 0
        narrative_parts<-c(narrative_parts,paste0(
          "The scoring punch comes from ",p1$Player," (",p1$player_ppg," PPG) and ",
          p2$Player," (",p2$player_ppg," PPG, ",p2_share,"%) — both are legitimate threats."))
      } else {
        narrative_parts<-c(narrative_parts,paste0(
          p1$Player," leads at ",p1$player_ppg," PPG but scoring is well-distributed. ",
          "No single player to shut down."))
      }
    }
    narrative_parts<-c(narrative_parts,paste0(
      "For ",rv$my_team,", ",
      if(kings_ppg>opp_ppg+15) paste0("this is a favorable matchup — we outscore them by ",round(kings_ppg-opp_ppg,0)," PPG. Stay focused and don't play down to competition. ")
      else if(kings_ppg>opp_ppg) paste0("we hold a scoring edge (",kings_ppg," vs ",opp_ppg," PPG) but can't afford complacency. ")
      else if(opp_ppg>kings_ppg+10) paste0("we'll need to overcome a scoring gap (",kings_ppg," vs ",opp_ppg," PPG). Tempo control and defense are critical. ")
      else paste0("this projects as a competitive game (",kings_ppg," vs ",opp_ppg," PPG). Execution and adjustments will decide it. "),
      "Overall, this is a ",tolower(threat),"-threat opponent that ",
      if(threat=="HIGH") "demands our full attention and best effort."
      else if(threat=="MODERATE") "we should handle with proper preparation and focus."
      else "we're expected to beat, but can't take lightly."))
    
    div(
      # Report Header
      div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                       "padding:30px;border-radius:14px;margin-bottom:20px;position:relative;overflow:hidden;"),
          div(style="position:absolute;right:-20px;top:-20px;font-size:12em;opacity:0.03;font-weight:900;","\U0001f50d"),
          div(style="display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
              if(opp_logo!="")
                tags$img(src=opp_logo,
                         style="width:80px;height:80px;border-radius:50%;border:3px solid rgba(255,255,255,0.3);object-fit:cover;background:white;"),
              div(div("PRE-GAME SCOUTING REPORT",
                      style="font-size:0.75em;font-weight:600;opacity:0.6;letter-spacing:2px;"),
                  h2(opp_name,style="margin:5px 0 0 0;font-weight:800;font-size:1.8em;"),
                  div(paste0(opp_record$Division[1]," Division | ",
                             opp_record$W[1],"-",opp_record$L[1]," (",opp_wpct,"% win rate)"),
                      style="opacity:0.7;margin-top:4px;")),
              div(style="margin-left:auto;text-align:right;",
                  div("THREAT LEVEL",style="font-size:0.7em;opacity:0.5;letter-spacing:1px;"),
                  div(threat,
                      style=paste0("font-size:1.8em;font-weight:800;color:",threat_color,
                                   ";background:rgba(255,255,255,0.1);padding:4px 20px;",
                                   "border-radius:8px;margin-top:4px;")))
          )
      ),
      
      # Quick stats row
      div(style="display:flex;gap:12px;margin-bottom:20px;flex-wrap:wrap;",
          lapply(list(
            list("\U0001f3c0","PPG",  opp_ppg,             "per game"),
            list("\U0001f4aa","RPG",  opp_rpg,             "per game"),
            list("\U0001f91d","APG",  opp_apg,             "per game"),
            list("\U0001f3af","2P%",  paste0(fg2_pct,"%"),  "from inside"),
            list("\u2604\ufe0f","3P%",paste0(fg3_pct,"%"),  paste0(three_rate,"% of shots")),
            list("\u2705",    "FT%",  paste0(ft_pct,"%"),   "from the line")
          ),function(item)
            div(style="flex:1;min-width:120px;background:white;border:1px solid #e2e8f0;border-radius:10px;padding:14px;text-align:center;",
                div(item[[1]],style="font-size:1.3em;"),
                div(item[[3]],style="font-size:1.5em;font-weight:800;color:#1e293b;margin:4px 0;"),
                div(item[[2]],style="font-size:0.75em;font-weight:700;color:#64748b;"),
                div(item[[4]],style="font-size:0.65em;color:#94a3b8;margin-top:2px;"))
          )
      ),
      
      # Two column layout
      div(style="display:flex;gap:20px;flex-wrap:wrap;",
          
          # Left: key players + tendencies
          div(style="flex:1;min-width:350px;",
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;margin-bottom:16px;",
                  h4("\U0001f464 Key Players to Watch",
                     style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  if(nrow(key_players)>0) lapply(1:nrow(key_players),function(i){
                    kp<-key_players[i,]
                    is_primary<-i<=2&&kp$player_ppg>=opp_ppg*0.15
                    streak_icon<-if(kp$streak>15)"\U0001f525" else if(kp$streak<-15)"\u2744\ufe0f" else ""
                    streak_text<-if(kp$streak>15)"HOT" else if(kp$streak<-15)"COLD" else ""
                    player_share<-if(opp_ppg>0) round(kp$player_ppg/opp_ppg*100,0) else 0
                    bg_c<-if(is_primary)"#fef2f2" else "#f8fafc"
                    bd_c<-if(is_primary)"#b91c1c" else "#e2e8f0"
                    tc_c<-if(is_primary)"#991b1b" else "#334155"
                    div(style=paste0("padding:12px;border-radius:10px;margin-bottom:8px;",
                                     "border-left:4px solid ",bd_c,";background:",bg_c,";"),
                        div(style="display:flex;justify-content:space-between;align-items:start;",
                            div(div(style="display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
                                    span(paste0("#",i),style="font-weight:800;color:#94a3b8;font-size:0.85em;"),
                                    strong(kp$Player,style=paste0("font-size:1em;color:",tc_c,";")),
                                    span(paste0("(",kp$player_pos,")"),style="color:#64748b;font-size:0.85em;"),
                                    if(streak_text!="") span(paste0(streak_icon," ",streak_text),
                                                             style=paste0("font-size:0.7em;font-weight:700;color:",
                                                                          if(streak_text=="HOT")"#b91c1c" else "#3b82f6",
                                                                          ";background:",
                                                                          if(streak_text=="HOT")"#fef2f2" else "#eff6ff",
                                                                          ";padding:1px 6px;border-radius:4px;"))),
                                div(style="display:flex;gap:8px;margin-top:6px;",
                                    span(paste0(kp$player_ppg," PPG"),
                                         style=paste0("font-size:0.8em;font-weight:700;color:",ohio_kings_primary,";background:#fee2e2;padding:2px 6px;border-radius:4px;")),
                                    span(paste0(kp$player_rpg," RPG"),style="font-size:0.8em;font-weight:700;color:#1e40af;background:#dbeafe;padding:2px 6px;border-radius:4px;"),
                                    span(paste0(kp$player_apg," APG"),style="font-size:0.8em;font-weight:700;color:#b45309;background:#fef3c7;padding:2px 6px;border-radius:4px;")),
                                div(paste0(kp$gp," games played"),style="font-size:0.7em;color:#94a3b8;margin-top:4px;")),
                            if(player_share>=15)
                              div(style=paste0("text-align:center;background:#b91c1c;",
                                               "color:white;padding:4px 10px;border-radius:8px;",
                                               "font-size:0.7em;font-weight:700;white-space:nowrap;"),
                                  paste0(player_share,"% of\nteam PPG"))
                        )
                    )
                  }) else p("No player data available",style="color:#94a3b8;text-align:center;")
              ),
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                  h4("\U0001f4ca Team Tendencies",style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  div(style="display:flex;gap:12px;margin-bottom:14px;",
                      div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                       if(play_style=="Perimeter-Heavy")"#dbeafe"
                                       else if(play_style=="Interior-Focused")"#fee2e2" else "#f0fdf4",";"),
                          div(play_style,style="font-weight:700;font-size:0.95em;color:#334155;"),
                          div(paste0(three_rate,"% of shots from 3"),style="font-size:0.75em;color:#64748b;margin-top:2px;")),
                      div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                       if(concentration=="Star-Dependent")"#fef2f2"
                                       else if(concentration=="Balanced Scoring")"#f0fdf4" else "#fffbeb",";"),
                          div(concentration,style="font-weight:700;font-size:0.95em;color:#334155;"),
                          div(paste0("Top 2 = ",top2_share,"% of scoring"),style="font-size:0.75em;color:#64748b;margin-top:2px;"))
                  ),
                  div(div(style="display:flex;justify-content:space-between;margin-bottom:6px;",
                          span("Scoring Distribution",style="font-weight:600;color:#475569;font-size:0.85em;"),
                          span(paste0(opp_ppg," PPG"),style="font-weight:700;color:#334155;font-size:0.85em;")),
                      div(style="height:28px;border-radius:8px;overflow:hidden;display:flex;",
                          div(style=paste0("width:",pct_2,"%;background:",ohio_kings_primary,
                                           ";display:flex;align-items:center;justify-content:center;"),
                              if(pct_2>=15) span(paste0("2P ",pct_2,"%"),style="color:white;font-size:0.65em;font-weight:700;")),
                          div(style=paste0("width:",pct_3,"%;background:",ohio_kings_secondary,
                                           ";display:flex;align-items:center;justify-content:center;"),
                              if(pct_3>=15) span(paste0("3P ",pct_3,"%"),style="color:white;font-size:0.65em;font-weight:700;")),
                          div(style=paste0("width:",pct_ft,"%;background:",ohio_kings_accent,
                                           ";display:flex;align-items:center;justify-content:center;"),
                              if(pct_ft>=10) span(paste0("FT ",pct_ft,"%"),style="color:#1e293b;font-size:0.65em;font-weight:700;")))
                  )
              )
          ),
          
          # Right: strategy + matchup comparison
          div(style="flex:1;min-width:350px;",
              div(style="background:white;border:2px solid #15803d;border-radius:12px;padding:20px;margin-bottom:16px;",
                  h4("\u2705 How to Beat Them",style="margin:0 0 14px 0;color:#15803d;font-weight:700;"),
                  lapply(seq_along(strategy_points),function(i){
                    sp       <- strategy_points[i]
                    is_alert <- grepl("\U0001f525 HOT",sp)
                    is_cold  <- grepl("\u2744",sp)
                    is_danger<- grepl("DANGER|MISMATCH|outscore",sp)
                    bg_s <- if(is_alert)"#fef2f2;border:1px solid #fecaca;"
                    else if(is_cold)"#eff6ff;border:1px solid #bfdbfe;"
                    else if(is_danger)"#fffbeb;border:1px solid #fde68a;"
                    else "#f0fdf4;border:1px solid #bbf7d0;"
                    nb   <- if(is_alert)"#b91c1c" else if(is_cold)"#3b82f6"
                    else if(is_danger)"#d97706" else "#15803d"
                    tc_s <- if(is_alert)"#991b1b" else if(is_cold)"#1e40af"
                    else if(is_danger)"#92400e" else "#166534"
                    div(style=paste0("padding:10px 12px;border-radius:8px;margin-bottom:8px;",
                                     "display:flex;align-items:start;gap:10px;background:",bg_s),
                        div(style=paste0("width:24px;height:24px;border-radius:50%;flex-shrink:0;",
                                         "display:flex;align-items:center;justify-content:center;",
                                         "font-weight:800;font-size:0.75em;color:white;background:",nb,";"),i),
                        p(sp,style=paste0("margin:0;font-size:0.88em;font-weight:600;color:",tc_s,";")))
                  })
              ),
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                  h4(paste0("\u2694\ufe0f ",rv$my_team," vs ",opp_name),
                     style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  lapply(list(
                    list("Record",
                         paste0(if(nrow(kings_record)>0) paste0(kings_record$W[1],"-",kings_record$L[1]) else "N/A"),
                         paste0(opp_record$W[1],"-",opp_record$L[1]),
                         kings_wpct, opp_wpct),
                    list("PPG",    kings_ppg, opp_ppg, kings_ppg, opp_ppg),
                    list("RPG",    kings_rpg, opp_rpg, kings_rpg, opp_rpg),
                    list("APG",    kings_apg, opp_apg, kings_apg, opp_apg)
                  ),function(comp){
                    k_num <- as.numeric(comp[[4]]); o_num <- as.numeric(comp[[5]])
                    total <- k_num+o_num
                    k_pct <- if(total>0) round(k_num/total*100,0) else 50
                    k_wins_comp <- k_num>o_num
                    div(style="margin-bottom:10px;",
                        div(style="display:flex;justify-content:space-between;margin-bottom:4px;",
                            span(paste0(rv$my_team,": ",comp[[2]]),
                                 style=paste0("font-size:0.8em;font-weight:700;color:",
                                              if(k_wins_comp)"#15803d" else "#b91c1c",";")),
                            span(comp[[1]],style="font-size:0.75em;font-weight:600;color:#94a3b8;"),
                            span(paste0(comp[[3]]," :",opp_name),
                                 style=paste0("font-size:0.8em;font-weight:700;color:",
                                              if(!k_wins_comp)"#15803d" else "#b91c1c",";"))),
                        div(style="height:8px;border-radius:4px;display:flex;overflow:hidden;",
                            div(style=paste0("width:",k_pct,"%;background:",ohio_kings_primary,";")),
                            div(style=paste0("width:",100-k_pct,"%;background:#94a3b8;"))))
                  })
              )
          )
      ),
      
      # Auto-generated narrative
      div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                       "border-radius:12px;padding:24px;margin-top:20px;"),
          h4("\U0001f4dd Game Preview",style="margin:0 0 12px 0;opacity:0.8;letter-spacing:0.5px;"),
          p(paste(narrative_parts,collapse=" "),
            style="margin:0;line-height:1.8;font-size:0.95em;opacity:0.9;")
      ),
      
      # Footer
      div(style="text-align:center;margin-top:20px;padding:15px;color:#94a3b8;font-size:0.8em;",
          paste0("Generated ",format(Sys.time(),"%B %d, %Y at %I:%M %p"),
                 " | ",rv$my_team," Scouting Dashboard"),
          div(style="margin-top:8px;",
              actionButton("print_report","\U0001f5a8\ufe0f Print Report",
                           class="btn-sm",
                           style="background:#475569;color:white;border:none;padding:8px 20px;border-radius:8px;font-weight:600;",
                           onclick="window.print();"))
      )
    )
  })
  
  # ============================================================================
  # FIXES — paste this block BEFORE the closing  } # end server  line
  # It re-defines the broken outputs so they override the earlier definitions.
  # Broken outputs fixed: court_visualization, four_factors, situation_success,
  #                       player_matchups (predicted scores), scouting_report
  # ============================================================================
  
  # --------------------------------------------------------------------------
  # SAFE HELPER — pulls a stat from a roster safely (no name collision)
  # --------------------------------------------------------------------------
  .get_roster_stat <- function(roster, col) {
    tr <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
    if (nrow(tr) > 0 && col %in% names(tr))
      return(suppressWarnings(as.numeric(tr[[col]][1])))
    pr <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
    if (col %in% names(pr))
      return(sum(suppressWarnings(as.numeric(pr[[col]])), na.rm = TRUE))
    return(0)
  }
  
  # --------------------------------------------------------------------------
  # SAFE LINEUP HELPER — gets a numeric column from lineup with NA guard
  # --------------------------------------------------------------------------
  .lv <- function(lineup, col) {
    if (!col %in% names(lineup)) return(rep(0, nrow(lineup)))
    v <- suppressWarnings(as.numeric(lineup[[col]]))
    ifelse(is.na(v) | is.nan(v), 0, v)
  }
  
  # --------------------------------------------------------------------------
  # COURT VISUALIZATION — fixed
  # --------------------------------------------------------------------------
  output$court_visualization <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    a_col <- rv$team_cols$accent
    
    if (input$situation_select == "")
      return(p("Select a situation to see court visualization",
               style = "text-align:center;color:#999;padding:40px;"))
    
    kings_lineup <- tryCatch(
      build_lineup(rv$my_team, input$situation_select, rv$aba_data),
      error = function(e) NULL)
    
    if (is.null(kings_lineup) || nrow(kings_lineup) == 0)
      return(p("No lineup data available — ensure player stats have been loaded.",
               style = "text-align:center;color:#999;padding:40px;"))
    
    has_opp    <- !is.null(input$opponent_select) && input$opponent_select != ""
    opp_lineup <- NULL
    if (has_opp) {
      opp_crit <- switch(input$situation_select,
                         "off_down3"    = "need_stop", "off_down2"    = "need_stop",
                         "need_stop"    = "scoring",   "def_last"     = "scoring",
                         "scoring"      = "need_stop", "ft_situation" = "ft_situation",
                         input$situation_select)
      opp_lineup <- tryCatch(
        build_lineup(input$opponent_select, opp_crit, rv$aba_data),
        error = function(e) NULL)
    }
    
    # Apply custom bench swaps
    swaps         <- isolate(rv_custom_lineup$swaps)
    selected_slot <- isolate(rv_custom_lineup$selected_slot)
    
    if (length(swaps) > 0) {
      kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
      team_record  <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      gp           <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
      for (slot_str in names(swaps)) {
        slot_num  <- suppressWarnings(as.integer(slot_str))
        swap_name <- swaps[[slot_str]]
        if (!is.na(slot_num) && slot_num <= nrow(kings_lineup) && !is.null(swap_name)) {
          real_ppg <- 0; real_rpg <- 0; real_apg <- 0
          real_spg <- 0; real_bpg <- 0
          if (swap_name %in% names(rv$aba_data$all_player_stats)) {
            pg <- rv$aba_data$all_player_stats[[swap_name]]$per_game
            if (!is.null(pg) && nrow(pg) > 0) {
              real_ppg <- if (!is.na(pg$PPG[1])) pg$PPG[1] else 0
              real_rpg <- if (!is.na(pg$RPG[1])) pg$RPG[1] else 0
              real_apg <- if (!is.na(pg$APG[1])) pg$APG[1] else 0
              real_spg <- if (!is.na(pg$SPG[1])) pg$SPG[1] else 0
              real_bpg <- if (!is.na(pg$BPG[1])) pg$BPG[1] else 0
            }
          }
          if (real_ppg == 0 && swap_name %in% (kings_roster$Player %||% c())) {
            sr <- kings_roster %>% filter(Player == swap_name) %>% head(1)
            if (nrow(sr) > 0) {
              real_ppg <- round(suppressWarnings(as.numeric(sr$PTS[1])) / gp, 2)
              real_rpg <- round(suppressWarnings(as.numeric(sr$REB[1])) / gp, 2)
              real_apg <- round(suppressWarnings(as.numeric(sr$AST[1])) / gp, 2)
            }
          }
          kings_lineup$Player[slot_num]            <- swap_name
          kings_lineup$PPG[slot_num]               <- real_ppg
          kings_lineup$RPG[slot_num]               <- real_rpg
          kings_lineup$APG[slot_num]               <- real_apg
          kings_lineup$SPG[slot_num]               <- real_spg
          kings_lineup$BPG[slot_num]               <- real_bpg
          kings_lineup$team_percentile[slot_num]   <- 50
          kings_lineup$league_percentile[slot_num] <- 50
        }
      }
    }
    
    pos_kings <- list("PG"=list(x=22,y=78),"SG"=list(x=8,y=46),
                      "SF"=list(x=28,y=14),"PF"=list(x=38,y=58),"C"=list(x=34,y=36))
    pos_opp   <- list("PG"=list(x=78,y=78),"SG"=list(x=92,y=46),
                      "SF"=list(x=72,y=14),"PF"=list(x=62,y=58),"C"=list(x=66,y=36))
    
    shorten_name <- function(name) {
      parts <- strsplit(as.character(name), " ")[[1]]
      if (length(parts) >= 2) paste0(substr(parts[1],1,1),". ",paste(parts[-1],collapse=" ")) else name
    }
    pct_dot_color <- function(pct) {
      pct <- suppressWarnings(as.numeric(pct))
      if (is.na(pct)) return("#94a3b8")
      if (pct >= 75) "#22c55e" else if (pct >= 50) "#a3e635"
      else if (pct >= 25) "#fbbf24" else "#f87171"
    }
    safe_round <- function(x, digits = 1) {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v)) return("–") else round(v, digits)
    }
    
    # Get streak for a player
    get_streak <- function(pname) {
      tryCatch({
        pd <- rv$aba_data$all_player_stats[[pname]]
        if (is.null(pd$game_log) || nrow(pd$game_log) < 3 || is.null(pd$per_game))
          return(0)
        rec     <- pd$game_log %>% arrange(desc(date)) %>% head(3)
        rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)), na.rm = TRUE)
        szn_avg <- pd$per_game$PPG[1]
        if (is.na(rec_avg) || is.na(szn_avg) || szn_avg == 0) return(0)
        round(((rec_avg - szn_avg) / szn_avg) * 100, 1)
      }, error = function(e) 0)
    }
    
    make_court_card <- function(player, coords, is_kings = TRUE, slot_index = NULL) {
      accent_c  <- if (is_kings) p_col else "#991b1b"
      border_c  <- if (is_kings) p_col else "#dc2626"
      card_bg   <- if (is_kings) "rgba(255,255,255,0.93)" else "rgba(255,240,240,0.93)"
      is_sel    <- is_kings && !is.null(selected_slot) && !is.null(slot_index) &&
        selected_slot == slot_index
      onclick_a <- if (is_kings && !is.null(slot_index))
        paste0("Shiny.setInputValue('lineup_slot_clicked',", slot_index, ",{priority:'event'})") else ""
      
      ppg_v <- safe_round(player$PPG, 1)
      rpg_v <- safe_round(player$RPG, 1)
      apg_v <- safe_round(player$APG, 1)
      ss_v  <- safe_round(player$situation_score, 1)
      tp_v  <- suppressWarnings(as.numeric(player$team_percentile));   if (is.na(tp_v)) tp_v <- 50
      lp_v  <- suppressWarnings(as.numeric(player$league_percentile)); if (is.na(lp_v)) lp_v <- 50
      
      p_streak <- if (is_kings) get_streak(as.character(player$Player)) else 0
      
      div(style = paste0("position:absolute;left:", coords$x, "%;top:", coords$y,
                         "%;transform:translate(-50%,-50%);z-index:10;width:130px;"),
          div(style = paste0("background:", card_bg, ";border:",
                             if (is_sel) "3px" else "2px", " solid ",
                             if (is_sel) "#f59e0b" else border_c,
                             ";border-radius:12px;padding:8px 6px;text-align:center;",
                             "box-shadow:0 4px 15px rgba(0,0,0,0.25);cursor:",
                             if (is_kings) "pointer" else "default", ";"),
              onclick = onclick_a,
              # Position badge
              div(style = paste0("display:inline-block;background:", accent_c,
                                 ";color:white;font-weight:800;font-size:0.8em;",
                                 "padding:2px 10px;border-radius:8px;margin-bottom:4px;"),
                  as.character(player$lineup_position)),
              # Name
              div(style = "font-weight:700;font-size:0.75em;color:#1e293b;margin:3px 0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                  shorten_name(player$Player)),
              # Stats badges
              div(style = "display:flex;justify-content:center;gap:4px;margin:4px 0;",
                  span(paste0(ppg_v),
                       style = paste0("font-size:0.65em;font-weight:700;color:", p_col,
                                      ";background:#fee2e2;padding:1px 4px;border-radius:4px;")),
                  span(paste0(rpg_v),
                       style = "font-size:0.65em;font-weight:700;color:#1e40af;background:#dbeafe;padding:1px 4px;border-radius:4px;"),
                  span(paste0(apg_v),
                       style = "font-size:0.65em;font-weight:700;color:#b45309;background:#fef3c7;padding:1px 4px;border-radius:4px;")),
              div(style = "display:flex;justify-content:center;gap:6px;",
                  span("PPG", style = "font-size:0.5em;color:#94a3b8;font-weight:600;"),
                  span("RPG", style = "font-size:0.5em;color:#94a3b8;font-weight:600;"),
                  span("APG", style = "font-size:0.5em;color:#94a3b8;font-weight:600;")),
              # Situation score + streak
              div(style = "margin-top:4px;display:flex;justify-content:center;gap:6px;align-items:center;",
                  span(paste0("\u2b50 ", ss_v),
                       style = "font-size:0.6em;color:#475569;font-weight:600;"),
                  if (p_streak > 15)  span("\U0001f525", style = "font-size:0.7em;")
                  else if (p_streak < -15) span("\u2744\ufe0f", style = "font-size:0.7em;")
              ),
              # Team / League percentile bars
              div(style = "margin-top:4px;",
                  div(style = "display:flex;align-items:center;gap:2px;margin-bottom:2px;",
                      span("T", style = "font-size:0.5em;color:#94a3b8;font-weight:700;width:10px;"),
                      div(style = "flex:1;height:6px;background:#e2e8f0;border-radius:3px;overflow:hidden;",
                          div(style = paste0("width:", min(tp_v, 100), "%;height:100%;background:",
                                             pct_dot_color(tp_v), ";border-radius:3px;"))),
                      span(tp_v, style = "font-size:0.45em;color:#64748b;font-weight:700;width:16px;text-align:right;")),
                  div(style = "display:flex;align-items:center;gap:2px;",
                      span("L", style = "font-size:0.5em;color:#94a3b8;font-weight:700;width:10px;"),
                      div(style = "flex:1;height:6px;background:#e2e8f0;border-radius:3px;overflow:hidden;",
                          div(style = paste0("width:", min(lp_v, 100), "%;height:100%;background:",
                                             pct_dot_color(lp_v), ";border-radius:3px;"))),
                      span(lp_v, style = "font-size:0.45em;color:#64748b;font-weight:700;width:16px;text-align:right;"))
              ),
              if (is_sel)
                div(style = paste0("margin-top:4px;font-size:0.55em;font-weight:700;color:#f59e0b;"),
                    "\u2193 Tap bench to swap")
          )
      )
    }
    
    # Build bench
    bench_players <- data.frame()
    tryCatch({
      kings_roster_all <- rv$aba_data$all_team_stats[[rv$my_team]]
      if (!is.null(kings_roster_all)) {
        team_record <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
        gp_team     <- if (nrow(team_record) > 0) team_record$W[1] + team_record$L[1] else 15
        
        bench_players <- kings_roster_all %>%
          filter(!str_detect(toupper(Player), "TOTAL|TEAM"),
                 !Player %in% kings_lineup$Player) %>%
          mutate(
            GP_individual = sapply(Player, function(pl) {
              if (pl %in% names(rv$aba_data$all_player_stats)) {
                gl <- rv$aba_data$all_player_stats[[pl]]$game_log
                if (!is.null(gl)) return(nrow(gl))
              }; return(0)
            }),
            Position = sapply(Player, function(pl) {
              if (pl %in% names(rv$aba_data$all_player_stats)) {
                pos <- rv$aba_data$all_player_stats[[pl]]$position
                if (!is.null(pos) && !is.na(pos) && pos != "") return(pos)
              }; return("G")
            }),
            PPG = sapply(Player, function(pl) {
              if (pl %in% names(rv$aba_data$all_player_stats)) {
                pg <- rv$aba_data$all_player_stats[[pl]]$per_game
                if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$PPG[1])) return(pg$PPG[1])
              }; return(round(suppressWarnings(as.numeric(PTS)) / gp_team, 2))
            }),
            RPG = sapply(Player, function(pl) {
              if (pl %in% names(rv$aba_data$all_player_stats)) {
                pg <- rv$aba_data$all_player_stats[[pl]]$per_game
                if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$RPG[1])) return(pg$RPG[1])
              }; return(round(suppressWarnings(as.numeric(REB)) / gp_team, 2))
            }),
            APG = sapply(Player, function(pl) {
              if (pl %in% names(rv$aba_data$all_player_stats)) {
                pg <- rv$aba_data$all_player_stats[[pl]]$per_game
                if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$APG[1])) return(pg$APG[1])
              }; return(round(suppressWarnings(as.numeric(AST)) / gp_team, 2))
            })
          ) %>%
          filter(GP_individual >= 3) %>%
          mutate(PPG = ifelse(is.na(PPG) | is.nan(PPG), 0, PPG)) %>%
          arrange(desc(PPG)) %>%
          head(8)
      }
    }, error = function(e) { bench_players <<- data.frame() })
    
    is_slot_selected <- !is.null(selected_slot)
    
    div(
      # Court
      div(style = paste0("position:relative;width:100%;padding-bottom:52%;",
                         "background:linear-gradient(180deg,#1a5632 0%,#2d6a4f 50%,#1a5632 100%);",
                         "border-radius:14px;overflow:hidden;",
                         "box-shadow:0 8px 32px rgba(0,0,0,0.2),inset 0 0 60px rgba(0,0,0,0.15);"),
          HTML('<svg viewBox="0 0 1000 520" style="position:absolute;top:0;left:0;width:100%;height:100%;" preserveAspectRatio="xMidYMid meet">
            <rect x="15" y="15" width="970" height="490" fill="none" stroke="rgba(255,255,255,0.35)" stroke-width="2.5" rx="4"/>
            <line x1="500" y1="15" x2="500" y2="505" stroke="rgba(255,255,255,0.35)" stroke-width="2.5"/>
            <circle cx="500" cy="260" r="55" fill="none" stroke="rgba(255,255,255,0.35)" stroke-width="2.5"/>
            <circle cx="500" cy="260" r="4" fill="rgba(255,255,255,0.35)"/>
            <rect x="15" y="155" width="175" height="210" fill="rgba(200,16,46,0.08)" stroke="rgba(255,255,255,0.25)" stroke-width="2"/>
            <circle cx="190" cy="260" r="55" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5" stroke-dasharray="6,6"/>
            <path d="M 15 120 Q 260 260 15 400" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5"/>
            <circle cx="50" cy="260" r="10" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
            <rect x="15" y="250" width="25" height="20" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
            <rect x="810" y="155" width="175" height="210" fill="rgba(153,27,27,0.08)" stroke="rgba(255,255,255,0.25)" stroke-width="2"/>
            <circle cx="810" cy="260" r="55" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5" stroke-dasharray="6,6"/>
            <path d="M 985 120 Q 740 260 985 400" fill="none" stroke="rgba(255,255,255,0.2)" stroke-width="1.5"/>
            <circle cx="950" cy="260" r="10" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
            <rect x="960" y="250" width="25" height="20" fill="none" stroke="rgba(255,255,255,0.4)" stroke-width="2"/>
          </svg>'),
          div(style = paste0("position:absolute;top:10px;left:20px;color:rgba(255,255,255,0.6);",
                             "font-size:1.4em;font-weight:800;letter-spacing:2px;"),
              toupper(rv$my_team)),
          if (has_opp)
            div(style = "position:absolute;top:10px;right:20px;color:rgba(255,255,255,0.4);font-size:1.4em;font-weight:800;letter-spacing:2px;",
                toupper(input$opponent_select)),
          lapply(1:nrow(kings_lineup), function(i) {
            player <- kings_lineup[i, ]
            pos    <- as.character(player$lineup_position)
            coords <- if (pos %in% names(pos_kings)) pos_kings[[pos]] else list(x=25, y=50)
            make_court_card(player, coords, is_kings = TRUE, slot_index = i)
          }),
          if (!is.null(opp_lineup) && nrow(opp_lineup) > 0) {
            lapply(1:nrow(opp_lineup), function(i) {
              player <- opp_lineup[i, ]
              pos    <- as.character(player$lineup_position)
              coords <- if (pos %in% names(pos_opp)) pos_opp[[pos]] else list(x=75, y=50)
              make_court_card(player, coords, is_kings = FALSE)
            })
          }
      ),
      
      # Legend
      div(style = "display:flex;justify-content:center;gap:20px;margin-top:10px;padding:8px;",
          span("T = Team Rank",       style = "font-size:0.75em;color:#64748b;font-weight:600;"),
          span("L = League Rank",     style = "font-size:0.75em;color:#64748b;font-weight:600;"),
          span("\u2b50 = Situation Score", style = "font-size:0.75em;color:#64748b;font-weight:600;"),
          div(style = "display:flex;align-items:center;gap:4px;",
              div(style="width:10px;height:10px;border-radius:50%;background:#22c55e;"),
              span("75+", style="font-size:0.65em;color:#64748b;"),
              div(style="width:10px;height:10px;border-radius:50%;background:#a3e635;"),
              span("50+", style="font-size:0.65em;color:#64748b;"),
              div(style="width:10px;height:10px;border-radius:50%;background:#fbbf24;"),
              span("25+", style="font-size:0.65em;color:#64748b;"),
              div(style="width:10px;height:10px;border-radius:50%;background:#f87171;"),
              span("<25", style="font-size:0.65em;color:#64748b;"))
      ),
      
      # Bench
      div(style = "margin-top:15px;background:linear-gradient(180deg,#f8fafc,#f1f5f9);border-radius:12px;padding:16px;border:1px solid #e2e8f0;",
          div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
              h4(paste0("\U0001f4ba ", rv$my_team, " Bench"),
                 style = "margin:0;color:#334155;font-weight:700;"),
              if (is_slot_selected)
                span(paste0("\u2b05 Swapping slot ", selected_slot, " — click a bench player"),
                     style = paste0("font-size:0.85em;font-weight:700;color:", p_col, ";"))
              else
                span("Click a court player, then a bench player to swap",
                     style = "font-size:0.8em;color:#94a3b8;font-style:italic;")
          ),
          if (nrow(bench_players) > 0) {
            div(style = "display:flex;gap:10px;overflow-x:auto;padding-bottom:8px;",
                lapply(1:nrow(bench_players), function(j) {
                  bp <- bench_players[j, ]
                  div(
                    style = paste0("min-width:130px;background:white;border:2px solid ",
                                   if (is_slot_selected) p_col else "#e2e8f0",
                                   ";border-radius:10px;padding:10px 8px;text-align:center;",
                                   "cursor:", if (is_slot_selected) "pointer" else "default",
                                   ";transition:all 0.2s;flex-shrink:0;"),
                    onclick = if (is_slot_selected)
                      paste0("Shiny.setInputValue('swap_player',{slot:", selected_slot,
                             ",player:'", gsub("'", "\\\\'", as.character(bp$Player)),
                             "'},{priority:'event'})") else "",
                    span(as.character(bp$Position),
                         style = paste0("display:inline-block;background:", s_col,
                                        ";color:white;font-size:0.65em;font-weight:700;padding:1px 6px;border-radius:6px;")),
                    div(style = "font-weight:700;font-size:0.75em;color:#1e293b;margin:5px 0 3px 0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                        {parts <- strsplit(as.character(bp$Player)," ")[[1]]; if(length(parts)>=2) paste0(substr(parts[1],1,1),". ",paste(parts[-1],collapse=" ")) else as.character(bp$Player)}),
                    div(style = "display:flex;justify-content:center;gap:3px;margin:3px 0;",
                        span(paste0(round(bp$PPG,1)),
                             style = paste0("font-size:0.6em;font-weight:700;color:", p_col, ";background:#fee2e2;padding:1px 3px;border-radius:3px;")),
                        span(paste0(round(bp$RPG,1)),
                             style = "font-size:0.6em;font-weight:700;color:#1e40af;background:#dbeafe;padding:1px 3px;border-radius:3px;"),
                        span(paste0(round(bp$APG,1)),
                             style = "font-size:0.6em;font-weight:700;color:#b45309;background:#fef3c7;padding:1px 3px;border-radius:3px;")),
                    if (is_slot_selected)
                      div(style = paste0("margin-top:5px;font-size:0.65em;font-weight:700;color:", p_col, ";"),
                          "\u21bb Tap to swap")
                  )
                })
            )
          } else {
            p("No bench players available", style = "color:#94a3b8;text-align:center;")
          }
      )
    )
  })
  
  # --------------------------------------------------------------------------
  # FOUR FACTORS — fixed (no function name conflict, full SVG gauges)
  # --------------------------------------------------------------------------
  output$four_factors <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    a_col <- rv$team_cols$accent
    
    kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
    if (is.null(kings_roster) || nrow(kings_roster) == 0)
      return(p("No roster data available", style = "text-align:center;color:#999;padding:20px;"))
    
    total_2pm  <- .get_roster_stat(kings_roster, "2PM")
    total_2pa  <- .get_roster_stat(kings_roster, "2PA")
    total_3pm  <- .get_roster_stat(kings_roster, "3PM")
    total_3pa  <- .get_roster_stat(kings_roster, "3PA")
    total_fgm  <- total_2pm + total_3pm
    total_fga  <- total_2pa + total_3pa
    total_fta  <- .get_roster_stat(kings_roster, "FTA")
    total_ftm  <- .get_roster_stat(kings_roster, "FTM")
    total_oreb <- .get_roster_stat(kings_roster, "OREB")
    total_dreb <- .get_roster_stat(kings_roster, "DREB")
    
    efg      <- if (total_fga > 0) round(min((total_fgm + 0.5*total_3pm)/total_fga*100, 100), 1) else 0
    tov_pct  <- 12
    oreb_pct <- if ((total_oreb+total_dreb) > 0) round(total_oreb/(total_oreb+total_dreb)*100, 1) else 0
    ft_rate  <- if (total_fga > 0) round(total_fta/total_fga, 2) else 0
    
    efg_grade  <- if(efg  >= 55)"A" else if(efg  >= 50)"B" else if(efg  >= 45)"C" else "D"
    tov_grade  <- if(tov_pct <= 12)"A" else if(tov_pct <= 15)"B" else if(tov_pct <= 18)"C" else "D"
    oreb_grade <- if(oreb_pct >= 35)"A" else if(oreb_pct >= 30)"B" else if(oreb_pct >= 25)"C" else "D"
    ftr_grade  <- if(ft_rate >= 0.35)"A" else if(ft_rate >= 0.25)"B" else if(ft_rate >= 0.18)"C" else "D"
    
    grade_color_fn <- function(g) switch(g,"A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c","#64748b")
    grade_bg_fn    <- function(g) switch(g,
                                         "A"="linear-gradient(135deg,#15803d,#22c55e)",
                                         "B"="linear-gradient(135deg,#65a30d,#a3e635)",
                                         "C"="linear-gradient(135deg,#d97706,#fbbf24)",
                                         "D"="linear-gradient(135deg,#b91c1c,#f87171)",
                                         "linear-gradient(135deg,#64748b,#94a3b8)")
    
    make_gauge_box <- function(value, label, grade, description, fill_pct) {
      fill_pct  <- max(0, min(100, fill_pct))
      cx <- 60; cy <- 60; r <- 45
      to_rad <- function(deg) deg * pi / 180
      start_angle <- 135; total_sweep <- 270
      fill_angle  <- start_angle + (fill_pct/100) * total_sweep
      bg_x1  <- cx + r * cos(to_rad(start_angle));        bg_y1  <- cy + r * sin(to_rad(start_angle))
      bg_x2  <- cx + r * cos(to_rad(start_angle+total_sweep)); bg_y2 <- cy + r * sin(to_rad(start_angle+total_sweep))
      fill_x2 <- cx + r * cos(to_rad(fill_angle));        fill_y2 <- cy + r * sin(to_rad(fill_angle))
      bg_large_arc   <- 1
      fill_large_arc <- if ((fill_pct/100)*total_sweep > 180) 1 else 0
      gc <- grade_color_fn(grade)
      
      div(style = paste0("background:linear-gradient(180deg,#ffffff,#fafafa);border-radius:16px;",
                         "padding:20px;text-align:center;box-shadow:0 4px 16px rgba(0,0,0,0.06);",
                         "border:2px solid ", gc, "25;position:relative;"),
          div(style = paste0("position:absolute;top:10px;right:10px;width:36px;height:36px;",
                             "background:", grade_bg_fn(grade), ";border-radius:50%;",
                             "display:flex;align-items:center;justify-content:center;",
                             "font-weight:800;font-size:0.95em;color:",
                             if (grade %in% c("B","C")) "#1a1a2e" else "white", ";"),
              grade),
          HTML(paste0(
            '<svg width="120" height="100" viewBox="0 0 120 100" style="margin:0 auto;display:block;">',
            '<path d="M ', round(bg_x1,2), ' ', round(bg_y1,2),
            ' A ', r, ' ', r, ' 0 ', bg_large_arc, ' 1 ',
            round(bg_x2,2), ' ', round(bg_y2,2), '"',
            ' fill="none" stroke="#e2e8f0" stroke-width="10" stroke-linecap="round"/>',
            if (fill_pct > 1)
              paste0('<path d="M ', round(bg_x1,2), ' ', round(bg_y1,2),
                     ' A ', r, ' ', r, ' 0 ', fill_large_arc, ' 1 ',
                     round(fill_x2,2), ' ', round(fill_y2,2), '"',
                     ' fill="none" stroke="', gc, '" stroke-width="10" stroke-linecap="round"/>'),
            '</svg>'
          )),
          h3(value, style = paste0("margin:5px 0 0 0;font-size:1.8em;font-weight:800;color:", gc, ";")),
          p(label, style = "margin:5px 0 0 0;color:#475569;font-weight:600;font-size:0.95em;"),
          p(description, style = "margin:4px 0 0 0;color:#94a3b8;font-size:0.78em;"),
          p(paste0(round(fill_pct, 0), "th %ile league-wide"),
            style = paste0("margin:6px 0 0 0;font-size:0.72em;font-weight:600;color:", gc, ";opacity:0.8;"))
      )
    }
    
    efg_fill  <- min(100, max(0, (efg - 35)        / (65 - 35)       * 100))
    tov_fill  <- min(100, max(0, (25 - tov_pct)    / (25 - 5)        * 100))
    oreb_fill <- min(100, max(0, (oreb_pct - 15)   / (45 - 15)       * 100))
    ftr_fill  <- min(100, max(0, (ft_rate - 0.10)  / (0.45 - 0.10)   * 100))
    
    fluidRow(
      column(3, make_gauge_box(paste0(efg, "%"),     "eFG%",     "Shooting efficiency",  efg_grade,  efg_fill)),
      column(3, make_gauge_box(paste0(tov_pct, "%"), "TOV%",     "Lower is better",      tov_grade,  tov_fill)),
      column(3, make_gauge_box(paste0(oreb_pct,"%"), "OREB%",    "Offensive boards",     oreb_grade, oreb_fill)),
      column(3, make_gauge_box(ft_rate,              "FT Rate",  "Getting to the line",  ftr_grade,  ftr_fill))
    )
  })
  
  # --------------------------------------------------------------------------
  # SITUATION SUCCESS — fixed (robust column access, no crash on missing cols)
  # --------------------------------------------------------------------------
  output$situation_success <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    a_col <- rv$team_cols$accent
    
    if (input$situation_select == "")
      return(p("Select a situation to see success prediction",
               style = "text-align:center;color:#999;padding:20px;"))
    
    kings_lineup <- tryCatch(
      build_lineup(rv$my_team, input$situation_select, rv$aba_data),
      error = function(e) NULL)
    
    if (is.null(kings_lineup) || nrow(kings_lineup) == 0)
      return(p("No lineup data available", style = "text-align:center;color:#999;padding:20px;"))
    
    # Apply swaps from reactive state
    swaps <- isolate(rv_custom_lineup$swaps)
    if (length(swaps) > 0) {
      for (slot_str in names(swaps)) {
        slot_num  <- suppressWarnings(as.integer(slot_str))
        swap_name <- swaps[[slot_str]]
        if (!is.na(slot_num) && slot_num <= nrow(kings_lineup) && !is.null(swap_name) &&
            swap_name %in% names(rv$aba_data$all_player_stats)) {
          pg <- rv$aba_data$all_player_stats[[swap_name]]$per_game
          if (!is.null(pg) && nrow(pg) > 0) {
            kings_lineup$Player[slot_num]  <- swap_name
            kings_lineup$PPG[slot_num]     <- if (!is.na(pg$PPG[1])) pg$PPG[1] else 0
            kings_lineup$RPG[slot_num]     <- if (!is.na(pg$RPG[1])) pg$RPG[1] else 0
            kings_lineup$APG[slot_num]     <- if (!is.na(pg$APG[1])) pg$APG[1] else 0
            kings_lineup$SPG[slot_num]     <- if (!is.na(pg$SPG[1])) pg$SPG[1] else 0
            kings_lineup$BPG[slot_num]     <- if (!is.na(pg$BPG[1])) pg$BPG[1] else 0
            if ("FG2_pct" %in% names(kings_lineup) && !is.na(pg$FG2_pct[1]))
              kings_lineup$FG2_pct[slot_num] <- pg$FG2_pct[1]
            if ("FG3_pct" %in% names(kings_lineup) && !is.na(pg$FG3_pct[1]))
              kings_lineup$FG3_pct[slot_num] <- pg$FG3_pct[1]
            if ("FT_pct" %in% names(kings_lineup) && !is.na(pg$FT_pct[1]))
              kings_lineup$FT_pct[slot_num] <- pg$FT_pct[1]
          }
        }
      }
    }
    
    # Safe column extraction
    ppgs <- .lv(kings_lineup, "PPG")
    rpgs <- .lv(kings_lineup, "RPG")
    apgs <- .lv(kings_lineup, "APG")
    spgs <- .lv(kings_lineup, "SPG")
    bpgs <- .lv(kings_lineup, "BPG")
    fg2s <- .lv(kings_lineup, "FG2_pct")
    fg3s <- .lv(kings_lineup, "FG3_pct")
    fts  <- .lv(kings_lineup, "FT_pct")
    
    total_ppg_l  <- sum(ppgs)
    shot_weights <- if (total_ppg_l > 0) ppgs / total_ppg_l else rep(0.2, length(ppgs))
    primary_idx  <- which.max(shot_weights)
    primary_pl   <- as.character(kings_lineup$Player[primary_idx])
    primary_wt   <- round(shot_weights[primary_idx] * 100, 0)
    
    # Opponent defensive impact
    opp_def_factor <- 1.0
    has_opp_ss <- !is.null(input$opponent_select) && input$opponent_select != ""
    opp_lineup_ss <- NULL
    if (has_opp_ss) {
      opp_crit <- switch(input$situation_select,
                         "off_down3"="need_stop","off_down2"="need_stop","need_stop"="scoring",
                         "def_last"="scoring","scoring"="need_stop","ft_situation"="ft_situation",
                         input$situation_select)
      opp_lineup_ss <- tryCatch(build_lineup(input$opponent_select, opp_crit, rv$aba_data),
                                error = function(e) NULL)
      if (!is.null(opp_lineup_ss) && nrow(opp_lineup_ss) > 0) {
        opp_spg <- sum(.lv(opp_lineup_ss, "SPG"))
        opp_bpg <- sum(.lv(opp_lineup_ss, "BPG"))
        opp_def_factor <- max(0.85, 1 - (opp_spg + opp_bpg - 3) * 0.025)
      }
    }
    
    criteria <- input$situation_select
    situation_data <- tryCatch({
      switch(criteria,
             "off_down3" = {
               three_attempts <- sapply(as.character(kings_lineup$Player), function(pl) {
                 tryCatch({
                   gl <- rv$aba_data$all_player_stats[[pl]]$game_log
                   if (!is.null(gl) && "3PA" %in% names(gl))
                     sum(suppressWarnings(as.numeric(gl$`3PA`)), na.rm = TRUE) else 0
                 }, error = function(e) 0)
               })
               min_3pa <- 15; league_avg_3 <- 32
               adj_3pcts <- ifelse(three_attempts >= min_3pa, fg3s,
                                   ifelse(three_attempts == 0, 0,
                                          (fg3s*three_attempts + league_avg_3*min_3pa) / (three_attempts + min_3pa)))
               raw_pct <- sum(shot_weights * adj_3pcts)
               single_poss_pct <- round(raw_pct * opp_def_factor, 1)
               reliable_idx <- which(three_attempts >= min_3pa & fg3s > 0)
               best_3_idx <- if (length(reliable_idx) > 0) reliable_idx[which.max(fg3s[reliable_idx])] else which.max(three_attempts)
               best_3_pl <- as.character(kings_lineup$Player[best_3_idx]); best_3_pct <- fg3s[best_3_idx]
               optimal_pct <- round(best_3_pct, 1); improvement <- round(optimal_pct - single_poss_pct, 1)
               list(title="Down 3 — One Possession to Hit a Three",
                    primary_stat=paste0(single_poss_pct,"%"),
                    primary_label="Probability of hitting a 3 (weighted by shot distribution)",
                    secondary_stat=paste0(optimal_pct,"%"),
                    secondary_label=paste0("If drawn for ",best_3_pl," (",round(best_3_pct,1),"% 3P%)"),
                    detail=paste0("\U0001f3af Primary option: ",primary_pl," (~",primary_wt,"% shot share)"),
                    detail2=if(improvement>2) paste0("\u2b06\ufe0f +",improvement,"% boost if you go to ",best_3_pl) else NULL,
                    vs_league="League avg 3P%: ~32%", vs_diff=single_poss_pct-32,
                    gauge_pct=min(100,max(0,(single_poss_pct-15)/(50-15)*100)))
             },
             "off_down2" = {
               raw_pct <- sum(shot_weights * fg2s)
               single_poss_pct <- round(raw_pct * opp_def_factor, 1)
               best_2_idx <- which.max(fg2s); best_2_pl <- as.character(kings_lineup$Player[best_2_idx])
               optimal_pct <- round(fg2s[best_2_idx], 1); improvement <- round(optimal_pct - single_poss_pct, 1)
               list(title="Down 2 — One Possession to Tie or Take the Lead",
                    primary_stat=paste0(single_poss_pct,"%"),
                    primary_label="Probability of hitting a 2 (weighted by shot distribution)",
                    secondary_stat=paste0(optimal_pct,"%"),
                    secondary_label=paste0("If play drawn for ",best_2_pl," (best 2P shooter)"),
                    detail=paste0("\U0001f3af Primary option: ",primary_pl," (",round(fg2s[primary_idx],1),"% from 2, ~",primary_wt,"% share)"),
                    detail2=if(improvement>2) paste0("\u2b06\ufe0f +",improvement,"% boost to ",best_2_pl) else NULL,
                    vs_league="League avg 2P%: ~47%", vs_diff=single_poss_pct-47,
                    gauge_pct=min(100,max(0,(single_poss_pct-30)/(65-30)*100)))
             },
             "ft_situation" = {
               fouled_idx  <- primary_idx; fouled_pl <- as.character(kings_lineup$Player[fouled_idx])
               fouled_ft   <- fts[fouled_idx]
               both_fts    <- round((fouled_ft/100)^2 * 100, 1)
               at_least_one <- round((1-((1-fouled_ft/100)^2)) * 100, 1)
               best_ft_idx <- which.max(fts); best_ft_pl <- as.character(kings_lineup$Player[best_ft_idx])
               best_ft_pct <- fts[best_ft_idx]; best_both <- round((best_ft_pct/100)^2*100,1)
               worst_nz <- which(fts > 0)
               worst_ft_idx <- if (length(worst_nz) > 0) worst_nz[which.min(fts[worst_nz])] else 1
               worst_ft_pl <- as.character(kings_lineup$Player[worst_ft_idx]); worst_ft_pct <- fts[worst_ft_idx]
               list(title="Free Throw Situation — Most Likely Player Fouled",
                    primary_stat=paste0(both_fts,"%"),
                    primary_label=paste0("Prob ",fouled_pl," makes BOTH FTs (",fouled_ft,"% shooter)"),
                    secondary_stat=paste0(at_least_one,"%"),
                    secondary_label="Prob makes at least 1 of 2",
                    detail=paste0("\U0001f3af Best FT option: ",best_ft_pl," (",best_ft_pct,"% → ",best_both,"% both)"),
                    detail2=paste0("\u26a0\ufe0f Protect ",worst_ft_pl," (",worst_ft_pct,"% FT) — don't let them get fouled"),
                    vs_league="Good FT%: 75%+", vs_diff=fouled_ft-75,
                    gauge_pct=min(100,max(0,(fouled_ft-40)/(90-40)*100)))
             },
             "need_stop" = {
               def_plays_pg <- sum(spgs) + sum(bpgs)
               stop_prob    <- round(min(95, (def_plays_pg/70)*100), 1)
               base_miss    <- 55; def_boost <- min(10, def_plays_pg*1.5)
               opp_penalty  <- 0
               if (!is.null(opp_lineup_ss) && nrow(opp_lineup_ss) > 0) {
                 opp_total_ppg <- sum(.lv(opp_lineup_ss,"PPG"))
                 opp_penalty   <- max(-8, min(0, (80-opp_total_ppg)*0.15))
               }
               adj_miss <- round(base_miss + def_boost + opp_penalty, 1)
               best_def_idx <- which.max(spgs + bpgs)
               best_def_pl  <- as.character(kings_lineup$Player[best_def_idx])
               best_def_val <- round(spgs[best_def_idx] + bpgs[best_def_idx], 1)
               list(title="Need a Stop — One Defensive Possession",
                    primary_stat=paste0(adj_miss,"%"),
                    primary_label="Estimated probability opponent misses this possession",
                    secondary_stat=paste0(stop_prob,"%"),
                    secondary_label="Chance of forced turnover or block",
                    detail=paste0("\U0001f6e1\ufe0f Defensive anchor: ",best_def_pl," (",best_def_val," STL+BLK per game)"),
                    detail2=paste0("\U0001f3c0 Combined ",round(sum(rpgs),1)," RPG — board presence if they miss"),
                    vs_league=paste0(round(def_plays_pg,1)," STL+BLK per game"), vs_diff=def_plays_pg-4,
                    gauge_pct=min(100,max(0,(adj_miss-45)/(75-45)*100)))
             },
             "def_last" = {
               total_rpg_l <- sum(rpgs); total_bpg_l <- sum(bpgs)
               reb_share   <- round(min(90, total_rpg_l/45*100), 1)
               contest_rate <- round(min(40, total_bpg_l/0.7*10), 1)
               best_reb_idx <- which.max(rpgs)
               best_rebounder <- as.character(kings_lineup$Player[best_reb_idx])
               list(title="Defense — Last Possession",
                    primary_stat=paste0(reb_share,"%"),
                    primary_label="Estimated rebound probability if opponent misses",
                    secondary_stat=paste0(contest_rate,"%"),
                    secondary_label="Shot contest/block probability",
                    detail=paste0("\U0001f3c0 Key rebounder: ",best_rebounder," (",rpgs[best_reb_idx]," RPG)"),
                    detail2=paste0("Combined ",round(total_bpg_l,1)," BPG — ",
                                   if(total_bpg_l>=2)"strong rim protection" else "limited shot blocking"),
                    vs_league=paste0("Combined ",round(total_rpg_l,1)," RPG"), vs_diff=total_rpg_l-35,
                    gauge_pct=min(100,max(0,(reb_share-40)/(90-40)*100)))
             },
             "scoring" = {
               weighted_fg  <- round(sum(shot_weights * ((fg2s+fg3s)/2)), 1)
               expected_pps <- round(sum(shot_weights * (fg2s/100*2 + fg3s/100*3*0.3)), 2)
               list(title="Pure Scoring — Maximum Offensive Output",
                    primary_stat=round(total_ppg_l,1),
                    primary_label="Combined lineup PPG",
                    secondary_stat=paste0(expected_pps," pts"),
                    secondary_label="Expected points per possession",
                    detail=paste0("\U0001f525 Primary scorer: ",primary_pl," (",ppgs[primary_idx]," PPG, ~",primary_wt,"% usage)"),
                    detail2=paste0("Lineup weighted FG%: ",weighted_fg,"%"),
                    vs_league="Good offense: 1.0+ pts/poss", vs_diff=expected_pps-1.0,
                    gauge_pct=min(100,max(0,(total_ppg_l-50)/(120-50)*100)))
             },
             NULL
      )
    }, error = function(e) NULL)
    
    if (is.null(situation_data))
      return(p("Unable to calculate for selected situation.",
               style = "text-align:center;color:#999;padding:20px;"))
    
    gauge_pct <- situation_data$gauge_pct
    grade     <- if(gauge_pct>=75)"A" else if(gauge_pct>=50)"B" else if(gauge_pct>=25)"C" else "D"
    grade_col <- switch(grade,"A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c")
    grade_bg_s <- switch(grade,
                         "A"="linear-gradient(135deg,#15803d,#22c55e)","B"="linear-gradient(135deg,#65a30d,#a3e635)",
                         "C"="linear-gradient(135deg,#d97706,#fbbf24)","D"="linear-gradient(135deg,#b91c1c,#f87171)")
    vs_color <- if(situation_data$vs_diff>0)"#15803d" else if(situation_data$vs_diff<0)"#b91c1c" else "#64748b"
    
    # Momentum
    get_streak_ss <- function(pname) tryCatch({
      pd <- rv$aba_data$all_player_stats[[pname]]
      if (is.null(pd$game_log)||nrow(pd$game_log)<3||is.null(pd$per_game)) return(0)
      rec <- pd$game_log %>% arrange(desc(date)) %>% head(3)
      rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)),na.rm=TRUE)
      szn_avg <- pd$per_game$PPG[1]
      if (is.na(rec_avg)||is.na(szn_avg)||szn_avg==0) return(0)
      round(((rec_avg-szn_avg)/szn_avg)*100,1)
    }, error=function(e) 0)
    
    streaks   <- sapply(as.character(kings_lineup$Player), get_streak_ss)
    hot_count <- sum(streaks > 15); cold_count <- sum(streaks < -15)
    avg_streak <- round(mean(streaks), 1)
    mom_label  <- if(avg_streak>10)"\U0001f525 On Fire" else if(avg_streak>0)"\u2b06\ufe0f Trending Up"
    else if(avg_streak>-10)"\u27a1\ufe0f Steady" else "\u2744\ufe0f Cold Stretch"
    mom_color  <- if(avg_streak>10)"#15803d" else if(avg_streak>0)"#65a30d"
    else if(avg_streak>-10)"#d97706" else "#b91c1c"
    
    div(
      div(style="display:flex;gap:20px;flex-wrap:wrap;",
          div(style=paste0("flex:1;min-width:280px;background:white;border:2px solid ",grade_col,"25;",
                           "border-radius:14px;padding:24px;text-align:center;position:relative;",
                           "box-shadow:0 4px 16px rgba(0,0,0,0.06);"),
              div(style=paste0("position:absolute;top:12px;right:12px;width:40px;height:40px;",
                               "background:",grade_bg_s,";border-radius:50%;display:flex;",
                               "align-items:center;justify-content:center;font-weight:800;",
                               "font-size:1.1em;color:",if(grade%in%c("B","C"))"#1a1a2e" else "white",";"),
                  grade),
              h4(situation_data$title,style="margin:0 0 15px 0;color:#334155;font-weight:700;"),
              h2(situation_data$primary_stat,style=paste0("margin:0;font-size:3.5em;font-weight:800;color:",grade_col,";")),
              p(situation_data$primary_label,style="margin:5px 0 0 0;color:#64748b;font-weight:600;"),
              div(style="margin:15px auto;max-width:250px;",
                  div(style="height:10px;background:#e2e8f0;border-radius:5px;overflow:hidden;",
                      div(style=paste0("width:",gauge_pct,"%;height:100%;background:",grade_col,";border-radius:5px;"))),
                  div(style="display:flex;justify-content:space-between;margin-top:4px;",
                      span("Poor",style="font-size:0.65em;color:#94a3b8;"),
                      span("Elite",style="font-size:0.65em;color:#94a3b8;"))),
              div(style="margin-top:10px;",
                  span(situation_data$vs_league,style="font-size:0.85em;color:#64748b;"),
                  span(paste0(" (",ifelse(situation_data$vs_diff>0,"+",""),
                              round(situation_data$vs_diff,1),")"),
                       style=paste0("font-size:0.85em;font-weight:700;color:",vs_color,";margin-left:5px;")))
          ),
          div(style="flex:1;min-width:280px;display:flex;flex-direction:column;gap:12px;",
              div(style="background:#f8fafc;border-radius:12px;padding:18px;border:1px solid #e2e8f0;",
                  h3(situation_data$secondary_stat,
                     style=paste0("margin:0;font-size:1.8em;font-weight:800;color:",s_col,";")),
                  p(situation_data$secondary_label,style="margin:4px 0 0 0;color:#64748b;font-size:0.85em;")),
              div(style=paste0("background:",grade_col,"10;border-left:4px solid ",grade_col,
                               ";border-radius:8px;padding:14px;"),
                  p(situation_data$detail,style="margin:0;color:#334155;font-weight:600;font-size:0.95em;"),
                  if (!is.null(situation_data$detail2))
                    p(situation_data$detail2,style="margin:8px 0 0 0;color:#475569;font-weight:600;font-size:0.88em;")),
              if (has_opp_ss && !is.null(opp_lineup_ss) && nrow(opp_lineup_ss) > 0) {
                opp_ppg_total <- sum(.lv(opp_lineup_ss, "PPG"))
                kings_on_off  <- criteria %in% c("off_down3","off_down2","scoring","ft_situation")
                div(style="background:#fef2f2;border:1px solid #fecaca;border-radius:10px;padding:14px;",
                    div(style="display:flex;justify-content:space-between;align-items:center;",
                        div(div(style="display:flex;align-items:center;gap:6px;",
                                span("\u26a0\ufe0f",style="font-size:1.2em;"),
                                span(paste0("vs ",input$opponent_select),
                                     style="font-weight:700;color:#991b1b;font-size:0.95em;")),
                            div(if(kings_on_off)"Defending (defensive lineup)" else "Attacking (offensive lineup)",
                                style="font-size:0.8em;color:#64748b;margin-top:2px;")),
                        div(paste0(round(opp_ppg_total,1)," combined PPG"),
                            style="font-weight:700;color:#991b1b;font-size:0.9em;"))
                )
              },
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:14px;",
                  div(style="display:flex;justify-content:space-between;align-items:center;",
                      div(span("Lineup Momentum",style="font-weight:600;color:#334155;font-size:0.9em;"),
                          div(paste0(hot_count," hot / ",cold_count," cold / ",
                                     5-hot_count-cold_count," steady"),
                              style="margin-top:4px;font-size:0.8em;color:#64748b;")),
                      div(style="text-align:right;",
                          div(mom_label,style=paste0("font-weight:700;font-size:1.1em;color:",mom_color,";")),
                          div(paste0(ifelse(avg_streak>0,"+",""),avg_streak,"% avg"),
                              style=paste0("font-size:0.8em;font-weight:600;color:",mom_color,";"))))
              )
          )
      )
    )
  })
  
  # --------------------------------------------------------------------------
  # PLAYER MATCHUPS — fixed (predicted scores properly shown)
  # --------------------------------------------------------------------------
  output$player_matchups <- renderUI({
    req(rv$logged_in, input$situation_select, input$opponent_select, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    a_col <- rv$team_cols$accent
    
    if (input$situation_select == "" || input$opponent_select == "")
      return(p("Select a situation and opponent to see player matchup predictions",
               style = "text-align:center;color:#999;padding:20px;"))
    
    kings_lineup <- tryCatch(build_lineup(rv$my_team,            "scoring", rv$aba_data), error=function(e) NULL)
    opp_lineup   <- tryCatch(build_lineup(input$opponent_select, "scoring", rv$aba_data), error=function(e) NULL)
    
    if (is.null(kings_lineup)||is.null(opp_lineup)||nrow(kings_lineup)==0||nrow(opp_lineup)==0)
      return(p("Lineup data not available for matchup analysis",
               style = "text-align:center;color:#999;padding:20px;"))
    
    n_matchups <- min(nrow(kings_lineup), nrow(opp_lineup))
    
    adj <- function(a, b, k=0.15) 100 / (1 + exp(-k*(a-b)))
    
    all_edges <- sapply(1:n_matchups, function(i) {
      k <- kings_lineup[i,]; o <- opp_lineup[i,]
      adj(.lv(k,"PPG"),.lv(o,"PPG"))*0.40 +
        adj(.lv(k,"RPG"),.lv(o,"RPG"),0.3)*0.25 +
        adj(.lv(k,"APG"),.lv(o,"APG"),0.3)*0.20 +
        adj(.lv(k,"SPG")+.lv(k,"BPG"),.lv(o,"SPG")+.lv(o,"BPG"),0.5)*0.15
    })
    best_idx  <- which.max(all_edges)
    worst_idx <- which.min(all_edges)
    
    div(
      # Exploit/Watch callout
      div(style="display:flex;gap:12px;margin-bottom:15px;",
          div(style="flex:1;background:#f0fdf4;border:1px solid #bbf7d0;border-radius:10px;padding:14px;",
              div(style="display:flex;align-items:center;gap:8px;",
                  span("\u2705",style="font-size:1.3em;"),
                  div(div("EXPLOIT THIS",style="font-weight:800;font-size:0.75em;color:#15803d;letter-spacing:0.5px;"),
                      div(paste0(kings_lineup$lineup_position[best_idx],": ",
                                 kings_lineup$Player[best_idx]," has ",round(all_edges[best_idx],1),
                                 "% edge vs ",opp_lineup$Player[best_idx]),
                          style="font-weight:600;font-size:0.9em;color:#166534;margin-top:2px;")))),
          div(style="flex:1;background:#fef2f2;border:1px solid #fecaca;border-radius:10px;padding:14px;",
              div(style="display:flex;align-items:center;gap:8px;",
                  span("\u26a0\ufe0f",style="font-size:1.3em;"),
                  div(div("WATCH OUT",style="font-weight:800;font-size:0.75em;color:#b91c1c;letter-spacing:0.5px;"),
                      div(paste0(kings_lineup$lineup_position[worst_idx],": ",
                                 opp_lineup$Player[worst_idx]," has ",round(100-all_edges[worst_idx],1),
                                 "% edge vs ",kings_lineup$Player[worst_idx]),
                          style="font-weight:600;font-size:0.9em;color:#991b1b;margin-top:2px;")))
          )
      ),
      
      lapply(1:n_matchups, function(i) {
        k <- kings_lineup[i,]; o <- opp_lineup[i,]
        
        k_ppg <- .lv(k,"PPG"); k_rpg <- .lv(k,"RPG"); k_apg <- .lv(k,"APG")
        k_spg <- .lv(k,"SPG"); k_bpg <- .lv(k,"BPG")
        o_ppg <- .lv(o,"PPG"); o_rpg <- .lv(o,"RPG"); o_apg <- .lv(o,"APG")
        o_spg <- .lv(o,"SPG"); o_bpg <- .lv(o,"BPG")
        
        sc_adv <- adj(k_ppg, o_ppg, 0.15)
        rb_adv <- adj(k_rpg, o_rpg, 0.3)
        as_adv <- adj(k_apg, o_apg, 0.3)
        df_adv <- adj(k_spg+k_bpg, o_spg+o_bpg, 0.5)
        overall <- round(sc_adv*0.40 + rb_adv*0.25 + as_adv*0.20 + df_adv*0.15, 1)
        opp_adv <- round(100-overall, 1)
        winner  <- if(overall>55)"kings" else if(overall<45)"opp" else "even"
        
        # Predicted stats for this game
        k_pred <- tryCatch(predict_player_game(as.character(k$Player), input$opponent_select),
                           error=function(e) list(pts=NA,reb=NA,ast=NA))
        o_pred <- tryCatch(predict_player_game(as.character(o$Player), rv$my_team),
                           error=function(e) list(pts=NA,reb=NA,ast=NA))
        
        k_pred_pts <- if (!is.null(k_pred$pts) && !is.na(k_pred$pts)) k_pred$pts else round(k_ppg,1)
        k_pred_reb <- if (!is.null(k_pred$reb) && !is.na(k_pred$reb)) k_pred$reb else round(k_rpg,1)
        k_pred_ast <- if (!is.null(k_pred$ast) && !is.na(k_pred$ast)) k_pred$ast else round(k_apg,1)
        o_pred_pts <- if (!is.null(o_pred$pts) && !is.na(o_pred$pts)) o_pred$pts else round(o_ppg,1)
        o_pred_reb <- if (!is.null(o_pred$reb) && !is.na(o_pred$reb)) o_pred$reb else round(o_rpg,1)
        o_pred_ast <- if (!is.null(o_pred$ast) && !is.na(o_pred$ast)) o_pred$ast else round(o_apg,1)
        
        border_c <- if(winner=="kings")"#10b981" else if(winner=="opp")"#ef4444" else "#f59e0b"
        bg_c     <- if(winner=="kings")"#f0fdf4" else if(winner=="opp")"#fef2f2" else "#fffbeb"
        
        div(style=paste0("background:",bg_c,";border-left:5px solid ",border_c,
                         ";padding:18px;margin:12px 0;border-radius:8px;",
                         "box-shadow:0 2px 6px rgba(0,0,0,0.08);"),
            div(style="text-align:center;margin-bottom:12px;",
                span(paste0("— ",k$lineup_position," MATCHUP —"),
                     style="font-weight:700;color:#64748b;font-size:0.85em;letter-spacing:1px;")),
            div(style="display:flex;justify-content:space-between;align-items:center;",
                # Kings player
                div(style="flex:1;",
                    div(style="display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
                        strong(as.character(k$Player),style=paste0("font-size:1.1em;color:",p_col,";")),
                        span(paste0("(",k$Position,")"),style="color:#64748b;font-size:0.9em;")),
                    div(style="font-size:0.85em;color:#64748b;margin-top:4px;",
                        paste0("Season: ",round(k_ppg,1)," / ",round(k_rpg,1)," / ",round(k_apg,1))),
                    div(style="margin-top:4px;",
                        div("PREDICTED THIS GAME:",style="font-size:0.7em;color:#94a3b8;font-weight:600;margin-bottom:3px;"),
                        div(style="display:flex;gap:4px;",
                            span(paste0(k_pred_pts," PTS"),style=paste0("font-size:0.78em;font-weight:700;color:white;background:",p_col,";padding:3px 7px;border-radius:4px;")),
                            span(paste0(k_pred_reb," REB"),style=paste0("font-size:0.78em;font-weight:700;color:white;background:",s_col,";padding:3px 7px;border-radius:4px;")),
                            span(paste0(k_pred_ast," AST"),style=paste0("font-size:0.78em;font-weight:700;color:#1e293b;background:",a_col,";padding:3px 7px;border-radius:4px;")))
                    )
                ),
                # Edge indicator
                div(style="flex:0 0 110px;text-align:center;padding:0 8px;",
                    div(if(winner=="kings") paste0(overall,"%") else if(winner=="opp") paste0(opp_adv,"%") else "EVEN",
                        style=paste0("font-size:1.8em;font-weight:800;color:",border_c,";")),
                    div(if(winner=="kings") paste0(rv$my_team," Edge") else if(winner=="opp") "Opp Edge" else "Toss-Up",
                        style=paste0("font-size:0.7em;font-weight:600;color:",border_c,";text-align:center;"))),
                # Opp player
                div(style="flex:1;text-align:right;",
                    div(style="display:flex;align-items:center;justify-content:flex-end;gap:6px;flex-wrap:wrap;",
                        strong(as.character(o$Player),style="font-size:1.1em;color:#991b1b;"),
                        span(paste0("(",o$Position,")"),style="color:#64748b;font-size:0.9em;")),
                    div(style="font-size:0.85em;color:#64748b;margin-top:4px;",
                        paste0("Season: ",round(o_ppg,1)," / ",round(o_rpg,1)," / ",round(o_apg,1))),
                    div(style="margin-top:4px;",
                        div("PREDICTED THIS GAME:",style="font-size:0.7em;color:#94a3b8;font-weight:600;margin-bottom:3px;text-align:right;"),
                        div(style="display:flex;gap:4px;justify-content:flex-end;",
                            span(paste0(o_pred_pts," PTS"),style="font-size:0.78em;font-weight:700;color:white;background:#991b1b;padding:3px 7px;border-radius:4px;"),
                            span(paste0(o_pred_reb," REB"),style="font-size:0.78em;font-weight:700;color:white;background:#7f1d1d;padding:3px 7px;border-radius:4px;"),
                            span(paste0(o_pred_ast," AST"),style="font-size:0.78em;font-weight:700;color:#1e293b;background:#fca5a5;padding:3px 7px;border-radius:4px;"))
                    )
                )
            ),
            # Overall advantage bar
            div(style="margin-top:12px;",
                div(style="display:flex;align-items:center;gap:8px;",
                    span(rv$my_team,style=paste0("font-size:0.75em;font-weight:600;color:",p_col,";min-width:80px;")),
                    div(style=paste0("flex-grow:1;height:14px;background:#fee2e2;border-radius:7px;overflow:hidden;"),
                        div(style=paste0("width:",overall,"%;height:100%;",
                                         "background:linear-gradient(90deg,",p_col,",",s_col,");border-radius:7px;"))),
                    span("Opp",style="font-size:0.75em;font-weight:600;color:#991b1b;min-width:30px;text-align:right;"))),
            # Category breakdown
            div(style="display:flex;gap:12px;margin-top:10px;justify-content:center;flex-wrap:wrap;",
                span(paste0("\U0001f3af Scoring: ",round(sc_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(sc_adv>55)"#10b981" else if(sc_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f3c0 Boards: ",round(rb_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(rb_adv>55)"#10b981" else if(rb_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f91d Playmaking: ",round(as_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(as_adv>55)"#10b981" else if(as_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f6e1\ufe0f Defense: ",round(df_adv,0),"%"),
                     style=paste0("font-size:0.78em;color:",if(df_adv>55)"#10b981" else if(df_adv<45)"#ef4444" else "#64748b",";")))
        )
      })
    )
  })
  
  # --------------------------------------------------------------------------
  # SCOUTING REPORT — fixed (rpg/apg helpers inline, no missing function refs)
  # --------------------------------------------------------------------------
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    a_col <- rv$team_cols$accent
    
    if (input$scout_team == "")
      return(div(style="text-align:center;padding:60px 20px;",
                 div("\U0001f50d",style="font-size:4em;opacity:0.3;"),
                 h3("Select an opponent to generate scouting report",style="color:#94a3b8;margin-top:15px;"),
                 p("Choose a team from the dropdown above",style="color:#cbd5e1;")))
    
    opp_name   <- input$scout_team
    opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
    opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
    if (is.null(opp_roster) || nrow(opp_record) == 0)
      return(p("No data available for this team",
               style="text-align:center;color:#999;padding:40px;"))
    
    opp_gp   <- opp_record$W[1] + opp_record$L[1]
    opp_wpct <- if(opp_gp>0) round(opp_record$W[1]/opp_gp*100,0) else 50
    
    # All stats via safe helper
    opp_pts  <- .get_roster_stat(opp_roster,"PTS"); opp_reb <- .get_roster_stat(opp_roster,"REB")
    opp_ast  <- .get_roster_stat(opp_roster,"AST")
    opp_2pm  <- .get_roster_stat(opp_roster,"2PM"); opp_2pa <- .get_roster_stat(opp_roster,"2PA")
    opp_3pm  <- .get_roster_stat(opp_roster,"3PM"); opp_3pa <- .get_roster_stat(opp_roster,"3PA")
    opp_ftm  <- .get_roster_stat(opp_roster,"FTM"); opp_fta <- .get_roster_stat(opp_roster,"FTA")
    
    opp_ppg <- get_accurate_team_ppg(opp_name, rv$aba_data)
    if (opp_ppg == 0 && opp_gp > 0) opp_ppg <- round(opp_pts/opp_gp,1)
    opp_rpg <- if(opp_gp>0) round(opp_reb/opp_gp,1) else 0
    opp_apg <- if(opp_gp>0) round(opp_ast/opp_gp,1) else 0
    
    fg2_pct    <- if(opp_2pa>0) round(opp_2pm/opp_2pa*100,1) else 0
    fg3_pct    <- if(opp_3pa>0) round(opp_3pm/opp_3pa*100,1) else 0
    ft_pct     <- if(opp_fta>0) round(opp_ftm/opp_fta*100,1) else 0
    three_rate <- if((opp_2pa+opp_3pa)>0) round(opp_3pa/(opp_2pa+opp_3pa)*100,0) else 0
    play_style <- if(three_rate>=40)"Perimeter-Heavy" else if(three_rate>=25)"Balanced" else "Interior-Focused"
    
    # Scoring distribution breakdown
    pts_from_2 <- opp_2pm*2; pts_from_3 <- opp_3pm*3; pts_from_ft <- opp_ftm
    total_scored <- pts_from_2+pts_from_3+pts_from_ft
    pct_2  <- if(total_scored>0) round(pts_from_2/total_scored*100,0) else 33
    pct_3  <- if(total_scored>0) round(pts_from_3/total_scored*100,0) else 33
    pct_ft <- if(total_scored>0) round(pts_from_ft/total_scored*100,0) else 34
    
    # Key players
    player_roster <- opp_roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    key_players <- player_roster %>%
      mutate(
        player_ppg = sapply(Player,function(pl){ tryCatch({
          pg<-rv$aba_data$all_player_stats[[pl]]$per_game
          if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) pg$PPG[1] else 0
        },error=function(e) 0)}),
        player_rpg = sapply(Player,function(pl){ tryCatch({
          pg<-rv$aba_data$all_player_stats[[pl]]$per_game
          if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$RPG[1])) pg$RPG[1] else 0
        },error=function(e) 0)}),
        player_apg = sapply(Player,function(pl){ tryCatch({
          pg<-rv$aba_data$all_player_stats[[pl]]$per_game
          if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$APG[1])) pg$APG[1] else 0
        },error=function(e) 0)}),
        player_pos = sapply(Player,function(pl){ tryCatch({
          pos<-rv$aba_data$all_player_stats[[pl]]$position
          if(!is.null(pos)&&!is.na(pos)&&pos!="") pos else "G"
        },error=function(e) "G")}),
        gp = sapply(Player,function(pl){ tryCatch({
          gl<-rv$aba_data$all_player_stats[[pl]]$game_log
          if(!is.null(gl)) nrow(gl) else 0
        },error=function(e) 0)}),
        streak = sapply(Player,function(pl){ tryCatch({
          pd<-rv$aba_data$all_player_stats[[pl]]
          if(is.null(pd$game_log)||nrow(pd$game_log)<3||is.null(pd$per_game)) return(0)
          rec<-pd$game_log%>%arrange(desc(date))%>%head(3)
          rec_avg<-mean(suppressWarnings(as.numeric(rec$PTS)),na.rm=TRUE)
          szn_avg<-pd$per_game$PPG[1]
          if(is.na(rec_avg)||is.na(szn_avg)||szn_avg==0) return(0)
          round(((rec_avg-szn_avg)/szn_avg)*100,1)
        },error=function(e) 0)})
      ) %>%
      filter(gp>=3, player_ppg>0) %>%
      arrange(desc(player_ppg)) %>%
      head(5)
    
    top_scorer_share <- if(opp_ppg>0&&nrow(key_players)>0) round(key_players$player_ppg[1]/opp_ppg*100,0) else 0
    top2_share <- if(opp_ppg>0&&nrow(key_players)>=2) round(sum(key_players$player_ppg[1:2])/opp_ppg*100,0) else top_scorer_share
    concentration <- if(top_scorer_share>=30)"Star-Dependent" else if(top2_share>=50)"Top-Heavy" else "Balanced Scoring"
    
    hot_players  <- key_players %>% filter(streak>15)
    cold_players <- key_players %>% filter(streak<-15)
    
    threat       <- if(opp_wpct>=70)"HIGH" else if(opp_wpct>=50)"MODERATE" else "LOW"
    threat_color <- if(threat=="HIGH")"#b91c1c" else if(threat=="MODERATE")"#d97706" else "#15803d"
    
    # My team stats (inline, no helper function dependency)
    kings_record <- rv$aba_data$league_data %>% filter(Team==rv$my_team)
    kings_ppg    <- get_accurate_team_ppg(rv$my_team, rv$aba_data)
    kings_gp     <- if(nrow(kings_record)>0) kings_record$W[1]+kings_record$L[1] else 0
    kings_wpct   <- if(kings_gp>0) round(kings_record$W[1]/kings_gp*100,0) else 50
    
    kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
    kings_rpg <- 0; kings_apg <- 0
    if (!is.null(kings_roster) && kings_gp > 0) {
      kings_rpg <- round(.get_roster_stat(kings_roster,"REB") / kings_gp, 1)
      kings_apg <- round(.get_roster_stat(kings_roster,"AST") / kings_gp, 1)
    }
    
    opp_logo <- ""
    if("logo_url"%in%names(rv$aba_data$team_lookup)){
      lr<-rv$aba_data$team_lookup%>%filter(team_name==opp_name)%>%pull(logo_url)
      if(length(lr)>0&&!is.na(lr[1])) opp_logo<-lr[1]
    }
    
    # Strategy generation
    strategy_points <- c()
    if(nrow(key_players)>=1){
      significant<-key_players%>%filter(player_ppg>=opp_ppg*0.15); n_threats<-nrow(significant)
      if(n_threats==1){
        strategy_points<-c(strategy_points,paste0(
          "Contain ",significant$Player[1]," (",significant$player_ppg[1]," PPG, ",top_scorer_share,
          "% of scoring) — one dominant option. Double aggressively, force role players to create."))
      } else if(n_threats==2){
        p2_share<-if(opp_ppg>0) round(significant$player_ppg[2]/opp_ppg*100,0) else 0
        strategy_points<-c(strategy_points,paste0(
          "Two-headed attack: ",significant$Player[1]," (",significant$player_ppg[1]," PPG) and ",
          significant$Player[2]," (",significant$player_ppg[2]," PPG, ",p2_share,
          "%) combine for ",top2_share,"% — can't focus on just one."))
      } else if(n_threats>=3){
        strategy_points<-c(strategy_points,paste0(
          "Deep scoring team (",n_threats," players at 15%+ of team scoring) — prioritize team defense."))
      }
    }
    if(three_rate>=35&&fg3_pct>=35){
      strategy_points<-c(strategy_points,paste0(
        "DANGER from 3: ",three_rate,"% of shots from deep at ",fg3_pct,"% — close out hard on every three."))
    } else if(three_rate<25){
      strategy_points<-c(strategy_points,paste0(
        "Rarely shoots 3s (",three_rate,"%) — sag off perimeter, clog driving lanes, pack the paint."))
    } else {
      strategy_points<-c(strategy_points,paste0(
        "Balanced attack: ",three_rate,"% from 3 (",fg3_pct,"%), ",fg2_pct,"% from 2 — play honest defense."))
    }
    if(opp_rpg>kings_rpg+3){
      strategy_points<-c(strategy_points,paste0(
        "REBOUNDING MISMATCH: They average ",opp_rpg," RPG vs our ",kings_rpg," — crash boards harder."))
    } else if(kings_rpg>opp_rpg+3){
      strategy_points<-c(strategy_points,paste0(
        "We dominate the glass (",kings_rpg," vs ",opp_rpg," RPG) — push pace off defensive boards."))
    }
    if(ft_pct>=75){
      strategy_points<-c(strategy_points,paste0("Strong FT shooting (",ft_pct,"%) — avoid fouling in close games."))
    } else if(ft_pct<60){
      strategy_points<-c(strategy_points,paste0("Poor FT shooting (",ft_pct,"%) — consider strategic fouling late."))
    }
    if(nrow(hot_players)>0){
      strategy_points<-c(strategy_points,paste0(
        "\U0001f525 HOT ALERT: ",paste(hot_players$Player,collapse=", ")," — playing above season average."))
    }
    if(nrow(cold_players)>0){
      strategy_points<-c(strategy_points,paste0(
        "\u2744\ufe0f Cold stretch: ",paste(cold_players$Player,collapse=", ")," — still dangerous but trending down."))
    }
    if(kings_ppg>opp_ppg+10){
      strategy_points<-c(strategy_points,paste0("We outscore them by ",round(kings_ppg-opp_ppg,0)," PPG — push pace, make it a track meet."))
    } else if(opp_ppg>kings_ppg+10){
      strategy_points<-c(strategy_points,paste0("They outscore us by ",round(opp_ppg-kings_ppg,0)," PPG — slow the game down, limit possessions."))
    }
    
    # Narrative
    narrative_parts <- c(
      paste0("The ",opp_name," enter this game at ",opp_record$W[1],"-",opp_record$L[1],
             " (",opp_wpct,"%) from the ",opp_record$Division[1]," Division, averaging ",opp_ppg," PPG."),
      paste0("Their offense is ",tolower(play_style),
             if(play_style=="Perimeter-Heavy") paste0(", launching ",three_rate,"% of shots from deep at ",fg3_pct,"% efficiency.")
             else if(play_style=="Interior-Focused") paste0(", preferring inside work with a ",fg2_pct,"% 2P rate.")
             else paste0(" with a ",three_rate,"% 3-point rate (",fg3_pct,"%) and ",fg2_pct,"% on twos.")),
      if(nrow(key_players)>=1) {
        p1<-key_players[1,]
        if(concentration=="Star-Dependent")
          paste0(p1$Player," carries ",top_scorer_share,"% of scoring (",p1$player_ppg," PPG). Contain them and this team struggles.")
        else if(top2_share>=45&&nrow(key_players)>=2) {
          p2<-key_players[2,]; p2s<-if(opp_ppg>0) round(p2$player_ppg/opp_ppg*100,0) else 0
          paste0(p1$Player," (",p1$player_ppg," PPG) and ",p2$Player," (",p2$player_ppg," PPG, ",p2s,"%) are both legitimate threats.")
        } else paste0(p1$Player," leads at ",p1$player_ppg," PPG but scoring is well-distributed — no single player to shut down.")
      } else "",
      paste0("For ",rv$my_team,": ",
             if(kings_ppg>opp_ppg+15) paste0("favorable matchup — we outscore them by ",round(kings_ppg-opp_ppg,0)," PPG. Stay focused.")
             else if(kings_ppg>opp_ppg) paste0("we hold a scoring edge (",kings_ppg," vs ",opp_ppg," PPG) — don't be complacent.")
             else if(opp_ppg>kings_ppg+10) paste0("we face a scoring gap (",kings_ppg," vs ",opp_ppg," PPG). Tempo control and defense are critical.")
             else paste0("this projects as a competitive game (",kings_ppg," vs ",opp_ppg," PPG). Execution decides it."),
             " Overall, a ",tolower(threat),"-threat opponent.")
    )
    narrative_parts <- narrative_parts[nchar(narrative_parts) > 0]
    
    div(
      # Header
      div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                       "padding:30px;border-radius:14px;margin-bottom:20px;position:relative;overflow:hidden;"),
          div(style="position:absolute;right:-20px;top:-20px;font-size:12em;opacity:0.03;font-weight:900;","\U0001f50d"),
          div(style="display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
              if(opp_logo!="")
                tags$img(src=opp_logo,
                         style="width:80px;height:80px;border-radius:50%;border:3px solid rgba(255,255,255,0.3);object-fit:cover;background:white;"),
              div(div("PRE-GAME SCOUTING REPORT",style="font-size:0.75em;font-weight:600;opacity:0.6;letter-spacing:2px;"),
                  h2(opp_name,style="margin:5px 0 0 0;font-weight:800;font-size:1.8em;"),
                  div(paste0(opp_record$Division[1]," Division | ",
                             opp_record$W[1],"-",opp_record$L[1]," (",opp_wpct,"% win rate)"),
                      style="opacity:0.7;margin-top:4px;")),
              div(style="margin-left:auto;text-align:right;",
                  div("THREAT LEVEL",style="font-size:0.7em;opacity:0.5;letter-spacing:1px;"),
                  div(threat,style=paste0("font-size:1.8em;font-weight:800;color:",threat_color,
                                          ";background:rgba(255,255,255,0.1);padding:4px 20px;",
                                          "border-radius:8px;margin-top:4px;")))
          )
      ),
      
      # Quick stats
      div(style="display:flex;gap:12px;margin-bottom:20px;flex-wrap:wrap;",
          lapply(list(
            list("\U0001f3c0","PPG",  opp_ppg,              "per game"),
            list("\U0001f4aa","RPG",  opp_rpg,              "per game"),
            list("\U0001f91d","APG",  opp_apg,              "per game"),
            list("\U0001f3af","2P%",  paste0(fg2_pct,"%"),  "from inside"),
            list("\u2604\ufe0f","3P%",paste0(fg3_pct,"%"),  paste0(three_rate,"% of shots")),
            list("\u2705",    "FT%",  paste0(ft_pct,"%"),   "from the line")
          ),function(item)
            div(style="flex:1;min-width:120px;background:white;border:1px solid #e2e8f0;border-radius:10px;padding:14px;text-align:center;",
                div(item[[1]],style="font-size:1.3em;"),
                div(item[[3]],style="font-size:1.5em;font-weight:800;color:#1e293b;margin:4px 0;"),
                div(item[[2]],style="font-size:0.75em;font-weight:700;color:#64748b;"),
                div(item[[4]],style="font-size:0.65em;color:#94a3b8;margin-top:2px;"))
          )
      ),
      
      # Two column
      div(style="display:flex;gap:20px;flex-wrap:wrap;",
          
          div(style="flex:1;min-width:350px;",
              # Key players
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;margin-bottom:16px;",
                  h4("\U0001f464 Key Players to Watch",style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  if(nrow(key_players)>0) lapply(1:nrow(key_players),function(i){
                    kp<-key_players[i,]
                    is_prim<-i<=2&&kp$player_ppg>=opp_ppg*0.15
                    si<-if(kp$streak>15)"\U0001f525" else if(kp$streak<-15)"\u2744\ufe0f" else ""
                    st<-if(kp$streak>15)"HOT" else if(kp$streak<-15)"COLD" else ""
                    ps<-if(opp_ppg>0) round(kp$player_ppg/opp_ppg*100,0) else 0
                    bg_c<-if(is_prim)"#fef2f2" else "#f8fafc"
                    bd_c<-if(is_prim)"#b91c1c" else "#e2e8f0"
                    tc_c<-if(is_prim)"#991b1b" else "#334155"
                    div(style=paste0("padding:12px;border-radius:10px;margin-bottom:8px;",
                                     "border-left:4px solid ",bd_c,";background:",bg_c,";"),
                        div(style="display:flex;justify-content:space-between;align-items:start;",
                            div(div(style="display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
                                    span(paste0("#",i),style="font-weight:800;color:#94a3b8;font-size:0.85em;"),
                                    strong(kp$Player,style=paste0("font-size:1em;color:",tc_c,";")),
                                    span(paste0("(",kp$player_pos,")"),style="color:#64748b;font-size:0.85em;"),
                                    if(st!="") span(paste0(si," ",st),
                                                    style=paste0("font-size:0.7em;font-weight:700;",
                                                                 "color:",if(st=="HOT")"#b91c1c" else "#3b82f6",
                                                                 ";background:",if(st=="HOT")"#fef2f2" else "#eff6ff",
                                                                 ";padding:1px 6px;border-radius:4px;"))),
                                div(style="display:flex;gap:8px;margin-top:6px;",
                                    span(paste0(kp$player_ppg," PPG"),style=paste0("font-size:0.8em;font-weight:700;color:",p_col,";background:#fee2e2;padding:2px 6px;border-radius:4px;")),
                                    span(paste0(kp$player_rpg," RPG"),style="font-size:0.8em;font-weight:700;color:#1e40af;background:#dbeafe;padding:2px 6px;border-radius:4px;"),
                                    span(paste0(kp$player_apg," APG"),style="font-size:0.8em;font-weight:700;color:#b45309;background:#fef3c7;padding:2px 6px;border-radius:4px;")),
                                div(paste0(kp$gp," games played"),style="font-size:0.7em;color:#94a3b8;margin-top:4px;")),
                            if(ps>=15) div(style=paste0("text-align:center;background:#b91c1c;color:white;",
                                                        "padding:4px 10px;border-radius:8px;font-size:0.7em;font-weight:700;white-space:nowrap;"),
                                           paste0(ps,"% of\nteam PPG"))
                        )
                    )
                  }) else p("No player data available",style="color:#94a3b8;text-align:center;")
              ),
              # Tendencies
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                  h4("\U0001f4ca Team Tendencies",style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  div(style="display:flex;gap:12px;margin-bottom:14px;",
                      div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                       if(play_style=="Perimeter-Heavy")"#dbeafe"
                                       else if(play_style=="Interior-Focused")"#fee2e2" else "#f0fdf4",";"),
                          div(play_style,style="font-weight:700;font-size:0.95em;color:#334155;"),
                          div(paste0(three_rate,"% of shots from 3"),style="font-size:0.75em;color:#64748b;margin-top:2px;")),
                      div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                       if(concentration=="Star-Dependent")"#fef2f2"
                                       else if(concentration=="Balanced Scoring")"#f0fdf4" else "#fffbeb",";"),
                          div(concentration,style="font-weight:700;font-size:0.95em;color:#334155;"),
                          div(paste0("Top 2 = ",top2_share,"% of scoring"),style="font-size:0.75em;color:#64748b;margin-top:2px;"))
                  ),
                  div(style="display:flex;justify-content:space-between;margin-bottom:6px;",
                      span("Scoring Distribution",style="font-weight:600;color:#475569;font-size:0.85em;"),
                      span(paste0(opp_ppg," PPG"),style="font-weight:700;color:#334155;font-size:0.85em;")),
                  div(style="height:28px;border-radius:8px;overflow:hidden;display:flex;",
                      div(style=paste0("width:",pct_2,"%;background:",p_col,";display:flex;align-items:center;justify-content:center;"),
                          if(pct_2>=15) span(paste0("2P ",pct_2,"%"),style="color:white;font-size:0.65em;font-weight:700;")),
                      div(style=paste0("width:",pct_3,"%;background:",s_col,";display:flex;align-items:center;justify-content:center;"),
                          if(pct_3>=15) span(paste0("3P ",pct_3,"%"),style="color:white;font-size:0.65em;font-weight:700;")),
                      div(style=paste0("width:",pct_ft,"%;background:",a_col,";display:flex;align-items:center;justify-content:center;"),
                          if(pct_ft>=10) span(paste0("FT ",pct_ft,"%"),style="color:#1e293b;font-size:0.65em;font-weight:700;")))
              )
          ),
          
          div(style="flex:1;min-width:350px;",
              # Strategy
              div(style="background:white;border:2px solid #15803d;border-radius:12px;padding:20px;margin-bottom:16px;",
                  h4("\u2705 How to Beat Them",style="margin:0 0 14px 0;color:#15803d;font-weight:700;"),
                  lapply(seq_along(strategy_points),function(i){
                    sp<-strategy_points[i]
                    is_alert<-grepl("\U0001f525",sp); is_cold<-grepl("\u2744",sp)
                    is_warn<-grepl("DANGER|MISMATCH|outscore",sp)
                    bg_s<-if(is_alert)"#fef2f2;border:1px solid #fecaca;"
                    else if(is_cold)"#eff6ff;border:1px solid #bfdbfe;"
                    else if(is_warn)"#fffbeb;border:1px solid #fde68a;"
                    else "#f0fdf4;border:1px solid #bbf7d0;"
                    nb<-if(is_alert)"#b91c1c" else if(is_cold)"#3b82f6"
                    else if(is_warn)"#d97706" else "#15803d"
                    tc_s<-if(is_alert)"#991b1b" else if(is_cold)"#1e40af"
                    else if(is_warn)"#92400e" else "#166534"
                    div(style=paste0("padding:10px 12px;border-radius:8px;margin-bottom:8px;",
                                     "display:flex;align-items:start;gap:10px;background:",bg_s),
                        div(style=paste0("width:24px;height:24px;border-radius:50%;flex-shrink:0;",
                                         "display:flex;align-items:center;justify-content:center;",
                                         "font-weight:800;font-size:0.75em;color:white;background:",nb,";"),i),
                        p(sp,style=paste0("margin:0;font-size:0.88em;font-weight:600;color:",tc_s,";")))
                  })
              ),
              # Head to head comparison
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                  h4(paste0("\u2694\ufe0f ",rv$my_team," vs ",opp_name),
                     style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  lapply(list(
                    list("Record",paste0(if(nrow(kings_record)>0) paste0(kings_record$W[1],"-",kings_record$L[1]) else "N/A"),
                         paste0(opp_record$W[1],"-",opp_record$L[1]),kings_wpct,opp_wpct),
                    list("PPG",    kings_ppg, opp_ppg, kings_ppg, opp_ppg),
                    list("RPG",    kings_rpg, opp_rpg, kings_rpg, opp_rpg),
                    list("APG",    kings_apg, opp_apg, kings_apg, opp_apg)
                  ),function(comp){
                    k_num<-suppressWarnings(as.numeric(comp[[4]])); o_num<-suppressWarnings(as.numeric(comp[[5]]))
                    if(is.na(k_num)) k_num<-50; if(is.na(o_num)) o_num<-50
                    total<-k_num+o_num
                    k_pct<-if(total>0) round(k_num/total*100,0) else 50
                    k_win<-k_num>o_num
                    div(style="margin-bottom:10px;",
                        div(style="display:flex;justify-content:space-between;margin-bottom:4px;",
                            span(paste0(rv$my_team,": ",comp[[2]]),
                                 style=paste0("font-size:0.8em;font-weight:700;color:",if(k_win)"#15803d" else "#b91c1c",";")),
                            span(comp[[1]],style="font-size:0.75em;font-weight:600;color:#94a3b8;"),
                            span(paste0(comp[[3]]," :",opp_name),
                                 style=paste0("font-size:0.8em;font-weight:700;color:",if(!k_win)"#15803d" else "#b91c1c",";"))),
                        div(style="height:8px;border-radius:4px;display:flex;overflow:hidden;",
                            div(style=paste0("width:",k_pct,"%;background:",p_col,";")),
                            div(style=paste0("width:",100-k_pct,"%;background:#94a3b8;"))))
                  })
              )
          )
      ),
      
      # Narrative
      div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                       "border-radius:12px;padding:24px;margin-top:20px;"),
          h4("\U0001f4dd Game Preview",style="margin:0 0 12px 0;opacity:0.8;letter-spacing:0.5px;"),
          p(paste(narrative_parts,collapse=" "),
            style="margin:0;line-height:1.8;font-size:0.95em;opacity:0.9;")
      ),
      
      # Footer
      div(style="text-align:center;margin-top:20px;padding:15px;color:#94a3b8;font-size:0.8em;",
          paste0("Generated ",format(Sys.time(),"%B %d, %Y at %I:%M %p"),
                 " | ",rv$my_team," Scouting Dashboard"),
          div(style="margin-top:8px;",
              actionButton("print_report","\U0001f5a8\ufe0f Print Report",
                           class="btn-sm",
                           style="background:#475569;color:white;border:none;padding:8px 20px;border-radius:8px;font-weight:600;",
                           onclick="window.print();"))
      )
    )
  })
  
  # ============================================================================
  # PATCH 2 — paste BEFORE  } # end server
  # Fixes: (1) Four Factors badge overflow  (2) Scouting %||% crash
  # This overrides the previous app_fixes.R definitions for these two outputs.
  # ============================================================================
  
  # --------------------------------------------------------------------------
  # FOUR FACTORS — badge overflow fixed
  # The grade circle now sits INSIDE the card flow (not position:absolute)
  # so it never clips over the gauge arc.
  # --------------------------------------------------------------------------
  output$four_factors <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    
    kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
    if (is.null(kings_roster) || nrow(kings_roster) == 0)
      return(p("No roster data available", style = "text-align:center;color:#999;padding:20px;"))
    
    total_2pm  <- .get_roster_stat(kings_roster, "2PM")
    total_2pa  <- .get_roster_stat(kings_roster, "2PA")
    total_3pm  <- .get_roster_stat(kings_roster, "3PM")
    total_3pa  <- .get_roster_stat(kings_roster, "3PA")
    total_fgm  <- total_2pm + total_3pm
    total_fga  <- total_2pa + total_3pa
    total_fta  <- .get_roster_stat(kings_roster, "FTA")
    total_ftm  <- .get_roster_stat(kings_roster, "FTM")
    total_oreb <- .get_roster_stat(kings_roster, "OREB")
    total_dreb <- .get_roster_stat(kings_roster, "DREB")
    
    efg      <- if (total_fga > 0) round(min((total_fgm + 0.5*total_3pm)/total_fga*100, 100), 1) else 0
    tov_pct  <- 12
    oreb_pct <- if ((total_oreb+total_dreb) > 0) round(total_oreb/(total_oreb+total_dreb)*100, 1) else 0
    ft_rate  <- if (total_fga > 0) round(total_fta/total_fga, 2) else 0
    
    efg_grade  <- if(efg  >= 55)"A" else if(efg  >= 50)"B" else if(efg  >= 45)"C" else "D"
    tov_grade  <- if(tov_pct <= 12)"A" else if(tov_pct <= 15)"B" else if(tov_pct <= 18)"C" else "D"
    oreb_grade <- if(oreb_pct >= 35)"A" else if(oreb_pct >= 30)"B" else if(oreb_pct >= 25)"C" else "D"
    ftr_grade  <- if(ft_rate >= 0.35)"A" else if(ft_rate >= 0.25)"B" else if(ft_rate >= 0.18)"C" else "D"
    
    gc_fn <- function(g) switch(g,"A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c","#64748b")
    gb_fn <- function(g) switch(g,
                                "A"="linear-gradient(135deg,#15803d,#22c55e)",
                                "B"="linear-gradient(135deg,#65a30d,#a3e635)",
                                "C"="linear-gradient(135deg,#d97706,#fbbf24)",
                                "D"="linear-gradient(135deg,#b91c1c,#f87171)",
                                "linear-gradient(135deg,#64748b,#94a3b8)")
    
    # Build the SVG gauge arc
    make_arc_svg <- function(fill_pct, color) {
      fill_pct <- max(0, min(100, fill_pct))
      cx <- 60; cy <- 62; r <- 44
      to_rad <- function(deg) deg * pi / 180
      start_angle <- 135; total_sweep <- 270
      fill_angle  <- start_angle + (fill_pct / 100) * total_sweep
      
      sx <- cx + r * cos(to_rad(start_angle)); sy <- cy + r * sin(to_rad(start_angle))
      ex <- cx + r * cos(to_rad(start_angle + total_sweep)); ey <- cy + r * sin(to_rad(start_angle + total_sweep))
      fx <- cx + r * cos(to_rad(fill_angle)); fy <- cy + r * sin(to_rad(fill_angle))
      
      bg_large   <- 1
      fill_large <- if ((fill_pct / 100) * total_sweep > 180) 1 else 0
      
      HTML(paste0(
        '<svg width="120" height="110" viewBox="0 0 120 110" style="display:block;margin:0 auto;">',
        '<path d="M ', round(sx,2), ' ', round(sy,2),
        ' A ', r, ' ', r, ' 0 ', bg_large, ' 1 ', round(ex,2), ' ', round(ey,2), '"',
        ' fill="none" stroke="#e2e8f0" stroke-width="11" stroke-linecap="round"/>',
        if (fill_pct > 0.5)
          paste0('<path d="M ', round(sx,2), ' ', round(sy,2),
                 ' A ', r, ' ', r, ' 0 ', fill_large, ' 1 ', round(fx,2), ' ', round(fy,2), '"',
                 ' fill="none" stroke="', color, '" stroke-width="11" stroke-linecap="round"/>'),
        '</svg>'
      ))
    }
    
    make_ff_card <- function(value, label, description, grade, fill_pct) {
      gc <- gc_fn(grade); gb <- gb_fn(grade)
      tc_badge <- if (grade %in% c("B","C")) "#1a1a2e" else "white"
      
      div(style = paste0("background:white;border-radius:16px;padding:16px 12px;",
                         "text-align:center;box-shadow:0 4px 16px rgba(0,0,0,0.06);",
                         "border:2px solid ", gc, "30;"),
          # Grade badge — in normal flow at the top, no absolute positioning
          div(style = paste0("display:inline-flex;align-items:center;justify-content:center;",
                             "width:36px;height:36px;border-radius:50%;",
                             "background:", gb, ";margin:0 auto 6px auto;",
                             "font-weight:800;font-size:1em;color:", tc_badge, ";",
                             "box-shadow:0 2px 8px rgba(0,0,0,0.15);"),
              grade),
          # SVG gauge
          make_arc_svg(fill_pct, gc),
          # Value
          h3(value, style = paste0("margin:4px 0 0 0;font-size:1.75em;font-weight:800;color:", gc, ";")),
          p(label, style = "margin:4px 0 0 0;color:#475569;font-weight:600;font-size:0.9em;"),
          p(description, style = "margin:3px 0 0 0;color:#94a3b8;font-size:0.73em;"),
          p(paste0(round(fill_pct, 0), "th %ile"),
            style = paste0("margin:4px 0 0 0;font-size:0.7em;font-weight:600;color:", gc, ";"))
      )
    }
    
    efg_fill  <- min(100, max(0, (efg      - 35)   / (65   - 35)   * 100))
    tov_fill  <- min(100, max(0, (25 - tov_pct)    / (25   - 5)    * 100))
    oreb_fill <- min(100, max(0, (oreb_pct - 15)   / (45   - 15)   * 100))
    ftr_fill  <- min(100, max(0, (ft_rate  - 0.10) / (0.45 - 0.10) * 100))
    
    fluidRow(
      column(3, make_ff_card(paste0(efg,     "%"), "eFG%",    "Shooting efficiency", efg_grade,  efg_fill)),
      column(3, make_ff_card(paste0(tov_pct, "%"), "TOV%",    "Lower is better",     tov_grade,  tov_fill)),
      column(3, make_ff_card(paste0(oreb_pct,"%"), "OREB%",   "Offensive boards",    oreb_grade, oreb_fill)),
      column(3, make_ff_card(ft_rate,               "FT Rate", "Getting to the line", ftr_grade,  ftr_fill))
    )
  })
  
  # --------------------------------------------------------------------------
  # SCOUTING REPORT — fixed: removed %||% (not in base R), all tryCatch guards
  # --------------------------------------------------------------------------
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    p_col <- rv$team_cols$primary
    s_col <- rv$team_cols$secondary
    a_col <- rv$team_cols$accent
    
    if (is.null(input$scout_team) || input$scout_team == "")
      return(div(style="text-align:center;padding:60px 20px;",
                 div("\U0001f50d", style="font-size:4em;opacity:0.3;"),
                 h3("Select an opponent to generate scouting report",
                    style="color:#94a3b8;margin-top:15px;")))
    
    opp_name   <- input$scout_team
    opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
    opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
    
    if (is.null(opp_roster) || nrow(opp_record) == 0)
      return(p("No data available for this team. Ensure data has been loaded.",
               style = "text-align:center;color:#999;padding:40px;"))
    
    tryCatch({
      
      opp_gp   <- as.numeric(opp_record$W[1]) + as.numeric(opp_record$L[1])
      opp_wpct <- if (opp_gp > 0) round(opp_record$W[1] / opp_gp * 100, 0) else 50
      
      opp_pts  <- .get_roster_stat(opp_roster, "PTS")
      opp_reb  <- .get_roster_stat(opp_roster, "REB")
      opp_ast  <- .get_roster_stat(opp_roster, "AST")
      opp_2pm  <- .get_roster_stat(opp_roster, "2PM"); opp_2pa <- .get_roster_stat(opp_roster, "2PA")
      opp_3pm  <- .get_roster_stat(opp_roster, "3PM"); opp_3pa <- .get_roster_stat(opp_roster, "3PA")
      opp_ftm  <- .get_roster_stat(opp_roster, "FTM"); opp_fta <- .get_roster_stat(opp_roster, "FTA")
      
      opp_ppg <- get_accurate_team_ppg(opp_name, rv$aba_data)
      if (opp_ppg == 0 && opp_gp > 0) opp_ppg <- round(opp_pts / opp_gp, 1)
      opp_rpg <- if (opp_gp > 0) round(opp_reb / opp_gp, 1) else 0
      opp_apg <- if (opp_gp > 0) round(opp_ast / opp_gp, 1) else 0
      
      fg2_pct    <- if (opp_2pa > 0) round(opp_2pm / opp_2pa * 100, 1) else 0
      fg3_pct    <- if (opp_3pa > 0) round(opp_3pm / opp_3pa * 100, 1) else 0
      ft_pct     <- if (opp_fta > 0) round(opp_ftm / opp_fta * 100, 1) else 0
      three_rate <- if ((opp_2pa + opp_3pa) > 0) round(opp_3pa / (opp_2pa + opp_3pa) * 100, 0) else 0
      play_style <- if (three_rate >= 40) "Perimeter-Heavy"
      else if (three_rate >= 25) "Balanced" else "Interior-Focused"
      
      pts_from_2   <- opp_2pm * 2; pts_from_3 <- opp_3pm * 3; pts_from_ft <- opp_ftm
      total_scored <- pts_from_2 + pts_from_3 + pts_from_ft
      pct_2  <- if (total_scored > 0) round(pts_from_2  / total_scored * 100, 0) else 33
      pct_3  <- if (total_scored > 0) round(pts_from_3  / total_scored * 100, 0) else 33
      pct_ft <- if (total_scored > 0) round(pts_from_ft / total_scored * 100, 0) else 34
      
      # Key players — all wrapped in tryCatch per player
      player_roster <- opp_roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      
      get_player_val <- function(pl, field, default = 0) {
        tryCatch({
          pd <- rv$aba_data$all_player_stats[[pl]]
          if (is.null(pd)) return(default)
          if (field == "ppg") {
            pg <- pd$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$PPG[1])) pg$PPG[1] else default
          } else if (field == "rpg") {
            pg <- pd$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$RPG[1])) pg$RPG[1] else default
          } else if (field == "apg") {
            pg <- pd$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$APG[1])) pg$APG[1] else default
          } else if (field == "pos") {
            pos <- pd$position
            if (!is.null(pos) && !is.na(pos) && nchar(pos) > 0) pos else "G"
          } else if (field == "gp") {
            gl <- pd$game_log
            if (!is.null(gl)) nrow(gl) else default
          } else if (field == "streak") {
            gl <- pd$game_log; pg <- pd$per_game
            if (is.null(gl) || nrow(gl) < 3 || is.null(pg)) return(0)
            rec <- gl %>% arrange(desc(date)) %>% head(3)
            rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)), na.rm = TRUE)
            szn_avg <- pg$PPG[1]
            if (is.na(rec_avg) || is.na(szn_avg) || szn_avg == 0) return(0)
            round(((rec_avg - szn_avg) / szn_avg) * 100, 1)
          } else default
        }, error = function(e) default)
      }
      
      key_players <- player_roster %>%
        mutate(
          player_ppg = sapply(Player, get_player_val, "ppg"),
          player_rpg = sapply(Player, get_player_val, "rpg"),
          player_apg = sapply(Player, get_player_val, "apg"),
          player_pos = sapply(Player, get_player_val, "pos", "G"),
          gp         = sapply(Player, get_player_val, "gp"),
          streak     = sapply(Player, get_player_val, "streak")
        ) %>%
        filter(gp >= 3, player_ppg > 0) %>%
        arrange(desc(player_ppg)) %>%
        head(5)
      
      top_scorer_share <- if (opp_ppg > 0 && nrow(key_players) > 0)
        round(key_players$player_ppg[1] / opp_ppg * 100, 0) else 0
      top2_share <- if (opp_ppg > 0 && nrow(key_players) >= 2)
        round(sum(key_players$player_ppg[1:2]) / opp_ppg * 100, 0) else top_scorer_share
      concentration <- if (top_scorer_share >= 30) "Star-Dependent"
      else if (top2_share >= 50)   "Top-Heavy"
      else                          "Balanced Scoring"
      
      hot_players  <- key_players %>% filter(streak > 15)
      cold_players <- key_players %>% filter(streak < -15)
      
      threat       <- if (opp_wpct >= 70) "HIGH" else if (opp_wpct >= 50) "MODERATE" else "LOW"
      threat_color <- if (threat == "HIGH") "#b91c1c"
      else if (threat == "MODERATE") "#d97706" else "#15803d"
      
      # My team stats — all inline, no external helper dependency
      kings_record <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      kings_ppg    <- get_accurate_team_ppg(rv$my_team, rv$aba_data)
      kings_gp     <- if (nrow(kings_record) > 0) as.numeric(kings_record$W[1]) + as.numeric(kings_record$L[1]) else 0
      kings_wpct   <- if (kings_gp > 0) round(kings_record$W[1] / kings_gp * 100, 0) else 50
      kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
      kings_rpg    <- 0; kings_apg <- 0
      if (!is.null(kings_roster) && kings_gp > 0) {
        kings_rpg <- round(.get_roster_stat(kings_roster, "REB") / kings_gp, 1)
        kings_apg <- round(.get_roster_stat(kings_roster, "AST") / kings_gp, 1)
      }
      
      opp_logo <- ""
      tryCatch({
        if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
          lr <- rv$aba_data$team_lookup %>% filter(team_name == opp_name) %>% pull(logo_url)
          if (length(lr) > 0 && !is.na(lr[1]) && nchar(lr[1]) > 0) opp_logo <- lr[1]
        }
      }, error = function(e) {})
      
      # Strategy points
      strategy_points <- c()
      if (nrow(key_players) >= 1) {
        significant <- key_players %>% filter(player_ppg >= opp_ppg * 0.15)
        n_threats   <- nrow(significant)
        if (n_threats == 1) {
          strategy_points <- c(strategy_points, paste0(
            "Contain ", significant$Player[1], " (", significant$player_ppg[1], " PPG, ",
            top_scorer_share, "% of scoring) — one dominant option. Double aggressively."))
        } else if (n_threats == 2) {
          p2s <- if (opp_ppg > 0) round(significant$player_ppg[2] / opp_ppg * 100, 0) else 0
          strategy_points <- c(strategy_points, paste0(
            "Two threats: ", significant$Player[1], " (", significant$player_ppg[1], " PPG) and ",
            significant$Player[2], " (", significant$player_ppg[2], " PPG, ", p2s,
            "%) — can't focus on just one."))
        } else if (n_threats >= 3) {
          strategy_points <- c(strategy_points,
                               paste0("Deep scoring team — prioritize team defense over individual assignments."))
        }
      }
      if (three_rate >= 35 && fg3_pct >= 35) {
        strategy_points <- c(strategy_points, paste0(
          "DANGER from 3: ", three_rate, "% of shots from deep at ", fg3_pct, "% — close out hard."))
      } else if (three_rate < 25) {
        strategy_points <- c(strategy_points, paste0(
          "Rarely shoots 3s (", three_rate, "%) — sag off perimeter, clog the paint."))
      } else {
        strategy_points <- c(strategy_points, paste0(
          "Balanced attack: ", three_rate, "% from 3 (", fg3_pct, "%), ", fg2_pct, "% from 2 — play honest defense."))
      }
      if (opp_rpg > kings_rpg + 3) {
        strategy_points <- c(strategy_points, paste0(
          "REBOUNDING MISMATCH: They average ", opp_rpg, " RPG vs our ", kings_rpg, " — crash boards harder."))
      } else if (kings_rpg > opp_rpg + 3) {
        strategy_points <- c(strategy_points, paste0(
          "We dominate the glass (", kings_rpg, " vs ", opp_rpg, " RPG) — push pace off defensive boards."))
      }
      if (ft_pct >= 75) {
        strategy_points <- c(strategy_points,
                             paste0("Strong FT shooting (", ft_pct, "%) — avoid fouling in close games."))
      } else if (ft_pct < 60) {
        strategy_points <- c(strategy_points,
                             paste0("Poor FT shooting (", ft_pct, "%) — consider strategic fouling late."))
      }
      if (nrow(hot_players) > 0)
        strategy_points <- c(strategy_points, paste0(
          "\U0001f525 HOT: ", paste(hot_players$Player, collapse=", "), " — playing above season average."))
      if (nrow(cold_players) > 0)
        strategy_points <- c(strategy_points, paste0(
          "\u2744\ufe0f Cold: ", paste(cold_players$Player, collapse=", "), " — trending below average."))
      if (kings_ppg > opp_ppg + 10) {
        strategy_points <- c(strategy_points, paste0(
          "We outscore them by ", round(kings_ppg - opp_ppg, 0), " PPG — push pace."))
      } else if (opp_ppg > kings_ppg + 10) {
        strategy_points <- c(strategy_points, paste0(
          "They outscore us by ", round(opp_ppg - kings_ppg, 0), " PPG — slow it down, limit possessions."))
      }
      
      # Narrative
      narrative <- paste(
        paste0("The ", opp_name, " enter at ", opp_record$W[1], "-", opp_record$L[1],
               " (", opp_wpct, "%) from the ", opp_record$Division[1], " Division, averaging ", opp_ppg, " PPG."),
        if (play_style == "Perimeter-Heavy")
          paste0("Their offense is perimeter-heavy, launching ", three_rate, "% of shots from deep at ", fg3_pct, "%.")
        else if (play_style == "Interior-Focused")
          paste0("They prefer interior work with a ", fg2_pct, "% 2P rate and only ", three_rate, "% threes.")
        else
          paste0("Balanced attack: ", three_rate, "% from 3 (", fg3_pct, "%) and ", fg2_pct, "% on twos."),
        if (nrow(key_players) >= 1) {
          p1 <- key_players[1, ]
          if (concentration == "Star-Dependent")
            paste0(p1$Player, " carries ", top_scorer_share, "% of scoring (", p1$player_ppg, " PPG). Contain them and this team struggles.")
          else if (top2_share >= 45 && nrow(key_players) >= 2) {
            p2 <- key_players[2, ]
            p2s <- if (opp_ppg > 0) round(p2$player_ppg / opp_ppg * 100, 0) else 0
            paste0(p1$Player, " (", p1$player_ppg, " PPG) and ", p2$Player, " (", p2$player_ppg, " PPG, ", p2s, "%) are both threats.")
          } else paste0(p1$Player, " leads at ", p1$player_ppg, " PPG but scoring is well-distributed.")
        } else "",
        paste0("For ", rv$my_team, ": ",
               if (kings_ppg > opp_ppg + 15) paste0("favorable matchup — we outscore them by ", round(kings_ppg - opp_ppg, 0), " PPG.")
               else if (kings_ppg > opp_ppg)  paste0("we hold a scoring edge (", kings_ppg, " vs ", opp_ppg, " PPG).")
               else if (opp_ppg > kings_ppg + 10) paste0("we face a scoring gap — tempo control is critical.")
               else paste0("this is a competitive matchup. Execution decides it."),
               " This is a ", tolower(threat), "-threat opponent.")
      )
      
      div(
        # Header
        div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                         "padding:28px 30px;border-radius:14px;margin-bottom:20px;overflow:hidden;position:relative;"),
            div(style="position:absolute;right:-15px;top:-15px;font-size:10em;opacity:0.03;font-weight:900;","\U0001f50d"),
            div(style="display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
                if (opp_logo != "")
                  tags$img(src=opp_logo,
                           style="width:80px;height:80px;border-radius:50%;border:3px solid rgba(255,255,255,0.3);object-fit:cover;background:white;"),
                div(
                  div("PRE-GAME SCOUTING REPORT",
                      style="font-size:0.7em;font-weight:600;opacity:0.6;letter-spacing:2px;"),
                  h2(opp_name, style="margin:5px 0 0 0;font-weight:800;font-size:1.8em;"),
                  div(paste0(opp_record$Division[1], " Division | ",
                             opp_record$W[1], "-", opp_record$L[1], " (", opp_wpct, "% win rate)"),
                      style="opacity:0.7;margin-top:4px;")
                ),
                div(style="margin-left:auto;text-align:right;",
                    div("THREAT LEVEL", style="font-size:0.7em;opacity:0.5;letter-spacing:1px;"),
                    div(threat,
                        style=paste0("font-size:1.8em;font-weight:800;color:", threat_color,
                                     ";background:rgba(255,255,255,0.1);padding:4px 20px;",
                                     "border-radius:8px;margin-top:4px;")))
            )
        ),
        
        # Quick stats row
        div(style="display:flex;gap:12px;margin-bottom:20px;flex-wrap:wrap;",
            lapply(list(
              list("\U0001f3c0","PPG",   opp_ppg,             "per game"),
              list("\U0001f4aa","RPG",   opp_rpg,             "per game"),
              list("\U0001f91d","APG",   opp_apg,             "per game"),
              list("\U0001f3af","2P%",   paste0(fg2_pct,"%"), "from inside"),
              list("\u2604\ufe0f","3P%", paste0(fg3_pct,"%"), paste0(three_rate,"% of shots")),
              list("\u2705",    "FT%",   paste0(ft_pct,"%"),  "from the line")
            ), function(item)
              div(style="flex:1;min-width:120px;background:white;border:1px solid #e2e8f0;
                       border-radius:10px;padding:14px;text-align:center;",
                  div(item[[1]], style="font-size:1.3em;"),
                  div(item[[3]], style="font-size:1.5em;font-weight:800;color:#1e293b;margin:4px 0;"),
                  div(item[[2]], style="font-size:0.75em;font-weight:700;color:#64748b;"),
                  div(item[[4]], style="font-size:0.65em;color:#94a3b8;margin-top:2px;"))
            )
        ),
        
        # Two-column main body
        div(style="display:flex;gap:20px;flex-wrap:wrap;",
            
            # Left column — key players + tendencies
            div(style="flex:1;min-width:320px;",
                div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;margin-bottom:16px;",
                    h4("\U0001f464 Key Players to Watch",
                       style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                    if (nrow(key_players) > 0) {
                      lapply(1:nrow(key_players), function(i) {
                        kp <- key_players[i, ]
                        is_prim   <- i <= 2 && kp$player_ppg >= opp_ppg * 0.15
                        s_icon    <- if (kp$streak > 15) "\U0001f525" else if (kp$streak < -15) "\u2744\ufe0f" else ""
                        s_text    <- if (kp$streak > 15) "HOT" else if (kp$streak < -15) "COLD" else ""
                        pl_share  <- if (opp_ppg > 0) round(kp$player_ppg / opp_ppg * 100, 0) else 0
                        bg_c      <- if (is_prim) "#fef2f2" else "#f8fafc"
                        bd_c      <- if (is_prim) "#b91c1c" else "#e2e8f0"
                        tc_c      <- if (is_prim) "#991b1b" else "#334155"
                        div(style=paste0("padding:12px;border-radius:10px;margin-bottom:8px;",
                                         "border-left:4px solid ", bd_c, ";background:", bg_c, ";"),
                            div(style="display:flex;justify-content:space-between;align-items:start;",
                                div(
                                  div(style="display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
                                      span(paste0("#", i),
                                           style="font-weight:800;color:#94a3b8;font-size:0.85em;"),
                                      strong(kp$Player, style=paste0("font-size:1em;color:", tc_c, ";")),
                                      span(paste0("(", kp$player_pos, ")"),
                                           style="color:#64748b;font-size:0.85em;"),
                                      if (s_text != "")
                                        span(paste0(s_icon, " ", s_text),
                                             style=paste0("font-size:0.7em;font-weight:700;",
                                                          "color:", if(s_text=="HOT")"#b91c1c" else "#3b82f6", ";",
                                                          "background:", if(s_text=="HOT")"#fef2f2" else "#eff6ff", ";",
                                                          "padding:1px 6px;border-radius:4px;"))
                                  ),
                                  div(style="display:flex;gap:8px;margin-top:6px;",
                                      span(paste0(kp$player_ppg, " PPG"),
                                           style=paste0("font-size:0.8em;font-weight:700;color:", p_col,
                                                        ";background:#fee2e2;padding:2px 6px;border-radius:4px;")),
                                      span(paste0(kp$player_rpg, " RPG"),
                                           style="font-size:0.8em;font-weight:700;color:#1e40af;background:#dbeafe;padding:2px 6px;border-radius:4px;"),
                                      span(paste0(kp$player_apg, " APG"),
                                           style="font-size:0.8em;font-weight:700;color:#b45309;background:#fef3c7;padding:2px 6px;border-radius:4px;")),
                                  div(paste0(kp$gp, " games played"),
                                      style="font-size:0.7em;color:#94a3b8;margin-top:4px;")
                                ),
                                if (pl_share >= 15)
                                  div(style="text-align:center;background:#b91c1c;color:white;padding:4px 10px;border-radius:8px;font-size:0.7em;font-weight:700;",
                                      paste0(pl_share, "%\nof PPG"))
                            )
                        )
                      })
                    } else p("No player data available", style="color:#94a3b8;text-align:center;")
                ),
                
                div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                    h4("\U0001f4ca Team Tendencies",
                       style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                    div(style="display:flex;gap:12px;margin-bottom:14px;",
                        div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                         if(play_style=="Perimeter-Heavy")"#dbeafe"
                                         else if(play_style=="Interior-Focused")"#fee2e2" else "#f0fdf4",";"),
                            div(play_style, style="font-weight:700;font-size:0.95em;color:#334155;"),
                            div(paste0(three_rate, "% of shots from 3"),
                                style="font-size:0.75em;color:#64748b;margin-top:2px;")),
                        div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                         if(concentration=="Star-Dependent")"#fef2f2"
                                         else if(concentration=="Balanced Scoring")"#f0fdf4" else "#fffbeb",";"),
                            div(concentration, style="font-weight:700;font-size:0.95em;color:#334155;"),
                            div(paste0("Top 2 = ", top2_share, "% of scoring"),
                                style="font-size:0.75em;color:#64748b;margin-top:2px;"))
                    ),
                    div(style="display:flex;justify-content:space-between;margin-bottom:6px;",
                        span("Scoring Distribution",
                             style="font-weight:600;color:#475569;font-size:0.85em;"),
                        span(paste0(opp_ppg, " PPG"),
                             style="font-weight:700;color:#334155;font-size:0.85em;")),
                    div(style="height:28px;border-radius:8px;overflow:hidden;display:flex;",
                        div(style=paste0("width:", pct_2, "%;background:", p_col,
                                         ";display:flex;align-items:center;justify-content:center;"),
                            if (pct_2 >= 15) span(paste0("2P ", pct_2, "%"),
                                                  style="color:white;font-size:0.65em;font-weight:700;")),
                        div(style=paste0("width:", pct_3, "%;background:", s_col,
                                         ";display:flex;align-items:center;justify-content:center;"),
                            if (pct_3 >= 15) span(paste0("3P ", pct_3, "%"),
                                                  style="color:white;font-size:0.65em;font-weight:700;")),
                        div(style=paste0("width:", pct_ft, "%;background:", a_col,
                                         ";display:flex;align-items:center;justify-content:center;"),
                            if (pct_ft >= 10) span(paste0("FT ", pct_ft, "%"),
                                                   style="color:#1e293b;font-size:0.65em;font-weight:700;")))
                )
            ),
            
            # Right column — strategy + head-to-head
            div(style="flex:1;min-width:320px;",
                div(style="background:white;border:2px solid #15803d;border-radius:12px;padding:20px;margin-bottom:16px;",
                    h4("\u2705 How to Beat Them",
                       style="margin:0 0 14px 0;color:#15803d;font-weight:700;"),
                    lapply(seq_along(strategy_points), function(i) {
                      sp       <- strategy_points[i]
                      is_alert <- grepl("\U0001f525", sp)
                      is_cold  <- grepl("\u2744",     sp)
                      is_warn  <- grepl("DANGER|MISMATCH|outscore", sp)
                      bg_s <- if (is_alert) "#fef2f2;border:1px solid #fecaca;"
                      else if (is_cold) "#eff6ff;border:1px solid #bfdbfe;"
                      else if (is_warn) "#fffbeb;border:1px solid #fde68a;"
                      else "#f0fdf4;border:1px solid #bbf7d0;"
                      nb <- if (is_alert) "#b91c1c" else if (is_cold) "#3b82f6"
                      else if (is_warn) "#d97706" else "#15803d"
                      tc_s <- if (is_alert) "#991b1b" else if (is_cold) "#1e40af"
                      else if (is_warn) "#92400e" else "#166534"
                      div(style=paste0("padding:10px 12px;border-radius:8px;margin-bottom:8px;",
                                       "display:flex;align-items:start;gap:10px;background:", bg_s),
                          div(style=paste0("min-width:24px;height:24px;border-radius:50%;",
                                           "display:flex;align-items:center;justify-content:center;",
                                           "font-weight:800;font-size:0.75em;color:white;background:", nb, ";"), i),
                          p(sp, style=paste0("margin:0;font-size:0.88em;font-weight:600;color:", tc_s, ";")))
                    })
                ),
                
                div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                    h4(paste0("\u2694\ufe0f ", rv$my_team, " vs ", opp_name),
                       style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                    lapply(list(
                      list("Record",
                           if (nrow(kings_record)>0) paste0(kings_record$W[1],"-",kings_record$L[1]) else "N/A",
                           paste0(opp_record$W[1],"-",opp_record$L[1]),
                           kings_wpct, opp_wpct),
                      list("PPG", kings_ppg, opp_ppg, kings_ppg, opp_ppg),
                      list("RPG", kings_rpg, opp_rpg, kings_rpg, opp_rpg),
                      list("APG", kings_apg, opp_apg, kings_apg, opp_apg)
                    ), function(comp) {
                      k_num <- suppressWarnings(as.numeric(comp[[4]]))
                      o_num <- suppressWarnings(as.numeric(comp[[5]]))
                      if (is.na(k_num)) k_num <- 50; if (is.na(o_num)) o_num <- 50
                      total <- k_num + o_num
                      k_pct <- if (total > 0) round(k_num / total * 100, 0) else 50
                      k_win <- k_num > o_num
                      div(style="margin-bottom:10px;",
                          div(style="display:flex;justify-content:space-between;margin-bottom:4px;",
                              span(paste0(rv$my_team, ": ", comp[[2]]),
                                   style=paste0("font-size:0.8em;font-weight:700;color:",
                                                if(k_win)"#15803d" else "#b91c1c",";")),
                              span(comp[[1]], style="font-size:0.75em;font-weight:600;color:#94a3b8;"),
                              span(paste0(comp[[3]], " :", opp_name),
                                   style=paste0("font-size:0.8em;font-weight:700;color:",
                                                if(!k_win)"#15803d" else "#b91c1c",";"))),
                          div(style="height:8px;border-radius:4px;display:flex;overflow:hidden;",
                              div(style=paste0("width:", k_pct, "%;background:", p_col, ";")),
                              div(style=paste0("width:", 100-k_pct, "%;background:#94a3b8;"))))
                    })
                )
            )
        ),
        
        # Narrative
        div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                         "border-radius:12px;padding:24px;margin-top:20px;"),
            h4("\U0001f4dd Game Preview",
               style="margin:0 0 12px 0;opacity:0.8;letter-spacing:0.5px;"),
            p(narrative, style="margin:0;line-height:1.8;font-size:0.95em;opacity:0.9;")
        ),
        
        # Footer
        div(style="text-align:center;margin-top:20px;padding:15px;color:#94a3b8;font-size:0.8em;",
            paste0("Generated ", format(Sys.time(), "%B %d, %Y at %I:%M %p"),
                   " | ", rv$my_team, " Scouting Dashboard"),
            div(style="margin-top:8px;",
                actionButton("print_report", "\U0001f5a8\ufe0f Print Report",
                             class="btn-sm",
                             style="background:#475569;color:white;border:none;padding:8px 20px;border-radius:8px;font-weight:600;",
                             onclick="window.print();"))
        )
      )
      
    }, error = function(e) {
      div(style="background:#fee2e2;border-left:4px solid #ef4444;padding:20px;border-radius:10px;margin:20px;",
          h4("Error generating scouting report", style="color:#991b1b;margin:0 0 8px 0;"),
          p(paste("Details:", e$message), style="color:#7f1d1d;margin:0;font-size:0.9em;"),
          p("Try selecting a different team, or refresh the data.",
            style="color:#991b1b;margin:8px 0 0 0;font-size:0.85em;"))
    })
  })
  
  # ============================================================================
  # PATCH 3 — paste BEFORE  } # end server
  # Restores these 4 outputs to exactly match the original Ohio Kings script,
  # with only rv$my_team / rv$team_cols substituted for hardcoded Ohio Kings refs.
  # Overrides all previous patch versions of these outputs.
  # ============================================================================
  
  # --------------------------------------------------------------------------
  # SITUATION SUCCESS — exact original
  # --------------------------------------------------------------------------
  output$situation_success <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    p <- rv$team_cols$primary
    
    if (input$situation_select == "")
      return(p("Select a situation to see success prediction",
               style = "text-align:center;color:#999;padding:20px;"))
    
    kings_lineup <- tryCatch(
      build_lineup(rv$my_team, input$situation_select, rv$aba_data),
      error = function(e) NULL)
    
    if (is.null(kings_lineup) || nrow(kings_lineup) == 0)
      return(p("No lineup data available", style = "text-align:center;color:#999;padding:20px;"))
    
    ppgs         <- kings_lineup$PPG;   ppgs[is.na(ppgs)] <- 0
    total_ppg_l  <- sum(ppgs)
    shot_weights <- if (total_ppg_l > 0) ppgs / total_ppg_l else rep(0.2, length(ppgs))
    primary_idx  <- which.max(shot_weights)
    primary_pl   <- kings_lineup$Player[primary_idx]
    primary_wt   <- round(shot_weights[primary_idx] * 100, 0)
    
    get_streaks_ss <- function(player_name) {
      tryCatch({
        if (player_name %in% names(rv$aba_data$all_player_stats)) {
          pd <- rv$aba_data$all_player_stats[[player_name]]
          if (!is.null(pd$game_log) && nrow(pd$game_log) >= 3 && !is.null(pd$per_game)) {
            rec     <- pd$game_log %>% arrange(desc(date)) %>% head(3)
            rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)), na.rm = TRUE)
            szn_avg <- pd$per_game$PPG[1]
            if (!is.na(rec_avg) && !is.na(szn_avg) && szn_avg > 0)
              return(round(((rec_avg - szn_avg) / szn_avg) * 100, 1))
          }
        }
        return(0)
      }, error = function(e) 0)
    }
    
    streaks    <- sapply(as.character(kings_lineup$Player), get_streaks_ss)
    hot_count  <- sum(streaks > 15)
    cold_count <- sum(streaks < -15)
    avg_streak <- round(mean(streaks), 1)
    mom_label  <- if (avg_streak > 10)   "\U0001f525 On Fire"
    else if (avg_streak > 0)   "\u2b06\ufe0f Trending Up"
    else if (avg_streak > -10) "\u27a1\ufe0f Steady"
    else "\u2744\ufe0f Cold Stretch"
    mom_color  <- if (avg_streak > 10) "#15803d" else if (avg_streak > 0) "#65a30d"
    else if (avg_streak > -10) "#d97706" else "#b91c1c"
    
    criteria <- input$situation_select
    fg2s     <- kings_lineup$FG2_pct; fg2s[is.na(fg2s)] <- 0
    fg3s     <- kings_lineup$FG3_pct; fg3s[is.na(fg3s)] <- 0
    fts      <- kings_lineup$FT_pct;  fts[is.na(fts)]   <- 0
    spgs     <- kings_lineup$SPG;     spgs[is.na(spgs)]  <- 0
    bpgs     <- kings_lineup$BPG;     bpgs[is.na(bpgs)]  <- 0
    rpgs     <- kings_lineup$RPG;     rpgs[is.na(rpgs)]  <- 0
    
    primary_stat <- switch(criteria,
                           "off_down3"    = paste0(round(sum(shot_weights * fg3s), 1), "%"),
                           "off_down2"    = paste0(round(sum(shot_weights * fg2s), 1), "%"),
                           "ft_situation" = paste0(round((fts[primary_idx] / 100)^2 * 100, 1), "%"),
                           "need_stop"    = paste0(round(min(95, (sum(spgs) + sum(bpgs)) / 70 * 100), 1), "%"),
                           "def_last"     = paste0(round(min(90, sum(rpgs) / 45 * 100), 1), "%"),
                           "scoring"      = round(total_ppg_l, 1)
    )
    
    primary_label <- switch(criteria,
                            "off_down3"    = "Est. probability of hitting a 3",
                            "off_down2"    = "Est. probability of hitting a 2",
                            "ft_situation" = paste0("Prob ", primary_pl, " makes both FTs"),
                            "need_stop"    = "Est. probability opponent misses",
                            "def_last"     = "Est. rebound probability if they miss",
                            "scoring"      = "Combined lineup PPG"
    )
    
    gauge_pct <- switch(criteria,
                        "off_down3"    = min(100, max(0, (sum(shot_weights * fg3s) - 15) / (50 - 15) * 100)),
                        "off_down2"    = min(100, max(0, (sum(shot_weights * fg2s) - 30) / (65 - 30) * 100)),
                        "ft_situation" = min(100, max(0, (fts[primary_idx] - 40) / (90 - 40) * 100)),
                        "need_stop"    = min(100, max(0, ((sum(spgs) + sum(bpgs)) / 5) * 100)),
                        "def_last"     = min(100, max(0, (sum(rpgs) - 20) / (60 - 20) * 100)),
                        "scoring"      = min(100, max(0, (total_ppg_l - 50) / (120 - 50) * 100))
    )
    
    grade     <- if (gauge_pct >= 75) "A" else if (gauge_pct >= 50) "B"
    else if (gauge_pct >= 25) "C" else "D"
    grade_col <- switch(grade, "A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c")
    grade_bg  <- switch(grade,
                        "A"="linear-gradient(135deg,#15803d,#22c55e)",
                        "B"="linear-gradient(135deg,#65a30d,#a3e635)",
                        "C"="linear-gradient(135deg,#d97706,#fbbf24)",
                        "D"="linear-gradient(135deg,#b91c1c,#f87171)")
    
    div(style = "display:flex;gap:20px;flex-wrap:wrap;",
        div(style = paste0("flex:1;min-width:250px;background:white;border:2px solid ",
                           grade_col, "25;border-radius:14px;padding:24px;text-align:center;",
                           "position:relative;box-shadow:0 4px 16px rgba(0,0,0,0.06);"),
            div(style = paste0("position:absolute;top:12px;right:12px;width:40px;height:40px;",
                               "background:", grade_bg, ";border-radius:50%;display:flex;",
                               "align-items:center;justify-content:center;font-weight:800;",
                               "font-size:1.1em;color:", if (grade %in% c("B","C")) "#1a1a2e" else "white", ";"),
                grade),
            h4("Situation Analysis", style = "margin:0 0 15px 0;color:#334155;font-weight:700;"),
            h2(primary_stat, style = paste0("margin:0;font-size:3.5em;font-weight:800;color:", grade_col, ";")),
            p(primary_label, style = "margin:5px 0 0 0;color:#64748b;font-weight:600;"),
            div(style = "margin:15px auto;max-width:250px;",
                div(style = "height:10px;background:#e2e8f0;border-radius:5px;overflow:hidden;",
                    div(style = paste0("width:", gauge_pct, "%;height:100%;background:", grade_col, ";border-radius:5px;"))),
                div(style = "display:flex;justify-content:space-between;margin-top:4px;",
                    span("Poor",  style = "font-size:0.65em;color:#94a3b8;"),
                    span("Elite", style = "font-size:0.65em;color:#94a3b8;")))),
        div(style = "flex:1;min-width:250px;display:flex;flex-direction:column;gap:12px;",
            div(style = paste0("background:", mom_color, "10;border-left:4px solid ", mom_color,
                               ";padding:16px;border-radius:10px;"),
                div(style = "display:flex;justify-content:space-between;align-items:center;",
                    div(div(mom_label, style = paste0("font-weight:700;font-size:1.1em;color:", mom_color, ";")),
                        div("Lineup Momentum", style = "font-size:0.8em;color:#64748b;margin-top:3px;")),
                    div(paste0(ifelse(avg_streak > 0, "+", ""), avg_streak, "%"),
                        style = paste0("font-size:1.3em;font-weight:800;color:", mom_color, ";")))),
            div(style = "background:#f8fafc;border:1px solid #e2e8f0;border-radius:10px;padding:14px;",
                div(style = "display:flex;justify-content:space-between;",
                    span("Primary Option", style = "font-weight:600;color:#334155;font-size:0.9em;"),
                    span(paste0("\u2b50 ", primary_wt, "% usage"),
                         style = paste0("font-weight:700;color:", p, ";font-size:0.85em;"))),
                div(primary_pl, style = paste0("font-size:1.1em;font-weight:800;color:", p, ";margin-top:6px;"))),
            div(style = "background:#f8fafc;border:1px solid #e2e8f0;border-radius:10px;padding:14px;",
                div(paste0(hot_count, " hot / ", cold_count, " cold / ",
                           5 - hot_count - cold_count, " steady"),
                    style = "font-size:0.85em;color:#475569;font-weight:600;")))
    )
  })
  
  # --------------------------------------------------------------------------
  # WIN PROBABILITY — exact original (simple: win%, bar, predicted score, records)
  # --------------------------------------------------------------------------
  output$win_probability <- renderUI({
    req(rv$logged_in, input$opponent_select, rv$aba_data, rv$my_team)
    p <- rv$team_cols$primary
    s <- rv$team_cols$secondary
    a <- rv$team_cols$accent
    
    if (input$opponent_select == "")
      return(p("Select an opponent to see win probability analysis",
               style = "text-align:center;color:#999;padding:20px;"))
    
    result <- calculate_unified_win_probability(
      NULL, input$opponent_select, rv$aba_data,
      include_schedule = TRUE, my_team_name = rv$my_team)
    
    win_prob     <- result$prob
    opp_win_prob <- 100 - win_prob
    
    kings_record <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
    opp_record   <- rv$aba_data$league_data %>% filter(Team == input$opponent_select)
    
    k_ppg <- get_accurate_team_ppg(rv$my_team,            rv$aba_data)
    o_ppg <- get_accurate_team_ppg(input$opponent_select,  rv$aba_data)
    
    avg_total   <- (k_ppg + o_ppg) / 2
    team_spread <- (win_prob - 50) / 50 * 12
    pred_kings  <- round(avg_total + team_spread / 2, 0)
    pred_opp    <- round(avg_total - team_spread / 2, 0)
    if (win_prob > 50 && pred_kings <= pred_opp) pred_kings <- pred_opp + 1
    if (win_prob < 50 && pred_opp <= pred_kings) pred_opp   <- pred_kings + 1
    
    if (!result$has_full_data) {
      div(
        div(style = "background:#fffbeb;border:2px solid #f59e0b;border-radius:10px;padding:14px;margin-bottom:15px;",
            div(style = "display:flex;align-items:center;gap:10px;",
                span("\u26a0\ufe0f", style = "font-size:1.5em;"),
                div(strong("Limited Data", style = "color:#92400e;"),
                    p(result$message, style = "margin:4px 0 0 0;color:#78350f;font-size:0.85em;")))),
        div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:15px;",
            div(h3(rv$my_team,    style = paste0("margin:0;color:", p, ";font-weight:800;")),
                h2(paste0(win_prob, "%"), style = paste0("margin:5px 0 0 0;color:", p, ";font-size:3em;"))),
            div(style = "text-align:center;", h1("VS", style = "margin:0;color:#1e293b;font-size:2.5em;")),
            div(style = "text-align:right;",
                h3(input$opponent_select, style = "margin:0;color:#ef4444;font-weight:800;"),
                h2(paste0(opp_win_prob, "%"), style = "margin:5px 0 0 0;color:#ef4444;font-size:3em;")))
      )
    } else {
      div(
        div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:15px;",
            div(h3(rv$my_team, style = paste0("margin:0;color:", p, ";font-weight:800;")),
                h2(paste0(win_prob, "%"), style = paste0("margin:5px 0 0 0;color:", p, ";font-size:3em;"))),
            div(style = "text-align:center;",
                h1("VS", style = "margin:0;color:#1e293b;font-size:2.5em;")),
            div(style = "text-align:right;",
                h3(input$opponent_select, style = "margin:0;color:#ef4444;font-weight:800;"),
                h2(paste0(opp_win_prob, "%"), style = "margin:5px 0 0 0;color:#ef4444;font-size:3em;"))),
        div(style = "height:40px;background:#e2e8f0;border-radius:20px;overflow:hidden;position:relative;",
            div(style = paste0("width:", win_prob, "%;height:100%;",
                               "background:linear-gradient(90deg,", p, ",", s, ");",
                               "display:flex;align-items:center;justify-content:flex-end;padding-right:15px;"),
                span(paste0(win_prob, "%"), style = "color:white;font-weight:bold;font-size:1.1em;")),
            div(style = "position:absolute;right:15px;top:50%;transform:translateY(-50%);color:#1e293b;font-weight:bold;font-size:1.1em;",
                paste0(opp_win_prob, "%"))),
        div(style = "display:flex;gap:15px;margin-top:15px;",
            div(style = paste0("flex:1;background:linear-gradient(135deg,#667eea,#764ba2);",
                               "color:white;padding:20px;border-radius:12px;text-align:center;"),
                h4("PREDICTED SCORE",
                   style = "margin:0 0 10px 0;opacity:0.8;font-size:0.85em;letter-spacing:1px;"),
                div(style = "display:flex;justify-content:center;align-items:center;gap:20px;",
                    div(div(rv$my_team,    style = "font-size:0.8em;opacity:0.8;"),
                        div(pred_kings, style = "font-size:3em;font-weight:800;line-height:1;")),
                    div("-", style = "font-size:2em;opacity:0.5;"),
                    div(div(input$opponent_select, style = "font-size:0.8em;opacity:0.8;"),
                        div(pred_opp, style = "font-size:3em;font-weight:800;line-height:1;")))),
            div(style = paste0("flex:1;background:linear-gradient(135deg,", s, ",#020f21);",
                               "color:white;padding:20px;border-radius:12px;"),
                div(paste0("PPG: ", k_ppg, " vs ", o_ppg),
                    style = "font-size:0.85em;opacity:0.8;margin-bottom:6px;"),
                if (nrow(kings_record) > 0 && nrow(opp_record) > 0)
                  div(paste0(rv$my_team, ": ", kings_record$W[1], "-", kings_record$L[1],
                             " | ", input$opponent_select, ": ",
                             opp_record$W[1], "-", opp_record$L[1]),
                      style = "font-size:0.85em;opacity:0.8;"))
        )
      )
    }
  })
  
  # --------------------------------------------------------------------------
  # PLAYER MATCHUPS — exact original (predicted stats inline, simple bar)
  # --------------------------------------------------------------------------
  output$player_matchups <- renderUI({
    req(rv$logged_in, input$situation_select, input$opponent_select, rv$aba_data, rv$my_team)
    p <- rv$team_cols$primary
    s <- rv$team_cols$secondary
    a <- rv$team_cols$accent
    
    if (input$situation_select == "" || input$opponent_select == "")
      return(p("Select a situation and opponent to see player matchup predictions",
               style = "text-align:center;color:#999;padding:20px;"))
    
    kings_lineup <- tryCatch(build_lineup(rv$my_team,            "scoring", rv$aba_data), error=function(e) NULL)
    opp_lineup   <- tryCatch(build_lineup(input$opponent_select, "scoring", rv$aba_data), error=function(e) NULL)
    
    if (is.null(kings_lineup) || is.null(opp_lineup) ||
        nrow(kings_lineup) == 0 || nrow(opp_lineup) == 0)
      return(p("Lineup data not available for matchup analysis",
               style = "text-align:center;color:#999;padding:20px;"))
    
    n_matchups <- min(nrow(kings_lineup), nrow(opp_lineup))
    
    div(
      lapply(1:n_matchups, function(i) {
        k <- kings_lineup[i, ]
        o <- opp_lineup[i, ]
        
        k_ppg <- if (is.na(k$PPG)) 0 else k$PPG
        k_rpg <- if (is.na(k$RPG)) 0 else k$RPG
        k_apg <- if (is.na(k$APG)) 0 else k$APG
        k_spg <- if (is.na(k$SPG)) 0 else k$SPG
        k_bpg <- if (is.na(k$BPG)) 0 else k$BPG
        o_ppg <- if (is.na(o$PPG)) 0 else o$PPG
        o_rpg <- if (is.na(o$RPG)) 0 else o$RPG
        o_apg <- if (is.na(o$APG)) 0 else o$APG
        o_spg <- if (is.na(o$SPG)) 0 else o$SPG
        o_bpg <- if (is.na(o$BPG)) 0 else o$BPG
        
        sc_adv <- 100 / (1 + exp(-0.15 * (k_ppg - o_ppg)))
        rb_adv <- 100 / (1 + exp(-0.3  * (k_rpg - o_rpg)))
        as_adv <- 100 / (1 + exp(-0.3  * (k_apg - o_apg)))
        df_adv <- 100 / (1 + exp(-0.5  * ((k_spg + k_bpg) - (o_spg + o_bpg))))
        overall <- round(sc_adv * 0.40 + rb_adv * 0.25 + as_adv * 0.20 + df_adv * 0.15, 1)
        opp_adv <- round(100 - overall, 1)
        winner  <- if (overall > 55) "kings" else if (overall < 45) "opp" else "even"
        
        k_pred <- tryCatch(predict_player_game(as.character(k$Player), input$opponent_select),
                           error = function(e) list(pts=NA,reb=NA,ast=NA))
        o_pred <- tryCatch(predict_player_game(as.character(o$Player), rv$my_team),
                           error = function(e) list(pts=NA,reb=NA,ast=NA))
        k_pred_pts <- if (!is.null(k_pred$pts) && !is.na(k_pred$pts)) k_pred$pts else k_ppg
        o_pred_pts <- if (!is.null(o_pred$pts) && !is.na(o_pred$pts)) o_pred$pts else o_ppg
        k_pred_reb <- if (!is.null(k_pred$reb) && !is.na(k_pred$reb)) k_pred$reb else k_rpg
        o_pred_reb <- if (!is.null(o_pred$reb) && !is.na(o_pred$reb)) o_pred$reb else o_rpg
        k_pred_ast <- if (!is.null(k_pred$ast) && !is.na(k_pred$ast)) k_pred$ast else k_apg
        o_pred_ast <- if (!is.null(o_pred$ast) && !is.na(o_pred$ast)) o_pred$ast else o_apg
        
        border_c <- if (winner=="kings") "#10b981" else if (winner=="opp") "#ef4444" else "#f59e0b"
        bg_c     <- if (winner=="kings") "#f0fdf4" else if (winner=="opp") "#fef2f2" else "#fffbeb"
        
        div(style = paste0("background:", bg_c, ";border-left:5px solid ", border_c,
                           ";padding:18px;margin:12px 0;border-radius:8px;",
                           "box-shadow:0 2px 6px rgba(0,0,0,0.08);"),
            div(style = "text-align:center;margin-bottom:12px;",
                span(paste0("-- ", k$lineup_position, " MATCHUP --"),
                     style = "font-weight:700;color:#64748b;font-size:0.85em;letter-spacing:1px;")),
            div(style = "display:flex;justify-content:space-between;align-items:center;",
                # Kings player — left
                div(style = "flex:1;",
                    div(strong(as.character(k$Player),
                               style = paste0("font-size:1.1em;color:", p, ";")),
                        span(paste0(" (", k$Position, ")"), style = "color:#64748b;font-size:0.9em;")),
                    div(style = "font-size:0.85em;color:#64748b;margin-top:4px;",
                        paste0("Season: ", k_ppg, " / ", k_rpg, " / ", k_apg)),
                    div(style = "display:flex;gap:4px;margin-top:4px;",
                        span(paste0(k_pred_pts, " PTS"),
                             style = paste0("font-size:0.75em;font-weight:700;color:white;background:", p,
                                            ";padding:2px 6px;border-radius:4px;")),
                        span(paste0(k_pred_reb, " REB"),
                             style = paste0("font-size:0.75em;font-weight:700;color:white;background:", s,
                                            ";padding:2px 6px;border-radius:4px;")),
                        span(paste0(k_pred_ast, " AST"),
                             style = paste0("font-size:0.75em;font-weight:700;color:#1e293b;background:", a,
                                            ";padding:2px 6px;border-radius:4px;")))),
                # Edge indicator — centre
                div(style = "flex:0 0 100px;text-align:center;",
                    div(if (winner=="kings") paste0(overall, "%")
                        else if (winner=="opp") paste0(opp_adv, "%") else "EVEN",
                        style = paste0("font-size:1.8em;font-weight:800;color:", border_c, ";")),
                    div(if (winner=="kings") "My Team Edge"
                        else if (winner=="opp") "Opp Edge" else "Toss-Up",
                        style = paste0("font-size:0.75em;font-weight:600;color:", border_c, ";"))),
                # Opp player — right
                div(style = "flex:1;text-align:right;",
                    div(strong(as.character(o$Player),
                               style = "font-size:1.1em;color:#991b1b;"),
                        span(paste0(" (", o$Position, ")"), style = "color:#64748b;font-size:0.9em;")),
                    div(style = "font-size:0.85em;color:#64748b;margin-top:4px;",
                        paste0("Season: ", o_ppg, " / ", o_rpg, " / ", o_apg)),
                    div(style = "display:flex;gap:4px;margin-top:4px;justify-content:flex-end;",
                        span(paste0(o_pred_pts, " PTS"),
                             style = "font-size:0.75em;font-weight:700;color:white;background:#991b1b;padding:2px 6px;border-radius:4px;"),
                        span(paste0(o_pred_reb, " REB"),
                             style = "font-size:0.75em;font-weight:700;color:white;background:#7f1d1d;padding:2px 6px;border-radius:4px;"),
                        span(paste0(o_pred_ast, " AST"),
                             style = "font-size:0.75em;font-weight:700;color:#1e293b;background:#fca5a5;padding:2px 6px;border-radius:4px;")))),
            div(style = "margin-top:10px;",
                div(style = "display:flex;align-items:center;gap:8px;",
                    span("My Team",
                         style = paste0("font-size:0.75em;font-weight:600;color:", p, ";width:60px;")),
                    div(style = "flex-grow:1;height:14px;background:#fee2e2;border-radius:7px;overflow:hidden;",
                        div(style = paste0("width:", overall, "%;height:100%;",
                                           "background:linear-gradient(90deg,", p, ",", s, ");",
                                           "border-radius:7px;"))),
                    span("Opp",
                         style = "font-size:0.75em;font-weight:600;color:#991b1b;width:30px;text-align:right;")))
        )
      })
    )
  })
  
  # --------------------------------------------------------------------------
  # SCOUTING REPORT — exact original, wrapped in tryCatch to prevent crash
  # --------------------------------------------------------------------------
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    p <- rv$team_cols$primary
    s <- rv$team_cols$secondary
    a <- rv$team_cols$accent
    
    if (is.null(input$scout_team) || input$scout_team == "")
      return(div(style = "text-align:center;padding:60px 20px;",
                 div("\U0001f50d", style = "font-size:4em;opacity:0.3;"),
                 h3("Select an opponent to generate scouting report",
                    style = "color:#94a3b8;margin-top:15px;"),
                 p("Choose a team from the dropdown above", style = "color:#cbd5e1;")))
    
    tryCatch({
      
      opp_name   <- input$scout_team
      opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
      opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
      
      if (is.null(opp_roster) || nrow(opp_record) == 0)
        return(p("No data available for this team",
                 style = "text-align:center;color:#999;padding:40px;"))
      
      opp_gp   <- as.numeric(opp_record$W[1]) + as.numeric(opp_record$L[1])
      opp_wpct <- if (opp_gp > 0) round(opp_record$W[1] / opp_gp * 100, 0) else 50
      
      # Local stat helper — no name conflict
      get_s <- function(roster, col) {
        tr <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
        if (nrow(tr) > 0 && col %in% names(tr)) return(suppressWarnings(as.numeric(tr[[col]][1])))
        pr <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
        if (col %in% names(pr)) return(sum(suppressWarnings(as.numeric(pr[[col]])), na.rm = TRUE))
        return(0)
      }
      
      opp_pts <- get_s(opp_roster,"PTS"); opp_reb <- get_s(opp_roster,"REB")
      opp_ast <- get_s(opp_roster,"AST")
      opp_2pm <- get_s(opp_roster,"2PM"); opp_2pa <- get_s(opp_roster,"2PA")
      opp_3pm <- get_s(opp_roster,"3PM"); opp_3pa <- get_s(opp_roster,"3PA")
      opp_ftm <- get_s(opp_roster,"FTM"); opp_fta <- get_s(opp_roster,"FTA")
      
      opp_ppg <- get_accurate_team_ppg(opp_name, rv$aba_data)
      if (opp_ppg == 0 && opp_gp > 0) opp_ppg <- round(opp_pts / opp_gp, 1)
      opp_rpg <- if (opp_gp > 0) round(opp_reb / opp_gp, 1) else 0
      opp_apg <- if (opp_gp > 0) round(opp_ast / opp_gp, 1) else 0
      
      fg2_pct    <- if (opp_2pa > 0) round(opp_2pm / opp_2pa * 100, 1) else 0
      fg3_pct    <- if (opp_3pa > 0) round(opp_3pm / opp_3pa * 100, 1) else 0
      ft_pct     <- if (opp_fta > 0) round(opp_ftm / opp_fta * 100, 1) else 0
      three_rate <- if ((opp_2pa + opp_3pa) > 0)
        round(opp_3pa / (opp_2pa + opp_3pa) * 100, 0) else 0
      play_style <- if (three_rate >= 40) "Perimeter-Heavy"
      else if (three_rate >= 25) "Balanced" else "Interior-Focused"
      
      player_roster <- opp_roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      
      key_players <- player_roster %>%
        mutate(
          player_ppg = sapply(Player, function(pl) { tryCatch({
            pg <- rv$aba_data$all_player_stats[[pl]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$PPG[1])) pg$PPG[1] else 0
          }, error=function(e) 0) }),
          player_rpg = sapply(Player, function(pl) { tryCatch({
            pg <- rv$aba_data$all_player_stats[[pl]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$RPG[1])) pg$RPG[1] else 0
          }, error=function(e) 0) }),
          player_apg = sapply(Player, function(pl) { tryCatch({
            pg <- rv$aba_data$all_player_stats[[pl]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$APG[1])) pg$APG[1] else 0
          }, error=function(e) 0) }),
          player_pos = sapply(Player, function(pl) { tryCatch({
            pos <- rv$aba_data$all_player_stats[[pl]]$position
            if (!is.null(pos) && !is.na(pos) && nchar(pos) > 0) pos else "G"
          }, error=function(e) "G") }),
          gp = sapply(Player, function(pl) { tryCatch({
            gl <- rv$aba_data$all_player_stats[[pl]]$game_log
            if (!is.null(gl)) nrow(gl) else 0
          }, error=function(e) 0) }),
          streak = sapply(Player, function(pl) { tryCatch({
            pd <- rv$aba_data$all_player_stats[[pl]]
            if (is.null(pd$game_log)||nrow(pd$game_log)<3||is.null(pd$per_game)) return(0)
            rec     <- pd$game_log %>% arrange(desc(date)) %>% head(3)
            rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)), na.rm=TRUE)
            szn_avg <- pd$per_game$PPG[1]
            if (!is.na(rec_avg) && !is.na(szn_avg) && szn_avg > 0)
              round(((rec_avg - szn_avg) / szn_avg) * 100, 1) else 0
          }, error=function(e) 0) })
        ) %>%
        filter(gp >= 3, player_ppg > 0) %>%
        arrange(desc(player_ppg)) %>%
        head(5)
      
      top_scorer_share <- if (opp_ppg > 0 && nrow(key_players) > 0)
        round(key_players$player_ppg[1] / opp_ppg * 100, 0) else 0
      top2_share <- if (opp_ppg > 0 && nrow(key_players) >= 2)
        round(sum(key_players$player_ppg[1:2]) / opp_ppg * 100, 0) else top_scorer_share
      
      concentration <- if (top_scorer_share >= 30) "Star-Dependent"
      else if (top2_share >= 50)   "Top-Heavy" else "Balanced Scoring"
      
      threat       <- if (opp_wpct >= 70) "HIGH" else if (opp_wpct >= 50) "MODERATE" else "LOW"
      threat_color <- if (threat=="HIGH") "#b91c1c" else if (threat=="MODERATE") "#d97706" else "#15803d"
      
      kings_ppg    <- get_accurate_team_ppg(rv$my_team, rv$aba_data)
      kings_record <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      kings_gp     <- if (nrow(kings_record) > 0) as.numeric(kings_record$W[1]) + as.numeric(kings_record$L[1]) else 0
      kings_wpct   <- if (kings_gp > 0) round(kings_record$W[1] / kings_gp * 100, 0) else 50
      kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]]
      kings_rpg    <- if (!is.null(kings_roster) && kings_gp > 0)
        round(get_s(kings_roster,"REB") / kings_gp, 1) else 0
      
      hot_players  <- key_players %>% filter(streak > 15)
      cold_players <- key_players %>% filter(streak < -15)
      
      opp_logo <- ""
      tryCatch({
        if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
          lr <- rv$aba_data$team_lookup %>% filter(team_name == opp_name) %>% pull(logo_url)
          if (length(lr) > 0 && !is.na(lr[1]) && nchar(lr[1]) > 0) opp_logo <- lr[1]
        }
      }, error = function(e) {})
      
      # Strategy points — identical to original
      strategy_points <- c()
      if (nrow(key_players) >= 1) {
        significant <- key_players %>% filter(player_ppg >= opp_ppg * 0.15)
        n_threats   <- nrow(significant)
        if (n_threats == 1) {
          strategy_points <- c(strategy_points,
                               paste0("Contain ", significant$Player[1], " (", significant$player_ppg[1],
                                      " PPG, ", top_scorer_share, "% of scoring) - team has one dominant option"))
        } else if (n_threats == 2) {
          p2_share <- if (opp_ppg > 0) round(significant$player_ppg[2] / opp_ppg * 100, 0) else 0
          strategy_points <- c(strategy_points,
                               paste0("Two threats: ", significant$Player[1], " (", significant$player_ppg[1],
                                      " PPG) and ", significant$Player[2], " (", significant$player_ppg[2],
                                      " PPG, ", p2_share, "%) - cannot focus on just one"))
        } else if (n_threats >= 3) {
          strategy_points <- c(strategy_points,
                               paste0("Deep scoring team (", n_threats, " players at 15%+ of team scoring) - prioritize team defense"))
        }
      }
      if (three_rate >= 35 && fg3_pct >= 35) {
        strategy_points <- c(strategy_points,
                             paste0("DANGER: ", three_rate, "% of shots from deep at ", fg3_pct, "% - close out hard on every three"))
      } else if (three_rate < 25) {
        strategy_points <- c(strategy_points,
                             paste0("Rarely shoots 3s (", three_rate, "%) - sag off perimeter, clog driving lanes"))
      } else {
        strategy_points <- c(strategy_points,
                             paste0("Balanced attack: ", three_rate, "% from 3 (", fg3_pct, "%), ", fg2_pct, "% from 2 - play honest defense"))
      }
      if (opp_rpg > kings_rpg + 3) {
        strategy_points <- c(strategy_points,
                             paste0("REBOUNDING MISMATCH: They average ", opp_rpg, " RPG vs our ",
                                    kings_rpg, " - must crash boards harder"))
      } else if (kings_rpg > opp_rpg + 3) {
        strategy_points <- c(strategy_points,
                             paste0("We dominate the boards (", kings_rpg, " vs ", opp_rpg, " RPG) - push pace off defensive rebounds"))
      }
      if (ft_pct >= 75) {
        strategy_points <- c(strategy_points,
                             paste0("Strong FT shooting (", ft_pct, "%) - avoid putting them on the line late"))
      } else if (ft_pct < 60) {
        strategy_points <- c(strategy_points,
                             paste0("Poor FT shooting (", ft_pct, "%) - consider strategic fouling in late-game situations"))
      }
      if (nrow(hot_players) > 0) {
        strategy_points <- c(strategy_points,
                             paste0("\U0001f525 HOT: ", paste(hot_players$Player, collapse = ", "),
                                    " trending above season avg - extra attention needed"))
      }
      if (nrow(cold_players) > 0) {
        strategy_points <- c(strategy_points,
                             paste0("\u2744\ufe0f Cold stretch: ", paste(cold_players$Player, collapse = ", "),
                                    " - still dangerous, but may be less aggressive"))
      }
      if (kings_ppg > opp_ppg + 10) {
        strategy_points <- c(strategy_points,
                             paste0("We outscore them by ", round(kings_ppg - opp_ppg, 0), " PPG - push pace, make this a track meet"))
      } else if (opp_ppg > kings_ppg + 10) {
        strategy_points <- c(strategy_points,
                             paste0("They outscore us by ", round(opp_ppg - kings_ppg, 0), " PPG - slow game down, limit possessions"))
      }
      
      div(
        # Report header
        div(style = paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                           "padding:30px;border-radius:14px;margin-bottom:20px;"),
            div(style = "display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
                if (opp_logo != "")
                  tags$img(src = opp_logo,
                           style = "width:80px;height:80px;border-radius:50%;border:3px solid rgba(255,255,255,0.3);object-fit:cover;background:white;"),
                div(
                  div("PRE-GAME SCOUTING REPORT",
                      style = "font-size:0.75em;font-weight:600;opacity:0.6;letter-spacing:2px;"),
                  h2(opp_name, style = "margin:5px 0 0 0;font-weight:800;font-size:1.8em;"),
                  div(paste0(opp_record$Division[1], " Division | ",
                             opp_record$W[1], "-", opp_record$L[1],
                             " (", opp_wpct, "% win rate)"),
                      style = "opacity:0.7;margin-top:4px;")
                ),
                div(style = "margin-left:auto;text-align:right;",
                    div("THREAT LEVEL", style = "font-size:0.7em;opacity:0.5;letter-spacing:1px;"),
                    div(threat,
                        style = paste0("font-size:1.8em;font-weight:800;color:", threat_color,
                                       ";background:rgba(255,255,255,0.1);padding:4px 20px;",
                                       "border-radius:8px;margin-top:4px;")))
            )
        ),
        
        # Quick stats row
        div(style = "display:flex;gap:12px;margin-bottom:20px;flex-wrap:wrap;",
            lapply(list(
              list("\U0001f3c0","PPG",   opp_ppg,             "per game"),
              list("\U0001f4aa","RPG",   opp_rpg,             "per game"),
              list("\U0001f91d","APG",   opp_apg,             "per game"),
              list("\U0001f3af","2P%",   paste0(fg2_pct,"%"), "from inside"),
              list("\u2604\ufe0f","3P%", paste0(fg3_pct,"%"), paste0(three_rate,"% of shots")),
              list("\u2705",    "FT%",   paste0(ft_pct,"%"),  "from the line")
            ), function(item)
              div(style = "flex:1;min-width:120px;background:white;border:1px solid #e2e8f0;border-radius:10px;padding:14px;text-align:center;",
                  div(item[[1]], style = "font-size:1.3em;"),
                  div(item[[3]], style = "font-size:1.5em;font-weight:800;color:#1e293b;margin:4px 0;"),
                  div(item[[2]], style = "font-size:0.75em;font-weight:700;color:#64748b;"),
                  div(item[[4]], style = "font-size:0.65em;color:#94a3b8;margin-top:2px;"))
            )
        ),
        
        # Two-column layout
        div(style = "display:flex;gap:20px;flex-wrap:wrap;",
            
            # Left: key players + tendencies
            div(style = "flex:1;min-width:350px;",
                div(style = "background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;margin-bottom:16px;",
                    h4("\U0001f464 Key Players to Watch",
                       style = "margin:0 0 14px 0;color:#334155;font-weight:700;"),
                    if (nrow(key_players) > 0) {
                      lapply(1:nrow(key_players), function(i) {
                        kp           <- key_players[i, ]
                        is_primary   <- i <= 2 && kp$player_ppg >= opp_ppg * 0.15
                        streak_icon  <- if (kp$streak > 15) "\U0001f525" else if (kp$streak < -15) "\u2744\ufe0f" else ""
                        streak_text  <- if (kp$streak > 15) "HOT" else if (kp$streak < -15) "COLD" else ""
                        player_share <- if (opp_ppg > 0) round(kp$player_ppg / opp_ppg * 100, 0) else 0
                        bg_c <- if (is_primary) "#fef2f2" else "#f8fafc"
                        bd_c <- if (is_primary) "#b91c1c" else "#e2e8f0"
                        tc_c <- if (is_primary) "#991b1b" else "#334155"
                        div(style = paste0("padding:12px;border-radius:10px;margin-bottom:8px;",
                                           "border-left:4px solid ", bd_c, ";background:", bg_c, ";"),
                            div(style = "display:flex;justify-content:space-between;align-items:start;",
                                div(
                                  div(style = "display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
                                      span(paste0("#", i), style = "font-weight:800;color:#94a3b8;font-size:0.85em;"),
                                      strong(kp$Player, style = paste0("font-size:1em;color:", tc_c, ";")),
                                      span(paste0("(", kp$player_pos, ")"), style = "color:#64748b;font-size:0.85em;"),
                                      if (streak_text != "")
                                        span(paste0(streak_icon, " ", streak_text),
                                             style = paste0("font-size:0.7em;font-weight:700;color:",
                                                            if(streak_text=="HOT")"#b91c1c" else "#3b82f6",
                                                            ";background:",
                                                            if(streak_text=="HOT")"#fef2f2" else "#eff6ff",
                                                            ";padding:1px 6px;border-radius:4px;"))
                                  ),
                                  div(style = "display:flex;gap:8px;margin-top:6px;",
                                      span(paste0(kp$player_ppg, " PPG"),
                                           style = paste0("font-size:0.8em;font-weight:700;color:", p,
                                                          ";background:#fee2e2;padding:2px 6px;border-radius:4px;")),
                                      span(paste0(kp$player_rpg, " RPG"),
                                           style = "font-size:0.8em;font-weight:700;color:#1e40af;background:#dbeafe;padding:2px 6px;border-radius:4px;"),
                                      span(paste0(kp$player_apg, " APG"),
                                           style = "font-size:0.8em;font-weight:700;color:#b45309;background:#fef3c7;padding:2px 6px;border-radius:4px;")),
                                  div(paste0(kp$gp, " games played"),
                                      style = "font-size:0.7em;color:#94a3b8;margin-top:4px;")
                                ),
                                if (player_share >= 15)
                                  div(style = paste0("text-align:center;background:#b91c1c;",
                                                     "color:white;padding:4px 10px;border-radius:8px;",
                                                     "font-size:0.7em;font-weight:700;white-space:nowrap;"),
                                      paste0(player_share, "% of\nteam PPG"))
                            )
                        )
                      })
                    } else p("No player data available", style = "color:#94a3b8;text-align:center;")
                ),
                
                div(style = "background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                    h4("\U0001f4ca Team Tendencies",
                       style = "margin:0 0 14px 0;color:#334155;font-weight:700;"),
                    div(style = "display:flex;gap:12px;margin-bottom:14px;",
                        div(style = paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                           if (play_style=="Perimeter-Heavy") "#dbeafe"
                                           else if (play_style=="Interior-Focused") "#fee2e2"
                                           else "#f0fdf4", ";"),
                            div(play_style, style = "font-weight:700;font-size:0.95em;color:#334155;"),
                            div(paste0(three_rate, "% of shots from 3"),
                                style = "font-size:0.75em;color:#64748b;margin-top:2px;")),
                        div(style = paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                           if (concentration=="Star-Dependent") "#fef2f2"
                                           else if (concentration=="Balanced Scoring") "#f0fdf4"
                                           else "#fffbeb", ";"),
                            div(concentration, style = "font-weight:700;font-size:0.95em;color:#334155;"),
                            div(paste0("Top 2 = ", top2_share, "% of scoring"),
                                style = "font-size:0.75em;color:#64748b;margin-top:2px;"))
                    )
                )
            ),
            
            # Right: strategy + head-to-head
            div(style = "flex:1;min-width:350px;",
                div(style = "background:white;border:2px solid #15803d;border-radius:12px;padding:20px;margin-bottom:16px;",
                    h4("\u2705 How to Beat Them",
                       style = "margin:0 0 14px 0;color:#15803d;font-weight:700;"),
                    lapply(seq_along(strategy_points), function(i) {
                      sp       <- strategy_points[i]
                      is_alert <- grepl("\U0001f525", sp)
                      is_cold  <- grepl("\u2744",     sp)
                      is_warn  <- grepl("DANGER|MISMATCH|outscore", sp)
                      bg_s <- if (is_alert) "#fef2f2;border:1px solid #fecaca;"
                      else if (is_cold) "#eff6ff;border:1px solid #bfdbfe;"
                      else if (is_warn) "#fffbeb;border:1px solid #fde68a;"
                      else "#f0fdf4;border:1px solid #bbf7d0;"
                      nb   <- if (is_alert) "#b91c1c" else if (is_cold) "#3b82f6"
                      else if (is_warn) "#d97706" else "#15803d"
                      tc_s <- if (is_alert) "#991b1b" else if (is_cold) "#1e40af"
                      else if (is_warn) "#92400e" else "#166534"
                      div(style = paste0("padding:10px 12px;border-radius:8px;margin-bottom:8px;",
                                         "display:flex;align-items:start;gap:10px;background:", bg_s),
                          div(style = paste0("width:24px;height:24px;border-radius:50%;flex-shrink:0;",
                                             "display:flex;align-items:center;justify-content:center;",
                                             "font-weight:800;font-size:0.75em;color:white;background:", nb, ";"), i),
                          p(sp, style = paste0("margin:0;font-size:0.88em;font-weight:600;color:", tc_s, ";")))
                    })
                ),
                
                div(style = "background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                    h4(paste0("\u2694\ufe0f ", rv$my_team, " vs ", opp_name),
                       style = "margin:0 0 14px 0;color:#334155;font-weight:700;"),
                    lapply(list(
                      list("Record",
                           if (nrow(kings_record)>0) paste0(kings_record$W[1],"-",kings_record$L[1]) else "N/A",
                           paste0(opp_record$W[1],"-",opp_record$L[1]),
                           kings_wpct, opp_wpct),
                      list("PPG", kings_ppg, opp_ppg, kings_ppg, opp_ppg),
                      list("RPG", kings_rpg, opp_rpg, kings_rpg, opp_rpg)
                    ), function(comp) {
                      k_num <- suppressWarnings(as.numeric(comp[[4]]))
                      o_num <- suppressWarnings(as.numeric(comp[[5]]))
                      if (is.na(k_num)) k_num <- 50; if (is.na(o_num)) o_num <- 50
                      total       <- k_num + o_num
                      k_pct       <- if (total > 0) round(k_num / total * 100, 0) else 50
                      k_wins_comp <- k_num > o_num
                      div(style = "margin-bottom:10px;",
                          div(style = "display:flex;justify-content:space-between;margin-bottom:4px;",
                              span(paste0(rv$my_team, ": ", comp[[2]]),
                                   style = paste0("font-size:0.8em;font-weight:700;color:",
                                                  if (k_wins_comp) "#15803d" else "#b91c1c", ";")),
                              span(comp[[1]], style = "font-size:0.75em;font-weight:600;color:#94a3b8;"),
                              span(paste0(comp[[3]], " :", opp_name),
                                   style = paste0("font-size:0.8em;font-weight:700;color:",
                                                  if (!k_wins_comp) "#15803d" else "#b91c1c", ";"))),
                          div(style = "height:8px;border-radius:4px;display:flex;overflow:hidden;",
                              div(style = paste0("width:", k_pct, "%;background:", p, ";")),
                              div(style = paste0("width:", 100-k_pct, "%;background:#94a3b8;"))))
                    })
                )
            )
        ),
        
        # Footer
        div(style = "text-align:center;margin-top:20px;padding:15px;color:#94a3b8;font-size:0.8em;",
            paste0("Generated ", format(Sys.time(), "%B %d, %Y at %I:%M %p"),
                   " | ", rv$my_team, " Scouting Dashboard"))
      )
      
    }, error = function(e) {
      div(style = "background:#fee2e2;border-left:4px solid #ef4444;padding:20px;border-radius:10px;margin:20px;",
          h4("Error loading scouting report", style = "color:#991b1b;margin:0 0 8px 0;"),
          p(paste("Details:", conditionMessage(e)),
            style = "color:#7f1d1d;margin:0;font-size:0.9em;"),
          p("Try selecting a different team or refreshing data.",
            style = "color:#991b1b;margin:8px 0 0 0;"))
    })
  })
  
  # ============================================================================
  # PATCH 5 — paste BEFORE  } # end server
  # This is the EXACT original Ohio Kings code from the source script.
  # The ONLY changes made are:
  #   "Ohio Kings"           -> rv$my_team
  #   ohio_kings_primary     -> ohio_kings_primary  (set at top of each output)
  #   ohio_kings_secondary   -> ohio_kings_secondary
  #   ohio_kings_accent      -> ohio_kings_accent
  #   build_lineup("Ohio Kings",...) -> build_lineup(rv$my_team,...)
  #   predict_player_game(o$Player, "Ohio Kings") -> predict_player_game(o$Player, rv$my_team)
  #   get_bench_score("Ohio Kings",...) calls -> use rv$my_team
  #   "Kings" text labels -> rv$my_team
  # All logic, structure, and UI is otherwise identical to the original.
  # ============================================================================
  
  output$situation_success <- renderUI({
    req(rv$logged_in, input$situation_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$situation_select == "") {
      return(p("Select a situation to see success prediction",
               style = "text-align: center; color: #999; padding: 20px;"))
    }
    
    current_criteria <- input$situation_select
    kings_lineup <- tryCatch(build_lineup(rv$my_team, current_criteria, rv$aba_data), error=function(e) NULL)
    
    if (is.null(kings_lineup) || nrow(kings_lineup) == 0)
      return(p("No lineup data available", style = "text-align: center; color: #999; padding: 20px;"))
    
    # Get opponent lineup (opposite situation)
    opp_lineup_ss <- NULL
    has_opp_ss <- !is.null(input$opponent_select) && input$opponent_select != ""
    if (has_opp_ss) {
      opp_criteria <- switch(current_criteria,
                             "off_down3"="need_stop","off_down2"="need_stop","need_stop"="scoring",
                             "def_last"="scoring","scoring"="need_stop","ft_situation"="ft_situation",
                             current_criteria)
      opp_lineup_ss <- tryCatch(build_lineup(input$opponent_select, opp_criteria, rv$aba_data), error=function(e) NULL)
    }
    
    # Calculate opponent defensive impact
    opp_def_factor <- 1.0
    opp_def_text <- NULL
    if (!is.null(opp_lineup_ss) && nrow(opp_lineup_ss) > 0) {
      opp_spg <- sum(opp_lineup_ss$SPG, na.rm=TRUE)
      opp_bpg <- sum(opp_lineup_ss$BPG, na.rm=TRUE)
      opp_rpg <- sum(opp_lineup_ss$RPG, na.rm=TRUE)
      opp_def_plays <- opp_spg + opp_bpg
      opp_def_factor <- max(0.85, 1 - (opp_def_plays - 3) * 0.025)
      opp_total_ppg <- get_accurate_team_ppg(input$opponent_select, rv$aba_data)
      if (opp_total_ppg == 0) opp_total_ppg <- sum(opp_lineup_ss$PPG, na.rm=TRUE)
      kings_on_offense <- current_criteria %in% c("off_down3","off_down2","scoring","ft_situation")
      if (kings_on_offense) {
        opp_situation_label <- switch(current_criteria,
                                      "off_down3"="protecting lead (defensive lineup)","off_down2"="protecting lead (defensive lineup)",
                                      "scoring"="in defensive mode","ft_situation"="standard lineup","responding")
        opp_threat_label <- "Defense"; opp_threat_val <- opp_def_plays
        opp_threat_detail <- paste0(round(opp_spg,1)," STL/g | ",round(opp_bpg,1)," BLK/g | ",round(opp_rpg,1)," RPG")
        opp_threat_quality <- if(opp_def_plays>=5)"Elite" else if(opp_def_plays>=3.5)"Good" else if(opp_def_plays>=2)"Average" else "Weak"
        opp_threat_color <- if(opp_def_plays>=5)"#b91c1c" else if(opp_def_plays>=3.5)"#d97706" else if(opp_def_plays>=2)"#65a30d" else "#15803d"
      } else {
        opp_situation_label <- switch(current_criteria,
                                      "need_stop"="trying to score (offensive lineup)","def_last"="trying to extend lead (offensive lineup)","responding")
        opp_threat_label <- "Offense"; opp_threat_val <- opp_total_ppg
        opp_threat_detail <- paste0(round(opp_total_ppg,1)," combined PPG | Top: ",
                                    as.character(opp_lineup_ss$Player[which.max(opp_lineup_ss$PPG)])," (",max(opp_lineup_ss$PPG)," PPG)")
        opp_threat_quality <- if(opp_total_ppg>=90)"Elite" else if(opp_total_ppg>=70)"Good" else if(opp_total_ppg>=50)"Average" else "Weak"
        opp_threat_color <- if(opp_total_ppg>=90)"#b91c1c" else if(opp_total_ppg>=70)"#d97706" else if(opp_total_ppg>=50)"#65a30d" else "#15803d"
      }
    }
    
    # Apply swaps
    if (length(rv_custom_lineup$swaps) > 0) {
      for (slot_str in names(rv_custom_lineup$swaps)) {
        slot_num <- suppressWarnings(as.integer(slot_str))
        swap_name <- rv_custom_lineup$swaps[[slot_str]]
        if (!is.na(slot_num) && slot_num <= nrow(kings_lineup) && !is.null(swap_name)) {
          if (swap_name %in% names(rv$aba_data$all_player_stats)) {
            pg <- rv$aba_data$all_player_stats[[swap_name]]$per_game
            if (!is.null(pg) && nrow(pg) > 0) {
              kings_lineup$Player[slot_num] <- swap_name
              kings_lineup$PPG[slot_num] <- pg$PPG[1]
              kings_lineup$RPG[slot_num] <- pg$RPG[1]
              kings_lineup$APG[slot_num] <- pg$APG[1]
              kings_lineup$SPG[slot_num] <- pg$SPG[1]
              kings_lineup$BPG[slot_num] <- pg$BPG[1]
              if (!is.na(pg$FG2_pct[1])) kings_lineup$FG2_pct[slot_num] <- pg$FG2_pct[1]
              if (!is.na(pg$FG3_pct[1])) kings_lineup$FG3_pct[slot_num] <- pg$FG3_pct[1]
              if (!is.na(pg$FT_pct[1]))  kings_lineup$FT_pct[slot_num]  <- pg$FT_pct[1]
            }
          }
        }
      }
    }
    
    # Get player streaks
    get_streak <- function(player_name) {
      tryCatch({
        if (player_name %in% names(rv$aba_data$all_player_stats)) {
          p_data <- rv$aba_data$all_player_stats[[player_name]]
          if (!is.null(p_data$game_log) && nrow(p_data$game_log) >= 3 && !is.null(p_data$per_game)) {
            recent <- p_data$game_log %>% arrange(desc(date)) %>% head(3)
            recent_avg <- mean(suppressWarnings(as.numeric(recent$PTS)), na.rm=TRUE)
            season_avg <- p_data$per_game$PPG[1]
            if (!is.na(recent_avg) && !is.na(season_avg) && season_avg > 0)
              return(round(((recent_avg - season_avg) / season_avg) * 100, 1))
          }
        }
        return(0)
      }, error=function(e) 0)
    }
    
    streaks <- sapply(as.character(kings_lineup$Player), get_streak)
    hot_count <- sum(streaks > 15); cold_count <- sum(streaks < -15)
    avg_streak <- round(mean(streaks), 1)
    momentum_label <- if(avg_streak>10)"\U0001f525 On Fire" else if(avg_streak>0)"\u2b06\ufe0f Trending Up"
    else if(avg_streak>-10)"\u27a1\ufe0f Steady" else "\u2744\ufe0f Cold Stretch"
    momentum_color <- if(avg_streak>10)"#15803d" else if(avg_streak>0)"#65a30d"
    else if(avg_streak>-10)"#d97706" else "#b91c1c"
    
    ppgs <- kings_lineup$PPG; ppgs[is.na(ppgs)] <- 0
    total_ppg <- sum(ppgs)
    shot_weights <- if(total_ppg>0) ppgs/total_ppg else rep(0.2,length(ppgs))
    primary_idx <- which.max(shot_weights)
    primary_player <- as.character(kings_lineup$Player[primary_idx])
    primary_weight <- round(shot_weights[primary_idx]*100, 0)
    
    fg2s <- kings_lineup$FG2_pct; fg2s[is.na(fg2s)] <- 0
    fg3s <- kings_lineup$FG3_pct; fg3s[is.na(fg3s)] <- 0
    fts  <- kings_lineup$FT_pct;  fts[is.na(fts)]   <- 0
    spgs <- kings_lineup$SPG;     spgs[is.na(spgs)]  <- 0
    bpgs <- kings_lineup$BPG;     bpgs[is.na(bpgs)]  <- 0
    rpgs <- kings_lineup$RPG;     rpgs[is.na(rpgs)]  <- 0
    
    situation_data <- switch(current_criteria,
                             "off_down3" = {
                               three_attempts <- sapply(as.character(kings_lineup$Player), function(p) {
                                 tryCatch({
                                   if (p %in% names(rv$aba_data$all_player_stats)) {
                                     gl <- rv$aba_data$all_player_stats[[p]]$game_log
                                     if (!is.null(gl) && "3PA" %in% names(gl))
                                       return(sum(suppressWarnings(as.numeric(gl$`3PA`)), na.rm=TRUE))
                                   }; return(0)
                                 }, error=function(e) 0)
                               })
                               min_3pa <- 15; league_avg_3 <- 32
                               reliable_shooter <- three_attempts >= min_3pa
                               adjusted_3pcts <- ifelse(reliable_shooter, fg3s,
                                                        ifelse(three_attempts==0, 0,
                                                               (fg3s*three_attempts+league_avg_3*min_3pa)/(three_attempts+min_3pa)))
                               raw_pct <- sum(shot_weights * adjusted_3pcts)
                               single_poss_pct <- round(raw_pct * opp_def_factor, 1)
                               reliable_idx <- which(reliable_shooter & fg3s > 0)
                               best_3_idx <- if(length(reliable_idx)>0) reliable_idx[which.max(fg3s[reliable_idx])] else which.max(three_attempts)
                               best_3_player <- as.character(kings_lineup$Player[best_3_idx])
                               best_3_pct <- fg3s[best_3_idx]; best_3_attempts <- three_attempts[best_3_idx]
                               optimal_pct <- round(best_3_pct,1); improvement <- round(optimal_pct - single_poss_pct, 1)
                               all_3pct <- sapply(rv$aba_data$all_player_stats, function(p) {
                                 if(!is.null(p$per_game)&&nrow(p$per_game)>0) p$per_game$FG3_pct[1] else NA})
                               league_avg_3_display <- round(mean(all_3pct[!is.na(all_3pct)&all_3pct>0], na.rm=TRUE), 1)
                               list(title="Down 3 \u2014 One Possession to Hit a Three",
                                    primary_stat=paste0(single_poss_pct,"%"),
                                    primary_label="Probability of hitting a 3 (weighted by shot distribution)",
                                    secondary_stat=paste0(optimal_pct,"%"),
                                    secondary_label=paste0("If drawn for ",best_3_player," (",round(best_3_pct,1),"% on ",best_3_attempts," att",
                                                           if(!reliable_shooter[best_3_idx])" \u2014 small sample" else "",")"),
                                    detail=paste0("\U0001f3af Primary option: ",primary_player,
                                                  " (",round(adjusted_3pcts[primary_idx],1),"% adj 3P% | ",
                                                  three_attempts[primary_idx]," att | ~",primary_weight,"% shot share)",
                                                  if(!reliable_shooter[primary_idx]) " \u26a0\ufe0f small sample" else ""),
                                    detail2=if(improvement>2) paste0("\u2b06\ufe0f +",improvement,"% boost if you go to ",best_3_player," instead") else NULL,
                                    vs_league=paste0("League avg 3P%: ",league_avg_3_display,"%"),
                                    vs_diff=single_poss_pct-league_avg_3_display,
                                    gauge_pct=min(100,max(0,(single_poss_pct-15)/(50-15)*100)))
                             },
                             "off_down2" = {
                               two_pcts <- fg2s
                               raw_pct <- sum(shot_weights * two_pcts)
                               single_poss_pct <- round(raw_pct * opp_def_factor, 1)
                               best_2_idx <- which.max(two_pcts); best_2_player <- as.character(kings_lineup$Player[best_2_idx])
                               best_2_pct <- two_pcts[best_2_idx]; optimal_pct <- round(best_2_pct,1)
                               improvement <- round(optimal_pct - single_poss_pct, 1)
                               all_2pct <- sapply(rv$aba_data$all_player_stats, function(p) {
                                 if(!is.null(p$per_game)&&nrow(p$per_game)>0) p$per_game$FG2_pct[1] else NA})
                               league_avg_2 <- round(mean(all_2pct[!is.na(all_2pct)&all_2pct>0], na.rm=TRUE), 1)
                               list(title="Down 2 \u2014 One Possession to Tie or Take the Lead",
                                    primary_stat=paste0(single_poss_pct,"%"),
                                    primary_label="Probability of hitting a 2 (weighted by shot distribution)",
                                    secondary_stat=paste0(optimal_pct,"%"),
                                    secondary_label=paste0("If play drawn for ",best_2_player," (best 2P shooter)"),
                                    detail=paste0("\U0001f3af Primary option: ",primary_player,
                                                  " (",round(two_pcts[primary_idx],1),"% from 2, ~",primary_weight,"% shot share)"),
                                    detail2=if(improvement>2) paste0("\u2b06\ufe0f +",improvement,"% boost if you go to ",best_2_player," instead") else NULL,
                                    vs_league=paste0("League avg 2P%: ",league_avg_2,"%"),
                                    vs_diff=single_poss_pct-league_avg_2,
                                    gauge_pct=min(100,max(0,(single_poss_pct-30)/(65-30)*100)))
                             },
                             "ft_situation" = {
                               fouled_idx <- primary_idx; fouled_player <- as.character(kings_lineup$Player[fouled_idx])
                               fouled_ft <- fts[fouled_idx]
                               both_fts <- round((fouled_ft/100)^2*100,1)
                               at_least_one <- round((1-((1-fouled_ft/100)^2))*100,1)
                               best_ft_idx <- which.max(fts); best_ft_player <- as.character(kings_lineup$Player[best_ft_idx])
                               best_ft_pct <- fts[best_ft_idx]; best_both <- round((best_ft_pct/100)^2*100,1)
                               nonzero_ft <- which(fts>0)
                               worst_ft_idx <- if(length(nonzero_ft)>0) nonzero_ft[which.min(fts[nonzero_ft])] else 1
                               worst_ft_player <- as.character(kings_lineup$Player[worst_ft_idx]); worst_ft_pct <- fts[worst_ft_idx]
                               list(title="Free Throw Situation \u2014 Most Likely Player Fouled",
                                    primary_stat=paste0(both_fts,"%"),
                                    primary_label=paste0("Prob ",fouled_player," makes BOTH FTs (",fouled_ft,"% shooter)"),
                                    secondary_stat=paste0(at_least_one,"%"),
                                    secondary_label="Prob makes at least 1 of 2",
                                    detail=paste0("\U0001f3af Best FT option: ",best_ft_player," (",best_ft_pct,"% \u2192 ",best_both,"% both)"),
                                    detail2=paste0("\u26a0\ufe0f Protect ",worst_ft_player," (",worst_ft_pct,"% FT) \u2014 don't let them get fouled"),
                                    vs_league="Good FT%: 75%+", vs_diff=fouled_ft-75,
                                    gauge_pct=min(100,max(0,(fouled_ft-40)/(90-40)*100)))
                             },
                             "need_stop" = {
                               total_spg <- sum(spgs); total_bpg <- sum(bpgs); total_rpg <- sum(rpgs)
                               defensive_plays_pg <- total_spg + total_bpg
                               stop_prob <- round(min(95,(defensive_plays_pg/70)*100),1)
                               base_miss_rate <- 55; def_boost <- min(10, defensive_plays_pg*1.5)
                               opp_off_penalty <- 0
                               if (!is.null(opp_lineup_ss) && nrow(opp_lineup_ss)>0) {
                                 opp_total_ppg2 <- sum(opp_lineup_ss$PPG, na.rm=TRUE)
                                 opp_off_penalty <- max(-8, min(0, (80-opp_total_ppg2)*0.15))
                               }
                               adjusted_miss_rate <- round(base_miss_rate + def_boost + opp_off_penalty, 1)
                               best_def_idx <- which.max(kings_lineup$SPG + kings_lineup$BPG)
                               best_defender <- as.character(kings_lineup$Player[best_def_idx])
                               best_def_val <- round(kings_lineup$SPG[best_def_idx] + kings_lineup$BPG[best_def_idx], 1)
                               list(title="Need a Stop \u2014 One Defensive Possession",
                                    primary_stat=paste0(adjusted_miss_rate,"%"),
                                    primary_label="Estimated probability opponent misses this possession",
                                    secondary_stat=paste0(stop_prob,"%"),
                                    secondary_label="Chance of forced turnover or block on any possession",
                                    detail=paste0("\U0001f6e1\ufe0f Defensive anchor: ",best_defender," (",best_def_val," STL+BLK per game)"),
                                    detail2=paste0("\U0001f3c0 Combined ",round(total_rpg,1)," RPG \u2014 strong board presence if they miss"),
                                    vs_league=paste0(round(defensive_plays_pg,1)," STL+BLK per game"),
                                    vs_diff=defensive_plays_pg-4,
                                    gauge_pct=min(100,max(0,(adjusted_miss_rate-45)/(75-45)*100)))
                             },
                             "def_last" = {
                               total_rpg <- sum(rpgs); total_bpg <- sum(bpgs)
                               reb_share <- round(min(90, total_rpg/45*100),1)
                               contest_rate <- round(min(40, total_bpg/0.7*10),1)
                               best_reb_idx <- which.max(rpgs); best_rebounder <- as.character(kings_lineup$Player[best_reb_idx])
                               list(title="Defense \u2014 Last Possession",
                                    primary_stat=paste0(reb_share,"%"),
                                    primary_label="Estimated rebound probability if opponent misses",
                                    secondary_stat=paste0(contest_rate,"%"),
                                    secondary_label="Shot contest/block probability",
                                    detail=paste0("\U0001f3c0 Key rebounder: ",best_rebounder," (",rpgs[best_reb_idx]," RPG)"),
                                    detail2=paste0("Combined ",round(total_bpg,1)," BPG \u2014 ",
                                                   if(total_bpg>=2)"strong rim protection" else "limited shot blocking"),
                                    vs_league=paste0("Combined ",round(total_rpg,1)," RPG"),
                                    vs_diff=total_rpg-35,
                                    gauge_pct=min(100,max(0,(reb_share-40)/(90-40)*100)))
                             },
                             "scoring" = {
                               total_ppg_lineup <- sum(ppgs)
                               weighted_fg <- round(sum(shot_weights*((fg2s+fg3s)/2)),1)
                               best_scorer_idx <- which.max(ppgs); best_scorer <- as.character(kings_lineup$Player[best_scorer_idx])
                               expected_pps <- round(sum(shot_weights*(fg2s/100*2+fg3s/100*3*0.3)),2)
                               list(title="Pure Scoring \u2014 Maximum Offensive Output",
                                    primary_stat=paste0(round(total_ppg_lineup,1)),
                                    primary_label="Combined lineup PPG",
                                    secondary_stat=paste0(expected_pps," pts"),
                                    secondary_label="Expected points per possession",
                                    detail=paste0("\U0001f525 Primary scorer: ",best_scorer," (",ppgs[best_scorer_idx]," PPG, ~",
                                                  round(shot_weights[best_scorer_idx]*100,0),"% usage)"),
                                    detail2=paste0("Lineup weighted FG%: ",weighted_fg,"%"),
                                    vs_league="Good offense: 1.0+ pts/poss", vs_diff=expected_pps-1.0,
                                    gauge_pct=min(100,max(0,(total_ppg_lineup-50)/(120-50)*100)))
                             }
    )
    
    if (is.null(situation_data))
      return(p("Select a situation", style="text-align:center;color:#999;padding:20px;"))
    
    gauge_pct <- situation_data$gauge_pct
    grade <- if(gauge_pct>=75)"A" else if(gauge_pct>=50)"B" else if(gauge_pct>=25)"C" else "D"
    grade_col <- switch(grade,"A"="#15803d","B"="#65a30d","C"="#d97706","D"="#b91c1c")
    grade_bg  <- switch(grade,
                        "A"="linear-gradient(135deg, #15803d, #22c55e)","B"="linear-gradient(135deg, #65a30d, #a3e635)",
                        "C"="linear-gradient(135deg, #d97706, #fbbf24)","D"="linear-gradient(135deg, #b91c1c, #f87171)")
    vs_color <- if(situation_data$vs_diff>0)"#15803d" else if(situation_data$vs_diff<0)"#b91c1c" else "#64748b"
    
    div(
      div(
        style = "display: flex; gap: 20px; flex-wrap: wrap;",
        div(
          style = paste0("flex: 1; min-width: 280px; background: white; border: 2px solid ",grade_col,"25;
                   border-radius: 14px; padding: 24px; text-align: center; position: relative;
                   box-shadow: 0 4px 16px rgba(0,0,0,0.06);"),
          div(style=paste0("position: absolute; top: 12px; right: 12px; width: 40px; height: 40px;
                     background: ",grade_bg,"; border-radius: 50%; display: flex;
                     align-items: center; justify-content: center; font-weight: 800; font-size: 1.1em;
                     box-shadow: 0 2px 8px rgba(0,0,0,0.15);",
                           if(grade%in%c("B","C"))" color: #1a1a2e;" else " color: white;"), grade),
          h4(situation_data$title, style="margin: 0 0 15px 0; color: #334155; font-weight: 700;"),
          h2(situation_data$primary_stat, style=paste0("margin: 0; font-size: 3.5em; font-weight: 800; color: ",grade_col,";")),
          p(situation_data$primary_label, style="margin: 5px 0 0 0; color: #64748b; font-weight: 600;"),
          div(style="margin: 15px auto; max-width: 250px;",
              div(style="height: 10px; background: #e2e8f0; border-radius: 5px; overflow: hidden;",
                  div(style=paste0("width: ",gauge_pct,"%; height: 100%; background: ",grade_col,"; border-radius: 5px; transition: width 0.5s;"))),
              div(style="display: flex; justify-content: space-between; margin-top: 4px;",
                  span("Poor",  style="font-size: 0.65em; color: #94a3b8;"),
                  span("Elite", style="font-size: 0.65em; color: #94a3b8;"))),
          div(style="margin-top: 10px;",
              span(situation_data$vs_league, style="font-size: 0.85em; color: #64748b;"),
              span(paste0(" (",ifelse(situation_data$vs_diff>0,"+",""),round(situation_data$vs_diff,1),")"),
                   style=paste0("font-size: 0.85em; font-weight: 700; color: ",vs_color,"; margin-left: 5px;")))
        ),
        div(
          style="flex: 1; min-width: 280px; display: flex; flex-direction: column; gap: 12px;",
          div(style="background: #f8fafc; border-radius: 12px; padding: 18px; border: 1px solid #e2e8f0;",
              div(style="display: flex; justify-content: space-between; align-items: center;",
                  div(h3(situation_data$secondary_stat,
                         style=paste0("margin: 0; font-size: 1.8em; font-weight: 800; color: ",ohio_kings_secondary,";")),
                      p(situation_data$secondary_label, style="margin: 4px 0 0 0; color: #64748b; font-size: 0.85em;")))),
          div(style=paste0("background: ",grade_col,"10; border-left: 4px solid ",grade_col,"; border-radius: 8px; padding: 14px;"),
              p(situation_data$detail, style="margin: 0; color: #334155; font-weight: 600; font-size: 0.95em;"),
              if(!is.null(situation_data$detail2))
                p(situation_data$detail2, style="margin: 8px 0 0 0; color: #475569; font-weight: 600; font-size: 0.88em;")),
          if (has_opp_ss && !is.null(opp_lineup_ss) && nrow(opp_lineup_ss)>0) {
            div(style="background: #fef2f2; border: 1px solid #fecaca; border-radius: 10px; padding: 14px;",
                div(style="display: flex; justify-content: space-between; align-items: center;",
                    div(div(style="display: flex; align-items: center; gap: 6px;",
                            span("\u26a0\ufe0f", style="font-size: 1.2em;"),
                            span(paste0("vs ",input$opponent_select),
                                 style="font-weight: 700; color: #991b1b; font-size: 0.95em;")),
                        div(paste0("Opponent is ",opp_situation_label),
                            style="font-size: 0.8em; color: #64748b; margin-top: 2px;")),
                    div(style="text-align: right;",
                        div(paste0(opp_threat_quality," ",opp_threat_label),
                            style=paste0("font-weight: 700; font-size: 0.9em; color: ",opp_threat_color,";")),
                        div(opp_threat_detail, style="font-size: 0.75em; color: #64748b; margin-top: 2px;"))),
                div(style="margin-top: 8px; display: flex; gap: 6px; flex-wrap: wrap;",
                    lapply(1:nrow(opp_lineup_ss), function(j) {
                      op <- opp_lineup_ss[j,]
                      parts <- strsplit(as.character(op$Player)," ")[[1]]
                      short <- if(length(parts)>=2) paste0(substr(parts[1],1,1),". ",paste(parts[-1],collapse=" ")) else as.character(op$Player)
                      span(paste0(op$lineup_position,": ",short),
                           style="font-size: 0.7em; background: #fee2e2; color: #991b1b; padding: 2px 8px; border-radius: 6px; font-weight: 600;")
                    })),
                if (kings_on_offense && opp_def_factor < 1) {
                  div(style="margin-top: 8px; font-size: 0.78em; color: #991b1b; font-weight: 600;",
                      paste0("Their defense reduces our success rate by ~",round((1-opp_def_factor)*100,1),"%"))
                } else if (!kings_on_offense && !is.null(opp_lineup_ss)) {
                  opp_best <- as.character(opp_lineup_ss$Player[which.max(opp_lineup_ss$PPG)])
                  opp_best_ppg <- max(opp_lineup_ss$PPG)
                  div(style="margin-top: 8px; font-size: 0.78em; color: #991b1b; font-weight: 600;",
                      paste0("Key threat: ",opp_best," (",opp_best_ppg," PPG) \u2014 primary scoring option to contain"))
                }
            )
          },
          div(style="background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 14px;",
              div(style="display: flex; justify-content: space-between; align-items: center;",
                  div(span("Lineup Momentum", style="font-weight: 600; color: #334155; font-size: 0.9em;"),
                      div(paste0(hot_count," hot / ",cold_count," cold / ",5-hot_count-cold_count," steady"),
                          style="margin-top: 4px; font-size: 0.8em; color: #64748b;")),
                  div(style="text-align: right;",
                      div(momentum_label, style=paste0("font-weight: 700; font-size: 1.1em; color: ",momentum_color,";")),
                      div(paste0(ifelse(avg_streak>0,"+",""),avg_streak,"% avg"),
                          style=paste0("font-size: 0.8em; font-weight: 600; color: ",momentum_color,";"))))
          )
        )
      )
    )
  })
  
  output$win_probability <- renderUI({
    req(rv$logged_in, input$opponent_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$opponent_select == "")
      return(p("Select an opponent to see win probability analysis",
               style="text-align: center; color: #999; padding: 20px;"))
    
    if (!rv$my_team %in% names(rv$aba_data$all_team_stats) ||
        !input$opponent_select %in% names(rv$aba_data$all_team_stats))
      return(p("Team data not available", style="text-align: center; color: #999; padding: 20px;"))
    
    kings_roster <- rv$aba_data$all_team_stats[[rv$my_team]] %>%
      filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    opp_roster   <- rv$aba_data$all_team_stats[[input$opponent_select]] %>%
      filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    
    if (is.null(kings_roster)||is.null(opp_roster)||nrow(kings_roster)==0||nrow(opp_roster)==0)
      return(p("Unable to calculate win probability - missing team data",
               style="text-align: center; color: #999; padding: 20px;"))
    
    kings_record <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
    opp_record   <- rv$aba_data$league_data %>% filter(Team == input$opponent_select)
    kings_gp <- if(nrow(kings_record)>0) as.numeric(kings_record$W[1])+as.numeric(kings_record$L[1]) else 15
    opp_gp   <- if(nrow(opp_record)>0)   as.numeric(opp_record$W[1])+as.numeric(opp_record$L[1])   else 15
    
    # Kings stats
    k_2pm<-sum(suppressWarnings(as.numeric(kings_roster$`2PM`)),na.rm=TRUE)
    k_2pa<-sum(suppressWarnings(as.numeric(kings_roster$`2PA`)),na.rm=TRUE)
    k_3pm<-sum(suppressWarnings(as.numeric(kings_roster$`3PM`)),na.rm=TRUE)
    k_3pa<-sum(suppressWarnings(as.numeric(kings_roster$`3PA`)),na.rm=TRUE)
    k_fgm<-k_2pm+k_3pm; k_fga<-k_2pa+k_3pa
    k_fta<-sum(suppressWarnings(as.numeric(kings_roster$FTA)),na.rm=TRUE)
    k_ftm<-sum(suppressWarnings(as.numeric(kings_roster$FTM)),na.rm=TRUE)
    k_oreb<-sum(suppressWarnings(as.numeric(kings_roster$OREB)),na.rm=TRUE)
    k_dreb<-sum(suppressWarnings(as.numeric(kings_roster$DREB)),na.rm=TRUE)
    k_pts<-sum(suppressWarnings(as.numeric(kings_roster$PTS)),na.rm=TRUE)
    k_to <- 0
    for (p in as.character(kings_roster$Player)) {
      tryCatch({
        p_data <- rv$aba_data$all_player_stats[[p]]
        if (!is.null(p_data$game_log) && "TO" %in% names(p_data$game_log))
          k_to <- k_to + sum(suppressWarnings(as.numeric(p_data$game_log$TO)), na.rm=TRUE)
      }, error=function(e){})
    }
    
    # Opp stats
    o_2pm<-sum(suppressWarnings(as.numeric(opp_roster$`2PM`)),na.rm=TRUE)
    o_2pa<-sum(suppressWarnings(as.numeric(opp_roster$`2PA`)),na.rm=TRUE)
    o_3pm<-sum(suppressWarnings(as.numeric(opp_roster$`3PM`)),na.rm=TRUE)
    o_3pa<-sum(suppressWarnings(as.numeric(opp_roster$`3PA`)),na.rm=TRUE)
    o_fgm<-o_2pm+o_3pm; o_fga<-o_2pa+o_3pa
    o_fta<-sum(suppressWarnings(as.numeric(opp_roster$FTA)),na.rm=TRUE)
    o_ftm<-sum(suppressWarnings(as.numeric(opp_roster$FTM)),na.rm=TRUE)
    o_oreb<-sum(suppressWarnings(as.numeric(opp_roster$OREB)),na.rm=TRUE)
    o_dreb<-sum(suppressWarnings(as.numeric(opp_roster$DREB)),na.rm=TRUE)
    o_pts<-sum(suppressWarnings(as.numeric(opp_roster$PTS)),na.rm=TRUE)
    o_to <- 0
    for (p in as.character(opp_roster$Player)) {
      tryCatch({
        p_data <- rv$aba_data$all_player_stats[[p]]
        if (!is.null(p_data$game_log) && "TO" %in% names(p_data$game_log))
          o_to <- o_to + sum(suppressWarnings(as.numeric(p_data$game_log$TO)), na.rm=TRUE)
      }, error=function(e){})
    }
    
    # Four factors
    k_efg     <- if(k_fga>0) round((k_fgm+0.5*k_3pm)/k_fga*100,1) else 0
    k_tov_pct <- if(k_fga>0) round(k_to/(k_fga+0.44*k_fta+k_to)*100,1) else 0
    k_oreb_pct<- if((k_oreb+k_dreb)>0) round(k_oreb/(k_oreb+k_dreb)*100,1) else 0
    k_ftr     <- if(k_fga>0) round(k_fta/k_fga,2) else 0
    o_efg     <- if(o_fga>0) round((o_fgm+0.5*o_3pm)/o_fga*100,1) else 0
    o_tov_pct <- if(o_fga>0) round(o_to/(o_fga+0.44*o_fta+o_to)*100,1) else 0
    o_oreb_pct<- if((o_oreb+o_dreb)>0) round(o_oreb/(o_oreb+o_dreb)*100,1) else 0
    o_ftr     <- if(o_fga>0) round(o_fta/o_fga,2) else 0
    
    # Player matchup analysis
    kings_lineup_wp <- tryCatch(build_lineup(rv$my_team,"scoring",rv$aba_data), error=function(e) NULL)
    opp_lineup_wp   <- tryCatch(build_lineup(input$opponent_select,"scoring",rv$aba_data), error=function(e) NULL)
    starter_matchup_adv <- 50; bench_depth_adv <- 50; combined_matchup_adv <- 50
    k_bench_avg <- 0; o_bench_avg <- 0
    
    if (!is.null(kings_lineup_wp)&&!is.null(opp_lineup_wp)&&nrow(kings_lineup_wp)>0&&nrow(opp_lineup_wp)>0) {
      n_mu <- min(nrow(kings_lineup_wp),nrow(opp_lineup_wp))
      starter_edges <- sapply(1:n_mu, function(i) {
        k<-kings_lineup_wp[i,]; o<-opp_lineup_wp[i,]
        sc<-100/(1+exp(-0.15*(k$PPG-o$PPG))); rb<-100/(1+exp(-0.3*(k$RPG-o$RPG)))
        as_<-100/(1+exp(-0.3*(k$APG-o$APG))); df<-100/(1+exp(-0.5*((k$SPG+k$BPG)-(o$SPG+o$BPG))))
        sc*0.40+rb*0.25+as_*0.20+df*0.15
      })
      starter_matchup_adv <- mean(starter_edges)
      get_bench_score <- function(team_name, lineup) {
        roster <- rv$aba_data$all_team_stats[[team_name]]
        if (is.null(roster)) return(0)
        bench <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"),!Player%in%lineup$Player) %>%
          mutate(bench_ppg=sapply(Player,function(p){
            tryCatch({pg<-rv$aba_data$all_player_stats[[p]]$per_game;
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) pg$PPG[1] else 0},error=function(e)0)})) %>%
          arrange(desc(bench_ppg)) %>% head(3)
        mean(bench$bench_ppg,na.rm=TRUE)
      }
      k_bench_avg <- get_bench_score(rv$my_team, kings_lineup_wp)
      o_bench_avg <- get_bench_score(input$opponent_select, opp_lineup_wp)
      bench_depth_adv <- 100/(1+exp(-0.2*(k_bench_avg-o_bench_avg)))
      combined_matchup_adv <- starter_matchup_adv*0.85 + bench_depth_adv*0.15
    }
    
    # Win probability
    k_win_pct <- if(nrow(kings_record)>0&&kings_gp>0) kings_record$W[1]/kings_gp else 0.5
    o_win_pct <- if(nrow(opp_record)>0&&opp_gp>0)   opp_record$W[1]/opp_gp   else 0.5
    k_ppg <- get_accurate_team_ppg(rv$my_team, rv$aba_data)
    o_ppg <- get_accurate_team_ppg(input$opponent_select, rv$aba_data)
    if(k_ppg==0) k_ppg <- round(k_pts/kings_gp,1)
    if(o_ppg==0) o_ppg <- round(o_pts/opp_gp,1)
    
    avg_total <- (k_ppg+o_ppg)/2
    win_result_full <- calculate_unified_win_probability(NULL,input$opponent_select,rv$aba_data,TRUE,my_team_name=rv$my_team)
    win_prob <- win_result_full$prob; opp_win_prob <- 100-win_prob
    team_spread <- (win_prob-50)/50*12
    predicted_kings <- round(avg_total+team_spread/2,0); predicted_opp <- round(avg_total-team_spread/2,0)
    if(win_prob>50&&predicted_kings<=predicted_opp) predicted_kings<-predicted_opp+1
    if(win_prob<50&&predicted_opp<=predicted_kings) predicted_opp<-predicted_kings+1
    matchup_spread <- (combined_matchup_adv-50)/50*15
    matchup_pred_kings <- round(avg_total+matchup_spread/2,0); matchup_pred_opp <- round(avg_total-matchup_spread/2,0)
    if(combined_matchup_adv>50&&matchup_pred_kings<=matchup_pred_opp) matchup_pred_kings<-matchup_pred_opp+1
    if(combined_matchup_adv<50&&matchup_pred_opp<=matchup_pred_kings) matchup_pred_opp<-matchup_pred_kings+1
    
    score_diff_team <- predicted_kings-predicted_opp; score_diff_matchup <- matchup_pred_kings-matchup_pred_opp
    prediction_gap <- abs(score_diff_team-score_diff_matchup)
    confidence <- if(prediction_gap<=5)"High" else if(prediction_gap<=10)"Moderate" else "Mixed Signals"
    confidence_color <- if(confidence=="High")"#15803d" else if(confidence=="Moderate")"#d97706" else "#b91c1c"
    confidence_icon  <- if(confidence=="High")"\u2705" else if(confidence=="Moderate")"\u26a0\ufe0f" else "\u26d4"
    confidence_desc  <- if(confidence=="High") "Team stats and player matchups agree \u2014 prediction is reliable"
    else if(confidence=="Moderate") "Some divergence between team stats and matchups \u2014 review key positions"
    else "Team stats and matchup analysis disagree significantly \u2014 high uncertainty"
    
    efg_diff <- k_efg-o_efg; tov_diff <- o_tov_pct-k_tov_pct
    reb_diff  <- k_oreb_pct-o_oreb_pct; ftr_diff <- k_ftr-o_ftr
    
    div(
      if(!win_result_full$has_full_data)
        div(style="background: #fffbeb; border: 2px solid #f59e0b; border-radius: 10px; padding: 14px; margin-bottom: 15px;",
            div(style="display: flex; align-items: center; gap: 10px;",
                span("\u26a0\ufe0f",style="font-size: 1.5em;"),
                div(strong("Limited Data Available",style="color: #92400e; font-size: 1em;"),
                    p(win_result_full$message,style="margin: 4px 0 0 0; color: #78350f; font-size: 0.85em;")))),
      div(
        div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            div(h3(rv$my_team,style=paste0("margin: 0; color: ",ohio_kings_primary,"; font-weight: 800;")),
                h2(paste0(win_prob,"%"),style=paste0("margin: 5px 0 0 0; color: ",ohio_kings_primary,"; font-size: 3em;"))),
            div(style="text-align: center; padding: 0 20px;",
                p("WIN PROBABILITY",style="margin: 0; color: #64748b; font-size: 0.9em; font-weight: 600;"),
                h1("VS",style="margin: 10px 0; color: #1e293b; font-size: 2.5em;")),
            div(style="text-align: right;",
                h3(input$opponent_select,style="margin: 0; color: #ef4444; font-weight: 800;"),
                h2(paste0(opp_win_prob,"%"),style="margin: 5px 0 0 0; color: #ef4444; font-size: 3em;"))),
        div(style="height: 40px; background: #e2e8f0; border-radius: 20px; overflow: hidden; position: relative; box-shadow: inset 0 2px 4px rgba(0,0,0,0.1);",
            div(style=paste0("width: ",win_prob,"%; height: 100%; background: linear-gradient(90deg, ",ohio_kings_primary," 0%, ",ohio_kings_secondary," 100%);
                         display: flex; align-items: center; justify-content: flex-end; padding-right: 15px; transition: width 0.5s ease;"),
                span(paste0(win_prob,"%"),style="color: white; font-weight: bold; font-size: 1.1em;")),
            div(style="position: absolute; right: 15px; top: 50%; transform: translateY(-50%); color: #1e293b; font-weight: bold; font-size: 1.1em;",
                paste0(opp_win_prob,"%")))
      ),
      div(style="margin-bottom: 20px;",
          h4("Key Factors:",style="margin: 0 0 15px 0; color: #334155;"),
          fluidRow(
            column(6, div(style=paste0("background: ",if(efg_diff>0)"#d1fae5" else "#fee2e2","; border-left: 4px solid ",if(efg_diff>0)"#10b981" else "#ef4444","; padding: 15px; border-radius: 8px;"),
                          div(style="display: flex; justify-content: space-between; align-items: center;",
                              span("\U0001f3af Shooting Efficiency (eFG%)",style="font-weight: 600; color: #1e293b;"),
                              span(paste0(if(efg_diff>=0)"\u2713 " else "\u2717 ",ifelse(efg_diff>=0,"+",""),round(efg_diff,1),"%"),
                                   style=paste0("font-weight: 700; font-size: 1.2em; color: ",if(efg_diff>=0)"#10b981" else "#ef4444",";"))),
                          p(paste0(rv$my_team,": ",k_efg,"% | Opp: ",o_efg,"%"),style="margin: 8px 0 0 0; color: #64748b; font-size: 0.9em;"))),
            column(6, div(style=paste0("background: ",if(tov_diff>=0)"#d1fae5" else "#fee2e2","; border-left: 4px solid ",if(tov_diff>=0)"#10b981" else "#ef4444","; padding: 15px; border-radius: 8px;"),
                          div(style="display: flex; justify-content: space-between; align-items: center;",
                              span("\U0001f512 Ball Security (TOV%)",style="font-weight: 600; color: #1e293b;"),
                              span(paste0(if(k_tov_pct<=o_tov_pct)"\u2713 " else "\u2717 ",ifelse(k_tov_pct<o_tov_pct,"-","+"),round(abs(k_tov_pct-o_tov_pct),1),"%"),
                                   style=paste0("font-weight: 700; font-size: 1.2em; color: ",if(k_tov_pct<=o_tov_pct)"#10b981" else "#ef4444",";"))),
                          p(paste0(rv$my_team,": ",k_tov_pct,"% | Opp: ",o_tov_pct,"%"),style="margin: 8px 0 0 0; color: #64748b; font-size: 0.9em;")))
          ),
          fluidRow(
            column(6, div(style=paste0("background: ",if(reb_diff>=0)"#d1fae5" else "#fee2e2","; border-left: 4px solid ",if(reb_diff>=0)"#10b981" else "#ef4444","; padding: 15px; border-radius: 8px; margin-top: 10px;"),
                          div(style="display: flex; justify-content: space-between; align-items: center;",
                              span("\U0001f3c0 Rebounding (OREB%)",style="font-weight: 600; color: #1e293b;"),
                              span(paste0(if(reb_diff>=0)"\u2713 " else "\u2717 ",ifelse(reb_diff>=0,"+",""),round(reb_diff,1),"%"),
                                   style=paste0("font-weight: 700; font-size: 1.2em; color: ",if(reb_diff>=0)"#10b981" else "#ef4444",";"))),
                          p(paste0(rv$my_team,": ",k_oreb_pct,"% | Opp: ",o_oreb_pct,"%"),style="margin: 8px 0 0 0; color: #64748b; font-size: 0.9em;"))),
            column(6, div(style=paste0("background: ",if(ftr_diff>=0)"#d1fae5" else "#fee2e2","; border-left: 4px solid ",if(ftr_diff>=0)"#10b981" else "#ef4444","; padding: 15px; border-radius: 8px; margin-top: 10px;"),
                          div(style="display: flex; justify-content: space-between; align-items: center;",
                              span("\U0001f4ca Free Throw Rate",style="font-weight: 600; color: #1e293b;"),
                              span(paste0(if(ftr_diff>=0)"\u2713 " else "\u2717 ",ifelse(ftr_diff>=0,"+",""),round(ftr_diff,2)),
                                   style=paste0("font-weight: 700; font-size: 1.2em; color: ",if(ftr_diff>=0)"#10b981" else "#ef4444",";"))),
                          p(paste0(rv$my_team,": ",k_ftr," | Opp: ",o_ftr),style="margin: 8px 0 0 0; color: #64748b; font-size: 0.9em;")))
          )
      ),
      div(style=paste0("background: white; border: 2px solid ",confidence_color,"; border-radius: 12px; padding: 16px 20px; margin-top: 20px; display: flex; align-items: center; gap: 15px;"),
          div(style=paste0("width: 50px; height: 50px; border-radius: 50%; background: ",confidence_color,"15; display: flex; align-items: center; justify-content: center; font-size: 1.5em; flex-shrink: 0;"),
              confidence_icon),
          div(div(style="display: flex; align-items: center; gap: 8px;",
                  span(paste0("Prediction Confidence: ",confidence),
                       style=paste0("font-weight: 800; font-size: 1.1em; color: ",confidence_color,";")),
                  span(paste0("(",round(prediction_gap,1)," pt spread)"),style="font-size: 0.85em; color: #64748b;")),
              p(confidence_desc,style="margin: 4px 0 0 0; color: #64748b; font-size: 0.85em;"))
      ),
      div(style="display: flex; gap: 15px; margin-top: 15px;",
          div(style="flex: 1; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 12px; text-align: center;",
              h4("TEAM STATS PREDICTION",style="margin: 0 0 10px 0; opacity: 0.8; font-size: 0.85em; letter-spacing: 1px;"),
              div(style="display: flex; justify-content: center; align-items: center; gap: 20px;",
                  div(div(rv$my_team,style="font-size: 0.8em; opacity: 0.8;"),
                      div(predicted_kings,style="font-size: 3em; font-weight: 800; line-height: 1;")),
                  div("-",style="font-size: 2em; opacity: 0.5;"),
                  div(div(input$opponent_select,style="font-size: 0.8em; opacity: 0.8;"),
                      div(predicted_opp,style="font-size: 3em; font-weight: 800; line-height: 1;"))),
              p("Based on PPG + four factors + record",style="margin: 10px 0 0 0; opacity: 0.7; font-size: 0.8em;")),
          div(style=paste0("flex: 1; background: linear-gradient(135deg, #1e293b 0%, #334155 100%); color: white; padding: 20px; border-radius: 12px; text-align: center;"),
              h4("MATCHUP-ADJUSTED",style="margin: 0 0 10px 0; opacity: 0.8; font-size: 0.85em; letter-spacing: 1px;"),
              div(style="display: flex; justify-content: center; align-items: center; gap: 20px;",
                  div(div(rv$my_team,style="font-size: 0.8em; opacity: 0.8;"),
                      div(matchup_pred_kings,style="font-size: 3em; font-weight: 800; line-height: 1;")),
                  div("-",style="font-size: 2em; opacity: 0.5;"),
                  div(div(input$opponent_select,style="font-size: 0.8em; opacity: 0.8;"),
                      div(matchup_pred_opp,style="font-size: 3em; font-weight: 800; line-height: 1;"))),
              p(paste0("Starters: ",round(starter_matchup_adv,1),"% | Bench: ",round(bench_depth_adv,1),"%"),
                style="margin: 10px 0 0 0; opacity: 0.7; font-size: 0.8em;"))
      ),
      div(style="margin-top: 15px; background: #f8fafc; border-radius: 12px; padding: 16px; border: 1px solid #e2e8f0;",
          div(style="display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
              span("Player Matchup Factor",style="font-weight: 700; color: #334155; font-size: 0.95em;"),
              span(paste0(round(combined_matchup_adv,1),"% ",rv$my_team," Edge"),
                   style=paste0("font-weight: 700; font-size: 0.95em; color: ",
                                if(combined_matchup_adv>55)"#15803d" else if(combined_matchup_adv<45)"#b91c1c" else "#d97706",";"))),
          div(style="margin-bottom: 8px;",
              div(style="display: flex; justify-content: space-between; margin-bottom: 4px;",
                  span("Starters (85%)",style="font-size: 0.8em; color: #64748b; font-weight: 600;"),
                  span(paste0(round(starter_matchup_adv,1),"%"),style="font-size: 0.8em; color: #475569; font-weight: 700;")),
              div(style="height: 10px; background: #e2e8f0; border-radius: 5px; overflow: hidden;",
                  div(style=paste0("width: ",round(starter_matchup_adv),"%; height: 100%; background: linear-gradient(90deg, ",ohio_kings_primary,", ",ohio_kings_secondary,"); border-radius: 5px; transition: width 0.5s;")))),
          div(div(style="display: flex; justify-content: space-between; margin-bottom: 4px;",
                  span("Bench Depth (15%)",style="font-size: 0.8em; color: #64748b; font-weight: 600;"),
                  span(paste0(round(bench_depth_adv,1),"%"),style="font-size: 0.8em; color: #475569; font-weight: 700;")),
              div(style="height: 10px; background: #e2e8f0; border-radius: 5px; overflow: hidden;",
                  div(style=paste0("width: ",round(bench_depth_adv),"%; height: 100%; background: linear-gradient(90deg, ",ohio_kings_accent,", #f59e0b); border-radius: 5px; transition: width 0.5s;")))))
    )
  })
  
  output$player_matchups <- renderUI({
    req(rv$logged_in, input$situation_select, input$opponent_select, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$situation_select==""||input$opponent_select=="")
      return(p("Select a situation and opponent to see player matchup predictions",
               style="text-align: center; color: #999; padding: 20px;"))
    
    kings_lineup <- tryCatch(build_lineup(rv$my_team,"scoring",rv$aba_data), error=function(e) NULL)
    opp_lineup   <- tryCatch(build_lineup(input$opponent_select,"scoring",rv$aba_data), error=function(e) NULL)
    
    if(is.null(kings_lineup)||is.null(opp_lineup)||nrow(kings_lineup)==0||nrow(opp_lineup)==0)
      return(p("Lineup data not available for matchup analysis",
               style="text-align: center; color: #999; padding: 20px;"))
    
    n_matchups <- min(nrow(kings_lineup), nrow(opp_lineup))
    
    all_edges <- sapply(1:n_matchups, function(i) {
      k<-kings_lineup[i,]; o<-opp_lineup[i,]
      sc<-100/(1+exp(-0.15*(k$PPG-o$PPG))); rb<-100/(1+exp(-0.3*(k$RPG-o$RPG)))
      as_<-100/(1+exp(-0.3*(k$APG-o$APG))); df<-100/(1+exp(-0.5*((k$SPG+k$BPG)-(o$SPG+o$BPG))))
      sc*0.40+rb*0.25+as_*0.20+df*0.15
    })
    best_idx  <- which.max(all_edges); worst_idx <- which.min(all_edges)
    
    k_best_last <- tryCatch({
      parts <- strsplit(as.character(kings_lineup$Player[best_idx])," ")[[1]]; tail(parts,1)
    }, error=function(e) as.character(kings_lineup$Player[best_idx]))
    k_worst_last <- tryCatch({
      parts <- strsplit(as.character(kings_lineup$Player[worst_idx])," ")[[1]]; tail(parts,1)
    }, error=function(e) as.character(kings_lineup$Player[worst_idx]))
    
    div(
      div(style="display: flex; gap: 12px; margin-bottom: 15px;",
          div(style="flex: 1; background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 10px; padding: 14px;",
              div(style="display: flex; align-items: center; gap: 8px;",
                  span("\u2705",style="font-size: 1.3em;"),
                  div(div("EXPLOIT THIS",style="font-weight: 800; font-size: 0.75em; color: #15803d; letter-spacing: 0.5px;"),
                      div(paste0(kings_lineup$lineup_position[best_idx],": ",kings_lineup$Player[best_idx],
                                 " has ",round(all_edges[best_idx],1),"% edge vs ",opp_lineup$Player[best_idx]),
                          style="font-weight: 600; font-size: 0.9em; color: #166534; margin-top: 2px;"),
                      div(paste0("Feed ",k_best_last," early and often"),
                          style="font-size: 0.8em; color: #15803d; margin-top: 3px; font-style: italic;")))),
          div(style="flex: 1; background: #fef2f2; border: 1px solid #fecaca; border-radius: 10px; padding: 14px;",
              div(style="display: flex; align-items: center; gap: 8px;",
                  span("\u26a0\ufe0f",style="font-size: 1.3em;"),
                  div(div("WATCH OUT",style="font-weight: 800; font-size: 0.75em; color: #b91c1c; letter-spacing: 0.5px;"),
                      div(paste0(kings_lineup$lineup_position[worst_idx],": ",opp_lineup$Player[worst_idx],
                                 " has ",round(100-all_edges[worst_idx],1),"% edge vs ",kings_lineup$Player[worst_idx]),
                          style="font-weight: 600; font-size: 0.9em; color: #991b1b; margin-top: 2px;"),
                      div(paste0("Send help defense to ",k_worst_last,"'s matchup"),
                          style="font-size: 0.8em; color: #b91c1c; margin-top: 3px; font-style: italic;")))
          )
      ),
      
      lapply(1:n_matchups, function(i) {
        k<-kings_lineup[i,]; o<-opp_lineup[i,]
        k_ppg<-if(is.na(k$PPG))0 else k$PPG; k_rpg<-if(is.na(k$RPG))0 else k$RPG
        k_apg<-if(is.na(k$APG))0 else k$APG; k_spg<-if(is.na(k$SPG))0 else k$SPG; k_bpg<-if(is.na(k$BPG))0 else k$BPG
        o_ppg<-if(is.na(o$PPG))0 else o$PPG; o_rpg<-if(is.na(o$RPG))0 else o$RPG
        o_apg<-if(is.na(o$APG))0 else o$APG; o_spg<-if(is.na(o$SPG))0 else o$SPG; o_bpg<-if(is.na(o$BPG))0 else o$BPG
        
        k_pred <- tryCatch(predict_player_game(as.character(k$Player),input$opponent_select),error=function(e)list(pts=NA,reb=NA,ast=NA))
        o_pred <- tryCatch(predict_player_game(as.character(o$Player),rv$my_team),error=function(e)list(pts=NA,reb=NA,ast=NA))
        k_pred_pts<-if(!is.null(k_pred$pts)&&!is.na(k_pred$pts)) k_pred$pts else k_ppg
        k_pred_reb<-if(!is.null(k_pred$reb)&&!is.na(k_pred$reb)) k_pred$reb else k_rpg
        k_pred_ast<-if(!is.null(k_pred$ast)&&!is.na(k_pred$ast)) k_pred$ast else k_apg
        o_pred_pts<-if(!is.null(o_pred$pts)&&!is.na(o_pred$pts)) o_pred$pts else o_ppg
        o_pred_reb<-if(!is.null(o_pred$reb)&&!is.na(o_pred$reb)) o_pred$reb else o_rpg
        o_pred_ast<-if(!is.null(o_pred$ast)&&!is.na(o_pred$ast)) o_pred$ast else o_apg
        
        scoring_adv  <- 100/(1+exp(-0.15*(k_ppg-o_ppg)))
        rebound_adv  <- 100/(1+exp(-0.3*(k_rpg-o_rpg)))
        assist_adv   <- 100/(1+exp(-0.3*(k_apg-o_apg)))
        defense_adv  <- 100/(1+exp(-0.5*((k_spg+k_bpg)-(o_spg+o_bpg))))
        overall_adv  <- round(scoring_adv*0.40+rebound_adv*0.25+assist_adv*0.20+defense_adv*0.15,1)
        opp_adv      <- round(100-overall_adv,1)
        winner <- if(overall_adv>55)"kings" else if(overall_adv<45)"opp" else "even"
        border_color <- if(winner=="kings")"#10b981" else if(winner=="opp")"#ef4444" else "#f59e0b"
        bg_color     <- if(winner=="kings")"#f0fdf4" else if(winner=="opp")"#fef2f2" else "#fffbeb"
        
        div(style=paste0("background: ",bg_color,"; border-left: 5px solid ",border_color,"; padding: 18px; margin: 12px 0; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);"),
            div(style="text-align: center; margin-bottom: 12px;",
                span(paste0("— ",k$lineup_position," MATCHUP —"),
                     style="font-weight: 700; color: #64748b; font-size: 0.85em; letter-spacing: 1px;")),
            div(style="display: flex; justify-content: space-between; align-items: center;",
                div(style="flex: 1; text-align: left;",
                    div(strong(as.character(k$Player),style=paste0("font-size: 1.1em; color: ",ohio_kings_primary,";")),
                        span(paste0(" (",k$Position,")"),style="color: #64748b; font-size: 0.9em;")),
                    div(style="font-size: 0.85em; color: #64748b; margin-top: 4px;",
                        paste0("Season: ",k_ppg," / ",k_rpg," / ",k_apg)),
                    div(style="display: flex; gap: 4px; margin-top: 4px;",
                        span(paste0(k_pred_pts," PTS"),style=paste0("font-size: 0.75em; font-weight: 700; color: white; background: ",ohio_kings_primary,"; padding: 2px 6px; border-radius: 4px;")),
                        span(paste0(k_pred_reb," REB"),style=paste0("font-size: 0.75em; font-weight: 700; color: white; background: ",ohio_kings_secondary,"; padding: 2px 6px; border-radius: 4px;")),
                        span(paste0(k_pred_ast," AST"),style=paste0("font-size: 0.75em; font-weight: 700; color: #1e293b; background: ",ohio_kings_accent,"; padding: 2px 6px; border-radius: 4px;"))),
                    div(style="font-size: 0.7em; color: #94a3b8; margin-top: 2px; font-style: italic;","\u2191 Predicted for this game")),
                div(style="flex: 0 0 120px; text-align: center; padding: 0 10px;",
                    div(if(winner=="kings") paste0(overall_adv,"%") else if(winner=="opp") paste0(opp_adv,"%") else "EVEN",
                        style=paste0("font-size: 1.8em; font-weight: 800; color: ",border_color,";")),
                    div(if(winner=="kings") paste0(rv$my_team," Edge") else if(winner=="opp") "Opp Edge" else "Toss-Up",
                        style=paste0("font-size: 0.75em; font-weight: 600; color: ",border_color,";"))),
                div(style="flex: 1; text-align: right;",
                    div(strong(as.character(o$Player),style="font-size: 1.1em; color: #991b1b;"),
                        span(paste0(" (",o$Position,")"),style="color: #64748b; font-size: 0.9em;")),
                    div(style="font-size: 0.85em; color: #64748b; margin-top: 4px;",
                        paste0("Season: ",o_ppg," / ",o_rpg," / ",o_apg)),
                    div(style="display: flex; gap: 4px; margin-top: 4px; justify-content: flex-end;",
                        span(paste0(o_pred_pts," PTS"),style="font-size: 0.75em; font-weight: 700; color: white; background: #991b1b; padding: 2px 6px; border-radius: 4px;"),
                        span(paste0(o_pred_reb," REB"),style="font-size: 0.75em; font-weight: 700; color: white; background: #7f1d1d; padding: 2px 6px; border-radius: 4px;"),
                        span(paste0(o_pred_ast," AST"),style="font-size: 0.75em; font-weight: 700; color: #1e293b; background: #fca5a5; padding: 2px 6px; border-radius: 4px;")),
                    div(style="font-size: 0.7em; color: #94a3b8; margin-top: 2px; font-style: italic; text-align: right;","\u2191 Predicted for this game"))
            ),
            div(style="margin-top: 12px;",
                div(style="display: flex; align-items: center; gap: 8px;",
                    span(rv$my_team,style=paste0("font-size: 0.75em; font-weight: 600; color: ",ohio_kings_primary,"; width: 80px;")),
                    div(style="flex-grow: 1; height: 14px; background: #fee2e2; border-radius: 7px; overflow: hidden;",
                        div(style=paste0("width: ",overall_adv,"%; height: 100%; background: linear-gradient(90deg, ",ohio_kings_primary,", ",ohio_kings_secondary,"); border-radius: 7px; transition: width 0.5s;"))),
                    span("Opp",style="font-size: 0.75em; font-weight: 600; color: #991b1b; width: 30px; text-align: right;"))),
            div(style="display: flex; gap: 15px; margin-top: 10px; justify-content: center;",
                span(paste0("\U0001f3af Scoring: ",round(scoring_adv,0),"%"),
                     style=paste0("font-size: 0.78em; color: ",if(scoring_adv>55)"#10b981" else if(scoring_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f3c0 Boards: ",round(rebound_adv,0),"%"),
                     style=paste0("font-size: 0.78em; color: ",if(rebound_adv>55)"#10b981" else if(rebound_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f3af Playmaking: ",round(assist_adv,0),"%"),
                     style=paste0("font-size: 0.78em; color: ",if(assist_adv>55)"#10b981" else if(assist_adv<45)"#ef4444" else "#64748b",";")),
                span(paste0("\U0001f6e1\ufe0f Defense: ",round(defense_adv,0),"%"),
                     style=paste0("font-size: 0.78em; color: ",if(defense_adv>55)"#10b981" else if(defense_adv<45)"#ef4444" else "#64748b",";")))
        )
      }),
      
      {
        total_k_pred_pts <- sum(sapply(1:n_matchups, function(i) {
          pred <- tryCatch(predict_player_game(as.character(kings_lineup$Player[i]),input$opponent_select),error=function(e)list(pts=NA))
          if(!is.null(pred$pts)&&!is.na(pred$pts)) pred$pts else kings_lineup$PPG[i]
        }))
        total_o_pred_pts <- sum(sapply(1:n_matchups, function(i) {
          pred <- tryCatch(predict_player_game(as.character(opp_lineup$Player[i]),rv$my_team),error=function(e)list(pts=NA))
          if(!is.null(pred$pts)&&!is.na(pred$pts)) pred$pts else opp_lineup$PPG[i]
        }))
        total_kings_adv <- mean(all_edges)
        matchups_won <- sum(all_edges>55)
        summary_color <- if(total_kings_adv>55)"#10b981" else if(total_kings_adv<45)"#ef4444" else "#f59e0b"
        
        get_bench_ppg2 <- function(team_name, lineup) {
          roster <- rv$aba_data$all_team_stats[[team_name]]
          if(is.null(roster)) return(0)
          bench <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"),!Player%in%lineup$Player) %>%
            mutate(bench_ppg=sapply(Player,function(p){
              tryCatch({pg<-rv$aba_data$all_player_stats[[p]]$per_game;
              if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) pg$PPG[1] else 0},error=function(e)0)})) %>%
            arrange(desc(bench_ppg)) %>% head(3)
          round(mean(bench$bench_ppg,na.rm=TRUE),1)
        }
        k_bench2 <- get_bench_ppg2(rv$my_team, kings_lineup)
        o_bench2 <- get_bench_ppg2(input$opponent_select, opp_lineup)
        bench_edge2 <- 100/(1+exp(-0.2*(k_bench2-o_bench2)))
        combined_edge2 <- round(total_kings_adv*0.85+bench_edge2*0.15,1)
        
        tagList(
          div(style="background: linear-gradient(135deg, #1e293b 0%, #334155 100%); color: white; padding: 24px; border-radius: 12px; margin-top: 15px;",
              h4("OVERALL MATCHUP SUMMARY",style="margin: 0 0 15px 0; opacity: 0.9; letter-spacing: 1px; text-align: center;"),
              div(style="display: flex; justify-content: center; align-items: center; gap: 40px; margin-bottom: 20px;",
                  div(style="text-align: center;",
                      h1(paste0(round(total_kings_adv,1),"%"),
                         style=paste0("margin: 0; font-size: 2.8em; font-weight: 800; color: ",summary_color,";")),
                      p("Starter Edge",style="margin: 5px 0 0 0; opacity: 0.8; font-size: 0.9em;")),
                  div(style="text-align: center;",
                      h2(paste0(matchups_won,"/",n_matchups),style="margin: 0; font-size: 2.2em; font-weight: 700;"),
                      p("Matchups Won",style="margin: 5px 0 0 0; opacity: 0.8; font-size: 0.9em;")),
                  div(style="text-align: center;",
                      h2(paste0(round(combined_edge2,1),"%"),
                         style=paste0("margin: 0; font-size: 2.2em; font-weight: 800; color: ",
                                      if(combined_edge2>55)"#22c55e" else if(combined_edge2<45)"#f87171" else "#fbbf24",";")),
                      p("Combined Edge",style="margin: 5px 0 0 0; opacity: 0.8; font-size: 0.9em;"))),
              div(style="background: rgba(255,255,255,0.08); border-radius: 10px; padding: 14px; margin-top: 10px;",
                  div(style="display: flex; justify-content: space-between; align-items: center;",
                      span("\U0001f4ba Bench Depth (Top 3 Avg PPG)",style="font-weight: 600; font-size: 0.9em; opacity: 0.9;"),
                      span(paste0(round(bench_edge2,1),"% ",rv$my_team),
                           style=paste0("font-weight: 700; font-size: 0.9em; color: ",
                                        if(bench_edge2>55)"#22c55e" else if(bench_edge2<45)"#f87171" else "#fbbf24",";"))),
                  div(style="display: flex; justify-content: space-between; margin-top: 10px;",
                      div(style="text-align: center; flex: 1;",
                          div(k_bench2,style="font-size: 1.5em; font-weight: 800;"),
                          div(paste0(rv$my_team," Bench"),style="font-size: 0.75em; opacity: 0.7;")),
                      div(style="text-align: center; flex: 0 0 60px; padding-top: 5px;",
                          div("VS",style="font-size: 1em; opacity: 0.5; font-weight: 700;")),
                      div(style="text-align: center; flex: 1;",
                          div(o_bench2,style="font-size: 1.5em; font-weight: 800;"),
                          div("Opp Bench",style="font-size: 0.75em; opacity: 0.7;")))
              )
          ),
          div(style="background: rgba(255,255,255,0.08); border-radius: 10px; padding: 14px; margin-top: 15px; background: #1e293b; color: white;",
              div(style="text-align: center; margin-bottom: 8px;",
                  span("PLAYER MODEL PREDICTED SCORE",style="font-size: 0.7em; font-weight: 700; opacity: 0.6; letter-spacing: 1px;")),
              div(style="display: flex; justify-content: center; align-items: center; gap: 20px;",
                  div(style="text-align: center;",
                      div(rv$my_team,style="font-size: 0.8em; opacity: 0.7;"),
                      div(round(total_k_pred_pts,0),style="font-size: 2.2em; font-weight: 800;")),
                  div("-",style="font-size: 1.5em; opacity: 0.5;"),
                  div(style="text-align: center;",
                      div(input$opponent_select,style="font-size: 0.8em; opacity: 0.7;"),
                      div(round(total_o_pred_pts,0),style="font-size: 2.2em; font-weight: 800;"))),
              p("Based on individual player linear regression model (starters only)",
                style="text-align: center; font-size: 0.7em; opacity: 0.5; margin: 8px 0 0 0;"))
        )
      }
    )
  })
  
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (is.null(input$scout_team) || input$scout_team == "")
      return(div(style="text-align: center; padding: 60px 20px;",
                 div("\U0001f50d",style="font-size: 4em; opacity: 0.3;"),
                 h3("Select an opponent to generate scouting report",style="color: #94a3b8; margin-top: 15px;"),
                 p("Choose a team from the dropdown above to see a full pre-game breakdown",style="color: #cbd5e1;")))
    
    tryCatch({
      
      opp_name   <- input$scout_team
      opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
      opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
      if (is.null(opp_roster)||nrow(opp_record)==0)
        return(p("No data available for this team",style="text-align: center; color: #999; padding: 40px;"))
      
      opp_gp   <- as.numeric(opp_record$W[1])+as.numeric(opp_record$L[1])
      opp_wpct <- if(opp_gp>0) round(opp_record$W[1]/opp_gp*100,0) else 50
      
      get_stat <- function(roster, col) {
        tr <- roster %>% filter(str_detect(toupper(Player),"TOTAL|TEAM"))
        if(nrow(tr)>0&&col%in%names(tr)) return(suppressWarnings(as.numeric(tr[[col]][1])))
        pr <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
        if(col%in%names(pr)) return(sum(suppressWarnings(as.numeric(pr[[col]])),na.rm=TRUE))
        return(0)
      }
      
      opp_pts<-get_stat(opp_roster,"PTS"); opp_reb<-get_stat(opp_roster,"REB"); opp_ast<-get_stat(opp_roster,"AST")
      opp_2pm<-get_stat(opp_roster,"2PM"); opp_2pa<-get_stat(opp_roster,"2PA")
      opp_3pm<-get_stat(opp_roster,"3PM"); opp_3pa<-get_stat(opp_roster,"3PA")
      opp_ftm<-get_stat(opp_roster,"FTM"); opp_fta<-get_stat(opp_roster,"FTA")
      opp_ppg<-get_accurate_team_ppg(opp_name,rv$aba_data)
      if(opp_ppg==0&&opp_gp>0) opp_ppg<-round(opp_pts/opp_gp,1)
      opp_rpg<-if(opp_gp>0) round(opp_reb/opp_gp,1) else 0
      opp_apg<-if(opp_gp>0) round(opp_ast/opp_gp,1) else 0
      
      pts_from_2<-opp_2pm*2; pts_from_3<-opp_3pm*3; pts_from_ft<-opp_ftm
      total_scored<-pts_from_2+pts_from_3+pts_from_ft
      if(total_scored<opp_pts&&total_scored>0){
        diff<-opp_pts-total_scored
        pts_from_2<-pts_from_2+round(diff*pts_from_2/total_scored)
        pts_from_3<-pts_from_3+round(diff*pts_from_3/total_scored)
        pts_from_ft<-opp_pts-pts_from_2-pts_from_3
      }
      pct_2 <-if(opp_pts>0) round(pts_from_2/opp_pts*100,0) else 0
      pct_3 <-if(opp_pts>0) round(pts_from_3/opp_pts*100,0) else 0
      pct_ft<-if(opp_pts>0) round(pts_from_ft/opp_pts*100,0) else 0
      
      fg2_pct   <-if(opp_2pa>0) round(opp_2pm/opp_2pa*100,1) else 0
      fg3_pct   <-if(opp_3pa>0) round(opp_3pm/opp_3pa*100,1) else 0
      ft_pct    <-if(opp_fta>0) round(opp_ftm/opp_fta*100,1) else 0
      three_rate<-if((opp_2pa+opp_3pa)>0) round(opp_3pa/(opp_2pa+opp_3pa)*100,0) else 0
      play_style<-if(three_rate>=40)"Perimeter-Heavy" else if(three_rate>=25)"Balanced" else "Interior-Focused"
      
      player_roster <- opp_roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
      
      key_players <- player_roster %>%
        mutate(
          player_ppg=sapply(Player,function(p){tryCatch({pg<-rv$aba_data$all_player_stats[[p]]$per_game;if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) pg$PPG[1] else 0},error=function(e)0)}),
          player_rpg=sapply(Player,function(p){tryCatch({pg<-rv$aba_data$all_player_stats[[p]]$per_game;if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$RPG[1])) pg$RPG[1] else 0},error=function(e)0)}),
          player_apg=sapply(Player,function(p){tryCatch({pg<-rv$aba_data$all_player_stats[[p]]$per_game;if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$APG[1])) pg$APG[1] else 0},error=function(e)0)}),
          player_pos=sapply(Player,function(p){tryCatch({pos<-rv$aba_data$all_player_stats[[p]]$position;if(!is.null(pos)&&!is.na(pos)) pos else "G"},error=function(e)"G")}),
          gp=sapply(Player,function(p){tryCatch({gl<-rv$aba_data$all_player_stats[[p]]$game_log;if(!is.null(gl)) nrow(gl) else 0},error=function(e)0)}),
          streak=sapply(Player,function(p){tryCatch({
            pd<-rv$aba_data$all_player_stats[[p]]
            if(is.null(pd$game_log)||nrow(pd$game_log)<3||is.null(pd$per_game)) return(0)
            rec<-pd$game_log%>%arrange(desc(date))%>%head(3)
            rec_avg<-mean(suppressWarnings(as.numeric(rec$PTS)),na.rm=TRUE)
            szn_avg<-pd$per_game$PPG[1]
            if(!is.na(rec_avg)&&!is.na(szn_avg)&&szn_avg>0) round(((rec_avg-szn_avg)/szn_avg)*100,1) else 0
          },error=function(e)0)})
        ) %>%
        filter(gp>=3,player_ppg>0) %>% arrange(desc(player_ppg)) %>% head(5)
      
      if(nrow(key_players)>=2){
        top_scorer_share<-if(opp_ppg>0) round(key_players$player_ppg[1]/opp_ppg*100,0) else 0
        top2_share      <-if(opp_ppg>0) round(sum(key_players$player_ppg[1:min(2,nrow(key_players))])/opp_ppg*100,0) else 0
        concentration   <-if(top_scorer_share>=30)"Star-Dependent" else if(top2_share>=50)"Top-Heavy" else "Balanced Scoring"
      } else { top_scorer_share<-0; top2_share<-0; concentration<-"Unknown" }
      
      all_team_ppgs <- sapply(names(rv$aba_data$all_team_stats),function(tn) get_accurate_team_ppg(tn,rv$aba_data))
      ppg_rank  <- sum(all_team_ppgs>=opp_ppg)
      ppg_total <- length(all_team_ppgs[all_team_ppgs>0])
      
      kings_record  <- rv$aba_data$league_data %>% filter(Team==rv$my_team)
      kings_gp      <- if(nrow(kings_record)>0) as.numeric(kings_record$W[1])+as.numeric(kings_record$L[1]) else 15
      kings_ppg     <- get_accurate_team_ppg(rv$my_team,rv$aba_data)
      kings_roster  <- rv$aba_data$all_team_stats[[rv$my_team]]
      if(kings_ppg==0&&!is.null(kings_roster)&&kings_gp>0) kings_ppg<-round(get_stat(kings_roster,"PTS")/kings_gp,1)
      kings_rpg     <- if(!is.null(kings_roster)&&kings_gp>0) round(get_stat(kings_roster,"REB")/kings_gp,1) else 0
      kings_apg_team<- if(!is.null(kings_roster)&&kings_gp>0) round(get_stat(kings_roster,"AST")/kings_gp,1) else 0
      kings_wpct    <- if(kings_gp>0) round(kings_record$W[1]/kings_gp*100,0) else 50
      
      hot_players  <- key_players %>% filter(streak>15)
      cold_players <- key_players %>% filter(streak<-15)
      threat       <- if(opp_wpct>=70)"HIGH" else if(opp_wpct>=50)"MODERATE" else "LOW"
      threat_color <- if(threat=="HIGH")"#b91c1c" else if(threat=="MODERATE")"#d97706" else "#15803d"
      
      opp_logo <- ""
      tryCatch({
        if("logo_url"%in%names(rv$aba_data$team_lookup)){
          lr<-rv$aba_data$team_lookup%>%filter(team_name==opp_name)%>%pull(logo_url)
          if(length(lr)>0&&!is.na(lr[1])) opp_logo<-lr[1]
        }
      },error=function(e){})
      
      # Strategy — exact original
      strategy_points <- c()
      if(nrow(key_players)>=2){
        significant_scorers<-key_players%>%filter(player_ppg>=opp_ppg*0.15); n_threats<-nrow(significant_scorers)
        if(n_threats==1){
          strategy_points<-c(strategy_points,paste0("Contain ",significant_scorers$Player[1]," (",significant_scorers$player_ppg[1]," PPG, ",top_scorer_share,"% of scoring) \u2014 this team has one dominant option. Double aggressively and force role players to create"))
        } else if(n_threats==2){
          p1<-significant_scorers[1,];p2<-significant_scorers[2,];p2_share<-if(opp_ppg>0) round(p2$player_ppg/opp_ppg*100,0) else 0
          strategy_points<-c(strategy_points,paste0("Two-headed attack: ",p1$Player," (",p1$player_ppg," PPG, ",top_scorer_share,"%) and ",p2$Player," (",p2$player_ppg," PPG, ",p2_share,"%) combine for ",top2_share,"% of scoring \u2014 can't focus on just one. Need strong individual defense on both"))
        } else {
          strategy_points<-c(strategy_points,paste0("Deep scoring team with ",n_threats," players averaging 15%+ of team scoring \u2014 no single player to shut down. Prioritize team defense and communication"))
        }
      } else if(nrow(key_players)==1){
        strategy_points<-c(strategy_points,paste0("Limit ",key_players$Player[1]," (",key_players$player_ppg[1]," PPG) \u2014 primary offensive threat"))
      }
      if(three_rate>=35&&fg3_pct>=35){
        strategy_points<-c(strategy_points,paste0("DANGER from 3: They shoot ",three_rate,"% of shots from deep at ",fg3_pct,"% \u2014 close out hard, no open looks. Contest every three"))
      } else if(three_rate>=35&&fg3_pct<35){
        strategy_points<-c(strategy_points,paste0("They attempt a lot of 3s (",three_rate,"% of shots) but shoot poorly (",fg3_pct,"%) \u2014 let them settle for contested threes, don't over-help"))
      } else if(three_rate<25&&fg2_pct>=50){
        strategy_points<-c(strategy_points,paste0("Interior-focused team shooting ",fg2_pct,"% from 2 \u2014 pack the paint, wall up on drives, force them into uncomfortable perimeter shots"))
      } else if(three_rate<25){
        strategy_points<-c(strategy_points,paste0("They rarely shoot 3s (",three_rate,"% of attempts) \u2014 sag off perimeter players and clog driving lanes"))
      } else {
        strategy_points<-c(strategy_points,paste0("Balanced shooting attack (",three_rate,"% from 3 at ",fg3_pct,"%, ",fg2_pct,"% on 2s) \u2014 play honest defense, don't overcommit"))
      }
      if(opp_rpg>kings_rpg+3){
        strategy_points<-c(strategy_points,paste0("REBOUNDING MISMATCH: They average ",opp_rpg," RPG vs our ",kings_rpg," \u2014 must crash boards harder, consider bigger lineup"))
      } else if(opp_rpg>kings_rpg){
        strategy_points<-c(strategy_points,paste0("Slight rebounding edge to them (",opp_rpg," vs our ",kings_rpg," RPG) \u2014 box out consistently, don't give up second chances"))
      } else if(kings_rpg>opp_rpg+3){
        strategy_points<-c(strategy_points,paste0("We dominate the glass (",kings_rpg," vs their ",opp_rpg," RPG) \u2014 push tempo off defensive rebounds, attack offensive glass for second chances"))
      } else {
        strategy_points<-c(strategy_points,paste0("Even on the boards (",kings_rpg," vs ",opp_rpg," RPG) \u2014 effort and positioning will be the difference"))
      }
      if(opp_apg>18){strategy_points<-c(strategy_points,paste0("High assist team (",opp_apg," APG) \u2014 they move the ball well. Disrupt passing lanes, pressure the ball handler"))}
      else if(opp_apg<10){strategy_points<-c(strategy_points,paste0("Low assist team (",opp_apg," APG) \u2014 isolation-heavy offense. Stay in front of your man, limit dribble penetration"))}
      if(ft_pct>=75){strategy_points<-c(strategy_points,paste0("Strong FT shooting (",ft_pct,"%) \u2014 avoid putting them on the line in close games"))}
      else if(ft_pct<60){strategy_points<-c(strategy_points,paste0("Poor FT shooting (",ft_pct,"%) \u2014 consider strategic fouling in late-game situations"))}
      if(nrow(hot_players)>0){
        hot_details<-sapply(1:nrow(hot_players),function(i) paste0(hot_players$Player[i]," (+",hot_players$streak[i],"% above avg)"))
        strategy_points<-c(strategy_points,paste0("\U0001f525 HOT ALERT: ",paste(hot_details,collapse=", ")," \u2014 extra attention needed, playing above season average"))
      }
      if(nrow(cold_players)>0){
        cold_details<-sapply(1:nrow(cold_players),function(i) paste0(cold_players$Player[i]," (",cold_players$streak[i],"% below avg)"))
        strategy_points<-c(strategy_points,paste0("\u2744\ufe0f Cold stretch: ",paste(cold_details,collapse=", ")," \u2014 still dangerous, don't ignore them, but may be less aggressive"))
      }
      if(kings_ppg>opp_ppg+10){strategy_points<-c(strategy_points,paste0("\U0001f4aa We outscore them by ",round(kings_ppg-opp_ppg,0)," PPG \u2014 push the pace, get into transition, make this a track meet"))}
      else if(opp_ppg>kings_ppg+10){strategy_points<-c(strategy_points,paste0("\u26a0\ufe0f They outscore us by ",round(opp_ppg-kings_ppg,0)," PPG \u2014 slow the game down, limit possessions, make every trip count"))}
      
      div(
        div(style="background: linear-gradient(135deg, #1e293b 0%, #334155 100%); color: white; padding: 30px; border-radius: 14px; margin-bottom: 20px; position: relative; overflow: hidden;",
            div(style="position: absolute; right: -20px; top: -20px; font-size: 12em; opacity: 0.03; font-weight: 900;","\U0001f50d"),
            div(style="display: flex; align-items: center; gap: 20px; flex-wrap: wrap;",
                if(opp_logo!="") tags$img(src=opp_logo,style="width: 80px; height: 80px; border-radius: 50%; border: 3px solid rgba(255,255,255,0.3); object-fit: cover; background: white;"),
                div(div("PRE-GAME SCOUTING REPORT",style="font-size: 0.75em; font-weight: 600; opacity: 0.6; letter-spacing: 2px;"),
                    h2(opp_name,style="margin: 5px 0 0 0; font-weight: 800; font-size: 1.8em;"),
                    div(paste0(opp_record$Division[1]," Division | ",opp_record$W[1],"-",opp_record$L[1]," (",opp_wpct,"% win rate)"),style="opacity: 0.7; margin-top: 4px;")),
                div(style="margin-left: auto; text-align: right;",
                    div("THREAT LEVEL",style="font-size: 0.7em; opacity: 0.5; letter-spacing: 1px;"),
                    div(threat,style=paste0("font-size: 1.8em; font-weight: 800; color: ",threat_color,"; background: rgba(255,255,255,0.1); padding: 4px 20px; border-radius: 8px; margin-top: 4px;")))
            )
        ),
        
        div(style="display: flex; gap: 12px; margin-bottom: 20px; flex-wrap: wrap;",
            lapply(list(
              list("\U0001f3c0","PPG", opp_ppg,             paste0("#",ppg_rank,"/",ppg_total)),
              list("\U0001f4aa","RPG", opp_rpg,             "per game"),
              list("\U0001f91d","APG", opp_apg,             "per game"),
              list("\U0001f3af","2P%", paste0(fg2_pct,"%"), "from inside"),
              list("\u2604\ufe0f","3P%",paste0(fg3_pct,"%"),paste0(three_rate,"% of shots")),
              list("\u2705",    "FT%", paste0(ft_pct,"%"),  "from the line")
            ),function(item)
              div(style="flex: 1; min-width: 120px; background: white; border: 1px solid #e2e8f0; border-radius: 10px; padding: 14px; text-align: center;",
                  div(item[[1]],style="font-size: 1.3em;"),
                  div(item[[3]],style="font-size: 1.5em; font-weight: 800; color: #1e293b; margin: 4px 0;"),
                  div(item[[2]],style="font-size: 0.75em; font-weight: 700; color: #64748b;"),
                  div(item[[4]],style="font-size: 0.65em; color: #94a3b8; margin-top: 2px;"))
            )
        ),
        
        div(style="display: flex; gap: 20px; flex-wrap: wrap;",
            div(style="flex: 1; min-width: 350px;",
                div(style="background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px; margin-bottom: 16px;",
                    h4("\U0001f464 Key Players to Watch",style="margin: 0 0 14px 0; color: #334155; font-weight: 700;"),
                    if(nrow(key_players)>0) lapply(1:nrow(key_players),function(i){
                      kp<-key_players[i,]
                      is_primary<-i<=2&&kp$player_ppg>=opp_ppg*0.15
                      streak_icon<-if(kp$streak>15)"\U0001f525" else if(kp$streak<-15)"\u2744\ufe0f" else ""
                      streak_text<-if(kp$streak>15)"HOT" else if(kp$streak<-15)"COLD" else ""
                      streak_col <-if(kp$streak>15)"#b91c1c" else if(kp$streak<-15)"#3b82f6" else "#64748b"
                      player_share<-if(opp_ppg>0) round(kp$player_ppg/opp_ppg*100,0) else 0
                      div(style=paste0("padding: 12px; border-radius: 10px; margin-bottom: 8px; border-left: 4px solid ",
                                       if(is_primary)"#b91c1c" else "#e2e8f0",";",
                                       if(is_primary)" background: #fef2f2;" else " background: #f8fafc;"),
                          div(style="display: flex; justify-content: space-between; align-items: start;",
                              div(div(style="display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
                                      span(paste0("#",i),style="font-weight: 800; color: #94a3b8; font-size: 0.85em;"),
                                      strong(kp$Player,style=paste0("font-size: 1em; color: ",if(is_primary)"#991b1b" else "#334155",";")),
                                      span(paste0("(",kp$player_pos,")"),style="color: #64748b; font-size: 0.85em;"),
                                      if(streak_text!="") span(paste0(streak_icon," ",streak_text),
                                                               style=paste0("font-size: 0.7em; font-weight: 700; color: ",streak_col,"; background: ",streak_col,"15; padding: 1px 6px; border-radius: 4px;"))),
                                  div(style="display: flex; gap: 8px; margin-top: 6px;",
                                      span(paste0(kp$player_ppg," PPG"),style=paste0("font-size: 0.8em; font-weight: 700; color: ",ohio_kings_primary,"; background: #fee2e2; padding: 2px 6px; border-radius: 4px;")),
                                      span(paste0(kp$player_rpg," RPG"),style="font-size: 0.8em; font-weight: 700; color: #1e40af; background: #dbeafe; padding: 2px 6px; border-radius: 4px;"),
                                      span(paste0(kp$player_apg," APG"),style="font-size: 0.8em; font-weight: 700; color: #b45309; background: #fef3c7; padding: 2px 6px; border-radius: 4px;")),
                                  div(paste0(kp$gp," games played"),style="font-size: 0.7em; color: #94a3b8; margin-top: 4px;")),
                              if(player_share>=15) div(style=paste0("text-align: center; background: ",if(is_primary)"#b91c1c" else "#64748b","; color: white; padding: 4px 10px; border-radius: 8px; font-size: 0.7em; font-weight: 700; white-space: nowrap;"),
                                                       paste0(player_share,"% of\nteam PPG"))))
                    }) else p("No player data available",style="color: #94a3b8; text-align: center;")
                ),
                div(style="background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px;",
                    h4("\U0001f4ca Team Tendencies",style="margin: 0 0 14px 0; color: #334155; font-weight: 700;"),
                    div(style="display: flex; gap: 12px; margin-bottom: 14px;",
                        div(style=paste0("flex: 1; padding: 12px; border-radius: 8px; text-align: center; background: ",
                                         if(play_style=="Perimeter-Heavy")"#dbeafe" else if(play_style=="Interior-Focused")"#fee2e2" else "#f0fdf4",";"),
                            div(play_style,style="font-weight: 700; font-size: 0.95em; color: #334155;"),
                            div(paste0(three_rate,"% of shots from 3"),style="font-size: 0.75em; color: #64748b; margin-top: 2px;")),
                        div(style=paste0("flex: 1; padding: 12px; border-radius: 8px; text-align: center; background: ",
                                         if(concentration=="Star-Dependent")"#fef2f2" else if(concentration=="Balanced Scoring")"#f0fdf4" else "#fffbeb",";"),
                            div(concentration,style="font-weight: 700; font-size: 0.95em; color: #334155;"),
                            div(paste0("Top 2 = ",top2_share,"% of scoring"),style="font-size: 0.75em; color: #64748b; margin-top: 2px;"))
                    ),
                    div(div(style="display: flex; justify-content: space-between; margin-bottom: 6px;",
                            span("Scoring Distribution",style="font-weight: 600; color: #475569; font-size: 0.85em;"),
                            span(paste0(opp_ppg," PPG"),style="font-weight: 700; color: #334155; font-size: 0.85em;")),
                        div(style="height: 28px; border-radius: 8px; overflow: hidden; display: flex;",
                            div(style=paste0("width: ",pct_2,"%; background: ",ohio_kings_primary,"; display: flex; align-items: center; justify-content: center;"),
                                if(pct_2>=15) span(paste0("2P ",pct_2,"%"),style="color: white; font-size: 0.65em; font-weight: 700;")),
                            div(style=paste0("width: ",pct_3,"%; background: ",ohio_kings_secondary,"; display: flex; align-items: center; justify-content: center;"),
                                if(pct_3>=15) span(paste0("3P ",pct_3,"%"),style="color: white; font-size: 0.65em; font-weight: 700;")),
                            div(style=paste0("width: ",pct_ft,"%; background: ",ohio_kings_accent,"; display: flex; align-items: center; justify-content: center;"),
                                if(pct_ft>=10) span(paste0("FT ",pct_ft,"%"),style="color: #1e293b; font-size: 0.65em; font-weight: 700;"))))
                )
            ),
            
            div(style="flex: 1; min-width: 350px;",
                div(style="background: white; border: 2px solid #15803d; border-radius: 12px; padding: 20px; margin-bottom: 16px;",
                    h4("\u2705 How to Beat Them",style="margin: 0 0 14px 0; color: #15803d; font-weight: 700;"),
                    lapply(seq_along(strategy_points),function(i){
                      sp<-strategy_points[i]
                      is_alert<-grepl("\U0001f525 HOT",sp); is_cold<-grepl("\u2744",sp)
                      is_danger<-grepl("DANGER|MISMATCH",sp); is_advantage<-grepl("dominate|outscore|We ",sp)
                      bg_s<-if(is_alert)" background: #fef2f2; border: 1px solid #fecaca;"
                      else if(is_cold)" background: #eff6ff; border: 1px solid #bfdbfe;"
                      else if(is_danger)" background: #fffbeb; border: 1px solid #fde68a;"
                      else " background: #f0fdf4; border: 1px solid #bbf7d0;"
                      nb<-if(is_alert)"#b91c1c" else if(is_cold)"#3b82f6" else if(is_danger)"#d97706" else "#15803d"
                      tc_s<-if(is_alert)"#991b1b" else if(is_cold)"#1e40af" else if(is_danger)"#92400e" else "#166634"
                      div(style=paste0("padding: 10px 12px; border-radius: 8px; margin-bottom: 8px; display: flex; align-items: start; gap: 10px;",bg_s),
                          div(style=paste0("width: 24px; height: 24px; border-radius: 50%; flex-shrink: 0; display: flex; align-items: center; justify-content: center; font-weight: 800; font-size: 0.75em; color: white; background: ",nb,";"),i),
                          p(sp,style=paste0("margin: 0; font-size: 0.88em; font-weight: 600; color: ",tc_s,";")))
                    })
                ),
                div(style="background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px;",
                    h4(paste0("\u2694\ufe0f ",rv$my_team," vs ",opp_name),style="margin: 0 0 14px 0; color: #334155; font-weight: 700;"),
                    lapply(list(
                      list("Record",paste0(kings_record$W[1],"-",kings_record$L[1]),paste0(opp_record$W[1],"-",opp_record$L[1]),kings_wpct,opp_wpct),
                      list("PPG",kings_ppg,opp_ppg,kings_ppg,opp_ppg),
                      list("RPG",kings_rpg,opp_rpg,kings_rpg,opp_rpg),
                      list("APG",kings_apg_team,opp_apg,kings_apg_team,opp_apg)
                    ),function(comp){
                      k_num<-suppressWarnings(as.numeric(comp[[4]])); o_num<-suppressWarnings(as.numeric(comp[[5]]))
                      if(is.na(k_num)) k_num<-50; if(is.na(o_num)) o_num<-50
                      total<-k_num+o_num; k_pct<-if(total>0) round(k_num/total*100,0) else 50; k_wins<-k_num>o_num
                      div(style="margin-bottom: 10px;",
                          div(style="display: flex; justify-content: space-between; margin-bottom: 4px;",
                              span(paste0(rv$my_team,": ",comp[[2]]),style=paste0("font-size: 0.8em; font-weight: 700; color: ",if(k_wins)"#15803d" else "#b91c1c",";")),
                              span(comp[[1]],style="font-size: 0.75em; font-weight: 600; color: #94a3b8;"),
                              span(paste0(comp[[3]]," :",opp_name),style=paste0("font-size: 0.8em; font-weight: 700; color: ",if(!k_wins)"#15803d" else "#b91c1c",";"))),
                          div(style="height: 8px; border-radius: 4px; display: flex; overflow: hidden;",
                              div(style=paste0("width: ",k_pct,"%; background: ",ohio_kings_primary,";")),
                              div(style=paste0("width: ",100-k_pct,"%; background: #94a3b8;"))))
                    })
                )
            )
        ),
        
        div(style="background: linear-gradient(135deg, #1e293b, #334155); color: white; border-radius: 12px; padding: 24px; margin-top: 20px;",
            h4("\U0001f4dd Game Preview",style="margin: 0 0 12px 0; opacity: 0.8; letter-spacing: 0.5px;"),
            {
              narrative_parts <- c()
              narrative_parts<-c(narrative_parts,paste0("The ",opp_name," enter at ",opp_record$W[1],"-",opp_record$L[1]," (",opp_wpct,"%) from the ",opp_record$Division[1]," Division, averaging ",opp_ppg," points per game (",if(ppg_rank<=10)"one of the league's elite offenses" else if(ppg_rank<=ppg_total*0.33)"an above-average scoring team" else if(ppg_rank<=ppg_total*0.66)"a middle-of-the-pack offense" else "a below-average scoring team",", #",ppg_rank," of ",ppg_total,")."))
              narrative_parts<-c(narrative_parts,paste0("Their offense is ",tolower(play_style),
                                                        if(play_style=="Perimeter-Heavy") paste0(", launching ",three_rate,"% of their shots from deep",if(fg3_pct>=38) paste0(" and converting at a lethal ",fg3_pct,"%.") else if(fg3_pct>=30) paste0(" at a solid ",fg3_pct,"% clip.") else paste0(" though they only connect at ",fg3_pct,"%, making them volume shooters rather than efficient ones."))
                                                        else if(play_style=="Interior-Focused") paste0(", preferring to work inside where they shoot ",fg2_pct,"% from two-point range.")
                                                        else paste0(" with ",three_rate,"% of shots from three (",fg3_pct,"%) and a ",fg2_pct,"% mark from inside.")))
              if(nrow(key_players)>=2){
                p1<-key_players[1,]; p2<-key_players[2,]; p2_share<-if(opp_ppg>0) round(p2$player_ppg/opp_ppg*100,0) else 0
                if(concentration=="Star-Dependent"){
                  p1_last <- tryCatch({parts<-strsplit(as.character(p1$Player)," ")[[1]];tail(parts,1)},error=function(e) as.character(p1$Player))
                  narrative_parts<-c(narrative_parts,paste0(p1$Player," is the engine, carrying ",top_scorer_share,"% of the scoring load at ",p1$player_ppg," PPG. When ",p1_last," is contained, this team struggles to find alternative offense."))
                } else if(top2_share>=45){
                  narrative_parts<-c(narrative_parts,paste0("The scoring punch comes from ",p1$Player," (",p1$player_ppg," PPG, ",top_scorer_share,"%) and ",p2$Player," (",p2$player_ppg," PPG, ",p2_share,"%). Both are legitimate threats that demand attention \u2014 you can't sell out to stop just one."))
                } else {
                  narrative_parts<-c(narrative_parts,paste0(p1$Player," leads at ",p1$player_ppg," PPG but the scoring is well-distributed across the roster. This team doesn't rely on one player to bail them out."))
                }
              }
              if(nrow(hot_players)>0||nrow(cold_players)>0){
                mt<-c()
                if(nrow(hot_players)>0) mt<-c(mt,paste0(paste(hot_players$Player,collapse=" and "),if(nrow(hot_players)==1)" is" else " are"," trending up recently"))
                if(nrow(cold_players)>0) mt<-c(mt,paste0(paste(cold_players$Player,collapse=" and "),if(nrow(cold_players)==1)" has" else " have"," been in a scoring slump, though ",if(nrow(cold_players)==1)"this player" else "these players"," can't be ignored based on season averages"))
                narrative_parts<-c(narrative_parts,paste0("Momentum-wise, ",paste(mt,collapse=", while "),"."))
              }
              narrative_parts<-c(narrative_parts,paste0(
                "For the ",rv$my_team,", ",
                if(kings_ppg>opp_ppg+15) paste0("this should be a favorable matchup on paper \u2014 we outscore them by ",round(kings_ppg-opp_ppg,0)," PPG. The key is staying focused and not playing down to the competition. ")
                else if(kings_ppg>opp_ppg) paste0("we hold a scoring edge (",kings_ppg," vs ",opp_ppg," PPG) but can't afford complacency. ")
                else if(opp_ppg>kings_ppg+10) paste0("we'll need to overcome a significant scoring gap (",kings_ppg," vs ",opp_ppg," PPG). Tempo control and defense will be critical. ")
                else paste0("this projects as a competitive game with similar scoring outputs (",kings_ppg," vs ",opp_ppg," PPG). Execution and adjustments will decide it. "),
                if(opp_rpg>kings_rpg+3) paste0("Their rebounding advantage (",opp_rpg," vs our ",kings_rpg," RPG) is a real concern that needs to be addressed in the game plan. ")
                else if(kings_rpg>opp_rpg+3) paste0("Our edge on the boards (",kings_rpg," vs ",opp_rpg," RPG) should translate to extra possessions and transition opportunities. ")
                else "",
                "Overall, this is a ",tolower(threat),"-threat opponent that ",
                if(threat=="HIGH") "demands our full attention and best effort."
                else if(threat=="MODERATE") "we should handle with proper preparation and focus."
                else "we're expected to beat, but can't afford to take lightly."))
              p(paste(narrative_parts,collapse=" "),style="margin: 0; line-height: 1.8; font-size: 0.95em; opacity: 0.9;")
            }
        ),
        
        div(style="text-align: center; margin-top: 20px; padding: 15px; color: #94a3b8; font-size: 0.8em;",
            paste0("Generated ",format(Sys.time(),"%B %d, %Y at %I:%M %p")," | ",rv$my_team," Scouting Dashboard"),
            div(style="margin-top: 8px;",
                actionButton("print_report","\U0001f5a8\ufe0f Print Report",class="btn-sm",
                             style="background: #475569; color: white; border: none; padding: 8px 20px; border-radius: 8px; font-weight: 600;",
                             onclick="window.print();"))
        )
      )
      
    },error=function(e){
      div(style="background: #fee2e2; border-left: 4px solid #ef4444; padding: 20px; border-radius: 10px; margin: 20px;",
          h4("Error loading scouting report",style="color: #991b1b; margin: 0 0 8px 0;"),
          p(paste("Details:",conditionMessage(e)),style="color: #7f1d1d; margin: 0; font-size: 0.9em;"),
          p("Try selecting a different team or refreshing data.",style="color: #991b1b; margin: 8px 0 0 0;"))
    })
  })
  
  # ============================================================================
  # PATCH 6 — paste BEFORE  } # end server  (after patch5)
  # Fixes: dplyr streak filter crash  filter(streak < -15)
  # ============================================================================
  
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (is.null(input$scout_team) || input$scout_team == "")
      return(div(style="text-align: center; padding: 60px 20px;",
                 div("\U0001f50d",style="font-size: 4em; opacity: 0.3;"),
                 h3("Select an opponent to generate scouting report",style="color: #94a3b8; margin-top: 15px;"),
                 p("Choose a team from the dropdown above to see a full pre-game breakdown",style="color: #cbd5e1;")))
    
    tryCatch({
      
      opp_name   <- input$scout_team
      opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
      opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
      if (is.null(opp_roster) || nrow(opp_record) == 0)
        return(p("No data available for this team", style="text-align: center; color: #999; padding: 40px;"))
      
      opp_gp   <- as.numeric(opp_record$W[1]) + as.numeric(opp_record$L[1])
      opp_wpct <- if (opp_gp > 0) round(opp_record$W[1] / opp_gp * 100, 0) else 50
      
      get_stat <- function(roster, col) {
        tr <- roster %>% filter(str_detect(toupper(Player), "TOTAL|TEAM"))
        if (nrow(tr) > 0 && col %in% names(tr)) return(suppressWarnings(as.numeric(tr[[col]][1])))
        pr <- roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
        if (col %in% names(pr)) return(sum(suppressWarnings(as.numeric(pr[[col]])), na.rm = TRUE))
        return(0)
      }
      
      opp_pts <- get_stat(opp_roster,"PTS"); opp_reb <- get_stat(opp_roster,"REB"); opp_ast <- get_stat(opp_roster,"AST")
      opp_2pm <- get_stat(opp_roster,"2PM"); opp_2pa <- get_stat(opp_roster,"2PA")
      opp_3pm <- get_stat(opp_roster,"3PM"); opp_3pa <- get_stat(opp_roster,"3PA")
      opp_ftm <- get_stat(opp_roster,"FTM"); opp_fta <- get_stat(opp_roster,"FTA")
      opp_ppg <- get_accurate_team_ppg(opp_name, rv$aba_data)
      if (opp_ppg == 0 && opp_gp > 0) opp_ppg <- round(opp_pts / opp_gp, 1)
      opp_rpg <- if (opp_gp > 0) round(opp_reb / opp_gp, 1) else 0
      opp_apg <- if (opp_gp > 0) round(opp_ast / opp_gp, 1) else 0
      
      pts_from_2 <- opp_2pm*2; pts_from_3 <- opp_3pm*3; pts_from_ft <- opp_ftm
      total_scored <- pts_from_2 + pts_from_3 + pts_from_ft
      if (total_scored < opp_pts && total_scored > 0) {
        diff <- opp_pts - total_scored
        pts_from_2  <- pts_from_2  + round(diff * pts_from_2  / total_scored)
        pts_from_3  <- pts_from_3  + round(diff * pts_from_3  / total_scored)
        pts_from_ft <- opp_pts - pts_from_2 - pts_from_3
      }
      pct_2  <- if (opp_pts > 0) round(pts_from_2  / opp_pts * 100, 0) else 0
      pct_3  <- if (opp_pts > 0) round(pts_from_3  / opp_pts * 100, 0) else 0
      pct_ft <- if (opp_pts > 0) round(pts_from_ft / opp_pts * 100, 0) else 0
      
      fg2_pct    <- if (opp_2pa > 0) round(opp_2pm / opp_2pa * 100, 1) else 0
      fg3_pct    <- if (opp_3pa > 0) round(opp_3pm / opp_3pa * 100, 1) else 0
      ft_pct     <- if (opp_fta > 0) round(opp_ftm / opp_fta * 100, 1) else 0
      three_rate <- if ((opp_2pa + opp_3pa) > 0) round(opp_3pa / (opp_2pa + opp_3pa) * 100, 0) else 0
      play_style <- if (three_rate >= 40) "Perimeter-Heavy" else if (three_rate >= 25) "Balanced" else "Interior-Focused"
      
      player_roster <- opp_roster %>% filter(!str_detect(toupper(Player), "TOTAL|TEAM"))
      
      key_players <- player_roster %>%
        mutate(
          player_ppg = sapply(Player, function(p) { tryCatch({
            pg <- rv$aba_data$all_player_stats[[p]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$PPG[1])) pg$PPG[1] else 0
          }, error = function(e) 0) }),
          player_rpg = sapply(Player, function(p) { tryCatch({
            pg <- rv$aba_data$all_player_stats[[p]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$RPG[1])) pg$RPG[1] else 0
          }, error = function(e) 0) }),
          player_apg = sapply(Player, function(p) { tryCatch({
            pg <- rv$aba_data$all_player_stats[[p]]$per_game
            if (!is.null(pg) && nrow(pg) > 0 && !is.na(pg$APG[1])) pg$APG[1] else 0
          }, error = function(e) 0) }),
          player_pos = sapply(Player, function(p) { tryCatch({
            pos <- rv$aba_data$all_player_stats[[p]]$position
            if (!is.null(pos) && !is.na(pos)) pos else "G"
          }, error = function(e) "G") }),
          gp = sapply(Player, function(p) { tryCatch({
            gl <- rv$aba_data$all_player_stats[[p]]$game_log
            if (!is.null(gl)) nrow(gl) else 0
          }, error = function(e) 0) }),
          streak_val = sapply(Player, function(p) { tryCatch({
            pd <- rv$aba_data$all_player_stats[[p]]
            if (is.null(pd$game_log) || nrow(pd$game_log) < 3 || is.null(pd$per_game)) return(0)
            rec     <- pd$game_log %>% arrange(desc(date)) %>% head(3)
            rec_avg <- mean(suppressWarnings(as.numeric(rec$PTS)), na.rm = TRUE)
            szn_avg <- pd$per_game$PPG[1]
            if (!is.na(rec_avg) && !is.na(szn_avg) && szn_avg > 0)
              round(((rec_avg - szn_avg) / szn_avg) * 100, 1) else 0
          }, error = function(e) 0) })
        ) %>%
        filter(gp >= 3, player_ppg > 0) %>%
        arrange(desc(player_ppg)) %>%
        head(5)
      
      # Use streak_val (not streak) to avoid dplyr NSE parsing -15 as an expression
      hot_players  <- key_players %>% filter(streak_val > 15)
      cold_players <- key_players %>% filter(streak_val < (-15))
      
      if (nrow(key_players) >= 2) {
        top_scorer_share <- if (opp_ppg > 0) round(key_players$player_ppg[1] / opp_ppg * 100, 0) else 0
        top2_share       <- if (opp_ppg > 0) round(sum(key_players$player_ppg[1:min(2, nrow(key_players))]) / opp_ppg * 100, 0) else 0
        concentration    <- if (top_scorer_share >= 30) "Star-Dependent" else if (top2_share >= 50) "Top-Heavy" else "Balanced Scoring"
      } else { top_scorer_share <- 0; top2_share <- 0; concentration <- "Unknown" }
      
      all_team_ppgs <- sapply(names(rv$aba_data$all_team_stats), function(tn) get_accurate_team_ppg(tn, rv$aba_data))
      ppg_rank  <- sum(all_team_ppgs >= opp_ppg)
      ppg_total <- length(all_team_ppgs[all_team_ppgs > 0])
      
      kings_record   <- rv$aba_data$league_data %>% filter(Team == rv$my_team)
      kings_gp       <- if (nrow(kings_record) > 0) as.numeric(kings_record$W[1]) + as.numeric(kings_record$L[1]) else 15
      kings_ppg      <- get_accurate_team_ppg(rv$my_team, rv$aba_data)
      kings_roster   <- rv$aba_data$all_team_stats[[rv$my_team]]
      if (kings_ppg == 0 && !is.null(kings_roster) && kings_gp > 0)
        kings_ppg <- round(get_stat(kings_roster, "PTS") / kings_gp, 1)
      kings_rpg      <- if (!is.null(kings_roster) && kings_gp > 0) round(get_stat(kings_roster, "REB") / kings_gp, 1) else 0
      kings_apg_team <- if (!is.null(kings_roster) && kings_gp > 0) round(get_stat(kings_roster, "AST") / kings_gp, 1) else 0
      kings_wpct     <- if (kings_gp > 0) round(kings_record$W[1] / kings_gp * 100, 0) else 50
      
      threat       <- if (opp_wpct >= 70) "HIGH" else if (opp_wpct >= 50) "MODERATE" else "LOW"
      threat_color <- if (threat == "HIGH") "#b91c1c" else if (threat == "MODERATE") "#d97706" else "#15803d"
      
      opp_logo <- ""
      tryCatch({
        if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
          lr <- rv$aba_data$team_lookup %>% filter(team_name == opp_name) %>% pull(logo_url)
          if (length(lr) > 0 && !is.na(lr[1])) opp_logo <- lr[1]
        }
      }, error = function(e) {})
      
      # Strategy — exact original wording
      strategy_points <- c()
      if (nrow(key_players) >= 2) {
        significant_scorers <- key_players %>% filter(player_ppg >= opp_ppg * 0.15)
        n_threats <- nrow(significant_scorers)
        if (n_threats == 1) {
          strategy_points <- c(strategy_points, paste0(
            "Contain ", significant_scorers$Player[1], " (", significant_scorers$player_ppg[1], " PPG, ",
            top_scorer_share, "% of scoring) \u2014 this team has one dominant option. Double aggressively and force role players to create"))
        } else if (n_threats == 2) {
          p1 <- significant_scorers[1,]; p2 <- significant_scorers[2,]
          p2_share <- if (opp_ppg > 0) round(p2$player_ppg / opp_ppg * 100, 0) else 0
          strategy_points <- c(strategy_points, paste0(
            "Two-headed attack: ", p1$Player, " (", p1$player_ppg, " PPG, ", top_scorer_share,
            "%) and ", p2$Player, " (", p2$player_ppg, " PPG, ", p2_share,
            "%) combine for ", top2_share, "% of scoring \u2014 can't focus on just one. Need strong individual defense on both"))
        } else if (n_threats >= 3) {
          strategy_points <- c(strategy_points, paste0(
            "Deep scoring team with ", n_threats, " players averaging 15%+ of team scoring \u2014 ",
            "no single player to shut down. Prioritize team defense and communication"))
        }
      } else if (nrow(key_players) == 1) {
        strategy_points <- c(strategy_points, paste0(
          "Limit ", key_players$Player[1], " (", key_players$player_ppg[1], " PPG) \u2014 primary offensive threat"))
      }
      if (three_rate >= 35 && fg3_pct >= 35) {
        strategy_points <- c(strategy_points, paste0(
          "DANGER from 3: They shoot ", three_rate, "% of shots from deep at ", fg3_pct,
          "% \u2014 close out hard, no open looks. Contest every three"))
      } else if (three_rate >= 35 && fg3_pct < 35) {
        strategy_points <- c(strategy_points, paste0(
          "They attempt a lot of 3s (", three_rate, "% of shots) but shoot poorly (", fg3_pct,
          "%) \u2014 let them settle for contested threes, don't over-help"))
      } else if (three_rate < 25 && fg2_pct >= 50) {
        strategy_points <- c(strategy_points, paste0(
          "Interior-focused team shooting ", fg2_pct,
          "% from 2 \u2014 pack the paint, wall up on drives, force them into uncomfortable perimeter shots"))
      } else if (three_rate < 25) {
        strategy_points <- c(strategy_points, paste0(
          "They rarely shoot 3s (", three_rate, "% of attempts) \u2014 sag off perimeter players and clog driving lanes"))
      } else {
        strategy_points <- c(strategy_points, paste0(
          "Balanced shooting attack (", three_rate, "% from 3 at ", fg3_pct, "%, ", fg2_pct,
          "% on 2s) \u2014 play honest defense, don't overcommit"))
      }
      if (opp_rpg > kings_rpg + 3) {
        strategy_points <- c(strategy_points, paste0(
          "REBOUNDING MISMATCH: They average ", opp_rpg, " RPG vs our ", kings_rpg,
          " \u2014 must crash boards harder, consider bigger lineup"))
      } else if (opp_rpg > kings_rpg) {
        strategy_points <- c(strategy_points, paste0(
          "Slight rebounding edge to them (", opp_rpg, " vs our ", kings_rpg,
          " RPG) \u2014 box out consistently, don't give up second chances"))
      } else if (kings_rpg > opp_rpg + 3) {
        strategy_points <- c(strategy_points, paste0(
          "We dominate the glass (", kings_rpg, " vs their ", opp_rpg,
          " RPG) \u2014 push tempo off defensive rebounds, attack offensive glass for second chances"))
      } else {
        strategy_points <- c(strategy_points, paste0(
          "Even on the boards (", kings_rpg, " vs ", opp_rpg, " RPG) \u2014 effort and positioning will be the difference"))
      }
      if (opp_apg > 18) {
        strategy_points <- c(strategy_points, paste0(
          "High assist team (", opp_apg, " APG) \u2014 they move the ball well. Disrupt passing lanes, pressure the ball handler"))
      } else if (opp_apg < 10) {
        strategy_points <- c(strategy_points, paste0(
          "Low assist team (", opp_apg, " APG) \u2014 isolation-heavy offense. Stay in front of your man, limit dribble penetration"))
      }
      if (ft_pct >= 75) {
        strategy_points <- c(strategy_points, paste0(
          "Strong FT shooting (", ft_pct, "%) \u2014 avoid putting them on the line in close games"))
      } else if (ft_pct < 60) {
        strategy_points <- c(strategy_points, paste0(
          "Poor FT shooting (", ft_pct, "%) \u2014 consider strategic fouling in late-game situations"))
      }
      if (nrow(hot_players) > 0) {
        hot_details <- sapply(1:nrow(hot_players), function(i)
          paste0(hot_players$Player[i], " (+", hot_players$streak_val[i], "% above avg)"))
        strategy_points <- c(strategy_points, paste0(
          "\U0001f525 HOT ALERT: ", paste(hot_details, collapse = ", "),
          " \u2014 extra attention needed, playing above season average"))
      }
      if (nrow(cold_players) > 0) {
        cold_details <- sapply(1:nrow(cold_players), function(i)
          paste0(cold_players$Player[i], " (", cold_players$streak_val[i], "% below avg)"))
        strategy_points <- c(strategy_points, paste0(
          "\u2744\ufe0f Cold stretch: ", paste(cold_details, collapse = ", "),
          " \u2014 still dangerous, don't ignore them, but may be less aggressive"))
      }
      if (kings_ppg > opp_ppg + 10) {
        strategy_points <- c(strategy_points, paste0(
          "\U0001f4aa We outscore them by ", round(kings_ppg - opp_ppg, 0),
          " PPG \u2014 push the pace, get into transition, make this a track meet"))
      } else if (opp_ppg > kings_ppg + 10) {
        strategy_points <- c(strategy_points, paste0(
          "\u26a0\ufe0f They outscore us by ", round(opp_ppg - kings_ppg, 0),
          " PPG \u2014 slow the game down, limit possessions, make every trip count"))
      }
      
      div(
        # Header
        div(style = "background: linear-gradient(135deg, #1e293b 0%, #334155 100%); color: white; padding: 30px; border-radius: 14px; margin-bottom: 20px; position: relative; overflow: hidden;",
            div(style = "position: absolute; right: -20px; top: -20px; font-size: 12em; opacity: 0.03; font-weight: 900;", "\U0001f50d"),
            div(style = "display: flex; align-items: center; gap: 20px; flex-wrap: wrap;",
                if (opp_logo != "") tags$img(src = opp_logo, style = "width: 80px; height: 80px; border-radius: 50%; border: 3px solid rgba(255,255,255,0.3); object-fit: cover; background: white;"),
                div(div("PRE-GAME SCOUTING REPORT", style = "font-size: 0.75em; font-weight: 600; opacity: 0.6; letter-spacing: 2px;"),
                    h2(opp_name, style = "margin: 5px 0 0 0; font-weight: 800; font-size: 1.8em;"),
                    div(paste0(opp_record$Division[1], " Division | ", opp_record$W[1], "-", opp_record$L[1], " (", opp_wpct, "% win rate)"), style = "opacity: 0.7; margin-top: 4px;")),
                div(style = "margin-left: auto; text-align: right;",
                    div("THREAT LEVEL", style = "font-size: 0.7em; opacity: 0.5; letter-spacing: 1px;"),
                    div(threat, style = paste0("font-size: 1.8em; font-weight: 800; color: ", threat_color, "; background: rgba(255,255,255,0.1); padding: 4px 20px; border-radius: 8px; margin-top: 4px;"))))
        ),
        
        # Quick stats
        div(style = "display: flex; gap: 12px; margin-bottom: 20px; flex-wrap: wrap;",
            lapply(list(
              list("\U0001f3c0","PPG",  opp_ppg,              paste0("#", ppg_rank, "/", ppg_total)),
              list("\U0001f4aa","RPG",  opp_rpg,              "per game"),
              list("\U0001f91d","APG",  opp_apg,              "per game"),
              list("\U0001f3af","2P%",  paste0(fg2_pct, "%"), "from inside"),
              list("\u2604\ufe0f","3P%",paste0(fg3_pct, "%"), paste0(three_rate, "% of shots")),
              list("\u2705",    "FT%",  paste0(ft_pct,  "%"), "from the line")
            ), function(item)
              div(style = "flex: 1; min-width: 120px; background: white; border: 1px solid #e2e8f0; border-radius: 10px; padding: 14px; text-align: center;",
                  div(item[[1]], style = "font-size: 1.3em;"),
                  div(item[[3]], style = "font-size: 1.5em; font-weight: 800; color: #1e293b; margin: 4px 0;"),
                  div(item[[2]], style = "font-size: 0.75em; font-weight: 700; color: #64748b;"),
                  div(item[[4]], style = "font-size: 0.65em; color: #94a3b8; margin-top: 2px;"))
            )
        ),
        
        # Two columns
        div(style = "display: flex; gap: 20px; flex-wrap: wrap;",
            
            # Left: key players + tendencies
            div(style = "flex: 1; min-width: 350px;",
                div(style = "background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px; margin-bottom: 16px;",
                    h4("\U0001f464 Key Players to Watch", style = "margin: 0 0 14px 0; color: #334155; font-weight: 700;"),
                    if (nrow(key_players) > 0) {
                      lapply(1:nrow(key_players), function(i) {
                        kp <- key_players[i,]
                        is_primary   <- i <= 2 && kp$player_ppg >= opp_ppg * 0.15
                        streak_icon  <- if (kp$streak_val > 15) "\U0001f525" else if (kp$streak_val < (-15)) "\u2744\ufe0f" else ""
                        streak_text  <- if (kp$streak_val > 15) "HOT" else if (kp$streak_val < (-15)) "COLD" else ""
                        streak_col   <- if (kp$streak_val > 15) "#b91c1c" else if (kp$streak_val < (-15)) "#3b82f6" else "#64748b"
                        player_share <- if (opp_ppg > 0) round(kp$player_ppg / opp_ppg * 100, 0) else 0
                        div(style = paste0("padding: 12px; border-radius: 10px; margin-bottom: 8px; border-left: 4px solid ",
                                           if (is_primary) "#b91c1c" else "#e2e8f0", ";",
                                           if (is_primary) " background: #fef2f2;" else " background: #f8fafc;"),
                            div(style = "display: flex; justify-content: space-between; align-items: start;",
                                div(
                                  div(style = "display: flex; align-items: center; gap: 6px; flex-wrap: wrap;",
                                      span(paste0("#", i), style = "font-weight: 800; color: #94a3b8; font-size: 0.85em;"),
                                      strong(kp$Player, style = paste0("font-size: 1em; color: ", if (is_primary) "#991b1b" else "#334155", ";")),
                                      span(paste0("(", kp$player_pos, ")"), style = "color: #64748b; font-size: 0.85em;"),
                                      if (streak_text != "")
                                        span(paste0(streak_icon, " ", streak_text),
                                             style = paste0("font-size: 0.7em; font-weight: 700; color: ", streak_col,
                                                            "; background: ", streak_col, "15; padding: 1px 6px; border-radius: 4px;"))
                                  ),
                                  div(style = "display: flex; gap: 8px; margin-top: 6px;",
                                      span(paste0(kp$player_ppg, " PPG"), style = paste0("font-size: 0.8em; font-weight: 700; color: ", ohio_kings_primary, "; background: #fee2e2; padding: 2px 6px; border-radius: 4px;")),
                                      span(paste0(kp$player_rpg, " RPG"), style = "font-size: 0.8em; font-weight: 700; color: #1e40af; background: #dbeafe; padding: 2px 6px; border-radius: 4px;"),
                                      span(paste0(kp$player_apg, " APG"), style = "font-size: 0.8em; font-weight: 700; color: #b45309; background: #fef3c7; padding: 2px 6px; border-radius: 4px;")),
                                  div(paste0(kp$gp, " games played"), style = "font-size: 0.7em; color: #94a3b8; margin-top: 4px;")
                                ),
                                if (player_share >= 15)
                                  div(style = paste0("text-align: center; background: ", if (is_primary) "#b91c1c" else "#64748b",
                                                     "; color: white; padding: 4px 10px; border-radius: 8px; font-size: 0.7em; font-weight: 700; white-space: nowrap;"),
                                      paste0(player_share, "% of\nteam PPG"))
                            )
                        )
                      })
                    } else p("No player data available", style = "color: #94a3b8; text-align: center;")
                ),
                div(style = "background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px;",
                    h4("\U0001f4ca Team Tendencies", style = "margin: 0 0 14px 0; color: #334155; font-weight: 700;"),
                    div(style = "display: flex; gap: 12px; margin-bottom: 14px;",
                        div(style = paste0("flex: 1; padding: 12px; border-radius: 8px; text-align: center; background: ",
                                           if (play_style == "Perimeter-Heavy") "#dbeafe" else if (play_style == "Interior-Focused") "#fee2e2" else "#f0fdf4", ";"),
                            div(play_style, style = "font-weight: 700; font-size: 0.95em; color: #334155;"),
                            div(paste0(three_rate, "% of shots from 3"), style = "font-size: 0.75em; color: #64748b; margin-top: 2px;")),
                        div(style = paste0("flex: 1; padding: 12px; border-radius: 8px; text-align: center; background: ",
                                           if (concentration == "Star-Dependent") "#fef2f2" else if (concentration == "Balanced Scoring") "#f0fdf4" else "#fffbeb", ";"),
                            div(concentration, style = "font-weight: 700; font-size: 0.95em; color: #334155;"),
                            div(paste0("Top 2 = ", top2_share, "% of scoring"), style = "font-size: 0.75em; color: #64748b; margin-top: 2px;"))
                    ),
                    div(div(style = "display: flex; justify-content: space-between; margin-bottom: 6px;",
                            span("Scoring Distribution", style = "font-weight: 600; color: #475569; font-size: 0.85em;"),
                            span(paste0(opp_ppg, " PPG"), style = "font-weight: 700; color: #334155; font-size: 0.85em;")),
                        div(style = "height: 28px; border-radius: 8px; overflow: hidden; display: flex;",
                            div(style = paste0("width: ", pct_2,  "%; background: ", ohio_kings_primary,   "; display: flex; align-items: center; justify-content: center;"), if (pct_2  >= 15) span(paste0("2P ", pct_2,  "%"), style = "color: white; font-size: 0.65em; font-weight: 700;")),
                            div(style = paste0("width: ", pct_3,  "%; background: ", ohio_kings_secondary,  "; display: flex; align-items: center; justify-content: center;"), if (pct_3  >= 15) span(paste0("3P ", pct_3,  "%"), style = "color: white; font-size: 0.65em; font-weight: 700;")),
                            div(style = paste0("width: ", pct_ft, "%; background: ", ohio_kings_accent,     "; display: flex; align-items: center; justify-content: center;"), if (pct_ft >= 10) span(paste0("FT ", pct_ft, "%"), style = "color: #1e293b; font-size: 0.65em; font-weight: 700;"))))
                )
            ),
            
            # Right: strategy + head-to-head
            div(style = "flex: 1; min-width: 350px;",
                div(style = "background: white; border: 2px solid #15803d; border-radius: 12px; padding: 20px; margin-bottom: 16px;",
                    h4("\u2705 How to Beat Them", style = "margin: 0 0 14px 0; color: #15803d; font-weight: 700;"),
                    lapply(seq_along(strategy_points), function(i) {
                      sp        <- strategy_points[i]
                      is_alert  <- grepl("\U0001f525 HOT", sp)
                      is_cold   <- grepl("\u2744",         sp)
                      is_danger <- grepl("DANGER|MISMATCH",sp)
                      bg_s <- if (is_alert)  " background: #fef2f2; border: 1px solid #fecaca;"
                      else if (is_cold)   " background: #eff6ff; border: 1px solid #bfdbfe;"
                      else if (is_danger) " background: #fffbeb; border: 1px solid #fde68a;"
                      else " background: #f0fdf4; border: 1px solid #bbf7d0;"
                      nb   <- if (is_alert)  "#b91c1c" else if (is_cold)   "#3b82f6" else if (is_danger) "#d97706" else "#15803d"
                      tc_s <- if (is_alert)  "#991b1b" else if (is_cold)   "#1e40af" else if (is_danger) "#92400e" else "#166634"
                      div(style = paste0("padding: 10px 12px; border-radius: 8px; margin-bottom: 8px; display: flex; align-items: start; gap: 10px;", bg_s),
                          div(style = paste0("width: 24px; height: 24px; border-radius: 50%; flex-shrink: 0; display: flex; align-items: center; justify-content: center; font-weight: 800; font-size: 0.75em; color: white; background: ", nb, ";"), i),
                          p(sp, style = paste0("margin: 0; font-size: 0.88em; font-weight: 600; color: ", tc_s, ";")))
                    })
                ),
                div(style = "background: white; border: 1px solid #e2e8f0; border-radius: 12px; padding: 20px;",
                    h4(paste0("\u2694\ufe0f ", rv$my_team, " vs ", opp_name), style = "margin: 0 0 14px 0; color: #334155; font-weight: 700;"),
                    lapply(list(
                      list("Record", paste0(kings_record$W[1], "-", kings_record$L[1]), paste0(opp_record$W[1], "-", opp_record$L[1]), kings_wpct, opp_wpct),
                      list("PPG", kings_ppg, opp_ppg, kings_ppg, opp_ppg),
                      list("RPG", kings_rpg, opp_rpg, kings_rpg, opp_rpg),
                      list("APG", kings_apg_team, opp_apg, kings_apg_team, opp_apg)
                    ), function(comp) {
                      k_num <- suppressWarnings(as.numeric(comp[[4]])); o_num <- suppressWarnings(as.numeric(comp[[5]]))
                      if (is.na(k_num)) k_num <- 50; if (is.na(o_num)) o_num <- 50
                      total <- k_num + o_num; k_pct <- if (total > 0) round(k_num / total * 100, 0) else 50; k_wins <- k_num > o_num
                      div(style = "margin-bottom: 10px;",
                          div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                              span(paste0(rv$my_team, ": ", comp[[2]]), style = paste0("font-size: 0.8em; font-weight: 700; color: ", if (k_wins) "#15803d" else "#b91c1c", ";")),
                              span(comp[[1]], style = "font-size: 0.75em; font-weight: 600; color: #94a3b8;"),
                              span(paste0(comp[[3]], " :", opp_name), style = paste0("font-size: 0.8em; font-weight: 700; color: ", if (!k_wins) "#15803d" else "#b91c1c", ";"))),
                          div(style = "height: 8px; border-radius: 4px; display: flex; overflow: hidden;",
                              div(style = paste0("width: ", k_pct, "%; background: ", ohio_kings_primary, ";")),
                              div(style = paste0("width: ", 100 - k_pct, "%; background: #94a3b8;"))))
                    })
                )
            )
        ),
        
        # Narrative
        div(style = "background: linear-gradient(135deg, #1e293b, #334155); color: white; border-radius: 12px; padding: 24px; margin-top: 20px;",
            h4("\U0001f4dd Game Preview", style = "margin: 0 0 12px 0; opacity: 0.8; letter-spacing: 0.5px;"),
            {
              narrative_parts <- c()
              narrative_parts <- c(narrative_parts, paste0(
                "The ", opp_name, " enter at ", opp_record$W[1], "-", opp_record$L[1], " (", opp_wpct, "%) from the ",
                opp_record$Division[1], " Division, averaging ", opp_ppg, " points per game (",
                if (ppg_rank <= 10) "one of the league's elite offenses"
                else if (ppg_rank <= ppg_total * 0.33) "an above-average scoring team"
                else if (ppg_rank <= ppg_total * 0.66) "a middle-of-the-pack offense"
                else "a below-average scoring team",
                ", #", ppg_rank, " of ", ppg_total, ")."))
              narrative_parts <- c(narrative_parts, paste0(
                "Their offense is ", tolower(play_style),
                if (play_style == "Perimeter-Heavy")
                  paste0(", launching ", three_rate, "% of their shots from deep",
                         if (fg3_pct >= 38) paste0(" and converting at a lethal ", fg3_pct, "%.")
                         else if (fg3_pct >= 30) paste0(" at a solid ", fg3_pct, "% clip.")
                         else paste0(" though they only connect at ", fg3_pct, "%, making them volume shooters rather than efficient ones."))
                else if (play_style == "Interior-Focused")
                  paste0(", preferring to work inside where they shoot ", fg2_pct, "% from two-point range.")
                else
                  paste0(" with ", three_rate, "% of shots from three (", fg3_pct, "%) and a ", fg2_pct, "% mark from inside.")))
              if (nrow(key_players) >= 2) {
                p1 <- key_players[1,]; p2 <- key_players[2,]
                p2_share <- if (opp_ppg > 0) round(p2$player_ppg / opp_ppg * 100, 0) else 0
                if (concentration == "Star-Dependent") {
                  p1_last <- tryCatch({ parts <- strsplit(as.character(p1$Player), " ")[[1]]; tail(parts, 1) }, error = function(e) as.character(p1$Player))
                  narrative_parts <- c(narrative_parts, paste0(
                    p1$Player, " is the engine, carrying ", top_scorer_share, "% of the scoring load at ", p1$player_ppg,
                    " PPG. When ", p1_last, " is contained, this team struggles to find alternative offense."))
                } else if (top2_share >= 45) {
                  narrative_parts <- c(narrative_parts, paste0(
                    "The scoring punch comes from ", p1$Player, " (", p1$player_ppg, " PPG, ", top_scorer_share,
                    "%) and ", p2$Player, " (", p2$player_ppg, " PPG, ", p2_share,
                    "%). Both are legitimate threats that demand attention \u2014 you can't sell out to stop just one."))
                } else {
                  narrative_parts <- c(narrative_parts, paste0(
                    p1$Player, " leads at ", p1$player_ppg, " PPG but the scoring is well-distributed across the roster. ",
                    "This team doesn't rely on one player to bail them out."))
                }
              }
              if (nrow(hot_players) > 0 || nrow(cold_players) > 0) {
                mt <- c()
                if (nrow(hot_players) > 0)
                  mt <- c(mt, paste0(paste(hot_players$Player, collapse = " and "),
                                     if (nrow(hot_players) == 1) " is" else " are", " trending up recently"))
                if (nrow(cold_players) > 0)
                  mt <- c(mt, paste0(paste(cold_players$Player, collapse = " and "),
                                     if (nrow(cold_players) == 1) " has" else " have",
                                     " been in a scoring slump, though ",
                                     if (nrow(cold_players) == 1) "this player" else "these players",
                                     " can't be ignored based on season averages"))
                narrative_parts <- c(narrative_parts, paste0("Momentum-wise, ", paste(mt, collapse = ", while "), "."))
              }
              narrative_parts <- c(narrative_parts, paste0(
                "For the ", rv$my_team, ", ",
                if (kings_ppg > opp_ppg + 15)
                  paste0("this should be a favorable matchup on paper \u2014 we outscore them by ", round(kings_ppg - opp_ppg, 0), " PPG. The key is staying focused and not playing down to the competition. ")
                else if (kings_ppg > opp_ppg)
                  paste0("we hold a scoring edge (", kings_ppg, " vs ", opp_ppg, " PPG) but can't afford complacency. ")
                else if (opp_ppg > kings_ppg + 10)
                  paste0("we'll need to overcome a significant scoring gap (", kings_ppg, " vs ", opp_ppg, " PPG). Tempo control and defense will be critical. ")
                else
                  paste0("this projects as a competitive game with similar scoring outputs (", kings_ppg, " vs ", opp_ppg, " PPG). Execution and adjustments will decide it. "),
                if (opp_rpg > kings_rpg + 3)
                  paste0("Their rebounding advantage (", opp_rpg, " vs our ", kings_rpg, " RPG) is a real concern that needs to be addressed in the game plan. ")
                else if (kings_rpg > opp_rpg + 3)
                  paste0("Our edge on the boards (", kings_rpg, " vs ", opp_rpg, " RPG) should translate to extra possessions and transition opportunities. ")
                else "",
                "Overall, this is a ", tolower(threat), "-threat opponent that ",
                if (threat == "HIGH")     "demands our full attention and best effort."
                else if (threat == "MODERATE") "we should handle with proper preparation and focus."
                else "we're expected to beat, but can't afford to take lightly."))
              p(paste(narrative_parts, collapse = " "), style = "margin: 0; line-height: 1.8; font-size: 0.95em; opacity: 0.9;")
            }
        ),
        
        # Footer
        div(style = "text-align: center; margin-top: 20px; padding: 15px; color: #94a3b8; font-size: 0.8em;",
            paste0("Generated ", format(Sys.time(), "%B %d, %Y at %I:%M %p"), " | ", rv$my_team, " Scouting Dashboard"),
            div(style = "margin-top: 8px;",
                actionButton("print_report", "\U0001f5a8\ufe0f Print Report", class = "btn-sm",
                             style = "background: #475569; color: white; border: none; padding: 8px 20px; border-radius: 8px; font-weight: 600;",
                             onclick = "window.print();"))
        )
      )
      
    }, error = function(e) {
      div(style = "background: #fee2e2; border-left: 4px solid #ef4444; padding: 20px; border-radius: 10px; margin: 20px;",
          h4("Error loading scouting report", style = "color: #991b1b; margin: 0 0 8px 0;"),
          p(paste("Details:", conditionMessage(e)), style = "color: #7f1d1d; margin: 0; font-size: 0.9em;"),
          p("Try selecting a different team or refreshing data.", style = "color: #991b1b; margin: 8px 0 0 0;"))
    })
  })
  
  # ==========================================================================
  # SCHEDULE TAB OUTPUTS
  # ==========================================================================
  
  output$calendar_view <- renderUI({
    req(rv$logged_in, rv$aba_data, input$schedule_month, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data available",
               style = "text-align:center;color:#999;padding:40px;"))
    
    kings_schedule <- rv$aba_data$team_schedules[[rv$my_team]]
    month_num      <- match(input$schedule_month, month.name)
    
    # Derive season year dynamically from the actual scraped schedule dates
    # so this works in any future season without code changes.
    # ABA seasons span Oct-Sep: months Oct-Dec belong to the start year,
    # months Jan-Sep belong to the following year.
    all_dates      <- kings_schedule$date[!is.na(kings_schedule$date)]
    season_start_year <- if (length(all_dates) > 0) {
      # Find the earliest year that has games in Oct-Dec (season start)
      oct_dec_years <- year(all_dates[month(all_dates) >= 10])
      if (length(oct_dec_years) > 0) min(oct_dec_years) else year(Sys.Date())
    } else {
      # Fallback: if no dates scraped, infer from current date
      if (month(Sys.Date()) >= 10) year(Sys.Date()) else year(Sys.Date()) - 1
    }
    year_for_month <- if (month_num >= 10) season_start_year else season_start_year + 1
    
    schedule_filtered <- kings_schedule %>%
      filter(month(date) == month_num, year(date) == year_for_month) %>%
      filter(!is.na(opponent), opponent != "") %>%
      mutate(
        difficulty = sapply(opponent, function(opp) {
          tryCatch(calculate_game_difficulty(opp, rv$aba_data), error = function(e) 5)
        }),
        difficulty = ifelse(is.na(difficulty) | is.nan(difficulty), 5, difficulty),
        diff_info  = lapply(difficulty, function(d) {
          tryCatch(get_difficulty_label(d), error = function(e)
            list(label = "\u2696\ufe0f Even", color = "#65a30d", bg = "#f7fee7"))
        })
      )
    
    if (nrow(schedule_filtered) == 0)
      return(p(paste("No games scheduled in", input$schedule_month),
               style = "text-align:center;color:#999;padding:40px;"))
    
    first_day      <- as.Date(paste0(year_for_month, "-", sprintf("%02d", month_num), "-01"))
    last_day       <- ceiling_date(first_day, "month") - days(1)
    start_weekday  <- wday(first_day, week_start = 1)
    if (is.na(start_weekday)) start_weekday <- 1
    padding_before <- max(0, start_weekday - 1)
    calendar_days  <- seq(first_day, last_day, by = "day")
    
    safe_str <- function(x, fallback = "") {
      if (is.null(x) || length(x) == 0 || is.na(x)) fallback else as.character(x)
    }
    safe_num <- function(x, fallback = 0) {
      v <- suppressWarnings(as.numeric(x))
      if (is.null(v) || length(v) == 0 || is.na(v)) fallback else v
    }
    
    div(
      style = "background:linear-gradient(180deg,#f8fafc,#ffffff);border-radius:12px;padding:20px;",
      
      div(style = "text-align:center;margin-bottom:20px;",
          h3(paste(input$schedule_month, year_for_month),
             style = "margin:0;color:#1e293b;font-weight:800;")),
      
      div(style = "display:grid;grid-template-columns:repeat(7,1fr);gap:8px;margin-bottom:8px;",
          lapply(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), function(d)
            div(d, style = "text-align:center;font-weight:700;color:#64748b;font-size:0.85em;padding:8px;"))),
      
      div(style = "display:grid;grid-template-columns:repeat(7,1fr);gap:8px;",
          
          if (padding_before > 0)
            lapply(seq_len(padding_before), function(i)
              div(style = "min-height:100px;background:#f8fafc;border-radius:8px;")),
          
          lapply(calendar_days, function(day) {
            day_num     <- day(day)
            games_today <- schedule_filtered %>% filter(date == day)
            
            if (nrow(games_today) > 0) {
              game      <- games_today[1, ]
              opp       <- safe_str(game$opponent, "Unknown")
              t_score   <- safe_num(game$team_score)
              o_score   <- safe_num(game$opp_score)
              result    <- safe_str(game$result)
              ha        <- safe_str(game$home_away, "Home")
              is_past   <- !is.na(game$date) && game$date < Sys.Date()
              diff_info <- tryCatch({
                di <- game$diff_info[[1]]
                if (is.null(di) || !is.list(di))
                  list(label="\u2696\ufe0f Even",color="#65a30d",bg="#f7fee7") else di
              }, error = function(e)
                list(label="\u2696\ufe0f Even",color="#65a30d",bg="#f7fee7"))
              
              opp_logo <- ""
              if ("logo_url" %in% names(rv$aba_data$team_lookup)) {
                lr <- rv$aba_data$team_lookup %>% filter(team_name==opp) %>% pull(logo_url)
                if (length(lr) > 0 && !is.na(lr[1])) opp_logo <- lr[1]
              }
              opp_short <- tryCatch({
                parts <- strsplit(opp," ")[[1]]
                if (length(parts)>1) tail(parts,1) else opp
              }, error=function(e) opp)
              
              div(
                style = paste0(
                  "min-height:100px;background:", safe_str(diff_info$bg,"#f7fee7"),
                  ";border:2px solid ", safe_str(diff_info$color,"#65a30d"),
                  ";border-radius:10px;padding:10px;cursor:pointer;position:relative;",
                  "transition:all 0.2s;box-shadow:0 2px 8px rgba(0,0,0,0.06);"
                ),
                onclick     = paste0("Shiny.setInputValue('selected_game_date','",
                                     as.character(day),"',{priority:'event'})"),
                onmouseover = "this.style.transform='translateY(-3px)';this.style.boxShadow='0 6px 16px rgba(0,0,0,0.12)';",
                onmouseout  = "this.style.transform='translateY(0)';this.style.boxShadow='0 2px 8px rgba(0,0,0,0.06)';",
                
                div(day_num, style="font-weight:800;font-size:1.1em;color:#1e293b;margin-bottom:6px;"),
                
                if (opp_logo != "")
                  div(style="text-align:center;margin:6px 0;",
                      tags$img(src=opp_logo,
                               style="width:32px;height:32px;border-radius:50%;border:1px solid rgba(0,0,0,0.1);")),
                
                div(style="font-size:0.7em;font-weight:600;color:#475569;text-align:center;margin:4px 0;",
                    opp_short),
                
                if (is_past && result != "") {
                  div(style="text-align:center;margin-top:6px;",
                      div(style=paste0(
                        "display:inline-block;background:",
                        if(result=="W")"#15803d" else "#b91c1c",
                        ";color:white;padding:3px 8px;border-radius:6px;font-weight:700;font-size:0.75em;"),
                        paste0(result," ",t_score,"-",o_score)))
                } else {
                  div(style="text-align:center;margin-top:6px;",
                      div(safe_str(diff_info$label,"\u2696\ufe0f Even"),
                          style=paste0("font-size:0.65em;font-weight:700;color:",
                                       safe_str(diff_info$color,"#65a30d"),";")))
                },
                
                div(style="position:absolute;top:6px;right:6px;font-size:0.6em;
                           background:rgba(0,0,0,0.05);padding:2px 6px;border-radius:4px;
                           font-weight:600;color:#64748b;",
                    if (ha=="Home") "\U0001f3e0" else "\u2708\ufe0f")
              )
            } else {
              div(style="min-height:100px;background:white;border:1px solid #e2e8f0;border-radius:8px;padding:10px;",
                  div(day_num,style="font-weight:600;font-size:0.95em;color:#cbd5e1;"))
            }
          })
      ),
      
      div(style="display:flex;justify-content:center;gap:20px;margin-top:20px;padding:15px;
                 background:white;border-radius:8px;border:1px solid #e2e8f0;flex-wrap:wrap;",
          lapply(list(
            list("\U0001f60c Routine",    "#15803d"),
            list("\u2696\ufe0f Even",     "#65a30d"),
            list("\U0001f4aa Competitive","#d97706"),
            list("\U0001f525 Battle",     "#b91c1c"),
            list("\u2694\ufe0f War",      "#7f1d1d")
          ), function(item)
            div(style="display:flex;align-items:center;gap:6px;",
                div(style=paste0("width:12px;height:12px;border-radius:3px;background:",item[[2]],";")),
                span(item[[1]],style="font-size:0.8em;font-weight:600;color:#64748b;"))),
          div(style="display:flex;align-items:center;gap:8px;margin-left:20px;padding-left:20px;border-left:2px solid #e2e8f0;",
              span("\U0001f3e0 Home",style="font-size:0.75em;color:#64748b;font-weight:600;"),
              span("\u2708\ufe0f Away",  style="font-size:0.75em;color:#64748b;font-weight:600;"))
      )
    )
  })
  
  output$schedule_insights <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    full_schedule <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      mutate(
        difficulty = sapply(opponent, function(opp) {
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        is_past = date < Sys.Date()
      )
    
    past_games   <- full_schedule %>% filter(is_past==TRUE,  !is_forfeit)
    future_games <- full_schedule %>% filter(is_past==FALSE)
    
    streak_label <- "Season Start"; streak_icon <- "\U0001f3c0"; streak_color <- "#64748b"
    if (nrow(past_games) > 0) {
      recent <- past_games %>% filter(!is.na(result)) %>% arrange(desc(date))
      if (nrow(recent) > 0) {
        streak_count <- 0; streak_type <- recent$result[1]
        for (i in 1:nrow(recent)) {
          if (recent$result[i]==streak_type) streak_count <- streak_count+1 else break
        }
        if (streak_count > 0) {
          streak_label <- paste0(streak_count, streak_type)
          streak_icon  <- if(streak_type=="W") "\U0001f525" else "\u2744\ufe0f"
          streak_color <- if(streak_type=="W") "#15803d" else "#b91c1c"
        }
      }
    }
    
    upcoming_label <- "Season End"; upcoming_color <- "#64748b"; next_3_diff <- 5
    if (nrow(future_games) >= 3) {
      next_3_diff <- mean(future_games$difficulty[1:3],na.rm=TRUE)
      if (!is.na(next_3_diff)) {
        upcoming_label <- if(next_3_diff>=7)"Brutal Stretch"
        else if(next_3_diff>=5.5)"Tough Schedule"
        else if(next_3_diff>=4)"Balanced" else "Easy Run"
        upcoming_color <- if(next_3_diff>=7)"#b91c1c"
        else if(next_3_diff>=5.5)"#d97706"
        else if(next_3_diff>=4)"#65a30d" else "#15803d"
      }
    }
    
    avg_difficulty <- round(mean(full_schedule$difficulty,na.rm=TRUE),1)
    if (is.na(avg_difficulty)) avg_difficulty <- 5
    sos_label <- if(avg_difficulty>=6.5)"Brutal" else if(avg_difficulty>=5)"Tough"
    else if(avg_difficulty>=4)"Average" else "Easy"
    
    div(
      div(style=paste0("background:linear-gradient(135deg,",streak_color,",",streak_color,
                       "cc);color:white;padding:20px;border-radius:12px;text-align:center;margin-bottom:12px;"),
          div(streak_icon,style="font-size:2em;"),
          h3(streak_label,style="margin:8px 0 0 0;font-size:2em;font-weight:800;"),
          p("Current Streak",style="margin:5px 0 0 0;opacity:0.8;font-size:0.9em;")),
      div(style=paste0("background:",upcoming_color,"15;border:2px solid ",upcoming_color,
                       ";padding:16px;border-radius:10px;margin-bottom:12px;"),
          div(style="display:flex;justify-content:space-between;align-items:center;",
              div(div("Next 3 Games",style="font-size:0.75em;color:#64748b;font-weight:600;"),
                  div(upcoming_label,
                      style=paste0("font-size:1.2em;font-weight:800;color:",upcoming_color,";margin-top:2px;"))),
              div(round(next_3_diff,1),
                  style=paste0("font-size:2em;font-weight:800;color:",upcoming_color,";"))),
          div(style="margin-top:10px;font-size:0.8em;color:#475569;",
              "Avg difficulty rating (1-10 scale)")),
      div(style="background:white;border:1px solid #e2e8f0;padding:16px;border-radius:10px;",
          div(style="display:flex;justify-content:space-between;align-items:center;",
              span("Season SOS",style="font-size:0.85em;font-weight:600;color:#64748b;"),
              div(span(sos_label,style="font-weight:700;color:#334155;margin-right:8px;"),
                  span(avg_difficulty,style=paste0("font-weight:800;font-size:1.3em;color:",ohio_kings_primary,";")))))
    )
  })
  
  output$next_games_preview <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    upcoming <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(date >= Sys.Date()) %>%
      arrange(date) %>%
      head(5) %>%
      rowwise() %>%
      mutate(
        difficulty = calculate_game_difficulty(opponent,rv$aba_data),
        diff_info  = list(get_difficulty_label(difficulty))
      ) %>%
      ungroup()
    
    if (nrow(upcoming) == 0)
      return(div(style="text-align:center;padding:40px;",
                 div("\U0001f3c1",style="font-size:3em;opacity:0.3;"),
                 p("Season Complete",style="color:#94a3b8;margin-top:10px;")))
    
    div(lapply(1:nrow(upcoming), function(i) {
      game      <- upcoming[i,]
      diff_info <- game$diff_info[[1]]
      win_result <- tryCatch(
        calculate_unified_win_probability(NULL, game$opponent, rv$aba_data,
                                          include_schedule=TRUE, my_team_name=rv$my_team),
        error=function(e) list(prob=50,has_full_data=FALSE))
      win_prob <- if (is.list(win_result)) win_result$prob else 50
      
      div(style=paste0("background:white;border-left:4px solid ",diff_info$color,
                       ";padding:12px;margin-bottom:10px;border-radius:8px;",
                       "cursor:pointer;transition:all 0.2s;"),
          onclick     = paste0("Shiny.setInputValue('selected_game_date','",
                               as.character(game$date),"',{priority:'event'})"),
          onmouseover = "this.style.background='#f8fafc';",
          onmouseout  = "this.style.background='white';",
          div(style="display:flex;justify-content:space-between;align-items:center;",
              div(div(format(game$date,"%b %d"),style="font-size:0.75em;color:#64748b;font-weight:600;"),
                  div(paste0("vs ",game$opponent),
                      style="font-weight:700;color:#1e293b;font-size:0.95em;margin-top:2px;"),
                  div(style="display:flex;gap:8px;margin-top:4px;",
                      span(diff_info$label,
                           style=paste0("font-size:0.65em;font-weight:700;color:",diff_info$color,";")),
                      span(if(game$home_away=="Home")"\U0001f3e0 Home" else "\u2708\ufe0f Away",
                           style="font-size:0.65em;color:#94a3b8;font-weight:600;"))),
              div(style="text-align:right;",
                  div(paste0(win_prob,"%"),
                      style=paste0("font-size:1.5em;font-weight:800;color:",
                                   if(win_prob>=60)"#15803d" else if(win_prob>=40)"#d97706" else "#b91c1c",";")),
                  div("Win Prob",style="font-size:0.65em;color:#94a3b8;font-weight:600;")))
      )
    }))
  })
  
  output$schedule_matchup_breakdown <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    full_schedule <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      mutate(
        difficulty = sapply(opponent,function(opp){
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        is_past = date < Sys.Date()
      )
    
    categorize_diff <- function(d) {
      if(d>=8.5)"War" else if(d>=7)"Battle"
      else if(d>=5.5)"Competitive" else if(d>=4)"Even" else "Routine"
    }
    cat_color <- function(cat) switch(cat,"War"="#7f1d1d","Battle"="#b91c1c",
                                      "Competitive"="#d97706","Even"="#65a30d",
                                      "Routine"="#15803d","#64748b")
    cat_icon  <- function(cat) switch(cat,"War"="\u2694\ufe0f","Battle"="\U0001f525",
                                      "Competitive"="\U0001f4aa","Even"="\u2696\ufe0f",
                                      "Routine"="\U0001f60c","\U0001f3c0")
    
    past   <- full_schedule %>% filter(is_past==TRUE,  !is_forfeit)
    future <- full_schedule %>% filter(is_past==FALSE)
    
    past_breakdown <- if (nrow(past)>0) {
      past %>%
        mutate(category=sapply(difficulty,categorize_diff)) %>%
        group_by(category) %>%
        summarize(games=n(), wins=sum(result=="W",na.rm=TRUE),
                  avg_diff=round(mean(difficulty,na.rm=TRUE),1), .groups="drop") %>%
        arrange(desc(avg_diff))
    } else data.frame()
    
    future_breakdown <- if (nrow(future)>0) {
      future %>% mutate(category=sapply(difficulty,categorize_diff)) %>%
        count(category,name="games")
    } else data.frame()
    
    div(
      h4("By Opponent Strength",style="margin:0 0 16px 0;color:#334155;font-weight:700;"),
      if (nrow(past_breakdown)>0) tagList(
        div("Past Games Performance",
            style="font-size:0.75em;color:#64748b;font-weight:600;margin-bottom:10px;"),
        lapply(1:nrow(past_breakdown),function(i) {
          cat <- past_breakdown[i,]
          cc  <- cat_color(cat$category); ci <- cat_icon(cat$category)
          div(style="margin-bottom:10px;",
              div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:4px;",
                  span(paste(ci,cat$category),style="font-size:0.85em;font-weight:600;color:#475569;"),
                  div(span(paste0(cat$games," games"),style="font-weight:600;color:#64748b;margin-right:6px;font-size:0.8em;"),
                      span(paste0(cat$wins,"-",cat$games-cat$wins),
                           style=paste0("font-weight:700;color:",cc,";")))),
              div(style=paste0("height:24px;background:",cc,"15;border-radius:6px;",
                               "padding:4px 10px;margin-top:4px;"),
                  span(paste0("Avg Difficulty: ",cat$avg_diff,"/10"),
                       style=paste0("font-size:0.75em;font-weight:700;color:",cc,";"))))
        })
      ),
      if (nrow(future_breakdown)>0) tagList(
        div("Remaining Schedule",
            style="font-size:0.75em;color:#64748b;font-weight:600;margin:20px 0 10px;"),
        div(style="display:flex;gap:8px;flex-wrap:wrap;",
            lapply(1:nrow(future_breakdown),function(i) {
              cat <- future_breakdown[i,]; cc <- cat_color(cat$category)
              div(style=paste0("background:",cc,"10;border:1px solid ",cc,
                               ";padding:8px 12px;border-radius:8px;"),
                  span(paste0(cat$games," ",cat$category),
                       style=paste0("font-size:0.8em;font-weight:700;color:",cc,";")))
            }))
      )
    )
  })
  
  output$difficulty_timeline <- renderPlotly({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(plotly_empty())
    
    timeline_data <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      arrange(date) %>%
      mutate(
        difficulty  = sapply(opponent,function(opp){
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        game_num    = row_number(),
        is_past     = date < Sys.Date(),
        point_color = case_when(
          is_past & result=="W" ~ "#22c55e",
          is_past & result=="L" ~ "#ef4444",
          TRUE                  ~ "#94a3b8"
        ),
        hover_text = paste0(
          "Game ",game_num,": ",opponent,"\n",
          format(date,"%b %d")," | ",
          if_else(home_away=="Home","\U0001f3e0 Home","\u2708\ufe0f Away"),"\n",
          if_else(is_past,paste0(result," ",team_score,"-",opp_score),"Upcoming"),"\n",
          "Difficulty: ",round(difficulty,1),"/10"
        )
      )
    
    plot_ly(timeline_data, x=~game_num, y=~difficulty) %>%
      add_trace(type="scatter",mode="lines+markers",
                line   = list(color=ohio_kings_primary,width=2),
                marker = list(size=10,color=~point_color,line=list(color="white",width=2)),
                text=~hover_text,hoverinfo="text",name="Difficulty") %>%
      add_trace(y=7,type="scatter",mode="lines",
                line=list(color="#b91c1c",width=1,dash="dash"),
                name="Tough Threshold",hoverinfo="skip",showlegend=FALSE) %>%
      add_trace(y=4,type="scatter",mode="lines",
                line=list(color="#65a30d",width=1,dash="dash"),
                name="Easy Threshold",hoverinfo="skip",showlegend=FALSE) %>%
      layout(xaxis=list(title="Game Number"),
             yaxis=list(title="Difficulty (1-10)",range=c(0,10)),
             hovermode="closest",showlegend=FALSE,margin=list(t=20,b=40))
  })
  
  output$division_games_tracker <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    ohio_kings_accent  <- rv$team_cols$accent
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    kings_div <- rv$aba_data$league_data %>% filter(Team==rv$my_team) %>% pull(Division)
    if (length(kings_div)==0) return(p("Division info not available",style="color:#999;"))
    
    div_teams <- rv$aba_data$league_data %>%
      filter(Division==kings_div[1], Team!=rv$my_team) %>% pull(Team)
    
    full_schedule <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(!is.na(date)) %>%
      mutate(is_division=opponent %in% div_teams, is_past=date<Sys.Date())
    
    div_past   <- full_schedule %>% filter(is_division, is_past,  !is_forfeit)
    div_future <- full_schedule %>% filter(is_division, !is_past)
    
    div_wins   <- if(nrow(div_past)>0) sum(div_past$result=="W",na.rm=TRUE) else 0
    div_losses <- if(nrow(div_past)>0) sum(div_past$result=="L",na.rm=TRUE) else 0
    div_wpct   <- if((div_wins+div_losses)>0) round(div_wins/(div_wins+div_losses)*100,0) else 0
    
    div(
      div(style=paste0("background:linear-gradient(135deg,",ohio_kings_accent,",#e6a800);",
                       "color:#451a03;padding:20px;border-radius:12px;",
                       "text-align:center;margin-bottom:16px;"),
          div("\U0001f451",style="font-size:2em;"),
          h3(paste0(div_wins,"-",div_losses),
             style="margin:8px 0 0 0;font-size:2.5em;font-weight:800;"),
          p(paste0("Division Record (",div_wpct,"%)"),
            style="margin:5px 0 0 0;opacity:0.9;font-size:0.9em;font-weight:600;")),
      if (nrow(div_future)>0) tagList(
        div(paste0(nrow(div_future)," Division Games Remaining"),
            style="font-size:0.8em;font-weight:700;color:#64748b;margin-bottom:10px;text-transform:uppercase;"),
        lapply(1:min(5,nrow(div_future)),function(i) {
          game      <- div_future[i,]
          diff      <- tryCatch(calculate_game_difficulty(game$opponent,rv$aba_data),error=function(e) 5)
          diff_info <- get_difficulty_label(diff)
          div(style=paste0("background:white;border-left:3px solid ",diff_info$color,
                           ";padding:10px;margin-bottom:8px;border-radius:6px;"),
              div(style="display:flex;justify-content:space-between;align-items:center;",
                  div(div(format(game$date,"%b %d"),style="font-size:0.7em;color:#94a3b8;"),
                      div(game$opponent,style="font-weight:700;color:#1e293b;font-size:0.9em;margin-top:2px;")),
                  span(diff_info$label,
                       style=paste0("font-size:0.7em;font-weight:700;color:",diff_info$color,";"))))
        })
      ) else p("No upcoming division games",
               style="text-align:center;color:#94a3b8;padding:20px;font-style:italic;")
    )
  })
  
  output$key_games_watch <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary <- rv$team_cols$primary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    upcoming <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(date >= Sys.Date()) %>% arrange(date) %>%
      mutate(difficulty=sapply(opponent,function(opp){
        tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
      }))
    
    if (nrow(upcoming)==0)
      return(p("Season complete",style="text-align:center;color:#94a3b8;padding:20px;"))
    
    kings_div <- rv$aba_data$league_data %>% filter(Team==rv$my_team) %>% pull(Division)
    div_teams <- if(length(kings_div)>0) {
      rv$aba_data$league_data %>% filter(Division==kings_div[1],Team!=rv$my_team) %>% pull(Team)
    } else c()
    
    key_games <- upcoming %>%
      head(10) %>%
      mutate(
        importance=case_when(
          opponent %in% div_teams & difficulty>=7 ~ "\U0001f525 MUST-WIN Division Battle",
          difficulty>=8.5                          ~ "\u2694\ufe0f Season-Defining Game",
          opponent %in% div_teams                  ~ "\U0001f451 Division Game",
          difficulty>=7                            ~ "\U0001f4aa Tough Test",
          difficulty<=3.5                          ~ "\u2705 Winnable Game",
          TRUE                                     ~ "\U0001f4ca Regular Game"
        ),
        imp_rank=case_when(
          opponent %in% div_teams & difficulty>=7 ~ 1,
          difficulty>=8.5                          ~ 2,
          opponent %in% div_teams                  ~ 3,
          difficulty>=7                            ~ 4,
          TRUE                                     ~ 5
        )
      ) %>%
      arrange(imp_rank,desc(difficulty)) %>%
      head(5)
    
    div(lapply(1:nrow(key_games),function(i) {
      game      <- key_games[i,]
      diff_info <- get_difficulty_label(game$difficulty)
      bc <- switch(as.character(game$imp_rank),
                   "1"="#b91c1c","2"="#7f1d1d","3"="#f59e0b","4"="#d97706","#64748b")
      div(style=paste0("background:white;border:2px solid ",bc,
                       ";padding:14px;margin-bottom:10px;border-radius:10px;",
                       "cursor:pointer;transition:all 0.2s;"),
          onclick     = paste0("Shiny.setInputValue('selected_game_date','",
                               as.character(game$date),"',{priority:'event'})"),
          onmouseover = "this.style.transform='translateY(-2px)';this.style.boxShadow='0 4px 12px rgba(0,0,0,0.1)';",
          onmouseout  = "this.style.transform='translateY(0)';this.style.boxShadow='none';",
          div(game$importance,
              style=paste0("font-size:0.75em;font-weight:700;color:",bc,";margin-bottom:6px;letter-spacing:0.3px;")),
          div(style="display:flex;justify-content:space-between;align-items:center;",
              div(div(format(game$date,"%A, %B %d"),
                      style="font-size:0.75em;color:#64748b;font-weight:600;"),
                  div(paste0("vs ",game$opponent),
                      style="font-weight:700;color:#1e293b;font-size:1em;margin-top:2px;"),
                  div(style="margin-top:4px;",
                      span(diff_info$label,
                           style=paste0("font-size:0.7em;font-weight:600;color:",diff_info$color,";margin-right:8px;")),
                      span(if(game$home_away=="Home")"\U0001f3e0 Home" else "\u2708\ufe0f Away",
                           style="font-size:0.7em;color:#94a3b8;"))),
              div(style="text-align:right;",
                  div(round(game$difficulty,1),
                      style=paste0("font-size:2em;font-weight:800;color:",ohio_kings_primary,";")),
                  div("/10",style="font-size:0.7em;color:#94a3b8;")))
      )
    }))
  })
  
  output$schedule_win_loss_prediction <- renderUI({
    req(rv$logged_in, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    
    if (!rv$my_team %in% names(rv$aba_data$team_schedules))
      return(p("No schedule data",style="color:#999;"))
    
    upcoming <- rv$aba_data$team_schedules[[rv$my_team]] %>%
      filter(date >= Sys.Date()) %>% arrange(date) %>%
      mutate(
        difficulty = sapply(opponent,function(opp){
          tryCatch(calculate_game_difficulty(opp,rv$aba_data),error=function(e) 5)
        }),
        win_prob = sapply(opponent,function(opp){
          tryCatch({
            res <- calculate_unified_win_probability(NULL,opp,rv$aba_data,
                                                     include_schedule=TRUE,
                                                     my_team_name=rv$my_team)
            if(is.list(res)) res$prob else as.numeric(res)
          },error=function(e) 50)
        })
      )
    
    if (nrow(upcoming)==0)
      return(p("Season complete - no games remaining",
               style="text-align:center;color:#94a3b8;padding:20px;"))
    
    expected_wins  <- round(sum(upcoming$win_prob)/100,1)
    k_rec          <- rv$aba_data$league_data %>% filter(Team==rv$my_team)
    current_wins   <- if(nrow(k_rec)>0) k_rec$W[1] else 0
    current_losses <- if(nrow(k_rec)>0) k_rec$L[1] else 0
    current_total  <- current_wins + current_losses
    proj_wins      <- current_wins + round(expected_wins)
    proj_losses    <- current_total + nrow(upcoming) - proj_wins
    proj_wpct      <- round(proj_wins/(proj_wins+proj_losses)*100,0)
    
    div(
      div(style="display:flex;gap:20px;margin-bottom:20px;flex-wrap:wrap;",
          div(style="flex:1;min-width:200px;background:linear-gradient(135deg,#475569,#64748b);
                     color:white;padding:20px;border-radius:12px;text-align:center;",
              div("CURRENT",style="font-size:0.7em;opacity:0.6;letter-spacing:1px;margin-bottom:8px;"),
              h2(paste0(current_wins,"-",current_losses),
                 style="margin:0;font-size:2.5em;font-weight:800;"),
              p(paste0(if(current_total>0) round(current_wins/current_total*100,0) else 0,"% win rate"),
                style="margin:8px 0 0 0;opacity:0.8;font-size:0.85em;")),
          div(style="flex:1;min-width:200px;background:linear-gradient(135deg,#3b82f6,#2563eb);
                     color:white;padding:20px;border-radius:12px;text-align:center;",
              div("EXPECTED WINS",style="font-size:0.7em;opacity:0.6;letter-spacing:1px;margin-bottom:8px;"),
              h2(expected_wins,style="margin:0;font-size:2.5em;font-weight:800;"),
              p(paste0("From next ",nrow(upcoming)," games"),
                style="margin:8px 0 0 0;opacity:0.8;font-size:0.85em;")),
          div(style=paste0("flex:1;min-width:200px;background:linear-gradient(135deg,",
                           ohio_kings_primary,",",ohio_kings_secondary,");",
                           "color:white;padding:20px;border-radius:12px;text-align:center;"),
              div("PROJECTED FINAL",style="font-size:0.7em;opacity:0.6;letter-spacing:1px;margin-bottom:8px;"),
              h2(paste0(proj_wins,"-",proj_losses),style="margin:0;font-size:2.5em;font-weight:800;"),
              p(paste0(proj_wpct,"% win rate"),
                style="margin:8px 0 0 0;opacity:0.8;font-size:0.85em;font-weight:700;"))
      ),
      div(style="background:#f8fafc;border-radius:10px;padding:16px;",
          h4("Remaining Games",style="margin:0 0 12px 0;color:#334155;font-weight:700;"),
          div(style="max-height:300px;overflow-y:auto;",
              lapply(1:nrow(upcoming),function(i) {
                game <- upcoming[i,]
                div(style="background:white;border:1px solid #e2e8f0;padding:10px;margin-bottom:8px;border-radius:6px;",
                    div(style="display:flex;justify-content:space-between;align-items:center;",
                        div(div(format(game$date,"%b %d"),style="font-size:0.7em;color:#94a3b8;font-weight:600;"),
                            div(game$opponent,style="font-weight:700;color:#1e293b;font-size:0.85em;margin-top:2px;")),
                        div(style="text-align:right;",
                            div(paste0(game$win_prob,"%"),
                                style=paste0("font-size:1.2em;font-weight:800;color:",
                                             if(game$win_prob>=60)"#15803d" else if(game$win_prob>=40)"#d97706" else "#b91c1c",";")),
                            div("win prob",style="font-size:0.6em;color:#94a3b8;"))))
              })))
    )
  })
  
  observeEvent(input$selected_game_date, {
    req(rv$aba_data, rv$my_team)
    game_date <- as.Date(input$selected_game_date)
    if (!rv$my_team %in% names(rv$aba_data$team_schedules)) return()
    game_info <- rv$aba_data$team_schedules[[rv$my_team]] %>% filter(date == game_date)
    if (nrow(game_info) == 0) return()
    game <- game_info[1,]
    updateSelectInput(session, "scout_team",      selected = game$opponent)
    updateSelectInput(session, "scout_game_select",selected = as.character(game_date))
    updateTabItems(session, "tabs", "scouting")
  })
  
  # ==========================================================================
  # SCOUTING REPORT TAB
  # ==========================================================================
  
  output$scouting_report <- renderUI({
    req(rv$logged_in, input$scout_team, rv$aba_data, rv$my_team)
    ohio_kings_primary   <- rv$team_cols$primary
    ohio_kings_secondary <- rv$team_cols$secondary
    ohio_kings_accent    <- rv$team_cols$accent
    
    if (input$scout_team == "")
      return(div(style="text-align:center;padding:60px 20px;",
                 div("\U0001f50d",style="font-size:4em;opacity:0.3;"),
                 h3("Select an opponent to generate scouting report",
                    style="color:#94a3b8;margin-top:15px;"),
                 p("Choose a team from the dropdown above",style="color:#cbd5e1;")))
    
    opp_name   <- input$scout_team
    opp_roster <- rv$aba_data$all_team_stats[[opp_name]]
    opp_record <- rv$aba_data$league_data %>% filter(Team == opp_name)
    if (is.null(opp_roster)||nrow(opp_record)==0)
      return(p("No data available for this team",
               style="text-align:center;color:#999;padding:40px;"))
    
    opp_gp   <- opp_record$W[1] + opp_record$L[1]
    opp_wpct <- if(opp_gp>0) round(opp_record$W[1]/opp_gp*100,0) else 50
    
    get_stat <- function(roster,col) {
      tr <- roster %>% filter(str_detect(toupper(Player),"TOTAL|TEAM"))
      if(nrow(tr)>0&&col%in%names(tr)) return(as.numeric(tr[[col]][1]))
      pr <- roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
      if(col%in%names(pr)) return(sum(as.numeric(pr[[col]]),na.rm=TRUE))
      return(0)
    }
    
    opp_pts <- get_stat(opp_roster,"PTS"); opp_reb <- get_stat(opp_roster,"REB")
    opp_ast <- get_stat(opp_roster,"AST")
    opp_2pm <- get_stat(opp_roster,"2PM"); opp_2pa <- get_stat(opp_roster,"2PA")
    opp_3pm <- get_stat(opp_roster,"3PM"); opp_3pa <- get_stat(opp_roster,"3PA")
    opp_ftm <- get_stat(opp_roster,"FTM"); opp_fta <- get_stat(opp_roster,"FTA")
    
    opp_ppg <- get_accurate_team_ppg(opp_name,rv$aba_data)
    if(opp_ppg==0) opp_ppg <- round(opp_pts/opp_gp,1)
    opp_rpg <- round(opp_reb/opp_gp,1)
    opp_apg <- round(opp_ast/opp_gp,1)
    
    fg2_pct    <- if(opp_2pa>0) round(opp_2pm/opp_2pa*100,1) else 0
    fg3_pct    <- if(opp_3pa>0) round(opp_3pm/opp_3pa*100,1) else 0
    ft_pct     <- if(opp_fta>0) round(opp_ftm/opp_fta*100,1) else 0
    three_rate <- if((opp_2pa+opp_3pa)>0) round(opp_3pa/(opp_2pa+opp_3pa)*100,0) else 0
    play_style <- if(three_rate>=40)"Perimeter-Heavy" else if(three_rate>=25)"Balanced" else "Interior-Focused"
    
    pts_from_2 <- opp_2pm*2; pts_from_3 <- opp_3pm*3; pts_from_ft <- opp_ftm
    total_scored <- pts_from_2+pts_from_3+pts_from_ft
    if(total_scored<opp_pts&&total_scored>0){
      diff <- opp_pts-total_scored
      pts_from_2 <- pts_from_2+round(diff*pts_from_2/total_scored)
      pts_from_3 <- pts_from_3+round(diff*pts_from_3/total_scored)
      pts_from_ft <- opp_pts-pts_from_2-pts_from_3
    }
    pct_2  <- if(opp_pts>0) round(pts_from_2/opp_pts*100,0) else 0
    pct_3  <- if(opp_pts>0) round(pts_from_3/opp_pts*100,0) else 0
    pct_ft <- if(opp_pts>0) round(pts_from_ft/opp_pts*100,0) else 0
    
    player_roster <- opp_roster %>% filter(!str_detect(toupper(Player),"TOTAL|TEAM"))
    
    key_players <- player_roster %>%
      mutate(
        player_ppg = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pg<-rv$aba_data$all_player_stats[[pl]]$per_game
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$PPG[1])) return(pg$PPG[1])
          }; return(0)
        }),
        player_rpg = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pg<-rv$aba_data$all_player_stats[[pl]]$per_game
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$RPG[1])) return(pg$RPG[1])
          }; return(0)
        }),
        player_apg = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pg<-rv$aba_data$all_player_stats[[pl]]$per_game
            if(!is.null(pg)&&nrow(pg)>0&&!is.na(pg$APG[1])) return(pg$APG[1])
          }; return(0)
        }),
        player_pos = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pos<-rv$aba_data$all_player_stats[[pl]]$position
            if(!is.null(pos)&&!is.na(pos)) return(pos)
          }; return("G")
        }),
        gp = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            gl<-rv$aba_data$all_player_stats[[pl]]$game_log
            if(!is.null(gl)) return(nrow(gl))
          }; return(0)
        }),
        streak = sapply(Player,function(pl){
          if(pl%in%names(rv$aba_data$all_player_stats)){
            pd<-rv$aba_data$all_player_stats[[pl]]
            if(!is.null(pd$game_log)&&nrow(pd$game_log)>=3&&!is.null(pd$per_game)){
              rec<-pd$game_log%>%arrange(desc(date))%>%head(3)
              rec_avg<-mean(suppressWarnings(as.numeric(rec$PTS)),na.rm=TRUE)
              szn_avg<-pd$per_game$PPG[1]
              if(!is.na(rec_avg)&&!is.na(szn_avg)&&szn_avg>0)
                return(round(((rec_avg-szn_avg)/szn_avg)*100,1))
            }
          }; return(0)
        })
      ) %>%
      filter(gp>=3,player_ppg>0) %>%
      arrange(desc(player_ppg)) %>%
      head(5)
    
    top_scorer_share <- if(opp_ppg>0&&nrow(key_players)>0)
      round(key_players$player_ppg[1]/opp_ppg*100,0) else 0
    top2_share <- if(opp_ppg>0&&nrow(key_players)>=2)
      round(sum(key_players$player_ppg[1:2])/opp_ppg*100,0) else top_scorer_share
    concentration <- if(top_scorer_share>=30)"Star-Dependent"
    else if(top2_share>=50)"Top-Heavy" else "Balanced Scoring"
    
    hot_players  <- key_players %>% filter(streak>15)
    cold_players <- key_players %>% filter(streak<-15)
    
    threat       <- if(opp_wpct>=70)"HIGH" else if(opp_wpct>=50)"MODERATE" else "LOW"
    threat_color <- if(threat=="HIGH")"#b91c1c" else if(threat=="MODERATE")"#d97706" else "#15803d"
    
    kings_record <- rv$aba_data$league_data %>% filter(Team==rv$my_team)
    kings_ppg    <- get_accurate_team_ppg(rv$my_team,rv$aba_data)
    kings_rpg    <- get_accurate_team_rpg(rv$my_team,rv$aba_data)
    kings_apg    <- get_accurate_team_apg(rv$my_team,rv$aba_data)
    kings_gp     <- if(nrow(kings_record)>0) kings_record$W[1]+kings_record$L[1] else 0
    kings_wpct   <- if(kings_gp>0) round(kings_record$W[1]/kings_gp*100,0) else 50
    
    opp_logo <- ""
    if("logo_url"%in%names(rv$aba_data$team_lookup)){
      lr<-rv$aba_data$team_lookup%>%filter(team_name==opp_name)%>%pull(logo_url)
      if(length(lr)>0&&!is.na(lr[1])) opp_logo<-lr[1]
    }
    
    # Strategy generation
    strategy_points <- c()
    if(nrow(key_players)>=1){
      significant <- key_players %>% filter(player_ppg>=opp_ppg*0.15)
      n_threats   <- nrow(significant)
      if(n_threats==1){
        strategy_points<-c(strategy_points,paste0(
          "Contain ",significant$Player[1]," (",significant$player_ppg[1]," PPG, ",
          top_scorer_share,"% of scoring) — team has one dominant option. ",
          "Double aggressively and force role players to create."))
      } else if(n_threats==2){
        p2_share<-if(opp_ppg>0) round(significant$player_ppg[2]/opp_ppg*100,0) else 0
        strategy_points<-c(strategy_points,paste0(
          "Two-headed attack: ",significant$Player[1]," (",significant$player_ppg[1]," PPG) and ",
          significant$Player[2]," (",significant$player_ppg[2]," PPG, ",p2_share,
          "%) combine for ",top2_share,"% of scoring — can't focus on just one."))
      } else if(n_threats>=3){
        strategy_points<-c(strategy_points,paste0(
          "Deep scoring team with ",n_threats," players averaging 15%+ of team scoring — ",
          "prioritize team defense and communication over individual assignments."))
      }
    }
    if(three_rate>=35&&fg3_pct>=35){
      strategy_points<-c(strategy_points,paste0(
        "DANGER from 3: They shoot ",three_rate,"% of shots from deep at ",fg3_pct,
        "% — close out hard, no open looks. Contest every three-point attempt."))
    } else if(three_rate>=35&&fg3_pct<35){
      strategy_points<-c(strategy_points,paste0(
        "They attempt a lot of 3s (",three_rate,"% of shots) but shoot poorly (",fg3_pct,
        "%) — let them settle for contested threes, don't over-help."))
    } else if(three_rate<25&&fg2_pct>=50){
      strategy_points<-c(strategy_points,paste0(
        "Interior-focused team shooting ",fg2_pct,
        "% from 2 — pack the paint, wall up on drives, force perimeter shots."))
    } else {
      strategy_points<-c(strategy_points,paste0(
        "Balanced attack: ",three_rate,"% from 3 (",fg3_pct,"%), ",fg2_pct,
        "% on 2s — play honest defense, don't overcommit either way."))
    }
    if(opp_rpg>kings_rpg+3){
      strategy_points<-c(strategy_points,paste0(
        "REBOUNDING MISMATCH: They average ",opp_rpg," RPG vs our ",kings_rpg,
        " — must crash boards harder, consider a bigger lineup."))
    } else if(opp_rpg>kings_rpg){
      strategy_points<-c(strategy_points,paste0(
        "Slight rebounding edge to them (",opp_rpg," vs our ",kings_rpg,
        " RPG) — box out consistently, don't give up second chances."))
    } else if(kings_rpg>opp_rpg+3){
      strategy_points<-c(strategy_points,paste0(
        "We dominate the glass (",kings_rpg," vs their ",opp_rpg,
        " RPG) — push tempo off defensive boards, attack offensive glass."))
    }
    if(opp_apg>18){
      strategy_points<-c(strategy_points,paste0(
        "High assist team (",opp_apg," APG) — they move the ball well. ",
        "Disrupt passing lanes, pressure the ball handler."))
    } else if(opp_apg<10){
      strategy_points<-c(strategy_points,paste0(
        "Low assist team (",opp_apg," APG) — isolation-heavy offense. ",
        "Stay in front of your man, limit dribble penetration."))
    }
    if(ft_pct>=75){
      strategy_points<-c(strategy_points,paste0(
        "Strong FT shooting (",ft_pct,"%) — avoid putting them on the line in close games."))
    } else if(ft_pct<60){
      strategy_points<-c(strategy_points,paste0(
        "Poor FT shooting (",ft_pct,"%) — consider strategic fouling in late-game situations."))
    }
    if(nrow(hot_players)>0){
      hot_details<-sapply(1:nrow(hot_players),function(i)
        paste0(hot_players$Player[i]," (+",hot_players$streak[i],"% above avg)"))
      strategy_points<-c(strategy_points,paste0(
        "\U0001f525 HOT ALERT: ",paste(hot_details,collapse=", "),
        " — extra attention needed, playing well above season average."))
    }
    if(nrow(cold_players)>0){
      cold_details<-sapply(1:nrow(cold_players),function(i)
        paste0(cold_players$Player[i]," (",cold_players$streak[i],"% below avg)"))
      strategy_points<-c(strategy_points,paste0(
        "\u2744\ufe0f Cold stretch: ",paste(cold_details,collapse=", "),
        " — still dangerous based on season averages, but may be less aggressive."))
    }
    if(kings_ppg>opp_ppg+10){
      strategy_points<-c(strategy_points,paste0(
        "\U0001f4aa We outscore them by ",round(kings_ppg-opp_ppg,0),
        " PPG — push the pace, get into transition, make this a track meet."))
    } else if(opp_ppg>kings_ppg+10){
      strategy_points<-c(strategy_points,paste0(
        "\u26a0\ufe0f They outscore us by ",round(opp_ppg-kings_ppg,0),
        " PPG — slow the game down, limit possessions, make every trip count."))
    }
    
    # Auto-generated narrative
    narrative_parts <- c()
    narrative_parts<-c(narrative_parts,paste0(
      "The ",opp_name," enter at ",opp_record$W[1],"-",opp_record$L[1],
      " (",opp_wpct,"%) from the ",opp_record$Division[1]," Division, ",
      "averaging ",opp_ppg," points per game."))
    narrative_parts<-c(narrative_parts,paste0(
      "Their offense is ",tolower(play_style),
      if(play_style=="Perimeter-Heavy") paste0(", launching ",three_rate,
                                               "% of their shots from deep",
                                               if(fg3_pct>=38) paste0(" and converting at a lethal ",fg3_pct,"%.") else paste0(" at a ",fg3_pct,"% clip."))
      else if(play_style=="Interior-Focused") paste0(", preferring to work inside where they shoot ",fg2_pct,"% from two.")
      else paste0(" with ",three_rate,"% from 3 (",fg3_pct,"%) and ",fg2_pct,"% from inside.")))
    if(nrow(key_players)>=1){
      p1<-key_players[1,]
      if(concentration=="Star-Dependent"){
        narrative_parts<-c(narrative_parts,paste0(
          p1$Player," carries ",top_scorer_share,"% of the scoring at ",p1$player_ppg,
          " PPG. When contained, this team struggles to find offense elsewhere."))
      } else if(top2_share>=45&&nrow(key_players)>=2){
        p2<-key_players[2,]
        p2_share<-if(opp_ppg>0) round(p2$player_ppg/opp_ppg*100,0) else 0
        narrative_parts<-c(narrative_parts,paste0(
          "The scoring punch comes from ",p1$Player," (",p1$player_ppg," PPG) and ",
          p2$Player," (",p2$player_ppg," PPG, ",p2_share,"%) — both are legitimate threats."))
      } else {
        narrative_parts<-c(narrative_parts,paste0(
          p1$Player," leads at ",p1$player_ppg," PPG but scoring is well-distributed. ",
          "No single player to shut down."))
      }
    }
    narrative_parts<-c(narrative_parts,paste0(
      "For ",rv$my_team,", ",
      if(kings_ppg>opp_ppg+15) paste0("this is a favorable matchup — we outscore them by ",round(kings_ppg-opp_ppg,0)," PPG. Stay focused and don't play down to competition. ")
      else if(kings_ppg>opp_ppg) paste0("we hold a scoring edge (",kings_ppg," vs ",opp_ppg," PPG) but can't afford complacency. ")
      else if(opp_ppg>kings_ppg+10) paste0("we'll need to overcome a scoring gap (",kings_ppg," vs ",opp_ppg," PPG). Tempo control and defense are critical. ")
      else paste0("this projects as a competitive game (",kings_ppg," vs ",opp_ppg," PPG). Execution and adjustments will decide it. "),
      "Overall, this is a ",tolower(threat),"-threat opponent that ",
      if(threat=="HIGH") "demands our full attention and best effort."
      else if(threat=="MODERATE") "we should handle with proper preparation and focus."
      else "we're expected to beat, but can't take lightly."))
    
    div(
      # Report Header
      div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                       "padding:30px;border-radius:14px;margin-bottom:20px;position:relative;overflow:hidden;"),
          div(style="position:absolute;right:-20px;top:-20px;font-size:12em;opacity:0.03;font-weight:900;","\U0001f50d"),
          div(style="display:flex;align-items:center;gap:20px;flex-wrap:wrap;",
              if(opp_logo!="")
                tags$img(src=opp_logo,
                         style="width:80px;height:80px;border-radius:50%;border:3px solid rgba(255,255,255,0.3);object-fit:cover;background:white;"),
              div(div("PRE-GAME SCOUTING REPORT",
                      style="font-size:0.75em;font-weight:600;opacity:0.6;letter-spacing:2px;"),
                  h2(opp_name,style="margin:5px 0 0 0;font-weight:800;font-size:1.8em;"),
                  div(paste0(opp_record$Division[1]," Division | ",
                             opp_record$W[1],"-",opp_record$L[1]," (",opp_wpct,"% win rate)"),
                      style="opacity:0.7;margin-top:4px;")),
              div(style="margin-left:auto;text-align:right;",
                  div("THREAT LEVEL",style="font-size:0.7em;opacity:0.5;letter-spacing:1px;"),
                  div(threat,
                      style=paste0("font-size:1.8em;font-weight:800;color:",threat_color,
                                   ";background:rgba(255,255,255,0.1);padding:4px 20px;",
                                   "border-radius:8px;margin-top:4px;")))
          )
      ),
      
      # Quick stats row
      div(style="display:flex;gap:12px;margin-bottom:20px;flex-wrap:wrap;",
          lapply(list(
            list("\U0001f3c0","PPG",  opp_ppg,             "per game"),
            list("\U0001f4aa","RPG",  opp_rpg,             "per game"),
            list("\U0001f91d","APG",  opp_apg,             "per game"),
            list("\U0001f3af","2P%",  paste0(fg2_pct,"%"),  "from inside"),
            list("\u2604\ufe0f","3P%",paste0(fg3_pct,"%"),  paste0(three_rate,"% of shots")),
            list("\u2705",    "FT%",  paste0(ft_pct,"%"),   "from the line")
          ),function(item)
            div(style="flex:1;min-width:120px;background:white;border:1px solid #e2e8f0;border-radius:10px;padding:14px;text-align:center;",
                div(item[[1]],style="font-size:1.3em;"),
                div(item[[3]],style="font-size:1.5em;font-weight:800;color:#1e293b;margin:4px 0;"),
                div(item[[2]],style="font-size:0.75em;font-weight:700;color:#64748b;"),
                div(item[[4]],style="font-size:0.65em;color:#94a3b8;margin-top:2px;"))
          )
      ),
      
      # Two column layout
      div(style="display:flex;gap:20px;flex-wrap:wrap;",
          
          # Left: key players + tendencies
          div(style="flex:1;min-width:350px;",
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;margin-bottom:16px;",
                  h4("\U0001f464 Key Players to Watch",
                     style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  if(nrow(key_players)>0) lapply(1:nrow(key_players),function(i){
                    kp<-key_players[i,]
                    is_primary<-i<=2&&kp$player_ppg>=opp_ppg*0.15
                    streak_icon<-if(kp$streak>15)"\U0001f525" else if(kp$streak<-15)"\u2744\ufe0f" else ""
                    streak_text<-if(kp$streak>15)"HOT" else if(kp$streak<-15)"COLD" else ""
                    player_share<-if(opp_ppg>0) round(kp$player_ppg/opp_ppg*100,0) else 0
                    bg_c<-if(is_primary)"#fef2f2" else "#f8fafc"
                    bd_c<-if(is_primary)"#b91c1c" else "#e2e8f0"
                    tc_c<-if(is_primary)"#991b1b" else "#334155"
                    div(style=paste0("padding:12px;border-radius:10px;margin-bottom:8px;",
                                     "border-left:4px solid ",bd_c,";background:",bg_c,";"),
                        div(style="display:flex;justify-content:space-between;align-items:start;",
                            div(div(style="display:flex;align-items:center;gap:6px;flex-wrap:wrap;",
                                    span(paste0("#",i),style="font-weight:800;color:#94a3b8;font-size:0.85em;"),
                                    strong(kp$Player,style=paste0("font-size:1em;color:",tc_c,";")),
                                    span(paste0("(",kp$player_pos,")"),style="color:#64748b;font-size:0.85em;"),
                                    if(streak_text!="") span(paste0(streak_icon," ",streak_text),
                                                             style=paste0("font-size:0.7em;font-weight:700;color:",
                                                                          if(streak_text=="HOT")"#b91c1c" else "#3b82f6",
                                                                          ";background:",
                                                                          if(streak_text=="HOT")"#fef2f2" else "#eff6ff",
                                                                          ";padding:1px 6px;border-radius:4px;"))),
                                div(style="display:flex;gap:8px;margin-top:6px;",
                                    span(paste0(kp$player_ppg," PPG"),
                                         style=paste0("font-size:0.8em;font-weight:700;color:",ohio_kings_primary,";background:#fee2e2;padding:2px 6px;border-radius:4px;")),
                                    span(paste0(kp$player_rpg," RPG"),style="font-size:0.8em;font-weight:700;color:#1e40af;background:#dbeafe;padding:2px 6px;border-radius:4px;"),
                                    span(paste0(kp$player_apg," APG"),style="font-size:0.8em;font-weight:700;color:#b45309;background:#fef3c7;padding:2px 6px;border-radius:4px;")),
                                div(paste0(kp$gp," games played"),style="font-size:0.7em;color:#94a3b8;margin-top:4px;")),
                            if(player_share>=15)
                              div(style=paste0("text-align:center;background:#b91c1c;",
                                               "color:white;padding:4px 10px;border-radius:8px;",
                                               "font-size:0.7em;font-weight:700;white-space:nowrap;"),
                                  paste0(player_share,"% of\nteam PPG"))
                        )
                    )
                  }) else p("No player data available",style="color:#94a3b8;text-align:center;")
              ),
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                  h4("\U0001f4ca Team Tendencies",style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  div(style="display:flex;gap:12px;margin-bottom:14px;",
                      div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                       if(play_style=="Perimeter-Heavy")"#dbeafe"
                                       else if(play_style=="Interior-Focused")"#fee2e2" else "#f0fdf4",";"),
                          div(play_style,style="font-weight:700;font-size:0.95em;color:#334155;"),
                          div(paste0(three_rate,"% of shots from 3"),style="font-size:0.75em;color:#64748b;margin-top:2px;")),
                      div(style=paste0("flex:1;padding:12px;border-radius:8px;text-align:center;background:",
                                       if(concentration=="Star-Dependent")"#fef2f2"
                                       else if(concentration=="Balanced Scoring")"#f0fdf4" else "#fffbeb",";"),
                          div(concentration,style="font-weight:700;font-size:0.95em;color:#334155;"),
                          div(paste0("Top 2 = ",top2_share,"% of scoring"),style="font-size:0.75em;color:#64748b;margin-top:2px;"))
                  ),
                  div(div(style="display:flex;justify-content:space-between;margin-bottom:6px;",
                          span("Scoring Distribution",style="font-weight:600;color:#475569;font-size:0.85em;"),
                          span(paste0(opp_ppg," PPG"),style="font-weight:700;color:#334155;font-size:0.85em;")),
                      div(style="height:28px;border-radius:8px;overflow:hidden;display:flex;",
                          div(style=paste0("width:",pct_2,"%;background:",ohio_kings_primary,
                                           ";display:flex;align-items:center;justify-content:center;"),
                              if(pct_2>=15) span(paste0("2P ",pct_2,"%"),style="color:white;font-size:0.65em;font-weight:700;")),
                          div(style=paste0("width:",pct_3,"%;background:",ohio_kings_secondary,
                                           ";display:flex;align-items:center;justify-content:center;"),
                              if(pct_3>=15) span(paste0("3P ",pct_3,"%"),style="color:white;font-size:0.65em;font-weight:700;")),
                          div(style=paste0("width:",pct_ft,"%;background:",ohio_kings_accent,
                                           ";display:flex;align-items:center;justify-content:center;"),
                              if(pct_ft>=10) span(paste0("FT ",pct_ft,"%"),style="color:#1e293b;font-size:0.65em;font-weight:700;")))
                  )
              )
          ),
          
          # Right: strategy + matchup comparison
          div(style="flex:1;min-width:350px;",
              div(style="background:white;border:2px solid #15803d;border-radius:12px;padding:20px;margin-bottom:16px;",
                  h4("\u2705 How to Beat Them",style="margin:0 0 14px 0;color:#15803d;font-weight:700;"),
                  lapply(seq_along(strategy_points),function(i){
                    sp       <- strategy_points[i]
                    is_alert <- grepl("\U0001f525 HOT",sp)
                    is_cold  <- grepl("\u2744",sp)
                    is_danger<- grepl("DANGER|MISMATCH|outscore",sp)
                    bg_s <- if(is_alert)"#fef2f2;border:1px solid #fecaca;"
                    else if(is_cold)"#eff6ff;border:1px solid #bfdbfe;"
                    else if(is_danger)"#fffbeb;border:1px solid #fde68a;"
                    else "#f0fdf4;border:1px solid #bbf7d0;"
                    nb   <- if(is_alert)"#b91c1c" else if(is_cold)"#3b82f6"
                    else if(is_danger)"#d97706" else "#15803d"
                    tc_s <- if(is_alert)"#991b1b" else if(is_cold)"#1e40af"
                    else if(is_danger)"#92400e" else "#166534"
                    div(style=paste0("padding:10px 12px;border-radius:8px;margin-bottom:8px;",
                                     "display:flex;align-items:start;gap:10px;background:",bg_s),
                        div(style=paste0("width:24px;height:24px;border-radius:50%;flex-shrink:0;",
                                         "display:flex;align-items:center;justify-content:center;",
                                         "font-weight:800;font-size:0.75em;color:white;background:",nb,";"),i),
                        p(sp,style=paste0("margin:0;font-size:0.88em;font-weight:600;color:",tc_s,";")))
                  })
              ),
              div(style="background:white;border:1px solid #e2e8f0;border-radius:12px;padding:20px;",
                  h4(paste0("\u2694\ufe0f ",rv$my_team," vs ",opp_name),
                     style="margin:0 0 14px 0;color:#334155;font-weight:700;"),
                  lapply(list(
                    list("Record",
                         paste0(if(nrow(kings_record)>0) paste0(kings_record$W[1],"-",kings_record$L[1]) else "N/A"),
                         paste0(opp_record$W[1],"-",opp_record$L[1]),
                         kings_wpct, opp_wpct),
                    list("PPG",    kings_ppg, opp_ppg, kings_ppg, opp_ppg),
                    list("RPG",    kings_rpg, opp_rpg, kings_rpg, opp_rpg),
                    list("APG",    kings_apg, opp_apg, kings_apg, opp_apg)
                  ),function(comp){
                    k_num <- as.numeric(comp[[4]]); o_num <- as.numeric(comp[[5]])
                    total <- k_num+o_num
                    k_pct <- if(total>0) round(k_num/total*100,0) else 50
                    k_wins_comp <- k_num>o_num
                    div(style="margin-bottom:10px;",
                        div(style="display:flex;justify-content:space-between;margin-bottom:4px;",
                            span(paste0(rv$my_team,": ",comp[[2]]),
                                 style=paste0("font-size:0.8em;font-weight:700;color:",
                                              if(k_wins_comp)"#15803d" else "#b91c1c",";")),
                            span(comp[[1]],style="font-size:0.75em;font-weight:600;color:#94a3b8;"),
                            span(paste0(comp[[3]]," :",opp_name),
                                 style=paste0("font-size:0.8em;font-weight:700;color:",
                                              if(!k_wins_comp)"#15803d" else "#b91c1c",";"))),
                        div(style="height:8px;border-radius:4px;display:flex;overflow:hidden;",
                            div(style=paste0("width:",k_pct,"%;background:",ohio_kings_primary,";")),
                            div(style=paste0("width:",100-k_pct,"%;background:#94a3b8;"))))
                  })
              )
          )
      ),
      
      # Auto-generated narrative
      div(style=paste0("background:linear-gradient(135deg,#1e293b,#334155);color:white;",
                       "border-radius:12px;padding:24px;margin-top:20px;"),
          h4("\U0001f4dd Game Preview",style="margin:0 0 12px 0;opacity:0.8;letter-spacing:0.5px;"),
          p(paste(narrative_parts,collapse=" "),
            style="margin:0;line-height:1.8;font-size:0.95em;opacity:0.9;")
      ),
      
      # Footer
      div(style="text-align:center;margin-top:20px;padding:15px;color:#94a3b8;font-size:0.8em;",
          paste0("Generated ",format(Sys.time(),"%B %d, %Y at %I:%M %p"),
                 " | ",rv$my_team," Scouting Dashboard"),
          div(style="margin-top:8px;",
              actionButton("print_report","\U0001f5a8\ufe0f Print Report",
                           class="btn-sm",
                           style="background:#475569;color:white;border:none;padding:8px 20px;border-radius:8px;font-weight:600;",
                           onclick="window.print();"))
      )
    )
  })
  
} # end server

# ============================================================================
# LAUNCH
# ============================================================================
shinyApp(ui = ui, server = server)




rsconnect::deployApp(
  appDir = "C:/Users/lynch/OneDrive/Documents",
  appFiles = c(
    "ABA Dashboard.R",
    "aba_data_cache.rds"
  ),
  appPrimaryDoc = "ABA Dashboard.R",
  appName = "aba-dashboard",
  appTitle = "ABA Dashboard",
  forceUpdate = TRUE
)