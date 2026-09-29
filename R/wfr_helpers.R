# =============================================================================
#  wfr_helpers.R
#  欧州サッカー分析ヘルパー（Understat + FBref / worldfootballR）
#
#  データソース
#    - Understat            : https://understat.com/
#    - FBref                 : https://fbref.com/
#    - worldfootballR (vignette)
#        https://jaseziv.github.io/worldfootballR/articles/extract-understat-data.html
#        https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html
#        https://jaseziv.github.io/worldfootballR/articles/load-scraped-data.html
#
#  app/app.R / reports/*.Rmd から source() して使います。
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(ggplot2)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


# -----------------------------------------------------------------------------
# 1. 定数：リーグ定義とシーズン
# -----------------------------------------------------------------------------

#' Understat と FBref のリーグ対応表
LEAGUES <- tibble::tribble(
  ~understat,     ~label_ja,                 ~fb_country, ~fb_comp,
  "EPL",          "プレミアリーグ (ENG)",     "ENG",       "Premier League",
  "La liga",      "ラ・リーガ (ESP)",         "ESP",       "La Liga",
  "Bundesliga",   "ブンデスリーガ (GER)",     "GER",       "Fußball-Bundesliga",
  "Serie A",      "セリエA (ITA)",            "ITA",       "Serie A",
  "Ligue 1",      "リーグ・アン (FRA)",       "FRA",       "Ligue 1",
  "RFPL",         "ロシア・プレミア (RUS)",   NA,          NA
)

#' Understat でデータが存在する最初のシーズン（開始年）
FIRST_SEASON <- 2014L

#' 現在のシーズン開始年（8月以降なら今年、それ以前は前年）
current_season_start_year <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  if (m >= 8L) y else y - 1L
}

#' 2024 -> "2024/25"
season_label <- function(start_year) {
  start_year <- as.integer(start_year)
  sprintf("%d/%02d", start_year, (start_year + 1L) %% 100L)
}

#' リーグ名 -> 日本語ラベル
league_label <- function(understat_name) {
  idx <- match(understat_name, LEAGUES$understat)
  ifelse(is.na(idx), understat_name, LEAGUES$label_ja[idx])
}

#' 選択肢用：日本語ラベル付きのリーグ名ベクトル
league_choices <- function(exclude_rfpl = FALSE) {
  d <- LEAGUES
  if (exclude_rfpl) d <- dplyr::filter(d, !is.na(fb_country))
  stats::setNames(d$understat, d$label_ja)
}


# -----------------------------------------------------------------------------
# 2. キャッシュ（毎回スクレイピングしないための仕組み）
# -----------------------------------------------------------------------------

#' プロジェクトのルート（R/wfr_helpers.R を含むフォルダ）を上位へたどって探す。
#' app/ や reports/ など、どこから実行しても同じ場所を指す。
wfr_root <- function(start = getwd()) {
  d <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "R", "wfr_helpers.R"))) return(d)
    p <- dirname(d)
    if (identical(p, d)) return(normalizePath(getwd(), winslash = "/"))
    d <- p
  }
}

wfr_cache_dir <- function() {
  dir <- getOption("wfr.cache_dir", file.path(wfr_root(), "data", "cache"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

#' キャッシュ付きでデータ取得を実行する
#'
#' @param key      キャッシュのキー（ファイル名になる）
#' @param expr     データ取得式（遅延評価。キャッシュヒット時は実行されない）
#' @param ttl_hours キャッシュの有効時間
cached <- function(key, expr, ttl_hours = 24) {
  f <- file.path(wfr_cache_dir(),
                 paste0(gsub("[^A-Za-z0-9._-]", "_", key), ".rds"))

  if (file.exists(f)) {
    age <- as.numeric(difftime(Sys.time(), file.info(f)$mtime, units = "hours"))
    if (is.finite(age) && age <= ttl_hours) {
      val <- try(readRDS(f), silent = TRUE)
      if (!inherits(val, "try-error")) return(val)
    }
  }

  val <- tryCatch(expr, error = function(e) {
    message("[取得失敗] ", key, " : ", conditionMessage(e))
    NULL
  })
  if (!is.null(val)) try(saveRDS(val, f), silent = TRUE)
  val
}

wfr_cache_info <- function() {
  files <- list.files(wfr_cache_dir(), pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) {
    return(tibble(ファイル = character(), サイズMB = numeric(), 更新日時 = as.POSIXct(character())))
  }
  info <- file.info(files)
  tibble(
    ファイル = basename(files),
    サイズMB = round(info$size / 1024^2, 2),
    更新日時 = info$mtime
  ) %>% arrange(desc(更新日時))
}

wfr_clear_cache <- function(pattern = NULL) {
  files <- list.files(wfr_cache_dir(), pattern = "\\.rds$", full.names = TRUE)
  if (!is.null(pattern)) files <- files[grepl(pattern, basename(files))]
  n <- length(files)
  if (n) file.remove(files)
  invisible(n)
}


# -----------------------------------------------------------------------------
# 3. 列名ゆらぎ対策（worldfootballR のバージョン差を吸収する）
# -----------------------------------------------------------------------------

#' 正規表現パターンに最初にマッチした列名を返す
pick_col <- function(df, patterns, required = TRUE) {
  nm <- names(df)
  for (p in patterns) {
    hit <- nm[stringr::str_detect(nm, p)]
    if (length(hit)) return(hit[1])
  }
  if (required) {
    stop("必要な列が見つかりません（候補: ", paste(patterns, collapse = ", "),
         "）。実際の列名: ", paste(nm, collapse = ", "), call. = FALSE)
  }
  NA_character_
}

#' 列があればその値、なければ default で埋めたベクトルを返す
col_or <- function(df, patterns, default = NA) {
  nm <- pick_col(df, patterns, required = FALSE)
  if (is.na(nm)) rep(default, nrow(df)) else df[[nm]]
}


# -----------------------------------------------------------------------------
# 4. Understat：直接取得
#     Understat はページ埋め込みJSONを廃止し、getLeagueData / getTeamData /
#     getMatchData から読み込む方式に変わったため、worldfootballR の
#     understat_* スクレイパーは動きません。ここで直接取得します。
# -----------------------------------------------------------------------------

#' Understat の JSON エンドポイントを取得
#' @param path 例 "getLeagueData/La_liga/2024"
understat_json <- function(path, referer = "https://understat.com/") {
  res <- httr::GET(
    paste0("https://understat.com/", path),
    httr::add_headers(`X-Requested-With` = "XMLHttpRequest", Referer = referer),
    httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"),
    httr::timeout(60)
  )
  httr::stop_for_status(res, task = paste("Understat", path))
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, flatten = TRUE)
}

understat_slug <- function(x) gsub(" ", "_", trimws(as.character(x)))

#' リーグ・シーズンの試合一覧（worldfootballR::understat_league_match_results と同じ列名）
understat_league_matches <- function(league, season_start_year) {
  js <- understat_json(sprintf("getLeagueData/%s/%d", understat_slug(league),
                               as.integer(season_start_year)))
  d <- as_tibble(js$dates)
  if (!nrow(d)) return(tibble())
  d %>%
    transmute(
      league        = league,
      match_id      = as.character(id),
      isResult      = as.logical(isResult),
      home_id       = as.character(h.id),
      home_team     = h.title,
      home_abbr     = h.short_title,
      away_id       = as.character(a.id),
      away_team     = a.title,
      away_abbr     = a.short_title,
      home_goals    = suppressWarnings(as.numeric(goals.h)),
      away_goals    = suppressWarnings(as.numeric(goals.a)),
      home_xG       = suppressWarnings(as.numeric(xG.h)),
      away_xG       = suppressWarnings(as.numeric(xG.a)),
      datetime      = datetime,
      forecast_win  = suppressWarnings(as.numeric(forecast.w)),
      forecast_draw = suppressWarnings(as.numeric(forecast.d)),
      forecast_loss = suppressWarnings(as.numeric(forecast.l))
    )
}

#' 1試合分のシュート（load_understat_league_shots と同じ列名・すべて文字列）
understat_match_shots <- function(match_id, league) {
  js <- understat_json(sprintf("getMatchData/%s", match_id),
                       referer = sprintf("https://understat.com/match/%s", match_id))
  sh <- dplyr::bind_rows(js$shots$h, js$shots$a)
  if (is.null(sh) || !nrow(sh)) return(tibble())
  as_tibble(sh) %>%
    mutate(across(everything(), as.character)) %>%
    transmute(league = league, id, minute, result, X, Y, xG, player, h_a,
              player_id, situation, season, shotType, match_id,
              home_team = h_team, away_team = a_team,
              home_goals = h_goals, away_goals = a_goals,
              date, player_assisted, lastAction, home_away = h_a)
}


# -----------------------------------------------------------------------------
# 4b. Understat：試合結果
# -----------------------------------------------------------------------------

#' 試合結果を取得（複数シーズン対応・キャッシュ付き）
us_results <- function(league, seasons) {
  cur <- current_season_start_year()
  purrr::map_dfr(seasons, function(s) {
    ttl <- if (s >= cur) 6 else 24 * 60      # 進行中シーズンは短め
    dat <- cached(
      sprintf("understat_matches_%s_%d", understat_slug(league), s),
      understat_league_matches(league, s),
      ttl_hours = ttl
    )
    if (is.null(dat) || !nrow(dat)) return(NULL)
    dplyr::as_tibble(dat) %>%
      dplyr::mutate(league_understat = league,
                    season_start_year = as.integer(s))
  })
}

#' 試合結果をチーム×試合のロング形式に変換
#' （元のRmdの pivot_longer 処理を、対戦相手・累積ポイント・xPts付きに拡張）
tidy_results <- function(res) {
  if (is.null(res) || !nrow(res)) return(tibble())

  res <- as_tibble(res)
  if (!"isResult" %in% names(res)) res$isResult <- TRUE

  res %>%
    mutate(isResult = as.logical(isResult)) %>%
    filter(isResult, !is.na(home_goals), !is.na(away_goals)) %>%
    mutate(
      datetime  = suppressWarnings(as.POSIXct(datetime, tz = "UTC")),
      date      = as.Date(datetime),
      home_name = home_team,
      away_name = away_team
    ) %>%
    pivot_longer(c(home_team, away_team),
                 names_to = "venue", values_to = "team") %>%
    mutate(
      venue    = if_else(venue == "home_team", "H", "A"),
      is_home  = venue == "H",
      opponent = if_else(is_home, away_name, home_name),
      G        = if_else(is_home, home_goals, away_goals),
      GA       = if_else(is_home, away_goals, home_goals),
      xG       = if_else(is_home, home_xG,   away_xG),
      xGA      = if_else(is_home, away_xG,   home_xG),
      result   = case_when(G > GA ~ "W", G == GA ~ "D", TRUE ~ "L"),
      point    = case_when(result == "W" ~ 3L, result == "D" ~ 1L, TRUE ~ 0L),
      # Understat の勝敗確率から算出した期待勝点
      xPoint   = if_else(is_home,
                         3 * forecast_win  + forecast_draw,
                         3 * forecast_loss + forecast_draw)
    ) %>%
    select(-home_name, -away_name) %>%
    group_by(league_understat, season_start_year, team) %>%
    arrange(datetime, .by_group = TRUE) %>%
    mutate(match_no    = row_number(),
           cum_points  = cumsum(point),
           cum_xPoints = cumsum(xPoint)) %>%
    ungroup()
}

#' 順位表を作る
standings <- function(tm) {
  if (!nrow(tm)) return(tibble())
  tm %>%
    group_by(league_understat, season_start_year, team) %>%
    summarise(
      MP   = n(),
      W    = sum(result == "W"),
      D    = sum(result == "D"),
      L    = sum(result == "L"),
      Pts  = sum(point),
      G    = sum(G),
      GA   = sum(GA),
      GD   = G - GA,
      xG   = round(sum(xG), 1),
      xGA  = round(sum(xGA), 1),
      xGD  = round(xG - xGA, 1),
      xPts = round(sum(xPoint), 1),
      `G-xG`   = round(G - xG, 1),      # 決定力（プラスなら期待以上）
      `GA-xGA` = round(GA - xGA, 1),    # 守備・GK（マイナスなら期待以上）
      `Pts-xPts` = round(Pts - xPts, 1),
      .groups = "drop"
    ) %>%
    arrange(league_understat, season_start_year, desc(Pts), desc(GD)) %>%
    group_by(league_understat, season_start_year) %>%
    mutate(順位 = row_number()) %>%
    ungroup() %>%
    relocate(順位)
}


# -----------------------------------------------------------------------------
# 5. Understat：シュートデータ（選手分析のベース）
# -----------------------------------------------------------------------------

normalise_league_name <- function(x) gsub("_", " ", as.character(x))

#' load_understat_league_shots() の出力を整形（列名ゆらぎも吸収）
normalise_shots <- function(sh, league_fallback = NA_character_) {
  if (is.null(sh) || !nrow(sh)) return(tibble())
  sh <- as_tibble(sh)

  ha <- if ("h_a" %in% names(sh)) sh$h_a else NA_character_
  if (all(is.na(ha)) && "home_away" %in% names(sh)) ha <- sh$home_away
  if ("h_a" %in% names(sh) && "home_away" %in% names(sh)) {
    ha <- dplyr::coalesce(as.character(sh$h_a), as.character(sh$home_away))
  }

  lg <- if ("league" %in% names(sh)) normalise_league_name(sh$league) else league_fallback

  sh %>%
    mutate(
      h_a               = ha,
      league_understat  = lg,
      season_start_year = suppressWarnings(as.integer(season)),
      minute            = suppressWarnings(as.numeric(minute)),
      xG                = suppressWarnings(as.numeric(xG)),
      X                 = suppressWarnings(as.numeric(X)),
      Y                 = suppressWarnings(as.numeric(Y)),
      team              = if_else(h_a == "h", home_team, away_team),
      opponent          = if_else(h_a == "h", away_team, home_team),
      is_goal           = result == "Goal",
      is_penalty        = situation == "Penalty",
      date              = as.Date(substr(as.character(date), 1, 10))
    ) %>%
    filter(!is.na(player))
}

#' 事前取得データに無い試合のシュートを Understat から直接取得して補う
#' （worldfootballR の事前取得データは更新が止まることがあるため）
fill_missing_shots <- function(dat, league, seasons) {
  have <- if (is.null(dat) || !nrow(dat)) character() else as.character(dat$match_id)
  cur  <- current_season_start_year()

  extra <- purrr::map_dfr(seasons, function(s) {
    m <- cached(sprintf("understat_matches_%s_%d", understat_slug(league), s),
                understat_league_matches(league, s),
                ttl_hours = if (s >= cur) 6 else 24 * 60)
    if (is.null(m) || !nrow(m)) return(NULL)
    ids <- setdiff(m$match_id[m$isResult], have)
    if (!length(ids)) return(NULL)
    message(sprintf("[Understat] %s %s: %d 試合のシュートを取得します",
                    league, season_label(s), length(ids)))
    purrr::map_dfr(ids, function(id) {
      key <- sprintf("understat_matchshots_%s", id)
      hit <- file.exists(file.path(wfr_cache_dir(), paste0(key, ".rds")))
      out <- cached(key, understat_match_shots(id, league), ttl_hours = 24 * 365)
      if (!hit) Sys.sleep(0.3)   # サーバーに負荷をかけすぎない
      out
    })
  })

  if (!nrow(extra)) return(dat)
  dplyr::bind_rows(
    if (!is.null(dat) && nrow(dat)) mutate(as_tibble(dat), across(everything(), as.character)),
    extra
  )
}

#' リーグ単位でシュートデータを取得（全シーズン一括ロード）
#'
#' @param seasons          指定するとそのシーズンに絞る
#' @param complete_seasons 事前取得データに欠けている試合を Understat から補うシーズン
us_shots <- function(leagues, seasons = NULL, complete_seasons = NULL) {
  out <- purrr::map_dfr(leagues, function(lg) {
    dat <- cached(
      sprintf("understat_shots_%s", gsub(" ", "_", lg)),
      worldfootballR::load_understat_league_shots(league = lg),
      ttl_hours = 12
    )
    if (length(complete_seasons)) dat <- fill_missing_shots(dat, lg, complete_seasons)
    normalise_shots(dat, league_fallback = lg)
  })
  if (!is.null(seasons) && nrow(out)) out <- filter(out, season_start_year %in% seasons)
  out
}

#' シュートデータから選手成績を集計
#' （得点・xG に加えて、アシスト側（player_assisted）からKP・xA・アシストも算出）
player_shot_stats <- function(sh, by = c("player")) {
  if (!nrow(sh)) return(tibble())

  shooting <- sh %>%
    group_by(across(all_of(by))) %>%
    summarise(
      shots   = n(),
      goals   = sum(is_goal),
      xG      = sum(xG),
      npshots = sum(!is_penalty),
      npgoals = sum(is_goal & !is_penalty),
      npxG    = sum(xG[!is_penalty]),
      .groups = "drop"
    )

  assisting <- sh %>%
    filter(!is.na(player_assisted), player_assisted != "") %>%
    mutate(player = player_assisted) %>%
    group_by(across(all_of(by))) %>%
    summarise(
      key_passes = n(),
      xA         = sum(xG),
      assists    = sum(is_goal),
      .groups    = "drop"
    )

  shooting %>%
    full_join(assisting, by = by) %>%
    mutate(across(where(is.numeric), ~ tidyr::replace_na(.x, 0))) %>%
    mutate(
      決定率     = if_else(shots > 0, goals / shots, NA_real_),
      `G-xG`     = goals - xG,
      `npG-npxG` = npgoals - npxG,
      `G+A`      = goals + assists,
      `xG+xA`    = xG + xA
    ) %>%
    arrange(desc(goals))
}


# -----------------------------------------------------------------------------
# 6. Understat：チーム別 選手スタッツ（出場時間を含む）
# -----------------------------------------------------------------------------

#' チーム名から Understat のチームURLを組み立てる
understat_team_urls <- function(teams, season_start_year) {
  sprintf("https://understat.com/team/%s/%d",
          gsub(" ", "_", trimws(teams)), as.integer(season_start_year))
}

#' チーム単位の選手スタッツ（出場時間 time 付き）
#' 元Rmdの「出場時間」セクションはシュートの分(minute)を合計していて誤りだったため、
#' 正しい出場時間はこちらの time 列を使います。
us_team_players <- function(teams, season_start_year) {
  season_start_year <- as.integer(season_start_year)
  dat <- purrr::map_dfr(unique(teams), function(tm) {
    cached(
      sprintf("understat_teamdata_%s_%d", understat_slug(tm), season_start_year),
      {
        js <- understat_json(sprintf("getTeamData/%s/%d", understat_slug(tm), season_start_year),
                             referer = understat_team_urls(tm, season_start_year))
        as_tibble(js$players) %>%
          mutate(across(everything(), as.character), season = season_start_year)
      },
      ttl_hours = 24
    )
  })
  if (is.null(dat) || !nrow(dat)) return(tibble())
  as_tibble(dat) %>%
    mutate(across(any_of(c("games", "time", "goals", "assists", "shots",
                           "key_passes", "xG", "xA")),
                  ~ suppressWarnings(as.numeric(.x))),
           `分/得点`       = if_else(goals > 0, time / goals, NA_real_),
           `90分あたり得点` = if_else(time > 0, goals / (time / 90), NA_real_),
           `90分あたりxG`   = if_else(time > 0, xG   / (time / 90), NA_real_))
}

#' 依存パッケージを増やさないための簡易ハッシュ（キャッシュのキー用）
digest_chr <- function(x) {
  v <- as.numeric(utf8ToInt(paste(x, collapse = "")))
  paste0("h", format(sum(v * seq_along(v)) %% 1e9, scientific = FALSE))
}


# -----------------------------------------------------------------------------
# 7. FBref：オフサイドデータ
#     Understat にはオフサイドの指標が無いため、FBref の misc(その他) を使います。
#
#     Team_or_Opponent == "team"     -> そのチームの選手がオフサイドを取られた数
#                                       ＝「オフサイドにかかる数」
#     Team_or_Opponent == "opponent" -> 相手チームがオフサイドを取られた数
#                                       ＝「オフサイドにかける数」（オフサイドトラップ）
# -----------------------------------------------------------------------------

#' FBref ビッグ5リーグの misc スタッツ（事前スクレイプ済みデータをロード）
fb_misc <- function(team_or_player = c("team", "player")) {
  top <- match.arg(team_or_player)
  dat <- cached(
    sprintf("fbref_big5_misc_%s", top),
    worldfootballR::load_fb_big5_advanced_season_stats(
      stat_type = "misc", team_or_player = top
    ),
    ttl_hours = 12
  )
  if (is.null(dat)) return(NULL)
  as_tibble(dat)
}

#' FBref の国コード／大会名 -> Understat のリーグ名
fb_to_understat_league <- function(country, competition) {
  n   <- max(length(country), length(competition))
  cc  <- toupper(as.character(rep(country,     length.out = n)))
  comp<- tolower(as.character(rep(competition, length.out = n)))
  out <- rep(NA_character_, n)

  out[!is.na(cc) & cc == "ENG"] <- "EPL"
  out[!is.na(cc) & cc == "ESP"] <- "La liga"
  out[!is.na(cc) & cc == "GER"] <- "Bundesliga"
  out[!is.na(cc) & cc == "ITA"] <- "Serie A"
  out[!is.na(cc) & cc == "FRA"] <- "Ligue 1"

  idx <- is.na(out) & !is.na(comp)
  out[idx & grepl("bundesliga", comp)] <- "Bundesliga"
  idx <- is.na(out) & !is.na(comp)
  out[idx & grepl("premier",    comp)] <- "EPL"
  out[idx & grepl("la liga",    comp)] <- "La liga"
  out[idx & grepl("serie a",    comp)] <- "Serie A"
  out[idx & grepl("ligue 1",    comp)] <- "Ligue 1"
  out
}

#' チーム別オフサイド表（かける／かかる）を作る
offside_team_table <- function(misc_team) {
  stopifnot(!is.null(misc_team), nrow(misc_team) > 0)

  side <- tolower(as.character(col_or(misc_team, "^Team_or_Opponent$")))
  if (all(is.na(side))) {
    stop("Team_or_Opponent 列が見つかりません。worldfootballR を最新版に更新してください。",
         call. = FALSE)
  }

  base <- tibble(
    competition     = as.character(col_or(misc_team, c("^Competition_Name$", "^Comp$"))),
    country         = as.character(col_or(misc_team, "^Country$")),
    season_end_year = suppressWarnings(as.integer(col_or(misc_team, "^Season_End_Year$"))),
    squad           = str_squish(str_remove(as.character(col_or(misc_team, "^Squad$")), "^vs\\s+")),
    side            = side,
    offsides        = suppressWarnings(as.numeric(col_or(misc_team, c("^Off$", "^Off_", "^Offsides?$")))),
    n90             = suppressWarnings(as.numeric(col_or(misc_team, c("^Mins_Per_90$", "^X90s$", "^90s$"))))
  ) %>%
    filter(side %in% c("team", "opponent"), !is.na(squad))

  w <- base %>%
    distinct(competition, country, season_end_year, squad, side, .keep_all = TRUE) %>%
    pivot_wider(names_from = side, values_from = c(offsides, n90))

  if (!all(c("offsides_team", "offsides_opponent") %in% names(w))) {
    stop("チーム／相手の両方のオフサイドデータが揃いませんでした。", call. = FALSE)
  }

  w %>%
    transmute(
      league_understat  = fb_to_understat_league(country, competition),
      competition, country,
      season_end_year,
      season_start_year = season_end_year - 1L,
      squad,
      試合数 = dplyr::coalesce(n90_team, n90_opponent),
      かかる数 = offsides_team,        # 自チームがオフサイドを取られた
      かける数 = offsides_opponent,    # 相手をオフサイドに掛けた
      かかる数_試合平均 = かかる数 / 試合数,
      かける数_試合平均 = かける数 / 試合数,
      差分_試合平均     = かける数_試合平均 - かかる数_試合平均,
      トラップ比率      = かける数 / (かける数 + かかる数)
    ) %>%
    filter(!is.na(かかる数) | !is.na(かける数)) %>%
    arrange(desc(season_start_year), competition, desc(かける数_試合平均))
}

#' 選手別オフサイド表（＝オフサイドにかかった数。個人にかける数は定義できない）
offside_player_table <- function(misc_player) {
  stopifnot(!is.null(misc_player), nrow(misc_player) > 0)

  tibble(
    player          = as.character(col_or(misc_player, "^Player$")),
    squad           = str_squish(as.character(col_or(misc_player, "^Squad$"))),
    competition     = as.character(col_or(misc_player, c("^Comp$", "^Competition_Name$"))),
    country         = as.character(col_or(misc_player, "^Country$")),
    pos             = as.character(col_or(misc_player, "^Pos$")),
    season_end_year = suppressWarnings(as.integer(col_or(misc_player, "^Season_End_Year$"))),
    nineties        = suppressWarnings(as.numeric(col_or(misc_player, c("^Mins_Per_90$", "^X90s$", "^90s$")))),
    かかる数        = suppressWarnings(as.numeric(col_or(misc_player, c("^Off$", "^Off_", "^Offsides?$"))))
  ) %>%
    mutate(
      competition = str_squish(str_remove(competition, "^[a-z]{2}\\s")),  # "es La Liga" -> "La Liga"
      league_understat  = fb_to_understat_league(country, competition),
      season_start_year = season_end_year - 1L,
      かかる数_90分 = if_else(nineties > 0, かかる数 / nineties, NA_real_)
    ) %>%
    filter(!is.na(player), !is.na(かかる数))
}


# -----------------------------------------------------------------------------
# 7b. ESPN：オフサイドの補完（FBref の事前取得データが途中までのシーズン用）
#     ESPN の試合詳細（summary）にはチームごと・選手ごとのオフサイド数がある。
#     両チームの数を使えば、かかった数（自チーム）とかけた数（相手）を出せる。
#     2023/24 の Barcelona で FBref と完全に一致することを確認済み（99 / 117）。
# -----------------------------------------------------------------------------

ESPN_LEAGUES <- c("EPL" = "eng.1", "La liga" = "esp.1", "Bundesliga" = "ger.1",
                  "Serie A" = "ita.1", "Ligue 1" = "fra.1")
ESPN_UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36"

espn_url <- function(slug, path) sprintf("https://site.web.api.espn.com/apis/site/v2/sports/soccer/%s/%s", slug, path)
espn_handle <- function() curl::new_handle(useragent = ESPN_UA, accept_encoding = "gzip", timeout = 60)
espn_parse <- function(raw) jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)

#' シーズンの試合一覧（7/1〜翌6/30）
espn_season_events <- function(league, season_start_year) {
  slug <- ESPN_LEAGUES[[league]]
  s <- as.integer(season_start_year)
  res <- curl::curl_fetch_memory(
    espn_url(slug, sprintf("scoreboard?dates=%d0701-%d0630&limit=1000", s, s + 1L)), espn_handle())
  if (res$status_code != 200) stop("ESPN scoreboard: HTTP ", res$status_code, call. = FALSE)
  ev <- espn_parse(res$content)$events
  if (!length(ev)) return(tibble(event_id = character(), date = character(), completed = logical()))
  tibble(
    event_id  = vapply(ev, function(e) as.character(e$id), ""),
    date      = vapply(ev, function(e) substr(e$date, 1, 10), ""),
    completed = vapply(ev, function(e) isTRUE(e$status$type$completed), TRUE)
  )
}

#' 試合詳細 → チーム2行と選手行（オフサイド数）
espn_parse_summary <- function(j, event_id) {
  stat_of <- function(t, nm) {
    s <- Filter(function(x) identical(x$name, nm), t$statistics %||% list())
    if (length(s)) suppressWarnings(as.numeric(s[[1]]$displayValue)) else NA_real_
  }
  teams <- purrr::map_dfr(j$boxscore$teams %||% list(), function(t) tibble(
    event_id = event_id, team_id = as.character(t$team$id), team = t$team$displayName,
    home_away = t$homeAway %||% NA_character_, offsides = stat_of(t, "offsides")))
  if (nrow(teams) != 2 || anyNA(teams$offsides)) return(NULL)   # 統計が無い試合は使わない
  teams$opp_offsides <- rev(teams$offsides)

  players <- purrr::map_dfr(j$rosters %||% list(), function(r) {
    purrr::map_dfr(r$roster %||% list(), function(p) {
      st <- p$stats %||% list()
      val <- function(ab) { s <- Filter(function(x) identical(x$abbreviation, ab), st); if (length(s)) as.numeric(s[[1]]$value) else 0 }
      if (!length(st) || val("APP") < 1) return(NULL)
      tibble(event_id = event_id, team = r$team$displayName, player = p$athlete$displayName %||% NA_character_,
             pos = p$position$abbreviation %||% NA_character_, starter = isTRUE(p$starter), offsides = val("OF"))
    })
  })
  list(teams = teams, players = players)
}

#' 複数試合の詳細を並行取得（試合ごとにキャッシュ。終わった試合は取り直さない）
espn_summaries <- function(league, event_ids, concurrency = 6) {
  slug <- ESPN_LEAGUES[[league]]
  key  <- function(id) file.path(wfr_cache_dir(), sprintf("espn_summary_%s_%s.rds", slug, id))
  # 一時的な通信エラーに備えて、取れなかった試合は最大3回まで取り直す
  for (round in 1:3) {
    todo <- event_ids[!file.exists(key(event_ids))]
    if (!length(todo)) break
    if (round > 1) Sys.sleep(5 * (round - 1))
    message(sprintf("[ESPN] %s: %d 試合の詳細を取得します%s", league, length(todo), if (round > 1) "（再試行）" else ""))
    pool <- curl::new_pool(total_con = concurrency, host_con = concurrency)
    for (id in todo) local({
      i <- id
      curl::curl_fetch_multi(espn_url(slug, paste0("summary?event=", i)), pool = pool, handle = espn_handle(),
        done = function(res) {
          if (res$status_code != 200) return(invisible())
          parsed <- tryCatch(espn_parse_summary(espn_parse(res$content), i), error = function(e) NULL)
          # 統計が載っていない試合も「空」として保存し、毎回取り直さないようにする
          saveRDS(parsed %||% list(teams = NULL, players = NULL), key(i))
        },
        fail = function(msg) message("[ESPN 取得失敗] ", i, " : ", msg))
    })
    curl::multi_run(pool = pool)
  }
  have <- event_ids[file.exists(key(event_ids))]
  lapply(have, function(id) readRDS(key(id)))
}

#' ESPN から、FBref の offside_team_table / offside_player_table と同じ形の表を作る
espn_offside_tables <- function(league, seasons) {
  if (!league %in% names(ESPN_LEAGUES)) return(list(team = tibble(), player = tibble()))
  cur <- current_season_start_year()
  parts <- purrr::map(seasons, function(s) {
    ev <- cached(sprintf("espn_events_%s_%d", ESPN_LEAGUES[[league]], s),
                 espn_season_events(league, s), ttl_hours = if (s >= cur) 6 else 24 * 30)
    if (is.null(ev) || !nrow(ev)) return(NULL)
    sm <- espn_summaries(league, ev$event_id[ev$completed])
    if (!length(sm)) return(NULL)
    list(season = s,
         teams   = dplyr::bind_rows(lapply(sm, `[[`, "teams")),
         players = dplyr::bind_rows(lapply(sm, `[[`, "players")))
  })
  parts <- Filter(Negate(is.null), parts)

  team <- purrr::map_dfr(parts, function(p) {
    if (!nrow(p$teams)) return(NULL)
    p$teams %>%
      group_by(squad = team) %>%
      summarise(試合数 = n(), かかる数 = sum(offsides), かける数 = sum(opp_offsides), .groups = "drop") %>%
      mutate(season_start_year = p$season)
  })
  player <- purrr::map_dfr(parts, function(p) {
    if (!nrow(p$players)) return(NULL)
    p$players %>%
      group_by(player, squad = team) %>%
      summarise(pos = dplyr::first(stats::na.omit(pos)), 出場試合 = n(), 先発 = sum(starter),
                かかる数 = sum(offsides), .groups = "drop") %>%
      mutate(season_start_year = p$season)
  })
  if (nrow(team)) {
    team <- team %>%
      transmute(league_understat = league, competition = NA_character_, country = NA_character_,
                season_end_year = season_start_year + 1L, season_start_year, squad, 試合数,
                かかる数, かける数,
                かかる数_試合平均 = かかる数 / 試合数, かける数_試合平均 = かける数 / 試合数,
                差分_試合平均 = かける数_試合平均 - かかる数_試合平均,
                トラップ比率 = かける数 / (かける数 + かかる数), source = "ESPN")
  }
  if (nrow(player)) {
    player <- player %>%
      transmute(player, squad, competition = NA_character_, country = NA_character_, pos,
                season_end_year = season_start_year + 1L, nineties = NA_real_, 出場試合, 先発, かかる数,
                league_understat = league, season_start_year, かかる数_90分 = NA_real_, source = "ESPN")
  }
  list(team = team, player = player)
}

#' FBref（〜データが揃っているシーズン）と ESPN（それ以降〜最新）をつないだオフサイド表
#'
#' FBref の収録試合数が、そのリーグで最も揃っているシーズンの 9 割未満になった
#' 最初のシーズンから、現在のシーズンまでを ESPN で置き換える。
#' ESPN 側のチーム名は FBref の表記にそろえる（同じチームの推移を追えるように）。
offside_tables_combined <- function(leagues, last_season = current_season_start_year()) {
  fb_t <- offside_team_table(fb_misc("team")) %>% mutate(source = "FBref", squad = unify_squad(squad))
  fb_p <- offside_player_table(fb_misc("player")) %>%
    mutate(source = "FBref", squad = unify_squad(squad), 出場試合 = NA_real_, 先発 = NA_real_)

  # FBref の古いシーズンには、オフサイドが記録されていない（リーグ合計が 0）ものがあるので除く
  fb_t <- fb_t %>%
    group_by(league_understat, season_start_year) %>%
    filter(sum(かかる数, na.rm = TRUE) > 0, sum(かける数, na.rm = TRUE) > 0) %>%
    ungroup()
  fb_p <- semi_join(fb_p, fb_t, by = c("league_understat", "season_start_year"))

  out <- purrr::map(leagues, function(lg) {
    ft <- filter(fb_t, league_understat == lg)
    # 揃っている = 1チームあたりの試合数が 2 ×（チーム数 − 1）の 9 割以上
    cov <- ft %>% group_by(season_start_year) %>%
      summarise(md = stats::median(試合数, na.rm = TRUE), full = 2 * (n() - 1), .groups = "drop")
    # FBref で最後に揃っているシーズンの次から ESPN を使う
    # （途中で打ち切られた 2019/20 のような過去シーズンは FBref のまま）
    ok   <- cov$season_start_year[cov$md >= 0.9 * cov$full]
    from <- if (length(ok)) max(ok) + 1L else 2024L
    seasons <- if (from <= last_season) seq(from, last_season) else integer()

    es <- if (length(seasons)) espn_offside_tables(lg, seasons) else list(team = tibble(), player = tibble())
    if (!nrow(es$team)) return(list(team = ft, player = filter(fb_p, league_understat == lg)))

    # ESPN 名 → FBref 名（そのリーグの全シーズンの表記から探す）
    fb_names <- unique(ft$squad)
    map <- harmonise_team_names(unique(es$team$squad), fb_names) %>%
      mutate(to = dplyr::coalesce(fb_name, us_name))
    rename_sq <- function(d) if (nrow(d)) mutate(d, squad = map$to[match(squad, map$us_name)]) else d
    list(
      team   = bind_rows(filter(ft, !season_start_year %in% seasons), rename_sq(es$team)),
      player = bind_rows(filter(fb_p, league_understat == lg, !season_start_year %in% seasons), rename_sq(es$player)),
      map    = mutate(map, league_understat = lg)
    )
  })
  list(
    team   = bind_rows(lapply(out, `[[`, "team")),
    player = bind_rows(lapply(out, `[[`, "player")),
    name_map = bind_rows(lapply(out, `[[`, "map"))
  )
}


# -----------------------------------------------------------------------------
# 8. Understat と FBref のチーム名を突合する
# -----------------------------------------------------------------------------

#' Understat 表記 -> FBref 表記（自動判定が効きにくいものを手動定義）
TEAM_NAME_DICT <- c(
  "Wolverhampton Wanderers" = "Wolves",
  "Nottingham Forest"       = "Nott'ham Forest",
  "Manchester United"       = "Manchester Utd",
  "Newcastle United"        = "Newcastle Utd",
  "Sheffield United"        = "Sheffield Utd",
  "West Bromwich Albion"    = "West Brom",
  "Leeds"                   = "Leeds United",
  "Paris Saint Germain"     = "Paris S-G",
  "Borussia M.Gladbach"     = "Gladbach",
  "RasenBallsport Leipzig"  = "RB Leipzig",
  "Eintracht Frankfurt"     = "Eint Frankfurt",
  "Bayer Leverkusen"        = "Leverkusen",
  "FC Cologne"              = "Köln",
  "Hertha Berlin"           = "Hertha BSC",
  "Internazionale"          = "Inter",
  "AC Milan"                = "Milan",
  "Verona"                  = "Hellas Verona",
  "SPAL 2013"               = "SPAL",
  "Atletico Madrid"         = "Atlético Madrid",
  "Real Betis"              = "Betis",
  "Deportivo La Coruna"     = "La Coruña",
  "Sporting Gijon"          = "Sporting Gijón",
  # ESPN 表記 -> FBref 表記
  "Brighton & Hove Albion"   = "Brighton",
  "Tottenham Hotspur"        = "Tottenham",
  "West Ham United"          = "West Ham",
  "Borussia Dortmund"        = "Dortmund",
  "Borussia Mönchengladbach" = "Gladbach",
  "VfL Bochum"               = "Bochum",
  "Paris Saint-Germain"      = "Paris S-G",
  "Stade Rennais"            = "Rennes",
  "Stade de Reims"           = "Reims",
  "Dijon FCO"                = "Dijon",
  "Deportivo"                = "La Coruña",
  # Understat 表記 -> FBref 表記（接尾辞の違い）
  "Leicester"                = "Leicester City",
  "Stoke"                    = "Stoke City",
  "Swansea"                  = "Swansea City",
  "Hull"                     = "Hull City",
  "Cardiff"                  = "Cardiff City",
  "Norwich"                  = "Norwich City",
  "Luton"                    = "Luton Town",
  "Ipswich"                  = "Ipswich Town",
  "Coventry"                 = "Coventry City",
  "Fortuna Duesseldorf"      = "Düsseldorf",
  "Arminia Bielefeld"        = "Arminia",
  "Parma Calcio 1913"        = "Parma"
)

#' FBref 自体の表記ゆれ（同じクラブがシーズンによって別表記）をそろえる
FBREF_SQUAD_ALIAS <- c("M'Gladbach" = "Gladbach")
unify_squad <- function(x) { i <- match(x, names(FBREF_SQUAD_ALIAS)); ifelse(is.na(i), x, unname(FBREF_SQUAD_ALIAS[i])) }

norm_name <- function(x) {
  x <- as.character(x)
  y <- suppressWarnings(iconv(x, to = "ASCII//TRANSLIT"))
  x <- ifelse(is.na(y), x, y)
  x <- tolower(x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- gsub("\\butd\\b", "united", x)
  x <- gsub("\\b(fc|cf|ac|as|ss|ssc|sc|afc|ud|cd|rcd|rc|sd|club|de|calcio|1899|1846|1904|04|05)\\b", " ", x)
  str_squish(x)
}

#' Understat のチーム名を FBref のチーム名に対応づける
#'
#' @return tibble(us_name, fb_name, 方法, 距離)
harmonise_team_names <- function(us_names, fb_names, max_rel_dist = 0.34) {
  us_names <- unique(as.character(us_names))
  fb_names <- unique(as.character(fb_names))
  if (!length(us_names) || !length(fb_names)) {
    return(tibble(us_name = character(), fb_name = character(), 方法 = character(), 距離 = numeric()))
  }

  fb_norm <- norm_name(fb_names)

  purrr::map_dfr(us_names, function(nm) {
    # 1) 完全一致
    if (nm %in% fb_names) {
      return(tibble(us_name = nm, fb_name = nm, 方法 = "完全一致", 距離 = 0))
    }
    # 2) 手動辞書
    if (nm %in% names(TEAM_NAME_DICT) && TEAM_NAME_DICT[[nm]] %in% fb_names) {
      return(tibble(us_name = nm, fb_name = unname(TEAM_NAME_DICT[[nm]]), 方法 = "辞書", 距離 = 0))
    }
    # 3) 正規化後の一致 / 最近傍
    nn <- norm_name(nm)
    d  <- as.numeric(utils::adist(nn, fb_norm))
    i  <- which.min(d)
    rel <- d[i] / max(nchar(nn), 1)
    if (length(i) && rel <= max_rel_dist) {
      tibble(us_name = nm, fb_name = fb_names[i],
             方法 = if (d[i] == 0) "正規化一致" else "類似一致", 距離 = round(rel, 3))
    } else {
      tibble(us_name = nm, fb_name = NA_character_, 方法 = "未対応", 距離 = NA_real_)
    }
  })
}

#' 順位表（Understat）とオフサイド表（FBref）を結合する
join_offside_with_xg <- function(stand, off_team) {
  if (!nrow(stand) || !nrow(off_team)) return(tibble())

  purrr::map_dfr(split(stand, list(stand$league_understat, stand$season_start_year), drop = TRUE), function(s) {
    lg <- s$league_understat[1]; yr <- s$season_start_year[1]
    o  <- off_team %>% filter(league_understat == lg, season_start_year == yr)
    if (!nrow(o)) return(NULL)

    map <- harmonise_team_names(s$team, o$squad)
    s %>%
      left_join(map, by = c("team" = "us_name")) %>%
      left_join(o, by = c("fb_name" = "squad",
                          "league_understat" = "league_understat",
                          "season_start_year" = "season_start_year")) %>%
      mutate(xG_試合平均  = xG  / MP,
             xGA_試合平均 = xGA / MP)
  })
}


# -----------------------------------------------------------------------------
# 8b. 統計分析：オフサイドにかけた数 × かかった数
#
#  分析の単位は「チーム×シーズン」（1試合あたりの回数）。注意点：
#   - オフサイドは必ず「誰かがかかる = 相手がかけた」なので、リーグ×シーズンでは
#     かけた数の合計 = かかった数の合計。全体水準が高いリーグ・シーズンほど両方が多くなり、
#     そのまま並べると見かけの正の相関が混ざる → リーグ×シーズン内で比べる。
#   - チームは自分自身と対戦しないため、統計の仕組みだけで弱い負の相関が生じる
#     → 帰無シミュレーションで大きさを見積もる。
#   - チーム力（押し込む力）が両方に効く可能性 → Understat の xG 差で統制する。
# -----------------------------------------------------------------------------

#' チーム力（Understat の xG 差・勝点／試合）をオフサイド表の squad に結合するための表
offside_team_quality <- function(off_team, leagues) {
  yrs <- sort(unique(off_team$season_start_year))
  purrr::map_dfr(leagues, function(lg) {
    st <- standings(tidy_results(us_results(lg, yrs)))
    if (!nrow(st)) return(NULL)
    purrr::map_dfr(split(st, st$season_start_year), function(u) {
      o <- filter(off_team, league_understat == lg, season_start_year == u$season_start_year[1])
      if (!nrow(o)) return(NULL)
      m <- harmonise_team_names(u$team, o$squad) %>% filter(!is.na(fb_name))
      m %>%
        transmute(league_understat = lg, season_start_year = u$season_start_year[1],
                  squad = fb_name, u_team = us_name, 突合 = 方法, 距離) %>%
        left_join(transmute(u, u_team = team, xgd_pm = xGD / MP, pts_pm = Pts / MP), by = "u_team")
    })
  }) %>%
    arrange(距離) %>%
    distinct(league_understat, season_start_year, squad, .keep_all = TRUE)
}

#' 分析用データ：シーズンを通して揃っているチーム×シーズンだけ
offside_corr_data <- function(off_team, quality = NULL) {
  d <- off_team %>%
    group_by(league_understat, season_start_year) %>%
    mutate(n_teams = n(), md = stats::median(試合数, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(md >= 0.9 * 2 * (n_teams - 1), 試合数 > 0) %>%
    mutate(ke = かける数 / 試合数, ka = かかる数 / 試合数,
           team = paste(league_understat, squad),
           ls = paste(league_understat, season_start_year))
  if (!is.null(quality)) {
    d <- left_join(d, select(quality, league_understat, season_start_year, squad, xgd_pm, pts_pm),
                   by = c("league_understat", "season_start_year", "squad"))
  }
  d %>%
    group_by(ls) %>%
    mutate(z_ke = as.numeric(scale(ke)), z_ka = as.numeric(scale(ka)),
           d_ke = ke - mean(ke), d_ka = ka - mean(ka),
           z_xgd = if ("xgd_pm" %in% names(.)) as.numeric(scale(xgd_pm)) else NA_real_) %>%
    ungroup()
}

#' 相関係数と 95% 信頼区間（Fisher の z 変換）・p 値、スピアマンの順位相関
cor_summary <- function(x, y, label, note = "") {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  p <- stats::cor.test(x, y)
  s <- suppressWarnings(stats::cor.test(x, y, method = "spearman"))
  tibble(分析 = label, n = length(x), r = unname(p$estimate), lo = p$conf.int[1], hi = p$conf.int[2],
         p = p$p.value, rho = unname(s$estimate), p_rho = s$p.value, 説明 = note)
}

#' 回帰係数のクラスタ頑健標準誤差（CR1、チーム単位でクラスタ）
cluster_coef <- function(fit, cluster, term) {
  keep <- !is.na(stats::coef(fit))
  X <- stats::model.matrix(fit)[, keep, drop = FALSE]
  u <- stats::resid(fit)
  n <- nrow(X); k <- ncol(X); cl <- as.character(cluster); G <- length(unique(cl))
  XtXi <- solve(crossprod(X))
  meat <- crossprod(rowsum(X * u, cl))
  V <- XtXi %*% meat %*% XtXi * (G / (G - 1)) * ((n - 1) / (n - k))
  j <- which(colnames(X) == term)
  est <- unname(stats::coef(fit)[term]); se <- sqrt(V[j, j]); tq <- stats::qt(0.975, G - 1)
  tibble(est = est, se = se, lo = est - tq * se, hi = est + tq * se,
         p = 2 * stats::pt(-abs(est / se), G - 1), n = n, clusters = G)
}

#' 回帰モデル（目的変数：かかった数／試合、説明変数：かけた数／試合）
offside_corr_models <- function(d) {
  fit_one <- function(id, label, formula, data, note) {
    data <- data[stats::complete.cases(data[, all.vars(formula)]), ]
    fit <- stats::lm(formula, data)
    cluster_coef(fit, data$team, "ke") %>%
      mutate(id = id, モデル = label, 説明 = note, r2 = summary(fit)$r.squared, .before = 1)
  }
  dplyr::bind_rows(
    fit_one("m1", "① 単純回帰", ka ~ ke, d, "全リーグ・全シーズンをそのまま"),
    fit_one("m2", "② リーグ×シーズン固定効果", ka ~ ke + factor(ls), d,
            "リーグ・シーズンごとの全体水準の違いを除く"),
    fit_one("m3", "③ ＋チーム固定効果", ka ~ ke + factor(ls) + factor(team), d,
            "同じチームの中で、かけた数が多いシーズンほどかかった数が少ないか"),
    fit_one("m4", "④ ＋チーム力（xG差／試合）", ka ~ ke + xgd_pm + factor(ls) + factor(team), d,
            "押し込む力の違いによる見かけの関係を除く（Understat と突合できたチームのみ）")
  )
}

#' 相関の一覧（素の値／リーグ×シーズン内／前季からの変化／チーム力を統制）
offside_corr_table <- function(d) {
  ch <- d %>%
    arrange(team, season_start_year) %>%
    group_by(team) %>%
    mutate(gap = season_start_year - lag(season_start_year),
           c_ke = d_ke - lag(d_ke), c_ka = d_ka - lag(d_ka)) %>%
    ungroup() %>%
    filter(gap == 1)
  dq <- filter(d, is.finite(z_xgd))
  r_ke <- stats::resid(stats::lm(z_ke ~ z_xgd, dq)); r_ka <- stats::resid(stats::lm(z_ka ~ z_xgd, dq))
  dplyr::bind_rows(
    cor_summary(d$ke, d$ka, "素の値", "全リーグ・全シーズンをそのまま並べる"),
    cor_summary(d$z_ke, d$z_ka, "リーグ×シーズン内", "同じリーグ・同じシーズンのチーム同士で比べる（z スコア）"),
    cor_summary(ch$c_ke, ch$c_ka, "前季からの変化", "同じチームが前季よりかけた数を増やしたとき、かかった数は減ったか"),
    cor_summary(r_ke, r_ka, "チーム力を統制", "リーグ×シーズン内の値から xG 差の影響を除いた偏相関")
  )
}

#' 帰無シミュレーション：「かけやすさ」と「かかりやすさ」が無関係でも生じる相関の大きさ
#'
#' 各リーグ×シーズンで、チームのかけやすさを並べ替えてかかりやすさと無関係にし、
#' 「自分とは対戦しない」日程の期待値にポアソン誤差を加えて、リーグ×シーズン内の相関を計算する。
offside_null_sim <- function(d, B = 500, seed = 1) {
  key <- sprintf("offside_null_%d_%d_%d", nrow(d), B, round(sum(d$かかる数) + sum(d$かける数)))
  cached(key, {
    set.seed(seed)
    groups <- split(d, d$ls)
    one <- function() {
      z <- lapply(groups, function(x) {
        n <- nrow(x); a <- x$ka; t <- sample(x$ke); g <- x$試合数
        exp_ka <- a * (sum(t) - t) / (n - 1) / mean(t)
        exp_ke <- t * (sum(a) - a) / (n - 1) / mean(a)
        ke <- stats::rpois(n, exp_ke * g) / g; ka <- stats::rpois(n, exp_ka * g) / g
        cbind(as.numeric(scale(ke)), as.numeric(scale(ka)))
      })
      z <- do.call(rbind, z)
      stats::cor(z[, 1], z[, 2], use = "complete.obs")
    }
    r_null <- replicate(B, one())
    obs <- stats::cor(d$z_ke, d$z_ka)
    list(r = r_null, mean = mean(r_null), lo = unname(stats::quantile(r_null, 0.025)),
         hi = unname(stats::quantile(r_null, 0.975)), obs = obs,
         p = (sum(r_null <= obs) + 1) / (B + 1), B = B)
  }, ttl_hours = 24 * 7)
}


# -----------------------------------------------------------------------------
# 9. 可視化ヘルパー
# -----------------------------------------------------------------------------

#' 日本語が化けないフォントを探して返す（見つからなければ既定フォント）
jp_family <- function() {
  cand <- c("Hiragino Sans", "Hiragino Kaku Gothic ProN", "Yu Gothic", "YuGothic",
            "Meiryo", "MS Gothic", "Noto Sans CJK JP", "Noto Sans JP",
            "IPAexGothic", "IPAPGothic", "TakaoPGothic")
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    have <- unique(systemfonts::system_fonts()$family)
    hit <- cand[cand %in% have]
    if (length(hit)) return(hit[1])
  }
  ""
}

theme_wfr <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size, base_family = jp_family()) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "grey35"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Understat 座標（X,Y が 0-1）用のハーフピッチ
gg_pitch_half <- function(fill = "white", line = "grey55") {
  L <- 105; W <- 68
  list(
    ggplot2::annotate("rect", xmin = L/2, xmax = L, ymin = 0, ymax = W,
                      fill = fill, colour = line),
    ggplot2::annotate("rect", xmin = L - 16.5, xmax = L, ymin = W/2 - 20.16, ymax = W/2 + 20.16,
                      fill = NA, colour = line),
    ggplot2::annotate("rect", xmin = L - 5.5,  xmax = L, ymin = W/2 - 9.16,  ymax = W/2 + 9.16,
                      fill = NA, colour = line),
    ggplot2::annotate("rect", xmin = L, xmax = L + 2, ymin = W/2 - 3.66, ymax = W/2 + 3.66,
                      fill = NA, colour = line),
    ggplot2::annotate("point", x = L - 11, y = W/2, colour = line, size = 1),
    ggplot2::coord_fixed(xlim = c(L/2 - 2, L + 3), ylim = c(-1, W + 1)),
    ggplot2::theme_void(base_family = jp_family()),
    ggplot2::theme(legend.position = "bottom")
  )
}

#' シュートマップ
gg_shotmap <- function(sh, title = NULL, subtitle = NULL) {
  d <- sh %>%
    mutate(x = X * 105, y = Y * 68,
           結果 = if_else(is_goal, "ゴール", "ゴール以外"))
  ggplot2::ggplot() +
    gg_pitch_half() +
    ggplot2::geom_point(data = filter(d, !is_goal),
                        ggplot2::aes(x, y, size = xG), colour = "grey45", alpha = .45) +
    ggplot2::geom_point(data = filter(d, is_goal),
                        ggplot2::aes(x, y, size = xG), colour = "#d7263d", alpha = .85) +
    ggplot2::scale_size_continuous(range = c(1, 7), name = "xG") +
    ggplot2::labs(title = title, subtitle = subtitle)
}
