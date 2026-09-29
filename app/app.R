# =============================================================================
#  app.R  —  欧州サッカー インタラクティブ分析ダッシュボード
#
#  元の reports/seasons/laliga_24-25.Rmd を「シーズン・リーグ・チーム・選手を
#  自由に切り替えて分析できる」形に作り替えたものです。
#
#  起動方法（プロジェクトのルートで）:
#    shiny::runApp("app")
#
#  データソース: Understat (https://understat.com/) / FBref (https://fbref.com/)
#                worldfootballR 経由
# =============================================================================

# ---- 0. パッケージ確認 ------------------------------------------------------
.req <- c("shiny", "bslib", "dplyr", "tidyr", "purrr", "stringr", "tibble",
          "ggplot2", "ggrepel", "plotly", "DT", "readr", "scales")
.missing <- .req[!vapply(.req, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("次のパッケージをインストールしてください:\n  install.packages(c(",
       paste0('"', .missing, '"', collapse = ", "), "))", call. = FALSE)
}
if (!requireNamespace("worldfootballR", quietly = TRUE)) {
  stop("worldfootballR が必要です:\n",
       '  install.packages("devtools"); devtools::install_github("JaseZiv/worldfootballR")',
       call. = FALSE)
}

if (utils::packageVersion("bslib") < "0.5.0") {
  stop('bslib 0.5.0 以上が必要です: install.packages("bslib")', call. = FALSE)
}

library(shiny)
library(bslib)
library(plotly)
library(DT)

# 更新後も現在の選択を保つ（選択肢に無ければ先頭を選ぶ）
keep_sel <- function(current, choices) {
  if (!is.null(current) && length(current) && all(nzchar(current)) &&
      all(current %in% choices)) current else choices[1]
}

.helpers <- Filter(file.exists, c("R/wfr_helpers.R", "../R/wfr_helpers.R"))
if (!length(.helpers)) {
  stop("R/wfr_helpers.R が見つかりません。プロジェクトのルートか app/ フォルダから起動してください。", call. = FALSE)
}
source(.helpers[1], encoding = "UTF-8")

CUR <- current_season_start_year()
ggplot2::theme_set(theme_wfr())

METRICS <- c(
  "勝点 (Pts)"           = "Pts",
  "期待勝点 (xPts)"      = "xPts",
  "勝点 - 期待勝点"      = "Pts-xPts",
  "得点 (G)"             = "G",
  "失点 (GA)"            = "GA",
  "得失点差 (GD)"        = "GD",
  "xG"                   = "xG",
  "xGA"                  = "xGA",
  "xGD"                  = "xGD",
  "決定力 (G - xG)"      = "G-xG",
  "守備力 (GA - xGA)"    = "GA-xGA"
)


# =============================================================================
#  UI
# =============================================================================
ui <- page_navbar(
  title = "欧州サッカー分析ダッシュボード",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,

  sidebar = sidebar(
    width = 320,
    h5("共通設定"),
    selectInput("leagues", "リーグ（複数選択可）",
                choices = league_choices(), selected = "La liga", multiple = TRUE),
    sliderInput("seasons", "シーズン（開始年）",
                min = FIRST_SEASON, max = CUR, value = c(CUR - 2L, CUR - 1L),
                step = 1, sep = ""),
    helpText(HTML("例: 2024 を選ぶと <b>2024/25</b> シーズンです。<br>",
                  "範囲を広げるほど取得に時間がかかります。")),
    actionButton("load", "データ取得 / 更新", class = "btn-primary w-100"),
    hr(),
    h6("キャッシュ"),
    helpText("取得済みデータは data/cache フォルダに保存され、次回から高速に読み込まれます。"),
    actionButton("clear_cache", "キャッシュを削除", class = "btn-outline-secondary btn-sm w-100"),
    hr(),
    helpText(HTML("出典: <a href='https://understat.com/' target='_blank'>Understat</a> / ",
                  "<a href='https://fbref.com/' target='_blank'>FBref</a>"))
  ),

  # ---------------------------------------------------------------- 順位表 --
  nav_panel(
    "順位表・チーム概要",
    layout_columns(
      col_widths = c(6, 6),
      selectInput("t1_league", "リーグ", choices = NULL),
      selectInput("t1_season", "シーズン", choices = NULL)
    ),
    card(
      card_header("順位表（実績 + 期待値）"),
      DTOutput("t1_table")
    ),
    card(
      card_header("累積勝点の推移"),
      sliderInput("t1_topn", "表示チーム数（上位）", min = 3, max = 20, value = 10, step = 1),
      plotlyOutput("t1_cum", height = "480px")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("xG と xGA"), plotOutput("t1_xg_xga", height = "480px")),
      card(card_header("得点と xG（決定力）"), plotOutput("t1_g_xg", height = "480px"))
    )
  ),

  # ------------------------------------------------------------ チーム比較 --
  nav_panel(
    "チーム比較",
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput("t2_metric", "比較する指標", choices = METRICS, selected = "xGD"),
      selectInput("t2_season", "シーズン（チーム別ランキング用）", choices = NULL),
      selectizeInput("t2_teams", "推移を見るチーム（複数選択可）", choices = NULL, multiple = TRUE)
    ),
    card(
      card_header("同一シーズンのチーム別比較"),
      plotOutput("t2_bar", height = "620px")
    ),
    card(
      card_header("シーズンごとの推移"),
      plotlyOutput("t2_trend", height = "480px")
    ),
    card(
      card_header("チーム×シーズン 一覧"),
      DTOutput("t2_table")
    )
  ),

  # -------------------------------------------------------------- 選手分析 --
  nav_panel(
    "選手分析",
    card(
      card_body(
        p("Understat のシュートデータ（2014/15以降の全シーズン）をリーグ単位で読み込みます。",
          "初回のみ時間がかかります。"),
        actionButton("t3_load", "シュートデータを読み込む", class = "btn-primary")
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput("t3_league", "リーグ", choices = NULL),
      selectInput("t3_season", "シーズン", choices = NULL),
      sliderInput("t3_minshots", "散布図の最小シュート数", min = 1, max = 60, value = 20)
    ),
    card(
      card_header("選手ランキング（シュートデータから算出）"),
      DTOutput("t3_table")
    ),
    card(
      card_header("得点 vs xG（選択した選手を強調）"),
      selectizeInput("t3_players", "注目する選手（複数選択可）", choices = NULL, multiple = TRUE,
                     options = list(placeholder = "選手名を入力して検索")),
      plotOutput("t3_g_xg", height = "560px")
    ),
    card(
      card_header("シュートマップ（選択した選手）"),
      plotOutput("t3_shotmap", height = "480px")
    ),
    card(
      card_header("選択した選手のシーズン推移"),
      plotlyOutput("t3_trend", height = "440px")
    ),
    card(
      card_header("得点の時間帯分布"),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("t3_team", "チーム", choices = NULL),
        radioButtons("t3_hist_scope", "対象", inline = TRUE,
                     choices = c("チームの得点" = "team", "選択選手の得点" = "player"))
      ),
      plotOutput("t3_hist", height = "400px")
    ),
    card(
      card_header("出場時間つきの選手スタッツ（Understat チームページから取得）"),
      card_body(
        p("シュートデータには出場時間が含まれないため、必要なときだけ個別に取得します。"),
        actionButton("t3_load_team", "選択中のチーム・シーズンで取得", class = "btn-outline-primary")
      ),
      DTOutput("t3_team_table")
    )
  ),

  # ---------------------------------------------------------- オフサイド --
  nav_panel(
    "オフサイド分析",
    card(
      card_body(
        HTML(paste0(
          "<p>Understat にはオフサイドの指標がないため、<b>FBref の misc（その他）スタッツ</b>を使います。",
          "対象はビッグ5リーグ（イングランド・スペイン・ドイツ・イタリア・フランス）です。</p>",
          "<ul>",
          "<li><b>かかる数</b>：自チームの選手がオフサイドを取られた回数</li>",
          "<li><b>かける数</b>：相手チームをオフサイドに掛けた回数（オフサイドトラップの成立回数）</li>",
          "<li><b>トラップ比率</b>：かける数 ÷（かける数 + かかる数）</li>",
          "</ul>"
        )),
        actionButton("t4_load", "オフサイドデータを読み込む", class = "btn-primary")
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      selectInput("t4_season", "シーズン（散布図・ランキング用）", choices = NULL),
      radioButtons("t4_basis", "集計単位", inline = TRUE,
                   choices = c("1試合あたり" = "p90", "合計" = "total"), selected = "p90"),
      selectizeInput("t4_teams", "推移を見るチーム（複数選択可）", choices = NULL, multiple = TRUE)
    ),
    card(
      card_header("かける数 × かかる数（チーム別・同一シーズン）"),
      plotOutput("t4_scatter", height = "640px")
    ),
    card(
      card_header("リーグ別・シーズン推移"),
      plotlyOutput("t4_league_trend", height = "460px")
    ),
    card(
      card_header("チーム別・シーズン推移"),
      plotlyOutput("t4_team_trend", height = "460px")
    ),
    card(
      card_header("チーム別 一覧"),
      DTOutput("t4_table")
    ),
    card(
      card_header("選手別 オフサイドにかかった数"),
      layout_columns(
        col_widths = c(6, 6),
        sliderInput("t4_min90", "最低出場（90分換算）", min = 0, max = 38, value = 10),
        sliderInput("t4_topn", "表示人数", min = 5, max = 50, value = 20)
      ),
      plotOutput("t4_player_bar", height = "560px"),
      DTOutput("t4_player_table")
    )
  ),

  # ---------------------------------------------------------------- 統合 --
  nav_panel(
    "統合分析（xG × オフサイド）",
    card(
      card_body(
        p("Understat の xG データと FBref のオフサイドデータをチーム名で突合し、",
          "「ラインの高さ」と守備・攻撃の関係を見ます。"),
        p(strong("※ 両サイトはチーム表記が異なるため、自動突合しています。"),
          "対応できなかったチームは下部の表で確認できます。")
      )
    ),
    selectInput("t5_season", "シーズン", choices = NULL),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("かける数 × 被xG（守備ライン）"), plotOutput("t5_def", height = "520px")),
      card(card_header("かかる数 × xG（攻撃の裏抜け）"), plotOutput("t5_att", height = "520px"))
    ),
    card(card_header("相関の要約"), verbatimTextOutput("t5_cor")),
    card(card_header("結合結果"), DTOutput("t5_table")),
    card(card_header("チーム名の突合結果"), DTOutput("t5_map"))
  ),

  # ------------------------------------------------------------- データ --
  nav_panel(
    "データ・出典",
    card(
      card_header("データのダウンロード"),
      card_body(
        downloadButton("dl_standings", "順位表 CSV"),
        downloadButton("dl_players",   "選手集計 CSV"),
        downloadButton("dl_offside",   "オフサイド CSV")
      )
    ),
    card(
      card_header("キャッシュの状況"),
      DTOutput("cache_table")
    ),
    card(
      card_header("出典と注意点"),
      card_body(HTML(paste0(
        "<ul>",
        "<li>試合結果・xG・シュート位置: <a href='https://understat.com/' target='_blank'>Understat</a>",
        "（<code>understat_league_match_results()</code> / <code>load_understat_league_shots()</code>）</li>",
        "<li>オフサイド: <a href='https://fbref.com/' target='_blank'>FBref</a>",
        "（<code>load_fb_big5_advanced_season_stats(stat_type = \"misc\")</code>）</li>",
        "<li>Understat の対応リーグは EPL / La liga / Bundesliga / Serie A / Ligue 1 / RFPL。",
        "FBref の misc 一括ロードはビッグ5リーグのみのため、RFPL のオフサイドは取得できません。</li>",
        "<li>期待勝点 (xPts) は Understat の勝敗確率から算出しています。</li>",
        "<li>選手集計はシュートデータからの算出のため、出場時間は含まれません",
        "（必要な場合は選手分析タブの取得ボタンを使用）。</li>",
        "<li>FBref は短時間に多数アクセスすると制限がかかります。キャッシュを活用してください。</li>",
        "</ul>"
      )))
    )
  )
)


# =============================================================================
#  Server
# =============================================================================
server <- function(input, output, session) {

  notify_fail <- function(msg) showNotification(msg, type = "error", duration = 8)

  seasons_sel <- reactive({
    req(input$seasons)
    seq.int(input$seasons[1], input$seasons[2])
  })

  # ---- Understat 試合結果 ---------------------------------------------------
  raw_results <- eventReactive(input$load, {
    lgs <- input$leagues
    if (!length(lgs)) { notify_fail("リーグを1つ以上選んでください。"); return(tibble()) }
    ss <- seq.int(input$seasons[1], input$seasons[2])

    withProgress(message = "Understat から試合結果を取得中", value = 0, {
      out <- purrr::map_dfr(seq_along(lgs), function(i) {
        incProgress(1 / length(lgs), detail = league_label(lgs[i]))
        us_results(lgs[i], ss)
      })
    })
    if (!nrow(out)) notify_fail("試合結果を取得できませんでした。ネットワークとリーグ／シーズンの指定を確認してください。")
    out
  }, ignoreNULL = FALSE)

  team_matches  <- reactive(tidy_results(raw_results()))
  tbl_standings <- reactive(standings(team_matches()))

  observeEvent(input$clear_cache, {
    n <- wfr_clear_cache()
    showNotification(sprintf("キャッシュを %d 件削除しました。", n), type = "message")
  })

  # 選択肢の更新
  observeEvent(tbl_standings(), {
    d <- tbl_standings()
    req(nrow(d) > 0)

    lgs <- sort(unique(d$league_understat))
    updateSelectInput(session, "t1_league",
                      choices = stats::setNames(lgs, league_label(lgs)),
                      selected = keep_sel(isolate(input$t1_league), lgs))

    ys <- sort(unique(d$season_start_year), decreasing = TRUE)
    ch <- stats::setNames(as.character(ys), season_label(ys))
    updateSelectInput(session, "t2_season", choices = ch, selected = ch[1])

    updateSelectizeInput(session, "t2_teams",
                         choices = sort(unique(d$team)),
                         selected = isolate(input$t2_teams))
  })

  observeEvent(list(input$t1_league, tbl_standings()), {
    d <- tbl_standings(); req(nrow(d) > 0, input$t1_league)
    ys <- d %>% dplyr::filter(league_understat == input$t1_league) %>%
      dplyr::pull(season_start_year) %>% unique() %>% sort(decreasing = TRUE)
    req(length(ys) > 0)
    updateSelectInput(session, "t1_season",
                      choices = stats::setNames(as.character(ys), season_label(ys)))
  })

  # ============================ タブ1：順位表 ================================
  t1_data <- reactive({
    d <- tbl_standings(); req(nrow(d) > 0, input$t1_league, input$t1_season)
    d %>% dplyr::filter(league_understat == input$t1_league,
                        season_start_year == as.integer(input$t1_season))
  })

  output$t1_table <- renderDT({
    d <- t1_data() %>%
      dplyr::select(順位, チーム = team, MP, W, D, L, Pts, G, GA, GD,
                    xG, xGA, xGD, xPts, `G-xG`, `GA-xGA`, `Pts-xPts`)
    datatable(d, rownames = FALSE, extensions = "Buttons",
              options = list(pageLength = 20, dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel"))) %>%
      formatStyle("G-xG", color = styleInterval(0, c("#c0392b", "#1e8449"))) %>%
      formatStyle("Pts-xPts", color = styleInterval(0, c("#c0392b", "#1e8449")))
  })

  output$t1_cum <- renderPlotly({
    st <- t1_data()
    tm <- team_matches() %>%
      dplyr::filter(league_understat == input$t1_league,
                    season_start_year == as.integer(input$t1_season))
    req(nrow(tm) > 0)
    top <- st %>% dplyr::slice_head(n = input$t1_topn) %>% dplyr::pull(team)

    g <- tm %>%
      dplyr::filter(team %in% top) %>%
      dplyr::mutate(ラベル = paste0(team, "<br>第", match_no, "節: ", cum_points, "点")) %>%
      ggplot2::ggplot(ggplot2::aes(match_no, cum_points, colour = team,
                                   group = team, text = ラベル)) +
      ggplot2::geom_line(linewidth = .7) +
      ggplot2::labs(x = "節", y = "累積勝点", colour = NULL,
                    title = paste0(league_label(input$t1_league), " ",
                                   season_label(input$t1_season))) +
      theme_wfr()
    ggplotly(g, tooltip = "text")
  })

  output$t1_xg_xga <- renderPlot({
    d <- t1_data(); req(nrow(d) > 0)
    ggplot2::ggplot(d, ggplot2::aes(xG, xGA, label = team)) +
      ggplot2::geom_abline(slope = 1, linetype = "dashed", colour = "grey60") +
      ggplot2::geom_point(ggplot2::aes(size = Pts), colour = "#2c7fb8", alpha = .8) +
      ggrepel::geom_text_repel(size = 3.4, max.overlaps = 30, family = jp_family()) +
      ggplot2::scale_size_continuous(range = c(2, 7), name = "勝点") +
      ggplot2::labs(x = "xG（期待得点）", y = "xGA（被期待得点）",
                    subtitle = "右下ほど強い（攻撃の期待値が高く、守備の被期待値が低い）") +
      theme_wfr()
  })

  output$t1_g_xg <- renderPlot({
    d <- t1_data(); req(nrow(d) > 0)
    ggplot2::ggplot(d, ggplot2::aes(xG, G, label = team)) +
      ggplot2::geom_abline(slope = 1, linetype = "dashed", colour = "grey60") +
      ggplot2::geom_point(ggplot2::aes(colour = `G-xG`), size = 3) +
      ggrepel::geom_text_repel(size = 3.4, max.overlaps = 30, family = jp_family()) +
      ggplot2::scale_colour_gradient2(low = "#c0392b", mid = "grey70", high = "#1e8449",
                                      midpoint = 0, name = "G - xG") +
      ggplot2::labs(x = "xG", y = "実得点",
                    subtitle = "対角線より上 = 決定力が期待値を上回る") +
      theme_wfr()
  })

  # ============================ タブ2：チーム比較 ============================
  output$t2_bar <- renderPlot({
    d <- tbl_standings(); req(nrow(d) > 0, input$t2_season, input$t2_metric)
    m <- input$t2_metric
    d2 <- d %>%
      dplyr::filter(season_start_year == as.integer(input$t2_season)) %>%
      dplyr::mutate(値 = .data[[m]],
                    リーグ = league_label(league_understat))
    req(nrow(d2) > 0)

    ggplot2::ggplot(d2, ggplot2::aes(stats::reorder(team, 値), 値, fill = 値)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~ リーグ, scales = "free_y") +
      ggplot2::scale_fill_gradient2(low = "#c0392b", mid = "grey80", high = "#1e8449",
                                    midpoint = stats::median(d2$値, na.rm = TRUE)) +
      ggplot2::labs(x = NULL, y = names(METRICS)[METRICS == m],
                    title = paste0(season_label(input$t2_season), " シーズン｜",
                                   names(METRICS)[METRICS == m]),
                    fill = NULL) +
      theme_wfr() + ggplot2::theme(legend.position = "none")
  })

  output$t2_trend <- renderPlotly({
    d <- tbl_standings(); req(nrow(d) > 0, input$t2_metric)
    m <- input$t2_metric
    sel <- input$t2_teams
    d2 <- d %>% dplyr::mutate(値 = .data[[m]], シーズン = season_label(season_start_year))
    if (length(sel)) d2 <- dplyr::filter(d2, team %in% sel)
    validate(need(nrow(d2) > 0, "チームを選んでください。"))

    g <- ggplot2::ggplot(d2, ggplot2::aes(season_start_year, 値, colour = team, group = team,
                                          text = paste0(team, "<br>", シーズン, "<br>",
                                                        names(METRICS)[METRICS == m], ": ",
                                                        round(値, 2)))) +
      ggplot2::geom_line(linewidth = .7) + ggplot2::geom_point(size = 1.6) +
      ggplot2::scale_x_continuous(breaks = sort(unique(d2$season_start_year)),
                                  labels = season_label(sort(unique(d2$season_start_year)))) +
      ggplot2::labs(x = NULL, y = names(METRICS)[METRICS == m], colour = NULL) +
      theme_wfr()
    ggplotly(g, tooltip = "text")
  })

  output$t2_table <- renderDT({
    d <- tbl_standings(); req(nrow(d) > 0)
    d %>%
      dplyr::transmute(リーグ = league_label(league_understat),
                       シーズン = season_label(season_start_year),
                       順位, チーム = team, MP, Pts, G, GA, GD, xG, xGA, xGD, xPts,
                       `G-xG`, `GA-xGA`) %>%
      datatable(rownames = FALSE, filter = "top",
                options = list(pageLength = 15, scrollX = TRUE))
  })

  # ============================ タブ3：選手分析 ==============================
  shots_data <- eventReactive(input$t3_load, {
    lgs <- input$leagues
    if (!length(lgs)) { notify_fail("リーグを選んでください。"); return(tibble()) }
    withProgress(message = "Understat のシュートデータを読み込み中", value = 0, {
      out <- purrr::map_dfr(seq_along(lgs), function(i) {
        incProgress(1 / length(lgs), detail = league_label(lgs[i]))
        us_shots(lgs[i])
      })
    })
    if (!nrow(out)) notify_fail("シュートデータを取得できませんでした。")
    out
  })

  observeEvent(shots_data(), {
    d <- shots_data(); req(nrow(d) > 0)
    lgs <- sort(unique(d$league_understat))
    updateSelectInput(session, "t3_league",
                      choices = stats::setNames(lgs, league_label(lgs)),
                      selected = lgs[1])
  })

  observeEvent(list(shots_data(), input$t3_league), {
    d <- shots_data(); req(nrow(d) > 0, input$t3_league)
    ys <- d %>% dplyr::filter(league_understat == input$t3_league) %>%
      dplyr::pull(season_start_year) %>% unique() %>% sort(decreasing = TRUE)
    req(length(ys) > 0)
    updateSelectInput(session, "t3_season",
                      choices = stats::setNames(as.character(ys), season_label(ys)))
  })

  t3_shots <- reactive({
    d <- tryCatch(shots_data(), error = function(e) tibble())
    validate(need(nrow(d) > 0, "上の「シュートデータを読み込む」を押してください。"))
    req(input$t3_league, input$t3_season)
    d %>% dplyr::filter(league_understat == input$t3_league,
                        season_start_year == as.integer(input$t3_season))
  })

  observeEvent(t3_shots(), {
    d <- t3_shots(); req(nrow(d) > 0)
    updateSelectizeInput(session, "t3_players",
                         choices = sort(unique(d$player)),
                         selected = isolate(input$t3_players), server = TRUE)
    updateSelectInput(session, "t3_team", choices = sort(unique(d$team)))
  })

  t3_stats <- reactive({
    d <- t3_shots(); req(nrow(d) > 0)
    player_shot_stats(d, by = c("player", "team"))
  })

  output$t3_table <- renderDT({
    d <- t3_stats(); req(nrow(d) > 0)
    d %>%
      dplyr::transmute(選手 = player, チーム = team, シュート = shots, 得点 = goals,
                       xG = round(xG, 2), `G-xG` = round(`G-xG`, 2),
                       決定率 = round(決定率, 3),
                       アシスト = assists, KP = key_passes, xA = round(xA, 2),
                       `G+A` = `G+A`, `xG+xA` = round(`xG+xA`, 2)) %>%
      datatable(rownames = FALSE, filter = "top", extensions = "Buttons",
                options = list(pageLength = 15, dom = "Bfrtip", scrollX = TRUE,
                               buttons = c("copy", "csv", "excel"),
                               order = list(list(3, "desc"))))
  })

  output$t3_g_xg <- renderPlot({
    d <- t3_stats(); req(nrow(d) > 0)
    d2 <- dplyr::filter(d, shots >= input$t3_minshots)
    validate(need(nrow(d2) > 0, "条件に合う選手がいません。最小シュート数を下げてください。"))
    sel <- input$t3_players
    d2 <- dplyr::mutate(d2, 強調 = player %in% sel)

    ggplot2::ggplot(d2, ggplot2::aes(xG, goals)) +
      ggplot2::geom_abline(slope = 1, linetype = "dashed", colour = "grey60") +
      ggplot2::geom_point(ggplot2::aes(colour = 強調, size = 強調), alpha = .85) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "grey65", `TRUE` = "#d7263d"), guide = "none") +
      ggplot2::scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 4), guide = "none") +
      ggrepel::geom_text_repel(
        data = dplyr::filter(d2, 強調 | goals >= sort(d2$goals, decreasing = TRUE)[min(10, nrow(d2))]),
        ggplot2::aes(label = player), size = 3.4, max.overlaps = 30, family = jp_family()) +
      ggplot2::labs(x = "xG", y = "得点",
                    subtitle = paste0(league_label(input$t3_league), " ",
                                      season_label(input$t3_season),
                                      "｜シュート", input$t3_minshots, "本以上")) +
      theme_wfr()
  })

  output$t3_shotmap <- renderPlot({
    d <- t3_shots(); sel <- input$t3_players
    validate(need(length(sel) > 0, "選手を選択するとシュートマップを表示します。"))
    d2 <- dplyr::filter(d, player %in% sel)
    validate(need(nrow(d2) > 0, "該当データがありません。"))

    gg_shotmap(d2,
               title = paste(sel, collapse = " / "),
               subtitle = paste0(league_label(input$t3_league), " ",
                                 season_label(input$t3_season),
                                 "｜シュート", nrow(d2), "本 / 得点", sum(d2$is_goal),
                                 " / xG ", round(sum(d2$xG), 1))) +
      ggplot2::facet_wrap(~ player)
  })

  output$t3_trend <- renderPlotly({
    d <- tryCatch(shots_data(), error = function(e) tibble())
    validate(need(nrow(d) > 0, "シュートデータを読み込んでください。"))
    sel <- input$t3_players
    validate(need(length(sel) > 0, "選手を選択するとシーズン推移を表示します。"))
    d2 <- d %>% dplyr::filter(player %in% sel)
    validate(need(nrow(d2) > 0, "該当データがありません。"))

    agg <- d2 %>%
      dplyr::group_by(player, season_start_year) %>%
      dplyr::summarise(得点 = sum(is_goal), xG = round(sum(xG), 2),
                       シュート = dplyr::n(), .groups = "drop") %>%
      tidyr::pivot_longer(c(得点, xG), names_to = "指標", values_to = "値")

    g <- ggplot2::ggplot(agg, ggplot2::aes(season_start_year, 値, colour = player,
                                           linetype = 指標, group = interaction(player, 指標),
                                           text = paste0(player, "<br>",
                                                         season_label(season_start_year), "<br>",
                                                         指標, ": ", 値))) +
      ggplot2::geom_line(linewidth = .7) + ggplot2::geom_point(size = 1.6) +
      ggplot2::scale_x_continuous(breaks = sort(unique(agg$season_start_year)),
                                  labels = season_label(sort(unique(agg$season_start_year)))) +
      ggplot2::labs(x = NULL, y = NULL, colour = NULL, linetype = NULL) +
      theme_wfr()
    ggplotly(g, tooltip = "text")
  })

  output$t3_hist <- renderPlot({
    d <- t3_shots(); req(nrow(d) > 0)
    if (input$t3_hist_scope == "player") {
      validate(need(length(input$t3_players) > 0, "選手を選択してください。"))
      d2 <- dplyr::filter(d, player %in% input$t3_players, is_goal)
      ttl <- paste(input$t3_players, collapse = " / ")
    } else {
      req(input$t3_team)
      d2 <- dplyr::filter(d, team == input$t3_team, is_goal)
      ttl <- input$t3_team
    }
    validate(need(nrow(d2) > 0, "該当する得点がありません。"))

    ggplot2::ggplot(d2, ggplot2::aes(minute)) +
      ggplot2::geom_histogram(binwidth = 5, fill = "#2c7fb8", colour = "white") +
      ggplot2::scale_x_continuous(breaks = seq(0, 90, 15)) +
      ggplot2::labs(x = "時間（分）", y = "得点数",
                    title = paste0(ttl, "｜得点の時間帯分布"),
                    subtitle = paste0(season_label(input$t3_season), " / 合計 ", nrow(d2), "点")) +
      theme_wfr()
  })

  t3_team_stats <- eventReactive(input$t3_load_team, {
    req(input$t3_team, input$t3_season)
    withProgress(message = "Understat チームページから取得中", value = .5, {
      us_team_players(input$t3_team, as.integer(input$t3_season))
    })
  })

  output$t3_team_table <- renderDT({
    d <- t3_team_stats()
    validate(need(!is.null(d) && nrow(d) > 0, "ボタンを押すと取得します。"))
    d %>%
      dplyr::transmute(選手 = player_name, ポジション = position,
                       試合 = games, 出場時間 = time, 得点 = goals,
                       xG = round(xG, 2), アシスト = assists, xA = round(xA, 2),
                       シュート = shots, KP = key_passes,
                       `90分あたり得点` = round(`90分あたり得点`, 2),
                       `90分あたりxG` = round(`90分あたりxG`, 2)) %>%
      datatable(rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE,
                                                 order = list(list(3, "desc"))))
  })

  # ========================== タブ4：オフサイド ==============================
  off_team <- eventReactive(input$t4_load, {
    withProgress(message = "FBref のオフサイドデータを読み込み中", value = .3, {
      m <- fb_misc("team")
      if (is.null(m)) { notify_fail("FBref データを取得できませんでした。"); return(tibble()) }
      tryCatch(offside_team_table(m), error = function(e) { notify_fail(conditionMessage(e)); tibble() })
    })
  })

  off_player <- eventReactive(input$t4_load, {
    withProgress(message = "FBref の選手別データを読み込み中", value = .7, {
      m <- fb_misc("player")
      if (is.null(m)) return(tibble())
      tryCatch(offside_player_table(m), error = function(e) tibble())
    })
  })

  observeEvent(off_team(), {
    d <- off_team(); req(nrow(d) > 0)
    ys <- sort(unique(d$season_start_year), decreasing = TRUE)
    ch <- stats::setNames(as.character(ys), season_label(ys))
    sel <- as.character(max(min(isolate(input$seasons)[2], max(ys)), min(ys)))
    updateSelectInput(session, "t4_season", choices = ch, selected = sel)
    updateSelectInput(session, "t5_season", choices = ch, selected = sel)
    updateSelectizeInput(session, "t4_teams", choices = sort(unique(d$squad)))
  })

  # 共通設定のリーグでフィルタ（ビッグ5のみ）
  off_team_f <- reactive({
    d <- tryCatch(off_team(), error = function(e) tibble())
    validate(need(nrow(d) > 0, "上の「オフサイドデータを読み込む」を押してください。"))
    lgs <- intersect(input$leagues, LEAGUES$understat[!is.na(LEAGUES$fb_country)])
    if (length(lgs)) d <- dplyr::filter(d, league_understat %in% lgs)
    validate(need(nrow(d) > 0, "選択中のリーグに対応するデータがありません（FBrefの一括ロードはビッグ5リーグのみ）。"))
    d
  })

  t4_season_data <- reactive({
    d <- off_team_f(); req(nrow(d) > 0, input$t4_season)
    dplyr::filter(d, season_start_year == as.integer(input$t4_season))
  })

  output$t4_scatter <- renderPlot({
    d <- t4_season_data()
    validate(need(nrow(d) > 0, "データがありません。選択中のリーグにビッグ5が含まれているか確認してください。"))
    p90 <- input$t4_basis == "p90"
    d2 <- d %>% dplyr::mutate(x = if (p90) かかる数_試合平均 else かかる数,
                              y = if (p90) かける数_試合平均 else かける数,
                              リーグ = league_label(league_understat))
    mx <- stats::median(d2$x, na.rm = TRUE); my <- stats::median(d2$y, na.rm = TRUE)
    unit <- if (p90) "（1試合あたり）" else "（シーズン合計）"

    ggplot2::ggplot(d2, ggplot2::aes(x, y, colour = リーグ, label = squad)) +
      ggplot2::geom_vline(xintercept = mx, linetype = "dashed", colour = "grey65") +
      ggplot2::geom_hline(yintercept = my, linetype = "dashed", colour = "grey65") +
      ggplot2::geom_point(size = 3, alpha = .85) +
      ggrepel::geom_text_repel(size = 3.3, max.overlaps = 40, show.legend = FALSE,
                               family = jp_family()) +
      ggplot2::labs(
        x = paste0("オフサイドにかかった数", unit),
        y = paste0("オフサイドにかけた数", unit),
        title = paste0(season_label(input$t4_season), " シーズン"),
        subtitle = "右上=お互い多い / 左上=かけるのみ多い（ハイライン守備） / 右下=かかるのみ多い（裏抜け主体の攻撃）",
        colour = NULL) +
      theme_wfr()
  })

  output$t4_league_trend <- renderPlotly({
    d <- off_team_f(); req(nrow(d) > 0)
    agg <- d %>%
      dplyr::group_by(league_understat, season_start_year) %>%
      dplyr::summarise(かける数 = round(mean(かける数_試合平均, na.rm = TRUE), 3),
                       かかる数 = round(mean(かかる数_試合平均, na.rm = TRUE), 3),
                       .groups = "drop") %>%
      tidyr::pivot_longer(c(かける数, かかる数), names_to = "指標", values_to = "値") %>%
      dplyr::mutate(リーグ = league_label(league_understat))

    g <- ggplot2::ggplot(agg, ggplot2::aes(season_start_year, 値, colour = リーグ,
                                           linetype = 指標,
                                           group = interaction(リーグ, 指標),
                                           text = paste0(リーグ, "<br>",
                                                         season_label(season_start_year), "<br>",
                                                         指標, ": ", 値, " / 試合"))) +
      ggplot2::geom_line(linewidth = .7) + ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_x_continuous(breaks = sort(unique(agg$season_start_year)),
                                  labels = season_label(sort(unique(agg$season_start_year)))) +
      ggplot2::labs(x = NULL, y = "1試合あたり回数", colour = NULL, linetype = NULL,
                    title = "リーグ平均の推移") +
      theme_wfr()
    ggplotly(g, tooltip = "text")
  })

  output$t4_team_trend <- renderPlotly({
    d <- tryCatch(off_team(), error = function(e) tibble())
    validate(need(nrow(d) > 0, "オフサイドデータを読み込んでください。"))
    sel <- input$t4_teams
    validate(need(length(sel) > 0, "チームを選択すると推移を表示します。"))
    agg <- d %>%
      dplyr::filter(squad %in% sel) %>%
      dplyr::transmute(squad, season_start_year,
                       かける数 = round(かける数_試合平均, 3),
                       かかる数 = round(かかる数_試合平均, 3)) %>%
      tidyr::pivot_longer(c(かける数, かかる数), names_to = "指標", values_to = "値")

    g <- ggplot2::ggplot(agg, ggplot2::aes(season_start_year, 値, colour = squad,
                                           linetype = 指標,
                                           group = interaction(squad, 指標),
                                           text = paste0(squad, "<br>",
                                                         season_label(season_start_year), "<br>",
                                                         指標, ": ", 値, " / 試合"))) +
      ggplot2::geom_line(linewidth = .7) + ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_x_continuous(breaks = sort(unique(agg$season_start_year)),
                                  labels = season_label(sort(unique(agg$season_start_year)))) +
      ggplot2::labs(x = NULL, y = "1試合あたり回数", colour = NULL, linetype = NULL) +
      theme_wfr()
    ggplotly(g, tooltip = "text")
  })

  output$t4_table <- renderDT({
    d <- off_team_f(); req(nrow(d) > 0)
    d %>%
      dplyr::transmute(リーグ = league_label(league_understat),
                       シーズン = season_label(season_start_year),
                       チーム = squad, 試合数 = round(試合数, 1),
                       かける数, かかる数,
                       `かける数/試合` = round(かける数_試合平均, 2),
                       `かかる数/試合` = round(かかる数_試合平均, 2),
                       `差分/試合` = round(差分_試合平均, 2),
                       トラップ比率 = round(トラップ比率, 3)) %>%
      datatable(rownames = FALSE, filter = "top", extensions = "Buttons",
                options = list(pageLength = 20, dom = "Bfrtip", scrollX = TRUE,
                               buttons = c("copy", "csv", "excel")))
  })

  t4_player_data <- reactive({
    d <- tryCatch(off_player(), error = function(e) tibble())
    validate(need(nrow(d) > 0, "オフサイドデータを読み込んでください。"))
    req(input$t4_season)
    lgs <- intersect(input$leagues, LEAGUES$understat[!is.na(LEAGUES$fb_country)])
    d <- dplyr::filter(d, season_start_year == as.integer(input$t4_season),
                       nineties >= input$t4_min90)
    if (length(lgs)) d <- dplyr::filter(d, league_understat %in% lgs)
    d
  })

  output$t4_player_bar <- renderPlot({
    d <- t4_player_data()
    validate(need(nrow(d) > 0, "条件に合う選手がいません。"))
    d2 <- d %>% dplyr::arrange(dplyr::desc(かかる数)) %>% dplyr::slice_head(n = input$t4_topn)

    ggplot2::ggplot(d2, ggplot2::aes(stats::reorder(player, かかる数), かかる数,
                                     fill = かかる数_90分)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient(low = "#a6cee3", high = "#d7263d", name = "90分あたり") +
      ggplot2::labs(x = NULL, y = "オフサイドにかかった数",
                    title = paste0(season_label(input$t4_season), " シーズン｜選手別"),
                    subtitle = paste0("出場 ", input$t4_min90, " （90分換算）以上")) +
      theme_wfr()
  })

  output$t4_player_table <- renderDT({
    d <- t4_player_data()
    validate(need(nrow(d) > 0, "条件に合う選手がいません。"))
    d %>%
      dplyr::transmute(選手 = player, チーム = squad, リーグ = league_label(league_understat),
                       ポジション = pos, `出場(90分換算)` = round(nineties, 1),
                       かかる数, `かかる数/90分` = round(かかる数_90分, 3)) %>%
      datatable(rownames = FALSE, filter = "top",
                options = list(pageLength = 15, scrollX = TRUE,
                               order = list(list(5, "desc"))))
  })

  # ============================ タブ5：統合分析 ==============================
  joined <- reactive({
    st <- tbl_standings()
    ot <- tryCatch(off_team(), error = function(e) tibble())
    validate(need(nrow(ot) > 0, "「オフサイド分析」タブでデータを読み込んでください。"))
    validate(need(nrow(st) > 0, "試合結果データがありません。"))
    req(input$t5_season)
    join_offside_with_xg(
      dplyr::filter(st, season_start_year == as.integer(input$t5_season)),
      ot
    )
  })

  output$t5_def <- renderPlot({
    d <- joined() %>% dplyr::filter(!is.na(かける数_試合平均))
    validate(need(nrow(d) > 0, "結合できたデータがありません（オフサイドデータを読み込んでください）。"))
    ggplot2::ggplot(d, ggplot2::aes(かける数_試合平均, xGA_試合平均, label = team)) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "grey70", linewidth = .6) +
      ggplot2::geom_point(ggplot2::aes(colour = league_label(league_understat)), size = 3) +
      ggrepel::geom_text_repel(size = 3.2, max.overlaps = 40, family = jp_family()) +
      ggplot2::labs(x = "オフサイドにかけた数 / 試合", y = "被xG / 試合", colour = NULL,
                    subtitle = "左下＝トラップに頼らず失点機会も少ない / 右上＝ハイラインでリスクも大きい") +
      theme_wfr()
  })

  output$t5_att <- renderPlot({
    d <- joined() %>% dplyr::filter(!is.na(かかる数_試合平均))
    validate(need(nrow(d) > 0, "結合できたデータがありません。"))
    ggplot2::ggplot(d, ggplot2::aes(かかる数_試合平均, xG_試合平均, label = team)) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, colour = "grey70", linewidth = .6) +
      ggplot2::geom_point(ggplot2::aes(colour = league_label(league_understat)), size = 3) +
      ggrepel::geom_text_repel(size = 3.2, max.overlaps = 40, family = jp_family()) +
      ggplot2::labs(x = "オフサイドにかかった数 / 試合", y = "xG / 試合", colour = NULL,
                    subtitle = "右上＝裏抜けを多用しつつチャンスも作れている") +
      theme_wfr()
  })

  output$t5_cor <- renderPrint({
    d <- joined()
    validate(need(nrow(d) > 0, "データがありません。"))
    f <- function(x, y) {
      ok <- stats::complete.cases(x, y)
      if (sum(ok) < 3) return(NA_real_)
      round(stats::cor(x[ok], y[ok]), 3)
    }
    cat("対象:", season_label(input$t5_season), "シーズン /", nrow(d), "チーム\n\n")
    cat("かける数(試合平均) と 被xG(試合平均) の相関 :", f(d$かける数_試合平均, d$xGA_試合平均), "\n")
    cat("かける数(試合平均) と 勝点             の相関 :", f(d$かける数_試合平均, d$Pts), "\n")
    cat("かかる数(試合平均) と xG(試合平均)     の相関 :", f(d$かかる数_試合平均, d$xG_試合平均), "\n")
    cat("かかる数(試合平均) と 得点             の相関 :", f(d$かかる数_試合平均, d$G), "\n\n")
    cat("※ 相関は因果を意味しません。サンプル数が少ない点にも注意してください。\n")
  })

  output$t5_table <- renderDT({
    d <- joined(); validate(need(nrow(d) > 0, "データがありません。"))
    d %>%
      dplyr::transmute(リーグ = league_label(league_understat), 順位, チーム = team,
                       `FBref表記` = fb_name, 突合 = 方法,
                       Pts, xG = round(xG, 1), xGA = round(xGA, 1),
                       `かける数/試合` = round(かける数_試合平均, 2),
                       `かかる数/試合` = round(かかる数_試合平均, 2),
                       トラップ比率 = round(トラップ比率, 3)) %>%
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE))
  })

  output$t5_map <- renderDT({
    d <- joined(); validate(need(nrow(d) > 0, "データがありません。"))
    d %>%
      dplyr::transmute(`Understat表記` = team, `FBref表記` = fb_name, 突合方法 = 方法, 距離) %>%
      dplyr::arrange(dplyr::desc(is.na(`FBref表記`)), 突合方法) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10))
  })

  # ============================ タブ6：データ ================================
  output$cache_table <- renderDT({
    input$clear_cache; input$load; input$t3_load; input$t4_load
    datatable(wfr_cache_info(), rownames = FALSE, options = list(pageLength = 10))
  })

  output$dl_standings <- downloadHandler(
    filename = function() paste0("standings_", Sys.Date(), ".csv"),
    content = function(file) readr::write_excel_csv(tbl_standings(), file)
  )
  output$dl_players <- downloadHandler(
    filename = function() paste0("players_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- try(t3_stats(), silent = TRUE)
      if (inherits(d, "try-error") || is.null(d)) d <- tibble()
      readr::write_excel_csv(d, file)
    }
  )
  output$dl_offside <- downloadHandler(
    filename = function() paste0("offside_", Sys.Date(), ".csv"),
    content = function(file) {
      d <- try(off_team(), silent = TRUE)
      if (inherits(d, "try-error") || is.null(d)) d <- tibble()
      readr::write_excel_csv(d, file)
    }
  )
}

shinyApp(ui, server)
