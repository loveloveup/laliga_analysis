# 欧州サッカー インタラクティブ分析ツール

`reports/seasons/laliga_24-25.Rmd` を、**リーグ・シーズン・チーム・選手を自由に切り替えて分析できる形**に作り替えたものです。
あわせて、ご要望の **オフサイドにかける数 / かかる数** の分析を追加しています。

データソース

- Understat <https://understat.com/>（試合結果・xG・シュート位置）
- FBref <https://fbref.com/>（オフサイド）
- 取得は [worldfootballR](https://jaseziv.github.io/worldfootballR/articles/extract-understat-data.html) 経由

---

## フォルダ構成

```
laliga_analysis/
├── app/app.R              Shiny アプリ本体
├── R/wfr_helpers.R        データ取得・整形・作図の共通関数(アプリとレポートが共有)
├── reports/               レポートのソース(Rmd)
│   ├── index.Rmd          公開トップ(→ docs/index.html)
│   ├── template.Rmd       パラメータでシーズン・選手を切り替える HTML レポート
│   ├── interactive.Rmd    Shiny 不要のインタラクティブ HTML レポート
│   └── seasons/           シーズン別の Rmd(laliga_22-23 / 23-24 / 24-25)
├── assets/fbx/            interactive.Rmd 用の JS / CSS
├── data/
│   ├── raw/               手作業で用意した元データ(監督の xlsx など)
│   └── cache/             取得済みデータの保存先(自動生成・git 管理外)
├── docs/                  公開用 HTML(GitHub Pages の公開元)
└── scripts/               補助スクリプト(render_docs.R など)
```

どのフォルダから実行しても、`R/wfr_helpers.R` を目印にプロジェクトのルートを自動で探します。

公開用 HTML の更新: `source("scripts/render_docs.R")` で `docs/` に書き出し、コミットして push します。
GitHub Pages の公開元は **main ブランチの /docs** に設定してください。

## セットアップ

```r
install.packages(c("shiny", "bslib", "tidyverse", "ggrepel",
                   "plotly", "DT", "scales", "systemfonts"))

install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR")   # CRAN版は古いので GitHub版を使う
```

## 起動

```r
# プロジェクトのルートで実行
shiny::runApp("app")
```

左のサイドバーでリーグ（複数選択可）とシーズン範囲を指定し、**「データ取得 / 更新」**を押します。
取得したデータは `data/cache/` に保存され、次回以降は瞬時に読み込まれます。

---

## タブの内容

### 1. 順位表・チーム概要
順位表（実績 + xG/xGA/期待勝点）、累積勝点の推移、xG–xGA 散布図、得点–xG 散布図。
元Rmdのランキング部分に相当しますが、リーグとシーズンをプルダウンで切り替えられます。

### 2. チーム比較
勝点・xGD・決定力など11指標から選び、**同一シーズンのチーム別比較**（リーグごとにファセット）と、
**チームごとのシーズン推移**を表示します。

### 3. 選手分析
「シュートデータを読み込む」を押すと、そのリーグの 2014/15 以降**全シーズン**のシュートデータが入ります。

- 選手ランキング（シュート・得点・xG・決定率・KP・xA・アシスト）
- 得点 vs xG 散布図（選択した選手を赤で強調）
- シュートマップ（ピッチ上に xG の大きさで表示）
- 選択した選手のシーズン推移
- 得点の時間帯分布（チーム / 選手を切替）
- 出場時間つきスタッツ（必要なときだけチームページから取得）

選手名は検索ボックスに入力して選べます。複数人の同時比較も可能です。

### 4. オフサイド分析 ★追加機能
FBref の misc（その他）スタッツを使います。

| 指標 | 定義 | FBref上の取得元 |
|---|---|---|
| **かかる数** | 自チームの選手がオフサイドを取られた回数 | `Team_or_Opponent == "team"` の `Off` |
| **かける数** | 相手をオフサイドに掛けた回数（トラップ成立） | `Team_or_Opponent == "opponent"` の `Off` |
| **トラップ比率** | かける数 ÷（かける数 + かかる数） | ― |

表示内容

- **かける数 × かかる数の散布図**（同一シーズン・全リーグのチームを一枚に）
  - 左上＝かけるだけ多い（ハイラインの守備）
  - 右下＝かかるだけ多い（裏抜け主体の攻撃）
- **リーグ別のシーズン推移**（リーグ間の差、VAR導入前後の変化などが見えます）
- **チーム別のシーズン推移**（任意のチームを重ねて比較）
- **選手別ランキング**（個人の「かかる数」。「かける数」は個人に帰属しないため守備側は個人集計できません）

### 5. 統合分析（xG × オフサイド）
Understat と FBref をチーム名で突合し、

- かける数 × 被xG → ハイラインとリスクの関係
- かかる数 × xG → 裏抜けの多さとチャンス創出の関係

を散布図と相関係数で確認します。両サイトはチーム表記が違うため（例: `Wolverhampton Wanderers` と `Wolves`）、
手動辞書＋類似度による自動突合を行い、結果を「チーム名の突合結果」表で確認できます。

### 6. データ・出典
CSV ダウンロード、キャッシュの状況確認、出典と注意点。

---

## レポート（Rmd）版の使い方

元のRmdに近い体裁の HTML レポートを、パラメータを変えるだけで何度でも出せます。

```r
rmarkdown::render(
  "reports/template.Rmd",
  params = list(
    league            = "EPL",          # "EPL" / "La liga" / "Bundesliga" / "Serie A" / "Ligue 1" / "RFPL"
    season_start_year = 2025,           # 2025 なら 2025/26 シーズン
    compare_seasons   = 2020:2025,
    focus_team        = "Liverpool",
    focus_players     = c("Mohamed Salah", "Cody Gakpo")
  ),
  output_file = "epl_2025.html"
)
```

RStudio なら Knit ボタン横の **「Knit with Parameters…」** からも指定できます。

---

## 元のRmdから直した点

| 箇所 | 元の状態 | 対応 |
|---|---|---|
| シュートデータのシーズン | 試合結果は `2024`、シュートは `2025` を指定していて別シーズンが混在 | シーズンを一元管理し、必ず同じシーズンを参照 |
| 出場時間 | シュートの発生分（`minute`）を合計しており出場時間ではなかった | Understat チームページの `time`（出場分数）を使用 |
| `understat_team_players_stats` | ラリーガの分析にリヴァプール／マンCを混ぜていた | 対象リーグのチームから選択する形に変更 |
| 選手の個人スタッツ | FBref の選手URLを9人分ベタ書き | 選手名を選ぶだけで切り替え可能に |
| 再実行 | 実行のたびにスクレイピング | ディスクキャッシュで高速化（進行中シーズンは6時間で自動更新） |

---

## 注意点

- **FBref はアクセス制限があります。** 短時間に何度も読み込まず、キャッシュを活用してください。
  本ツールは可能な限り事前スクレイプ済みデータ（`load_*` 系関数）を使っています。
- **FBref の一括ロードはビッグ5リーグのみ**です。RFPL のオフサイドは取得できません。
  他リーグが必要な場合は `fb_season_team_stats(country = "NED", gender = "M", season_end_year = 2025, tier = "1st", stat_type = "misc")` で個別に取得できます。
- **試合単位のオフサイド**を見たい場合は、`fb_team_match_log_stats(team_urls = ..., stat_type = "misc")` を使うと
  1試合ごとのログが取れます（スクレイピングのため時間がかかります）。
- グラフ内の日本語が文字化けする場合は、日本語フォント（Noto Sans JP 等）をインストールしてください。
  `R/wfr_helpers.R` の `jp_family()` が自動で候補を探します。
- xPts（期待勝点）は Understat の勝敗確率から算出しています。
- 相関はあくまで相関であり、因果関係を示すものではありません。
