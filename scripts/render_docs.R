# 公開用 HTML(GitHub Pages)を docs/ に書き出す。プロジェクトのルートで実行する:
#   source("scripts/render_docs.R")
#
# docs/index.html と docs/laliga_22-23.html は Rmd と同名の出力なので、
# Rmd のファイル名を変えると公開 URL が変わる点に注意。
rmarkdown::render("reports/index.Rmd",                    output_dir = "docs")
rmarkdown::render("reports/seasons/laliga_22-23.Rmd",     output_dir = "docs")
