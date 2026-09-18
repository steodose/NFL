# NFL Analytics site

R Markdown website published at <https://steodose.github.io/NFL>. GitHub Pages
serves it from the **root of `master`**, so the rendered `.html`, `site_libs/` and
`*_files/` are committed deliberately — they are the published artifact, not build
litter. `_site.yml` sets `output_dir: "."`, which is why output lands beside source.

Do not work from `~/Desktop/R Projects/NFL`. That is an iCloud-synced archive of
older one-off analyses; its file contents are evicted, which makes `du` report
wrong sizes and git index operations hang or time out.

## Rendering

```r
rmarkdown::render_site()              # whole site
rmarkdown::render_site("Games.Rmd")   # one page, correctly
```

**Never use bare `rmarkdown::render()` on a page here.** It ignores `_site.yml`,
defaults to `self_contained: true`, and *deletes* the page's `_files/` directory —
a single-file render produced a 7.7MB self-contained page and dropped the figures.
The RStudio Knit button is fine: `NFL.Rproj` sets `BuildType: Website`, so it
routes through `render_site()`.

Sanity check after rendering: pages stay in the tens-to-hundreds of KB and
reference `site_libs`. A jump to megabytes means a self-contained render.

## Gotchas that cost real debugging time

- **`theme_minimal()` breaks image-based axis labels** under ggplot2 4.0. Both
  `ggtext::element_markdown()` and `nflplotR::element_nfl_logo()` silently degrade —
  no error, just missing logos or raw HTML as axis text. Every other base theme
  (grey, bw, light, classic, linedraw) works. `theme_custom()` is therefore built on
  `theme_bw` and stripped back to match theme_minimal's appearance. Keep it that way.
- **Failures are silent.** Every Rmd sets `warning = FALSE`, so broken charts render
  clean and publish with a green checkmark. After changing chart code, compare figure
  file sizes against the previous render — a >50% swing means something broke.
- **`dpi:` in the YAML headers is inert.** `html_document`'s `fig_retina: 2` overrides
  it; figures are 1920x1344 shown at 960px, a deliberate 2x for Retina. Changing `dpi`
  does nothing. Do not "optimise" it.

## Adding an R package

Add it to `packages:` in `.github/workflows/refresh-site.yml`. CI installs with
`dependencies: '"hard"'` (Depends/Imports/LinkingTo only), because pulling Suggests
drags in `dataui`, a GitHub-only package that aborts the whole pak solve.

Consequence: anything a package loads **lazily** must be named explicitly — it will
work locally and fail in CI. That is how `svglite` (gtExtras' `gt_plt_*` render via
`ggsave(.svg)`) and `crosstalk` (reactable) were missed. To find the full set:

```r
rmarkdown::render_site(); writeLines(sort(loadedNamespaces()), "ns.txt")
```

then diff against `tools::package_dependencies(<the list>, which =
c("Depends","Imports","LinkingTo"), recursive = TRUE)`.

## Scheduled refresh

`.github/workflows/refresh-site.yml` renders and commits at 03:00 America/Los_Angeles
on Mon, Tue and Fri (after Sunday, MNF and TNF). GitHub cron is UTC-only, so both
10:00 and 11:00 UTC are registered and a `gate` job drops whichever one is not
03:00 Pacific today — that is what keeps it at one run per day across the November
DST change. Manual `workflow_dispatch` bypasses the gate entirely.

The gate must decide from `github.event.schedule` (the cron string that fired),
**never** from the wall clock. GitHub starts these runs hours after their nominal
time — 10:00 UTC crons have started at 14:06 and 14:41 UTC — so a
`TZ=America/Los_Angeles date +%H` = 03 test is false for *both* candidates and
silently drops the entire day's refresh. The symptom is a 2-second `gate` job and
a skipped `render`, with the run still reported green.

The push step rebases onto a moved `master` and retries, so a concurrent push is not
lost. Repo permissions must be Settings → Actions → General → **Read and write**.

`Forecast Simulations.Rmd` rewrites `Latest Game Predictions.csv` and
`Overall Predictions.csv` on every render, so those are committed too. It simulates
only games where `is.na(result)`, so completed games are picked up automatically —
no week needs hardcoding there.

## Season rollover

Bump the year in `NFL Summary Report.Rmd`, `Teams Weekly Report.Rmd`,
`Players Weekly Report.Rmd` (`load_pbp()`, season filters, chart titles), set
`current_week` in `Current Week Odds.Rmd`, and point Teams Weekly Report at that
season's `preseason_win_totals_<year>.csv` — it is fetched over HTTP from
raw.githubusercontent on `master`, so the file must be committed and pushed first or
the render 404s.
