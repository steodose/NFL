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
  **That heuristic only holds within one machine.** macOS writes RGBA + an `iCCP`
  colour profile + `eXIf`; Linux/CI writes plain RGB + `pHYs`. Identical charts, and
  the CI copy is about *half* the bytes — so every local↔CI comparison trips the rule
  for no reason. Before chasing one, check the encoding:
  `python3 -c "import struct;d=open(f,'rb').read();print(struct.unpack('>IIBBBBB',d[16:29]))"`
  (colour type 6 = RGBA, 2 = RGB) — if the colour types differ, that alone explains it.
- **Image axis labels need `fig.height` ~10 at 32 rows.** `nflplotR::element_nfl_logo()`
  and `element_path()` silently drop roughly a quarter of the images at the YAML
  default of 7 — no warning, the axis just comes up patchy. Set `fig.height = 10` on
  any per-team chunk and size the images ~0.95.
- **Team helmets live in `helmets/`** as `<ABBR>_left.png` / `<ABBR>_right.png`, all 32
  abbreviations matching nflfastR `posteam` exactly. Draw them with
  `nflplotR::element_path()`, which renders axis text that *is* a file path as an
  image: make the y aesthetic the path (`file.path("helmets", paste0(posteam,
  "_right.png"))`) and let `fct_reorder()` carry the ordering. Paths resolve at render
  time, so `helmets/` must stay committed or CI renders a blank axis.
- **Never map `colour` in a `geom_text()` layer that uses `position_stack()`.** ggplot
  groups by the interaction of all discrete aesthetics, so a mapped colour regroups
  that layer and stacks the labels in a different order from the bars — each team's
  label lands in the wrong segment, with no error. Compute positions instead:
  `group_by(team) |> arrange(<fill factor>) |> mutate(label_x = cumsum(share) - share/2)`.
- **Webfonts must be loaded by the page, not assumed installed.** `header.html`
  pulls Titillium Web from Google Fonts and `_site.yml` includes it in every
  page's head. Before that, the reactable themes asked for
  `fontFamily = "Titillium Web"` and no page ever loaded the family, so it
  resolved only against installed system fonts. Safari restricts CSS access to
  locally installed fonts as a fingerprinting defence, so every reactable table
  fell back to the default serif and rendered in Times -- on machines that *do*
  have the font installed. Chrome hid the bug. gt tables were unaffected because
  `opt_table_font(google_font(...))` emits its own `@import`. Any new font the
  site uses needs adding to `header.html`, and changing that file means
  re-rendering every page for the new head to appear.

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

## CLAUDE.html

`render_site()` renders every root-level `.md`, including this file, and
`_site.yml`'s `exclude:` does not stop it -- that key only filters which resources
get *copied* to the output dir, while `input_files()` picks up any `.md` not
prefixed with `_`. So `CLAUDE.html` is regenerated on every full render. It is
listed in `.gitignore` instead, which keeps it off GitHub Pages because the
scheduled refresh commits with `git add -A`. Do not commit it.

## Series data (`series_result`)

A series ends when the offense gains a first down **or** the possession ends, so one
drive contributes several series — ~30 per team per game, not ~12.

Take one row per series from its **first play**
(`group_by(game_id, series) |> slice_min(play_id, with_ties = FALSE)`).
`distinct(game_id, posteam, series, series_result)` looks equivalent and is not: after
a defensive touchdown the extra-point play carries the **scoring** team as `posteam`
while keeping the same `series` number and `series_result` ("Opp touchdown"), so the
series is emitted twice and the team that just scored is charged with a failed
offensive series. It is only a handful of series a week, so it will not look wrong.

`QB kneel` and `End of half` are not genuine offensive attempts; drop them.

## Forecast simulation determinism

`set.seed(8236)` immediately precedes `simulate_nfl(simulations = 10000)`, and it
works — a re-render with an unchanged schedule reproduces both prediction CSVs
byte-for-byte. So the numbers move only when an **input** moves, and the only remote
input is `load_schedules()`, refetched every render.

Only games with `is.na(result)` are simulated (255 of 272 as of Week 2). When a game
finishes it leaves that set, which both changes the starting conditions *and* shifts
every later draw's position in the RNG stream — so **all 32 teams' odds move, not just
the two who played**. A full rewrite of `Latest Game Predictions.csv` after a slate is
correct behaviour, not drift.

Two quieter sources: a schedule revision with no new results still moves things (the
Elo model reads `home_rest - away_rest`), and CI installs packages fresh, so an
`nflseedR` update changes results with identical seed *and* identical data.

## Season rollover

Bump the year in `NFL Summary Report.Rmd`, `Teams Weekly Report.Rmd`,
`Players Weekly Report.Rmd` (`load_pbp()`, season filters, chart titles) and in
`Forecast Simulations.Rmd` (`nfl_season`, the `load_schedules()` call feeding
`current_week`, and the table titles), and point Teams Weekly Report at that
season's `preseason_win_totals_<year>.csv` — it is fetched over HTTP from
raw.githubusercontent on `master`, so the file must be committed and pushed first or
the render 404s.
