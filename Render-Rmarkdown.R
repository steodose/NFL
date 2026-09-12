#### Render Rmarkdown Script for NFL Analytics Website ####

## run this script to render the specified markdown files

# List files with the specified names
files <- list.files(pattern = "^(Games\\.Rmd|NFL Summary Report\\.Rmd|Teams Weekly Report\\.Rmd|Players Weekly Report\\.Rmd|Forecast Simulations\\.Rmd)$")

# List files but without sims page
files <- list.files(pattern = "^(Games\\.Rmd|NFL Summary Report\\.Rmd|Teams Weekly Report\\.Rmd|Players Weekly Report\\.Rmd)$")

# Render function
for (f in files) rmarkdown::render(f)