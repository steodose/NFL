#!/bin/bash

echo "Rendering the website..."

Rscript -e "rmarkdown::render_site()"

if [[ "$(git status --porcelain)" != "" ]]; then
    git config --global user.name 'Stephan'
    git config --global user.email 'steodose@gmail.com'
    git add *
    git commit -m "Auto update website"
    git push
fi