# GlobalGeoTree data-visualisation project

Semester group project for a data-visualisation course. The report explores source bias, temporal patterns, worldwide coverage, taxonomic composition, Germany and Berlin, and the United States through a deliberately broad range of visualisation techniques.

## Data

The complete observation file is too large to track in Git. Place it at:

```text
data/GlobalGeoTree.csv
```

The report expects these columns: `sample_id`, `country_code`, `level0`, `level1_family`, `level2_genus`, `level3_species`, `location`, `source`, `species_key`, `year`, `longitude`, and `latitude`.

Small evaluation samples are committed in `data/`, but the merged report is configured to use the complete file above. Cached geographic boundaries and species common names are stored in `data/external/`.

## Requirements

- R with the packages `data.table`, `dplyr`, `ggplot2`, `ggridges`, `jsonlite`, `knitr`, `kableExtra`, `maps`, `purrr`, `ragg`, `rnaturalearth`, `scales`, `sf`, `stringr`, `tibble`, `tidyr`, `vegan`, and `viridisLite`
- Quarto
- A LaTeX installation for PDF output

## Render the report

Run the commands from the repository root so relative data paths resolve correctly:

```sh
quarto render merged/merged_report.qmd --to html
quarto render merged/merged_report.qmd --to pdf
```

The generated outputs are `merged/merged_report.html` and `merged/merged_report.pdf`.

The report uses fixed random seeds for sampled point and density maps. Aggregate counts, rankings, richness summaries, and rarefaction analyses use the complete relevant subset.
