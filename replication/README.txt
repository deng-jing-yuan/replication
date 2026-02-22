Replication Package
===================

"The intensive margin of polygyny matters for its effects on unmarried men"
Letter responding to Gaddy, Sear & Fortunato (2025, PNAS)


Data
----
The raw microdata are from IPUMS International (https://international.ipums.org/).
Because the census microdata are too large to distribute, we provide the
collapsed locality-level datasets used for regression analysis.

  data/locality_fig1.csv  -- 6,125 subnational localities x 12 variables
                             Used for per-census OLS regressions (Figure 1)

  data/locality_fig2.csv  -- 6,125 subnational localities x 16 variables
                             Used for pooled FE regressions (Figure 2)

Variables are constructed from IPUMS International census microdata for
64 censuses across 27 countries (1969-2019). Each row is a GEOLEV2
(second-level administrative) unit within a census-year.

Key variables:
  census_id     -- country code + year (e.g., "50_1991" = Bangladesh 1991)
  census_label  -- country name + year
  wh_ratio      -- ratio of married women to married men (age 20+)
  single_m20s   -- share of men 20-29 who are never married
  single_XXYY   -- share of men in age bin XX-YY who are never married
  avg_educ      -- weighted mean years of schooling (age 25+)
  log_popdens   -- log population density (IPUMS POPDENSGEO2)
  sex_ratio     -- ratio of men to women (age 20+)


Requirements
------------
R (tested on 4.4.3) with packages:
  - data.table
  - ggplot2
  - gridExtra
  - grid (base)


Reproducing the figures
-----------------------
Set the working directory to this folder, then run:

  Rscript fig1_forest.R       # -> output/fig1_forest.png   (Figure 1)
  Rscript fig2_pooled_fe.R    # -> output/fig2_pooled_fe.png (Figure 2)

Output is written to the output/ subfolder.


Figure descriptions
-------------------
Figure 1: Per-census standardized OLS coefficients of the wife-husband ratio
on the share of never-married men aged 20-29, across 64 censuses. Left panel:
bivariate. Right panel: with controls (average education, log population
density, adult sex ratio). Each panel is independently sorted by coefficient.

Figure 2: Pooled OLS coefficients (with census fixed effects) of the
wife-husband ratio on the share of never-married men, by 5-year age group
from 15-19 to 45-49. Left panel: bivariate. Right panel: with controls.
