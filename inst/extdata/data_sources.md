# Data sources and attribution

This package uses small example datasets for demonstration and testing. These examples are simplified and/or synthetic unless explicitly stated otherwise. Users should download authoritative source data directly from the original providers when reproducing real analyses.

## Australian Bureau of Statistics geography

Some examples are based on Australian Statistical Geography Standard (ASGS) geography concepts, including Local Government Areas.

Source:

Australian Bureau of Statistics. Australian Statistical Geography Standard (ASGS) Edition 3, July 2021 to June 2026. Digital boundary files.

ABS digital boundaries are available in OGC GeoPackage and Esri Shapefile formats.

Reference page:

https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files

## Australian Bureau of Statistics Census DataPacks

Some examples use fields derived from the 2021 Census General Community Profile, including total population fields for Local Government Areas.

Source:

Australian Bureau of Statistics. 2021 Census DataPacks. General Community Profile, Local Government Areas.

Reference page:

https://www.abs.gov.au/census/find-census-data/datapacks

The 2021 Census DataPacks page includes General Community Profile downloads for Local Government Areas, including Victoria.

## H3 / hexagonal indexing

Hexbin examples use H3-style hexagonal spatial indexing concepts.

H3 is a hierarchical hexagonal geospatial indexing system originally developed by Uber.

R workflows may use packages such as `h3jsr`, which provides access to Uber's H3 library via `h3-js` and V8.

References:

https://cran.r-project.org/package=h3jsr

https://obrl-soil.github.io/h3jsr/

## Synthetic example data

Synthetic example point and hexbin data included with this package were generated with fixed random seeds for reproducibility.

Synthetic data is not real incident, demographic, census, visitor safety, or operational data.

Generation scripts are stored in:

`data-raw/`

These scripts are included for transparency and reproducibility but are not part of the exported package API.

## Citation notes

Package documentation and vignettes should cite:

- ABS ASGS digital boundary files when using Australian geography examples.
- ABS Census DataPacks when using Census-derived demographic fields.
- H3 / `h3jsr` when using H3-style hexbin examples.
- Boscoe and Pradhan (2015) when demonstrating location quotient / distinctive risk ratio concepts.