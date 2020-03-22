# Data and code for "Mapping hospital demand: demographics, spatial variation, and the risk of 'hospital deserts' during COVID-19 in England and Wales"
Mark D. Verhagen, David M. Brazel, Jennifer Beam Dowd, Ilya Kashnitsky, Melinda C. Mills

Leverhulme Centre for Demographic Science, University of Oxford \& Nuffield College

Interdisciplinary Centre on Population Dynamics, University of Southern Denmark

March 20th, 2020

[![DOI](https://zenodo.org/badge/195251200.svg)](https://zenodo.org/badge/latestdoi/195251200)

### Introduction

This repository contains code used to generate local risk measures of hospitalization in the UK within the context of the COVID-19 pandemic. These local risk measures are overlayed with hospital capacity. The corresponding preprint is available at https://osf.io/g8s96/.

### Structure of the Repository

There are three main scripts, each with a related function script:
1. *UK_census_construction.R*
1. *graphing_UK.R*
1. *simulate_hospitals.R*

Their overall function is described below.

### UK_census_construction.R

This script is used to generate a demographic dataset on a low aggregation level of England and Wales. We use the mid-2018 census estimates brought out by the Office for National Statistics (ONS) which can be found [here](https://www.ons.gov.uk/datasets/mid-year-pop-est/editions/time-series/versions/4), which contains estimated population counts for every age from 0 to 89, binning together all individuals of 90 and older. This information is available on the Lower layer Super Output Area (LSOA) level.

We first reconstruct these individual year counts into 5-year age bins. For each bin, relevant hospitalization, infection and critical care risks are attached from [Ferguson et al](https://www.imperial.ac.uk/media/imperial-college/medicine/sph/ide/gida-fellowships/Imperial-College-COVID19-NPI-modelling-16-03-2020.pdf). Hospitalization and critical care rates are available in 10-year intervals, we linearly interpolate between bins to get 5-year intervals. The resultant dataframe looks as follows:

|AreaCodes |LSOA               | Age bin | Count |  H_rate| I_rate| CC_rate| Lethal_rate|
|:---------|:------------------|:---|-----:|-----:|--------------:|----------:|--------------:|
|E01020634 |County Durham 001A |0-4 |    81| 0.001|          0.025|       0.05|              0|
|E01020635 |County Durham 001B |0-4 |    83| 0.001|          0.025|       0.05|              0|
|E01020636 |County Durham 001C |0-4 |    91| 0.001|          0.025|       0.05|              0|
|E01020654 |County Durham 001D |0-4 |    81| 0.001|          0.025|       0.05|              0|
|E01020676 |County Durham 001E |0-4 |    70| 0.001|          0.025|       0.05|              0|
|E01020613 |County Durham 002A |0-4 |    60| 0.001|          0.025|       0.05|              0|

Where *H_rate* is the hospitalization rate, *CC_rate* is the percentage of hospitalization that requires critical care, and *I_rate* is the overall infection rate.

We calculate the expected number of hospitalizations by multiplying the *Count*, *I_rate* and *H_rate* variables to get the expected hospitalization within a certain LSOA an a certain age-bin. We then multiply this hospitalization rate with the age-relevant critical care risk. To get the absolute number of expected hospitalizations and the absolute number of expected critical care hospitalizations, we aggregate and sum all age bins on the *LSOA* level.

### graphing_UK.R

In this script, we generate the plots presented in the commentary. We use three separate shapefiles, one on the Regional level for England which can be found [here](https://geoportal.statistics.gov.uk/datasets/regions-december-2018-en-bfc), one on the LSOA level for both England and Wales which can be found [here](https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bfc) and one on the Ceremonial County level for both England and Wales, available [here](https://www.ordnancesurvey.co.uk/business-government/products/boundaryline). A lookup between LSOA and Local Authority District (LAD) is available as part of the census by ONS, although some minor adjustments have to be made to address administrative border changes.

We collected NHS bed capacity for England from the NHS, which can be found [here](https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/bed-data-overnight/). Acute bed capacity can be found [here](https://www.england.nhs.uk/statistics/statistical-work-areas/critical-care-capacity/). For Wales, we used StatWales to calculate hospital bed capacity based on their daily available metric, which can be found [here](https://statswales.gov.wales/Catalogue/Health-and-Social-Care/NHS-Hospital-Activity/NHS-Beds).

Generally speaking, lookups between LAD and most standard aggregation levels are readily available at the ONS' website. However, bed capacity data from the NHS uses different regional aggregations, specifically the NHS region (which consists of 7 regions instead of the standard 9) and Ceremonial Counties instead of the normal Counties. More specifically, on the regional level the NHS aggregates the regions 'West Midlands' and 'East Midlands' together into the single region 'Midlands'. The same is the case for the regions 'North East' and 'Yorkshire and The Humber' which are aggregated into the NHS region 'North East'. To be able to use the nine administrative regions nonetheless, the capacity in the aggregated region is proportionally assigned to the underlying regions based on relative population sizes. This allowed us to use the standard lookup table from LAD to Region, which can be found [here](https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-region-april-2019-lookup-in-england). To get a lookup table from LSOA to Ceremonial County, we overlaid the shapefiles to generate our own geographically informed lookup table. We choose to aggregate the Ceremonial Counties of Greater London and City of London together into one. The next step was to aggregate together our census data including hospitalization rates with the shapefiles. Resultant spatial dataframes can be downloaded [here](www.github.io).

Given these spatial data frames on both the Region and Ceremonial County level, we generated per 1,000 statistics for expected hospitalization and expected critical care hospitalization. For each Region and each Ceremonial County we also calculated the expected tipping point of infection rate where hospital bed capacity would be at maximum. Finally, we calculated the net excess demand of hospitalization relative to supply. This data was then visualised in our graphs.

We included LSOA level expected hospitalization rates for Wales and London. The spatial dataframe can be found [here](www.github.io). For Wales, we geo-coded the locations of each hospital together with the average bed capacity and included them in our Wales-specific graphs.

## Assumptions

By using a fixed infection rate across the population, we implicitly assume the infection to spread to every person equally likely. Although this assumption does not hold in practice, it is an appropriate way to illustrate the strong spatial variation in risk across the country. In practice, the pandemic at the time of writing was focused more on young / urban areas, but since we aim to highlight areas with high risk as the pandemic spreads we choose a general infection rate.

We linearly interpolated the 10-year hospitalization rates from the Ferguson et al. (2020) paper to 5-year levels to capture more potential spatial variation. Specifically, we assumed the reported rates reflected the rate at the midpoint of the bin (e.g. for a rate of 3.2% for 30-39 year, the midpoint is 34.5). We then divided the difference between two midpoints into four parts, adding one part to get the first 5-year bin and three parts to get the second 5-year bin. We interpolated in this way between two bins at a time.

In the tables below, the original Ferguson et al. rates are given, as well as our interpolated 5-year intervals.

###### Pre-interpolation

|age   |  rate| acute_rate|
|:-----|-----:|----------:|
|0-9   | 0.001|      0.050|
|10-19 | 0.003|      0.050|
|20-29 | 0.012|      0.050|
|30-39 | 0.032|      0.050|
|40-49 | 0.049|      0.063|
|50-59 | 0.102|      0.122|
|60-69 | 0.166|      0.274|
|70-79 | 0.243|      0.432|
|80-89 | 0.273|      0.709|
|90+   | 0.273|      0.709|
###### Post-interpolation
|age   |    rate| acute_rate|
|:-----|-------:|----------:|
|0-4   | 0.00100|    0.05000|
|5-9   | 0.00150|    0.05000|
|10-14 | 0.00250|    0.05000|
|15-19 | 0.00525|    0.05000|
|20-24 | 0.00975|    0.05000|
|25-29 | 0.01700|    0.05000|
|30-34 | 0.02700|    0.05000|
|35-39 | 0.03625|    0.05325|
|40-44 | 0.04475|    0.05975|
|45-49 | 0.06225|    0.07775|
|50-54 | 0.08875|    0.10725|
|55-59 | 0.11800|    0.16000|
|60-64 | 0.15000|    0.23600|
|65-69 | 0.18525|    0.31350|
|70-74 | 0.22375|    0.39250|
|75-79 | 0.25050|    0.50125|
|80-84 | 0.26550|    0.63975|
|85-89 | 0.27300|    0.70900|
|90+   | 0.27300|    0.70900|
