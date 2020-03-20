# Spatial variation in COVID-19 risk - a demographer's perspective
Mark D. Verhagen, D.avid M. Brazel, Charles Rahal, Jennifer Beam Dowd, Ilya Kashnitsky, Melinda C. Mills

March 20th, 2020


### Introduction

This repository contains code used to generate local risk measures of hospitalization in the UK within the context of the COVID-19 pandemic. These local risk measures are overlayed with hospital capacity.


### Structure of the Repository

There are three main scripts, each with a related function script:
1. *UK_census_construction.R*
1. *graphing_UK.R*
1. *simulate_hospitals.R*

Their overall function is described below.

### UK_census_construction.R

This script is used to transform mid-2018 census data from the UK, which contains population counts on the  on every age from 0 to 90 (and 90+) on the Lower layer Super Output Area (LSOA) level. These are re-constructed into 5-year age bins. For each bin, relevant hospitalization, infection and critical care risks are attached. Hospitalization and critical care rates are available in 10-year intervals, we linearly interpolate between bins to get 5-year intervals. The resultant dataframe looks as follows:

|AreaCodes |LSOA               | Age bin | Count |  H_rate| I_rate| CC_rate| Lethal_rate|
|:---------|:------------------|:---|-----:|-----:|--------------:|----------:|--------------:|
|E01020634 |County Durham 001A |0-4 |    81| 0.001|          0.025|       0.05|              0|
|E01020635 |County Durham 001B |0-4 |    83| 0.001|          0.025|       0.05|              0|
|E01020636 |County Durham 001C |0-4 |    91| 0.001|          0.025|       0.05|              0|
|E01020654 |County Durham 001D |0-4 |    81| 0.001|          0.025|       0.05|              0|
|E01020676 |County Durham 001E |0-4 |    70| 0.001|          0.025|       0.05|              0|
|E01020613 |County Durham 002A |0-4 |    60| 0.001|          0.025|       0.05|              0|

The last steps are to calculate the expected number of hospitalizations by multiplying the *Count*, *I_rate* and *H_rate* variable and aggregating all age bins on the *LSOA* level.

See below our interpolation of the relevant rates from 10-year intervals to 5-year intervals

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

### graphing_UK.R

This script uses the expected hospitalizations and critical care hospitalizations on the LSOA level, calculated above, and aggregates these counts to the Region and Ceremonial County level. This allows a direct comparison between the expected number of hospitalizations and critical care hospitalizations with available bed capacity.

##### Region level
Bed capacity on the region level is available through the NHS' SDCS data collection effort. However, data is collected amongst the seven NHS regions, instead of the standard 9 administrative regions in England. More specifically, the regions 'West Midlands' and 'East Midlands' in the latter are merged together into the single region 'Midlands' in the former. The same is the case for the regions 'North East' and 'Yorkshire and The Humber' which are aggregated into the NHS region 'North East'. To redistribute the beds, the greater NHS region's capacity is proportionally assigned to the underlying administrative regions. 




In data, corono_pop_proj has:

- age
- country
- pop
    - Population in thousands in 2020, in that age group and country, from the UN WPP 2019 data
- lethality_rate
    - The case fatality ratio in that age group from the Italian data. I note that this will vary substantially in practice, we do not model that variation here.
- dead
    - Projected number of deaths = pop * lethality rate * 0.4
    - 0.4 is the infection rate, drawn from Mark Lipsitch's work. Of course, there is a lot of uncertainty here.
- total_pop
    - Population in thousands in 2020 summed across all age groups in that country
- dead_prop
    - dead / total_pop

italy-13march.xlsx and italy-16march.xlsx are the CFR data from Liliana

JHU_03-15-2020 has the Johns Hopkins summary data from March 15th. See: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data

medicare_providers/ has Medicare/Medicaid approved facilities data for the US

DartmouthAtlas/ has hospital capacity data from 2012 for the US:
	 - hsa_bdry.zip has the shapefiles for Hospital Service Areas "Each HSA consists of a group of cities and towns that include one or more hospitals to which local residents have the plurality of their inpatient admissions." These are pretty local areas, multiple within major cities.
	- 2012_hosp_resource_hsa.xls (https://atlasdata.dartmouth.edu/static/general_atlas_rates#resources) has hospital resource data (beds and nurses per 1,000 residents)
