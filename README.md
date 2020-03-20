---
title: Spatial variation in COVID-19 risk - a demographer's perspective
author: Mark D. Verhagen, D.avid M. Brazel, Charles Rahal, Jennifer Beam Dowd, Ilya Kashnitsky, Melinda C. Mills
date: today
---

### Introduction



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
