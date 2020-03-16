get_county_data <- function(geo = "county", year = 2018) {
  return(get_acs(geography = "county", variables = c(All = "B01001_001",
                                                     M_0_4 = "B01001_003",
                                                     M_5_9 = "B01001_004",
                                                     M_10_14 = "B01001_005",
                                                     M_15_17 = "B01001_006",
                                                     M_18_19 = "B01001_007",
                                                     M_20 = "B01001_008",
                                                     M_21 = "B01001_009",
                                                     M_22_24 = "B01001_010",
                                                     M_25_29 = "B01001_011",
                                                     M_30_34 = "B01001_012",
                                                     M_35_39 = "B01001_013",
                                                     M_40_44 = "B01001_014",
                                                     M_45_59 = "B01001_015",
                                                     M_50_54 = "B01001_016",
                                                     M_55_59 = "B01001_017",
                                                     M_60_61 = "B01001_018",
                                                     M_62_64 = "B01001_019",
                                                     M_65_66 = "B01001_020",
                                                     M_67_69 = "B01001_021",
                                                     M_70_74 = "B01001_022",
                                                     M_75_79 = "B01001_023",
                                                     M_80_84 = "B01001_024",
                                                     M_85 = "B01001_025",
                                                     F_0_4 = "B01001_027",
                                                     F_5_9 = "B01001_028",
                                                     F_10_14 = "B01001_029",
                                                     F_15_17 = "B01001_030",
                                                     F_18_19 = "B01001_031",
                                                     F_20 = "B01001_032",
                                                     F_21 = "B01001_033",
                                                     F_22_24 = "B01001_034",
                                                     F_25_29 = "B01001_035",
                                                     F_30_34 = "B01001_036",
                                                     F_35_39 = "B01001_037",
                                                     F_40_44 = "B01001_038",
                                                     F_45_59 = "B01001_039",
                                                     F_50_54 = "B01001_040",
                                                     F_55_59 = "B01001_041",
                                                     F_60_61 = "B01001_042",
                                                     F_62_64 = "B01001_043",
                                                     F_65_66 = "B01001_044",
                                                     F_67_69 = "B01001_045",
                                                     F_70_74 = "B01001_046",
                                                     F_75_79 = "B01001_047",
                                                     F_80_84 = "B01001_048",
                                                     F_85 = "B01001_049"),
                 year=2018))
}

get_block_data <- function(geo = "block", year = 2010) {
  return(get_decennial(geography = geo, variables = c(All = "P012001",
                                                     M_0_4 = "P012003",
                                                     M_5_9 = "P012004",
                                                     M_10_14 = "P012005",
                                                     M_15_17 = "P012006",
                                                     M_18_19 = "P012007",
                                                     M_20 = "P012008",
                                                     M_21 = "P012009",
                                                     M_22_24 = "P012010",
                                                     M_25_29 = "P012011",
                                                     M_30_34 = "P012012",
                                                     M_35_39 = "P012013",
                                                     M_40_44 = "P012014",
                                                     M_45_59 = "P012015",
                                                     M_50_54 = "P012016",
                                                     M_55_59 = "P012017",
                                                     M_60_61 = "P012018",
                                                     M_62_64 = "P012019",
                                                     M_65_66 = "P012020",
                                                     M_67_69 = "P012021",
                                                     M_70_74 = "P012022",
                                                     M_75_79 = "P012023",
                                                     M_80_84 = "P012024",
                                                     M_85 = "P012025",
                                                     F_0_4 = "P012027",
                                                     F_5_9 = "P012028",
                                                     F_10_14 = "P012029",
                                                     F_15_17 = "P012030",
                                                     F_18_19 = "P012031",
                                                     F_20 = "P012032",
                                                     F_21 = "P012033",
                                                     F_22_24 = "P012034",
                                                     F_25_29 = "P012035",
                                                     F_30_34 = "P012036",
                                                     F_35_39 = "P012037",
                                                     F_40_44 = "P012038",
                                                     F_45_59 = "P012039",
                                                     F_50_54 = "P012040",
                                                     F_55_59 = "P012041",
                                                     F_60_61 = "P012042",
                                                     F_62_64 = "P012043",
                                                     F_65_66 = "P012044",
                                                     F_67_69 = "P012045",
                                                     F_70_74 = "P012046",
                                                     F_75_79 = "P012047",
                                                     F_80_84 = "P012048",
                                                     F_85 = "P012049"),
                 year=2018))
}


l2w <- function(df) {
  df$estimate <- as.numeric(df$estimate)
  df_cast <- df %>%
    dcast(GEOID + NAME ~ variable, value.var = "estimate")

  df_cast <- df_cast %>%
    mutate(Age_0_9 = M_0_4 + M_5_9 + F_0_4 + F_5_9,
           Age_10_19 = M_10_14 + M_15_17 + M_18_19 + F_10_14 + F_15_17 + F_18_19,
           Age_20_29 = M_20 + M_21 + M_22_24 + M_25_29 + F_20 + F_21 + F_22_24 + F_25_29,
           Age_30_39 = M_30_34 + M_35_39 + F_30_34 + F_35_39,
           Age_40_49 = M_40_44 + M_45_59 + F_40_44 + F_45_59,
           Age_50_59 = M_50_54 + M_55_59 + F_50_54 + F_55_59,
           Age_60_69 = M_60_61 + M_62_64 + M_65_66 + M_67_69 + F_60_61 + F_62_64 + F_65_66 + F_67_69,
           Age_70_79 = M_70_74 + M_75_79 + F_70_74 + F_75_79,
           Age_80_plus = M_80_84 + M_85 + F_80_84 + F_85) %>%
    mutate(Age_All = Age_0_9 + Age_10_19 + Age_20_29 + Age_30_39 + Age_40_49 + Age_50_59 +
             Age_60_69 + Age_70_79 + Age_80_plus)
  unit_test_pop(df_cast)
  return(df_cast %>%
          select(GEOID, NAME, starts_with("Age")))
}


unit_test_pop <- function(df) {
  if (all(df$Age_All == df$All)) {
    print("Aggregates correct")
  } else {
    print("ERROR: Aggregates not correct")
  }
}

get_rates <- function() {
  return(read_excel('data/italy-13march.xlsx') %>%
    select(age, lethality_rate) %>%
    mutate(age = as_factor(age),
           age = paste0("Age-", age)) %>%
    mutate(age = gsub("-", "_", age)) %>%
    filter(age != 'Age_90+') %>%
    mutate(age = gsub("_89", "_plus", age)))
}

make_final_set <- function(df) {
  return(df %>%
           select(NAME, GEOID, fatalities, value) %>%
           group_by(NAME) %>%
           summarise(fatalities = sum(fatalities),
                     pop = sum(value),
                     GEOID = unique(GEOID)) %>%
           mutate(relative_hazard = fatalities / pop)
  )
}
