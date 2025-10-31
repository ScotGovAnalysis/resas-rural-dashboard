# modules/agriculture_module.R
# PART 1: Setup, Configuration, and Data Loading Functions

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(plotly)
library(DT)

# Agriculture metrics available in the data 
agriculture_metrics <- list(
  "Agri-Environment Schemes" = list(
    file = "agri_environment_schemes.xlsx",
    classifications = c("UK"),
    full_name = "Area of land covered by higher-level or targeted agri-environment schemes in England, Wales, Scotland and Northern Ireland",
    unit = "Million Hectares"
  ),
  "Sustainably Managed Woodland" = list(
    file = "sustainable_woodland.xlsx",
    classifications = c("UK"),
    full_name = "Percentage of UK woodland area certified as sustainably managed by country",
    unit = "Percentage (%)"
  ),
  "GVA (Agri, Forestry, Fishing)" = list(
    file = "regional_gva.xlsx",
    classifications = c("4-fold"),
    full_name = "Regional Gross Value Added (GVA) (balanced) in pounds, million by Agriculture, Forestry and Fishing sectors",
    unit = "Millions of Pounds (£m)"
  ),
  "Employment (Agri, Forestry, Fishing)" = list(
    file = "employment_sector.xlsx",
    classifications = c("4-fold"),
    full_name = "Percentage (%) Employment by sector: Agriculture, Forestry and Fishing sector",
    unit = "Percentage (%)"
  ),
  "Fish Stocks" = list(
    file = "fish_stocks.xlsx",
    classifications = c("Scotland"),
    full_name = "Estimated % of commercial stocks fished at sustainable levels",
    unit = "Percentage (%)",
    single_point = FALSE,
    no_bar_chart = TRUE
  ),
  "Greenhouse Gas Emissions" = list(
    file = "greenhouse_gas.xlsx",
    classifications = c("Scotland"),
    full_name = "Greenhouse Gas Emissions in Scotland by Territorial Emissions Statistics Sector",
    unit = "kt CO2e",
    has_sub_metrics = TRUE
  ),
  "New Planting" = list(
    file = "woodland_creation.xlsx",
    classifications = c("UK"),
    full_name = "Area of woodland creation",
    unit = "Thousand Hectares"
  ),
  "Farm Income" = list(
    file = "farm_income.xlsx",
    classifications = c("Scotland"),
    full_name = "Farm Business Income by farm type",
    unit = "Pounds (£)",
    has_sub_metrics = TRUE
  ),
  "Diversified Activity And Incomes" = list(
    file = "diversified_activity.xlsx",
    classifications = c("Scotland"),
    full_name = "Diversified activity and incomes (5 year matched sample)",
    unit = "Various",
    has_sub_metrics = TRUE,
    no_bar_chart = TRUE
  ),
  "New To Crofting" = list(
    file = "new_crofting.xlsx",
    classifications = c("Scotland"),
    full_name = "Number of new entrants into crofting",
    unit = "Number",
    no_bar_chart = TRUE
  ),
  "Resident Crofters" = list(
    file = "resident_crofters.xlsx",
    classifications = c("Scotland"),
    full_name = "Number of crofters who are resident and actively using their croft",
    unit = "Percentage (%)",
    no_bar_chart = TRUE
  ),
  "Agritourism Value" = list(
    file = "agritourism_value.xlsx",
    classifications = c("Scotland"),
    full_name = "Value of Agritourism per annum in pounds, million",
    unit = "Millions of Pounds (£m)",
    no_bar_chart = TRUE
  )
)

# RESAS 4-fold classification mapping
resas_council_mapping_agriculture <- list(
  "Islands & Remote Rural" = c("Argyll and Bute", "Argyll & Bute", "Na h-Eileanan Siar", 
                               "Orkney Islands", "Shetland Islands"),
  "Mainly Rural" = c("Aberdeenshire", "Angus", "Clackmannanshire", "Dumfries and Galloway", 
                     "Dumfries & Galloway", "East Ayrshire", "East Lothian", "Highland", 
                     "Moray", "Perth and Kinross", "Perth & Kinross", "Scottish Borders", 
                     "South Ayrshire"),
  "Urban with Substantial Rural" = c("East Dunbartonshire", "East Renfrewshire", "Falkirk",
                                     "Fife", "Inverclyde", "Midlothian", "North Ayrshire", 
                                     "North Lanarkshire", "Renfrewshire", "South Lanarkshire", 
                                     "Stirling", "West Dunbartonshire", "West Lothian"),
  "Larger Cities" = c("Aberdeen City", "City of Edinburgh", "Edinburgh, City of", 
                      "Dundee City", "Glasgow City")
)

# Sub-metrics definitions
greenhouse_gas_sub_metrics <- c(
  "Agriculture" = "Agriculture",
  "LULUCF" = "LULUCF"
)

diversified_activity_sub_metrics <- c(
  "Average Diversified Income" = "Average diversified income of farms with diversified activity",
  "Average Diversified Income (% Of FBI)" = "Average diversified income of farms with diversified activity (% of FBI)"
)

#  key findings
agriculture_key_insights <- list(
  "Agri-Environment Schemes" = "The area of land covered by higher-level or targeted agri-environment schemes has been growing in the UK from 0.3m ha in 1992 to 3.7m ha in 2022. As of 2022, most of that area is in England (2.3m ha) with Scotland covering 0.9ha, Wales 0.4m ha and Northern Ireland only 0.1m ha (a drop from 0.4m ha since 2012.) ",
  "Sustainably Managed Woodland" = "Percentage of UK woodland area certified as sustainable managed as of 2024 is the highest in Scotland (60% up from 44% in 2001), closely followed by Northern Ireland (56% down from 75% in 2001). England has the lowest percentage of sustainably managed woodland (23%), although the proportion has remained steady since 2001.",
  "GVA (Agri, Forestry, Fishing)" = "Regional Gross Value added by agriculture, forestry and fishing sectors has seen the highest growth over the recent years in mainly rural areas (£1511m in 2018 to £2310m in 2022). Other regions remain relatively stable with urban with substantial rural areas contributing the second largest regional GVA (£532m) and larger cities the smallest (£111m) in 2022.",
  "Employment (Agri, Forestry, Fishing)" = "The highest proportion of people employed in agriculture, forestry and fishing sector as of 2023 is seen in Islands and remote rural areas (16.5%), followed by mainly rural areas (9.7%). Across Scotland in general, 3.4% of employment is represented by agriculture, forestry and fishing. Those numbers remained relatively stable since 2015.",
  "Fish Stocks" = "The percentage of commercial stocks fished at sustainable levels has been growing in Scotland since 1991, with a particular increase from 2007 to 2020 (40% to 69%).",
  "Greenhouse Gas Emissions" = "Greenhouse gas (GHG) emissions from agriculture have been in a long-term decline. Since 1990 agriculture emissions have fallen by 13% from 8.6 MtCO2e to 7.5 MtCO2e in 2023. Over the same time period, emissions from LULUCF fell by 91%, from 6 MtCO2e in 1990 to 0.5 MtCO2e in 2023. Total GHG emissions in Scotland fell by 51% from 81.2 MtCO2e in 1990 to 39.6 MtCO2e in 2023.",
  "New Planting" = "Scotland consistently has the highest area of woodland creation in the UK (15.1 thousand hectares in 2023-24 compared to 4.1 thousand hectares in England). Between 2022-23 and 2023-24 Scotland saw a large increase in woodland creation, going from 8.2 to 15.1 thousand hectares.",
  "Farm Income" = "Average farm income was around £35,500 in 2023-24, the lowest estimate since 2019-20 after adjusting for inflation. This is a 51% (£37,500) decrease from the high of the previous year, which was the highest income since 2012-13, the earliest year for which comparable data exists. Estimates for the average farm provide an overview of the financial health of the industry. The average farm is the weighted average across all farm types included in the Farm Business Survey.",
  "Diversified Activity And Incomes" = "In 2023-24, 59% of farms in the Farm Business Survey (matched sample) had at least one diversified activity. The average income from diversified activities increased by 18% in 2023-24 to around £12,200. The most common diversified activity continues to be renting out farm buildings (for purposes other than tourist accommodation). Other common profitable activities include micro electricity generation, wind turbines and providing tourist accommodation and catering.",
  "New To Crofting" = "The number of new entrants into crofting has remained steady from October 2021 to March 2024.",
  "Resident Crofters" = "Based on data from those who have returned their Annual Notice (Census), the proportion of crofters who live in Scotland and actively use their croft increased from 92% in 2020 to 94% in 2023",
  "Agritourism Value" = "The value of agritourism per annum in Scotland has increased from £55.7m in 2021 to £62.8m in 2022."
)
#notes
agriculture_notes <- list(
  "Agri-Environment Schemes" = "",
  "Sustainably Managed Woodland" = "", 
  "GVA (Agri, Forestry, Fishing)" = "1. GVA as current price estimates. 2. Scotland total is sum of regions.",
  "Employment (Agri, Forestry, Fishing)" = "1. The level of rounding applied varies by estimate. 2. Local Authority data obtained from open access and summed together. 3. Percentage employment calculated using source data. 4. Scotland total extracted from source data and not calculated hence different decimal rounding",
  "Fish Stocks" = "",
  "Greenhouse Gas Emissions" = "LULUCF stands for land use, land-use change, and forestry.",
  "New Planting" = "",
  "Farm Income" = "1. Real terms prices use the latest GDP deflator data, published 23 December 2024 at: https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-december-2024-quarterly-national-accounts. 2. LFA is Less Favoured Area. Average LFA is the weighted average of specialist sheep (LFA), specialist cattle (LFA) and cattle and sheep (LFA) farms.",
  "Diversified Activity And Incomes" = "1. This data is not weighted to the June Agricultural Census population. 2. Real terms prices use the latest GDP deflator data,  published 23 December 2024 at: https://www.gov.uk/government/statistics/gdp-deflators-at-market-prices-and-money-gdp-december-2024-quarterly-national-accounts/. 3. Data is from a subset of farms in the Farm Business Survey sample. These farms have been in the sample for a five-year period, during which they have engaged in diversification activities. 4. Estimate is not adjusted for inflation.",
  "New To Crofting" = "The count for 1 April 2023 to 31 March 2024 is provisional",
  "Resident Crofters" = "",
  "Agritourism Value" = "1. Value for 2021/22 is a business estimate. 2. VisitScotland will undertake a new agritourism tracker in 2025 that is to be published in 2026."
)

# Function to resolve file path
get_agriculture_file_path <- function(filename) {
  # Try current directory first, then agriculture subfolder
  if (file.exists(filename)) {
    return(filename)
  }
  
  filepath <- file.path("agriculture", filename)
  if (file.exists(filepath)) {
    return(filepath)
  }
  
  # Try main directory
  main_dir <- dirname(file.path(getwd(), "app.R"))
  filepath <- file.path(main_dir, "agriculture", filename)
  if (file.exists(filepath)) {
    return(filepath)
  }
  
  cat("File not found:", filename, "\n")
  return(filename)  # Return original filename as fallback
}

# 1. Load agri-environment schemes data (UK data)
load_agri_environment_data <- function() {
  filepath <- get_agriculture_file_path("agri_environment_schemes.xlsx")
  cat("Loading agri-environment schemes data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has headers: Region, 1992, 2002, 2012, 2022
    # Rows 3-7 have data for Scotland, England, Northern Ireland, Wales, UK
    headers <- c("Region", "1992", "2002", "2012", "2022")
    data_rows <- raw_data[3:7, 1:5]
    colnames(data_rows) <- headers
    
    processed_data <- data_rows %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),
        Area_Type = Region,
        Classification_UK = Region,
        Data_Source = "Scottish Government - Agri-Environment Schemes",
        Sub_Metric = NA_character_
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area_Type, Classification_UK, Value, Data_Source, Sub_Metric)
    
    cat("Processed", nrow(processed_data), "agri-environment records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading agri-environment data:", e$message, "\n")
    return(data.frame())
  })
}

# 2. Load sustainable woodland data (UK data)
load_sustainable_woodland_data <- function() {
  filepath <- get_agriculture_file_path("sustainable_woodland.xlsx")
  cat("Loading sustainable woodland data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has headers: Region, 2001, 2024
    # Rows 3-7 have data for Scotland, England, Wales, Northern Ireland, UK
    headers <- c("Region", "2001", "2024")
    data_rows <- raw_data[3:7, 1:3]
    colnames(data_rows) <- headers
    
    processed_data <- data_rows %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100,  # Convert to percentage
        Area_Type = Region,
        Classification_UK = Region,
        Data_Source = "Scottish Government - Sustainable Woodland",
        Sub_Metric = NA_character_
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area_Type, Classification_UK, Value, Data_Source, Sub_Metric)
    
    cat("Processed", nrow(processed_data), "sustainable woodland records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading sustainable woodland data:", e$message, "\n")
    return(data.frame())
  })
}
# Additional Data Loading Functions

# 3. Load Regional GVA data (RESAS classified)
load_regional_gva_data <- function() {
  filepath <- get_agriculture_file_path("regional_gva.xlsx")
  cat("Loading regional GVA data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has years from 1998-2022
    years <- as.character(raw_data[2, 2:26])  # Years 1998-2022
    
    processed_data <- data.frame()
    
    for (i in 3:7) {
      region_name <- as.character(raw_data[i, 1])
      if (is.na(region_name) || region_name == "") next
      
      values <- as.numeric(raw_data[i, 2:26])
      
      for (j in seq_along(years)) {
        year <- as.numeric(years[j])
        value <- values[j]
        
        if (!is.na(year) && !is.na(value) && year >= 1998) {
          # Apply RESAS classification
          classification_4fold <- case_when(
            region_name == "Islands & Remote Rural" ~ "Islands & Remote Rural",
            region_name == "Mainly Rural" ~ "Mainly Rural", 
            region_name == "Urban with Substantial Rural areas" ~ "Urban with Substantial Rural",
            region_name == "Larger Cities" ~ "Larger Cities",
            region_name == "Scotland (total)" ~ "Scotland",
            TRUE ~ region_name
          )
          
          
          processed_data <- rbind(processed_data, data.frame(
            Year = year,
            Area_Type = region_name,
            Classification_4fold = classification_4fold,
            Value = value,
            Data_Source = "Scottish Government - Regional GVA (Agriculture, Forestry, Fishing)",
            Sub_Metric = NA_character_,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat("Processed", nrow(processed_data), "regional GVA records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading regional GVA data:", e$message, "\n")
    return(data.frame())
  })
}

# test <- load_regional_gva_data()

# 4. Load employment sector data (RESAS classified)
load_employment_sector_data <- function() {
  filepath <- get_agriculture_file_path("employment_sector.xlsx")
  cat("Loading employment sector data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has years: 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023
    years <- as.character(raw_data[2, 2:10])  # Years 2015-2023
    
    processed_data <- data.frame()
    
    for (i in 3:7) {
      region_name <- as.character(raw_data[i, 1])
      if (is.na(region_name) || region_name == "") next
      
      values <- as.numeric(raw_data[i, 2:10]) * 100  # Convert to percentage
      
      for (j in seq_along(years)) {
        year <- as.numeric(years[j])
        value <- values[j]
        
        if (!is.na(year) && !is.na(value) && year >= 2015) {
          # Apply RESAS classification
          classification_4fold <- case_when(
            region_name == "Islands & Remote Rural" ~ "Islands & Remote Rural",
            region_name == "Mainly Rural" ~ "Mainly Rural", 
            region_name == "Urban with Substantial Rural areas" ~ "Urban with Substantial Rural",
            region_name == "Larger Cities" ~ "Larger Cities",
            region_name == "Scotland (average)" ~ "Scotland",
            TRUE ~ region_name
          )
          
          processed_data <- rbind(processed_data, data.frame(
            Year = year,
            Area_Type = region_name,
            Classification_4fold = classification_4fold,
            Value = value,
            Data_Source = "Scottish Government - Employment (Agriculture, Forestry, Fishing)",
            Sub_Metric = NA_character_,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat("Processed", nrow(processed_data), "employment sector records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading employment sector data:", e$message, "\n")
    return(data.frame())
  })
}

# 5. Load fish stocks data (Scotland time series)
load_fish_stocks_data <- function() {
  filepath <- get_agriculture_file_path("fish_stocks.xlsx")
  cat("Loading fish stocks data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has years from 1991-2020
    # Row 3 has percentage values
    years <- as.numeric(raw_data[2, 2:31])  # Years 1991-2020
    values <- as.numeric(raw_data[3, 2:31])  # Percentage values
    
    processed_data <- data.frame()
    
    for (i in seq_along(years)) {
      year <- years[i]
      value <- values[i]
      
      if (!is.na(year) && !is.na(value)) {
        processed_data <- rbind(processed_data, data.frame(
          Year = year,
          Area_Type = "Scotland",
          Classification_Scotland = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - Fish Stocks Sustainability",
          Sub_Metric = NA_character_,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    cat("Processed", nrow(processed_data), "fish stocks records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading fish stocks data:", e$message, "\n")
    return(data.frame())
  })
}

# 6. Load greenhouse gas data
load_greenhouse_gas_data <- function() {
  filepath <- get_agriculture_file_path("greenhouse_gas.xlsx")
  cat("Loading greenhouse gas data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    years <- as.character(raw_data[2, 3:30])
    processed_data <- data.frame()
    
    # Agriculture data (row 3)
    agriculture_values <- as.numeric(raw_data[3, 3:30])
    for (j in seq_along(years)) {
      year <- as.numeric(years[j])
      value <- agriculture_values[j]
      
      if (!is.na(year) && !is.na(value) && year >= 1990) {
        processed_data <- rbind(processed_data, data.frame(
          Year = year,
          Area_Type = "Scotland",
          Classification_Scotland = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - Greenhouse Gas Emissions",
          Sub_Metric = "Agriculture",
          Sector = "Agriculture",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # LULUCF data (row 10)
    lulucf_values <- as.numeric(raw_data[10, 3:30])
    for (j in seq_along(years)) {
      year <- as.numeric(years[j])
      value <- lulucf_values[j]
      
      if (!is.na(year) && !is.na(value) && year >= 1990) {
        processed_data <- rbind(processed_data, data.frame(
          Year = year,
          Area_Type = "Scotland",
          Classification_Scotland = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - Greenhouse Gas Emissions",
          Sub_Metric = "LULUCF",
          Sector = "LULUCF",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    cat("Processed", nrow(processed_data), "greenhouse gas records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading greenhouse gas data:", e$message, "\n")
    return(data.frame())
  })
}

# 7. Load woodland creation data (UK data) 
load_woodland_creation_data <- function() {
  filepath <- get_agriculture_file_path("woodland_creation.xlsx")
  cat("Loading woodland creation data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 1 has headers: Country, 2019/20, 2020/21, 2021/22, 2022/23, 2023/24
    # Rows 2-5 have data for Scotland, England, Wales, Northern Ireland
    headers <- c("Country", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
    data_rows <- raw_data[2:5, 1:6]
    colnames(data_rows) <- headers
    
    processed_data <- data_rows %>%
      filter(!is.na(Country)) %>%
      gather(key = "Year_Range", value = "Value", -Country) %>%
      mutate(
        # Extract the ending year from year ranges like "2019/20" -> 2020
        Year = case_when(
          Year_Range == "2019/20" ~ 2020,
          Year_Range == "2020/21" ~ 2021,
          Year_Range == "2021/22" ~ 2022,
          Year_Range == "2022/23" ~ 2023,
          Year_Range == "2023/24" ~ 2024,
          TRUE ~ NA_real_
        ),
        years_raw = gsub("/", "-", Year_Range),
        Value = as.numeric(Value),
        Area_Type = Country,
        Classification_UK = Country,
        Data_Source = "Scottish Government - Woodland Creation",
        Sub_Metric = NA_character_
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, years_raw, Area_Type, Classification_UK, Value, Data_Source, Sub_Metric)
    
    cat("Processed", nrow(processed_data), "woodland creation records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading woodland creation data:", e$message, "\n")
    return(data.frame())
  })
}

# 8. Load farm income data (Scotland with sub-metrics)
load_farm_income_data <- function() {
  filepath <- get_agriculture_file_path("farm_income.xlsx")
  cat("Loading farm income data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has years: 2012-13, 2013-14, ..., 2022-24
    years_raw <- as.character(raw_data[2, 2:13])
    
    processed_data <- data.frame()
    
    # Process farm type data (rows 3-12)
    for (i in 3:12) {
      farm_type <- as.character(raw_data[i, 1])
      if (is.na(farm_type) || farm_type == "") next
      
      values <- as.numeric(raw_data[i, 2:13])
      
      for (j in seq_along(years_raw)) {
        year_range <- years_raw[j]
        value <- values[j]
        
        # Extract the ending year from ranges like "2012-13" -> 2013
        year <- case_when(
          year_range == "2012-13" ~ 2013,
          year_range == "2013-14" ~ 2014,
          year_range == "2014-15" ~ 2015,
          year_range == "2015-16" ~ 2016,
          year_range == "2016-17" ~ 2017,
          year_range == "2017-18" ~ 2018,
          year_range == "2018-19" ~ 2019,
          year_range == "2019-20" ~ 2020,
          year_range == "2020-21" ~ 2021,
          year_range == "2021-22" ~ 2022,
          year_range == "2022-23" ~ 2023,
          year_range == "2023-24" ~ 2024,  
          TRUE ~ NA_real_
        )
        
        if (!is.na(year) && !is.na(value) && year >= 2013) {
          processed_data <- rbind(processed_data, data.frame(
            Year = year,
            Area_Type = "Scotland",
            Classification_Scotland = "Scotland",
            Value = value,
            Data_Source = "Scottish Government - Farm Business Income",
            Sub_Metric = farm_type,
            Farm_Type = farm_type,
            years_raw = year_range,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat("Processed", nrow(processed_data), "farm income records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading farm income data:", e$message, "\n")
    return(data.frame())
  })
}
# Agriculture Module Part 3: Remaining Data Functions and Helper Functions

# 9. Load diversified activity data (Scotland with sub-metrics) - CORRECTED
load_diversified_activity_data <- function(selected_metric = NULL) {
  filepath <- get_agriculture_file_path("diversified_activity.xlsx")
  cat("Loading diversified activity data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has years: 2019-20, 2020-21, 2021-22, 2022-23, 2023-24
    # Row 6: Average diversified income of farms with diversified activity
    # Row 7: Average diversified income of farms with diversified activity (% of FBI)
    years_raw <- as.character(raw_data[2, 2:6])
    
    processed_data <- data.frame()
    
    # Target rows with the specific metrics
    target_rows <- list(
      "Average diversified income of farms with diversified activity" = 6,
      "Average diversified income of farms with diversified activity (% of FBI)" = 7
    )
    
    for (metric_name in names(target_rows)) {
      row_idx <- target_rows[[metric_name]]
      values <- as.numeric(raw_data[row_idx, 2:6])
      
      for (j in seq_along(years_raw)) {
        year_range <- years_raw[j]
        value <- values[j]
        
        # Extract the ending year from ranges like "2019-20" -> 2020
        year <- case_when(
          year_range == "2019-20" ~ 2020,
          year_range == "2020-21" ~ 2021,
          year_range == "2021-22" ~ 2022,
          year_range == "2022-23" ~ 2023,
          year_range == "2023-24" ~ 2024,
          TRUE ~ NA_real_
        )
        
        if (!is.na(year) && !is.na(value)) {
          # Convert percentage values to actual percentages for the FBI metric
          final_value <- if (grepl("% of FBI", metric_name)) value * 100 else value
          
          processed_data <- rbind(processed_data, data.frame(
            Year = year,
            Area_Type = "Scotland",
            Classification_Scotland = "Scotland",
            Value = final_value,
            Data_Source = "Scottish Government - Diversified Activity and Incomes",
            Sub_Metric = metric_name,
            years_raw = year_range,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Filter by selected metric if specified
    if (!is.null(selected_metric) && selected_metric != "") {
      processed_data <- processed_data %>%
        filter(Sub_Metric == selected_metric)
    }
    
    cat("Processed", nrow(processed_data), "diversified activity records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading diversified activity data:", e$message, "\n")
    return(data.frame())
  })
}

# 10. Load new crofting data (Scotland)
load_new_crofting_data <- function() {
  filepath <- get_agriculture_file_path("new_crofting.xlsx")
  cat("Loading new crofting data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has periods: Oct 2021- Sep 2022, April 2022 - Mar 2023, Oct 2022- Sep 2023, Apr 2023- Mar 2024*
    # Row 3 has values: 538, 515, 537, 537
    periods <- as.character(raw_data[2, 2:5])
    periods <- periods[!is.na(periods) & periods != ""]
    
    values <- as.numeric(raw_data[3, 2:5])
    
    processed_data <- data.frame()
    
    # Define chronological order and assign sequential years with decimals for overlaps
    period_order <- c("Oct 2021- Sep 2022", "April 2022 - Mar 2023", "Oct 2022- Sep 2023", "Apr 2023- Mar 2024*")
    
    # Assign years: 2022, 2023, 2023.5, 2024 to handle overlaps in plotting
    year_mapping <- c(
      "Oct 2021- Sep 2022" = 2021,
      "April 2022 - Mar 2023" = 2022,
      "Oct 2022- Sep 2023" = 2023,
      "Apr 2023- Mar 2024*" = 2023.5
    )

    for (i in seq_along(periods)) {
      period <- trimws(periods[i])
      value <- values[i]
      
      if (!is.na(value) && period %in% names(year_mapping)) {
        processed_data <- rbind(processed_data, data.frame(
          Year = year_mapping[[period]],
          Area_Type = "Scotland",
          Classification_Scotland = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - New Entrants to Crofting",
          Sub_Metric = NA_character_,
          Period = period,  # Keep full period for tooltips
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Order by Year
    processed_data <- processed_data %>% arrange(Year)
    
    cat("Processed", nrow(processed_data), "new crofting records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading new crofting data:", e$message, "\n")
    return(data.frame())
  })
}

# 11. Load resident crofters data (Scotland)
load_resident_crofters_data <- function() {
  filepath <- get_agriculture_file_path("resident_crofters.xlsx")
  cat("Loading resident crofters data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Look for the specific row: "Percentage resident and actively using their croft"
    target_row <- NULL
    for (i in 1:nrow(raw_data)) {
      row_text <- as.character(raw_data[i, 1])
      if (!is.na(row_text) && grepl("Percentage resident and actively using", row_text, ignore.case = TRUE)) {
        target_row <- i
        break
      }
    }
    
    processed_data <- data.frame()
    
    if (!is.null(target_row)) {
      # Find year columns from header row
      year_row <- 2
      years <- as.character(raw_data[year_row, 2:ncol(raw_data)])
      years <- years[!is.na(years) & years != ""]
      
      values <- as.numeric(raw_data[target_row, 2:(length(years) + 1)])
      
      for (j in seq_along(years)) {
        year <- as.numeric(years[j])
        value <- values[j]
        
        if (!is.na(year) && !is.na(value) && year >= 2000) {
          processed_data <- rbind(processed_data, data.frame(
            Year = year,
            Area_Type = "Scotland",
            Classification_Scotland = "Scotland",
            Value = value*100,
            Data_Source = "Scottish Government - Resident Crofters",
            Sub_Metric = "Percentage resident and actively using their croft",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat("Processed", nrow(processed_data), "resident crofters records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading resident crofters data:", e$message, "\n")
    return(data.frame())
  })
}

# 12. Load agritourism value data (Scotland) 
load_agritourism_value_data <- function() {
  filepath <- get_agriculture_file_path("agritourism_value.xlsx")
  cat("Loading agritourism value data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read_excel(filepath, col_names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Row 2 has headers: Sector, 2020/21 reported, 2021/22 estimated
    # Row 3 has agritourism data: Agritourism, 55.7, 62.8
    
    processed_data <- data.frame()
    
    # Extract agritourism values from row 3
    agritourism_values <- as.numeric(raw_data[3, 2:3])
    years <- c(2021, 2022)  # 2020/21 -> 2021, 2021/22 -> 2022
    
    for (i in seq_along(years)) {
      year <- years[i]
      value <- agritourism_values[i]
      
      if (!is.na(year) && !is.na(value)) {
        processed_data <- rbind(processed_data, data.frame(
          Year = year,
          Area_Type = "Scotland", 
          Classification_Scotland = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - Agritourism Value",
          Sub_Metric = "Value of Agritourism",
          stringsAsFactors = FALSE
        ))
      }
    }
    
    cat("Processed", nrow(processed_data), "agritourism value records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading agritourism value data:", e$message, "\n")
    return(data.frame())
  })
}

# HELPER FUNCTIONS

# Function to aggregate data by classification
aggregate_agriculture_by_classification <- function(processed_data, classification_type = "2-fold") {
  if (nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  classification_col <- case_when(
    classification_type == "2-fold" ~ "Classification_2fold",
    classification_type == "4-fold" ~ "Classification_4fold", 
    classification_type == "UK" ~ "Classification_UK",
    classification_type == "Scotland" ~ "Classification_Scotland",
    TRUE ~ names(processed_data)[grepl("Classification", names(processed_data))][1]
  )
  
  if (!classification_col %in% names(processed_data)) {
    cat("Classification", classification_type, "not available in data\n")
    return(data.frame())
  }
  
  aggregated <- processed_data %>%
    group_by(Year, !!sym(classification_col), Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    rename(Area = !!sym(classification_col)) %>%
    mutate(Classification = classification_type) %>%
    filter(!is.na(Area))
  
  return(aggregated)
}


# Function to get 2-fold urban/rural values for value boxes
get_agriculture_urban_rural_2fold <- function(processed_data) {
  if (nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, year = NA))
  }
  
  # Check what type of data we have and aggregate accordingly
  unique_areas <- unique(processed_data$Area_Type)
  
  # Initialize values
  urban_val <- NA
  rural_val <- NA
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  # Case 1: Already has 2-fold classification
  if ("Classification_2fold" %in% names(processed_data)) {
    agg_2fold <- aggregate_agriculture_by_classification(processed_data, "2-fold")
    if (nrow(agg_2fold) > 0) {
      latest_data <- agg_2fold %>% filter(Year == latest_year)
      urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value)
      rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value)
    }
  }
  # Case 2: Has 4-fold RESAS areas
  else if (any(c(C) %in% unique_areas)) {
    # Urban = Larger Cities + Urban with Substantial Rural areas
    urban_areas <- latest_data %>% 
      filter(Area_Type %in% c("Larger Cities", "Urban with Substantial Rural areas"))
    if(nrow(urban_areas) > 0) {
      urban_val <- sum(urban_areas$Value, na.rm = TRUE) / nrow(urban_areas)
    }
    
    # Rural = Mainly Rural + Islands & Remote Rural
    rural_areas <- latest_data %>% 
      filter(Area_Type %in% c("Mainly Rural", "Islands & Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- sum(rural_areas$Value, na.rm = TRUE) / nrow(rural_areas)
    }
  }
  
  return(list(
    urban = ifelse(length(urban_val) > 0 && !is.na(urban_val[1]), urban_val[1], NA),
    rural = ifelse(length(rural_val) > 0 && !is.na(rural_val[1]), rural_val[1], NA),
    year = latest_year
  ))
}

# Function to get display name for metrics
get_agriculture_metric_display_name <- function(metric_name) {
  metric_info <- agriculture_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# Function to get Scotland summary
get_agriculture_scotland_summary <- function(processed_data, metric_name) {
  if (nrow(processed_data) == 0) {
    return(list(value = NA, year = NA, label = "No Data"))
  }
  
  latest_year <- max(processed_data$Year)
  
  # For metrics with Scotland totals, look for Scotland specifically
  scotland_data <- processed_data %>%
    filter(Year == latest_year) %>%
    filter(Area_Type == "Scotland" | grepl("Scotland", Area_Type, ignore.case = TRUE))
  
  if (nrow(scotland_data) > 0) {
    scotland_value <- scotland_data$Value[1]
    label <- "Scotland Total"
  } else {
    # Calculate average
    scotland_value <- processed_data %>%
      filter(Year == latest_year) %>%
      summarise(avg = mean(Value, na.rm = TRUE)) %>%
      pull(avg)
    label <- "Scotland Average"
  }
  
  return(list(
    value = scotland_value,
    year = latest_year,
    label = label
  ))
}

# Function to format values based on metric type
format_agriculture_value <- function(value, metric_name, decimal_places = 1) {
  if (is.na(value)) return("No Data")
  
  metric_info <- agriculture_metrics[[metric_name]]
  unit <- ifelse(!is.null(metric_info), metric_info$unit, "")
  
  if (grepl("Percentage", unit) || grepl("%", unit)) {
    return(paste0(round(value, decimal_places), "%"))
  } else if (grepl("Million", unit)) {
    if (grepl("Pounds", unit)) {
      return(paste0("£", round(value, decimal_places), ""))
    } else {
      return(paste0(round(value, decimal_places), "ha"))
    }
  } else if (grepl("Thousand", unit)) {
    return(paste0(round(value, decimal_places), "ha"))
  } else if (grepl("kt CO2e", unit)) {
    return(paste0(round(value, decimal_places), " kt CO2e"))
  } else if (grepl("Pounds", unit)) {
    return(paste0("£", scales::comma(round(value, 0))))
  } else {
    return(round(value, decimal_places))
  }
}

# Function to get appropriate y-axis label
get_agriculture_y_label <- function(metric_name) {
  metric_info <- agriculture_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    unit <- metric_info$unit
    return(unit)
  }
  return("Value")
}

# Function to check if metric should show bar chart
should_show_bar_chart <- function(metric_name) {
  metric_info <- agriculture_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    return(!isTRUE(metric_info$no_bar_chart))
  }
  return(TRUE)
}

get_agriculture_colors <- function(areas, classification_type, metric_name = NULL) {
  color_mapping <- list()
  
  # Use culture module color scheme for GVA and Employment
  if (metric_name %in% c("GVA (Agri, Forestry, Fishing)", "Employment (Agri, Forestry, Fishing)")) {
    if (classification_type == "4-fold") {
      color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural" = "#F4E470", 
                          "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")
    } else if (classification_type == "2-fold") {
      colors <- get_classification_colors("2-fold")
      if (!is.null(colors)) {
        color_mapping[["Urban"]] <- colors[["Urban"]]
        color_mapping[["Rural"]] <- colors[["Rural"]]
      }
    }
  }
  
  if (classification_type == "UK") {
    color_mapping[["England"]] <- "#FF0000"      # Red
    color_mapping[["Northern Ireland"]] <- "#FFA500"  # Orange  
    color_mapping[["Wales"]] <- "#00FF00"        # Green
    color_mapping[["Scotland"]] <- "#000080"     # Dark blue
  } else {
    # Always set Scotland to gray for non-UK classifications
    color_mapping[["Scotland"]] <- "#B2B2B2"
  }
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Agriculture Module Part 4: UI Dashboard - Exactly Matching Culture Module

# CREATE AGRICULTURE DASHBOARD UI - EXACTLY MATCHING CULTURE MODULE
agriculture_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      /* STANDARDIZED CSS FOR ALL DASHBOARD MODULES */
      .selectize-dropdown { z-index: 9999 !important; }
      .selectize-control.single .selectize-dropdown { z-index: 9999 !important; }
      .category-header { overflow: visible !important; }
      .category-header-overlay { overflow: visible !important; }
      
      /* CATEGORY HEADER - EDGE TO EDGE AND AT VERY TOP */
      .category-header {
        position: fixed !important;
        top: 0 !important;
        left: 237px !important;
        right: 0 !important;
        z-index: 1050 !important;
        margin: 0 !important;
        border-radius: 0 !important;
        min-height: 140px !important;
        max-height: 140px !important;
        width: calc(100vw - 237px) !important;
        background-size: cover !important;
        background-position: center !important;
        background-repeat: no-repeat !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
      }
      
      /* Account for collapsed sidebar */
      .sidebar-collapse .category-header {
        left: 0 !important;
        width: 100vw !important;
      }
      
      /* HEADER OVERLAY */
      .category-header-overlay {
        background: linear-gradient(135deg, rgba(0,0,0,0.7), rgba(0,0,0,0.4)) !important;
        position: relative !important;
        padding: 25px !important;
        display: flex !important;
        flex-direction: column !important;
        justify-content: center !important;
        align-items: center !important;
        color: white !important;
        border-radius: 0 !important;
        min-height: 180px !important;
        transition: all 0.3s ease !important;
      }
      
      /* STANDARDIZED LABEL STYLING WITH BACKGROUND */
      .category-header .form-group label {
        color: white !important; 
        font-weight: 600 !important;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.9) !important; 
        font-size: 14px !important;
        background: rgba(0,0,0,0.6) !important;
        padding: 6px 12px !important;
        border-radius: 4px !important;
        display: inline-block !important;
        margin-bottom: 8px !important;
      }
      
      /* DROPDOWN STYLING */
      .category-header .selectize-control.single .selectize-input {
        background: white !important; 
        border: 2px solid #ccc !important;
        border-radius: 6px !important; 
        box-shadow: 0 2px 8px rgba(0,0,0,0.4) !important;
        color: black !important; 
        font-weight: 500 !important;
      }
      
      .category-header .selectize-control.single .selectize-input.focus {
        border-color: #007bff !important;
        box-shadow: 0 2px 12px rgba(0,123,255,0.3) !important;
        background: white !important;
      }
      
      .category-header .selectize-dropdown {
        background: white !important;
        border: 1px solid #ccc !important;
        color: black !important;
      }
      
      .category-header .selectize-dropdown-content .option {
        background: white !important;
        color: black !important;
        border-bottom: 1px solid #eee !important;
      }
      
      .category-header .selectize-dropdown-content .option:hover,
      .category-header .selectize-dropdown-content .option.active {
        background: #f0f0f0 !important;
        color: black !important;
      }
      
      /* BUTTON STYLING */
      .category-header .btn {
        background: white !important;
        border: 2px solid #ccc !important;
        color: black !important;
        text-shadow: none !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.4) !important;
        padding: 10px 20px !important;
        font-weight: 500 !important;
        border-radius: 6px !important;
      }
      
      .category-header .btn:hover {
        background: #f0f0f0 !important;
        border-color: #007bff !important;
        transform: translateY(-1px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.3) !important;
        color: black !important;
      }
      
      /* METRIC SELECTION CARD STYLING */
      .agriculture-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .agriculture-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .agriculture-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .agriculture-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .agriculture-metric-description {
        color: #6c757d !important;
        font-size: 0.95em !important;
        line-height: 1.4 !important;
        margin-left: 30px !important;
      }
      "))
    ),
    
    fluidRow(
      div(
        class = "category-header",
        style = paste0("background-image: url('", cat_info$bg_image, "'); min-height: 140px; overflow: visible; margin-left: 15px; margin-right: 15px; border-radius: 8px;"),
        div(
          class = "category-header-overlay",
          style = "background: transparent; padding: 25px; position: relative; overflow: visible; border-radius: 8px;",
          
          # Title
          div(
            style = "position: absolute; left: 50px; top: 2px;",
            h2("Agriculture and Marine", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("agriculture_classification_selector")
          ),
          
          # Top-center-right: Agriculture metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("agriculture_metric", "Agriculture Metric:", 
                        choices = c("Select a policy metric..." = "", names(agriculture_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (when applicable)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.agriculture_metric == 'Diversified Activity And Incomes'",
              selectInput("selected_diversified_metric", "Sub-metric:", 
                          choices = names(diversified_activity_sub_metrics), 
                          selected = names(diversified_activity_sub_metrics)[1], width = "180px")
            )
          ),
          
          div(
            style = "position: absolute; bottom: 10px; left: 15px;",
            actionButton("back_to_categories", textOutput("back_button_text", inline = TRUE), class = "btn",
                         style = "padding: 10px 20px; font-weight: 500; border-radius: 6px; background: transparent; border: 2px solid rgba(255,255,255,0.6); color: white; text-shadow: 1px 1px 2px rgba(0,0,0,0.7); box-shadow: 0 2px 8px rgba(0,0,0,0.3);")
          )
        )
      )
    ),
    
    # Show metric selection when no metric is selected
    conditionalPanel(
      condition = "input.agriculture_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore agriculture and marine data across Scotland's regions.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Agriculture-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
                
                # Agri-Environment Schemes
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Agri-Environment Schemes', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("leaf", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Agri-Environment Schemes"),
                      div(class = "agriculture-metric-description", 
                          "Area of land covered by higher-level or targeted agri-environment schemes supporting biodiversity and sustainable land management.")
                    )
                  )
                ),
                
                # Sustainably Managed Woodland
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Sustainably Managed Woodland', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("tree", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Sustainably Managed Woodland"),
                      div(class = "agriculture-metric-description",
                          "Percentage of woodland area certified as sustainably managed, balancing ecological protection with timber production.")
                    )
                  )
                ),
                
                # GVA Agriculture
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'GVA (Agri, Forestry, Fishing)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("chart-line", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Regional GVA (Agri, Forestry, Fishing)"),
                      div(class = "agriculture-metric-description",
                          "Regional Gross Value Added from agriculture, forestry and fishing sectors across different area types.")
                    )
                  )
                ),
                
                # Employment
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Employment (Agri, Forestry, Fishing)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("users", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Employment (Agri, Forestry, Fishing)"),
                      div(class = "agriculture-metric-description",
                          "Percentage of total employment in agriculture, forestry and fishing sectors by regional classification.")
                    )
                  )
                ),
                
                # Fish Stocks
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Fish Stocks', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("fish", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Fish Stocks Sustainability"),
                      div(class = "agriculture-metric-description",
                          "Estimated percentage of commercial stocks fished at sustainable levels around Scotland.")
                    )
                  )
                ),
                
                # Greenhouse Gas Emissions
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Greenhouse Gas Emissions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("cloud", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Greenhouse Gas Emissions"),
                      div(class = "agriculture-metric-description",
                          "Greenhouse gas emissions from agriculture and land use, land-use change and forestry (LULUCF).")
                    )
                  )
                ),
                
                # New Planting
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'New Planting', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("seedling", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "New Woodland Creation"),
                      div(class = "agriculture-metric-description",
                          "Area of new woodland creation supporting climate targets and rural economic opportunities.")
                    )
                  )
                ),
                
                # Farm Income
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Farm Income', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("money-bill-wave", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Farm Business Income"),
                      div(class = "agriculture-metric-description",
                          "Farm business income by farm type, showing financial returns across different agricultural sectors.")
                    )
                  )
                ),
                
                # Diversified Activity
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Diversified Activity And Incomes', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("cogs", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Diversified Activity And Incomes"),
                      div(class = "agriculture-metric-description",
                          "Farm diversification providing additional income streams and rural business opportunities.")
                    )
                  )
                ),
                
                # New To Crofting
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'New To Crofting', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("home", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "New Entrants to Crofting"),
                      div(class = "agriculture-metric-description",
                          "Number of new entrants into crofting, vital for maintaining active crofting communities.")
                    )
                  )
                ),
                
                # Resident Crofters
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Resident Crofters', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("user-check", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Resident Crofters"),
                      div(class = "agriculture-metric-description",
                          "Percentage of crofters who are resident and actively using their croft for sustainable land management.")
                    )
                  )
                ),
                
                # Agritourism Value
                div(
                  class = "agriculture-metrics-card",
                  onclick = "Shiny.setInputValue('agriculture_metric_select', 'Agritourism Value', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("camera", class = "agriculture-metric-icon"),
                    div(
                      div(class = "agriculture-metric-title", "Agritourism Value"),
                      div(class = "agriculture-metric-description",
                          "Economic value of agritourism combining agriculture with tourism to showcase rural heritage.")
                    )
                  )
                )
              )
            )
          )
      )
    ),
    
    # Show content only when a metric is selected
    conditionalPanel(
      condition = "input.agriculture_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("agriculture_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("agriculture_data_summary"))
          ),
          
          
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                        uiOutput("agriculture_trend_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("agriculture_trend_download", "Download", class = "excel-download-btn"
                        ))),
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("agriculture_trend_chart") %>% withSpinner())
      ),
      
      conditionalPanel(
        condition = "!['Diversified Activity And Incomes', 'New To Crofting', 'Resident Crofters', 'Agritourism Value', 'Fish Stocks'].includes(input.agriculture_metric)",
        fluidRow(
          box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                          uiOutput("agriculture_comparison_title"),
                          div(style = "position: absolute; right: 20px;",
                              downloadButton("agriculture_comparison_download", "Download", class = "excel-download-btn"
                              ))),
                             status = "primary", solidHeader = TRUE, width = 12,
              div(style = "margin-bottom: 15px;", uiOutput("agriculture_year_selector")),
              plotlyOutput("agriculture_comparison_chart") %>% withSpinner())
        )
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                        uiOutput("agriculture_table_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("agriculture_table_download", "Download", class = "excel-download-btn"
                            ))),
            status = "info", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("agriculture_table_year_filter")),
                  column(6, uiOutput("agriculture_table_area_filter"))
                )),
            DT::dataTableOutput("agriculture_data_table") %>% withSpinner())
      )
    )
  )
}
# Agriculture Module Part 5: Server Functions

# AGRICULTURE MODULE SERVER FUNCTION 
agriculture_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  agriculture_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards
  observeEvent(input$agriculture_metric_select, {
    updateSelectInput(session, "agriculture_metric", selected = input$agriculture_metric_select)
  })
  
  # Dynamic UI outputs
  output$agriculture_summary_title <- renderUI({
    req(input$agriculture_metric, input$agriculture_classification_type)
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    classification_text <- case_when(
      input$agriculture_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$agriculture_classification_type == "4-fold" ~ "(4-fold RESAS)",
      input$agriculture_classification_type == "UK" ~ "(UK Countries)",
      input$agriculture_classification_type == "Scotland" ~ "(Scotland)",
      TRUE ~ paste0("(", input$agriculture_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$agriculture_year_selector <- renderUI({
    req(agriculture_values$processed_data)
    
    if (nrow(agriculture_values$processed_data) == 0) return(NULL)
    
    available_years <- sort(unique(agriculture_values$processed_data$Year), decreasing = TRUE)
    
    # Format years as financial years if applicable
    formatted_years <- if (input$agriculture_metric %in% c("Farm Income", "Diversified Activity And Incomes")) {
      sapply(available_years, function(year) paste0(year-1, "-", substr(year, 3, 4)))  # e.g., 2022 becomes "2022-23"
    } else {
      as.character(available_years)
    }
    
    selectInput(
      inputId = "agriculture_selected_year",
      label = "Select Year for Comparison:",
      choices = formatted_years,
      selected = formatted_years[1],
      width = "200px"
    )
  })
  output$agriculture_key_insights_box <- renderUI({
    req(input$agriculture_metric)
    
    # Only show key insights for GVA metric
    if (input$agriculture_metric == "GVA (Agri, Forestry, Fishing)") {
      fluidRow(
        box(title = uiOutput("agriculture_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
            fluidRow(
              valueBoxOutput("agriculture_urban_rate", width = 3),
              valueBoxOutput("agriculture_rural_rate", width = 3),
              valueBoxOutput("agriculture_scotland_rate", width = 3),
              valueBoxOutput("agriculture_urban_rural_gap", width = 3)
            ))
      )
    } else {
      # Return nothing for other metrics
      NULL
    }
  })

  output$agriculture_classification_selector <- renderUI({
    req(input$agriculture_metric)
    available_classifications <- agriculture_metrics[[input$agriculture_metric]]$classifications
    choices <- list()
    
    # Special handling for GVA - default to 4-fold and rename 2-fold
    if (input$agriculture_metric == "GVA (Agri, Forestry, Fishing)") {
      if ("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if ("2-fold" %in% available_classifications) choices[["2-fold (RESAS)"]] <- "2-fold"
    }
    # Employment - only 4-fold
    else if (input$agriculture_metric == "Employment (Agri, Forestry, Fishing)") {
      if ("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
    }
    # Other metrics - standard naming
    else {
      if ("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if ("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
      if ("UK" %in% available_classifications) choices[["UK Countries"]] <- "UK"
      if ("Scotland" %in% available_classifications) choices[["Scotland"]] <- "Scotland"
    }
    
    # Default to 4-fold for GVA, otherwise first available
    default_selection <- if (input$agriculture_metric == "GVA (Agri, Forestry, Fishing)" && "4-fold" %in% names(choices)) "4-fold" else if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("agriculture_classification_type", "Geographic Classification:", choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles
  output$agriculture_trend_title <- renderUI({
    req(input$agriculture_metric)
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$agriculture_comparison_title <- renderUI({
    req(input$agriculture_selected_year, input$agriculture_metric)
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    paste0("Single Year Comparison (", input$agriculture_selected_year, ") for ", display_name)
  })
  
  output$agriculture_table_title <- renderUI({
    req(input$agriculture_metric)
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    paste("Data Table for", display_name)
  })
  
  output$agriculture_key_insights_title <- renderUI({
    req(input$agriculture_metric)
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$agriculture_table_year_filter <- renderUI({
    req(agriculture_values$processed_data)
    if (nrow(agriculture_values$processed_data) == 0) return(NULL)
    
    # Determine year labels based on the selected metric
    if (input$agriculture_metric %in% c("Farm Income", "Diversified Activity And Incomes")) {
      all_years <- paste0(
        agriculture_values$processed_data$Year - 1, "-",
        substr(agriculture_values$processed_data$Year, 3, 4)
      )
    } else {
      all_years <- agriculture_values$processed_data$Year
    }
    
    all_years <- sort(unique(all_years), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for (year in all_years) {
      choices[[as.character(year)]] <- year
    }
    
    selectInput("agriculture_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$agriculture_table_area_filter <- renderUI({
    req(agriculture_values$processed_data, input$agriculture_classification_type)
    if (nrow(agriculture_values$processed_data) == 0) return(NULL)
    agg_data <- aggregate_agriculture_by_classification(agriculture_values$processed_data, input$agriculture_classification_type)
    if (nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for (area in all_areas) choices[[area]] <- area
    selectInput("agriculture_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Data processing reactive observer
  observe({
    if (is.null(input$agriculture_metric) || input$agriculture_metric == "") {
      agriculture_values$processed_data <- data.frame()
      agriculture_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$agriculture_classification_type)
    
    cat("Processing agriculture metric:", input$agriculture_metric, "\n")
    
    if (input$agriculture_metric == "Agri-Environment Schemes") {
      agriculture_values$processed_data <- load_agri_environment_data()
    } else if (input$agriculture_metric == "Sustainably Managed Woodland") {
      agriculture_values$processed_data <- load_sustainable_woodland_data()
    } else if (input$agriculture_metric == "GVA (Agri, Forestry, Fishing)") {
      agriculture_values$processed_data <- load_regional_gva_data()
    } else if (input$agriculture_metric == "Employment (Agri, Forestry, Fishing)") {
      agriculture_values$processed_data <- load_employment_sector_data()
    } else if (input$agriculture_metric == "Fish Stocks") {
      agriculture_values$processed_data <- load_fish_stocks_data()
    } else if (input$agriculture_metric == "Greenhouse Gas Emissions") {
      agriculture_values$processed_data <- load_greenhouse_gas_data()
    } else if (input$agriculture_metric == "New Planting") {
      agriculture_values$processed_data <- load_woodland_creation_data()
    } else if (input$agriculture_metric == "Farm Income") {
      agriculture_values$processed_data <- load_farm_income_data()
    } else if (input$agriculture_metric == "Diversified Activity And Incomes") {
      req(input$selected_diversified_metric)
      selected_metric_code <- diversified_activity_sub_metrics[input$selected_diversified_metric]
      agriculture_values$processed_data <- load_diversified_activity_data(selected_metric_code)
    } else if (input$agriculture_metric == "New To Crofting") {
      agriculture_values$processed_data <- load_new_crofting_data()
    } else if (input$agriculture_metric == "Resident Crofters") {
      agriculture_values$processed_data <- load_resident_crofters_data()
    } else if (input$agriculture_metric == "Agritourism Value") {
      agriculture_values$processed_data <- load_agritourism_value_data()
    } else {
      agriculture_values$processed_data <- data.frame()
    }
    
    agriculture_values$data_status <- if (nrow(agriculture_values$processed_data) > 0) "Agriculture data loaded" else "No data available"
    cat("Final data status:", agriculture_values$data_status, "with", nrow(agriculture_values$processed_data), "rows\n")
  })
  
  # Data summary
  output$agriculture_data_summary <- renderUI({
    req(agriculture_values$processed_data, input$agriculture_metric)
    
    if (nrow(agriculture_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    custom_insight <- agriculture_key_insights[[input$agriculture_metric]]
    custom_notes <- agriculture_notes[[input$agriculture_metric]]
    
    # Define source information
    source_info <- switch(input$agriculture_metric,
                          "Agri-Environment Schemes" = list(
                            text = "Defra 2024",
                            url = "https://jncc.gov.uk/resources/028095a0-0b83-4d7e-b830-b050cb8f5865"),
                          "Sustainably Managed Woodland" = list(
                            text = "Forestry Statistics 2024",
                            url = "https://jncc.gov.uk/our-work/ukbi-sustainable-forestry/"),
                          "GVA (Agri, Forestry, Fishing)" = list(
                            text = "Regional GVA (balanced) by industry A and BDE: RESAS classification for the rural economy, 1998 to 2022 - Office for National Statistics",
                            url = "https://www.ons.gov.uk/economy/regionalaccounts/grossdisposablehouseholdincome/adhocs/2011regionalgrossvalueaddedbalancedbyindustryaandbdescottishgovernmentsgruralandenvironmentalscienceandanalyticalservicesresasclassificationfortheruraleconomy1998to2022"),
                          "Employment (Agri, Forestry, Fishing)" = list(
                            text = "Business Register and Employment Survey",
                            url = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/businessregisterandemploymentsurveybresprovisionalresults/previousReleases"),
                          "Fish Stocks" = list(
                            text = "National Performance Framework (ICES data)",
                            url = "https://www.gov.scot/collections/national-performance-framework/?via=https://nationalperformance.gov.scot/national-outcomes/explore-national-outcomes/environment/measuring-progress-environment"),
                          "Greenhouse Gas Emissions" = list(
                            text = "Scottish Greenhouse Gas Statistics 2023",
                            url = "https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2023/"),
                          "New Planting" = list(
                            text = "Forest Research",
                            url = "https://www.forestresearch.gov.uk/tools-and-resources/statistics/publications/forestry-statistics/forestry-statistics-2024/2024-1-woodland-area-and-planting/"),
                          "Farm Income" = list(
                            text = "Scottish farm business income: annual estimates 2023-2024",
                            url = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/"),
                          "Diversified Activity And Incomes" = list(
                            text = "Scottish farm business income: annual estimates 2023-2024",
                            url = "https://www.gov.scot/collections/scottish-farm-business-income-fbi-annual-estimates/"),
                          "New To Crofting"  = list(
                            text = "Crofting Commission. April 2023 - March 2024 data provided by Crofting Policy Team, Scottish Government",
                            url = "https://crofting.scotland.gov.uk/annual-report-and-accounts"),
                          "Resident Crofters" = list(
                            text = "Crofting Commission April 2023 - March 2024 data provided by Crofting Policy Team, Scottish Government",
                            url = "https://crofting.scotland.gov.uk/annual-report-and-accounts"),
                          "Agritourism Value"  = list(
                            text = "Visit Scotland Agritourism Growth Tracker",
                            url = "https://www.visitscotland.org/tourism-events-industry/strategies/agritourism")
      
    )
    
    if (!is.null(custom_insight)) {
      insight_content <- div(
        h4("Key Finding", style = "font-size: 1.75em; margin: 0 0 10px 0; color: #155724; font-weight: 600;"),
        p(custom_insight, style = "font-size: 1.5em; line-height: 1.6; margin: 0 0 15px 0; color: #155724;")
      )
      
      # Add notes if they exist
      if (!is.null(custom_notes) && custom_notes != "" && custom_notes != "notes placeholder") {
        insight_content <- tagList(
          insight_content,
          hr(style = "border-top: 1px solid #c3e6cb; margin: 15px 0 10px 0;"),
          div(
            strong("Notes:", style = "font-size: 1.25em; color: #155724;"),
            p(custom_notes, style = "font-size: 1.25em; line-height: 1.4; margin: 5px 0 15px 0; color: #155724;")
          )
        )
      }
      
      # Add source
      insight_content <- tagList(
        insight_content,
        hr(style = "border-top: 1px solid #c3e6cb; margin: 15px 0 10px 0;"),
        div(
          style = "font-size: 1.25em; color: #155724;",
          strong("Source: ", style = "color: #155724;"),
          tags$a(
            href = source_info$url,
            target = "_blank",
            source_info$text,
            style = "color: #007bff; text-decoration: none; font-weight: 500;"
          )
        )
      )
      
      return(
        div(class = "comparison-highlight",
            style = "background: linear-gradient(135deg, #e8f5e8, #f0f9f0); border-left: 4px solid #28a745; padding: 20px; border-radius: 8px;",
            div(style = "display: flex; align-items: flex-start; gap: 15px;",
                icon("lightbulb", style = "font-size: 1.5em; color: #28a745; margin-top: 3px;"),
                div(insight_content)
            )
        )
      )
    }
    
    return(div(h4("Agriculture Data Loaded"), p(paste("Showing data for:", display_name))))
  })
  
  # Value boxes - ALWAYS show 2-fold urban/rural regardless of classification
  output$agriculture_urban_rate <- renderValueBox({
    req(agriculture_values$processed_data, input$agriculture_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_agriculture_urban_rural_2fold(agriculture_values$processed_data)
    val <- format_agriculture_value(key_insights$urban, input$agriculture_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    #valueBox(value = val, subtitle = paste("Urban Areas", year), icon = icon("city"), color = "yellow")
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", paste("Urban Areas", year)),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("city"),
      color = "yellow"
    )
    
  })
  
  output$agriculture_rural_rate <- renderValueBox({
    req(agriculture_values$processed_data, input$agriculture_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_agriculture_urban_rural_2fold(agriculture_values$processed_data)
    val <- format_agriculture_value(key_insights$rural, input$agriculture_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    #valueBox(value = val, subtitle = paste("Rural Areas", year), icon = icon("tree"), color = "olive")
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", paste("Rural Areas", year)),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("tree"),
      color = "olive"
    )
    
  })
  
  output$agriculture_scotland_rate <- renderValueBox({
    req(agriculture_values$processed_data, input$agriculture_metric)
    
    scotland_summary <- get_agriculture_scotland_summary(agriculture_values$processed_data, input$agriculture_metric)
    
    if (!is.na(scotland_summary$value)) {
      val <- format_agriculture_value(scotland_summary$value, input$agriculture_metric)
    } else {
      val <- "No Data"
    }
    
    # valueBox(
    #   value = val, 
    #   subtitle = paste(scotland_summary$label, scotland_summary$year), 
    #   icon = icon("flag"), 
    #   color = "maroon"
    # )
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", paste(scotland_summary$label, scotland_summary$year)),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("flag"),
      color = "aqua"
    )
    
    
  })
  
  output$agriculture_urban_rural_gap <- renderValueBox({
    req(agriculture_values$processed_data, input$agriculture_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_agriculture_urban_rural_2fold(agriculture_values$processed_data)
    
    if (!is.na(key_insights$urban) && !is.na(key_insights$rural)) {
      gap <- abs(key_insights$urban - key_insights$rural)
      val <- format_agriculture_value(gap, input$agriculture_metric)
    } else {
      val <- "No Data"
    }
    
    #valueBox(value = val, subtitle = "Urban-Rural Gap", icon = icon("balance-scale"), color = "aqua")
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", "Urban-Rural Gap"),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("balance-scale"),
      color = "maroon"
    )
    
    
  })
  
  # Trend chart with 45-degree rotated year labels
  output$agriculture_trend_chart <- renderPlotly({
    req(agriculture_values$processed_data, input$agriculture_metric)
    
    if (nrow(agriculture_values$processed_data) == 0) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available for selected options", textfont = list(size = 16)))
    }
    
    # Special handling for different metric types
    if (input$agriculture_metric %in% c("Farm Income", "Diversified Activity And Incomes")) {
      # Show all farm types
      farm_data <- agriculture_values$processed_data %>%
        mutate(Year = round(Year), Value_Rounded = round(Value, 1))
      
      p <- ggplot(farm_data, aes(x = Year, y = Value, color = Sub_Metric, group = Sub_Metric)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3, aes(text = paste0("Year: ", paste0(Year-1, "-", substr(Year, 3, 4)), "<br>Farm Type: ", Sub_Metric, "<br>Income: £", scales::comma(Value_Rounded)))) +
        theme_minimal() +
        theme(legend.position = "bottom", 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Year", y = get_agriculture_y_label(input$agriculture_metric), color = "Farm Type") +
        scale_color_viridis_d(option = "plasma", end = 0.8) +
        # edit x axis labels to financial year
        scale_x_continuous(
          breaks = function(x) {
            if (length(x) == 0 || all(is.na(x))) return(c())
            seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
          },
          labels = function(x) {
            paste0(x-1, "-", substr(x, 3, 4))  # e.g., 2022 becomes "2022-23"
          }
        )+
        scale_y_continuous(labels = scales::comma_format(prefix = "£"))
      
    } else if (input$agriculture_metric == "Greenhouse Gas Emissions") {
      # Show both Agriculture and LULUCF if available
      ghg_data <- agriculture_values$processed_data %>%
        mutate(Year = round(Year), Value_Rounded = round(Value, 1))
      
      # filter to remove 1990 and 1995 data - plot as points
      ghg_pre98 <- ghg_data |> filter(Year %in% c(1990, 1995))
      ghg_post98 <- ghg_data |> filter(Year >=1998)
      
      p <- ggplot(ghg_post98, aes(x = Year, y = Value, color = Sub_Metric, group = Sub_Metric)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3, aes(text = paste0("Year: ", Year, "<br>Sector: ", Sub_Metric, "<br>Emissions: ", Value_Rounded, " kt CO2e"))) +
        geom_point(data = ghg_pre98, aes(x = Year, y = Value, color = Sub_Metric, group = Sub_Metric, 
                                         text = paste0("Year: ", Year, "<br>Sector: ", Sub_Metric, "<br>Emissions: ", Value_Rounded, " kt CO2e")), size = 3) +
         theme_minimal() +
        theme(legend.position = "bottom", 
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "Year", y = get_agriculture_y_label(input$agriculture_metric), color = "Sector") +
        scale_color_viridis_d(option = "plasma", end = 0.8) +
        scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
      
    } else {
      # Handle classification-based metrics or time series
      if (input$agriculture_classification_type %in% c("UK", "Scotland")) {
        # Direct plotting without aggregation
        plot_data <- agriculture_values$processed_data %>%
          mutate(Year = round(Year), Value_Rounded = round(Value, 1))
        
        if (input$agriculture_classification_type == "UK") {
          p <- ggplot(plot_data, aes(x = Year, y = Value, color = Area_Type, group = Area_Type)) +
            geom_line(size = 1.2, alpha = 0.8) +
            geom_point(size = 3, aes(text = paste0("Year: ", Year, "<br>Country: ", Area_Type, "<br>Value: ", Value_Rounded))) +
            theme_minimal() +
            theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Year", y = get_agriculture_y_label(input$agriculture_metric), color = "Country") +
            scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
          
          # Apply colors with Scotland as gray
          area_colors <- get_agriculture_colors(unique(plot_data$Area_Type), input$agriculture_classification_type, input$agriculture_metric)
          if (length(area_colors) > 0) {
            p <- p + scale_color_manual(values = unlist(area_colors))
          }
          
          # Edit x-axis labels for financial year
          if (input$agriculture_metric == "New Planting") {
            p <- p +
              scale_x_continuous(
                breaks = function(x) {
                  if (length(x) == 0 || all(is.na(x))) return(c())
                  seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
                },
                labels = function(x) {
                  paste0(x - 1, "-", substr(x, 3, 4))  # e.g., 2022 becomes "2021-22"
                }
              ) +
              scale_y_continuous(labels = scales::comma_format(prefix = "£"))
          }
          
        } else {
          # Scotland only - single line in gray
          tooltip_text <- if (input$agriculture_metric == "New To Crofting") {
            paste0("Period: ", plot_data$Period, "<br>Value: ", plot_data$Value_Rounded)
          } else {
            paste0("Year: ", plot_data$Year, "<br>Value: ", plot_data$Value_Rounded)
          }
          
          p <- ggplot(plot_data, aes(x = Year, y = Value)) +
            geom_line(size = 1.2, alpha = 0.8, color = "#B2B2B2") +
            geom_point(size = 3, aes(text = tooltip_text), color = "#B2B2B2") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Year", y = get_agriculture_y_label(input$agriculture_metric)) +
            scale_x_continuous(breaks = unique(plot_data$Year), labels = unique(plot_data$Year))
          
          # Add period labels for "New To Crofting"
          if (input$agriculture_metric == "New To Crofting") {
            p <- p +
              scale_x_continuous(
                # breaks = function(x) {
                #   if (length(x) == 0 || all(is.na(x))) return(c())
                #   seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
                # },
                labels = unique(plot_data$Period)
              )
          }
        }
      }
       
        else {
        # aggregated data for RESAS classifications
        agg_data <- aggregate_agriculture_by_classification(agriculture_values$processed_data, input$agriculture_classification_type)
        
        if (nrow(agg_data) == 0) {
          return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No aggregated data available", textfont = list(size = 16)))
        }
        
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        agg_data$Year <- round(agg_data$Year)
        
        p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3, aes(text = paste0("Year: ", Year, "<br>Area: ", Area, "<br>Value: ", Value_Rounded))) +
          theme_minimal() +
          theme(legend.position = "bottom", 
                axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(x = "Year", y = get_agriculture_y_label(input$agriculture_metric), color = "Area Type") +
          scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
        
        # Apply colors with Scotland as gray
        area_colors <- get_agriculture_colors(unique(agg_data$Area), input$agriculture_classification_type, input$agriculture_metric)
        if (length(area_colors) > 0) {
          p <- p + scale_color_manual(values = unlist(area_colors))
        }
      }
      
      # Apply appropriate y-axis formatting
      if (grepl("Percentage|%", get_agriculture_y_label(input$agriculture_metric))) {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      } else if (grepl("Million.*Pounds|£m", get_agriculture_y_label(input$agriculture_metric))) {
        p <- p + scale_y_continuous(labels = scales::comma_format(prefix = "£", suffix = ""))
      } else if (grepl("Thousand", get_agriculture_y_label(input$agriculture_metric))) {
        p <- p + scale_y_continuous(labels = scales::comma_format(suffix = ""))
      } else if (grepl("Million.*Hectares", get_agriculture_y_label(input$agriculture_metric))) {
        p <- p + scale_y_continuous(labels = scales::comma_format(suffix = "ha"))
      } else if (grepl("Pounds", get_agriculture_y_label(input$agriculture_metric))) {
        p <- p + scale_y_continuous(labels = scales::comma_format(prefix = "£"))
      } else {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      }
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  # Comparison chart
  output$agriculture_comparison_chart <- renderPlotly({
    req(agriculture_values$processed_data, input$agriculture_classification_type, input$agriculture_selected_year, input$agriculture_metric)
    
    # Skip for metrics that don't show bar charts
    if (!should_show_bar_chart(input$agriculture_metric)) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "Bar chart not applicable for this metric", textfont = list(size = 16)))
    }
    
    # Special handling for Farm Income - show farm types directly
    if (input$agriculture_metric == "Farm Income") {
      selected_data <- agriculture_values$processed_data %>% 
        filter(years_raw == (input$agriculture_selected_year)) %>% # for financial years
        mutate(Value_Rounded = round(Value, 1))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$agriculture_selected_year), textfont = list(size = 16)))
      }
      
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Sub_Metric, Value), fill = Sub_Metric)) +
        geom_col(alpha = 0.8, width = 0.7, aes(text = paste0("Farm Type: ", Sub_Metric, "<br>Income: £", scales::comma(Value_Rounded)))) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1)) +
        labs(y = "", x = "Farm Business Income (£)") +
        scale_fill_viridis_d(option = "plasma", end = 0.8) +
        scale_x_continuous(labels = scales::comma_format(prefix = "£"))
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # Special handling for Greenhouse Gas Emissions - show both sectors
    if (input$agriculture_metric == "Greenhouse Gas Emissions") {
      selected_data <- agriculture_values$processed_data %>% 
        filter(Year == as.numeric(input$agriculture_selected_year)) %>%
        mutate(Value_Rounded = round(Value, 1))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$agriculture_selected_year), textfont = list(size = 16)))
      }
      
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Sub_Metric, Value), fill = Sub_Metric)) +
        geom_col(alpha = 0.8, width = 0.7, aes(text = paste0("Sector: ", Sub_Metric, "<br>Emissions: ", Value_Rounded, " kt CO2e"))) +
        theme_minimal() +
        theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1)) +
        labs(y = "", x = "Greenhouse Gas Emissions (kt CO2e)") +
        scale_fill_viridis_d(option = "plasma", end = 0.8) +
        scale_x_continuous(labels = scales::comma_format())
      
      return(ggplotly(p, tooltip = "text"))
    }
    
    # For all other metrics - use standard aggregated approach
    agg_data <- aggregate_agriculture_by_classification(agriculture_values$processed_data, input$agriculture_classification_type)
    
    if (nrow(agg_data) == 0) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
    }
    
    selected_data <- agg_data %>% filter(Year == as.numeric(input$agriculture_selected_year))
    
    if (nrow(selected_data) == 0) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$agriculture_selected_year), textfont = list(size = 16)))
    }
    
    selected_data$Value_Rounded <- round(selected_data$Value, 1)
    
    # Create tooltip text based on metric type
    tooltip_text <- case_when(
      input$agriculture_metric == "Agri-Environment Schemes" ~ paste0("Country: ", selected_data$Area, "<br>Area: ", selected_data$Value_Rounded, " million hectares"),
      input$agriculture_metric == "Sustainably Managed Woodland" ~ paste0("Country: ", selected_data$Area, "<br>Percentage: ", selected_data$Value_Rounded, "%"),
      input$agriculture_metric == "GVA (Agri, Forestry, Fishing)" ~ paste0("Area: ", selected_data$Area, "<br>GVA: £", selected_data$Value_Rounded, "m"),
      input$agriculture_metric == "Employment (Agri, Forestry, Fishing)" ~ paste0("Area: ", selected_data$Area, "<br>Employment: ", selected_data$Value_Rounded, "%"),
      input$agriculture_metric == "New Planting" ~ paste0("Country: ", selected_data$Area, "<br>Area: ", selected_data$Value_Rounded, " thousand hectares"),
      TRUE ~ paste0("Area: ", selected_data$Area, "<br>Value: ", selected_data$Value_Rounded)
    )
    
    p <- ggplot(selected_data, aes(x = Value, y = reorder(Area, Value), fill = Area)) +
      geom_col(alpha = 0.8, width = 0.7, aes(text = tooltip_text)) +
      theme_minimal() +
      theme(legend.position = "none", axis.text.y = element_text(angle = 0, hjust = 1)) +
      labs(y = "", x = get_agriculture_y_label(input$agriculture_metric))
    
    # Apply colors with Scotland as gray
    area_colors <- get_agriculture_colors(unique(selected_data$Area), input$agriculture_classification_type, input$agriculture_metric)
    if (length(area_colors) > 0) {
      p <- p + scale_fill_manual(values = unlist(area_colors))
    }
    
    # Apply appropriate x-axis formatting based on metric type
    if (grepl("Percentage|%", get_agriculture_y_label(input$agriculture_metric))) {
      p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
    } else if (grepl("Million.*Pounds|£m", get_agriculture_y_label(input$agriculture_metric))) {
      p <- p + scale_x_continuous(labels = scales::comma_format(prefix = "£", suffix = "m"))
    } else if (grepl("Thousand", get_agriculture_y_label(input$agriculture_metric))) {
      p <- p + scale_x_continuous(labels = scales::comma_format(suffix = "k"))
    } else if (grepl("Million.*Hectares", get_agriculture_y_label(input$agriculture_metric))) {
      p <- p + scale_x_continuous(labels = scales::comma_format(suffix = "m ha"))
    } else if (grepl("kt CO2e", get_agriculture_y_label(input$agriculture_metric))) {
      p <- p + scale_x_continuous(labels = scales::comma_format())
    } else if (grepl("Pounds", get_agriculture_y_label(input$agriculture_metric))) {
      p <- p + scale_x_continuous(labels = scales::comma_format(prefix = "£"))
    } else {
      p <- p + scale_x_continuous(labels = scales::comma_format())
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  # Download handlers matching culture module
 
   # trend download
  output$agriculture_trend_download <- downloadHandler(
    filename = function() {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$agriculture_metric)
      paste0("Agriculture_Trend_", metric_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(agriculture_values$processed_data)
      
      download_data <- agriculture_values$processed_data %>%
        if(input$housing_metric %in% c("Farm Income", "Diversified Activity And Incomes")){
            select(years_raw, Area_Type, Value, Data_Source) %>%
            arrange(`years_raw`, Area_Type)}
      else{
        select(Year, Area_Type, Value, Data_Source) %>%
        arrange(Year, Area_Type)}
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # comparison download
  
  output$agriculture_comparison_download <- downloadHandler(
    filename = function() {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$agriculture_metric)
      paste0("Agriculture_Comparison_", metric_name, "_", input$agriculture_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(agriculture_values$processed_data, input$agriculture_selected_year)
      
      if (input$agriculture_metric == "Farm Income") {
        download_data <- agriculture_values$processed_data %>%
          filter(years_raw == input$agriculture_selected_year) %>%
          select(Year = years_raw, Sub_Metric, Value, Data_Source) %>%
          arrange(Sub_Metric)} else if (input$agriculture_metric =="Greenhouse Gas Emissions") {
        download_data <- agriculture_values$processed_data %>%
          filter(Year == as.numeric(input$agriculture_selected_year)) %>%
          select(Year, Sub_Metric, Value, Data_Source) %>%
          arrange(Sub_Metric)
      } else {
        agg_data <- aggregate_agriculture_by_classification(agriculture_values$processed_data, input$agriculture_classification_type)
        download_data <- agg_data %>%
          if(input$agriculture_metric == "Diversified Activity And Incomes"){filter(years_raw == input$agriculture_selected_year) %>%
          select(Year = years_raw, Area, Value, Data_Source)} 
        else{filter(Year == as.numeric(input$agriculture_selected_year)) %>%
            select(Year, Area, Value, Data_Source)} %>%
          arrange(Area)
      }
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # table download
  output$agriculture_table_download <- downloadHandler(
    filename = function() {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$agriculture_metric)
      paste0("Agriculture_Table_", metric_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(agriculture_values$processed_data)
      
      if (input$agriculture_metric %in% c("Farm Income", "Greenhouse Gas Emissions", "New To Crofting",
                                          "Diversified Activity And Incomes", "Resident Crofters", 
                                          "Agritourism Value", "Fish Stocks")) {
        
        if (input$agriculture_metric %in% c("Farm Income", "Diversified Activity And Incomes")) {
          download_data <- agriculture_values$processed_data %>%
            select(Year = years_raw, Area_Type, Value, Sub_Metric, Data_Source) %>%
            arrange(desc(Year), Area_Type)
        } else {
          download_data <- agriculture_values$processed_data %>%
            select(Year, Area_Type, Value, Sub_Metric, Data_Source) %>%
            arrange(desc(Year), Area_Type)
        }
        
      } else {
        agg_data <- aggregate_agriculture_by_classification(agriculture_values$processed_data, 
                                                            input$agriculture_classification_type)
        
        
        if (input$agriculture_metric == "New Planting"){ 
        download_data <- agg_data %>%
          select(Year = years_raw, Area, Value, Data_Source) %>%
          arrange(desc(Year), Area)}
        
        else{ download_data <- agg_data %>%
          select(Year, Area, Value, Data_Source) %>%
          arrange(desc(Year), Area)
        }
      }
      
      if (!is.null(input$agriculture_table_year_filter) && input$agriculture_table_year_filter != "all") {
        download_data <- download_data %>% filter(Year == as.numeric(input$agriculture_table_year_filter))
      }
      
      if (!is.null(input$agriculture_table_area_filter) && input$agriculture_table_area_filter != "all") {
        if ("Area" %in% names(download_data)) {
          download_data <- download_data %>% filter(Area == input$agriculture_table_area_filter)
        } else if ("Area_Type" %in% names(download_data)) {
          download_data <- download_data %>% filter(Area_Type == input$agriculture_table_area_filter)
        }
      }
      
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  # Data table matching culture module format  
  output$agriculture_data_table <- DT::renderDataTable({
    req(agriculture_values$processed_data)
    
    if (nrow(agriculture_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_agriculture_metric_display_name(input$agriculture_metric)
    
    # Special handling for metrics with sub-metrics or different structures
    if (input$agriculture_metric == "Farm Income") {
      # Show all farm types in the table
      table_data <- agriculture_values$processed_data %>%
        arrange(desc(Year), Sub_Metric) %>%
        select(Year = years_raw, Sub_Metric, Value) %>%
        mutate(Value = round(Value, 0)) %>%
        rename(`Farm Type` = Sub_Metric, `Income (£)` = Value)
      
      
      # Apply filters if any
      if (!is.null(input$agriculture_table_year_filter) && input$agriculture_table_year_filter != "all") {
        table_data <- table_data %>% filter(Year == input$agriculture_table_year_filter)
      }
      
      dt <- DT::datatable(table_data, 
                          options = list(pageLength = 15, scrollX = TRUE),
                          caption = paste("Data for:", display_name))
      
      dt <- dt %>% formatCurrency(columns = "Income (£)", currency = "£", digits = 0)
      
    } else if (input$agriculture_metric == "Greenhouse Gas Emissions") {
      # Show both sectors with full table
      table_data <- agriculture_values$processed_data %>%
        arrange(desc(Year), Sub_Metric) %>%
        select(Year, Sub_Metric, Value) %>%
        mutate(Value = round(Value, 1)) %>%
        rename(Sector = Sub_Metric, `Emissions (kt CO2e)` = Value)
      
      # Apply filters if any
      if (!is.null(input$agriculture_table_year_filter) && input$agriculture_table_year_filter != "all") {
        table_data <- table_data %>% filter(Year == as.numeric(input$agriculture_table_year_filter))
      }
      
      dt <- DT::datatable(table_data, 
                          options = list(pageLength = 15, scrollX = TRUE),
                          caption = paste("Data for:", display_name))
      
      dt <- dt %>% formatRound(columns = "Emissions (kt CO2e)", digits = 1)
      
    } else if (input$agriculture_metric == "New To Crofting") {
      # Show periods chronologically
      table_data <- agriculture_values$processed_data %>%
        arrange(Year) %>%
        select(Period, Value) %>%
        mutate(Value = round(Value, 0)) %>%
        rename(`Number` = Value)
      
      dt <- DT::datatable(table_data, 
                          options = list(pageLength = 15, scrollX = TRUE),
                          caption = paste("Data for:", display_name))
      
      dt <- dt %>% formatRound(columns = "Number", digits = 0)
      
    } else if (input$agriculture_metric %in% c("Diversified Activity And Incomes", "Resident Crofters", "Agritourism Value", "Fish Stocks")) {
      # Simple single-metric table
      table_data <- agriculture_values$processed_data %>%
        arrange(desc(Year)) %>%
        {
          if (input$agriculture_metric == "Diversified Activity And Incomes") {
            select(., Year = years_raw, Area_Type, Value)
          } else {
            select(., Year, Area_Type, Value)
          }
        } %>%
        mutate(Value = round(Value, 1)) %>%
        rename(Area = Area_Type)
      # Set appropriate column name for value
      value_col_name <- case_when(
        input$agriculture_metric == "Diversified Activity And Incomes" ~ "Value",
        input$agriculture_metric == "Resident Crofters" ~ "Percentage (%)",
        input$agriculture_metric == "Agritourism Value" ~ "Value (£m)",
        input$agriculture_metric == "Fish Stocks" ~ "Percentage (%)",
        TRUE ~ "Value"
      )
      
      names(table_data)[names(table_data) == "Value"] <- value_col_name
      
      # Apply filters if any
      if (!is.null(input$agriculture_table_year_filter) && input$agriculture_table_year_filter != "all") {
        table_data <- table_data %>% filter(Year == input$agriculture_table_year_filter)
      }
      
      dt <- DT::datatable(table_data, 
                          options = list(pageLength = 15, scrollX = TRUE),
                          caption = paste("Data for:", display_name))
      
      if (grepl("£m", value_col_name)) {
        dt <- dt %>% formatCurrency(columns = value_col_name, currency = "£", digits = 1)
      } else {
        dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
      }
      
    } else {
      # Standard aggregated table
      if (is.null(input$agriculture_classification_type)) return(NULL)
      
      agg_data <- aggregate_agriculture_by_classification(agriculture_values$processed_data, input$agriculture_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
      }
      
      filtered_data <- agg_data
      
      if (!is.null(input$agriculture_table_year_filter) && input$agriculture_table_year_filter != "all") {
        filtered_data <- filtered_data %>% filter(Year == input$agriculture_table_year_filter)
        
        
        
      }
      
      if (!is.null(input$agriculture_table_area_filter) && input$agriculture_table_area_filter != "all") {
        filtered_data <- filtered_data %>% filter(Area == input$agriculture_table_area_filter)
      }
      
      if (nrow(filtered_data) == 0) {
        return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
      }
      
      filtered_data <- filtered_data %>%
        arrange(desc(Year), Area) %>%
        {
          if (input$agriculture_metric =="Diversified Activity And Income") {
            select(., Year = years_raw, Area, Value, Data_Source)
          } else {
            select(., Year, Area, Value, Data_Source)
          }
        } %>%
        mutate(Value = round(Value, 1))
      

      # Set appropriate column name for value based on metric
      value_col_name <- case_when(
        input$agriculture_metric == "Agri-Environment Schemes" ~ "Area (Million Hectares)",
        input$agriculture_metric == "Sustainably Managed Woodland" ~ "Percentage (%)",
        input$agriculture_metric == "GVA (Agri, Forestry, Fishing)" ~ "GVA (£m)",
        input$agriculture_metric == "Employment (Agri, Forestry, Fishing)" ~ "Employment (%)",
        input$agriculture_metric == "GVA (Agri, Forestry, Fishing)" ~ "Area (Thousand Hectares)",
        TRUE ~ "Value"
      )
      
      names(filtered_data)[names(filtered_data) == "Value"] <- value_col_name
      
      dt <- DT::datatable(filtered_data, 
                          options = list(pageLength = 15, scrollX = TRUE),
                          caption = paste("Data for:", display_name, "by", input$agriculture_classification_type, "Classification"))
      
      # Apply appropriate formatting
      if (grepl("Percentage|%", value_col_name)) {
        dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
      } else if (grepl("£m", value_col_name)) {
        dt <- dt %>% formatCurrency(columns = value_col_name, currency = "£", digits = 1)
      } else if (grepl("Million Hectares", value_col_name)) {
        dt <- dt %>% formatRound(columns = value_col_name, digits = 2)
      } else if (grepl("Thousand Hectares", value_col_name)) {
        dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
      } else {
        dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
      }
    }
    
    return(dt)
  })
}
# Module UI and Server
agriculture_ui <- function(id) {
  ns <- NS(id)
  agriculture_dashboard_ui(id)
}

agriculture_server_module <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    agriculture_server(id, values, session)
  })
}
