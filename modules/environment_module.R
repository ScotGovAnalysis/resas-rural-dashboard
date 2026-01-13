
# ===========================================
# ENVIRONMENT DASHBOARD - PART 1: DATA FUNCTIONS AND CONFIGURATIONS
# ===========================================

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(plotly)
library(DT)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(scales)

# UPDATED ENVIRONMENT METRICS LIST 
environment_metrics <- list(
  "Percentage of household waste recycled" = list(
    files = c("environment/waste_recycled.xlsx"),
    classifications = c("4-fold"),  
    full_name = "Percentage (%) of household waste recycled",
    include_scotland = TRUE,
    has_urban_rural = TRUE
  ),
  "Renewable Electricity" = list(
    files = "environment/renewable_generation.xlsx",
    classifications = c("4-fold"),  
    full_name = "Renewable Electricity Generation (MWh per household)",
    include_scotland = TRUE,
    units = "MWh per household",
    has_urban_rural = TRUE
  ),
  "Clean Seas" = list(
    files = "environment/clean_seas.xlsx",
    classifications = c("Scotland"),
    full_name = "Clean Seas: the percentage of biogeographic regions with acceptably low levels of contaminants",
    include_scotland = TRUE,
    no_bar_chart = TRUE,
    has_urban_rural = FALSE
  ),
  "Biodiversity: Scottish species" = list(
    files = "environment/biodiversity_species.xlsx",
    classifications = c("Marine Species Abundance", "Terrestrial Species Abundance", "Terrestrial Species Occupancy"),
    full_name = "Biodiversity: combination of three indices of Scottish species - abundance of marine species, abundance of terrestrial species and occupancy of terrestrial species",
    include_scotland = TRUE,
    no_bar_chart = TRUE,
    has_urban_rural = FALSE
  ),
  "Fresh water" = list(
    files = "environment/fresh_water.xlsx",
    classifications = c("Water Quality", "Water Resources (flows and levels)", "Access For Fish Migration", "Physical Condition"),
    full_name = "Fresh Water condition: The percentage of river and loch waterbodies achieving 'Good' or better status in terms of four metrics; water quality, water resources (flows and levels), access to fish migration and physical condition",
    include_scotland = TRUE,
    no_bar_chart = TRUE,
    has_urban_rural = FALSE
  ),
  "Restored peatland" = list(
    files = "environment/peatland.xlsx",
    classifications = c("Scotland"),
    full_name = "Hectares of restored peatland: Cumulative area of peatland restored since 2012 (thousands hectares)",
    include_scotland = TRUE,
    units = "thousands hectares",
    no_bar_chart = TRUE,
    has_urban_rural = FALSE
  ),
  "Biodiversity awareness" = list(
    files = "environment/biodiversity_awareness.xlsx",
    classifications = c("Not aware", "Not engaged", "Some engagement", "High engagement"),
    full_name = "Awareness, understanding and support for biodiversity conservation",
    include_scotland = TRUE,
    no_bar_chart = TRUE,
    has_urban_rural = FALSE
  ),
  "Rare and threatened species" = list(
    files = "environment/rare_species.xlsx",
    classifications = c("Scotland"),
    full_name = "Status of rare and threatened species (UK Red List Index)",
    include_scotland = TRUE,
    no_bar_chart = TRUE,
    has_urban_rural = FALSE,
    accentuate_difference = TRUE
  ),
  "Forests and woodlands" = list(
    files = "environment/woodlands.csv",
    classifications = c("Scotland"),
    full_name = "Woodland area by forest type and ownership (thousand hectares)",
    include_scotland = TRUE,
    units = "thousands hectares",
    has_sub_metrics = TRUE,
    no_bar_chart = TRUE,
    has_urban_rural = FALSE
  )
)

# Static Key Insights for environment metrics
environment_key_insights <- list(
  "Percentage of household waste recycled" = "Overall the percentage of generated household waste recycled has increased between 2011 and 2023 for all remote/rural classifications.",
  
  "Renewable Electricity" = "Renewable energy generation in Scotland has increased substantially between 2014 and 2023, with mainly rural areas providing the largest overall proportion of renewable electricity generated.",
  
  "Clean Seas" = "The percentage of biogeographic regions with acceptably low levels of contaminants increased from 75% to 79% between 2015 and 2020 in Scotland.",
  
  "Biodiversity: Scottish species" = "The percentage of river and loch waterbodies in Scotland achieving ‘Good’ or better status for water quality increased from 80.3% to 83.9% between 2016 and 2020.",
  
  "Fresh water" = "The percentage of river and loch waterbodies in Scotland achieving ‘Good’ or better status for water quality increased from 80.3% to 83.9% between 2016 and 2020.",
  
  "Restored peatland" = "There has been a significant and consistent increase in the cumulative area of peatland restored since 2012, growing from 0.27 thousand hectares in 2013 to 43.79 thousand hectares by 2023.",
  
  "Biodiversity awareness" = "Between 2014 and 2018, awareness of and engagement with biodiversity has improved in Scotland: the proportion of people with high engagement increased from 12.5% to 20.3%, while those not aware decreased from 42.8% to 37%.",
  
  "Rare and threatened species" = "From 2000 to 2023 the UK Red List Index has declining only slightly from 0.965 to 0.962, indicating a slow but persistent deterioration in the status of rare and threatened species over this period.",
  
  "Forests and woodlands" = "The area of woodland in Scotland has steadily increased from 1.3 million hectares in 1998 to 1.51 million hectares in 2024, representing a growth of about 16% over 26 years."
)

#  Notes for environment metrics
environment_notes <- list(
  "Percentage of household waste recycled" = "1. Figures are taken from most recent data available due to yearly revisions. 2. Data from 2011-2013 has been revised to account for changes to recycling definitions.",
  
  "Renewable Electricity" = "1. Regional totals do not sum to the Scotland total as some local authority data is supressed to prevent the output of local plants being revealed. 2. Suppressed data makes up a large proportion of the total electricity generated for Scotland from 2019 onwards. Comparisons between regions or trends over time should be made with caution.",
  
  "Clean Seas" = "",
  
  "Biodiversity: Scottish species" = "No update was available for terrestrial occupancy from 2016 onwards",
  
  "Fresh water" = "",
  
  "Restored peatland" = "1. Original data source last updated in July 2025 with data to March 2025. 2. The indicator reports the cumulative area of peatland assessed as ‘on the road to restoration’ each year. The Scottish Government cumulative target of 250,000 hectares is to be delivered by 2030.",
  
  "Biodiversity awareness" = "",
  
  "Rare and threatened species" = "1. Indicator description: The Red List Index is based on global estimates of the extinction risk (IUCN Red List categories) of all mammals, birds, amphibians, corals and cycads, derived from local and national data, disaggregated to the national scale and weighted by the proportion of each species's distribution in the country or region (in this case the UK).

The Red List Index reported here is based on global classifications for each species. In other words this Index does not indicate risk of extinction within the UK, but rather, risk of global extinction of species found within the UK. 2. 2. IUCN Red List: The IUCN Red List of Threatened Species is internationally recognised as the most respected and robust inventory of global species conservation status. It provides a standard and repeatable method for assessing the extinction risk status of thousands of animal, fungus and plant species.
  3. Red List Index: The Red List Index value ranges from 1 (all species are categorised as ‘Least Concern’) to 0 (all species are categorised as ‘Extinct’), thus indicating how far the set of species has moved overall towards extinction. A downward trend in the Red List Index over time means that the expected rate of future species extinctions is worsening (i.e., the rate of biodiversity loss is increasing). An upward trend means that the expected rate of species extinctions is abating (i.e., the rate of biodiversity loss is decreasing), and a horizontal line means that the expected rate of species extinctions remains constant, although in each of these cases it does not mean that biodiversity loss has stopped. An upward Red List Index trend would indicate that the SDG Target 15.5 of reducing the degradation of natural habitats and protecting threatened species is on track. A Red List Index value of 1 would indicate that biodiversity loss has been halted.
   4. Data last updated May 2023",
  
  "Forests and woodlands" = ""
)

# Sub-metrics definitions
biodiversity_species_sub_metrics <- c(
  "Marine Species Abundance" = "Marine Abundance",
  "Terrestrial Species Abundance" = "Terrestrial Abundance", 
  "Terrestrial Species Occupancy" = "Terrestrial Occupancy "
)

freshwater_sub_metrics <- c(
  "Water Quality" = "Water quality ",
  "Water Resources (flows and levels)" = "Water resources flows and levels ",
  "Access For Fish Migration" = "Access for fish migration ",
  "Physical Condition" = "Physical Condition "
)

biodiversity_awareness_sub_metrics <- c(
  "Not aware" = "Not aware",
  "Not engaged" = "Not engaged",
  "Some engagement" = "Some engagement",
  "High engagement" = "High engagement"
)

woodlands_sub_metrics <- c(
  "Scotland total" = "Scotland total",
  "Private sector broadleaves" = "Private sector broadleaves",
  "Public sector broadleaves" = "Public sector broadleaves",
  "Private sector conifers" = "Private sector conifers",
  "Public sector conifers" = "Public sector conifers"
)

# DATA LOADING FUNCTIONS

# Function to load waste recycling data with proper debugging
load_waste_recycling_data <- function() {
  filepath <- "environment/waste_recycled.xlsx"
  cat("Attempting to load waste recycling data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath, skip = 1)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      region_name <- as.character(raw_data[i, 1])
      
      for (year_col in year_columns) {
        value <- as.numeric(raw_data[i, year_col])
        year <- as.numeric(year_col)
        
        if (!is.na(value) && !is.na(year)) {
          value_percent <- value * 100
          
          if (region_name == "SCOTLAND") {
            new_row <- data.frame(
              Year = year,
              Area = "Scotland",
              Value = value_percent,
              Data_Source = "Scottish Government - Household Waste Recycling",
              stringsAsFactors = FALSE
            )
          } else {
            # Map to 4-fold RESAS classifications
            area_4fold <- case_when(
              region_name == "Islands & Remote Rural" ~ "Islands & Remote Rural",
              region_name == "Mainly Rural" ~ "Mainly Rural",
              region_name == "Urban with Substantial Rural areas" ~ "Urban with Substantial Rural areas",
              region_name == "Larger Cities" ~ "Larger Cities",
              TRUE ~ as.character(region_name)
            )
            
            new_row <- data.frame(
              Year = year,
              Area = area_4fold,
              Value = value_percent,
              Data_Source = "Scottish Government - Household Waste Recycling",
              stringsAsFactors = FALSE
            )
          }
          
          processed_data <- rbind(processed_data, new_row)
        }
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "waste recycling records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading waste recycling data:", e$message, "\n")
    return(data.frame())
  })
}

# Load renewable electricity data 
load_renewable_electricity_data <- function() {
  #filepath <- "environment/renewable_generation.xlsx" until data fix
  filepath <- ""
  cat("Attempting to load renewable electricity data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      region_name <- as.character(raw_data[i, 1])
      
      for (year_col in year_columns) {
        generation_mwh <- as.numeric(raw_data[i, year_col])
        year <- as.numeric(year_col)
        
        if (!is.na(generation_mwh) && !is.na(year) && generation_mwh > 0) {
          
          # Approximate household numbers
          households <- case_when(
            region_name == "SCOTLAND" ~ 2500000,
            region_name == "Islands & Remote Rural" ~ 120000,
            region_name == "Mainly Rural" ~ 800000, 
            region_name == "Urban with Substantial Rural Areas" ~ 900000,
            region_name == "Larger Cities" ~ 680000,
            TRUE ~ 100000
          )
          
          mwh_per_household <- generation_mwh / households
          
          if (region_name == "SCOTLAND") {
            new_row <- data.frame(
              Year = year,
              Area = "Scotland",
              Value = mwh_per_household,
              Data_Source = "Scottish Government - Renewable Electricity Generation",
              stringsAsFactors = FALSE
            )
          } else {
            # Map to 4-fold RESAS classifications
            area_4fold <- case_when(
              region_name == "Islands & Remote Rural" ~ "Islands & Remote Rural",
              region_name == "Mainly Rural" ~ "Mainly Rural",
              region_name == "Urban with Substantial Rural Areas" ~ "Urban with Substantial Rural areas",
              region_name == "Larger Cities" ~ "Larger Cities",
              TRUE ~ as.character(region_name)
            )
            
            new_row <- data.frame(
              Year = year,
              Area = area_4fold,
              Value = mwh_per_household,
              Data_Source = "Scottish Government - Renewable Electricity Generation",
              stringsAsFactors = FALSE
            )
          }
          
          processed_data <- rbind(processed_data, new_row)
        }
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "renewable electricity records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading renewable electricity data:", e$message, "\n")
    return(data.frame())
  })
}

# Load clean seas data
load_clean_seas_data <- function() {
  filepath <- "environment/clean_seas.xlsx"
  cat("Attempting to load clean seas data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath, skip = 1)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) < 1) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    year_columns <- year_columns[!is.na(year_columns) & year_columns != ""]
    
    processed_data <- data.frame()
    
    for (year_col in year_columns) {
      year_val <- as.numeric(year_col)
      if (is.na(year_val)) next
      
      percentage_val <- as.numeric(raw_data[1, year_col])
      if (is.na(percentage_val)) next
      
      new_row <- data.frame(
        Year = year_val,
        Area = "Scotland",
        Value = percentage_val,
        Data_Source = "Scottish Government - Clean Seas",
        stringsAsFactors = FALSE
      )
      
      processed_data <- bind_rows(processed_data, new_row)
    }
    
    cat("Successfully processed", nrow(processed_data), "clean seas records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading clean seas data:", e$message, "\n")
    return(data.frame())
  })
}

# Load biodiversity species data
load_biodiversity_species_data <- function() {
  filepath <- "environment/biodiversity_species.xlsx"
  cat("Attempting to load biodiversity species data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath, skip = 2)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      measure_name <- as.character(raw_data[i, 1])
      
      for (year_col in year_columns) {
        value <- as.numeric(raw_data[i, year_col])
        year <- as.numeric(year_col)
        
        if (!is.na(value) && !is.na(year)) {
          new_row <- data.frame(
            Year = year,
            Area = measure_name,
            Value = value,
            Activity = measure_name,
            Data_Source = "Scottish Government - Biodiversity Species",
            stringsAsFactors = FALSE
          )
          
          processed_data <- rbind(processed_data, new_row)
        }
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "biodiversity species records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading biodiversity species data:", e$message, "\n")
    return(data.frame())
  })
}

# Load fresh water data 
load_fresh_water_data <- function() {
  filepath <- "environment/fresh_water.xlsx"
  cat("Attempting to load fresh water data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    # Read the Excel file, skipping title row
    raw_data <- read_excel(filepath, skip = 1)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    # Show exact column names with their character representation
    cat("Exact column names with quotes:\n")
    for (i in 1:ncol(raw_data)) {
      col_name <- names(raw_data)[i]
      cat(paste0("  Column ", i, ": \"", col_name, "\" (length: ", nchar(col_name), ")\n"))
    }
    
    if (nrow(raw_data) == 0) {
      cat("No data found in Excel file\n")
      return(data.frame())
    }
    
    # Print first few rows for debugging
    cat("First 3 rows of raw data:\n")
    print(head(raw_data, 3))
    
    # Get the metric columns (everything except Year) - use exact column names
    metric_columns <- names(raw_data)[2:ncol(raw_data)]
    cat("Metric columns found:\n")
    for (i in 1:length(metric_columns)) {
      cat(paste0("  \"", metric_columns[i], "\"\n"))
    }
    
    processed_data <- data.frame()
    
    # Process each year (row) for each metric
    for (i in 1:nrow(raw_data)) {
      year <- as.numeric(raw_data[i, 1])  # First column should be Year
      if (is.na(year)) {
        cat("Skipping row", i, "- invalid year:", raw_data[i, 1], "\n")
        next
      }
      
      cat("Processing year:", year, "\n")
      
      for (metric_col in metric_columns) {
        value <- as.numeric(raw_data[i, metric_col])
        
        if (!is.na(value)) {
          new_row <- data.frame(
            Year = year,
            Area = metric_col,  # Use the EXACT column name with spaces
            Value = value,
            Activity = metric_col,
            Data_Source = "Scottish Government - Fresh Water Condition",
            stringsAsFactors = FALSE
          )
          
          processed_data <- rbind(processed_data, new_row)
          cat("Added row: Year=", year, "Activity=\"", metric_col, "\" Value=", value, "\n")
        } else {
          cat("Skipping NA value for year", year, "metric \"", metric_col, "\"\n")
        }
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "fresh water records\n")
    if (nrow(processed_data) > 0) {
      cat("Year range:", min(processed_data$Year), "to", max(processed_data$Year), "\n")
      cat("Exact Activities in processed data:\n")
      unique_activities <- unique(processed_data$Activity)
      for (i in 1:length(unique_activities)) {
        cat(paste0("  \"", unique_activities[i], "\"\n"))
      }
      cat("Value range:", round(min(processed_data$Value), 1), "% to", round(max(processed_data$Value), 1), "%\n")
      
      # Show sample of processed data
      cat("Sample processed data:\n")
      print(head(processed_data, 8))
    } else {
      cat("WARNING: No records were processed!\n")
    }
    
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading fresh water data:", e$message, "\n")
    cat("Error details:", toString(e), "\n")
    return(data.frame())
  })
}

# Load peatland data
load_peatland_data <- function() {
  #filepath <- "environment/peatland.xlsx" # until fix
  filepath <- ""
  cat("Attempting to load peatland data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath, skip = 1)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    processed_data <- data.frame()
    
    for (year_col in year_columns) {
      value <- as.numeric(raw_data[1, year_col])
      year <- as.numeric(year_col)
      
      if (!is.na(value) && !is.na(year)) {
        new_row <- data.frame(
          Year = year,
          Area = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - Peatland Restoration",
          stringsAsFactors = FALSE
        )
        
        processed_data <- rbind(processed_data, new_row)
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "peatland records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading peatland data:", e$message, "\n")
    return(data.frame())
  })
}

# Load biodiversity awareness data
load_biodiversity_awareness_data <- function() {
  filepath <- "environment/biodiversity_awareness.xlsx"
  cat("Attempting to load biodiversity awareness data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath, skip = 1)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      engagement_level <- as.character(raw_data[i, 1])
      
      for (year_col in year_columns) {
        value <- as.numeric(raw_data[i, year_col])
        year <- as.numeric(year_col)
        
        if (!is.na(value) && !is.na(year)) {
          new_row <- data.frame(
            Year = year,
            Area = engagement_level,
            Value = value,
            Activity = engagement_level,
            Data_Source = "Scottish Government - Biodiversity Awareness",
            stringsAsFactors = FALSE
          )
          
          processed_data <- rbind(processed_data, new_row)
        }
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "biodiversity awareness records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading biodiversity awareness data:", e$message, "\n")
    return(data.frame())
  })
}

# Load rare species data
load_rare_species_data <- function() {
  filepath <- "environment/rare_species.xlsx"
  cat("Attempting to load rare species data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read_excel(filepath, skip = 1)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    year_columns <- names(raw_data)[2:ncol(raw_data)]
    processed_data <- data.frame()
    
    for (year_col in year_columns) {
      value <- as.numeric(raw_data[1, year_col])
      year <- as.numeric(year_col)
      
      if (!is.na(value) && !is.na(year)) {
        new_row <- data.frame(
          Year = year,
          Area = "Scotland",
          Value = value,
          Data_Source = "Scottish Government - Rare and Threatened Species",
          stringsAsFactors = FALSE
        )
        
        processed_data <- rbind(processed_data, new_row)
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "rare species records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading rare species data:", e$message, "\n")
    return(data.frame())
  })
}

# Load woodlands data
load_woodlands_data <- function() {
  filepath <- "environment/woodlands.csv"
  cat("Attempting to load woodlands data from:", filepath, "\n")
  
  if (!file.exists(filepath)) {
    cat("ERROR: File does not exist at:", filepath, "\n")
    return(data.frame())
  }
  
  tryCatch({
    raw_data <- read.csv(filepath, skip = 4, stringsAsFactors = FALSE)
    cat("Raw data loaded with", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    names(raw_data) <- c("Year", "Private_Broadleaves", "Public_Broadleaves", 
                        "Private_Conifers", "Public_Conifers", "Scotland_Total", "Note")
    
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      year <- as.numeric(raw_data[i, "Year"])
      if (is.na(year)) next
      
      woodland_types <- list(
        "Scotland total" = raw_data[i, "Scotland_Total"],
        "Private sector broadleaves" = raw_data[i, "Private_Broadleaves"],
        "Public sector broadleaves" = raw_data[i, "Public_Broadleaves"],
        "Private sector conifers" = raw_data[i, "Private_Conifers"],
        "Public sector conifers" = raw_data[i, "Public_Conifers"]
      )
      
      for (woodland_type in names(woodland_types)) {
        value <- as.numeric(gsub(",", "", woodland_types[[woodland_type]]))
        
        if (!is.na(value)) {
          new_row <- data.frame(
            Year = year,
            Area = "Scotland",
            Value = value,
            Activity = woodland_type,
            Data_Source = "Scottish Government - Woodland Area",
            stringsAsFactors = FALSE
          )
          
          processed_data <- rbind(processed_data, new_row)
        }
      }
    }
    
    cat("Successfully processed", nrow(processed_data), "woodland records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("ERROR loading woodland data:", e$message, "\n")
    return(data.frame())
  })
}

# HELPER FUNCTIONS

# Function to get display name for metrics
get_environment_metric_display_name <- function(metric_name) {
  metric_info <- environment_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# Load environment data function
load_environment_data_simple <- function(metric_name, classification_type, selected_activity = NULL) {
  metric_info <- environment_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  if (metric_name == "Percentage of household waste recycled") {
    return(load_waste_recycling_data())
  } else if (metric_name == "Renewable Electricity") {
    return(load_renewable_electricity_data())
  } else if (metric_name == "Clean Seas") {
    return(load_clean_seas_data())
  } else if (metric_name == "Biodiversity: Scottish species") {
    # No sub-metric filtering - load all data to show as multiple lines
    return(load_biodiversity_species_data())
  } else if (metric_name == "Fresh water") {
    # No sub-metric filtering - load all data to show as multiple lines
    return(load_fresh_water_data())
  } else if (metric_name == "Restored peatland") {
    return(load_peatland_data())
  } else if (metric_name == "Biodiversity awareness") {
    # No sub-metric filtering - load all data to show as multiple lines
    return(load_biodiversity_awareness_data())
  } else if (metric_name == "Rare and threatened species") {
    return(load_rare_species_data())
  } else if (metric_name == "Forests and woodlands") {
    data <- load_woodlands_data()
    if (!is.null(selected_activity) && "Activity" %in% names(data)) {
      return(data %>% filter(Activity == selected_activity))
    }
    return(data)
  }
  
  return(data.frame())
}

# Simple aggregate function 
simple_aggregate_environment_data <- function(processed_data, classification_type = "4-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  # For Scotland-only metrics, just pass through
  if(all(processed_data$Area %in% c("Scotland", names(biodiversity_species_sub_metrics), names(freshwater_sub_metrics), names(biodiversity_awareness_sub_metrics)))) {
    return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
  }
  
  # For 4-fold data, pass through as-is
  return(processed_data %>%
         group_by(Year, Area, Data_Source) %>%
         summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}

# Get key insights - matching culture structure for 4-fold data 
get_simple_environment_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "4-fold", selected_activity = NULL) {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  cat("Processing environment key insights for:", metric_name, "\n")
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  # Initialize values
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  # Case 1: Scotland only metrics
  if (all(unique_areas == "Scotland")) {
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
    return(list(urban = NA, rural = NA, scotland = scotland_val, year = latest_year))
  }
  
  # Case 2: 4-fold RESAS areas - aggregate to urban/rural
  if (any(c("Larger Cities", "Urban with Substantial Rural areas", "Mainly Rural", "Islands & Remote Rural") %in% unique_areas)) {
    cat("Data in 4-fold RESAS format, aggregating to 2-fold\n")
    
    # Urban = Larger Cities + Urban with Substantial Rural areas
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Larger Cities", "Urban with Substantial Rural areas"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    # Rural = Mainly Rural + Islands & Remote Rural
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Mainly Rural", "Islands & Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    # Scotland = all areas average
    scotland_areas <- latest_data %>% 
      filter(!grepl("Scotland", Area, ignore.case = TRUE))
    if(nrow(scotland_areas) > 0) {
      scotland_val <- mean(scotland_areas$Value, na.rm = TRUE)
    }
  }
  
  cat("Key insights for year", latest_year, "- Urban:", urban_val, "Rural:", rural_val, "Scotland:", scotland_val, "\n")
  
  return(list(
    urban = ifelse(!is.na(urban_val), urban_val, NA),
    rural = ifelse(!is.na(rural_val), rural_val, NA),
    scotland = ifelse(!is.na(scotland_val), scotland_val, NA),
    year = latest_year
  ))
}

# COLOR MAPPING FUNCTION 
get_simple_environment_colors <- function(areas, classification_type) {
  color_mapping <- list()
  
  # Debug: Print what areas we're actually getting
  cat("Areas passed to color function:", paste(areas, collapse = ", "), "\n")
  
  # Check if this is a multi-line metric based on the areas - USING ACTUAL NAMES FROM YOUR DATA
  is_multi_line_biodiversity_species <- any(c("Marine Abundance", "Terrestrial Abundance", "Terrestrial Occupancy") %in% areas) ||
                                        any(grepl("Marine|Terrestrial", areas, ignore.case = TRUE))
  
  is_multi_line_freshwater <- any(grepl("Water|Physical|Access", areas, ignore.case = TRUE))
  
  is_multi_line_biodiversity_awareness <- any(c("Not aware", "Not engaged", "Some engagement", "High engagement") %in% areas)
  
  # Handle multi-line metrics with muted red-purple-blue spectrum
  if (is_multi_line_biodiversity_species) {
    # Map whatever the actual column names are to colors
    unique_areas <- unique(areas)
    if (length(unique_areas) >= 3) {
      color_mapping[[unique_areas[1]]] <- "#8E6A94"     # muted purple
      color_mapping[[unique_areas[2]]] <- "#A85A85"     # muted red-purple  
      color_mapping[[unique_areas[3]]] <- "#6B8CAE"     # muted blue
    }
  } else if (is_multi_line_freshwater) {
    # Map the actual fresh water column names to colors
    unique_areas <- unique(areas)
    colors_to_use <- c("#8E6A94", "#A85A85", "#6B8CAE", "#9B7B8A")
    for (i in seq_along(unique_areas)) {
      if (i <= length(colors_to_use)) {
        color_mapping[[unique_areas[i]]] <- colors_to_use[i]
      }
    }
  } else if (is_multi_line_biodiversity_awareness) {
    color_mapping[["Not aware"]] <- "#C85A5A"          # muted red
    color_mapping[["Not engaged"]] <- "#A85A85"        # muted red-purple
    color_mapping[["Some engagement"]] <- "#8E6A94"    # muted purple
    color_mapping[["High engagement"]] <- "#6B8CAE"    # muted blue
  } else {
    # Get base colors from config.R if available for regular metrics
    if (classification_type == "4-fold") {
        color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural areas" = "#F4E470", 
                        "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")

    } else if (classification_type == "2-fold") {
      colors <- get_classification_colors("2-fold")
      if (!is.null(colors)) {
        color_mapping[["Urban"]] <- colors[["Urban"]]
        color_mapping[["Rural"]] <- colors[["Rural"]]
      }
    }
  }
  
  # Always set Scotland to gray
  color_mapping[["Scotland"]] <- "#B2B2B2"
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  cat("Final color mapping:", paste(names(final_colors), "=", final_colors, collapse = ", "), "\n")
  return(final_colors[!sapply(final_colors, is.null)])
}
#ui
environment_dashboard_ui <- function(category_id) {
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
      .environment-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .environment-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .environment-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .environment-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .environment-metric-description {
        color: #6c757d !important;
        font-size: 0.95em !important;
        line-height: 1.4 !important;
        margin-left: 30px !important;
      }
      "))
    ),
    uiOutput('dev_banner'),
    
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
            h2("Environment and Climate Change", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("environment_classification_selector")
          ),
          
          # Top-center-right: Environment metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("environment_metric", "Environment Metric:", 
                        choices = c("Select a policy metric..." = "", names(environment_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (ONLY for woodlands now)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
                condition = "input.environment_metric == 'Forests and woodlands'", # ONLY for woodlands now
                selectInput("selected_woodlands_activity", "Sub-metric:", 
                            choices = names(woodlands_sub_metrics), 
                            selected = names(woodlands_sub_metrics)[1], 
                            width = "180px")
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
      condition = "input.environment_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore environment and climate change data across Scotland's regions.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Environment-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Percentage of household waste recycled
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Percentage of household waste recycled', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("recycle", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Percentage of household waste recycled"),
                      div(class = "environment-metric-description", 
                          "Tracks recycling rates as a percentage of total household waste across Scotland's regions. Shows progress in waste management and environmental sustainability practices.")
                    )
                  )
                ),
                
                # Renewable Electricity
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Renewable Electricity', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("wind", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Renewable Electricity"),
                      div(class = "environment-metric-description",
                          "Renewable electricity generation per household measured in MWh. Reflects Scotland's transition to clean energy and regional differences in renewable energy capacity.")
                    )
                  )
                ),
                
                # Clean Seas
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Clean Seas', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("water", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Clean Seas"),
                      div(class = "environment-metric-description",
                          "Percentage of biogeographic regions with acceptably low levels of marine contaminants. Monitors the health of Scotland's extensive marine ecosystems.")
                    )
                  )
                ),
                
                # Biodiversity: Scottish species
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Biodiversity: Scottish species', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("paw", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Biodiversity: Scottish species"),
                      div(class = "environment-metric-description",
                          "Combined index tracking abundance and occupancy of marine and terrestrial species across Scotland. Measures the health of Scotland's wildlife populations.")
                    )
                  )
                ),
                
                # Fresh water
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Fresh water', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("tint", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Fresh water"),
                      div(class = "environment-metric-description",
                          "Percentage of river and loch waterbodies achieving 'Good' or better status across multiple water quality metrics. Tracks the condition of Scotland's freshwater ecosystems.")
                    )
                  )
                ),
                
                # Restored peatland
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Restored peatland', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("seedling", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Restored peatland"),
                      div(class = "environment-metric-description",
                          "Cumulative area of peatland restored since 2012 measured in thousands of hectares. Reflects Scotland's commitment to peatland restoration for carbon storage and biodiversity.")
                    )
                  )
                ),
                
                # Biodiversity awareness
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Biodiversity awareness', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("eye", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Biodiversity awareness"),
                      div(class = "environment-metric-description",
                          "Public awareness, understanding and support for biodiversity conservation across different engagement levels. Measures community connection to environmental issues.")
                    )
                  )
                ),
                
                # Rare and threatened species
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Rare and threatened species', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("exclamation-triangle", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Rare and threatened species"),
                      div(class = "environment-metric-description",
                          "UK Red List Index tracking the conservation status of rare and threatened species. Lower values indicate greater threat to species survival.")
                    )
                  )
                ),
                
                # Forests and woodlands
                div(
                  class = "environment-metrics-card",
                  onclick = "Shiny.setInputValue('environment_metric_select', 'Forests and woodlands', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("tree", class = "environment-metric-icon"),
                    div(
                      div(class = "environment-metric-title", "Forests and woodlands"),
                      div(class = "environment-metric-description",
                          "Woodland area by forest type and ownership measured in thousands of hectares. Tracks Scotland's forest coverage across different woodland categories.")
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
      condition = "input.environment_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("environment_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("environment_data_summary"))
          )
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                        div(style = "margin-right: 80px; max-width: calc(100% - 100px);", uiOutput("environment_trend_title")),
                        div(style = "position: absolute; right: 20px; flex-shrink: 0;",
                            downloadButton("environment_trend_download", "Download", class = "excel-download-btn"
                            ))), 
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("environment_trend_chart") %>% withSpinner())
      ),
      
      # Comparison chart - ONLY for first two metrics (waste and renewable)
      conditionalPanel(
        condition = "input.environment_metric == 'Percentage of household waste recycled' || input.environment_metric == 'Renewable Electricity'",
        fluidRow(
          box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                          div(style = "margin-right: 80px; max-width: calc(100% - 100px);", uiOutput("environment_comparison_title")),
                          div(style = "position: absolute; right: 20px; flex-shrink: 0;",
                              downloadButton("environment_comparison_download", "Download", class = "excel-download-btn"
                              ))),
              status = "primary", solidHeader = TRUE, width = 12,
              div(style = "margin-bottom: 15px;", uiOutput("environment_year_selector")),
              plotlyOutput("environment_comparison_chart") %>% withSpinner())
        )
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                        div(style = "margin-right: 80px; max-width: calc(100% - 100px);", uiOutput("environment_table_title")),
                        div(style = "position: absolute; right: 20px; flex-shrink: 0;",
                            downloadButton("environment_table_download", "Download", class = "excel-download-btn"
                            ))),
            status = "info", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("environment_table_year_filter")),
                  column(6, uiOutput("environment_table_area_filter"))
                )),
            DT::dataTableOutput("environment_data_table") %>% withSpinner())
      )
    )
  )
}
# ===========================================
# ENVIRONMENT DASHBOARD - PART 3: SERVER FUNCTIONS
# ===========================================

environment_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  environment_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  onStop(function() {
    cat("Cleaning up environment_server\n")
    environment_values$processed_data <- NULL
    environment_values$data_status <- "Loading..."
    updateSelectInput(session, "environment_metric", selected = "")
    updateSelectInput(session, "environment_classification_type", selected = "")
    updateSelectInput(session, "selected_woodlands_activity", selected = names(woodlands_sub_metrics)[1])
  })
  # Handle metric selection from the metrics cards
  observeEvent(input$environment_metric_select, {
    updateSelectInput(session, "environment_metric", selected = input$environment_metric_select)
  })
  
  # Dynamic UI outputs
  output$environment_summary_title <- renderUI({
    req(input$environment_metric)
    
    # For multi-line metrics, set a default classification since dropdown is hidden
    classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      "multi-line"
    } else {
      req(input$environment_classification_type)
      input$environment_classification_type
    }
    
    display_name <- get_environment_metric_display_name(input$environment_metric)
    classification_text <- case_when(
      classification_type == "4-fold" ~ "(4-fold RESAS)",
      classification_type == "multi-line" ~ "",
      input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness") ~ "",
      TRUE ~ "(Scotland)"
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$environment_year_selector <- renderUI({
    req(environment_values$processed_data)
    if (nrow(environment_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(environment_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("environment_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  # FIXED CLASSIFICATION SELECTOR
  output$environment_classification_selector <- renderUI({
    req(input$environment_metric)
    
    # For multi-line metrics, don't show the classification selector since it's redundant
    if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      return(NULL)
    }
    
    available_classifications <- environment_metrics[[input$environment_metric]]$classifications
    choices <- list()
    
    # Special labeling matching culture module structure
    if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
    if("Scotland" %in% available_classifications) choices[["Scotland"]] <- "Scotland"
    
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("environment_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles
  output$environment_trend_title <- renderUI({
    req(input$environment_metric)
    display_name <- get_environment_metric_display_name(input$environment_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$environment_comparison_title <- renderUI({
    req(input$environment_selected_year, input$environment_metric)
    display_name <- get_environment_metric_display_name(input$environment_metric)
    paste0("Single Year Comparison (", input$environment_selected_year, ") for ", display_name)
  })
  
  output$environment_table_title <- renderUI({
    req(input$environment_metric)
    display_name <- get_environment_metric_display_name(input$environment_metric)
    paste("Data Table for", display_name)
  })
  
  output$environment_key_insights_title <- renderUI({
    req(input$environment_metric)
    display_name <- get_environment_metric_display_name(input$environment_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$environment_table_year_filter <- renderUI({
    req(environment_values$processed_data)
    if(nrow(environment_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(environment_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("environment_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$environment_table_area_filter <- renderUI({
    req(environment_values$processed_data)
    if(nrow(environment_values$processed_data) == 0) return(NULL)
    
    # For multi-line metrics, set default classification
    if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      classification_type <- "multi-line"
    } else {
      if (is.null(input$environment_classification_type)) {
        return()  # Wait for UI to initialize
      }
      classification_type <- input$environment_classification_type
    }

    
    agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("environment_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Process data reactively
  observe({
    if (is.null(input$environment_metric) || input$environment_metric == "") {
      environment_values$processed_data <- data.frame()
      environment_values$data_status <- "Please select a metric"
      return()
    }
    
    # For multi-line metrics, set default classification
    classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      "multi-line"
    } else {
      req(input$environment_classification_type)
      input$environment_classification_type
    }
    
    cat("=== REACTIVE DATA LOADING ===\n")
    cat("Metric:", input$environment_metric, "\n")
    cat("Classification:", classification_type, "\n")
    
    # Only Forests and woodlands needs sub-metric filtering now
    if (input$environment_metric == "Forests and woodlands") {
      if (is.null(input$selected_woodlands_activity)) {
        cat("Woodlands sub-metric not selected yet, loading all data\n")
        environment_values$processed_data <- load_environment_data_simple(
          input$environment_metric, 
          classification_type, 
          NULL
        )
      } else {
        selected_activity <- woodlands_sub_metrics[input$selected_woodlands_activity]
        cat("Woodlands selected activity:", selected_activity, "\n")
        environment_values$processed_data <- load_environment_data_simple(
          input$environment_metric, 
          classification_type, 
          selected_activity
        )
      }
    } else {
      # All other metrics load without sub-metric filtering
      cat("Loading metric without sub-metrics\n")
      environment_values$processed_data <- load_environment_data_simple(
        input$environment_metric, 
        classification_type
      )
    }
    
    data_rows <- nrow(environment_values$processed_data)
    environment_values$data_status <- if(data_rows > 0) "Environment data loaded" else "Data available at: www.gov.scot/publications/rural-scotland-data-dashboard-2025-data-tables "
    
    cat("Final processed data rows:", data_rows, "\n")
    if (data_rows > 0) {
      cat("Sample of final data:\n")
      print(head(environment_values$processed_data, 3))
    }
    cat("=== END REACTIVE DATA LOADING ===\n")
  })
  
  # DATA SUMMARY
  output$environment_data_summary <- renderUI({
    req(environment_values$processed_data, input$environment_metric)
    
    if (is.null(environment_values$processed_data) ||
        nrow(environment_values$processed_data) == 0) {
      return(
        div(
          class = "no-data-message",
          h4(
            "Data available at: ",
            tags$a(
              "Rural Scotland Data Dashboard data tables",
              href = "https://www.gov.scot/publications/rural-scotland-data-dashboard-2025-data-tables",
              target = "_blank",
              rel = "noopener noreferrer"
            )
          )
        )
      )
    }
    
    
    display_name <- get_environment_metric_display_name(input$environment_metric)
    custom_insight <- environment_key_insights[[input$environment_metric]]
    custom_notes <- environment_notes[[input$environment_metric]]
    
    # Define source information
    source_info <- switch(input$environment_metric,
                          "Percentage of household waste recycled" = list(
                            text = "Household waste data",
                            url = "https://www.sepa.org.uk/environment/waste/waste-data/waste-data-reporting/household-waste-data/"
                          ),
                          "Renewable Electricity" = list(
                            text = "Regional Renewable Statistics n", 
                            url = "https://www.gov.uk/government/statistics/regional-renewable-statistics/"
                          ),
                          "Clean Seas" = list(
                            text = "National Performance Framewor: Marine Environment Monitoring and Assessment National database (MERMAN) data",
                            url = "https://www.bodc.ac.uk/resources/portals_and_links/merman/assessments_and_data_access/"
                          ),
                          "Biodiversity: Scottish species" = list(
                            text = " Marine and Terrestrial Species Indicators 2022: Nature Scot",
                            url = "https://www.nature.scot/doc/official-statistics-marine-and-terrestrial-species-indicators-experimental-statistic"
                          ),
                          "Fresh water" = list(
                            text = "Environmental Strategy Monitoring Framework",
                            url = "https://data.gov.scot/environment/Outcome1.html#freshwater_condition/"
                          ),
                          "Restored peatland" = list(
                            text = "Nature Scot",
                            url = "https://www.nature.scot/climate-change/nature-based-solutions/nature-based-solutions-practice/peatland-action/peatland-action-what-we-have-achieved"
                          ),
                          "Biodiversity awareness" = list(
                            text = "JNCC UK Biodiversity Indicators",
                            url = "https://jncc.gov.uk/resources/111f1e51-711a-46c3-a64a-3bd1de2d4c4d"
                          ),
                          "Rare and threatened species" = list(
                            text = "IUCN, United Nations",
                            url = "https://sdgdata.gov.uk/15-5-1/"
                          ),
                          "Forests and woodlands" = list(
                            text = "Forest Research",
                            url = "https://www.forestresearch.gov.uk/tools-and-resources/statistics/time-series//"
                          ),
                          list(text = "Data Source", url = "#")
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
    
    return(div(h4("Environment Data Loaded"), p(paste("Showing data for:", display_name))))
  })
  

  
# UPDATED TREND CHART WITH RIGHT-SIDE LEGEND
output$environment_trend_chart <- renderPlotly({
  req(environment_values$processed_data)
  
  # For multi-line metrics, set default classification
  classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
    "multi-line"
  } else {
    req(input$environment_classification_type)
    input$environment_classification_type
  }
  
  tryCatch({
    agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
    
    if (nrow(agg_data) == 0) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text =  
      , textfont = list(size = 16)))
    }
    
    cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
    cat("Trend chart - unique areas:", paste(unique(agg_data$Area), collapse = ", "), "\n")
    
    # Check if this is a multi-line metric (Fresh water, Biodiversity species, Biodiversity awareness)
    is_multi_line <- input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")
    
    # Format data for display
    is_renewable_data <- input$environment_metric == "Renewable Electricity"
    is_peatland_data <- input$environment_metric == "Restored peatland"
    is_woodlands_data <- input$environment_metric == "Forests and woodlands"
    is_rare_species_data <- input$environment_metric == "Rare and threatened species"
    
    if (is_renewable_data) {
      agg_data$Value_Display <- paste0(round(agg_data$Value, 2), " MWh")
      y_label <- "MWh per Household"
      tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Generation: ", agg_data$Value_Display)
    } else if (is_peatland_data || is_woodlands_data) {
      agg_data$Value_Display <- paste0(round(agg_data$Value, 1), "k ha")
      y_label <- "Thousands Hectares"
      tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Area: ", agg_data$Value_Display)
    } else if (is_rare_species_data) {
      agg_data$Value_Rounded <- round(agg_data$Value, 4)
      y_label <- "Red List Index"
      tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Index: ", agg_data$Value_Rounded)
    } else {
      agg_data$Value_Rounded <- round(agg_data$Value, 1)
      y_label <- "Percentage (%)"
      tooltip_text <- paste0("Year: ", agg_data$Year, "<br>", 
                            if(is_multi_line) "Metric" else "Area", ": ", agg_data$Area, 
                            "<br>Rate: ", agg_data$Value_Rounded, "%")
    }
    
    agg_data$tooltip <- tooltip_text
    
    # Create plot
    p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 3) +
      theme_minimal() +
      theme(
        legend.position = "right",  # Changed from "bottom" to "right"
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(x = "Year", y = y_label, color = if(is_multi_line) "Metric" else "Area Type") +
      scale_x_continuous(breaks = seq(min(agg_data$Year, na.rm = TRUE), max(agg_data$Year, na.rm = TRUE), by = 1)) +
      scale_color_manual(values = get_simple_environment_colors(unique(agg_data$Area), classification_type)) +
      scale_y_continuous(labels = function(x) {
        if (is_renewable_data) {
          paste0(round(x, 2), " MWh")
        } else if (is_peatland_data || is_woodlands_data) {
          paste0(round(x, 1), "k ha")
        } else if (is_rare_species_data) {
          round(x, 4)
        } else {
          paste0(round(x, 1), "%")
        }
      })
    
    # Convert to plotly for interactivity
    p <- ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 80, t = 50, r = 150),  # Increased right margin for legend
        hovermode = "x unified",
        xaxis = list(
          tickangle = 45,
          title = list(text = "Year", standoff = 20),
          tickvals = seq(min(agg_data$Year, na.rm = TRUE), max(agg_data$Year, na.rm = TRUE), by = 1)
        ),
        yaxis = list(title = list(text = y_label, standoff = 20)),
        legend = list(
          orientation = "v",      # Vertical orientation
          x = 1.02,              # Position to the right of the plot
          xanchor = "left",      # Anchor to left of legend box
          y = 0.5,               # Center vertically
          yanchor = "middle"     # Anchor to middle of legend box
        )
      )
    
    # Apply accentuation for rare species
    if (is_rare_species_data && !is.null(environment_metrics[[input$environment_metric]]$accentuate_difference)) {
      p <- p %>% layout(
        annotations = list(
          list(
            x = max(agg_data$Year, na.rm = TRUE),
            y = max(agg_data$Value, na.rm = TRUE),
            text = "Higher values = better species status",
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            font = list(size = 12, color = "#666")
          )
        )
      )
    }
    
    return(p)
  }, error = function(e) {
    cat("ERROR in environment_trend_chart:", e$message, "\n")
    return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "Error generating chart", textfont = list(size = 16)))
  })
})

# Comparison chart - ONLY for waste and renewable metrics
output$environment_comparison_chart <- renderPlotly({
  req(environment_values$processed_data, input$environment_selected_year)
  
  # For multi-line metrics, set default classification
  classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
    "multi-line"
  } else {
    req(input$environment_classification_type)
    input$environment_classification_type
  }
  
  tryCatch({
    agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
    
    if (nrow(agg_data) == 0) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "", textfont = list(size = 16)))
    }
    
    # Filter for selected year
    year_data <- agg_data %>% filter(Year == as.numeric(input$environment_selected_year))
    
    if (nrow(year_data) == 0) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data for", input$environment_selected_year), textfont = list(size = 16)))
    }
    
    cat("Comparison chart - year_data dimensions:", nrow(year_data), "x", ncol(year_data), "\n")
    
    # Format data for display
    is_renewable_data <- input$environment_metric == "Renewable Electricity"
    is_peatland_data <- input$environment_metric == "Restored peatland"
    is_woodlands_data <- input$environment_metric == "Forests and woodlands"
    is_rare_species_data <- input$environment_metric == "Rare and threatened species"
    
    if (is_renewable_data) {
      year_data$Value_Display <- paste0(round(year_data$Value, 2), " MWh")
      y_label <- "MWh per Household"
      tooltip_text <- paste0("Area: ", year_data$Area, "<br>Generation: ", year_data$Value_Display)
    } else if (is_peatland_data || is_woodlands_data) {
      year_data$Value_Display <- paste0(round(year_data$Value, 1), "k ha")
      y_label <- "Thousands Hectares"
      tooltip_text <- paste0("Area: ", year_data$Area, "<br>Area: ", year_data$Value_Display)
    } else if (is_rare_species_data) {
      year_data$Value_Rounded <- round(year_data$Value, 4)
      y_label <- "Red List Index"
      tooltip_text <- paste0("Area: ", year_data$Area, "<br>Index: ", year_data$Value_Rounded)
    } else {
      year_data$Value_Rounded <- round(year_data$Value, 1)
      y_label <- "Percentage (%)"
      tooltip_text <- paste0("Area: ", year_data$Area, "<br>Rate: ", year_data$Value_Rounded, "%")
    }
    
    year_data$tooltip <- tooltip_text
    
    # Create bar plot
    p <- ggplot(year_data, aes(x = Area, y = Value, fill = Area, text = tooltip)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      labs(x = "Area Type", y = y_label) +
      scale_fill_manual(values = get_simple_environment_colors(unique(year_data$Area), classification_type)) +
      scale_y_continuous(labels = function(x) {
        if (is_renewable_data) {
          paste0(round(x, 2), " MWh")
        } else if (is_peatland_data || is_woodlands_data) {
          paste0(round(x, 1), "k ha")
        } else if (is_rare_species_data) {
          round(x, 4)
        } else {
          paste0(round(x, 1), "%")
        }
      })
    
    p <- ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 100),
        hovermode = "x unified",
        xaxis = list(title = list(text = "Area Type", standoff = 20)),
        yaxis = list(title = list(text = y_label, standoff = 20))
      )
    
    return(p)
  }, error = function(e) {
    cat("ERROR in environment_comparison_chart:", e$message, "\n")
    return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "Error generating chart", textfont = list(size = 16)))
  })
})

# Data table
output$environment_data_table <- DT::renderDataTable({
  req(environment_values$processed_data)
  
  # For multi-line metrics, set default classification
  classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
    "multi-line"
  } else {
    req(input$environment_classification_type)
    input$environment_classification_type
  }
  
  agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
  
  if (nrow(agg_data) == 0) {
    return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't')))
  }
  
  # Apply filters
  filtered_data <- agg_data
  if (!is.null(input$environment_table_year_filter) && input$environment_table_year_filter != "all") {
    filtered_data <- filtered_data %>% filter(Year == as.numeric(input$environment_table_year_filter))
  }
  if (!is.null(input$environment_table_area_filter) && input$environment_table_area_filter != "all") {
    filtered_data <- filtered_data %>% filter(Area == input$environment_table_area_filter)
  }
  
  # Format values
  is_renewable_data <- input$environment_metric == "Renewable Electricity"
  is_peatland_data <- input$environment_metric == "Restored peatland"
  is_woodlands_data <- input$environment_metric == "Forests and woodlands"
  is_rare_species_data <- input$environment_metric == "Rare and threatened species"
  
  filtered_data$Value_Display <- if (is_renewable_data) {
    paste0(round(filtered_data$Value, 2), " MWh")
  } else if (is_peatland_data || is_woodlands_data) {
    paste0(round(filtered_data$Value, 1), "k ha")
  } else if (is_rare_species_data) {
    sprintf("%.4f", filtered_data$Value)
  } else {
    paste0(round(filtered_data$Value, 1), "%")
  }
  
  # Select columns and rename
  table_data <- filtered_data %>%
    select(Year, Area, Value_Display, Data_Source) %>%
    rename("Value" = Value_Display, "Source" = Data_Source)
  
  DT::datatable(
    table_data,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      ),
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    extensions = 'Buttons',
    rownames = FALSE
  )
})

# Download handlers
output$environment_trend_download <- downloadHandler(
  filename = function() {
    paste0(input$environment_metric, "_trend_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # For multi-line metrics, set default classification
    classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      "multi-line"
    } else {
      req(input$environment_classification_type)
      input$environment_classification_type
    }
    agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
    write.csv(agg_data, file, row.names = FALSE)
  }
)

output$environment_comparison_download <- downloadHandler(
  filename = function() {
    paste0(input$environment_metric, "_comparison_", input$environment_selected_year, "_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # For multi-line metrics, set default classification
    classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      "multi-line"
    } else {
      req(input$environment_classification_type)
      input$environment_classification_type
    }
    agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
    year_data <- agg_data %>% filter(Year == as.numeric(input$environment_selected_year))
    write.csv(year_data, file, row.names = FALSE)
  }
)

output$environment_table_download <- downloadHandler(
  filename = function() {
    paste0(input$environment_metric, "_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # For multi-line metrics, set default classification
    classification_type <- if (input$environment_metric %in% c("Fresh water", "Biodiversity: Scottish species", "Biodiversity awareness")) {
      "multi-line"
    } else {
      req(input$environment_classification_type)
      input$environment_classification_type
    }
    agg_data <- simple_aggregate_environment_data(environment_values$processed_data, classification_type)
    filtered_data <- agg_data
    if (!is.null(input$environment_table_year_filter) && input$environment_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$environment_table_year_filter))
    }
    if (!is.null(input$environment_table_area_filter) && input$environment_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$environment_table_area_filter)
    }
    write.csv(filtered_data, file, row.names = FALSE)
  }
)
# Back button text
output$back_button_text <- renderText({
  "Back to Categories"
})

# Handle back button
observeEvent(input$back_to_categories, {
  values$current_category <- NULL
  updateSelectInput(session, "environment_metric", selected = "")
  updateSelectInput(session, "environment_classification_type", selected = "")
})

}
