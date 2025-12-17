<<<<<<< HEAD

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Housing metrics configuration 
housing_metrics <- list(
  "Median Property Price" = list(
    file_8fold = "housing/median_property_price.csv",
    classifications = c("8-fold"),
    full_name = "Median Property Price by 8-fold Urban Rural Classification"
  ),
  "Second Homes" = list(
    file_6fold = "housing/homes_second.xlsx",
    classifications = c("6-fold"),
    full_name = "Proportion of all dwellings that are second homes"
  ),
  "Vacant Homes" = list(
    file_6fold = "housing/homes_vacant.xlsx",
    classifications = c("6-fold"), 
    full_name = "Proportion of all dwellings that are vacant homes"
  ),
  "New Build Completions" = list(
    file_4fold = "housing/new_completions.csv",
    classifications = c("4-fold"),
    full_name = "New Build Completions (Total Number)",
    is_count_data = TRUE
  ),
  "AHSP Completions" = list(
    file_4fold = "housing/AHSP_completions.csv",
    classifications = c("4-fold"),
    full_name = "AHSP Completions (Total Number)",
    is_count_data = TRUE,
    bar_chart_only = TRUE
  ),
  "Housing Conditions" = list(
    file_2fold = "housing/housing_conditions.csv",
    classifications = c("2-fold"),
    full_name = "Dwellings with urgent disrepair to critical elements"
  ),
  "EPC rating" = list(
    file_2fold = "housing/epc.csv",
    classifications = c("2-fold"),
    full_name = "Proportion of households rated EPC C or above"
  )
)
housing_key_insights <- list(
  "Median Property Price" = "Median property price is the highest in accessible rural areas (£270,000) and lowest in remote small towns (£128,500). ",
  "Second Homes" = "The proportion of all dwellings that are second homes is the highest in remote rural areas (6.1%) whereas in other areas it is at or below 2% with the Scotland average being 0.8%.",
  "Vacant Homes" = "In 2024 3.3% of all dwellings in Scotland were vacant homes, 6.3% of all dwellings in remote rural areas and 4.8% in remote small towns were vacant homes. The lowest proportion of vacant homes was seen in accessible small towns (2.8%).",
  "New Build Completions" =  "The number of new build completions dropped from 2023 to 2024 in all geographic areas except Larger Cities. Urban with substantial rural areas saw 7,672 new build completions in 2024, which is more than larger cities (6,610). There were 235 new build completions in islands and remote rural areas in 2024.",
  "AHSP Completions" =  "There was a total of 10,466 AHSP completions across Scotland in 2023. the highest number of AHSP completions was 3,915 in urban with substantial rural areas and the lowest was 256 in islands and remote rural areas.",
  "Housing Conditions" = "19% of dwellings in rural areas are in need of urgent repair to critical elements compared with 16% in urban areas in 2023.",
  "EPC rating" = "60% of households in urban areas were rated EPC C or above in 2023 compared with 36% in rural areas. Scotland average is 56%."
)

# Static Notes for each health metric
housing_notes <- list(
  "Median Property Price" = "",
  "Second Homes" = "The 6-fold urban-rural classification is produced by aggregating 2011 datazones. Data for 2024 has also been published using 2022 datazones (source linked below) but has not been included here as it is not directly comparable to 2011 datazones. Back series of this metric using 2022 datazones will be available in future updates.",
  "Vacant Homes" = "The 6-fold urban-rural classification is produced by aggregating 2011 datazones. Data for 2024 has also been published using 2022 datazones (source linked below) but has not been included here as it is not directly comparable to 2011 datazones. Back series of this metric using 2022 datazones will be available in future updates.",
  "New Build Completions" = "",
  "AHSP Completions" = "",
  "Housing Conditions" = "1. The time period covered by these statistics means that time series results was affected by the coronavirus (COVID-19) pandemic. Specifically there is no 2020 data and 2021 data is not comparable to other waves of the survey. 2.  Urgent disrepair to critical elements has been calculated for the first time in 2019 and back updated for 2018 to allow a comparison. 3. The 2020 urban rural classification is used for 2022 and the 2013/14 urban rural classification (2011 data zone edition) is used for reporting 2018 and 2019 data.",
  "EPC rating" = "1. The Standard Assessment Procedure (SAP) for EPC ratings is periodically reviewed and updated. These different methodologies mean that only data from years with the same SAP metholdology can be compared (2007-2012 and 2014-2017 and 2018-2022). 2. The time period covered by these statistics means that time series results was affected by the coronavirus (COVID-19) pandemic. Specifically, there is no 2020 data and 2021 data is not comparable to other waves of the survey. 3. The most relevant available version of SG urban rural classification for each year is used."
)

# RESAS 4-fold classification mapping
resas_council_mapping <- list(
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

# Helper functions for number formatting 
format_housing_number <- function(number, metric_name) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  
  if (metric_name %in% c("New Build Completions", "AHSP Completions")) {
    return(scales::comma(round(number, 0)))
  } else if (metric_name == "Median Property Price") {
    return(paste0("£", scales::comma(round(number, 0))))
  } else {
    return(paste0(round(number, 1), "%"))
  }
}

# Function to get display name for metrics
get_housing_metric_display_name <- function(metric_name) {
  metric_info <- housing_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# ===========================================
# DATA LOADING FUNCTIONS
# ===========================================

# Load median property price data (8-fold only)
load_median_property_price_data <- function() {
  filepath <- "housing/median_property_price.csv"
  cat("Loading median property price data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read raw CSV and find header
    raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE)
    header_row <- 6  # Known position
    data_rows <- raw_data[7:9, ]  # Known data rows
    headers <- as.character(raw_data[header_row, ])
    colnames(data_rows) <- headers
    
    processed_data <- data_rows %>%
      filter(!is.na(`Financial year`)) %>%
      gather(key = "Area_Type", value = "Value", -`Financial year`) %>%
      mutate(
        Year = case_when(
          grepl("2022-23", `Financial year`) ~ 2022,
          grepl("2023-24", `Financial year`) ~ 2023, 
          grepl("2024-25", `Financial year`) ~ 2024,
          TRUE ~ NA_real_
        ),
        Value = as.numeric(gsub("[^0-9.]", "", Value)),
        # Map to 8-fold areas
        Area = case_when(
          Area_Type == "Large Urban Areas" ~ "Large Urban Areas",
          Area_Type == "Other Urban Areas" ~ "Other Urban Areas",
          Area_Type == "Accessible Small Towns" ~ "Accessible Small Towns", 
          Area_Type == "Remote Small Towns" ~ "Remote Small Towns",
          Area_Type == "Very Remote Small Towns" ~ "Very Remote Small Towns",
          Area_Type == "Accessible Rural Areas" ~ "Accessible Rural",
          Area_Type == "Remote Rural Areas" ~ "Remote Rural",
          Area_Type == "Very Remote Rural Areas" ~ "Very Remote Rural",
          TRUE ~ Area_Type
        ),
        Data_Source = "Registers of Scotland - Property Prices"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source, `Financial year`)
    
    cat(paste("Loaded", nrow(processed_data), "median property price records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading median property price data:", e$message, "\n")
    return(data.frame())
  })
}

load_second_homes_data <- function() {
  filepath <- "housing/homes_second.xlsx"
  cat("Loading second homes data from:", filepath, "\n")
  
  tryCatch({
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file, using second row as header
    raw_data <- read_excel(filepath, col_names = TRUE, skip = 1)
    cat("Raw data columns:", paste(colnames(raw_data), collapse = ", "), "\n")
    
    # Identify the column that likely represents 'Region'
    possible_region_cols <- c("Region", "region", "Area", "area", "Geography", "geography")
    region_col <- colnames(raw_data)[colnames(raw_data) %in% possible_region_cols | grepl("region|area|geography", colnames(raw_data), ignore.case = TRUE)]
    
    if (length(region_col) == 0) {
      cat("No 'Region' column found in data. Available columns:", paste(colnames(raw_data), collapse = ", "), "\n")
      return(data.frame())
    }
    
    region_col <- region_col[1]
    cat("Using region column:", region_col, "\n")
    
    # Rename the region column to 'Region' for consistency
    colnames(raw_data)[colnames(raw_data) == region_col] <- "Region"
    
    # Process data
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100,  # Convert to percentage
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          Region == "Scotland (All)" ~ "Scotland",
          TRUE ~ trimws(Region)
        ),
        Data_Source = "Scottish Government - Second Homes (% of all dwellings)"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "second homes records\n"))
    return(processed_data)
  }, error = function(e) {
    cat("Error loading second homes data:", e$message, "\n")
    return(data.frame())
  })
}

load_vacant_homes_data <- function() {
  filepath <- "housing/homes_vacant.xlsx"
  cat("Loading vacant homes data from:", filepath, "\n")
  
  tryCatch({
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file, using second row as header
    raw_data <- read_excel(filepath, col_names = TRUE, skip = 1)
    cat("Raw data columns:", paste(colnames(raw_data), collapse = ", "), "\n")
    
    # Identify the column that likely represents 'Region'
    possible_region_cols <- c("Region", "region", "Area", "area", "Geography", "geography")
    region_col <- colnames(raw_data)[colnames(raw_data) %in% possible_region_cols | grepl("region|area|geography", colnames(raw_data), ignore.case = TRUE)]
    
    if (length(region_col) == 0) {
      cat("No 'Region' column found in data. Available columns:", paste(colnames(raw_data), collapse = ", "), "\n")
      return(data.frame())
    }
    
    region_col <- region_col[1]
    cat("Using region column:", region_col, "\n")
    
    # Rename the region column to 'Region' for consistency
    colnames(raw_data)[colnames(raw_data) == region_col] <- "Region"
    
    # Process data
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100,  # Convert to percentage
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          Region == "Scotland (All)" ~ "Scotland",
          TRUE ~ trimws(Region)
        ),
        Data_Source = "Scottish Government - Vacant Homes (% of all dwellings)"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "vacant homes records\n"))
    return(processed_data)
  }, error = function(e) {
    cat("Error loading vacant homes data:", e$message, "\n")
    return(data.frame())
  })
}

# Load new build completions data (4-fold RESAS)
load_new_build_completions_data <- function() {
  filepath <- "housing/new_completions.csv"
  cat("Loading new build completions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
   
    raw_data <- read.csv(filepath, header = T, stringsAsFactors = FALSE)
    
    
    possible_region_cols <- c("Region", "region", "Area", "area", "Geography", "geography")
    region_col <- colnames(raw_data)[colnames(raw_data) %in% possible_region_cols | grepl("region|area|geography", colnames(raw_data), ignore.case = TRUE)]
    
    if (length(region_col) == 0) {
      cat("No 'Region' column found in data. Available columns:", paste(colnames(raw_data), collapse = ", "), "\n")
      return(data.frame())
    }
    
    region_col <- region_col[1]
    cat("Using region column:", region_col, "\n")
    
    # Rename the region column to 'Region' for consistency
    colnames(raw_data)[colnames(raw_data) == region_col] <- "Region"
    
 
    # Process data for years 2018-2024
    year_cols <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
    
    # RENAME YEAR COLS - GET RID OF x
    colnames(raw_data) <- gsub("X", "", colnames(raw_data))
    # Process data
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub('[^0-9]', '', Value)),
        Area = Region,
        Data_Source = "Scottish Government - New Build Completions",
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "new build completions records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading new build completions data:", e$message, "\n")
    return(data.frame())
  })
}

# Load AHSP completions data (4-fold RESAS) 
load_ahsp_completions_data <- function() {
  filepath <- "housing/AHSP_completions.csv"
  cat("Loading AHSP completions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read raw text and parse manually for AHSP data
    raw_text <- readLines(filepath)
    cat("AHSP raw text lines:", length(raw_text), "\n")
    
    # The data starts at line 2
    data_start_line <- 2
    
    if(data_start_line > length(raw_text)) {
      cat("Data start line exceeds file length\n")
      return(data.frame())
    }
    
    data_lines <- raw_text[data_start_line:length(raw_text)]
    data_lines <- data_lines[data_lines != "" & !grepl("^,*$", data_lines)]
    
    # Parse each data line manually
    councils_data <- data.frame()
    for(i in seq_along(data_lines)) {
      line <- data_lines[i]
      if(grepl("Scotland|Total|^,", line) || line == "") next
      
      parts <- strsplit(line, ",")[[1]]
      
      if(length(parts) >= 16) {
        council_name <- trimws(parts[1])
        totals_value <- trimws(parts[16])
        
        totals_clean <- as.numeric(gsub('[^0-9]', '', totals_value))
        
        if(!is.na(totals_clean) && council_name != "" && council_name != "Local Authority Area") {
          councils_data <- rbind(councils_data, data.frame(
            Council = council_name,
            Totals = totals_clean,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
      processed_data <- councils_data %>%
        mutate(
          Year = "2022-2023",
          Value = Totals,
          Area = Council,
          `Financial year` = Year,
          Data_Source = "Scottish Government - AHSP Completions"
        ) |> 
      select(Year, Area, Value, Data_Source)
    
  
    
    cat(paste("Loaded", nrow(processed_data), "AHSP completions records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading AHSP completions data:", e$message, "\n")
    return(data.frame())
  })
}

# Load housing conditions data (2-fold)
load_housing_conditions_data <- function() {
  filepath <- "housing/housing_conditions.csv"
  cat("Loading housing conditions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read housing conditions CSV
    raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # Look for urgent disrepair rows
    urban_row <- NULL
    rural_row <- NULL
    
    for(i in 1:nrow(raw_data)) {
      row_data <- raw_data[i, ]
      if(length(row_data) >= 3 && !is.na(row_data[1]) && !is.na(row_data[3])) {
        first_col <- as.character(row_data[1])
        third_col <- as.character(row_data[3])
        
        if(grepl("Urgent disrepair to one or more critical elements", first_col, ignore.case = TRUE)) {
          if(grepl("Urban", third_col, ignore.case = TRUE) && !grepl("Rural", third_col, ignore.case = TRUE)) {
            urban_row <- row_data
          } else if(grepl("Rural", third_col, ignore.case = TRUE)) {
            rural_row <- row_data
          }
        }
      }
    }
    
    if(is.null(urban_row) || is.null(rural_row)) {
      cat("Could not find both Urban and Rural rows for urgent disrepair\n")
      return(data.frame())
    }
    
    # Extract data from years 2018-2023
    years <- c(2018, 2019, 2020, 2021, 2022, 2023)
    year_columns <- 4:9
    
    clean_percentage <- function(x) {
      if(is.na(x) || x == "" || x == "[x]" || x == "[c]" || x == "[w]") return(NA)
      x <- gsub("%", "", as.character(x))
      x <- gsub("\\s+", "", x)
      x <- gsub('"', "", x)
      return(as.numeric(x))
    }
    
    processed_data <- data.frame()
    
    for(i in 1:length(years)) {
      year <- years[i]
      col_idx <- year_columns[i]
      
      if(col_idx <= length(urban_row) && col_idx <= length(rural_row)) {
        urban_value <- clean_percentage(urban_row[col_idx])
        rural_value <- clean_percentage(rural_row[col_idx])
        
        if(!is.na(urban_value) && !is.na(rural_value)) {
          processed_data <- rbind(processed_data, data.frame(
            Year = c(year, year),
            Area = c("Urban", "Rural"),
            Value = c(urban_value, rural_value),
            Data_Source = "Scottish Government - Housing Conditions",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat(paste("Loaded", nrow(processed_data), "housing conditions records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading housing conditions data:", e$message, "\n")
    return(data.frame())
  })
}

# Load EPC rating data (2-fold) 
load_epc_rating_data <- function() {
  filepath <- "housing/epc.csv"
  cat("Loading EPC rating data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read with explicit parameters to handle mixed types
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, 
                         check.names = FALSE, na.strings = c("", "NA", "#N/A", "N/A"))
    
    cat("Read", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    # Process EPC data
    epc_data <- raw_data %>%
      filter(!is.na(Region), Region != "SAP Methodology") %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        # Handle both percentage strings and numbers
        Value = case_when(
          is.na(Value) ~ NA_real_,
          Value %in% c("#N/A", "N/A", "") ~ NA_real_,
          TRUE ~ as.numeric(gsub("%", "", as.character(Value)))
        )
      ) %>%
      filter(!is.na(Value), Year != 2020) %>%
      mutate(
        Area = case_when(
          grepl("Urban.*EPC", Region, ignore.case = TRUE) ~ "Urban",
          grepl("Rural.*EPC", Region, ignore.case = TRUE) ~ "Rural",
          grepl("All.*EPC", Region, ignore.case = TRUE) ~ "All"
        ),
        Data_Source = "Scottish Government - EPC Rating (C or above)"
      ) %>%
      filter(!is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(epc_data), "EPC rating records\n"))
    return(epc_data)
    
  }, error = function(e) {
    cat("Error loading EPC data:", e$message, "\n")
    return(data.frame())
  })
}
# Processing Functions and Aggregation

load_housing_data_simple <- function(metric_name, classification_type) {
  metric_info <- housing_metrics[[metric_name]]
  if (is.null(metric_info)) return(data.frame())
  
  if (metric_name == "Second Homes" && classification_type == "6-fold") {
    return(load_second_homes_data())
  } else if (metric_name == "Vacant Homes" && classification_type == "6-fold") {
    return(load_vacant_homes_data())
  } else if (metric_name == "Median Property Price" && classification_type == "8-fold") {
    return(load_median_property_price_data())
  } else if (metric_name == "New Build Completions") {
    completions_4fold <- load_new_build_completions_data()
    if (classification_type == "4-fold") return(completions_4fold)
    if (classification_type == "2-fold") return(create_completions_2fold(completions_4fold))
  } else if (metric_name == "AHSP Completions") {
    ahsp_4fold <- load_ahsp_completions_data()
    if (classification_type == "4-fold") return(ahsp_4fold)
    if (classification_type == "2-fold") return(create_completions_2fold(ahsp_4fold))
  } else if (metric_name == "Housing Conditions" && classification_type == "2-fold") {
    return(load_housing_conditions_data())
  } else if (metric_name == "EPC rating" && classification_type == "2-fold") {
    return(load_epc_rating_data())
  }
  return(data.frame())
}

# Create 2-fold aggregation for homes data (second/vacant)
create_homes_2fold <- function(homes_6fold_data) {
  cat("Creating homes 2-fold aggregation\n")
  
  if(nrow(homes_6fold_data) == 0) {
    return(data.frame())
  }
  
  # Urban = average of 4 urban areas
  urban_data <- homes_6fold_data %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Rural = average of 2 rural areas
  rural_data <- homes_6fold_data %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Rural")
  
  # Scotland = average of all areas
  scotland_data <- homes_6fold_data %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Scotland")
  
  return(bind_rows(urban_data, rural_data, scotland_data))
}

# Create 3-fold aggregation for homes data (second/vacant)
create_homes_3fold <- function(homes_6fold_data) {
  cat("Creating homes 3-fold aggregation\n")
  
  if(nrow(homes_6fold_data) == 0) {
    return(data.frame())
  }
  
  # Urban = average of 4 urban areas
  urban_data <- homes_6fold_data %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Keep Rural areas separate (Accessible Rural and Remote Rural)
  rural_data <- homes_6fold_data %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural"))
  
  # Scotland = average of all areas
  scotland_data <- homes_6fold_data %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Scotland")
  
  return(bind_rows(urban_data, rural_data, scotland_data))
}
# 
# # # Create 2-fold aggregation for completions data (new build/AHSP) - SUMS not averages
# # create_completions_2fold <- function(completions_4fold_data) {
# #   cat("Creating completions 2-fold aggregation (using sums)\n")
# #   
# #   if(nrow(completions_4fold_data) == 0) {
# #     return(data.frame())
# #   }
# #   
#   # Urban = sum of Urban with Substantial Rural + Larger Cities
#   urban_data <- completions_4fold_data %>%
#     filter(Area %in% c("Urban with Substantial Rural", "Larger Cities")) %>%
#     group_by(Year, Data_Source) %>%
#     summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(Area = "Urban")
#   
#   # Rural = sum of Mainly Rural + Islands & Remote Rural
#   rural_data <- completions_4fold_data %>%
#     filter(Area %in% c("Mainly Rural", "Islands & Remote Rural")) %>%
#     group_by(Year, Data_Source) %>%
#     summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(Area = "Rural")
#   
#   # Scotland = sum of all areas
#   scotland_data <- completions_4fold_data %>%
#     group_by(Year, Data_Source) %>%
#     summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(Area = "Scotland")
#   
#   return(bind_rows(urban_data, rural_data, scotland_data))
# }

# Simple aggregate function 
simple_aggregate_housing_data <- function(processed_data, classification_type = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  # Data is already in the right format from loading functions
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}

# Get key insights function  
get_housing_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  cat("Processing key insights for:", metric_name, "\n")
  cat("Data dimensions:", nrow(processed_data), "rows\n")
  cat("Unique areas in data:", paste(unique(processed_data$Area), collapse = ", "), "\n")
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  # Initialize values
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  # Case 1: Already has Urban/Rural/Scotland (2-fold data)
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    cat("Data already in 2-fold format\n")
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
    
    # Case 2: Has 4-fold RESAS areas (completions data)
  } else if (any(c("Larger Cities", "Urban with Substantial Rural", "Mainly Rural", "Islands & Remote Rural") %in% unique_areas)) {
    cat("Data in 4-fold RESAS format, aggregating to 2-fold\n")
    
    # For count data (completions), use sums; for others, use means
    is_count_data <- metric_name %in% c("New Build Completions", "AHSP Completions")
    
    if (is_count_data) {
      # Urban = sum of Larger Cities + Urban with Substantial Rural
      urban_areas <- latest_data %>% 
        filter(Area %in% c("Larger Cities", "Urban with Substantial Rural"))
      if(nrow(urban_areas) > 0) {
        urban_val <- sum(urban_areas$Value, na.rm = TRUE)
      }
      
      # Rural = sum of Mainly Rural + Islands & Remote Rural
      rural_areas <- latest_data %>% 
        filter(Area %in% c("Mainly Rural", "Islands & Remote Rural"))
      if(nrow(rural_areas) > 0) {
        rural_val <- sum(rural_areas$Value, na.rm = TRUE)
      }
      
      # Scotland = sum of all areas
      scotland_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(scotland_areas) > 0) {
        scotland_val <- sum(scotland_areas$Value, na.rm = TRUE)
      }
    } else {
      # For non-count data, use averages
      urban_areas <- latest_data %>% 
        filter(Area %in% c("Larger Cities", "Urban with Substantial Rural"))
      if(nrow(urban_areas) > 0) {
        urban_val <- mean(urban_areas$Value, na.rm = TRUE)
      }
      
      rural_areas <- latest_data %>% 
        filter(Area %in% c("Mainly Rural", "Islands & Remote Rural"))
      if(nrow(rural_areas) > 0) {
        rural_val <- mean(rural_areas$Value, na.rm = TRUE)
      }
      
      scotland_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(scotland_areas) > 0) {
        scotland_val <- mean(scotland_areas$Value, na.rm = TRUE)
      }
    }
    
    # Case 3: Has 6-fold areas (second/vacant homes data)
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
    cat("Data in 6-fold format, aggregating to 2-fold\n")
    
    # Urban = average of 4 urban areas
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    # Rural = average of 2 rural areas
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    # Scotland = average of all areas or use Scotland area if exists
    scotland_areas <- latest_data %>% filter(Area == "Scotland")
    if(nrow(scotland_areas) > 0) {
      scotland_val <- scotland_areas$Value[1]
    } else {
      all_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(all_areas) > 0) {
        scotland_val <- mean(all_areas$Value, na.rm = TRUE)
      }
    }
    
    # Case 4: Has 8-fold areas (median property price)
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Very Remote Small Towns", "Accessible Rural", "Remote Rural", "Very Remote Rural") %in% unique_areas)) {
    cat("Data in 8-fold format, aggregating to 2-fold\n")
    
    # Urban = average of 5 urban areas 
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Very Remote Small Towns"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    # Rural = average of 3 rural areas
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural", "Very Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    # Scotland = average of all areas
    scotland_areas <- latest_data %>% 
      filter(!grepl("Scotland", Area, ignore.case = TRUE))
    if(nrow(scotland_areas) > 0) {
      scotland_val <- mean(scotland_areas$Value, na.rm = TRUE)
    }
    
    # Case 5: Has 3-fold areas (mixed urban + rural breakdown)
  } else if (any(c("Accessible Rural", "Remote Rural") %in% unique_areas) && "Urban" %in% unique_areas) {
    cat("Data in 3-fold format\n")
    
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    
    # Rural = average of Accessible Rural + Remote Rural
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
  }
  
  cat("Key insights for year", latest_year, "- Urban:", urban_val, "Rural:", rural_val, "Scotland:", scotland_val, "\n")
  
  return(list(
    urban = ifelse(!is.na(urban_val), urban_val, NA),
    rural = ifelse(!is.na(rural_val), rural_val, NA),
    scotland = ifelse(!is.na(scotland_val), scotland_val, NA),
    year = latest_year
  ))
}
# Color Mapping and UI Functions

# Create enhanced color mapping for housing - matching culture module with 8-fold additions
get_housing_colors <- function(areas, classification_type) {
  color_mapping <- list()
  
  # Get base colors from config.R and add 8-fold extensions
  if (classification_type == "8-fold") {
    # Use 6-fold base plus additional shades
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      # Add Very Remote Small Towns - another shade of orange/yellow
      color_mapping[["Very Remote Small Towns"]] <- "#FECEB1"  # Peach/light orange
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
      # Add Very Remote Rural - another shade of green
      color_mapping[["Very Remote Rural"]] <- "#002D04"  # Very dark green
    }
  } else if (classification_type == "6-fold") {
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    }
  } else if (classification_type == "4-fold") {
    color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural" = "#F4E470", 
                          "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")
  } else if (classification_type == "3-fold") {
    colors <- get_classification_colors("3-fold")
    if (!is.null(colors)) {
      color_mapping[["Urban"]] <- colors[["Urban"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    }
  } else if (classification_type == "2-fold") {
    colors <- get_classification_colors("2-fold")
    if (!is.null(colors)) {
      color_mapping[["Urban"]] <- colors[["Urban"]]
      color_mapping[["Rural"]] <- colors[["Rural"]]
    }
  }
  
  # Always set Scotland to gray
  color_mapping[["Scotland"]] <- "#B2B2B2"
  color_mapping[["All"]] <- "#B2B2B2"
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display 
format_housing_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name %in% c("New Build Completions", "AHSP Completions")) {
    return(scales::comma(round(value, 0)))
  } else if (metric_name == "Median Property Price") {
    return(paste0("£", scales::comma(round(value, 0))))
  } else {
    return(paste0(round(value, 1), "%"))
  }
}

# Calculate gap between urban and rural 
calculate_housing_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name %in% c("New Build Completions", "AHSP Completions")) {
    return(scales::comma(round(gap, 0)))
  } else if (metric_name == "Median Property Price") {
    return(paste0("£", scales::comma(round(gap, 0))))
  } else {
    return(paste0(round(gap, 1), "pp"))
  }
}

# Housing dashboard UI 
housing_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      /* STANDARDIZED CSS FOR ALL DASHBOARD MODULES - MATCHING CULTURE MODULE */
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
      .housing-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .housing-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .housing-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .housing-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .housing-metric-description {
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
            h2("Housing", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("housing_classification_selector")
          ),
          
          # Top-center-right: Housing metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("housing_metric", "Housing Metric:", 
                        choices = c("Select a policy metric..." = "", names(housing_metrics)), 
                        selected = "", width = "220px")
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
      condition = "input.housing_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore housing data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Housing-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Median Property Price
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Median Property Price', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("home", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Median Property Price"),
                      div(class = "housing-metric-description", 
                          "Median property prices across Scotland's 8-fold urban rural classification, showing housing affordability differences between large urban areas, towns, and rural communities.")
                    )
                  )
                ),
                
                # Second Homes
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Second Homes', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("key", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Second Homes"),
                      div(class = "housing-metric-description",
                          "Proportion of all dwellings that are second homes, indicating housing market pressures and availability for permanent residents across different area types.")
                    )
                  )
                ),
                
                # Vacant Homes
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Vacant Homes', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("door-open", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Vacant Homes"),
                      div(class = "housing-metric-description",
                          "Proportion of all dwellings that are vacant homes, showing unused housing stock and potential opportunities for increasing housing supply.")
                    )
                  )
                ),
                
                # New Build Completions
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'New Build Completions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("hammer", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "New Build Completions"),
                      div(class = "housing-metric-description",
                          "Total number of new housing completions, tracking housing supply increases and construction activity across urban and rural Scotland.")
                    )
                  )
                ),
                
                # AHSP Completions
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'AHSP Completions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("users", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "AHSP Completions"),
                      div(class = "housing-metric-description",
                          "Affordable Housing Supply Programme completions, showing delivery of affordable housing options across different community types.")
                    )
                  )
                ),
                
                # Housing Conditions
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Housing Conditions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("tools", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Housing Conditions"),
                      div(class = "housing-metric-description",
                          "Percentage of dwellings with urgent disrepair to critical elements, indicating housing quality and maintenance needs across urban and rural areas.")
                    )
                  )
                ),
                
                # EPC Rating
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'EPC rating', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("leaf", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "EPC Rating"),
                      div(class = "housing-metric-description",
                          "Proportion of households rated EPC C or above, tracking energy efficiency improvements and environmental performance of Scotland's housing stock.")
                    )
                  )
                )
              )
            )
          )
      )
    ),
    
    # Show content when a metric is selected
    conditionalPanel(
      condition = "input.housing_metric != ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("housing_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("housing_data_summary"))
          ),
          
          # Show key insights for all metrics except Median Property Price, Second Homes, and Vacant Homes
          conditionalPanel(
            condition = "input.housing_metric != 'Median Property Price' && input.housing_metric != 'Second Homes' && input.housing_metric != 'Vacant Homes'",
            fluidRow(
              box(title = uiOutput("housing_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(width = 3, valueBoxOutput("housing_urban_rate", width = NULL)),
                    column(width = 3, valueBoxOutput("housing_rural_rate", width = NULL)),
                    # Only show Scotland rate for New Build Completions and AHSP Completions
                    conditionalPanel(
                      condition = "input.housing_metric == 'New Build Completions' || input.housing_metric == 'AHSP Completions'",
                      column(width = 3, valueBoxOutput("housing_scotland_rate", width = NULL))
                    ),
                    column(width = 3, valueBoxOutput("housing_urban_rural_gap", width = NULL))
                  ),
                  tags$style(HTML("
                    .value-box {
                      width: 100% !important;
                      border-radius: 6px !important;
                      box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
                      padding: 15px !important;
                      margin-bottom: 15px !important;
                      background-clip: border-box !important;
                    }
                    .value-box .inner {
                      padding: 10px !important;
                    }
                    .value-box .small-box {
                      width: 100% !important;
                      min-height: 100px !important;
                      display: flex !important;
                      flex-direction: column !important;
                      justify-content: center !important;
                    }
                    .value-box h3 {
                      font-size: 1.8em !important;
                      margin: 5px 0 !important;
                      color: white !important;
                    }
                    .value-box p {
                      font-size: 1em !important;
                      margin: 5px 0 !important;
                      color: white !important;
                    }
                    .value-box .icon-large {
                      font-size: 2.5em !important;
                      opacity: 0.8 !important;
                      position: absolute !important;
                      right: 15px !important;
                      top: 15px !important;
                    }
                  "))
              )
            )
          ),
          
          # Show trend chart unless it's AHSP Completions
          conditionalPanel(
            condition = "input.housing_metric != 'AHSP Completions'",
            fluidRow(
              box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                              uiOutput("housing_trend_title"),
                              div(style = "position: absolute; right: 20px;",
                                  downloadButton("housing_trend_download", "Download", class = "excel-download-btn"
                                  ))),
                  status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("housing_trend_chart") %>% withSpinner())
            )
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                            uiOutput("housing_comparison_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("housing_comparison_download", "Download", class = "excel-download-btn"
                                ))),
                status = "primary", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;", uiOutput("housing_year_selector")),
                plotlyOutput("housing_comparison_chart") %>% withSpinner())
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                            uiOutput("housing_table_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("housing_table_download", "Download", class = "excel-download-btn"
                                ))), 
                status = "info", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;",
                    fluidRow(
                      column(6, uiOutput("housing_table_year_filter")),
                      column(6, uiOutput("housing_table_area_filter"))
                    )),
                DT::dataTableOutput("housing_data_table") %>% withSpinner())
          )
      )
    )
  )
}
# ===========================================
# SERVER FUNCTIONS
# ===========================================

# Housing module server
housing_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  housing_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards 
  observeEvent(input$housing_metric_select, {
    updateSelectInput(session, "housing_metric", selected = input$housing_metric_select)
  })
  
  # Dynamic UI outputs 
  output$housing_summary_title <- renderUI({
    req(input$housing_metric, input$housing_classification_type)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    classification_text <- case_when(
      input$housing_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$housing_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$housing_classification_type == "4-fold" ~ "(4-fold RESAS)",
      input$housing_classification_type == "6-fold" ~ "(6-fold Classification)",
      input$housing_classification_type == "8-fold" ~ "(8-fold Classification)",
      TRUE ~ paste0("(", input$housing_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$housing_year_selector <- renderUI({
    req(housing_values$processed_data)
    
    if (nrow(housing_values$processed_data) == 0) return(NULL)
    
    available_years <- if (input$housing_metric == "Median Property Price") {
      sort(unique(housing_values$processed_data$`Financial year`), decreasing = TRUE)
    } else {
      sort(unique(housing_values$processed_data$Year), decreasing = TRUE)
    }
    
    if (length(available_years) > 0) {
      selectInput(
        inputId = "housing_selected_year",
        label = "Select Year for Comparison:",
        choices = available_years,
        selected = available_years[1],
        width = "200px"
      )
    } else {
      NULL
    }
  })
  
  output$housing_classification_selector <- renderUI({
    req(input$housing_metric)
    available_classifications <- housing_metrics[[input$housing_metric]]$classifications
    choices <- list()
    
    # Special handling for different metrics - open most complex fold first
    if (input$housing_metric == "Median Property Price") {
      if("8-fold" %in% available_classifications) choices[["8-fold (Most Detailed)"]] <- "8-fold"
    } else if (input$housing_metric %in% c("Second Homes", "Vacant Homes")) {
      if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
      if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"  
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    } else if (input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    } else {
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    }
    
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("housing_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles 
  output$housing_trend_title <- renderUI({
    req(input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$housing_comparison_title <- renderUI({
    req(input$housing_selected_year, input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste0("Single Year Comparison (", input$housing_selected_year, ") for ", display_name)
  })
  
  output$housing_table_title <- renderUI({
    req(input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste("Data Table for", display_name)
  })
  
  output$housing_key_insights_title <- renderUI({
    req(input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$housing_table_year_filter <- renderUI({
    
    req(housing_values$processed_data)
    
    if (nrow(housing_values$processed_data) == 0) return(NULL)
    
    all_years <- sort(unique(housing_values$processed_data$Year), decreasing = TRUE)
    
    # Format years based on housing_metric
    formatted_years <- if (input$housing_metric == "Median Property Price") {
      sapply(all_years, function(year) {
        paste0(year, "-", substr(year+1, 3, 4))  # e.g., 2023 becomes "2022-23"
      })
    } else {
      as.character(all_years)
    }
    
    choices <- list("All Years" = "all")
    for (i in seq_along(formatted_years)) {
      choices[[formatted_years[i]]] <- all_years[i]
    }
    
    selectInput(
      inputId = "housing_table_year_filter",
      label = "Filter by Year:",
      choices = choices,
      selected = "all",
      width = "100%"
    )
  })
  
  
  
  output$housing_table_area_filter <- renderUI({
    req(housing_values$processed_data, input$housing_classification_type)
    if(nrow(housing_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("housing_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  # Process data reactively 
  observe({
    if (is.null(input$housing_metric) || input$housing_metric == "") {
      housing_values$processed_data <- data.frame()
      housing_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$housing_classification_type)
    
    housing_values$processed_data <- load_housing_data_simple(
      input$housing_metric, 
      input$housing_classification_type
    )
    
    housing_values$data_status <- if(nrow(housing_values$processed_data) > 0) "Housing data loaded" else "No data available"
  })
  
  output$housing_data_summary <- renderUI({
    req(housing_values$processed_data, input$housing_classification_type, input$housing_metric)
    
    if (nrow(housing_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_housing_metric_display_name(input$housing_metric)
    custom_insight <- housing_key_insights[[input$housing_metric]]
    custom_notes <- housing_notes[[input$housing_metric]]
    
    # Define source information with correct URLs
    source_info <- switch(input$housing_metric,
      "Median Property Price" = list(
        text = "Property market report 2024-25 - Registers of Scotland",
        url = "https://www.ros.gov.uk/data-and-statistics/property-market-statistics/property-market-report-2024-25"
      ),
      "Second Homes" = list(
        text = "Household estimates, National Records of Scotland (NRS)", 
        url = "https://www.nrscotland.gov.uk/publications/household-and-dwelling-estimates-other-geographies-2024/"
      ),
      "Vacant Homes" = list(
        text = "Household estimates, National Records of Scotland (NRS)",
        url = "https://www.nrscotland.gov.uk/publications/household-and-dwelling-estimates-other-geographies-2024/"
      ),
      "New Build Completions" = list(
        text = "Housing statistics quarterly update: new housebuilding and affordable housing supply (2024)",
        url = "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.gov.scot%2Fbinaries%2Fcontent%2Fdocuments%2Fgovscot%2Fpublications%2Fstatistics%2F2019%2F06%2Fhousing-statistics-for-scotland-new-house-building%2Fdocuments%2Fall-sectors-starts-and-completions%2Fall-sectors-starts-and-completions%2Fgovscot%253Adocument%2FMarch%252B2025%252B-%252BAll%252BSector%252BNew%252BBuild%252B-%252BWeb%252BTable.xlsx&wdOrigin=BROWSELINK"
      ),
      "AHSP Completions" = list(
        text = "Affordable Housing Supply Programme: out-turn report, Scottish Government",
        url = "https://www.gov.scot/publications/affordable-housing-supply-programme-out-turn-report/"
      ),
      "Housing Conditions" = list(
        text = "Scottish House Condition Survey",
        url = "https://www.gov.scot/collections/scottish-house-condition-survey/"
      ),
      "EPC rating" = list(
        text = "Scottish House Condition Survey", 
        url = "https://www.gov.scot/collections/scottish-house-condition-survey/"
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
  })
  # Value boxes - ALWAYS show 2-fold urban/rural regardless of classification
  output$housing_urban_rate <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- format_housing_value(key_insights$urban, input$housing_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
   # valueBox(value = val, subtitle = paste("Urban Areas", year), icon = icon("city"), color = "yellow")
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
  
  output$housing_rural_rate <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- format_housing_value(key_insights$rural, input$housing_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
   # valueBox(value = val, subtitle = paste("Rural Areas", year), icon = icon("tree"), color = "olive")
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
  
  output$housing_scotland_rate <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- format_housing_value(key_insights$scotland, input$housing_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    # Use appropriate label for completions
    label <- if(input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      "Scotland Total"
    } else {
      "Scotland Average"
    }
    
  #  valueBox(value = val, subtitle = paste(label, year), icon = icon("flag"), color = "maroon")
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", paste(label, year)),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("flag"),
      color = "aqua"
    )
    
  })
  
  output$housing_urban_rural_gap <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- calculate_housing_gap(key_insights$urban, key_insights$rural, input$housing_metric)
    
   # valueBox(value = val, subtitle = "Urban-Rural Difference", icon = icon("balance-scale"), color = "aqua")
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", "Urban-Rural Difference"),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("balance-scale"),
      color = "maroon"
    )
  })
  
  # Trend chart 
  output$housing_trend_chart <- renderPlotly({
    req(housing_values$processed_data, input$housing_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
      
      # Format data for display
      is_count_data <- input$housing_metric %in% c("New Build Completions", "AHSP Completions")
      is_price_data <- input$housing_metric == "Median Property Price"
      
      if (is_price_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 0)
        y_label <- "Price (£)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "-", substr(agg_data$Year+1,3,4), "<br>Area: ", agg_data$Area, "<br>Price: £", scales::comma(agg_data$Value_Rounded))
      } else if (is_count_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 0)
        y_label <- "Number of Completions"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Completions: ", scales::comma(agg_data$Value_Rounded))
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text
      
      # Create plot
      
     
        if (input$housing_metric == "EPC rating") {
          
          # Split data based on methodology change
          agg_data1 <- agg_data |>   filter(Year < 2020) # no data for 2020 or 2021
          agg_data2 <- agg_data |>  filter(Year >= 2022) 
          
          # Build plot with methodology change line
          p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
            geom_vline(xintercept = 2019, linetype = "dashed", color = "darkgrey", size = 0.5) +
            geom_vline(xintercept = 2022, linetype = "dashed", color = "darkgrey", size = 0.5) +
            
            annotate(
              "text",
              x = 2020.5,
              y = max(agg_data2$Value, na.rm = TRUE),
              label = "Data for 2020 and 2021 are impacted by COVID-19",
              angle = 90,
              hjust = 1,
              vjust = 1.1
            ) +
            
            geom_line(size = 1.2, alpha = 0.8) +
            geom_point(size = 3) +
            geom_point(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 3) +
            
            theme_minimal() +
            theme(
              legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            labs(x = "Year", y = y_label, color = "Area Type") +
            
            scale_x_continuous(
              limits = c(min(agg_data$Year, na.rm = TRUE), 2022),#max(agg_data$Year, na.rm = TRUE)),
              breaks = function(x) {
                if (length(x) == 0 || all(is.na(x))) return(c())
                seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)-1), by = 1)
              }
            )
        }
      else{
      
      p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)  # 45-degree rotation
        ) +
        labs(x = "Year", y = y_label, color = "Area Type") +
        
        if(input$housing_metric == "Median Property Price"){
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            },
            labels = function(x) {
              paste0(x, "-", substr(x+1, 3,4))  # e.g., 2022 becomes "2022-23"
            }
          )
        }
      else{
        scale_x_continuous(breaks = function(x) {
          if(length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
          })}}
      
      # Apply housing colors
      area_colors <- get_housing_colors(unique(agg_data$Area), input$housing_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      # Format y-axis
      if (is_price_data) {
        p <- p + scale_y_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_count_data) {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Comparison chart
  output$housing_comparison_chart <- renderPlotly({
    req(housing_values$processed_data, input$housing_classification_type, input$housing_selected_year)
    
    tryCatch({
      agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
      selected_data <- if(input$housing_metric == "Median Property Price"){agg_data %>% filter(Year ==substr(input$housing_selected_year, 1,4))} # take first year from financial years to get Year equiv
      else if(input$housing_metric == "AHSP Completions"){agg_data %>% filter(Year == input$housing_selected_year)}
       else{agg_data %>% filter(Year == as.numeric(input$housing_selected_year))}
      
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$housing_selected_year), textfont = list(size = 16)))
      }
      
      # Format data
      is_count_data <- input$housing_metric %in% c("New Build Completions", "AHSP Completions")
      is_price_data <- input$housing_metric == "Median Property Price"
      
      if (is_price_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 0)
        x_label <- "Price (£)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Price: £", scales::comma(selected_data$Value_Rounded))
      } else if (is_count_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 0)
        x_label <- "Number of Completions"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Completions: ", scales::comma(selected_data$Value_Rounded))
      } else {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "Percentage (%)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Rate: ", selected_data$Value_Rounded, "%")
      }
      
      selected_data$tooltip <- tooltip_text
      
      # Create horizontal bar chart
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Area, Value), fill = Area, text = tooltip)) +
        geom_col(alpha = 0.8, width = 0.7) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "", x = x_label)
      
      # Apply housing colors
      area_colors <- get_housing_colors(unique(selected_data$Area), input$housing_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      # Format x-axis
      if (is_price_data) {
        p <- p + scale_x_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_count_data) {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Download handlers 
  # Housing Trend Download
  output$housing_trend_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$housing_metric)
      paste0("Housing_Trend_", metric, "_", input$housing_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(housing_values$processed_data, input$housing_classification_type)
      
      data <- if(input$housing_metric %in% c("Median Property Price", "AHSP Completions")){simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type) %>%
          select(`Financial year`, Area, Value, Data_Source) %>%
          arrange(`Financial year`, Area)}
      else{simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)}
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Housing Comparison Download
  output$housing_comparison_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$housing_metric)
      paste0("Housing_Comparison_", metric, "_", input$housing_classification_type, "_", input$housing_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(housing_values$processed_data, input$housing_classification_type, input$housing_selected_year)
      
      data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type) %>%
        if(input$housing_metric %in% c("Median Property Price", "AHSP Completions")){
          filter(`Financial year` ==input$housing_selected_year) |> 
            select(`Financial year`, Value, Data_Source)
        }
      else{filter(Year == as.numeric(input$housing_selected_year)) %>%
        select(Year, Area, Value, Data_Source)} %>%
        arrange(Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Housing Table Download
  output$housing_table_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$housing_metric)
      paste0("Housing_Table_", metric, "_", input$housing_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(housing_values$processed_data, input$housing_classification_type)
      
      data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
      
      if (!is.null(input$housing_table_year_filter) && input$housing_table_year_filter != "all") {
        data <- data %>% filter(Year == as.numeric(input$housing_table_year_filter))
      }
      if (!is.null(input$housing_table_area_filter) && input$housing_table_area_filter != "all") {
        data <- data %>% filter(Area == input$housing_table_area_filter)
      }
      
      data <- data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area) 
      
      data <- if (input$housing_metric == "Median Property Price") {
        data$Year <-paste0(year, "-", substr(year+1, 3, 4))
        data
      } else {
        data
      }
      
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Data table 
  output$housing_data_table <- DT::renderDataTable({
    req(housing_values$processed_data, input$housing_classification_type)
    
    if(nrow(housing_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_housing_metric_display_name(input$housing_metric)
    agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    # Apply filters
    filtered_data <- agg_data
    
    if(!is.null(input$housing_table_year_filter) && input$housing_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$housing_table_year_filter))
    }
    
    if(!is.null(input$housing_table_area_filter) && input$housing_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$housing_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    # Prepare table data
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    # Format value column based on metric type
    if (input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      value_col_name <- "Number of Completions"
      table_data$Value <- round(table_data$Value, 0)
    } else if (input$housing_metric == "Median Property Price") {
      value_col_name <- "Price (£)"
      table_data$Value <- round(table_data$Value, 0)
      table_data$Year <-  paste0(table_data$Year, "-", table_data$Year+1)
    } else {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$housing_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "", digits = 0)
    } else if (input$housing_metric == "Median Property Price") {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "£", digits = 0)
    } else {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    }
    
    return(dt)
  })
}
=======

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Housing metrics configuration 
housing_metrics <- list(
  "Median Property Price" = list(
    file_8fold = "housing/median_property_price.csv",
    classifications = c("8-fold"),
    full_name = "Median Property Price by 8-fold Urban Rural Classification"
  ),
  "Second Homes" = list(
    file_6fold = "housing/homes_second.xlsx",
    classifications = c("6-fold"),
    full_name = "Proportion of all dwellings that are second homes"
  ),
  "Vacant Homes" = list(
    file_6fold = "housing/homes_vacant.xlsx",
    classifications = c("6-fold"), 
    full_name = "Proportion of all dwellings that are vacant homes"
  ),
  "New Build Completions" = list(
    file_4fold = "housing/new_completions.csv",
    classifications = c("4-fold"),
    full_name = "New Build Completions (Total Number)",
    is_count_data = TRUE
  ),
  "AHSP Completions" = list(
    file_4fold = "housing/AHSP_completions.csv",
    classifications = c("4-fold"),
    full_name = "AHSP Completions (Total Number)",
    is_count_data = TRUE,
    bar_chart_only = TRUE
  ),
  "Housing Conditions" = list(
    file_2fold = "housing/housing_conditions.csv",
    classifications = c("2-fold"),
    full_name = "Dwellings with urgent disrepair to critical elements"
  ),
  "EPC rating" = list(
    file_2fold = "housing/epc.csv",
    classifications = c("2-fold"),
    full_name = "Proportion of households rated EPC C or above"
  )
)
housing_key_insights <- list(
  "Median Property Price" = "Median property price is the highest in accessible rural areas (£270 000) and lowest in remote small towns (£128 500). ",
  "Second Homes" = "Proportion of all dwellings that are second homes is the highest in remote rural areas (6.1%) whereas in other areas it is below 2% with Scotland average being 0.8%.",
  "Vacant Homes" = "In 2024 3.3% of all dwellings in Scotland were vacant homes, 6.3% of all dwellings in remote rural areas and 4.8% in remote small towns were vacant homes. The lowest proportion of vacant homes was seen in accessible small towns (2.8%).",
  "New Build Completions" = "Urban with substantial rural areas saw 7672 new build completions in 2024, which is more than larger cities (6610). There were 235 new build completions in islands and remote rural areas in 2024.",
  "AHSP Completions" = "There were 4293 AHSP completions in 2023 in urban with substantial rural areas, which is more than all other areas combined. There was a total of 8272 AHSP completions across Scotland in 2023.",
  "Housing Conditions" = "19% of dwellings in rural areas are in need of urgent disrepair to critical elements compared with 16% in urban areas in 2023.",
  "EPC rating" = "60% of households in urban areas were rated EPC C or above in 2023 compared with 36% in rural areas. Scotland average is 56%."
)

# Static Notes for each health metric
housing_notes <- list(
  "Median Property Price" = "",
  "Second Homes" = "The 6-fold urban-rural classification is produced by aggregating 2011 datazones. Data for 2024 has also been published using 2022 datazones (source linked below) but has not been included here as it is not directly comparable to 2011 datazones. Back series of this metric using 2022 datazones will be available in future updates.",
  "Vacant Homes" = "The 6-fold urban-rural classification is produced by aggregating 2011 datazones. Data for 2024 has also been published using 2022 datazones (source linked below) but has not been included here as it is not directly comparable to 2011 datazones. Back series of this metric using 2022 datazones will be available in future updates.",
  "New Build Completions" = "",
  "AHSP Completions" = "",
  "Housing Conditions" = "1. The time period covered by these statistics means that time series results was affected by the coronavirus (COVID-19) pandemic. Specifically there is no 2020 data and 2021 data is not comparable to other waves of the survey. 2.  Urgent disrepair to critical elements has been calculated for the first time in 2019 and back updated for 2018 to allow a comparison. 3. The 2020 urban rural classification is used for 2022 and the 2013/14 urban rural classification (2011 data zone edition) is used for reporting 2018 and 2019 data.",
  "EPC rating" = "1. The Standard Assessment Procedure (SAP) for EPC ratings is periodically reviewed and updated. These different methodologies mean that only data from years with the same SAP metholdology can be compared (2007-2012 and 2014-2017 and 2018-2022). 2. The time period covered by these statistics means that time series results was affected by the coronavirus (COVID-19) pandemic. Specifically, there is no 2020 data and 2021 data is not comparable to other waves of the survey. 3. The most relevant available version of SG urban rural classification for each year is used."
)

# RESAS 4-fold classification mapping
resas_council_mapping <- list(
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

# Helper functions for number formatting 
format_housing_number <- function(number, metric_name) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  
  if (metric_name %in% c("New Build Completions", "AHSP Completions")) {
    return(scales::comma(round(number, 0)))
  } else if (metric_name == "Median Property Price") {
    return(paste0("£", scales::comma(round(number, 0))))
  } else {
    return(paste0(round(number, 1), "%"))
  }
}

# Function to get display name for metrics
get_housing_metric_display_name <- function(metric_name) {
  metric_info <- housing_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# ===========================================
# DATA LOADING FUNCTIONS
# ===========================================

# Load median property price data (8-fold only)
load_median_property_price_data <- function() {
  filepath <- "housing/median_property_price.csv"
  cat("Loading median property price data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read raw CSV and find header
    raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE)
    header_row <- 6  # Known position
    data_rows <- raw_data[7:9, ]  # Known data rows
    headers <- as.character(raw_data[header_row, ])
    colnames(data_rows) <- headers
    
    processed_data <- data_rows %>%
      filter(!is.na(`Financial year`)) %>%
      gather(key = "Area_Type", value = "Value", -`Financial year`) %>%
      mutate(
        Year = case_when(
          grepl("2022-23", `Financial year`) ~ 2022,
          grepl("2023-24", `Financial year`) ~ 2023, 
          grepl("2024-25", `Financial year`) ~ 2024,
          TRUE ~ NA_real_
        ),
        Value = as.numeric(gsub("[^0-9.]", "", Value)),
        # Map to 8-fold areas
        Area = case_when(
          Area_Type == "Large Urban Areas" ~ "Large Urban Areas",
          Area_Type == "Other Urban Areas" ~ "Other Urban Areas",
          Area_Type == "Accessible Small Towns" ~ "Accessible Small Towns", 
          Area_Type == "Remote Small Towns" ~ "Remote Small Towns",
          Area_Type == "Very Remote Small Towns" ~ "Very Remote Small Towns",
          Area_Type == "Accessible Rural Areas" ~ "Accessible Rural",
          Area_Type == "Remote Rural Areas" ~ "Remote Rural",
          Area_Type == "Very Remote Rural Areas" ~ "Very Remote Rural",
          TRUE ~ Area_Type
        ),
        Data_Source = "Registers of Scotland - Property Prices"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source, `Financial year`)
    
    cat(paste("Loaded", nrow(processed_data), "median property price records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading median property price data:", e$message, "\n")
    return(data.frame())
  })
}

load_second_homes_data <- function() {
  filepath <- "housing/homes_second.xlsx"
  cat("Loading second homes data from:", filepath, "\n")
  
  tryCatch({
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file, using second row as header
    raw_data <- read_excel(filepath, col_names = TRUE, skip = 1)
    cat("Raw data columns:", paste(colnames(raw_data), collapse = ", "), "\n")
    
    # Identify the column that likely represents 'Region'
    possible_region_cols <- c("Region", "region", "Area", "area", "Geography", "geography")
    region_col <- colnames(raw_data)[colnames(raw_data) %in% possible_region_cols | grepl("region|area|geography", colnames(raw_data), ignore.case = TRUE)]
    
    if (length(region_col) == 0) {
      cat("No 'Region' column found in data. Available columns:", paste(colnames(raw_data), collapse = ", "), "\n")
      return(data.frame())
    }
    
    region_col <- region_col[1]
    cat("Using region column:", region_col, "\n")
    
    # Rename the region column to 'Region' for consistency
    colnames(raw_data)[colnames(raw_data) == region_col] <- "Region"
    
    # Process data
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100,  # Convert to percentage
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          Region == "Scotland (All)" ~ "Scotland",
          TRUE ~ trimws(Region)
        ),
        Data_Source = "Scottish Government - Second Homes (% of all dwellings)"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "second homes records\n"))
    return(processed_data)
  }, error = function(e) {
    cat("Error loading second homes data:", e$message, "\n")
    return(data.frame())
  })
}

load_vacant_homes_data <- function() {
  filepath <- "housing/homes_vacant.xlsx"
  cat("Loading vacant homes data from:", filepath, "\n")
  
  tryCatch({
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file, using second row as header
    raw_data <- read_excel(filepath, col_names = TRUE, skip = 1)
    cat("Raw data columns:", paste(colnames(raw_data), collapse = ", "), "\n")
    
    # Identify the column that likely represents 'Region'
    possible_region_cols <- c("Region", "region", "Area", "area", "Geography", "geography")
    region_col <- colnames(raw_data)[colnames(raw_data) %in% possible_region_cols | grepl("region|area|geography", colnames(raw_data), ignore.case = TRUE)]
    
    if (length(region_col) == 0) {
      cat("No 'Region' column found in data. Available columns:", paste(colnames(raw_data), collapse = ", "), "\n")
      return(data.frame())
    }
    
    region_col <- region_col[1]
    cat("Using region column:", region_col, "\n")
    
    # Rename the region column to 'Region' for consistency
    colnames(raw_data)[colnames(raw_data) == region_col] <- "Region"
    
    # Process data
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100,  # Convert to percentage
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          Region == "Scotland (All)" ~ "Scotland",
          TRUE ~ trimws(Region)
        ),
        Data_Source = "Scottish Government - Vacant Homes (% of all dwellings)"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "vacant homes records\n"))
    return(processed_data)
  }, error = function(e) {
    cat("Error loading vacant homes data:", e$message, "\n")
    return(data.frame())
  })
}

# Load new build completions data (4-fold RESAS)
load_new_build_completions_data <- function() {
  filepath <- "housing/new_completions.csv"
  cat("Loading new build completions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
   
    raw_data <- read.csv(filepath, header = T, stringsAsFactors = FALSE)
    
    
    possible_region_cols <- c("Region", "region", "Area", "area", "Geography", "geography")
    region_col <- colnames(raw_data)[colnames(raw_data) %in% possible_region_cols | grepl("region|area|geography", colnames(raw_data), ignore.case = TRUE)]
    
    if (length(region_col) == 0) {
      cat("No 'Region' column found in data. Available columns:", paste(colnames(raw_data), collapse = ", "), "\n")
      return(data.frame())
    }
    
    region_col <- region_col[1]
    cat("Using region column:", region_col, "\n")
    
    # Rename the region column to 'Region' for consistency
    colnames(raw_data)[colnames(raw_data) == region_col] <- "Region"
    
 
    # Process data for years 2018-2024
    year_cols <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
    
    # RENAME YEAR COLS - GET RID OF x
    colnames(raw_data) <- gsub("X", "", colnames(raw_data))
    # Process data
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub('[^0-9]', '', Value)),
        Area = Region,
        Data_Source = "Scottish Government - New Build Completions",
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "new build completions records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading new build completions data:", e$message, "\n")
    return(data.frame())
  })
}

# Load AHSP completions data (4-fold RESAS) 
load_ahsp_completions_data <- function() {
  filepath <- "housing/AHSP_completions.csv"
  cat("Loading AHSP completions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read raw text and parse manually for AHSP data
    raw_text <- readLines(filepath)
    cat("AHSP raw text lines:", length(raw_text), "\n")
    
    # The data starts at line 2
    data_start_line <- 2
    
    if(data_start_line > length(raw_text)) {
      cat("Data start line exceeds file length\n")
      return(data.frame())
    }
    
    data_lines <- raw_text[data_start_line:length(raw_text)]
    data_lines <- data_lines[data_lines != "" & !grepl("^,*$", data_lines)]
    
    # Parse each data line manually
    councils_data <- data.frame()
    for(i in seq_along(data_lines)) {
      line <- data_lines[i]
      if(grepl("Scotland|Total|^,", line) || line == "") next
      
      parts <- strsplit(line, ",")[[1]]
      
      if(length(parts) >= 16) {
        council_name <- trimws(parts[1])
        totals_value <- trimws(parts[16])
        
        totals_clean <- as.numeric(gsub('[^0-9]', '', totals_value))
        
        if(!is.na(totals_clean) && council_name != "" && council_name != "Local Authority Area") {
          councils_data <- rbind(councils_data, data.frame(
            Council = council_name,
            Totals = totals_clean,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
      processed_data <- councils_data %>%
        mutate(
          Year = "2022-2023",
          Value = Totals,
          Area = Council,
          `Financial year` = Year,
          Data_Source = "Scottish Government - AHSP Completions"
        ) |> 
      select(Year, Area, Value, Data_Source)
    
  
    
    cat(paste("Loaded", nrow(processed_data), "AHSP completions records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading AHSP completions data:", e$message, "\n")
    return(data.frame())
  })
}

# Load housing conditions data (2-fold)
load_housing_conditions_data <- function() {
  filepath <- "housing/housing_conditions.csv"
  cat("Loading housing conditions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read housing conditions CSV
    raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
    
    # Look for urgent disrepair rows
    urban_row <- NULL
    rural_row <- NULL
    
    for(i in 1:nrow(raw_data)) {
      row_data <- raw_data[i, ]
      if(length(row_data) >= 3 && !is.na(row_data[1]) && !is.na(row_data[3])) {
        first_col <- as.character(row_data[1])
        third_col <- as.character(row_data[3])
        
        if(grepl("Urgent disrepair to one or more critical elements", first_col, ignore.case = TRUE)) {
          if(grepl("Urban", third_col, ignore.case = TRUE) && !grepl("Rural", third_col, ignore.case = TRUE)) {
            urban_row <- row_data
          } else if(grepl("Rural", third_col, ignore.case = TRUE)) {
            rural_row <- row_data
          }
        }
      }
    }
    
    if(is.null(urban_row) || is.null(rural_row)) {
      cat("Could not find both Urban and Rural rows for urgent disrepair\n")
      return(data.frame())
    }
    
    # Extract data from years 2018-2023
    years <- c(2018, 2019, 2020, 2021, 2022, 2023)
    year_columns <- 4:9
    
    clean_percentage <- function(x) {
      if(is.na(x) || x == "" || x == "[x]" || x == "[c]" || x == "[w]") return(NA)
      x <- gsub("%", "", as.character(x))
      x <- gsub("\\s+", "", x)
      x <- gsub('"', "", x)
      return(as.numeric(x))
    }
    
    processed_data <- data.frame()
    
    for(i in 1:length(years)) {
      year <- years[i]
      col_idx <- year_columns[i]
      
      if(col_idx <= length(urban_row) && col_idx <= length(rural_row)) {
        urban_value <- clean_percentage(urban_row[col_idx])
        rural_value <- clean_percentage(rural_row[col_idx])
        
        if(!is.na(urban_value) && !is.na(rural_value)) {
          processed_data <- rbind(processed_data, data.frame(
            Year = c(year, year),
            Area = c("Urban", "Rural"),
            Value = c(urban_value, rural_value),
            Data_Source = "Scottish Government - Housing Conditions",
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat(paste("Loaded", nrow(processed_data), "housing conditions records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading housing conditions data:", e$message, "\n")
    return(data.frame())
  })
}

# Load EPC rating data (2-fold) 
load_epc_rating_data <- function() {
  filepath <- "housing/epc.csv"
  cat("Loading EPC rating data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read with explicit parameters to handle mixed types
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, 
                         check.names = FALSE, na.strings = c("", "NA", "#N/A", "N/A"))
    
    cat("Read", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
    
    # Process EPC data
    epc_data <- raw_data %>%
      filter(!is.na(Region), Region != "SAP Methodology") %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        # Handle both percentage strings and numbers
        Value = case_when(
          is.na(Value) ~ NA_real_,
          Value %in% c("#N/A", "N/A", "") ~ NA_real_,
          TRUE ~ as.numeric(gsub("%", "", as.character(Value)))
        )
      ) %>%
      filter(!is.na(Value), Year != 2020) %>%
      mutate(
        Area = case_when(
          grepl("Urban.*EPC", Region, ignore.case = TRUE) ~ "Urban",
          grepl("Rural.*EPC", Region, ignore.case = TRUE) ~ "Rural",
          grepl("All.*EPC", Region, ignore.case = TRUE) ~ "All"
        ),
        Data_Source = "Scottish Government - EPC Rating (C or above)"
      ) %>%
      filter(!is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(epc_data), "EPC rating records\n"))
    return(epc_data)
    
  }, error = function(e) {
    cat("Error loading EPC data:", e$message, "\n")
    return(data.frame())
  })
}
# Processing Functions and Aggregation

load_housing_data_simple <- function(metric_name, classification_type) {
  metric_info <- housing_metrics[[metric_name]]
  if (is.null(metric_info)) return(data.frame())
  
  if (metric_name == "Second Homes" && classification_type == "6-fold") {
    return(load_second_homes_data())
  } else if (metric_name == "Vacant Homes" && classification_type == "6-fold") {
    return(load_vacant_homes_data())
  } else if (metric_name == "Median Property Price" && classification_type == "8-fold") {
    return(load_median_property_price_data())
  } else if (metric_name == "New Build Completions") {
    completions_4fold <- load_new_build_completions_data()
    if (classification_type == "4-fold") return(completions_4fold)
    if (classification_type == "2-fold") return(create_completions_2fold(completions_4fold))
  } else if (metric_name == "AHSP Completions") {
    ahsp_4fold <- load_ahsp_completions_data()
    if (classification_type == "4-fold") return(ahsp_4fold)
    if (classification_type == "2-fold") return(create_completions_2fold(ahsp_4fold))
  } else if (metric_name == "Housing Conditions" && classification_type == "2-fold") {
    return(load_housing_conditions_data())
  } else if (metric_name == "EPC rating" && classification_type == "2-fold") {
    return(load_epc_rating_data())
  }
  return(data.frame())
}

# Create 2-fold aggregation for homes data (second/vacant)
create_homes_2fold <- function(homes_6fold_data) {
  cat("Creating homes 2-fold aggregation\n")
  
  if(nrow(homes_6fold_data) == 0) {
    return(data.frame())
  }
  
  # Urban = average of 4 urban areas
  urban_data <- homes_6fold_data %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Rural = average of 2 rural areas
  rural_data <- homes_6fold_data %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Rural")
  
  # Scotland = average of all areas
  scotland_data <- homes_6fold_data %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Scotland")
  
  return(bind_rows(urban_data, rural_data, scotland_data))
}

# Create 3-fold aggregation for homes data (second/vacant)
create_homes_3fold <- function(homes_6fold_data) {
  cat("Creating homes 3-fold aggregation\n")
  
  if(nrow(homes_6fold_data) == 0) {
    return(data.frame())
  }
  
  # Urban = average of 4 urban areas
  urban_data <- homes_6fold_data %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Keep Rural areas separate (Accessible Rural and Remote Rural)
  rural_data <- homes_6fold_data %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural"))
  
  # Scotland = average of all areas
  scotland_data <- homes_6fold_data %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Scotland")
  
  return(bind_rows(urban_data, rural_data, scotland_data))
}
# 
# # # Create 2-fold aggregation for completions data (new build/AHSP) - SUMS not averages
# # create_completions_2fold <- function(completions_4fold_data) {
# #   cat("Creating completions 2-fold aggregation (using sums)\n")
# #   
# #   if(nrow(completions_4fold_data) == 0) {
# #     return(data.frame())
# #   }
# #   
#   # Urban = sum of Urban with Substantial Rural + Larger Cities
#   urban_data <- completions_4fold_data %>%
#     filter(Area %in% c("Urban with Substantial Rural", "Larger Cities")) %>%
#     group_by(Year, Data_Source) %>%
#     summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(Area = "Urban")
#   
#   # Rural = sum of Mainly Rural + Islands & Remote Rural
#   rural_data <- completions_4fold_data %>%
#     filter(Area %in% c("Mainly Rural", "Islands & Remote Rural")) %>%
#     group_by(Year, Data_Source) %>%
#     summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(Area = "Rural")
#   
#   # Scotland = sum of all areas
#   scotland_data <- completions_4fold_data %>%
#     group_by(Year, Data_Source) %>%
#     summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(Area = "Scotland")
#   
#   return(bind_rows(urban_data, rural_data, scotland_data))
# }

# Simple aggregate function 
simple_aggregate_housing_data <- function(processed_data, classification_type = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  # Data is already in the right format from loading functions
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}

# Get key insights function  
get_housing_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  cat("Processing key insights for:", metric_name, "\n")
  cat("Data dimensions:", nrow(processed_data), "rows\n")
  cat("Unique areas in data:", paste(unique(processed_data$Area), collapse = ", "), "\n")
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  # Initialize values
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  # Case 1: Already has Urban/Rural/Scotland (2-fold data)
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    cat("Data already in 2-fold format\n")
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
    
    # Case 2: Has 4-fold RESAS areas (completions data)
  } else if (any(c("Larger Cities", "Urban with Substantial Rural", "Mainly Rural", "Islands & Remote Rural") %in% unique_areas)) {
    cat("Data in 4-fold RESAS format, aggregating to 2-fold\n")
    
    # For count data (completions), use sums; for others, use means
    is_count_data <- metric_name %in% c("New Build Completions", "AHSP Completions")
    
    if (is_count_data) {
      # Urban = sum of Larger Cities + Urban with Substantial Rural
      urban_areas <- latest_data %>% 
        filter(Area %in% c("Larger Cities", "Urban with Substantial Rural"))
      if(nrow(urban_areas) > 0) {
        urban_val <- sum(urban_areas$Value, na.rm = TRUE)
      }
      
      # Rural = sum of Mainly Rural + Islands & Remote Rural
      rural_areas <- latest_data %>% 
        filter(Area %in% c("Mainly Rural", "Islands & Remote Rural"))
      if(nrow(rural_areas) > 0) {
        rural_val <- sum(rural_areas$Value, na.rm = TRUE)
      }
      
      # Scotland = sum of all areas
      scotland_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(scotland_areas) > 0) {
        scotland_val <- sum(scotland_areas$Value, na.rm = TRUE)
      }
    } else {
      # For non-count data, use averages
      urban_areas <- latest_data %>% 
        filter(Area %in% c("Larger Cities", "Urban with Substantial Rural"))
      if(nrow(urban_areas) > 0) {
        urban_val <- mean(urban_areas$Value, na.rm = TRUE)
      }
      
      rural_areas <- latest_data %>% 
        filter(Area %in% c("Mainly Rural", "Islands & Remote Rural"))
      if(nrow(rural_areas) > 0) {
        rural_val <- mean(rural_areas$Value, na.rm = TRUE)
      }
      
      scotland_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(scotland_areas) > 0) {
        scotland_val <- mean(scotland_areas$Value, na.rm = TRUE)
      }
    }
    
    # Case 3: Has 6-fold areas (second/vacant homes data)
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
    cat("Data in 6-fold format, aggregating to 2-fold\n")
    
    # Urban = average of 4 urban areas
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    # Rural = average of 2 rural areas
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    # Scotland = average of all areas or use Scotland area if exists
    scotland_areas <- latest_data %>% filter(Area == "Scotland")
    if(nrow(scotland_areas) > 0) {
      scotland_val <- scotland_areas$Value[1]
    } else {
      all_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(all_areas) > 0) {
        scotland_val <- mean(all_areas$Value, na.rm = TRUE)
      }
    }
    
    # Case 4: Has 8-fold areas (median property price)
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Very Remote Small Towns", "Accessible Rural", "Remote Rural", "Very Remote Rural") %in% unique_areas)) {
    cat("Data in 8-fold format, aggregating to 2-fold\n")
    
    # Urban = average of 5 urban areas 
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Very Remote Small Towns"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    # Rural = average of 3 rural areas
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural", "Very Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    # Scotland = average of all areas
    scotland_areas <- latest_data %>% 
      filter(!grepl("Scotland", Area, ignore.case = TRUE))
    if(nrow(scotland_areas) > 0) {
      scotland_val <- mean(scotland_areas$Value, na.rm = TRUE)
    }
    
    # Case 5: Has 3-fold areas (mixed urban + rural breakdown)
  } else if (any(c("Accessible Rural", "Remote Rural") %in% unique_areas) && "Urban" %in% unique_areas) {
    cat("Data in 3-fold format\n")
    
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    
    # Rural = average of Accessible Rural + Remote Rural
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
  }
  
  cat("Key insights for year", latest_year, "- Urban:", urban_val, "Rural:", rural_val, "Scotland:", scotland_val, "\n")
  
  return(list(
    urban = ifelse(!is.na(urban_val), urban_val, NA),
    rural = ifelse(!is.na(rural_val), rural_val, NA),
    scotland = ifelse(!is.na(scotland_val), scotland_val, NA),
    year = latest_year
  ))
}
# Color Mapping and UI Functions

# Create enhanced color mapping for housing - matching culture module with 8-fold additions
get_housing_colors <- function(areas, classification_type) {
  color_mapping <- list()
  
  # Get base colors from config.R and add 8-fold extensions
  if (classification_type == "8-fold") {
    # Use 6-fold base plus additional shades
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      # Add Very Remote Small Towns - another shade of orange/yellow
      color_mapping[["Very Remote Small Towns"]] <- "#FECEB1"  # Peach/light orange
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
      # Add Very Remote Rural - another shade of green
      color_mapping[["Very Remote Rural"]] <- "#002D04"  # Very dark green
    }
  } else if (classification_type == "6-fold") {
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    }
  } else if (classification_type == "4-fold") {
    color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural" = "#F4E470", 
                          "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")
  } else if (classification_type == "3-fold") {
    colors <- get_classification_colors("3-fold")
    if (!is.null(colors)) {
      color_mapping[["Urban"]] <- colors[["Urban"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    }
  } else if (classification_type == "2-fold") {
    colors <- get_classification_colors("2-fold")
    if (!is.null(colors)) {
      color_mapping[["Urban"]] <- colors[["Urban"]]
      color_mapping[["Rural"]] <- colors[["Rural"]]
    }
  }
  
  # Always set Scotland to gray
  color_mapping[["Scotland"]] <- "#B2B2B2"
  color_mapping[["All"]] <- "#B2B2B2"
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display 
format_housing_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name %in% c("New Build Completions", "AHSP Completions")) {
    return(scales::comma(round(value, 0)))
  } else if (metric_name == "Median Property Price") {
    return(paste0("£", scales::comma(round(value, 0))))
  } else {
    return(paste0(round(value, 1), "%"))
  }
}

# Calculate gap between urban and rural 
calculate_housing_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name %in% c("New Build Completions", "AHSP Completions")) {
    return(scales::comma(round(gap, 0)))
  } else if (metric_name == "Median Property Price") {
    return(paste0("£", scales::comma(round(gap, 0))))
  } else {
    return(paste0(round(gap, 1), "pp"))
  }
}

# Housing dashboard UI 
housing_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      /* STANDARDIZED CSS FOR ALL DASHBOARD MODULES - MATCHING CULTURE MODULE */
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
      .housing-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .housing-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .housing-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .housing-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .housing-metric-description {
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
            h2("Housing", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("housing_classification_selector")
          ),
          
          # Top-center-right: Housing metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("housing_metric", "Housing Metric:", 
                        choices = c("Select a policy metric..." = "", names(housing_metrics)), 
                        selected = "", width = "220px")
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
      condition = "input.housing_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore housing data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Housing-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Median Property Price
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Median Property Price', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("home", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Median Property Price"),
                      div(class = "housing-metric-description", 
                          "Median property prices across Scotland's 8-fold urban rural classification, showing housing affordability differences between large urban areas, towns, and rural communities.")
                    )
                  )
                ),
                
                # Second Homes
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Second Homes', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("key", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Second Homes"),
                      div(class = "housing-metric-description",
                          "Proportion of all dwellings that are second homes, indicating housing market pressures and availability for permanent residents across different area types.")
                    )
                  )
                ),
                
                # Vacant Homes
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Vacant Homes', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("door-open", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Vacant Homes"),
                      div(class = "housing-metric-description",
                          "Proportion of all dwellings that are vacant homes, showing unused housing stock and potential opportunities for increasing housing supply.")
                    )
                  )
                ),
                
                # New Build Completions
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'New Build Completions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("hammer", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "New Build Completions"),
                      div(class = "housing-metric-description",
                          "Total number of new housing completions, tracking housing supply increases and construction activity across urban and rural Scotland.")
                    )
                  )
                ),
                
                # AHSP Completions
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'AHSP Completions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("users", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "AHSP Completions"),
                      div(class = "housing-metric-description",
                          "Affordable Housing Supply Programme completions, showing delivery of affordable housing options across different community types.")
                    )
                  )
                ),
                
                # Housing Conditions
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'Housing Conditions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("tools", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "Housing Conditions"),
                      div(class = "housing-metric-description",
                          "Percentage of dwellings with urgent disrepair to critical elements, indicating housing quality and maintenance needs across urban and rural areas.")
                    )
                  )
                ),
                
                # EPC Rating
                div(
                  class = "housing-metrics-card",
                  onclick = "Shiny.setInputValue('housing_metric_select', 'EPC rating', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("leaf", class = "housing-metric-icon"),
                    div(
                      div(class = "housing-metric-title", "EPC Rating"),
                      div(class = "housing-metric-description",
                          "Proportion of households rated EPC C or above, tracking energy efficiency improvements and environmental performance of Scotland's housing stock.")
                    )
                  )
                )
              )
            )
          )
      )
    ),
    
    # Show content when a metric is selected
    conditionalPanel(
      condition = "input.housing_metric != ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("housing_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("housing_data_summary"))
          ),
          
          # Show key insights for all metrics except Median Property Price, Second Homes, and Vacant Homes
          conditionalPanel(
            condition = "input.housing_metric != 'Median Property Price' && input.housing_metric != 'Second Homes' && input.housing_metric != 'Vacant Homes'",
            fluidRow(
              box(title = uiOutput("housing_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(width = 3, valueBoxOutput("housing_urban_rate", width = NULL)),
                    column(width = 3, valueBoxOutput("housing_rural_rate", width = NULL)),
                    # Only show Scotland rate for New Build Completions and AHSP Completions
                    conditionalPanel(
                      condition = "input.housing_metric == 'New Build Completions' || input.housing_metric == 'AHSP Completions'",
                      column(width = 3, valueBoxOutput("housing_scotland_rate", width = NULL))
                    ),
                    column(width = 3, valueBoxOutput("housing_urban_rural_gap", width = NULL))
                  ),
                  tags$style(HTML("
                    .value-box {
                      width: 100% !important;
                      border-radius: 6px !important;
                      box-shadow: 0 2px 8px rgba(0,0,0,0.1) !important;
                      padding: 15px !important;
                      margin-bottom: 15px !important;
                      background-clip: border-box !important;
                    }
                    .value-box .inner {
                      padding: 10px !important;
                    }
                    .value-box .small-box {
                      width: 100% !important;
                      min-height: 100px !important;
                      display: flex !important;
                      flex-direction: column !important;
                      justify-content: center !important;
                    }
                    .value-box h3 {
                      font-size: 1.8em !important;
                      margin: 5px 0 !important;
                      color: white !important;
                    }
                    .value-box p {
                      font-size: 1em !important;
                      margin: 5px 0 !important;
                      color: white !important;
                    }
                    .value-box .icon-large {
                      font-size: 2.5em !important;
                      opacity: 0.8 !important;
                      position: absolute !important;
                      right: 15px !important;
                      top: 15px !important;
                    }
                  "))
              )
            )
          ),
          
          # Show trend chart unless it's AHSP Completions
          conditionalPanel(
            condition = "input.housing_metric != 'AHSP Completions'",
            fluidRow(
              box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                              uiOutput("housing_trend_title"),
                              div(style = "position: absolute; right: 20px;",
                                  downloadButton("housing_trend_download", "Download", class = "excel-download-btn"
                                  ))),
                  status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("housing_trend_chart") %>% withSpinner())
            )
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                            uiOutput("housing_comparison_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("housing_comparison_download", "Download", class = "excel-download-btn"
                                ))),
                status = "primary", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;", uiOutput("housing_year_selector")),
                plotlyOutput("housing_comparison_chart") %>% withSpinner())
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                            uiOutput("housing_table_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("housing_table_download", "Download", class = "excel-download-btn"
                                ))), 
                status = "info", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;",
                    fluidRow(
                      column(6, uiOutput("housing_table_year_filter")),
                      column(6, uiOutput("housing_table_area_filter"))
                    )),
                DT::dataTableOutput("housing_data_table") %>% withSpinner())
          )
      )
    )
  )
}
# ===========================================
# SERVER FUNCTIONS
# ===========================================

# Housing module server
housing_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  housing_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards 
  observeEvent(input$housing_metric_select, {
    updateSelectInput(session, "housing_metric", selected = input$housing_metric_select)
  })
  
  # Dynamic UI outputs 
  output$housing_summary_title <- renderUI({
    req(input$housing_metric, input$housing_classification_type)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    classification_text <- case_when(
      input$housing_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$housing_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$housing_classification_type == "4-fold" ~ "(4-fold RESAS)",
      input$housing_classification_type == "6-fold" ~ "(6-fold Classification)",
      input$housing_classification_type == "8-fold" ~ "(8-fold Classification)",
      TRUE ~ paste0("(", input$housing_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$housing_year_selector <- renderUI({
    req(housing_values$processed_data)
    
    if (nrow(housing_values$processed_data) == 0) return(NULL)
    
    available_years <- if (input$housing_metric == "Median Property Price") {
      sort(unique(housing_values$processed_data$`Financial year`), decreasing = TRUE)
    } else {
      sort(unique(housing_values$processed_data$Year), decreasing = TRUE)
    }
    
    if (length(available_years) > 0) {
      selectInput(
        inputId = "housing_selected_year",
        label = "Select Year for Comparison:",
        choices = available_years,
        selected = available_years[1],
        width = "200px"
      )
    } else {
      NULL
    }
  })
  
  output$housing_classification_selector <- renderUI({
    req(input$housing_metric)
    available_classifications <- housing_metrics[[input$housing_metric]]$classifications
    choices <- list()
    
    # Special handling for different metrics - open most complex fold first
    if (input$housing_metric == "Median Property Price") {
      if("8-fold" %in% available_classifications) choices[["8-fold (Most Detailed)"]] <- "8-fold"
    } else if (input$housing_metric %in% c("Second Homes", "Vacant Homes")) {
      if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
      if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"  
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    } else if (input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    } else {
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    }
    
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("housing_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles 
  output$housing_trend_title <- renderUI({
    req(input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$housing_comparison_title <- renderUI({
    req(input$housing_selected_year, input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste0("Single Year Comparison (", input$housing_selected_year, ") for ", display_name)
  })
  
  output$housing_table_title <- renderUI({
    req(input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste("Data Table for", display_name)
  })
  
  output$housing_key_insights_title <- renderUI({
    req(input$housing_metric)
    display_name <- get_housing_metric_display_name(input$housing_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$housing_table_year_filter <- renderUI({
    
    req(housing_values$processed_data)
    
    if (nrow(housing_values$processed_data) == 0) return(NULL)
    
    all_years <- sort(unique(housing_values$processed_data$Year), decreasing = TRUE)
    
    # Format years based on housing_metric
    formatted_years <- if (input$housing_metric == "Median Property Price") {
      sapply(all_years, function(year) {
        paste0(year, "-", substr(year+1, 3, 4))  # e.g., 2023 becomes "2022-23"
      })
    } else {
      as.character(all_years)
    }
    
    choices <- list("All Years" = "all")
    for (i in seq_along(formatted_years)) {
      choices[[formatted_years[i]]] <- all_years[i]
    }
    
    selectInput(
      inputId = "housing_table_year_filter",
      label = "Filter by Year:",
      choices = choices,
      selected = "all",
      width = "100%"
    )
  })
  
  
  
  output$housing_table_area_filter <- renderUI({
    req(housing_values$processed_data, input$housing_classification_type)
    if(nrow(housing_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("housing_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  # Process data reactively 
  observe({
    if (is.null(input$housing_metric) || input$housing_metric == "") {
      housing_values$processed_data <- data.frame()
      housing_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$housing_classification_type)
    
    housing_values$processed_data <- load_housing_data_simple(
      input$housing_metric, 
      input$housing_classification_type
    )
    
    housing_values$data_status <- if(nrow(housing_values$processed_data) > 0) "Housing data loaded" else "No data available"
  })
  
  output$housing_data_summary <- renderUI({
    req(housing_values$processed_data, input$housing_classification_type, input$housing_metric)
    
    if (nrow(housing_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_housing_metric_display_name(input$housing_metric)
    custom_insight <- housing_key_insights[[input$housing_metric]]
    custom_notes <- housing_notes[[input$housing_metric]]
    
    # Define source information with correct URLs
    source_info <- switch(input$housing_metric,
      "Median Property Price" = list(
        text = "Property market report 2024-25 - Registers of Scotland",
        url = "https://www.ros.gov.uk/data-and-statistics/property-market-statistics/property-market-report-2024-25"
      ),
      "Second Homes" = list(
        text = "Household estimates, National Records of Scotland (NRS)", 
        url = "https://www.nrscotland.gov.uk/publications/household-and-dwelling-estimates-other-geographies-2024/"
      ),
      "Vacant Homes" = list(
        text = "Household estimates, National Records of Scotland (NRS)",
        url = "https://www.nrscotland.gov.uk/publications/household-and-dwelling-estimates-other-geographies-2024/"
      ),
      "New Build Completions" = list(
        text = "Housing statistics quarterly update: new housebuilding and affordable housing supply (2024)",
        url = "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.gov.scot%2Fbinaries%2Fcontent%2Fdocuments%2Fgovscot%2Fpublications%2Fstatistics%2F2019%2F06%2Fhousing-statistics-for-scotland-new-house-building%2Fdocuments%2Fall-sectors-starts-and-completions%2Fall-sectors-starts-and-completions%2Fgovscot%253Adocument%2FMarch%252B2025%252B-%252BAll%252BSector%252BNew%252BBuild%252B-%252BWeb%252BTable.xlsx&wdOrigin=BROWSELINK"
      ),
      "AHSP Completions" = list(
        text = "Affordable Housing Supply Programme: out-turn report, Scottish Government",
        url = "https://www.gov.scot/publications/affordable-housing-supply-programme-out-turn-report/"
      ),
      "Housing Conditions" = list(
        text = "Scottish House Condition Survey",
        url = "https://www.gov.scot/collections/scottish-house-condition-survey/"
      ),
      "EPC rating" = list(
        text = "Scottish House Condition Survey", 
        url = "https://www.gov.scot/collections/scottish-house-condition-survey/"
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
  })
  # Value boxes - ALWAYS show 2-fold urban/rural regardless of classification
  output$housing_urban_rate <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- format_housing_value(key_insights$urban, input$housing_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
   # valueBox(value = val, subtitle = paste("Urban Areas", year), icon = icon("city"), color = "yellow")
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
  
  output$housing_rural_rate <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- format_housing_value(key_insights$rural, input$housing_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
   # valueBox(value = val, subtitle = paste("Rural Areas", year), icon = icon("tree"), color = "olive")
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
  
  output$housing_scotland_rate <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- format_housing_value(key_insights$scotland, input$housing_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    # Use appropriate label for completions
    label <- if(input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      "Scotland Total"
    } else {
      "Scotland Average"
    }
    
  #  valueBox(value = val, subtitle = paste(label, year), icon = icon("flag"), color = "maroon")
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", paste(label, year)),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("flag"),
      color = "aqua"
    )
    
  })
  
  output$housing_urban_rural_gap <- renderValueBox({
    req(housing_values$processed_data, input$housing_metric)
    
    key_insights <- get_housing_key_insights(housing_values$processed_data, input$housing_metric, "2-fold")
    val <- calculate_housing_gap(key_insights$urban, key_insights$rural, input$housing_metric)
    
   # valueBox(value = val, subtitle = "Urban-Rural Gap", icon = icon("balance-scale"), color = "aqua")
    
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
  
  # Trend chart 
  output$housing_trend_chart <- renderPlotly({
    req(housing_values$processed_data, input$housing_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
      
      # Format data for display
      is_count_data <- input$housing_metric %in% c("New Build Completions", "AHSP Completions")
      is_price_data <- input$housing_metric == "Median Property Price"
      
      if (is_price_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 0)
        y_label <- "Price (£)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "-", substr(agg_data$Year+1,3,4), "<br>Area: ", agg_data$Area, "<br>Price: £", scales::comma(agg_data$Value_Rounded))
      } else if (is_count_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 0)
        y_label <- "Number of Completions"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Completions: ", scales::comma(agg_data$Value_Rounded))
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text
      
      # Create plot
      
     
        if (input$housing_metric == "EPC rating") {
          
          # Split data based on methodology change
          agg_data1 <- agg_data |>   filter(Year < 2020) # no data for 2020 or 2021
          agg_data2 <- agg_data |>  filter(Year >= 2022) 
          
          # Build plot with methodology change line
          p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
            geom_vline(xintercept = 2019, linetype = "dashed", color = "darkgrey", size = 0.5) +
            geom_vline(xintercept = 2022, linetype = "dashed", color = "darkgrey", size = 0.5) +
            
            annotate(
              "text",
              x = 2020.5,
              y = max(agg_data2$Value, na.rm = TRUE),
              label = "Data for 2020 and 2021 are impacted by COVID-19",
              angle = 90,
              hjust = 1,
              vjust = 1.1
            ) +
            
            geom_line(size = 1.2, alpha = 0.8) +
            geom_point(size = 3) +
            geom_point(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 3) +
            
            theme_minimal() +
            theme(
              legend.position = "bottom",
              axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            labs(x = "Year", y = y_label, color = "Area Type") +
            
            scale_x_continuous(
              limits = c(min(agg_data$Year, na.rm = TRUE), 2022),#max(agg_data$Year, na.rm = TRUE)),
              breaks = function(x) {
                if (length(x) == 0 || all(is.na(x))) return(c())
                seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)-1), by = 1)
              }
            )
        }
      else{
      
      p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)  # 45-degree rotation
        ) +
        labs(x = "Year", y = y_label, color = "Area Type") +
        
        if(input$housing_metric == "Median Property Price"){
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            },
            labels = function(x) {
              paste0(x, "-", substr(x+1, 3,4))  # e.g., 2022 becomes "2022-23"
            }
          )
        }
      else{
        scale_x_continuous(breaks = function(x) {
          if(length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
          })}}
      
      # Apply housing colors
      area_colors <- get_housing_colors(unique(agg_data$Area), input$housing_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      # Format y-axis
      if (is_price_data) {
        p <- p + scale_y_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_count_data) {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Comparison chart
  output$housing_comparison_chart <- renderPlotly({
    req(housing_values$processed_data, input$housing_classification_type, input$housing_selected_year)
    
    tryCatch({
      agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
      selected_data <- if(input$housing_metric == "Median Property Price"){agg_data %>% filter(Year ==substr(input$housing_selected_year, 1,4))} # take first year from financial years to get Year equiv
      else if(input$housing_metric == "AHSP Completions"){agg_data %>% filter(Year == input$housing_selected_year)}
       else{agg_data %>% filter(Year == as.numeric(input$housing_selected_year))}
      
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$housing_selected_year), textfont = list(size = 16)))
      }
      
      # Format data
      is_count_data <- input$housing_metric %in% c("New Build Completions", "AHSP Completions")
      is_price_data <- input$housing_metric == "Median Property Price"
      
      if (is_price_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 0)
        x_label <- "Price (£)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Price: £", scales::comma(selected_data$Value_Rounded))
      } else if (is_count_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 0)
        x_label <- "Number of Completions"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Completions: ", scales::comma(selected_data$Value_Rounded))
      } else {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "Percentage (%)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Rate: ", selected_data$Value_Rounded, "%")
      }
      
      selected_data$tooltip <- tooltip_text
      
      # Create horizontal bar chart
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Area, Value), fill = Area, text = tooltip)) +
        geom_col(alpha = 0.8, width = 0.7) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "", x = x_label)
      
      # Apply housing colors
      area_colors <- get_housing_colors(unique(selected_data$Area), input$housing_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      # Format x-axis
      if (is_price_data) {
        p <- p + scale_x_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_count_data) {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Download handlers 
  # Housing Trend Download
  output$housing_trend_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$housing_metric)
      paste0("Housing_Trend_", metric, "_", input$housing_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(housing_values$processed_data, input$housing_classification_type)
      
      data <- if(input$housing_metric %in% c("Median Property Price", "AHSP Completions")){simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type) %>%
          select(`Financial year`, Area, Value, Data_Source) %>%
          arrange(`Financial year`, Area)}
      else{simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)}
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Housing Comparison Download
  output$housing_comparison_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$housing_metric)
      paste0("Housing_Comparison_", metric, "_", input$housing_classification_type, "_", input$housing_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(housing_values$processed_data, input$housing_classification_type, input$housing_selected_year)
      
      data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type) %>%
        if(input$housing_metric %in% c("Median Property Price", "AHSP Completions")){
          filter(`Financial year` ==input$housing_selected_year) |> 
            select(`Financial year`, Value, Data_Source)
        }
      else{filter(Year == as.numeric(input$housing_selected_year)) %>%
        select(Year, Area, Value, Data_Source)} %>%
        arrange(Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Housing Table Download
  output$housing_table_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$housing_metric)
      paste0("Housing_Table_", metric, "_", input$housing_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(housing_values$processed_data, input$housing_classification_type)
      
      data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
      
      if (!is.null(input$housing_table_year_filter) && input$housing_table_year_filter != "all") {
        data <- data %>% filter(Year == as.numeric(input$housing_table_year_filter))
      }
      if (!is.null(input$housing_table_area_filter) && input$housing_table_area_filter != "all") {
        data <- data %>% filter(Area == input$housing_table_area_filter)
      }
      
      data <- data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area) 
      
      data <- if (input$housing_metric == "Median Property Price") {
        data$Year <-paste0(year, "-", substr(year+1, 3, 4))
        data
      } else {
        data
      }
      
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Data table 
  output$housing_data_table <- DT::renderDataTable({
    req(housing_values$processed_data, input$housing_classification_type)
    
    if(nrow(housing_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_housing_metric_display_name(input$housing_metric)
    agg_data <- simple_aggregate_housing_data(housing_values$processed_data, input$housing_classification_type)
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    # Apply filters
    filtered_data <- agg_data
    
    if(!is.null(input$housing_table_year_filter) && input$housing_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$housing_table_year_filter))
    }
    
    if(!is.null(input$housing_table_area_filter) && input$housing_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$housing_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    # Prepare table data
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    # Format value column based on metric type
    if (input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      value_col_name <- "Number of Completions"
      table_data$Value <- round(table_data$Value, 0)
    } else if (input$housing_metric == "Median Property Price") {
      value_col_name <- "Price (£)"
      table_data$Value <- round(table_data$Value, 0)
      table_data$Year <-  paste0(table_data$Year, "-", table_data$Year+1)
    } else {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$housing_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (input$housing_metric %in% c("New Build Completions", "AHSP Completions")) {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "", digits = 0)
    } else if (input$housing_metric == "Median Property Price") {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "£", digits = 0)
    } else {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    }
    
    return(dt)
  })
}
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
