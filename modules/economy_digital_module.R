
# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

start_years <- list(
  "Table 1" = 1998,
  "Table 2" = 2010,
  "Table 3" = 2010,
  "Table 4" = 2010,
  "Table 5" = 2010,
  "Table 6" = 2010,
  "Table 7" = 2010,
  "Table 8" = 2010,
  "Table 9" = 2010,
  "Table 17" = 2004
)
# Economy and Digital metrics 
economy_metrics <- list(
  "Regional Gross Value Added" = list(
    file = "Table 1",
    file_type = "multi_sheet",
    classifications = c("2-fold", 
      "4-fold"),
    full_name = "Regional Gross Value Added (£ millions)",
    aggregate_method = "sum"  # sum instead of average
  ),
  "Number of VAT/PAYE Businesses" = list(
    file = "Table 2",
    file_type = "multi_sheet",
    classifications = c("2-fold", "4-fold"),
      "4-fold",
    full_name = "Number of VAT/PAYE Businesses",
    aggregate_method = "sum"  #sum instead of average
  ),
  "Registered private sector businesses" = list(
    file = "registered_businesses_2_6_fold.xlsx",
    file_type = "single_file", 
    classifications = c("2-fold", #"3-fold", 
                        "6-fold"),
    full_name = "Number of registered private sector businesses",
    aggregate_method = "sum"
  ),
  "High growth private businesses" = list(
    file = "Table 9",
    file_type = "multi_sheet",
<<<<<<< HEAD
    classifications = c("4-fold"), 
=======
    classifications = c("2-fold", "4-fold"), 
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
     # "4-fold",
    full_name = "Number of high growth registered private sector businesses",
    aggregate_method = "sum"  # sum instead of average
  ),
  "Economic inactivity" = list(
    file = "Table 17",
    file_type = "multi_sheet",
    classifications = c("3-fold", "4-fold"),  # NEW: removed 2-fold, added 3-fold
    full_name = "Economic inactivity rates for population aged 16 to 64",
    has_sub_metrics = TRUE,  #  add sub-metrics for 3-fold
    sub_metrics_file = "economic_inactivity_3fold.xlsx"  # 3-fold sub-metrics file
  ),
  "Broadband coverage" = list(
    file = "broadband.xlsx",
    file_type = "single_file",
    classifications = c("2-fold"),
    full_name = "Residential broadband coverage by service type/download speeds",
    has_sub_metrics = TRUE,
    sub_metrics = c("Superfast", "Gigabit capable")
  ),
  "4G coverage" = list(
    file = "4g.xlsx",
    file_type = "single_file",
    classifications = c("2-fold"),
    full_name = "Percentage 4G geographic coverage for at least one Mobile Network Operator (MNO)",
    has_sub_metrics = FALSE
  )
)
# Static Key Insights for each economy metric 
economy_key_insights <- list(
<<<<<<< HEAD
  "Regional Gross Value Added" = "Regional Gross Value Added in Scotland is the highest in larger cities (£7,0334m in 2022) and the lowest in islands and remote rural areas (£4,339m in 2022).",
  
  "Number of VAT/PAYE Businesses" = "Number of VAT/PAYE businesses in Scotland is higher in Urban (109,330) than in Rural Areas (68,260) as of 2024. However, in a 4-fold RESAS split, mainly rural areas have the highest number of VAT/PAYE businesses (59,815) closely followed by urban with substantial rural areas (58370) and larger cities (50960).",
  
  "Registered private sector businesses" = "The number of registered private sector businesses in Scotland remains stable across all 6-folds, with large urban areas showing the highest number in 2024 (60,295) and remote small towns the lowest (7,020).",
  
  "High growth private businesses" = "The number of high growth private businesses has increased since 2021 in all of the RESAS regions, following a drop from 2020 to 2021 due to the COVID pandemic. Data for recent years has been impacted by high inflation and a bounce back of business turnover following the COVID-19 pandemic.",
  
  "Economic inactivity 4-fold" = "Since 2023 economic inactivity has generally been lower the more rural the regional classification for a given local authority. 2016 saw the most substantial difference (10.1 percentage points between island and remote rural areas and larger cities). Since then the gap has closed with rates of inactivity growing in regions with rural areas and reducing in larger cities. In 2022 economic inactivity differed by only 2.1 percentage points in 2022, ranging from 21.3% in island and remote rural areas to 23.4% in larger cities.",
=======
  "Regional Gross Value Added" = "Regional Gross Value Added in Scotland is the highest in larger cities (£70334m in 2022) and the lowest in islands and remote rural areas (£4339m in 2022).",
  
  "Number of VAT/PAYE Businesses" = "Number of VAT/PAYE businesses in Scotland is higher in Urban (109330) than in Rural Areas (68260) as of 2024. However, in a 4-fold RESAS split, mainly rural areas have the highest number of VAT/PAYE businesses (59815) closely followed by urban with substantial rural areas (58370) and larger cities (50960).",
  
  "Registered private sector businesses" = "The number of registered private sector businesses in Scotland remains stable across all 6-folds, with large urban areas showing the highest number in 2024 (60295) and remote small towns the lowest (7020).",
  
  "High growth private businesses" = "The number of high growth private businesses has increased since 2021 in all of the RESAS regions, following a drop from 2020 to 2021 due to the COVID pandemic. The values have mostly recovered exceeding pre-pandemic levels.",
  
  "Economic inactivity 4-fold" = "Since 2023 economic inactivity has generally been lower the more rural the regional classification for a given local authority. 2016 saw the most substantial difference (10.1 percentage points between island and remote rural areas and larger cities). Since then the gap has closed with rates of inactivity growing in regions with rural areas and reducing in larger cities. In 2022 economic inactivity differed by only 2.1 percentage points in 2022, ranging from 21.3% in island and remote rural areas to 23.4% in larger cities.",
  
  
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
  "Economic inactivity 3-fold" = "In 2023, accessible rural areas showed the lowest economic inactivity rates (21%) followed by remote rural areas (22%). The rest of Scotland had the highest economic inactivity rates at 23%.",  
  
  
  "Broadband coverage" = "Superfast broadband coverage in 2023 is at 99% in urban areas and only 79% in rural areas, though that number has risen from 72% in 2020. Gigabit capable coverage is 80% in urban areas (from 47% in 2020) and only 34% in rural areas (up from 13% in 2020).",
  
  "4G coverage" = "4G coverage in urban areas is 100% and has been at this level since 2019. In rural areas the coverage is 89% (up from 79% in 2019)."
)

# Static Notes for each economy metric
economy_notes <- list(
  "Regional Gross Value Added" = "Data have been revised back to 1998",
  
  "Number of VAT/PAYE Businesses" = "",
  
  "Registered private sector businesses" = "",
  
  "High growth private businesses" = "",
  
  "Economic inactivity 4-fold" = "4-fold data uses the RESAS classification of urban / rural Local Authorities",
  
<<<<<<< HEAD
  "Economic inactivity 3-fold" = "1.  3-fold data uses the Scottish Government 3-fold Urban Rural classification 2020. 2.	This analysis was bespoke and some data were incomplete so should be treated as a provisional.",
=======
  "Economic inactivity 3-fold" = "1.	3-fold data uses the Scottish Government 3-fold Urban Rural classification 2020. 2.	This analysis was bespoke and some data were incomplete so should be treated as a provisional",
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
  
  "Broadband coverage" = "1. Superfast broadband refers to download speeds of >= 30Mbit/s. 2. Prior to 2019, source data does not distinguish between residential and commerical premises. 3. September figures have been taken as the annual value for each year, in accordance with annual report practice. See below for latest July 2024 update. 4. Locale classification is used to identify premises as being in either an urban or rural area. Locale is a third-party data source based on the analysis of 2011 census output areas (OAs).",
  
  "4G coverage" = "1. September figures have been taken as the annual value for each year, in accordance with annual report practice. 2.  Locale classification is used to identify premises as being in either an urban or rural area. Locale is a third-party data source based on the analysis of 2011 census output areas (OAs)."
)

<<<<<<< HEAD
economic_inactivity_3fold_sub_metrics <- c("Economically inactive" = "Economically inactive",
  "Economically Active" = "Economically Active",
  "In employment, education or training" = "In employment, education or training"
  
=======
economic_inactivity_3fold_sub_metrics <- c(
  "Economically Active" = "Economically Active",
  "In employment, education or training" = "In employment, education or training", 
  "Economically inactive" = "Economically inactive"
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
)

# Rural/Urban classification mapping (4-fold from Excel data)
rural_urban_4fold <- c(
  "Islands & Remote Rural" = "Islands & Remote Rural",
  "Mainly Rural" = "Mainly Rural", 
  "Urban with Substantial Rural areas" = "Urban with Substantial Rural areas",
  "Larger Cities" = "Larger Cities"
)

#  Load Economic inactivity 4-fold data from the main Excel file
load_economic_inactivity_4fold_data <- function(selected_sub_metric = NULL) {
  cat("Loading economic inactivity 4-fold data from main Excel file...\n")
  
  tryCatch({
    # Use the main Excel loading function for Table 17
    raw_data <- process_excel_data_for_dashboard("Table 17")
    
    if(nrow(raw_data) == 0) {
      cat("No data loaded from Table 17\n")
      return(data.frame())
    }
    
    # Process for 4-fold classification
    processed_data <- raw_data %>%
      filter(!is.na(Classification_4fold)) %>%
      mutate(
        Area = Classification_4fold,
        Classification_4fold = Area,
        # Also create 2-fold for consistency
        Classification_2fold = case_when(
          Area %in% c("Islands & Remote Rural", "Mainly Rural") ~ "Rural",
          Area %in% c("Urban with Substantial Rural areas", "Larger Cities") ~ "Urban",
          TRUE ~ Area
        ),
        Data_Source = "Sub-Scotland Economic Statistics - Economic Inactivity 4-fold"
      ) %>%
      select(Year, Area, Classification_4fold, Classification_2fold, Value, Data_Source)
    
    cat("Processed", nrow(processed_data), "economic inactivity 4-fold records\n")
    cat("Areas found:", paste(unique(processed_data$Area), collapse = ", "), "\n")
    cat("Year range:", min(processed_data$Year), "to", max(processed_data$Year), "\n")
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading economic inactivity 4-fold data:", e$message, "\n")
    return(data.frame())
  })
}
load_registered_businesses_data <- function() {
  cat("Loading registered private sector businesses data...\n")
  
  tryCatch({
    file_path <- "economy/registered_businesses_2_6_fold.xlsx"
    if(!file.exists(file_path)) {
      cat("Registered businesses file not found:", file_path, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(file_path, skip = 1) # Skip title row
    
    # Set column names based on the structure
    colnames(raw_data) <- c("Area_Description", 2015:2024)
    
    processed_data <- raw_data %>%
      filter(!is.na(Area_Description)) %>%
      gather(key = "Year", value = "Value", -Area_Description) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),
        # Map area descriptions - INCLUDING the Urban Areas row
        Area_Type = case_when(
          Area_Description == "1 Large Urban Areas" ~ "Large Urban Areas",
          Area_Description == "2 Other Urban Areas" ~ "Other Urban Areas", 
          Area_Description == "3 Accessible Small Towns" ~ "Accessible Small Towns",
          Area_Description == "4 Remote Small Towns" ~ "Remote Small Towns",
          Area_Description == "5 Accessible Rural" ~ "Accessible Rural",
          Area_Description == "6 Remote Rural" ~ "Remote Rural",
          Area_Description == "Rural Areas" ~ "Rural",
          Area_Description == "Scotland" ~ "Scotland",
          Area_Description == "Urban Areas" ~ "Urban",  # ADD THIS LINE
          TRUE ~ Area_Description
        ),
        # Create classifications
        Classification_6fold = case_when(
          Area_Type %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", 
                           "Remote Small Towns", "Accessible Rural", "Remote Rural") ~ Area_Type,
          TRUE ~ NA_character_
        ),
        # Classification_3fold = case_when(
        #   Area_Type %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns") ~ "Urban",
        #   Area_Type == "Accessible Rural" ~ "Accessible Rural",
        #   Area_Type == "Remote Rural" ~ "Remote Rural",
        #   Area_Type == "Urban" ~ "Urban",  # Use aggregated urban row
        #   Area_Type == "Scotland" ~ "Scotland",
        #   TRUE ~ NA_character_
        # ),
        Classification_2fold = case_when(
          Area_Type == "Urban" ~ "Urban",  # Use pre-aggregated urban
          Area_Type == "Rural" ~ "Rural", # Use pre-aggregated rural
          Area_Type == "Scotland" ~ "Scotland",
          # # Also map individual areas for completeness
          # Area_Type %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns") ~ "Urban",
          # Area_Type %in% c("Accessible Rural", "Remote Rural") ~ "Rural",
          TRUE ~ NA_character_
        ),
        Data_Source = "Sub-Scotland Economic Statistics - Registered Private Sector Businesses"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area_Type, Classification_2fold,
             #Classification_3fold,
             Classification_6fold, Value, Data_Source)
    
    cat("Processed", nrow(processed_data), "registered businesses records\n")
    cat("Areas found:", paste(unique(processed_data$Area_Type), collapse = ", "), "\n")
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading registered businesses data:", e$message, "\n")
    return(data.frame())
  })
}
#  Load Economic inactivity 3-fold data from the actual file structure
load_economic_inactivity_3fold_data <- function(selected_sub_metric = NULL) {
  filepath <- "economy/economic_inactivity_3fold.xlsx"
  cat("Loading economic inactivity 3-fold data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file - skip title row, header is row 2
    raw_data <- read_excel(filepath, skip = 1)
    
    cat("Economic inactivity 3-fold raw data dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
    
    # The file structure is:
    # Column 1: Region (Remote Rural, Accessible Rural, Rest of Scotland)  
    # Column 2: Economically Active
    # Column 3: In employment, education or training
    # Column 4: Economically inactive
    
    # Set proper column names
    colnames(raw_data) <- c("Region", "Economically_Active", "In_employment_education_training", "Economically_inactive")
    
    # Process data - convert to long format
    processed_data <- raw_data %>%
      filter(!is.na(Region), Region != "") %>%
      gather(key = "Sub_Metric", value = "Value", -Region) %>%
      mutate(
        Year = 2021,  # Data is from 2021
        Value = as.numeric(Value) * 100, # Convert to percentage (0.78 -> 78%)
        # Map regions to 3-fold classification
        Area = case_when(
          Region == "Remote Rural" ~ "Remote Rural",
          Region == "Accessible Rural" ~ "Accessible Rural",
<<<<<<< HEAD
          Region == "Rest of Scotland" ~ "Rest of Scotland" ,  # Rest of Scotland = Urban
=======
          Region == "Rest of Scotland" ~ "Urban",  # Rest of Scotland = Urban
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
          TRUE ~ Region
        ),
        # Clean up sub-metric names
        Sub_Metric_Clean = case_when(
          Sub_Metric == "Economically_Active" ~ "Economically Active",
          Sub_Metric == "In_employment_education_training" ~ "In employment, education or training",
          Sub_Metric == "Economically_inactive" ~ "Economically inactive",
          TRUE ~ Sub_Metric
        ),
        Data_Source = "Scottish Government - Economic Activity 3-fold"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Sub_Metric = Sub_Metric_Clean, Value, Data_Source)
    
    # Filter by selected sub-metric if specified
    if (!is.null(selected_sub_metric) && selected_sub_metric != "") {
      processed_data <- processed_data %>%
        filter(Sub_Metric == selected_sub_metric)
      cat("Filtered for sub-metric:", selected_sub_metric, "- Records:", nrow(processed_data), "\n")
    }
    
    cat("Processed", nrow(processed_data), "economic inactivity 3-fold records\n")
    cat("Sub-metrics found:", paste(unique(processed_data$Sub_Metric), collapse = ", "), "\n")
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading economic inactivity 3-fold data:", e$message, "\n")
    return(data.frame())
  })
}

# Function to load Excel data from specific table (multi-sheet metrics)
load_excel_table_data <- function(table_name) {
  cat("Loading data from:", table_name, "\n")
  
  tryCatch({
    filepath <- "economy/sub_economic_stats.xlsx"
    if(!file.exists(filepath)) {
      cat("Excel file not found:", filepath, "\n")
      cat("Current working directory:", getwd(), "\n")
      return(data.frame())
    }
    
    sheet_names <- excel_sheets(filepath)
    cat("Available sheets:", paste(sheet_names, collapse = ", "), "\n")
    
    target_sheet <- sheet_names[grepl(paste0("^", table_name, "$"), sheet_names, ignore.case = TRUE)]
    if(length(target_sheet) == 0) {
      target_sheet <- sheet_names[grepl(gsub("_", " ", table_name), sheet_names, ignore.case = TRUE)]
    }
    if(length(target_sheet) == 0) {
      cat("Sheet not found:", table_name, "\n")
      return(data.frame())
    }
    
    cat("Reading sheet:", target_sheet[1], "\n")
    
    # Read without column names
    raw_data <- read_excel(filepath, sheet = target_sheet[1], col_names = FALSE)
    
    cat("Raw data dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    
    # Determine prefix columns and year start column based on table
    if (grepl("Table 17", table_name, ignore.case = TRUE)) {
      prefix_cols <- 3
      year_start_col <- 4
    } else {
      prefix_cols <- 2
      year_start_col <- 3
    }
    
    # Find header row - look for row where columns year_start_col+ contain 4-digit years
<<<<<<< HEAD
    # data_start <- NA
    # for(i in 1:min(50, nrow(raw_data))) {
    #   year_cols <- sapply(year_start_col:ncol(raw_data), function(j) {
    #     val <- as.character(raw_data[[i, j]])
    #     !is.na(val) && grepl("^[0-9]{4}$", val)
    #   })
    #   if (sum(year_cols) >= 10) {  # At least 10 columns with 4-digit years
    #     data_start <- i
    #     break
    #   }
    # }
    # 
    # if (is.na(data_start)) {
    #   cat("Could not find data start row in", table_name, "\n")
    #   print(head(raw_data, 20))
    #   return(data.frame())
    # }
    # 
    # cat("Data starts at row:", data_start, "\n")
    # 
    # # Extract data rows (start after header)
    # data_rows <- raw_data[(data_start + 1):nrow(raw_data), ]
    # 
    # # Assign column names based on table
    # if (!table_name %in% names(start_years)) {
    #   cat("No start year defined for", table_name, "\n")
    #   return(data.frame())
    # }
    
    # Parameters to tweak
   max_scan_rows <- 200L   # scan deeper, if needed
    min_year_cols   <- 5L     # lower threshold if some tables have fewer year columns
   year_regex <- "^[0-9]{4}(\\s*[-/]\\s*[0-9]{2,4})?$"
    
    # Basic validations
    if (!is.data.frame(raw_data)) stop("raw_data must be a data.frame")
    if (!is.numeric(year_start_col) || length(year_start_col) != 1) stop("year_start_col must be a single numeric index")
    if (year_start_col < 1 || year_start_col > ncol(raw_data)) stop("year_start_col is out of bounds")
    
    data_start <- NA_integer_
    
    upper_row <- min(max_scan_rows, nrow(raw_data))
    for (i in seq_len(upper_row)) {
      cols <- year_start_col:ncol(raw_data)
      if (length(cols) == 0) next
      year_cols <- vapply(cols, function(j) {
        val <- raw_data[[i, j]]
        val_chr <- trimws(as.character(val))
        nzchar(val_chr) && !is.na(val_chr) && grepl(year_regex, val_chr)
      }, logical(1))
      if (sum(year_cols, na.rm = TRUE) >= min_year_cols) {
=======
    data_start <- NA
    for(i in 1:min(50, nrow(raw_data))) {
      year_cols <- sapply(year_start_col:ncol(raw_data), function(j) {
        val <- as.character(raw_data[[i, j]])
        !is.na(val) && grepl("^[0-9]{4}$", val)
      })
      if (sum(year_cols) >= 10) {  # At least 10 columns with 4-digit years
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
        data_start <- i
        break
      }
    }
    
    if (is.na(data_start)) {
      cat("Could not find data start row in", table_name, "\n")
<<<<<<< HEAD
      print(utils::head(raw_data, 20))
=======
      print(head(raw_data, 20))
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
      return(data.frame())
    }
    
    cat("Data starts at row:", data_start, "\n")
    
<<<<<<< HEAD
    # Only slice data rows if they exist
    if (data_start >= nrow(raw_data)) {
      cat("No data rows found after header for", table_name, "\n")
      return(data.frame())
    }
    
    data_rows <- raw_data[(data_start + 1L):nrow(raw_data), , drop = FALSE]
    
    # start_years presence check
=======
    # Extract data rows (start after header)
    data_rows <- raw_data[(data_start + 1):nrow(raw_data), ]
    
    # Assign column names based on table
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
    if (!table_name %in% names(start_years)) {
      cat("No start year defined for", table_name, "\n")
      return(data.frame())
    }
    
<<<<<<< HEAD
  
    
=======
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
    num_year_cols <- ncol(data_rows) - prefix_cols
    start_year <- start_years[[table_name]]
    years <- seq(start_year, start_year + num_year_cols - 1)
    
    if (length(years) != num_year_cols) {
      cat("Year count mismatch: expected", num_year_cols, ", generated", length(years), "\n")
      return(data.frame())
    }
    
    if (grepl("Table 17", table_name, ignore.case = TRUE)) {
      colnames(data_rows) <- c("Metric", "Geography_Level", "Area", as.character(years))
    } else {
      colnames(data_rows) <- c("Group", "Area", as.character(years))
    }
    
    cat("Assigned columns:", paste(colnames(data_rows)[1:min(6, ncol(data_rows))], collapse = ", "), "...\n")
    
    # Debug: Print sample raw data
    cat("Sample raw data from", table_name, ":\n")
    print(head(data_rows[, 1:min(6, ncol(data_rows))], 6))
    
    # Filter rows based on table
    if (grepl("Table 17", table_name, ignore.case = TRUE)) {
      # For Table 17: Keep only rows with Metric = "Economic inactivity rate" and Geography_Level = "Rural / Urban Economy Areas"
      rural_urban_data <- data_rows %>%
        filter(Metric == "Economic inactivity rate" & Geography_Level == "Rural / Urban Economy Areas") %>%
        filter(Area %in% c("Islands & Remote Rural", "Mainly Rural", "Urban with Substantial Rural areas", "Larger Cities")) %>%
        mutate(Table = table_name)
    } else {
      # For other tables: Filter for Group = "Rural / Urban Economy Areas"
      rural_urban_data <- data_rows %>%
        filter(Group %in% c("Rural / Urban Economy Areas", "Country")) %>%
        filter(!is.na(Area)) %>%
        mutate(Table = table_name)
    }
    
    cat("Loaded", nrow(rural_urban_data), "rural/urban records from", table_name, "\n")
    cat("Areas found:", paste(unique(rural_urban_data$Area), collapse = ", "), "\n")
    
    # Debug: Print filtered data
    if (nrow(rural_urban_data) > 0) {
      cat("Sample filtered data from", table_name, ":\n")
      print(head(rural_urban_data[, 1:min(6, ncol(rural_urban_data))], 4))
    } else {
      cat("No matching rows found after filtering in", table_name, "\n")
    }
    
    # Clean area names
    rural_urban_data <- rural_urban_data %>%
      mutate(
        Area = case_when(
          grepl("Island.*Remote|Remote.*Island", Area, ignore.case = TRUE) ~ "Islands & Remote Rural",
          grepl("Mainly.*Rural|Rural.*Main", Area, ignore.case = TRUE) ~ "Mainly Rural",
          grepl("Urban.*Rural|Substantial.*Rural", Area, ignore.case = TRUE) ~ "Urban with Substantial Rural areas",
          grepl("Larger.*Cit|Major.*Urban|Large.*Urban", Area, ignore.case = TRUE) ~ "Larger Cities",
          grepl("Scotland", Area, ignore.case = TRUE) ~ "Scotland",
          TRUE ~ Area
        )
      ) %>%
      rename(Region = Area)
    
    return(rural_urban_data)
    
  }, error = function(e) {
    cat("Error loading data from", table_name, ":", e$message, "\n")
    return(data.frame())
  })
}
# Load broadband data from single Excel file
load_broadband_data <- function() {
  cat("Loading broadband coverage data...\n")
  
  tryCatch({
    file_path <- "economy/broadband.xlsx"
    if(!file.exists(file_path)) {
      cat("Broadband file not found:", file_path, "\n")
      return(data.frame())
    }
    
    # Read the Excel file - skip first row (title), second row has headers
    raw_data <- read_excel(file_path, skip = 1)
    
    cat("Broadband raw data dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
    
    # Clean column names - remove trailing spaces
    names(raw_data) <- str_trim(names(raw_data))
    
    # Expected structure: Region, Download Speed, 2020, 2021, 2022, 2023
    processed_data <- raw_data %>%
      filter(!is.na(Region), Region != "", !is.na(`Download Speed`)) %>%
      # Remove any trailing spaces from text columns
      mutate(
        Region = str_trim(Region),
        `Download Speed` = str_trim(`Download Speed`)
      ) %>%
      # Convert to long format for years
      gather(key = "Year", value = "Value", `2020`, `2021`, `2022`, `2023`) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100, # Convert decimal to percentage (0.98 -> 98%)
        # Map regions to classifications
        Classification_2fold = case_when(
          Region == "Urban" ~ "Urban",
          Region == "Rural" ~ "Rural",
          Region == "Total" ~ "Scotland"
        ),
        Area_Type = Region,
        Sub_Metric = `Download Speed`,
        Data_Source = "Digital Scotland",
        Table_Name = "broadband.xlsx"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area_Type, Classification_2fold, Value, Sub_Metric, Data_Source, Table_Name)
    
    cat("Processed", nrow(processed_data), "broadband records\n")
    cat("Sub-metrics found:", paste(unique(processed_data$Sub_Metric), collapse = ", "), "\n")
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading broadband data:", e$message, "\n")
    return(data.frame())
  })
}

# Load 4G coverage data from single Excel file  
load_4g_data <- function() {
  cat("Loading 4G coverage data...\n")
  
  tryCatch({
    file_path <- "economy/4g.xlsx"
    if(!file.exists(file_path)) {
      cat("4G file not found:", file_path, "\n")
      return(data.frame())
    }
    
    # Read the Excel file - skip first row (title), second row has headers
    raw_data <- read_excel(file_path, skip = 1)
    
    cat("4G raw data dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
    
    # Clean column names - remove trailing spaces
    names(raw_data) <- str_trim(names(raw_data))
    
    # Get year columns - handle the typo where 2032 should be 2023
    year_cols <- names(raw_data)[names(raw_data) != "Region"]
    cat("Year columns found:", paste(year_cols, collapse = ", "), "\n")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), Region != "") %>%
      mutate(Region = str_trim(Region)) %>%
      # Convert to long format for years
      gather(key = "Year_Raw", value = "Value", all_of(year_cols)) %>%
      mutate(
        # Fix the 2032 typo to 2023
        Year = case_when(
          Year_Raw == "2032" ~ 2023,
          TRUE ~ as.numeric(Year_Raw)
        ),
        Value = as.numeric(Value) * 100, # Convert decimal to percentage (0.79 -> 79%)
        # Map regions to classifications
        Classification_2fold = case_when(
          Region == "Urban" ~ "Urban",
          Region == "Rural" ~ "Rural", 
          Region == "Total" ~ "Scotland"
        ),
        Area_Type = Region,
        Data_Source = "Digital Scotland",
        Table_Name = "4g.xlsx"
      ) %>%
      filter(!is.na(Value), !is.na(Year), Year >= 2019, Year <= 2024) %>%
      select(Year, Area_Type, Classification_2fold, Value, Data_Source, Table_Name)
    
    cat("Processed", nrow(processed_data), "4G coverage records\n")
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading 4G data:", e$message, "\n")
    return(data.frame())
  })
}
# Main data loading function with proper routing
load_economy_data_corrected <- function(metric_name, classification_type, selected_sub_metric = NULL) {
  metric_info <- economy_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  if (metric_name == "Registered private sector businesses") {
    return(load_registered_businesses_data())
  }
  
  # Handle Economic inactivity specially based on classification
  if (metric_name == "Economic inactivity") {
    if (classification_type == "3-fold") {
      return(load_economic_inactivity_3fold_data(selected_sub_metric))
    } else if (classification_type == "4-fold") {
      return(load_economic_inactivity_4fold_data(selected_sub_metric))
    }
  }
  
  # Handle broadband coverage
  if (metric_name == "Broadband coverage") {
    raw_data <- load_broadband_data()
    if(!is.null(selected_sub_metric) && selected_sub_metric != "") {
      raw_data <- raw_data %>% filter(Sub_Metric == selected_sub_metric)
    }
    return(raw_data)
  }
  
  # Handle 4G coverage
  if (metric_name == "4G coverage") {
    return(load_4g_data())
  }
  
  # Handle multi-sheet Excel data (Table 1, Table 2, Table 9, Table 17)
  table_name <- metric_info$file
  return(process_excel_data_for_dashboard(table_name, selected_sub_metric))
}

#  economy_metrics list to support 3 and 4-fold Economic inactivity from separate files
economy_metrics[["Economic inactivity"]]$classifications <- c("3-fold", "4-fold")

# Process Excel data for dashboard with proper sum/average handling
process_excel_data_for_dashboard <- function(table_names, selected_sub_metric = NULL) {
  if(length(table_names) == 0) {
    return(data.frame())
  }
  
  
  # Handle multi-sheet Excel file
  raw_data <- load_excel_table_data(table_names)
  
  if(nrow(raw_data) == 0) {
    cat("No data loaded from", table_names, "\n")
    return(data.frame())
  }
  
  # Get year columns 
  all_cols <- names(raw_data)
  year_cols <- all_cols[grepl("^[0-9]{4}$", all_cols)]
  
  if(length(year_cols) == 0) {
    cat("No year columns found in data. Available columns:", paste(all_cols, collapse = ", "), "\n")
    return(data.frame())
  }
  
  cat("Found year columns:", paste(year_cols, collapse = ", "), "\n")
  
  # Transform to long format
  long_data <- raw_data %>%
    select(all_of(c("Region", year_cols, "Table"))) %>%
    gather(key = "Year_Period", value = "Value", -Region, -Table) %>%
    mutate(
      Year = as.numeric(Year_Period),  
      
      Value = case_when(
        is.na(Value) ~ NA_real_,
        grepl("^\\[x\\]$|^\\[c\\]$|^\\[u\\]$|^\\[x1\\]$|^\\[c1\\]$|^\\[x\\]\\s*\\[see note .*\\]$", trimws(Value), ignore.case = TRUE) ~ NA_real_,
        Value %in% c("-", "*", "", "NA", "#N/A") ~ NA_real_,
        TRUE ~ as.numeric(as.character(Value))
      )
    ) %>%
    filter(!is.na(Value), !is.na(Year)) %>%
    filter(Year >= 2000, Year <= 2030) %>%  # Reasonable year range
    mutate(
      Area_Type = Region,
      Classification_4fold = Region,
      Classification_2fold = case_when(
        Region %in% c("Islands & Remote Rural", "Mainly Rural") ~ "Rural",
        Region %in% c("Urban with Substantial Rural areas", "Larger Cities") ~ "Urban",
        TRUE ~ Region
      ),
      Data_Source = case_when(
        table_names == "Table 1" ~ "Sub-Scotland Economic Statistics - Regional GVA",
        table_names == "Table 2" ~ "Sub-Scotland Economic Statistics - VAT/PAYE Businesses", 
        table_names == "Table 9" ~ "Sub-Scotland Economic Statistics - High Growth Businesses",
        table_names == "Table 17" ~ "Sub-Scotland Economic Statistics - Economic Inactivity",
        TRUE ~ "Sub-Scotland Economic Statistics"
      ),
      Sub_Metric = selected_sub_metric,
      Table_Name = table_names
    )
  
  # Apply percentage conversion for Economic inactivity (Table 17)
  if(table_names == "Table 17") {
    cat("Converting decimal percentages to whole percentages for Table 17\n")
    long_data <- long_data %>%
      mutate(Value = Value * 100)
    cat("After conversion - Value range:", min(long_data$Value, na.rm = TRUE), "to", max(long_data$Value, na.rm = TRUE), "\n")
  }
  
  cat("Processed", nrow(long_data), "records from", table_names, "\n")
  cat("Sample data:\n")
  if(nrow(long_data) > 0) {
    print(head(long_data %>% select(Year, Region, Value, Table_Name), 3))
  }
  
  return(long_data)
}

#  Aggregate function to handle all classification types properly
aggregate_economy_by_classification <- function(processed_data, classification_type = "2-fold", metric_name = NULL) {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  #  For registered businesses, filter to individual 6-fold rows only (excludes aggs to prevent double-counting)
<<<<<<< HEAD
  # if(!is.null(metric_name) && metric_name == "Registered private sector businesses") {
  #   processed_data <- processed_data %>% filter(!is.na(Classification_6fold))
  # }
=======
  if(!is.null(metric_name) && metric_name == "Registered private sector businesses") {
    processed_data <- processed_data %>% filter(!is.na(Classification_6fold))
  }
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
  
  # Determine classification column (fix for registered vs. inactivity)
  classification_col <- case_when(
    classification_type == "3-fold" && !is.null(metric_name) && metric_name == "Economic inactivity" ~ "Area",
    classification_type == "4-fold" && !is.null(metric_name) && metric_name == "Economic inactivity" ~ "Area",
    classification_type == "2-fold" ~ "Classification_2fold",
    classification_type == "3-fold" ~ "Classification_3fold",
    classification_type == "6-fold" ~ "Classification_6fold",
    classification_type == "4-fold" ~ "Classification_4fold",
    TRUE ~ "Classification_2fold"
  )
  
  # Check if classification column exists
  if(!classification_col %in% names(processed_data)) {
    cat("Classification column", classification_col, "not found in data\n")
    cat("Available columns:", paste(names(processed_data), collapse = ", "), "\n")
    return(data.frame())
  }
    
  # Special handling for 3-fold/4-fold economic inactivity (unchanged)
  if(classification_type == "3-fold" && !is.null(metric_name) && metric_name == "Economic inactivity") {
    return(processed_data %>%
             mutate(Classification = classification_type) %>%
             select(Year, Area, Value, Data_Source, Sub_Metric))
  }
  
  if(classification_type == "4-fold" && !is.null(metric_name) && metric_name == "Economic inactivity") {
    return(processed_data %>%
             mutate(Classification = classification_type) %>%
             select(Year, Area, Value, Data_Source))
  }
  
  
  # Determine aggregation method
  use_sum_method <- !is.null(metric_name) && 
    metric_name %in% c("Regional Gross Value Added", "Number of VAT/PAYE Businesses", "Registered private sector businesses", "High growth private businesses")
  
  if(use_sum_method) {
    cat("Using sum aggregation for", metric_name, "\n")
    
    # Sum aggregation (exclude any existing Scotland totals)
    regional_data <- processed_data %>%
      filter(!grepl("Scotland|Total", Area_Type, ignore.case = TRUE)) %>%
      group_by(Year, !!sym(classification_col), Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      rename(Area = !!sym(classification_col)) %>%
      mutate(Classification = classification_type) %>%
      filter(!is.na(Area))
    
    # Add Scotland totals for sum metrics
    scotland_data <- processed_data %>%
      filter(grepl("Scotland|Total", Area_Type, ignore.case = TRUE)) %>%
      # group_by(Year, Data_Source) %>%
      # summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        Area = "Scotland",
        Classification = classification_type
      )
    
    aggregated <- bind_rows(regional_data, scotland_data)
    
  } else {
    cat("Using average aggregation for", metric_name, "\n")
    
    # Average aggregation
    aggregated <- processed_data %>%
      group_by(Year, !!sym(classification_col), Data_Source) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      rename(Area = !!sym(classification_col)) %>%
      mutate(Classification = classification_type) %>%
      filter(!is.na(Area))
  }
  
  return(aggregated)
}
# Function to get Scotland summary 
get_economy_scotland_summary <- function(processed_data, metric_name) {
  if(nrow(processed_data) == 0) {
    return(list(value = NA, year = NA, label = "No Data"))
  }
  
  # Skip for Economic inactivity
  if(metric_name == "Economic inactivity") {
    return(list(value = NA, year = NA, label = "No Data"))
  }
  
  latest_year <- max(processed_data$Year)
  
  # Check if this metric should use sum aggregation
  use_sum_method <- metric_name %in% c("Regional Gross Value Added", "Number of VAT/PAYE Businesses", "Registered private sector businesses", "High growth private businesses")
  
  # if(use_sum_method) {
    # Sum for specified metrics
    scotland_value <- processed_data %>%
      filter(Year == latest_year) %>%
      filter(grepl("Scotland|Total", Area_Type, ignore.case = TRUE)) %>%
      #summarise(total = sum(Value, na.rm = TRUE)) %>%
      pull(Value)
    
    label <- "Scotland Total"
  # } else {
  #   # Average for other metrics - UNCHANGED FROM ORIGINAL
  #   scotland_value <- processed_data %>%
  #     filter(Year == latest_year) %>%
  #     summarise(avg = mean(Value, na.rm = TRUE)) %>%
  #     pull(avg)
  #   
  #   label <- "Scotland Average"
  # }
  
  return(list(
    value = scotland_value,
    year = latest_year,
    label = label
  ))
}

process_excel_data_for_dashboard <- function(table_names, selected_sub_metric = NULL) {
  if(length(table_names) == 0) {
    return(data.frame())
  }
  
  # Determine the file type based on the table_names
  if(table_names == "broadband.xlsx") {
    # Load broadband data
    raw_data <- load_broadband_data()
    
    # Filter by sub-metric if specified
    if(!is.null(selected_sub_metric) && selected_sub_metric != "") {
      raw_data <- raw_data %>% filter(Sub_Metric == selected_sub_metric)
      cat("Filtered broadband data for sub-metric:", selected_sub_metric, "- Records:", nrow(raw_data), "\n")
    }
    
    return(raw_data)
    
  } else if(table_names == "4g.xlsx") {
    # Load 4G data
    return(load_4g_data())
    
  } else {
    # Handle original multi-sheet Excel file 
    raw_data <- load_excel_table_data(table_names)
    
    if(nrow(raw_data) == 0) {
      return(data.frame())
    }
    
    # Get year columns - handle both single years (2015) and year ranges (2015-2018)
    all_cols <- names(raw_data)
    year_cols <- all_cols[grepl("^[0-9]{4}$|^[0-9]{4}-[0-9]{4}$", all_cols)]
    
    if(length(year_cols) == 0) {
      cat("No year columns found in data. Available columns:", paste(all_cols, collapse = ", "), "\n")
      return(data.frame())
    }
    
    cat("Found year columns:", paste(year_cols, collapse = ", "), "\n")
    
    # Transform to long format
    long_data <- raw_data %>%
      select(all_of(c("Region", year_cols, "Table"))) %>%
      gather(key = "Year_Period", value = "Value", -Region, -Table) %>%
      mutate(
        # For year ranges, use the end year; for single years, use as-is
        Year = case_when(
          grepl("^[0-9]{4}-[0-9]{4}$", Year_Period) ~ as.numeric(str_extract(Year_Period, "[0-9]{4}$")),
          grepl("^[0-9]{4}$", Year_Period) ~ as.numeric(Year_Period),
          TRUE ~ NA_real_
        ),
        Value = as.numeric(Value),
        Area_Type = Region,
        Classification_4fold = Region,
        Classification_2fold = case_when(
          Region %in% c("Islands & Remote Rural", "Mainly Rural") ~ "Rural",
          Region %in% c("Urban with Substantial Rural areas", "Larger Cities") ~ "Urban",
          TRUE ~ Region
        ),
        Data_Source = "Sub-Scotland Economic Statistics",
        Sub_Metric = selected_sub_metric,
        Table_Name = table_names
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      filter(Year >= 2010) # Focus on recent years
    
    # Apply percentage conversion for Economic inactivity (Table 17)
    # The Excel stores percentages as decimals (0.216 = 21.6%)
    if(table_names == "Table 17") {
      cat("Converting decimal percentages to whole percentages for Table 17\n")
      long_data <- long_data %>%
        mutate(Value = Value * 100)
      cat("After conversion - Value range:", min(long_data$Value, na.rm = TRUE), "to", max(long_data$Value, na.rm = TRUE), "\n")
    }
    
    cat("Processed", nrow(long_data), "records\n")
    return(long_data)
  }
}
#  function to get 2-fold urban/rural values for value boxes
get_economy_urban_rural_2fold <- function(processed_data, metric_name = NULL) {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  # Skip for ALL Economic inactivity classifications
  if(!is.null(metric_name) && metric_name == "Economic inactivity") {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  agg_2fold <- aggregate_economy_by_classification(processed_data, "2-fold", metric_name)
  
  if(nrow(agg_2fold) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  latest_year <- max(agg_2fold$Year)
  latest_data <- agg_2fold %>% filter(Year == latest_year)
  
  urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value)
  rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value)
  scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value)
  
  return(list(
    urban = ifelse(length(urban_val) > 0, urban_val[1], NA),
    rural = ifelse(length(rural_val) > 0, rural_val[1], NA),
    scotland = ifelse(length(scotland_val) > 0, scotland_val[1], NA),
    year = latest_year
  ))
}

# Function to check if metric uses percentage formatting
is_percentage_metric <- function(metric_name) {
  metric_info <- economy_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(grepl("share|rate|percentage|inactivity|coverage|broadband|4g", tolower(metric_info$full_name)))
  }
  return(grepl("share|rate|percentage|inactivity|coverage|broadband|4g", tolower(metric_name)))
}

# Function to get display name for metrics
get_economy_metric_display_name <- function(metric_name) {
  metric_info <- economy_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# Function to get sub-metrics for a given metric
get_economy_sub_metrics <- function(metric_name) {
  metric_info <- economy_metrics[[metric_name]]
  if(!is.null(metric_info) && !is.null(metric_info$sub_metrics)) {
    return(metric_info$sub_metrics)
  }
  # NEW: Handle 3-fold economic inactivity sub-metrics
  if(metric_name == "Economic inactivity" && !is.null(metric_info$has_sub_metrics) && metric_info$has_sub_metrics) {
    return(names(economic_inactivity_3fold_sub_metrics))
  }
  return(NULL)
}

# Function to check if metric has sub-metrics
has_economy_sub_metrics <- function(metric_name) {
  metric_info <- economy_metrics[[metric_name]]
  return(!is.null(metric_info) && !is.null(metric_info$has_sub_metrics) && metric_info$has_sub_metrics)
}

# Create color mapping with Scotland as gray (similar to culture module)
get_economy_colors <- function(areas, classification_type) {
  color_mapping <- list()
  if (classification_type == "6-fold") {
    color_mapping <- list(
      "Large Urban Areas" = "#FDBE41",        # Dark blue
      "Other Urban Areas" = "#F4E470",         # Light blue
      "Accessible Small Towns" = "#80BA27",    # Orange
      "Remote Small Towns" = "#23A845",        # Light orange
      "Accessible Rural" = "#00833E",          # Green
      "Remote Rural" = "#0E450B"               # Light green
    )
    # Get base colors from config.R if available  
  } else if (classification_type == "4-fold") {
    
      color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural areas" = "#F4E470", 
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
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display
format_economy_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (is_percentage_metric(metric_name)) {
    return(paste0(round(value, 1), "%"))
  } else {
    return(scales::comma(round(value, 0)))
  }
}

# Calculate gap between urban and rural
calculate_economy_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (is_percentage_metric(metric_name)) {
    return(paste0(round(gap, 1), "pp"))
  } else {
    return(scales::comma(round(gap, 0)))
  }
}
#ui
economy_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      /* STANDARDIZED CSS FOR ALL DASHBOARD MODULES - COPIED FROM CULTURE */
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
      .economy-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .economy-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .economy-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .economy-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .economy-metric-description {
        color: #6c757d !important;
        font-size: 0.95em !important;
        line-height: 1.4 !important;
        margin-left: 30px !important;
      }
      
      /* CHART AND TABLE TITLE MARGINS FOR DOWNLOAD BUTTONS */
      .box-title-with-download {
        margin-right: 100px !important;
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
            h2("Economy and Digital", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("economy_classification_selector")
          ),
          
          # Top-center-right: Economic metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("economy_metric", "Economic Metric:", 
                        choices = c("Select a policy metric..." = "", names(economy_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (when applicable)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.economy_metric == 'Broadband coverage'",
              selectInput("selected_broadband_speed", "Sub-metric:", 
                          choices = c("Superfast", "Gigabit capable"), selected = "Superfast", width = "180px")
            ),
            conditionalPanel(
              condition = "input.economy_metric == 'Economic inactivity' && input.economy_classification_type == '3-fold'",
              selectInput("selected_inactivity_reason", "Sub-metric:", 
                          choices = names(economic_inactivity_3fold_sub_metrics), selected = names(economic_inactivity_3fold_sub_metrics)[1], width = "180px")
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
      condition = "input.economy_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore economic and digital data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Economy-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Regional Gross Value Added
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', 'Regional Gross Value Added', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("chart-line", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "Regional Gross Value Added"),
                      div(class = "economy-metric-description", 
                          "Economic output measured in millions of pounds, showing the contribution to Scotland's economy by different regions. GVA is calculated by subtracting intermediate consumption from gross output.")
                    )
                  )
                ),
                
                # Number of VAT/PAYE Businesses
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', 'Number of VAT/PAYE Businesses', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("building", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "Number of VAT/PAYE Businesses"),
                      div(class = "economy-metric-description",
                          "Count of businesses registered for VAT and/or PAYE schemes, indicating entrepreneurial activity and economic dynamism across urban and rural Scotland.")
                    )
                  )
                ),
                # Registered private sector businesses
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', 'Registered private sector businesses', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("industry", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "Registered private sector businesses"),
                      div(class = "economy-metric-description",
                          "Total count of private sector businesses registered with Companies House, showing entrepreneurial activity and business formation across Scotland's regions.")
                    )
                  )
                ),
                # High growth private businesses
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', 'High growth private businesses', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("rocket", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "High growth private businesses"),
                      div(class = "economy-metric-description",
                          "Enterprises with average annualised growth in employees or turnover greater than 20% per annum over three years, with 10+ employees at start of period.")
                    )
                  )
                ),
                
                # Economic inactivity
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', 'Economic inactivity', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("users", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "Economic inactivity"),
                      div(class = "economy-metric-description",
                          "Proportion of working-age population not participating in the labour market, including students, carers, retired people, and those unable to work.")
                    )
                  )
                ),
                
                # Broadband coverage
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', 'Broadband coverage', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("wifi", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "Broadband coverage"),
                      div(class = "economy-metric-description",
                          "Percentage of premises that can access broadband services by speed category. Essential for modern economic participation and digital inclusion.")
                    )
                  )
                ),
                
                # 4G coverage
                div(
                  class = "economy-metrics-card",
                  onclick = "Shiny.setInputValue('economy_metric_select', '4G coverage', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("signal", class = "economy-metric-icon"),
                    div(
                      div(class = "economy-metric-title", "4G coverage"),
                      div(class = "economy-metric-description",
                          "Percentage of landmass covered by 4G services from at least one mobile network operator, enabling digital connectivity and economic participation.")
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
      condition = "input.economy_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("economy_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("economy_data_summary"))
          ),
          
          # Conditional key insights - hide for ALL Economic inactivity
          conditionalPanel(
            condition = "input.economy_metric != 'Economic inactivity'",
            fluidRow(
              box(title = uiOutput("economy_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("economy_urban_rate", width = 3),
                    valueBoxOutput("economy_rural_rate", width = 3),
                    valueBoxOutput("economy_scotland_rate", width = 3),
                    valueBoxOutput("economy_urban_rural_gap", width = 3)
                  ))
            )
          )
      ),
      
      # Conditional trend chart - hide for Economic inactivity 3-fold
      conditionalPanel(
        condition = "!(input.economy_metric == 'Economic inactivity' && input.economy_classification_type == '3-fold')",
        fluidRow(
          box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                          uiOutput("economy_trend_title"),
                          div(style = "position: absolute; right: 20px;",
                              tags$a(class = "excel-download-btn", href = "#", 
                                     onclick = "Shiny.setInputValue('economy_trend_download', Math.random());",
                                     icon("download"), "Download"))), 
              status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("economy_trend_chart") %>% withSpinner())
        )
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                        uiOutput("economy_comparison_title"),
                        div(style = "position: absolute; right: 20px;",
                            tags$a(class = "excel-download-btn", href = "#", 
                                   onclick = "Shiny.setInputValue('economy_comparison_download', Math.random());",
                                   icon("download"), "Download"))), 
            status = "primary", solidHeader = TRUE, width = 12,
            # UPDATED: Conditional year selector - hide for 3-fold Economic inactivity only
            conditionalPanel(
              condition = "!(input.economy_metric == 'Economic inactivity' && input.economy_classification_type == '3-fold')",
              div(style = "margin-bottom: 15px;", uiOutput("economy_year_selector"))
            ),
            plotlyOutput("economy_comparison_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                        uiOutput("economy_table_title"),
                        div(style = "position: absolute; right: 20px;",
                            tags$a(class = "excel-download-btn", href = "#", 
                                   onclick = "Shiny.setInputValue('economy_table_download', Math.random());",
                                   icon("download"), "Download"))), 
            status = "info", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("economy_table_year_filter")),
                  column(6, uiOutput("economy_table_area_filter"))
                )),
            DT::dataTableOutput("economy_data_table") %>% withSpinner())
      )
    )
  )
}

# Function to get Scotland summary - handle sum vs average labeling
get_economy_scotland_summary <- function(processed_data, metric_name) {
  if(nrow(processed_data) == 0) {
    return(list(value = NA, year = NA, label = "No Data"))
  }
  
  # NEW: Skip for Economic inactivity
  if(metric_name == "Economic inactivity") {
    return(list(value = NA, year = NA, label = "No Data"))
  }
  
  latest_year <- max(processed_data$Year)
  
  # NEW: Check if this metric should use sum aggregation
  use_sum <- FALSE
  metric_info <- economy_metrics[[metric_name]]
  if(!is.null(metric_info) && !is.null(metric_info$aggregate_method) && metric_info$aggregate_method == "sum") {
    use_sum <- TRUE
  }
  
  # if(use_sum) {
    # Sum for GVA, VAT/PAYE, High Growth businesses
    scotland_value <- processed_data %>%
      filter(Year == latest_year) %>%
      filter(grepl("Scotland|Total", Area_Type, ignore.case = TRUE)) %>%
      #summarise(total = sum(Value, na.rm = TRUE)) %>%
     # pull(total)
      pull(Value)
    
    label <- "Scotland Total"
  # } else {
  #   # Average for other metrics
  #   scotland_value <- processed_data %>%
  #     filter(Year == latest_year) %>%
  #     summarise(avg = mean(Value, na.rm = TRUE)) %>%
  #     pull(avg)
  #   
  #   label <- "Scotland Average"
  # }
  
  return(list(
    value = scotland_value,
    year = latest_year,
    label = label
  ))
}
# Server Functions - Updated to match culture module exactly

economy_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  economy_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards
  observeEvent(input$economy_metric_select, {
    updateSelectInput(session, "economy_metric", selected = input$economy_metric_select)
  })
  
  # Dynamic UI outputs
  output$economy_summary_title <- renderUI({
    req(input$economy_metric, input$economy_classification_type)
    display_name <- get_economy_metric_display_name(input$economy_metric)
    classification_text <- case_when(
      input$economy_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$economy_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$economy_classification_type == "4-fold" ~ "(4-fold RESAS)",
      TRUE ~ paste0("(", input$economy_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$economy_year_selector <- renderUI({
    req(economy_values$processed_data)
    if (nrow(economy_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(economy_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("economy_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$economy_classification_selector <- renderUI({
    req(input$economy_metric)
    available_classifications <- economy_metrics[[input$economy_metric]]$classifications
    choices <- list()
    
    # Add in order of detail level (most to least)
    if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
    if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
    if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"
    if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("economy_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles
  output$economy_trend_title <- renderUI({
    req(input$economy_metric)
    display_name <- get_economy_metric_display_name(input$economy_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$economy_comparison_title <- renderUI({
    req(input$economy_metric)
    display_name <- get_economy_metric_display_name(input$economy_metric)
    # NEW: For 3-fold economic inactivity, show fixed year since it's single year data
    if(input$economy_metric == "Economic inactivity" && !is.null(input$economy_classification_type) && input$economy_classification_type == "3-fold") {
      paste0("Single Year Comparison (2023) for ", display_name)
    } else {
      req(input$economy_selected_year)
      paste0("Single Year Comparison (", input$economy_selected_year, ") for ", display_name)
    }
  })
  
  output$economy_table_title <- renderUI({
    req(input$economy_metric)
    display_name <- get_economy_metric_display_name(input$economy_metric)
    paste("Data Table for", display_name)
  })
  
  output$economy_key_insights_title <- renderUI({
    req(input$economy_metric)
    display_name <- get_economy_metric_display_name(input$economy_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$economy_table_year_filter <- renderUI({
    req(economy_values$processed_data)
    if(nrow(economy_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(economy_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("economy_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$economy_table_area_filter <- renderUI({
    req(economy_values$processed_data, input$economy_classification_type)
    if(nrow(economy_values$processed_data) == 0) return(NULL)
    agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("economy_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Data processing observe block - replace in the server function
  observe({
    if (is.null(input$economy_metric) || input$economy_metric == "") {
      economy_values$processed_data <- data.frame()
      economy_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$economy_classification_type)
    
    cat("Processing metric:", input$economy_metric, "with classification:", input$economy_classification_type, "\n")
    
    # Handle Economic inactivity specially based on classification
    if (input$economy_metric == "Economic inactivity") {
      if (input$economy_classification_type == "3-fold") {
        req(input$selected_inactivity_reason)
        selected_reason <- economic_inactivity_3fold_sub_metrics[input$selected_inactivity_reason]
        economy_values$processed_data <- load_economic_inactivity_3fold_data(selected_reason)
      } else if (input$economy_classification_type == "4-fold") {
        # NEW: Load 4-fold economic inactivity data
        economy_values$processed_data <- load_economic_inactivity_4fold_data()
      }
    } else if (input$economy_metric == "Broadband coverage") {
      req(input$selected_broadband_speed)
      economy_values$processed_data <- load_economy_data_corrected(input$economy_metric, input$economy_classification_type, input$selected_broadband_speed)
    } else {
      # Use the updated loading function for all other metrics
      economy_values$processed_data <- load_economy_data_corrected(input$economy_metric, input$economy_classification_type)
    }
    
    economy_values$data_status <- if(nrow(economy_values$processed_data) > 0) "Economic data loaded" else "No data available"
    
    # Debug output
    cat("Metric:", input$economy_metric, "Classification:", input$economy_classification_type, "\n")
    cat("Loaded", nrow(economy_values$processed_data), "records\n")
    if(nrow(economy_values$processed_data) > 0) {
      cat("Sample columns:", paste(names(economy_values$processed_data), collapse = ", "), "\n")
      cat("Value range:", min(economy_values$processed_data$Value, na.rm = TRUE), "to", max(economy_values$processed_data$Value, na.rm = TRUE), "\n")
    }
  })  
  # Data summary
  output$economy_data_summary <- renderUI({
    req(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
    
    if (nrow(economy_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_economy_metric_display_name(input$economy_metric)
<<<<<<< HEAD
    custom_insight <-  if (input$economy_metric == "Economic inactivity") {
      economy_key_insights[[paste(input$economy_metric, input$economy_classification_type)]]
    } else {economy_key_insights[[input$economy_metric]]}
    
    custom_notes <- if (input$economy_metric == "Economic inactivity") {
      economy_notes[[paste(input$economy_metric, input$economy_classification_type)]]} 
    else {economy_notes[[input$economy_metric]]}
=======
    custom_insight <- economy_key_insights[[input$economy_metric]]
    custom_notes <- economy_notes[[input$economy_metric]]
    
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
    # Define source information
    # source_info <- list(
    #   text = "Sub-Scotland Economic Statistics / Digital Scotland",
    #   url = "https://www.gov.scot/collections/sub-scotland-economic-statistics/"
    # )
    
    metric_key <- if (input$economy_metric == "Economic inactivity") {
      paste(input$economy_metric, input$economy_classification_type)
    } else {
      input$economy_metric
    }
    
    source_info <- switch(metric_key,
                          "Regional Gross Value Added" = list(
                            text = "Sub-Scotland Economic Statistics Database (2024), originally from Gross value added (balanced), Office for National Statistics",
                            url = "https://www.gov.scot/publications/sub-scotland-economic-statistics-database/"),
                          "Number of VAT/PAYE Businesses" = list(
                            text = "Businesses in Scotland 2024 - Scottish Government - analysis of the Office for National Statistics (ONS) Inter-Departmental Business",
                            url = "https://www.gov.scot/publications/sub-scotland-economic-statistics-database/"),
                          "Registered private sector businesses" = list(
                            text = "Scottish Government analysis of the Office for National Statistics (ONS) Inter-Departmental Business Register (IDBR)",
                            url = "https://www.gov.scot/publications/businesses-in-scotland-2024/documents/"),
                          "High growth private businesses" = list(
                            text = "Businesses in Scotland 2023",
                            url = "https://www.gov.scot/publications/businesses-in-scotland-2023/"),
                          "Economic inactivity 4-fold" = list(
                            text = "Produced for Sub-Scotland Economic Statistics Database from Annual Population Survey (Jan to Dec)",
                            url = "https://www.gov.scot/publications/sub-scotland-economic-statistics-database/"),
                          "Economic inactivity 3-fold" = list(
                            text = "Bespoke analysis from Annual Population Survey, January to December 2022, Office for National Statistics",
                            url = NULL),
                          "Broadband coverage" = list(
                            text = "Ofcom Connected Nations 2024: Interactive Report",
                            url = "https://www.ofcom.org.uk/phones-and-broadband/coverage-and-speeds/connected-nations-2024/interactive-report-2024"),
                          "4G coverage" = list(
                            text = "Ofcom Connected Nations Report",
                            url = "https://www.ofcom.org.uk/phones-and-broadband/coverage-and-speeds/infrastructure-research")
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
            source_text <- source_info$text,
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
    
    return(div(h4("Economic Data Loaded"), p(paste("Showing data for:", display_name))))
  })
  
  # Value boxes -always show 2-fold urban/rural and include Scotland
  output$economy_urban_rate <- renderValueBox({
    req(economy_values$processed_data, input$economy_metric)
    
    # Skip for Economic inactivity
    if(input$economy_metric == "Economic inactivity") {
      return(valueBox(value = "N/A", subtitle = "Urban Areas", icon = icon("city"), color = "yellow"))
    }
    
    key_insights <- get_economy_urban_rural_2fold(economy_values$processed_data, input$economy_metric)
    val <- format_economy_value(key_insights$urban, input$economy_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    valueBox(value = val, subtitle = paste("Urban Areas", year), icon = icon("city"), color = "yellow")
  })
  
  output$economy_rural_rate <- renderValueBox({
    req(economy_values$processed_data, input$economy_metric)
    
    # Skip for Economic inactivity
    if(input$economy_metric == "Economic inactivity") {
      return(valueBox(value = "N/A", subtitle = "Rural Areas", icon = icon("tree"), color = "olive"))
    }
    
    key_insights <- get_economy_urban_rural_2fold(economy_values$processed_data, input$economy_metric)
    val <- format_economy_value(key_insights$rural, input$economy_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    valueBox(value = val, subtitle = paste("Rural Areas", year), icon = icon("tree"), color = "olive")
  })
  
  # NEW: Scotland value box
  output$economy_scotland_rate <- renderValueBox({
    req(economy_values$processed_data, input$economy_metric)
    
    # Skip for Economic inactivity
    if(input$economy_metric == "Economic inactivity") {
      return(valueBox(value = "N/A", subtitle = "Scotland", icon = icon("flag"), color = "maroon"))
    }
    
    scotland_summary <- get_economy_scotland_summary(economy_values$processed_data, input$economy_metric)
    val <- format_economy_value(scotland_summary$value, input$economy_metric)
    year <- if(!is.na(scotland_summary$year)) scotland_summary$year else ""
    
    valueBox(value = val, subtitle = paste(scotland_summary$label, year), icon = icon("flag"), color = "maroon")
  })
  
  output$economy_urban_rural_gap <- renderValueBox({
    req(economy_values$processed_data, input$economy_metric)
    
    # Skip for Economic inactivity
    if(input$economy_metric == "Economic inactivity") {
<<<<<<< HEAD
      return(valueBox(value = "N/A", subtitle = "Urban-Rural Difference", icon = icon("balance-scale"), color = "aqua"))
=======
      return(valueBox(value = "N/A", subtitle = "Urban-Rural Gap", icon = icon("balance-scale"), color = "aqua"))
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
    }
    
    key_insights <- get_economy_urban_rural_2fold(economy_values$processed_data, input$economy_metric)
    val <- calculate_economy_gap(key_insights$urban, key_insights$rural, input$economy_metric)
    
<<<<<<< HEAD
    valueBox(value = val, subtitle = "Urban-Rural Difference", icon = icon("balance-scale"), color = "aqua")
=======
    valueBox(value = val, subtitle = "Urban-Rural Gap", icon = icon("balance-scale"), color = "aqua")
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
  })
  
  # Trend chart with 45-degree rotated labels and Scotland as gray
  output$economy_trend_chart <- renderPlotly({
    req(economy_values$processed_data, input$economy_classification_type)
    
    tryCatch({
      agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
      cat("Trend chart - unique areas:", paste(unique(agg_data$Area), collapse = ", "), "\n")
      
      # Format data for display
      is_percentage <- is_percentage_metric(input$economy_metric)
      
      if (is_percentage) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 0)
<<<<<<< HEAD
        if(input$economy_metric == "Regional Gross Value Added") {y_label <-"£ millions"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>£ millions: ", scales::comma(agg_data$Value_Rounded))
        }
        else if(input$economy_metric %in% c("Number of VAT/PAYE Businesses",
                                            "Registered private sector businesses",
                                            "High growth private businesses"
                                            )) 
        {y_label <-"Count"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Count: ", scales::comma(agg_data$Value_Rounded))
        }  else{y_label <-"Count"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Count: ", scales::comma(agg_data$Value_Rounded))
        }}
          
          
        
=======
        y_label <- "Value"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Value: ", scales::comma(agg_data$Value_Rounded))
      }
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
      
      agg_data$tooltip <- tooltip_text
      
      p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(x = "Year", y = y_label, color = "Area Type")
      
      # Add x-axis scale conditionally
      if (input$economy_metric == "High growth private businesses") {
        year_breaks <- 2010:2021
        year_labels <- c(
          "2010-2013", "2011-2014", "2012-2015", "2013-2016",
          "2014-2017", "2015-2018", "2016-2019", "2017-2020",
          "2018-2021", "2019-2022", "2020-2023", "2021-2024"
        )
        p <- p + scale_x_continuous(breaks = year_breaks, labels = year_labels)
      } else {
        p <- p + scale_x_continuous(breaks = function(x) {
          if(length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
        })
      }
        

      # Apply colors with Scotland as gray
      area_colors <- get_economy_colors(unique(agg_data$Area), input$economy_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      # Format y-axis
      if (is_percentage) {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      } else {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Replace the comparison chart output in the server:
  output$economy_comparison_chart <- renderPlotly({
    req(economy_values$processed_data, input$economy_classification_type)
    
    tryCatch({
      # Handle 3-fold economic inactivity (single year data)
      if(input$economy_metric == "Economic inactivity" && input$economy_classification_type == "3-fold") {
        selected_data <- economy_values$processed_data %>%
          mutate(Value_Rounded = round(Value, 1))
      } 
      # NEW: Handle 4-fold economic inactivity (multi-year data)
      else if(input$economy_metric == "Economic inactivity" && input$economy_classification_type == "4-fold") {
        req(input$economy_selected_year)
        agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
        selected_data <- agg_data %>% filter(Year == as.numeric(input$economy_selected_year))
        
        if (nrow(selected_data) == 0) {
          return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$economy_selected_year), textfont = list(size = 16)))
        }
        
        selected_data <- selected_data %>%
          mutate(Value_Rounded = round(Value, 1))
      }
      else {
        req(input$economy_selected_year)
        agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
        selected_data <- agg_data %>% filter(Year == as.numeric(input$economy_selected_year))
        
        if (nrow(selected_data) == 0) {
          return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$economy_selected_year), textfont = list(size = 16)))
        }
        
        selected_data <- selected_data %>%
          mutate(Value_Rounded = round(Value, 1))
      }
      
      cat("Comparison chart - selected_data dimensions:", nrow(selected_data), "x", ncol(selected_data), "\n")
      
      # Format data
      is_percentage <- is_percentage_metric(input$economy_metric)
      
      if (is_percentage) {
        x_label <- "Percentage (%)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Rate: ", selected_data$Value_Rounded, "%")
      }else {
        x_label <- if(input$economy_metric == "Regional Gross Value Added"){"Millions of pounds" }
        else{"Number of businesses"}
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Value: ", scales::comma(selected_data$Value_Rounded))
      }
      
      selected_data$tooltip <- tooltip_text
      
      # Create horizontal bar chart
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Area, Value), fill = Area, text = tooltip)) +
        geom_col(alpha = 0.8, width = 0.7) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "", x = x_label)
      
      # Apply colors with Scotland as gray
      area_colors <- get_economy_colors(unique(selected_data$Area), input$economy_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      # Format x-axis
      if (is_percentage) {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }  else if(input$economy_metric == "Regional Gross Value Added"){
        p <- p + scale_x_continuous(labels = scales::comma_format(prefix = "£"))
      }else {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      }
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  # Download handlers 
  observeEvent(input$economy_trend_download, {
    req(economy_values$processed_data, input$economy_classification_type)
    
    agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
    
    if (nrow(agg_data) > 0) {
      download_data <- agg_data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)
      
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$economy_metric)
      filename <- paste0("Economy_Trend_", metric_name, "_", input$economy_classification_type, "_", Sys.Date(), ".csv")
      
      write.csv(download_data, filename, row.names = FALSE)
      showNotification(paste("Chart data downloaded as", filename), type = "success")
    }
  })
  
  observeEvent(input$economy_comparison_download, {
    req(economy_values$processed_data, input$economy_classification_type)
    
    if(input$economy_metric == "Economic inactivity" && input$economy_classification_type == "3-fold") {
      download_data <- economy_values$processed_data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Area)
    } else {
      req(input$economy_selected_year)
      agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
      download_data <- agg_data %>% 
        filter(Year == as.numeric(input$economy_selected_year)) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Area)
    }
    
    if (nrow(download_data) > 0) {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$economy_metric)
      year_suffix <- if(input$economy_metric == "Economic inactivity" && input$economy_classification_type == "3-fold") "2023" else input$economy_selected_year
      filename <- paste0("Economy_Comparison_", metric_name, "_", input$economy_classification_type, "_", year_suffix, "_", Sys.Date(), ".csv")
      
      write.csv(download_data, filename, row.names = FALSE)
      showNotification(paste("Chart data downloaded as", filename), type = "success")
    }
  })
  
  observeEvent(input$economy_table_download, {
    req(economy_values$processed_data, input$economy_classification_type)
    
    if(input$economy_metric == "Economic inactivity" && input$economy_classification_type == "3-fold") {
      filtered_data <- economy_values$processed_data
    } else {
      agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
      filtered_data <- agg_data
      
      if(!is.null(input$economy_table_year_filter) && input$economy_table_year_filter != "all") {
        filtered_data <- filtered_data %>% filter(Year == as.numeric(input$economy_table_year_filter))
      }
      
      if(!is.null(input$economy_table_area_filter) && input$economy_table_area_filter != "all") {
        filtered_data <- filtered_data %>% filter(Area == input$economy_table_area_filter)
      }
    }
    
    if (nrow(filtered_data) > 0) {
      download_data <- filtered_data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area)
      
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$economy_metric)
      filename <- paste0("Economy_Table_", metric_name, "_", input$economy_classification_type, "_", Sys.Date(), ".csv")
      
      write.csv(download_data, filename, row.names = FALSE)
      showNotification(paste("Table data downloaded as", filename), type = "success")
    }
  })
  
  # Data table
  output$economy_data_table <- DT::renderDataTable({
    req(economy_values$processed_data, input$economy_classification_type)
    
    if(nrow(economy_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_economy_metric_display_name(input$economy_metric)
    
    # Handle both 3-fold and 4-fold economic inactivity specially
    if(input$economy_metric == "Economic inactivity") {
      if(input$economy_classification_type == "3-fold") {
        # 3-fold: use raw data with sub-metrics
        table_data <- economy_values$processed_data %>%
          arrange(Area) %>%
          select(Year, Area, Value, Data_Source, Sub_Metric)
      } else if(input$economy_classification_type == "4-fold") {
        # 4-fold: use aggregated data
        agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
        
        if(nrow(agg_data) == 0) {
          return(DT::datatable(data.frame(Message = "No data available for 4-fold classification"), options = list(dom = 't')))
        }
        
        # Apply filters
        filtered_data <- agg_data
        
        if(!is.null(input$economy_table_year_filter) && input$economy_table_year_filter != "all") {
          filtered_data <- filtered_data %>% filter(Year == as.numeric(input$economy_table_year_filter))
        }
        
        if(!is.null(input$economy_table_area_filter) && input$economy_table_area_filter != "all") {
          filtered_data <- filtered_data %>% filter(Area == input$economy_table_area_filter)
        }
        
        table_data <- filtered_data %>%
          arrange(desc(Year), Area) %>%
          select(Year, Area, Value, Data_Source)
      }
    } else {
      # All other metrics
      agg_data <- aggregate_economy_by_classification(economy_values$processed_data, input$economy_classification_type, input$economy_metric)
      
      if(nrow(agg_data) == 0) {
        return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
      }
      
      # Apply filters
      filtered_data <- agg_data
      
      if(!is.null(input$economy_table_year_filter) && input$economy_table_year_filter != "all") {
        filtered_data <- filtered_data %>% filter(Year == as.numeric(input$economy_table_year_filter))
      }
      
      if(!is.null(input$economy_table_area_filter) && input$economy_table_area_filter != "all") {
        filtered_data <- filtered_data %>% filter(Area == input$economy_table_area_filter)
      }
      
      if(nrow(filtered_data) == 0) {
        return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
      }
      
      table_data <- filtered_data %>%
        arrange(desc(Year), Area) %>%
        select(Year, Area, Value, Data_Source)
    }
    
    # Format value column based on metric type
    if (is_percentage_metric(input$economy_metric)) {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    } else {
      value_col_name <- "Value"
      table_data$Value <- round(table_data$Value, 0)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$economy_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (is_percentage_metric(input$economy_metric)) {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    } else {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "", digits = 0)
    }
    
    return(dt)
  })
}














