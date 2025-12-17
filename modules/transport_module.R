<<<<<<< HEAD

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(plotly)
library(DT)

# 1. Transport metrics configuration 
transport_metrics <- list(
  "Public transport satisfaction" = list(
    file_6fold = "transport_satisfaction_6_fold.csv",
    classifications = c("6-fold"),
    full_name = "Satisfaction with the quality of public transport"
  ),
  "Road Condition" = list(
    file_4fold = #"road_conditions.csv", # remove until fix data
      "",
    classifications = c("2-fold", "4-fold"),
    full_name = "The Percentage Of Roads Needing Repairs (Red And Amber Classification) In Scotland"
  ),
  "EV Infrastructure" = list(
    file_4fold = #"ev_infrastructure.xlsx", # remove until data fix
      "" ,
    classifications = c("2-fold", "4-fold"),
    full_name = "Publicly available electric vehicle charging devices at all speeds by local authority per 100,000 population"
  ),
  "Weekly travel costs" = list(
    file_3fold_uk = "mis.csv",
    classifications = c("3-fold (UK)"),
    full_name = "Weekly travel costs in different Minimum Income Standard (MIS) budgets",
    has_sub_metrics = TRUE
  ),
  "Key service access" = list(
    file_3fold = "15min_drive.csv",
    classifications = c("3-fold"),
    full_name = "Percentage of population within 15 minute drive time by public transport of key service",
    has_sub_metrics = TRUE
  ),
  "Transport affordability" = list(
    file_6fold = "easy_to_afford_transport.csv",
    classifications = c("6-fold"),  # ONLY 6-fold for now
    full_name = "How easy people find it to afford transport costs",
    has_sub_metrics = FALSE
  ),
  "Mode of transport (work)" = list(
    file_6fold = "mode_of_transport.csv", 
    classifications = c("6-fold"),  # ONLY 6-fold for now
    full_name = "How adults usually travel to work (percentages)",
    has_sub_metrics = TRUE
  ),
  "Ferry reliability" = list(
    file = "ferry_reliability.csv",
    classifications = c("ferry operator"),
    full_name = "Percentage of Scottish lifeline ferry services that are reliable and punctual by operator",
    has_sub_metrics = TRUE
  )
)
transport_key_insights <- list(
   "Public transport satisfaction" = "Satisfaction with public transport is lowest amongst people living in rural areas.",
  "Road Condition" =  "Areas with a rural component have a higher percentage of roads needing repair than larger cities. However, this percentage has fallen since 2015.",
  "EV Infrastructure" = "The number of publicly available electric vehicle charging devices per 100,000 population has increased since 2019 across all area types. Islands and remote rural areas have the highest number per 100,000 population.",
  "Weekly travel costs"= "Island and Remote Scottish Mainland areas have higher weekly travel costs than the UK average when considered across different Minimum Income Budgets.",
  "Key service access"= "A lower percentage of the population in rural areas live within a 15 minute public transport journey of a post office, GP practice, and, most notably, shopping centre, compared with those that live in urban areas.",
  "Transport affordability"= "In 2023 a higher percentage of people in rural geographical classifications found it easy to afford transport costs (between 74% and 85%) than in urban areas (as low as 63% in large urban areas). Between 2022 and 2023 the biggest changes in positive perceptions of affordability were a 27 percentage points rise in remote rural areas, followed by 20 percentage points in remote small towns.",
  "Mode of transport (work)"= "Those living in rural areas are most likely to drive to work.",
  "Ferry reliability"= "Overall, ferry reliability and punctuality is high, with the reliability and punctuality measures for Northlink and CalMac all exceeding 98% in 2022-23."
  )
transport_notes <- list(
   "Public transport satisfaction" = "1. SHS dates back to 1999 but satisfaction with the quality of public transport only divided by rural-urban classification from 2013 onwards. 2. The results of the 2020 and 2021 SHS telephone survey are published as experimental statistics. They are not directly comparable to SHS face-to-face survey results. For interest, data for these years has been included below. 3. The most relevant available version of SG urban rural classification for each year is used in outputs from the SHS. The 2020 version of the Scottish Government six-fold and two-fold urban/rural classifications of Scotland are used in the 2021 to 2023 Scottish Household Survey outputs. 4. 2013 data was revised in 2015.",
  "Road Condition" =  "",
  "EV Infrastructure" = "note placeholder",
  "Weekly travel costs"= "1. Weekly travel costs includes motoring and other travel costs. 2. The Cost of Remoteness Study uses categories 4 and 6 (remote small towns and remote rural areas) of the SG 6-fold Urban Rural classification 2016 as its definition of remote. The inclusion of remote small towns and exclusion of non-remote rural areas makes this a measure of remoteness not rurality. See 2021 report for more details. 3. Source data does not provide sample sizes.",
  "Key service access"= "1. 2016, 2020 figures use Scottish Government Urban Rural Classification 2016. 2. 2012 figures use Scottish Government Urban Rural Classification 2013-2014.  3. 2009 figures use Scottish Government Urban Rural Classification 2009-2010. 4. 2006 figures use Scottish Government Urban Rural Classification 2007-2008. 5. Source data does not provide sample sizes  ",
  "Transport affordability"= "1. This is a new question from source survey for 2021. 2. The most relevant available version of SG urban rural classification for each year is used in outputs from the SHS. The 2020 version of the Scottish Government six-fold and two-fold urban/rural classifications of Scotland are used in the 2021 to 2023 Scottish Household Survey outputs. 3. Due to pandemic-related changes to the Scottish Household Survey, figures for 2021 are not comparable with other years. ",
  "Mode of transport (work)"= "1. Rail includes Glasgow underground. 2. Other includes Edinburgh trams. 3. Scottish Household Survey uses most relevant SG Urban Rural Classification for each year. 4. Due to pandemic-related changes to the Scottish Household Survey, figures for 2020 and 2021 are not comparable with other years.",
  "Ferry reliability"= ""
  )
# Sub-metrics definitions
mis_budget_sub_metrics <- c(
  "Couples with two children" = "Couples with two children",
  "Single, working-age, no children" = "Single, working-age, no children",
  "Couple, working-age, no children" = "Couple, working-age, no children",
  "Single Pensioner" = "Single Pensioner",
  "Couple Pensioner" = "Couple Pensioner"
)

key_service_sub_metrics <- c(
  "Shopping Centre" = "Shopping Centre",
  "Post Office" = "Post Office",
  "GP" = "GP"
)

transport_mode_sub_metrics <- c(
  "Walking" = "Walking",
  "Driver" = "Driver",
  "Passenger" = "Passenger",
  "Bicycle" = "Bicycle",
  "Bus" = "Bus",
  "Rail" = "Rail",
  "Other" = "Other"
)

ferry_measure_sub_metrics <- c(
  "Reliability" = "Reliability",
  "Punctuality" = "Punctuality",
  "Punctuality - Aberdeen routes" = "Punctuality - Aberdeen routes",
  "Punctuality - Pentland Firth" = "Punctuality - Pentland Firth"
)

# RESAS 4-fold classification mapping
resas_council_mapping_transport <- list(
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

# Function to get display name for metrics
get_transport_metric_display_name <- function(metric_name) {
  metric_info <- transport_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# Helper function to resolve file path
get_file_path <- function(filename) {
  main_dir <- dirname(file.path(getwd(), "app.R"))
  filepath <- file.path(main_dir, "transport", filename)
  if (!file.exists(filepath)) {
    filepath <- file.path("transport", filename)
    if (!file.exists(filepath)) {
      stop("File not found in either location: ", filename)
    }
  }
  return(filepath)
}
# Data Loading Functions

# Load 2-fold data from simplified CSV files 
load_transport_2fold_data_simple <- function(filename, selected_activity = NULL) {
  cat("Loading transport 2-fold data from:", filename, "\n")
  
  tryCatch({
    if(!file.exists(filename)) {
      cat("File not found:", filename, "\n")
      return(data.frame())
    }
    
    # Read CSV 
    raw_data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
    
    # Set proper column names
    colnames(raw_data) <- c("ID", "Year", "Council", "Activity", "Rest_of_Scotland", "Rural_Scotland", "All")
    
    # Filter for selected activity if specified
    if (!is.null(selected_activity)) {
      raw_data <- raw_data %>% filter(Activity == selected_activity)
    }
    
    # Remove any "Base" entries
    raw_data <- raw_data %>% filter(!grepl("Base", Activity, ignore.case = TRUE))
    
    # Process data - create records for Urban, Rural, Scotland
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      year <- as.numeric(raw_data$Year[i])
      if (is.na(year)) next
      
      activity <- raw_data$Activity[i]
      rest_scotland <- raw_data$Rest_of_Scotland[i]
      rural_scotland <- raw_data$Rural_Scotland[i]
      all_scotland <- raw_data$All[i]
      
      # Urban data
      if (!is.na(rest_scotland) && rest_scotland != "-" && rest_scotland != "*") {
        urban_row <- data.frame(
          Year = year,
          Activity = activity,
          Area = "Urban",
          Value = as.numeric(rest_scotland),
          Data_Source = "Scottish Household Survey - 2-fold",
          stringsAsFactors = FALSE
        )
        processed_data <- rbind(processed_data, urban_row)
      }
      
      # Rural data
      if (!is.na(rural_scotland) && rural_scotland != "-" && rural_scotland != "*") {
        rural_row <- data.frame(
          Year = year,
          Activity = activity,
          Area = "Rural",
          Value = as.numeric(rural_scotland),
          Data_Source = "Scottish Household Survey - 2-fold",
          stringsAsFactors = FALSE
        )
        processed_data <- rbind(processed_data, rural_row)
      }
      
      # Scotland total
      if (!is.na(all_scotland) && all_scotland != "-" && all_scotland != "*") {
        scotland_row <- data.frame(
          Year = year,
          Activity = activity,
          Area = "Scotland",
          Value = as.numeric(all_scotland),
          Data_Source = "Scottish Household Survey - 2-fold",
          stringsAsFactors = FALSE
        )
        processed_data <- rbind(processed_data, scotland_row)
      }
    }
    
    cat(paste("Loaded", nrow(processed_data), "transport 2-fold records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading transport 2-fold data:", e$message, "\n")
    return(data.frame())
  })
}

load_transport_satisfaction_6fold <- function() {
  filepath <- get_file_path("transport_satisfaction_6_fold.csv")
  cat("Loading transport satisfaction 6-fold data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    cat("Column names:", paste(colnames(raw_data), collapse = ", "), "\n")
    
    # Filter for satisfied responses only
    satisfied_data <- raw_data %>%
      filter(serv5g == "Satisfied") %>%
      filter(!is.na(Year), !is.na(Council))
    
    if (nrow(satisfied_data) == 0) {
      cat("No satisfied data found\n")
      return(data.frame())
    }
    
    # Process the data - handle column names without backticks
    processed_data <- satisfied_data %>%
      select(Year, Council, Large.urban.areas, Other.urban.areas, Accessible.small.towns, 
             Remote.small.towns, Accessible.rural, Remote.rural, All) %>%
      pivot_longer(cols = c(Large.urban.areas, Other.urban.areas, Accessible.small.towns, 
                            Remote.small.towns, Accessible.rural, Remote.rural), 
                   names_to = "Area", values_to = "Value") %>%
      mutate(
        Area = case_when(
          Area == "Large.urban.areas" ~ "Large Urban Areas",
          Area == "Other.urban.areas" ~ "Other Urban Areas", 
          Area == "Accessible.small.towns" ~ "Accessible Small Towns",
          Area == "Remote.small.towns" ~ "Remote Small Towns",
          Area == "Accessible.rural" ~ "Accessible Rural",
          Area == "Remote.rural" ~ "Remote Rural",
          TRUE ~ Area
        ),
        Value = case_when(
          Value %in% c("-", "*", "", "NA") ~ NA_real_,
          TRUE ~ as.numeric(Value)
        ),
        Data_Source = "Scottish Household Survey - Public Transport Satisfaction"
      ) %>%
      filter(!is.na(Value))
    
    # Add Scotland data from All column
    scotland_data <- satisfied_data %>%
      filter(!is.na(All), All != "-", All != "*") %>%
      select(Year, All) %>%
      mutate(
        Area = "Scotland",
        Value = as.numeric(All),
        Data_Source = "Scottish Household Survey - Public Transport Satisfaction"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source)
    
    # Combine both datasets
    final_data <- bind_rows(
      processed_data %>% select(Year, Area, Value, Data_Source),
      scotland_data
    )
    
    cat("Processed", nrow(final_data), "transport satisfaction records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading transport satisfaction data:", e$message, "\n")
    return(data.frame())
  })
}


# Road condition data - add Scotland aggregation
load_road_condition_data <- function() {
  #filepath <- get_file_path("road_conditions.csv")
  filepath <- ""
  cat("Loading road condition data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    
    if ("Indicator..road.network.traffic." %in% names(raw_data)) {
      names(raw_data)[names(raw_data) == "Indicator..road.network.traffic."] <- "Indicator"
    }
    
    filtered_data <- raw_data %>%
      filter(Indicator == "The Percentage Of Roads Needing Repairs (Red And Amber Classification) In Scotland") %>%
      filter(!is.na(DateCode), !is.na(Value), !is.na(FeatureName)) %>%
      mutate(
        Local_Authority = trimws(FeatureName),
        Year = as.numeric(DateCode),
        Value = as.numeric(Value)
      ) %>%
      filter(Year >= 2010, !is.na(Value))
    
    # Separate Scotland data
    scotland_data <- filtered_data %>%
      filter(Local_Authority == "Scotland") %>%
      mutate(
        Area = "Scotland",
        Data_Source = "Scottish Government - Road Condition"
      ) %>%
      select(Year, Area, Value, Data_Source)
    
    # Process non-Scotland data
    processed_data <- filtered_data %>%
      filter(Local_Authority != "Scotland") %>%
      mutate(
        Authority_Clean = case_when(
          Local_Authority == "City of Edinburgh" ~ "Edinburgh, City of",
          Local_Authority == "Edinburgh City" ~ "Edinburgh, City of",
          TRUE ~ Local_Authority
        ),
        Classification_4fold = case_when(
          Authority_Clean %in% resas_council_mapping_transport[["Islands & Remote Rural"]] ~ "Islands & Remote Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Mainly Rural"]] ~ "Mainly Rural", 
          Authority_Clean %in% resas_council_mapping_transport[["Urban with Substantial Rural"]] ~ "Urban with Substantial Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Larger Cities"]] ~ "Larger Cities",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Classification_4fold)) %>%
      group_by(Year, Classification_4fold) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        Area = Classification_4fold,
        Data_Source = "Scottish Government - Road Condition"
      ) %>%
      select(Year, Area, Value, Data_Source)
    
    # Combine with Scotland data
    final_data <- bind_rows(processed_data, scotland_data)
    
    cat("Processed", nrow(final_data), "road condition records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading road condition data:", e$message, "\n")
    return(data.frame())
  })
}
# Replace your load_ev_infrastructure_data function with this fixed version:

load_ev_infrastructure_data <- function(classification_type = "4-fold") {
  #filepath <- get_file_path("ev_infrastructure.xlsx") # until data fix
  filepath <- "" 
  cat("Loading EV infrastructure data from:", filepath, "\n")
  
  tryCatch({
    if (!file.exists(filepath)) {
      cat("EV infrastructure file not found at:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file with headers
    raw_data <- read_excel(filepath, col_names = TRUE, skip = 0)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      cat("No data found in Excel file\n")
      return(data.frame())
    }
    
    # Get column names - first two are authority code and name
    all_cols <- colnames(raw_data)
    authority_code_col <- all_cols[1]
    authority_name_col <- all_cols[2]
    date_cols <- all_cols[3:length(all_cols)]
    
    cat("Authority columns:", authority_code_col, "and", authority_name_col, "\n")
    cat("Date columns found:", length(date_cols), "\n")
    cat("Sample date columns:", paste(date_cols[1:min(5, length(date_cols))], collapse = ", "), "\n")
    
    # Group date columns by year to find the last column for each year
    year_to_latest_col <- list()
    
    for (col_name in date_cols) {
      # Extract year from column name (e.g., "Oct-19" -> "19", "Apr-25" -> "25")
      year_match <- str_extract(col_name, "\\d{2}$")
      
      if (!is.na(year_match)) {
        # Convert 2-digit year to 4-digit
        year_2digit <- as.numeric(year_match)
        year_4digit <- ifelse(year_2digit >= 19, 2000 + year_2digit, 2000 + year_2digit)
        year_key <- as.character(year_4digit)
        
        # Store the latest (rightmost) column for each year
        year_to_latest_col[[year_key]] <- col_name
      }
    }
    
    cat("Years and their latest columns:\n")
    for (year in names(year_to_latest_col)) {
      cat("  ", year, ":", year_to_latest_col[[year]], "\n")
    }
    
    processed_data <- data.frame()
    
    # Process each authority (each row)
    for (i in 1:nrow(raw_data)) {
      authority_code <- as.character(raw_data[i, authority_code_col])
      authority_name <- as.character(raw_data[i, authority_name_col])
      
      # Skip empty rows and Scotland totals (we'll calculate Scotland separately)
      if (is.na(authority_code) || is.na(authority_name) || 
          authority_name == "" || authority_name == "Scotland") {
        next
      }
      
      # Process each year using its latest available column
      for (year_str in names(year_to_latest_col)) {
        latest_col <- year_to_latest_col[[year_str]]
        year_value <- as.numeric(raw_data[i, latest_col])
        
        # Only include valid positive values
        if (!is.na(year_value) && year_value > 0) {
          processed_data <- rbind(processed_data, data.frame(
            Year = as.numeric(year_str),
            Local_Authority = trimws(authority_name),
            Authority_Code = authority_code,
            Value = year_value,
            Source_Column = latest_col,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat("Extracted", nrow(processed_data), "raw EV infrastructure records\n")
    
    if (nrow(processed_data) == 0) {
      cat("No valid data points extracted\n")
      return(data.frame())
    }
    
    # Clean authority names and map to RESAS classifications
    classified_data <- processed_data %>%
      mutate(
        Authority_Clean = case_when(
          Local_Authority == "City of Edinburgh" ~ "Edinburgh, City of",
          Local_Authority == "Edinburgh City" ~ "Edinburgh, City of",
          Local_Authority == "Argyll & Bute" ~ "Argyll and Bute",
          Local_Authority == "Dumfries & Galloway" ~ "Dumfries and Galloway", 
          Local_Authority == "Perth & Kinross" ~ "Perth and Kinross",
          Local_Authority == "Na h-Eileanan Siar" ~ "Na h-Eileanan Siar",
          TRUE ~ trimws(Local_Authority)
        ),
        Classification_4fold = case_when(
          Authority_Clean %in% resas_council_mapping_transport[["Islands & Remote Rural"]] ~ "Islands & Remote Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Mainly Rural"]] ~ "Mainly Rural", 
          Authority_Clean %in% resas_council_mapping_transport[["Urban with Substantial Rural"]] ~ "Urban with Substantial Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Larger Cities"]] ~ "Larger Cities",
          TRUE ~ NA_character_
        ),
        Classification_2fold = case_when(
          Classification_4fold %in% c("Islands & Remote Rural", "Mainly Rural") ~ "Rural",
          Classification_4fold %in% c("Urban with Substantial Rural", "Larger Cities") ~ "Urban",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Classification_4fold))
    
    cat("Classified", nrow(classified_data), "data points into RESAS categories\n")
    
    if (nrow(classified_data) == 0) {
      cat("No data points could be classified into RESAS categories\n")
      return(data.frame())
    }
    
    
    if (classification_type == "2-fold") {
      # 2-fold classification averages
      fold2_data <- classified_data %>%
        group_by(Year, Classification_2fold) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = Classification_2fold,
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      # Scotland average (calculated from all local authorities)
      scotland_data <- classified_data %>%
        group_by(Year) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = "Scotland",
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      final_data <- bind_rows(fold2_data, scotland_data)
      
    } else { 
      # 4-fold classification averages  
      fold4_data <- classified_data %>%
        group_by(Year, Classification_4fold) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = Classification_4fold,
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      # Scotland average (calculated from all local authorities)
      scotland_data <- classified_data %>%
        group_by(Year) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = "Scotland",
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      final_data <- bind_rows(fold4_data, scotland_data)
    }
    
    cat("Final processed data:", nrow(final_data), "records\n")
    cat("Years available:", paste(sort(unique(final_data$Year)), collapse = ", "), "\n")
    cat("Areas available:", paste(sort(unique(final_data$Area)), collapse = ", "), "\n")
    
    # Show sample of processed data
    if (nrow(final_data) > 0) {
      cat("Sample of final data:\n")
      print(head(final_data))
    }
    
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading EV infrastructure data:", e$message, "\n")
    cat("Full error traceback:\n")
    print(e)
    return(data.frame())
  })
}

# Load MIS travel costs data (3-fold UK)
load_mis_travel_costs_data <- function(selected_budget = NULL) {
  filepath <- get_file_path("mis.csv")
  cat("Loading MIS travel costs data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    expected_cols <- c("Region", "MIS budget", "2021", "2022")
    if (!all(expected_cols %in% colnames(raw_data))) {
      stop("Unexpected column names in mis.csv")
    }
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(`MIS budget`)) %>%
      mutate(
        Region = trimws(Region),
        `MIS budget` = trimws(`MIS budget`)
      ) %>%
      rename(MIS_Budget = `MIS budget`) %>%
      pivot_longer(cols = c("2021", "2022"), names_to = "Year", values_to = "Value") %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub("[£,]", "", Value)),
        Area = Region,
        Data_Source = "Scottish Government - MIS Travel Costs",
        Sub_Metric = MIS_Budget
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    if (!is.null(selected_budget) && selected_budget != "") {
      processed_data <- processed_data %>%
        filter(Sub_Metric == trimws(selected_budget))
    }
    
    cat("Processed", nrow(processed_data), "MIS travel cost records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading MIS travel costs data:", e$message, "\n")
    return(data.frame())
  })
}

# Load key service access data (3-fold only)
load_key_service_access_data <- function(selected_service = NULL) {
  filepath <- get_file_path("15min_drive.csv")
  cat("Loading key service access data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    expected_cols <- c("Region", "Service", "2006", "2009", "2012", "2016", "2020")
    if (!all(expected_cols %in% colnames(raw_data))) {
      stop("Unexpected column names in 15min_drive.csv")
    }
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Service)) %>%
      mutate(
        Region = trimws(Region),
        Service = trimws(Service)
      ) %>%
      pivot_longer(cols = c("2006", "2009", "2012", "2016", "2020"), names_to = "Year", values_to = "Value") %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub("%", "", Value)),
        Area = case_when(
          Region == "Rest of Scotland" ~ "Urban",
          Region == "Accessible Rural" ~ "Accessible Rural",
          Region == "Remote Rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Key Service Access",
        Sub_Metric = Service
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    if (!is.null(selected_service) && selected_service != "") {
      processed_data <- processed_data %>%
        filter(Sub_Metric == trimws(selected_service))
    }
    
    cat("Processed", nrow(processed_data), "key service access records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading key service access data:", e$message, "\n")
    return(data.frame())
  })
}

# Load ferry reliability data
load_ferry_reliability_data <- function(selected_measure = NULL) {
  filepath <- get_file_path("ferry_reliability.csv")
  cat("Loading ferry reliability data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    year_columns <- colnames(raw_data)[3:ncol(raw_data)]
    
    processed_data <- raw_data %>%
      filter(!is.na(Operator), !is.na(Measure)) %>%
      mutate(
        Operator = trimws(Operator),
        Measure = trimws(Measure)
      ) %>%
      pivot_longer(cols = all_of(year_columns), names_to = "Year_Range", values_to = "Value") %>%
      mutate(
        Year = as.character(Year_Range),
        Value = as.numeric(Value),
        Area = case_when(
          Operator == "CalMac" & grepl("Reliability", Measure) ~ "CalMac Reliability",
          Operator == "CalMac" & grepl("Punctuality", Measure) & !grepl("-", Measure) ~ "CalMac Punctuality",
          Operator == "NorthLink" & grepl("Aberdeen", Measure) ~ "NorthLink Punctuality - Aberdeen routes",
          Operator == "NorthLink" & grepl("Pentland", Measure) ~ "NorthLink Punctuality - Pentland Firth",
          TRUE ~ paste(Operator, Measure)
        ),
        Data_Source = "Scottish Government - Ferry Reliability",
        Sub_Metric = Measure
      ) %>%
      filter(!is.na(Value)) %>%
      mutate(
        Year_Numeric = as.numeric(paste0("20", substr(Year_Range, 6, 7)))
      ) %>%
      select(Year = Year_Numeric, Area, Value, Data_Source, Sub_Metric, Year_Range)
    
    cat("Processed", nrow(processed_data), "ferry reliability records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading ferry reliability data:", e$message, "\n")
    return(data.frame())
  })
}

# Main data loading function 
load_transport_data_simple <- function(metric_name, classification_type, selected_activity = NULL) {
  metric_info <- transport_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  # Handle each metric type
  if (metric_name == "Public transport satisfaction") {
    if (classification_type == "6-fold") {
      return(load_transport_satisfaction_6fold())
    } else {
      cat("Only 6-fold classification available for Public transport satisfaction\n")
      return(data.frame())
    }
    
  } else if (metric_name == "Transport affordability") {
    if (classification_type == "2-fold") {
      # Check for 2-fold file first
      file_2fold <- get_file_path("easy_to_afford_transport_2_fold.csv")
      if (file.exists(file_2fold)) {
        return(load_transport_2fold_data_simple(file_2fold, selected_activity))
      } else {
        # Show coming soon message
        cat("2-fold file not available for Transport affordability - showing coming soon\n")
        return(data.frame(Message = "Coming Soon"))
      }
    } else if (classification_type == "6-fold") {
      # Load from existing 6-fold file
      return(load_transport_affordability_6fold())
    } else if (classification_type == "3-fold") {
      # Create 3-fold from 6-fold (if available)
      data_6fold <- load_transport_affordability_6fold()
      if (nrow(data_6fold) > 0) {
        return(create_transport_3fold_simple(data_6fold))
      }
    }
    
  } else if (metric_name == "Mode of transport (work)") {
    if (classification_type == "2-fold") {
      # Check for 2-fold file first
      file_2fold <- get_file_path("mode_of_transport_2_fold.csv")
      if (file.exists(file_2fold)) {
        return(load_transport_2fold_data_simple(file_2fold, selected_activity))
      } else {
        # Show coming soon message
        cat("2-fold file not available for Mode of transport - showing coming soon\n")
        return(data.frame(Message = "Coming Soon"))
      }
    } else if (classification_type == "6-fold") {
      return(load_mode_of_transport_6fold(selected_activity))
    } else if (classification_type == "3-fold") {
      # Create 3-fold from 6-fold
      data_6fold <- load_mode_of_transport_6fold(selected_activity)
      if (nrow(data_6fold) > 0) {
        return(create_transport_3fold_simple(data_6fold))
      }
    }
    
  } else if (metric_name == "Road Condition") {
    if (classification_type %in% c("2-fold", "4-fold")) {
      return(load_road_condition_data())
    }
    
  } else if (metric_name == "EV Infrastructure") {
    if (classification_type %in% c("2-fold", "4-fold")) {
      return(load_ev_infrastructure_data(classification_type))  # Pass the classification_type
    }
    
  } else if (metric_name == "Weekly travel costs") {
    if (classification_type == "3-fold (UK)") {
      return(load_mis_travel_costs_data(selected_activity))
    }
    
  } else if (metric_name == "Key service access") {
    if (classification_type == "3-fold") {
      return(load_key_service_access_data(selected_activity))
    }
    
  } else if (metric_name == "Ferry reliability") {
    if (classification_type == "ferry operator") {
      return(load_ferry_reliability_data(selected_activity))
    }
  }
  
  return(data.frame())
}

# Transport affordability
load_transport_affordability_6fold <- function() {
  filepath <- get_file_path("easy_to_afford_transport.csv")
  cat("Loading transport affordability 6-fold data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    cat("Unique regions:", paste(unique(raw_data$Region), collapse = ", "), "\n")
    
    # Separate Scotland data
    scotland_data <- raw_data %>%
      filter(grepl("Scotland|All Scotland|Scotland \\(All\\)", Region, ignore.case = TRUE)) %>%
      mutate(
        Area = "Scotland",
        Value = as.numeric(Affordability),
        Data_Source = "Scottish Household Survey - Transport Affordability"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source)
    
    # Process non-Scotland data
    clean_data <- raw_data %>%
      filter(!grepl("Scotland|All Scotland|Scotland \\(All\\)", Region, ignore.case = TRUE)) %>%
      mutate(
        Region = trimws(Region),
        Year = as.numeric(Year),
        Affordability = as.numeric(Affordability)
      ) %>%
      filter(!is.na(Region), !is.na(Year), !is.na(Affordability))
    
    processed_data <- clean_data %>%
      mutate(
        Area = case_when(
          grepl("Large urban|Large Urban", Region, ignore.case = TRUE) ~ "Large Urban Areas",
          grepl("Other urban|Other Urban", Region, ignore.case = TRUE) ~ "Other Urban Areas",
          grepl("Small accessible|Accessible small", Region, ignore.case = TRUE) ~ "Accessible Small Towns",
          grepl("Small remote|Remote small", Region, ignore.case = TRUE) ~ "Remote Small Towns",
          grepl("Accessible rural", Region, ignore.case = TRUE) ~ "Accessible Rural",
          grepl("Remote rural", Region, ignore.case = TRUE) ~ "Remote Rural",
          TRUE ~ Region
        ),
        Value = as.numeric(Affordability),
        Data_Source = "Scottish Household Survey - Transport Affordability"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source)
    
    # Combine with Scotland data
    final_data <- bind_rows(processed_data, scotland_data) |> 
      filter(Year != 2021) # remove 2021 data as not comparable
    
    cat("Processed", nrow(final_data), "transport affordability records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading transport affordability 6-fold data:", e$message, "\n")
    return(data.frame())
  })
}

# Mode of transport
load_mode_of_transport_6fold <- function(selected_mode = NULL) {
  filepath <- get_file_path("mode_of_transport.csv")
  cat("Loading mode of transport 6-fold data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Clean column names
    names(raw_data) <- gsub("\r", "", names(raw_data))
    
    # Separate Scotland data
    scotland_data <- raw_data %>%
      filter(grepl("All Scotland|Scotland", Region, ignore.case = TRUE)) %>%
      mutate(
        Region = gsub("\r", "", Region),
        Region = trimws(Region),
        Mode = gsub("\r", "", `Mode of Travel`),
        Mode = trimws(Mode)
      ) %>%
      gather(key = "Year", value = "Value", -Region, -`Mode of Travel`, -Mode) %>%
      mutate(
        Year = as.numeric(Year),
        Value = case_when(
          Value == "#N/A" ~ NA_real_,
          Value == "" ~ NA_real_,
          TRUE ~ as.numeric(Value)
        )
      ) %>%
      filter(!is.na(Year), !is.na(Value)) %>%
      mutate(
        Area = "Scotland",
        Data_Source = "Scottish Household Survey - Mode of Transport to Work",
        Sub_Metric = Mode
      ) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    # Process non-Scotland data
    processed_data <- raw_data %>%
      filter(!grepl("All Scotland|Scotland", Region, ignore.case = TRUE)) %>%
      mutate(
        Region = gsub("\r", "", Region),
        Region = trimws(Region),
        Mode = gsub("\r", "", `Mode of Travel`),
        Mode = trimws(Mode)
      ) %>%
      gather(key = "Year", value = "Value", -Region, -`Mode of Travel`, -Mode) %>%
      mutate(
        Year = as.numeric(Year),
        Value = case_when(
          Value == "#N/A" ~ NA_real_,
          Value == "" ~ NA_real_,
          TRUE ~ as.numeric(Value)
        )
      ) %>%
      filter(!is.na(Year), !is.na(Value)) %>%
      mutate(
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban" ~ "Other Urban Areas",
          Region == "Small accessible towns" ~ "Accessible Small Towns",
          Region == "Small remote towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Household Survey - Mode of Transport to Work",
        Sub_Metric = Mode
      ) %>%
      filter(!is.na(Area)) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    # Combine processed data with Scotland data
    final_data <- bind_rows(processed_data, scotland_data)
    
    if (!is.null(selected_mode) && selected_mode != "") {
      final_data <- final_data %>%
        filter(Sub_Metric == selected_mode)
    }
    
    cat("Processed", nrow(final_data), "mode of transport records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading mode of transport 6-fold data:", e$message, "\n")
    return(data.frame())
  })
}


# Create 3-fold from 6-fold data 
create_transport_3fold_simple <- function(data_6fold) {
  cat("Creating transport 3-fold combination\n")
  
  if(nrow(data_6fold) == 0) {
    return(data.frame())
  }
  
  # Urban from 6-fold (4 urban areas combined)
  urban_data <- data_6fold %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Rural areas from 6-fold (keep separate)
  rural_data <- data_6fold %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural"))
  
  # Combine
  combined_3fold <- bind_rows(urban_data, rural_data)
  
  cat(paste("Created", nrow(combined_3fold), "transport 3-fold records\n"))
  return(combined_3fold)
}
#  UI and Helper Functions

# Helper functions for processing and colors
simple_aggregate_transport_data <- function(processed_data, classification_type = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  # Handle both "Area" and "Area_Type" column names
  if ("Area_Type" %in% names(processed_data) && !("Area" %in% names(processed_data))) {
    cat("Converting Area_Type to Area column\n")
    processed_data <- processed_data %>%
      mutate(Area = Area_Type) %>%
      select(-Area_Type)
  }
  
  # Handle RESAS 4-fold to 2-fold aggregation
  if(classification_type == "2-fold" && "Larger Cities" %in% unique(processed_data$Area)) {
    cat("Creating 2-fold aggregation from 4-fold RESAS data\n")
    
    # Urban (Larger Cities + Urban with Substantial Rural)
    urban_data <- processed_data %>%
      filter(Area %in% c("Larger Cities", "Urban with Substantial Rural")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Urban")
    
    # Rural (Mainly Rural + Islands & Remote Rural)
    rural_data <- processed_data %>%
      filter(Area %in% c("Mainly Rural", "Islands & Remote Rural")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Rural")
    
    # Include Scotland if it exists
    scotland_data <- processed_data %>%
      filter(Area == "Scotland") %>%
      select(Year, Area, Value, Data_Source)
    
    return(bind_rows(urban_data, rural_data, scotland_data))
  }
  
  
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}
# Get key insights
get_transport_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold", selected_activity = NULL) {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  # ALWAYS try to get Scotland data directly if it exists
  scotland_direct <- latest_data %>% filter(Area == "Scotland")
  if (nrow(scotland_direct) > 0) {
    scotland_val <- scotland_direct$Value[1]
  }
  
  # Handle different area types for urban/rural
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    
  } else if (any(c("Larger Cities", "Urban with Substantial Rural", "Mainly Rural", "Islands & Remote Rural") %in% unique_areas)) {
    # RESAS 4-fold aggregation
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
    
    # If we don't have direct Scotland data, calculate it from all areas
    if (is.na(scotland_val)) {
      all_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(all_areas) > 0) {
        scotland_val <- mean(all_areas$Value, na.rm = TRUE)
      }
    }
    
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
    # 6-fold aggregation
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
  }
  
  return(list(
    urban = ifelse(!is.na(urban_val), urban_val, NA),
    rural = ifelse(!is.na(rural_val), rural_val, NA),
    scotland = ifelse(!is.na(scotland_val), scotland_val, NA),
    year = latest_year
  ))
}

# Get colors 
get_transport_colors <- function(areas, classification_type, metric_name = NULL) {
  color_mapping <- list()
  
  # Special handling for metrics that keep original colors
  if (!is.null(metric_name)) {
    if (metric_name == "Weekly travel costs") {
      return(list()) # Keep original viridis colors
    }
    if (metric_name == "Ferry reliability") {
      return(list()) # Keep original viridis colors
    }
  }
  
  # Use culture module color scheme for other metrics
  if (classification_type == "6-fold") {
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    }
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
  } else if (classification_type == "4-fold") {
    color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural" = "#F4E470", 
                          "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")

  }
  
  # Always set Scotland to gray
  color_mapping[["Scotland"]] <- "#B2B2B2"
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display (matching culture module)
format_transport_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name == "Weekly travel costs") {
    return(paste0("£", round(value, 1)))
  } else if (metric_name == "EV Infrastructure") {
    return(round(value, 1))
  } else {
    return(paste0(round(value, 1), "%"))
  }
}

# Calculate gap between urban and rural (matching culture module)
calculate_transport_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name == "Weekly travel costs") {
    return(paste0("£", round(gap, 1)))
  } else if (metric_name == "EV Infrastructure") {
    return(round(gap, 1))
  } else {
    return(paste0(round(gap, 1), "pp"))
  }
}
# Transport Dashboard UI
transport_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      /* STANDARDIZED CSS FOR ALL DASHBOARD MODULES - EXACTLY SAME AS CULTURE */
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
      .transport-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .transport-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .transport-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .transport-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .transport-metric-description {
        color: #6c757d !important;
        font-size: 0.95em !important;
        line-height: 1.4 !important;
        margin-left: 30px !important;
      }
      "))
    ),
    
    # Main header layout
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
            h2("Transport", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("transport_classification_selector")
          ),
          
          # Top-center-right: Transport metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("transport_metric", "Transport Metric:", 
                        choices = c("Select a policy metric..." = "", names(transport_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (when applicable)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.transport_metric == 'Weekly travel costs'",
              selectInput("mis_budget", "Sub-metric:", 
                          choices = names(mis_budget_sub_metrics), selected = names(mis_budget_sub_metrics)[1], width = "180px")
            ),
            conditionalPanel(
              condition = "input.transport_metric == 'Key service access'",
              selectInput("service", "Sub-metric:", 
                          choices = names(key_service_sub_metrics), selected = names(key_service_sub_metrics)[1], width = "180px")
            ),
            conditionalPanel(
              condition = "input.transport_metric == 'Mode of transport (work)'",
              selectInput("mode", "Sub-metric:", 
                          choices = names(transport_mode_sub_metrics), selected = names(transport_mode_sub_metrics)[1], width = "180px")
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
    
    # UI SECTION
    
    # Show content only when a metric is selected
    conditionalPanel(
      condition = "input.transport_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore transport data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Transport-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Public transport satisfaction
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Public transport satisfaction', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("bus", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Public transport satisfaction"),
                      div(class = "transport-metric-description", 
                          "Satisfaction with the quality of public transport across Scotland's urban and rural areas.")
                    )
                  )
                ),
                
                # Road Condition
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Road Condition', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("road", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Road Condition"),
                      div(class = "transport-metric-description",
                          "Percentage of roads needing repairs across Scotland's local authorities.")
                    )
                  )
                ),
                
                # EV Infrastructure
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'EV Infrastructure', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("charging-station", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "EV Infrastructure"),
                      div(class = "transport-metric-description",
                          "Electric vehicle charging devices per 100,000 population by local authority.")
                    )
                  )
                ),
                
                # Weekly travel costs
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Weekly travel costs', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("pound-sign", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Weekly travel costs"),
                      div(class = "transport-metric-description",
                          "Weekly travel costs in different Minimum Income Standard (MIS) budgets across UK regions.")
                    )
                  )
                ),
                
                # Key service access
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Key service access', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("map-marker-alt", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Key service access"),
                      div(class = "transport-metric-description",
                          "Percentage of population within 15 minute drive time by public transport of key services.")
                    )
                  )
                ),
                
                # Transport affordability
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Transport affordability', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("wallet", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Transport affordability"),
                      div(class = "transport-metric-description",
                          "How easy people find it to afford transport costs across Scotland.")
                    )
                  )
                ),
                
                # Mode of transport (work)
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Mode of transport (work)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("car", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Mode of transport (work)"),
                      div(class = "transport-metric-description",
                          "How adults usually travel to work showing percentages by different transport modes.")
                    )
                  )
                ),
                
                # Ferry reliability
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Ferry reliability', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("ship", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Ferry reliability"),
                      div(class = "transport-metric-description",
                          "Percentage of Scottish lifeline ferry services that are reliable and punctual by operator.")
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
      condition = "input.transport_metric != ''",
      
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("transport_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("transport_data_summary"))
          ),
          
          # Show key insights for metrics that support them (EXCLUSION approach)
          conditionalPanel(
            condition = "input.transport_metric == 'EV Infrastructure'",
            fluidRow(
              box(title = uiOutput("transport_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("transport_urban_rate", width = 3),
                    valueBoxOutput("transport_rural_rate", width = 3),
                    valueBoxOutput("transport_scotland_rate", width = 3),
                    valueBoxOutput("transport_urban_rural_gap", width = 3)
                  ))
            )
          )
          
      ),
      
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("transport_trend_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("transport_trend_download", "Download", class = "excel-download-btn"
                            ))),
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("transport_trend_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("transport_comparison_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("transport_comparison_download", "Download", class = "excel-download-btn"
                            ))),
            status = "primary", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;", uiOutput("transport_year_selector")),
            plotlyOutput("transport_comparison_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("transport_table_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("transport_table_download", "Download", class = "excel-download-btn"
                            ))),
            status = "info", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("transport_table_year_filter")),
                  column(6, uiOutput("transport_table_area_filter"))
                )),
            DT::dataTableOutput("transport_data_table") %>% withSpinner())
      )
    )
  )
}
# Transport module server
transport_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  transport_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards (matching culture module)
  observeEvent(input$transport_metric_select, {
    updateSelectInput(session, "transport_metric", selected = input$transport_metric_select)
  })
  
  # Dynamic UI outputs (matching culture module)
  output$transport_summary_title <- renderUI({
    req(input$transport_metric, input$transport_classification_type)
    display_name <- get_transport_metric_display_name(input$transport_metric)
    classification_text <- case_when(
      input$transport_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$transport_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$transport_classification_type == "3-fold (UK)" ~ "(UK Regions)",
      input$transport_classification_type == "4-fold" ~ "(4-fold RESAS)",
      input$transport_classification_type == "6-fold" ~ "(6-fold Classification)",
      input$transport_classification_type == "ferry operator" ~ "(Ferry Operator)",
      TRUE ~ paste0("(", input$transport_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$transport_year_selector <- renderUI({
    req(transport_values$processed_data)
    if (nrow(transport_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(transport_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("transport_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$transport_classification_selector <- renderUI({
    req(input$transport_metric)
    available_classifications <- transport_metrics[[input$transport_metric]]$classifications
    choices <- list()
    
    # Special handling for Weekly travel costs
    if (input$transport_metric == "Weekly travel costs") {
      if("3-fold (UK)" %in% available_classifications) choices[["UK Regions"]] <- "3-fold (UK)"
    } else {
      if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
      if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
      if("ferry operator" %in% available_classifications) choices[["Ferry Operator"]] <- "ferry operator"
    }
    
    # Open the most complex fold first 
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("transport_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles 
  output$transport_trend_title <- renderUI({
    req(input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste("Trend Analysis for", display_name)
  })
  
  output$transport_comparison_title <- renderUI({
    req(input$transport_selected_year, input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste0("Single Year Comparison (", input$transport_selected_year, ") for ", display_name)
  })
  
  output$transport_table_title <- renderUI({
    req(input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste("Data Table for", display_name)
  })
  
  output$transport_key_insights_title <- renderUI({
    req(input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$transport_table_year_filter <- renderUI({
    req(transport_values$processed_data)
    if(nrow(transport_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(transport_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("transport_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$transport_table_area_filter <- renderUI({
    req(transport_values$processed_data, input$transport_classification_type)
    if(nrow(transport_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("transport_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Process data reactively
  observe({
    if (is.null(input$transport_metric) || input$transport_metric == "") {
      transport_values$processed_data <- data.frame()
      transport_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$transport_classification_type)
    
    # Use simplified data loading with sub-metrics
    if (input$transport_metric == "Weekly travel costs") {
      req(input$mis_budget)
      selected_budget <- mis_budget_sub_metrics[input$mis_budget]
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type, 
        selected_budget
      )
    } else if (input$transport_metric == "Key service access") {
      req(input$service)
      selected_service <- key_service_sub_metrics[input$service]
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type, 
        selected_service
      )
    } else if (input$transport_metric == "Mode of transport (work)") {
      req(input$mode)
      selected_mode <- transport_mode_sub_metrics[input$mode]
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type, 
        selected_mode
      )
    } else {
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type
      )
    }
    
    transport_values$data_status <- if(nrow(transport_values$processed_data) > 0) "Transport data loaded" else "No data available"
  })
  
  # Data summary 
    output$transport_data_summary <- renderUI({
    req(transport_values$processed_data, input$transport_classification_type, input$transport_metric)
    if (is.null(transport_values$processed_data) || nrow(transport_values$processed_data) == 0) {
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
    display_name <- get_transport_metric_display_name(input$transport_metric)
    
    custom_insight <- transport_key_insights[[input$transport_metric]]
    custom_notes <- transport_notes[[input$transport_metric]]
    
    # Define source information
    source_info <- switch(input$transport_metric,
                          "Public transport satisfaction" = list(
                            text = "Scottish Household Survey",
                            url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
                          ),
                          "Road Condition" = list(
                            text = "SCOTS road condition surveys", 
                            url = "https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Froad-network-traffic"
                          ),
                          "EV Infrastructure" = list(
                            text = "Zapmap EV infrastructure data",
                            url = "https://www.gov.uk/government/collections/electric-vehicle-charging-infrastructure-statistics"
                          ),
                          "Weekly travel costs" = list(
                            text = "The Cost of Remoteness, MIS 2022",
                            url = "https://www.gov.scot/publications/the-cost-of-remoteness-reflecting-higher-living-costs-in-remote-rural-scotland-when-measuring-fuel-poverty/"
                          ),
                          "Key service access" = list(
                            text = "Scottish Index of Multiple Deprivation 2006, 2009, 2012, 2016, 2020",
                            url = "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/"
                          ),
                          "Transport affordability" = list(
                            text = "Scottish Household Survey (Transport and Travel in Scotland)",
                            url = "https://www.transport.gov.scot/search-results/?q=transport+and+travel+in+scotland&type=publication"
                          ),
                          "Mode of transport (work)" = list(
                            text = "Scottish Household Survey (Transport and Travel in Scotland)",
                            url = "https://www.transport.gov.scot/search-results/?q=transport+and+travel+in+scotland&type=publication"
                          ),
                          "Ferry reliability" = list(
                            text = "Transport Scotland water transport statistics",
                            url = "https://www.transport.gov.scot/publication/scottish-transport-statistics-2023/chapter-9-water-transport/"
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
    } else {
      # For metrics without custom insights (managing financially and influence over local decisions)
      return(
        div(class = "comparison-highlight",
            style = "background: linear-gradient(135deg, #e3f2fd, #f0f8ff); border-left: 4px solid #2196f3; padding: 20px; border-radius: 8px;",
            div(style = "display: flex; align-items: flex-start; gap: 15px;",
                icon("info-circle", style = "font-size: 1.5em; color: #2196f3; margin-top: 3px;"),
                div(
                  h4("Data Loaded", style = "margin: 0 0 10px 0; color: #1976d2; font-weight: 600;"),
                  p(paste("Showing data for:", display_name), style = "font-size: 1.05em; line-height: 1.6; margin: 0 0 15px 0; color: #1976d2;"),
                  hr(style = "border-top: 1px solid #bbdefb; margin: 15px 0 10px 0;"),
                  div(
                    style = "font-size: 0.9em; color: #1976d2;",
                    strong("Source: ", style = "color: #1976d2;"),
                    tags$a(
                      href = source_info$url,
                      target = "_blank",
                      source_info$text,
                      style = "color: #007bff; text-decoration: none; font-weight: 500;"
                    )
                  )
                )
            )
        )
      )
    }
  })

  # Value boxes with special handling for different metrics
  output$transport_urban_rate <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    # Skip for metrics that don't support urban/rural comparison
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
      year <- ""
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- format_transport_value(key_insights$urban, input$transport_metric)
      year <- if(!is.na(key_insights$year)) key_insights$year else ""
    }
    
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
  
  output$transport_rural_rate <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
      year <- ""
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- format_transport_value(key_insights$rural, input$transport_metric)
      year <- if(!is.na(key_insights$year)) key_insights$year else ""
    }
    
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
  
  # Scotland rate with special handling for EV Infrastructure  
  output$transport_scotland_rate <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
      year <- ""
      subtitle <- "Scotland Total"
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- format_transport_value(key_insights$scotland, input$transport_metric)
      year <- if(!is.na(key_insights$year)) key_insights$year else ""
      
      # Special case for EV Infrastructure - show as "Scotland Average"
      if (input$transport_metric == "EV Infrastructure") {
        subtitle <- paste("Scotland Average", year)
      } else {
        subtitle <- paste("Scotland Total", year)
      }
    }
    
   # valueBox(value = val, subtitle = subtitle, icon = icon("flag"), color = "maroon")
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", subtitle),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("flag"),
      color = "aqua"
    )
    
  })
  
  output$transport_urban_rural_gap <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- calculate_transport_gap(key_insights$urban, key_insights$rural, input$transport_metric)
    }
    
    #valueBox(value = val, subtitle = "Urban-Rural Gap", icon = icon("balance-scale"), color = "aqua")
    
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
  
  # Trend chart (matching culture module with 45-degree year labels and Scotland in grey)
  output$transport_trend_chart <- renderPlotly({
    req(transport_values$processed_data, input$transport_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "", textfont = list(size = 16)))
      }
      
      # Format data for display
      is_cost_data <- input$transport_metric == "Weekly travel costs"
      is_ev_data <- input$transport_metric == "EV Infrastructure"
      
      if (is_cost_data) {
        agg_data$Value_Display <- paste0("£", round(agg_data$Value, 1))
        y_label <- "Weekly Cost (£)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Cost: ", agg_data$Value_Display)
      } else if (is_ev_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "EV Charging per 100k"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>EV Charging: ", agg_data$Value_Rounded)
      } 
      else if (input$transport_metric == "Public transport satisfaction")  {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage very or fairly satisfied (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      else if (input$transport_metric == "Ferry reliability")  {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", paste0(agg_data$Year, "-", substr(agg_data$Year + 1, 3,4)), "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text
      
      # Create plot
      if (input$transport_metric == "Ferry reliability") {
        p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            },
            labels = function(x) {
              paste0(x - 1, "-", substr(x, 3, 4))  # e.g., 2022 becomes "2021-22"
            }
          )
        
      } else if (input$transport_metric == "Mode of transport (work)") {
        agg_data1 <- agg_data |> filter(Year < 2020)
        agg_data2 <- agg_data |> filter(Year >= 2022)
        
        p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_vline(xintercept = 2019, linetype = "dashed", color = "darkgrey", size = 0.5) +
          geom_vline(xintercept = 2022, linetype = "dashed", color = "darkgrey", size = 0.5) +
          annotate(
            "text",
            x = 2020.5,
            y = max(agg_data1$Value, na.rm = TRUE),
            label = "Data for 2020 and 2021 are impacted by COVID-19",
            angle = 90,
            hjust = 1,
            vjust = 1.1
          ) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          geom_line(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            }
          )
        
      } else {
        p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            }
          )
      }
      
      # Apply colors with Scotland in grey
      area_colors <- get_transport_colors(unique(agg_data$Area), input$transport_classification_type, input$transport_metric)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      } else {
        # Use viridis for metrics that keep original colors, but still show Scotland in grey if present
        if ("Scotland" %in% unique(agg_data$Area)) {
          manual_colors <- c("#B2B2B2")
          names(manual_colors) <- "Scotland"
          other_areas <- unique(agg_data$Area)[unique(agg_data$Area) != "Scotland"]
          if (length(other_areas) > 0) {
            viridis_colors <- viridis_discrete(option = "plasma", end = 0.8)(length(other_areas))
            names(viridis_colors) <- other_areas
            manual_colors <- c(manual_colors, viridis_colors)
          }
          p <- p + scale_color_manual(values = manual_colors)
        } else {
          p <- p + scale_color_viridis_d(option = "plasma", end = 0.8)
        }
      }
      
      # Format y-axis
      if (is_cost_data) {
        p <- p + scale_y_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_ev_data) {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in transport trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Comparison chart 
  output$transport_comparison_chart <- renderPlotly({
    req(transport_values$processed_data, input$transport_classification_type, input$transport_selected_year)
    
    tryCatch({
      agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
      selected_data <- agg_data %>% filter(Year == as.numeric(input$transport_selected_year))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$transport_selected_year), textfont = list(size = 16)))
      }
      
      # Format data
      is_cost_data <- input$transport_metric == "Weekly travel costs"
      is_ev_data <- input$transport_metric == "EV Infrastructure"
      
      if (is_cost_data) {
        selected_data$Value_Display <- paste0("£", round(selected_data$Value, 1))
        x_label <- "Weekly Cost (£)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Cost: ", selected_data$Value_Display)
      } else if (is_ev_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "EV Charging per 100k"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>EV Charging: ", selected_data$Value_Rounded)
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
      
      #  Apply colors with Scotland in grey
      area_colors <- get_transport_colors(unique(selected_data$Area), input$transport_classification_type, input$transport_metric)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      } else {
        # Use viridis for metrics that keep original colors, but still show Scotland in grey if present
        if ("Scotland" %in% unique(selected_data$Area)) {
          manual_colors <- c("#B2B2B2")
          names(manual_colors) <- "Scotland"
          other_areas <- unique(selected_data$Area)[unique(selected_data$Area) != "Scotland"]
          if (length(other_areas) > 0) {
            viridis_colors <- viridis_discrete(option = "plasma", end = 0.8)(length(other_areas))
            names(viridis_colors) <- other_areas
            manual_colors <- c(manual_colors, viridis_colors)
          }
          p <- p + scale_fill_manual(values = manual_colors)
        } else {
          p <- p + scale_fill_viridis_d(option = "plasma", end = 0.8)
        }
      }
      
      # Format x-axis
      if (is_cost_data) {
        p <- p + scale_x_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_ev_data) {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in transport comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Download handlers (matching culture module)
  # Transport Trend Download
  output$transport_trend_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$transport_metric)
      paste0("Transport_Trend_", metric, "_", input$transport_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transport_values$processed_data, input$transport_classification_type)
      
      data <- simple_aggregate_transport_data(
        transport_values$processed_data,
        input$transport_classification_type
      ) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Transport Comparison Download
  output$transport_comparison_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$transport_metric)
      paste0("Transport_Comparison_", metric, "_", input$transport_classification_type, "_", input$transport_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transport_values$processed_data, input$transport_classification_type, input$transport_selected_year)
      
      data <- simple_aggregate_transport_data(
        transport_values$processed_data,
        input$transport_classification_type
      ) %>%
        filter(Year == as.numeric(input$transport_selected_year)) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Transport Table Download
  output$transport_table_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$transport_metric)
      paste0("Transport_Table_", metric, "_", input$transport_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transport_values$processed_data, input$transport_classification_type)
      
      data <- simple_aggregate_transport_data(
        transport_values$processed_data,
        input$transport_classification_type
      )
      
      if (!is.null(input$transport_table_year_filter) && input$transport_table_year_filter != "all") {
        data <- data %>% filter(Year == as.numeric(input$transport_table_year_filter))
      }
      if (!is.null(input$transport_table_area_filter) && input$transport_table_area_filter != "all") {
        data <- data %>% filter(Area == input$transport_table_area_filter)
      }
      
      data <- data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  # Data table 
  output$transport_data_table <- DT::renderDataTable({
    req(transport_values$processed_data, input$transport_classification_type)
    
    if(nrow(transport_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_transport_metric_display_name(input$transport_metric)
    agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    # Apply filters
    filtered_data <- agg_data
    
    if(!is.null(input$transport_table_year_filter) && input$transport_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$transport_table_year_filter))
    }
    
    if(!is.null(input$transport_table_area_filter) && input$transport_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$transport_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    # Prepare table data
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    # Format value column based on metric type
    if (input$transport_metric == "Weekly travel costs") {
      value_col_name <- "Weekly Cost (£)"
      table_data$Value <- round(table_data$Value, 1)
    } else if (input$transport_metric == "EV Infrastructure") {
      value_col_name <- "EV Charging per 100k"
      table_data$Value <- round(table_data$Value, 1)
    } else {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$transport_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (input$transport_metric == "Weekly travel costs") {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "£", digits = 1)
    } else if (input$transport_metric == "EV Infrastructure") {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
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
library(shiny)
library(plotly)
library(DT)

# 1. Transport metrics configuration 
transport_metrics <- list(
  "Public transport satisfaction" = list(
    file_6fold = "transport_satisfaction_6_fold.csv",
    classifications = c("6-fold"),
    full_name = "Satisfaction with the quality of public transport"
  ),
  "Road Condition" = list(
    file_4fold = "road_conditions.csv",
    classifications = c("2-fold", "4-fold"),
    full_name = "The Percentage Of Roads Needing Repairs (Red And Amber Classification) In Scotland"
  ),
  "EV Infrastructure" = list(
    file_4fold = "ev_infrastructure.xlsx",
    classifications = c("2-fold", "4-fold"),
    full_name = "Publicly available electric vehicle charging devices at all speeds by local authority per 100,000 population"
  ),
  "Weekly travel costs" = list(
    file_3fold_uk = "mis.csv",
    classifications = c("3-fold (UK)"),
    full_name = "Weekly travel costs in different Minimum Income Standard (MIS) budgets",
    has_sub_metrics = TRUE
  ),
  "Key service access" = list(
    file_3fold = "15min_drive.csv",
    classifications = c("3-fold"),
    full_name = "Percentage of population within 15 minute drive time by public transport of key service",
    has_sub_metrics = TRUE
  ),
  "Transport affordability" = list(
    file_6fold = "easy_to_afford_transport.csv",
    classifications = c("6-fold"),  # ONLY 6-fold for now
    full_name = "How easy people find it to afford transport costs",
    has_sub_metrics = FALSE
  ),
  "Mode of transport (work)" = list(
    file_6fold = "mode_of_transport.csv", 
    classifications = c("6-fold"),  # ONLY 6-fold for now
    full_name = "How adults usually travel to work (percentages)",
    has_sub_metrics = TRUE
  ),
  "Ferry reliability" = list(
    file = "ferry_reliability.csv",
    classifications = c("ferry operator"),
    full_name = "Percentage of Scottish lifeline ferry services that are reliable and punctual by operator",
    has_sub_metrics = TRUE
  )
)
transport_key_insights <- list(
   "Public transport satisfaction" = "Satisfaction with public transport is lowest amongst people living in rural areas.",
  "Road Condition" =  "Areas with a rural component have a higher percentage of roads needing repair than larger cities. However, this percentage has fallen since 2015.",
  "EV Infrastructure" = "The number of publicly available electric vehicle charging devices per 100,000 population has increased since 2019 across all area types. Islands and remote rural areas have the highest number per 100,000 population.",
  "Weekly travel costs"= "Island and Remote Scottish Mainland areas have higher weekly travel costs than the UK average when considered across different Minimum Income Budgets.",
  "Key service access"= "A lower percentage of the population in rural areas live within a 15 minute public transport journey of a post office, GP practice, and, most notably, shopping centre, compared with those that live in urban areas.",
  "Transport affordability"= "In 2023 a higher percentage of people in rural geographical classifications found it easy to afford transport costs (between 74% and 85%) than in urban areas (as low as 63% in large urban areas). Between 2022 and 2023 the biggest changes in positive perceptions of affordability were a 27 percentage points rise in remote rural areas, followed by 20 percentage points in remote small towns.",
  "Mode of transport (work)"= "Those living in rural areas are most likely to drive to work.",
  "Ferry reliability"= "Overall, ferry reliability and punctuality is high, with the reliability and punctuality measures for Northlink and CalMac all exceeding 98% in 2022-23."
  )
transport_notes <- list(
   "Public transport satisfaction" = "1. SHS dates back to 1999 but satisfaction with the quality of public transport only divided by rural-urban classification from 2013 onwards. 2. The results of the 2020 and 2021 SHS telephone survey are published as experimental statistics. They are not directly comparable to SHS face-to-face survey results. For interest, data for these years has been included below. 3. The most relevant available version of SG urban rural classification for each year is used in outputs from the SHS. The 2020 version of the Scottish Government six-fold and two-fold urban/rural classifications of Scotland are used in the 2021 to 2023 Scottish Household Survey outputs. 4. 2013 data was revised in 2015.",
  "Road Condition" =  "note placeholder",
  "EV Infrastructure" = "note placeholder",
  "Weekly travel costs"= "1. Weekly travel costs includes motoring and other travel costs. 2. The Cost of Remoteness Study uses categories 4 and 6 (remote small towns and remote rural areas) of the SG 6-fold Urban Rural classification 2016 as its definition of remote. The inclusion of remote small towns and exclusion of non-remote rural areas makes this a measure of remoteness not rurality. See 2021 report for more details. 3. Source data does not provide sample sizes.",
  "Key service access"= "note placeholder",
  "Transport affordability"= "1. This is a new question from source survey for 2021. 2. The most relevant available version of SG urban rural classification for each year is used in outputs from the SHS. The 2020 version of the Scottish Government six-fold and two-fold urban/rural classifications of Scotland are used in the 2021 to 2023 Scottish Household Survey outputs. 3. Due to pandemic-related changes to the Scottish Household Survey, figures for 2021 are not comparable with other years. ",
  "Mode of transport (work)"= "1. Rail includes Glasgow underground. 2. Other includes Edinburgh trams. 3. Scottish Household Survey uses most relevant SG Urban Rural Classification for each year. 4. Due to pandemic-related changes to the Scottish Household Survey, figures for 2020 and 2021 are not comparable with other years.",
  "Ferry reliability"= "note placeholder"
  )
# Sub-metrics definitions
mis_budget_sub_metrics <- c(
  "Couples with two children" = "Couples with two children",
  "Single, working-age, no children" = "Single, working-age, no children",
  "Couple, working-age, no children" = "Couple, working-age, no children",
  "Single Pensioner" = "Single Pensioner",
  "Couple Pensioner" = "Couple Pensioner"
)

key_service_sub_metrics <- c(
  "Shopping Centre" = "Shopping Centre",
  "Post Office" = "Post Office",
  "GP" = "GP"
)

transport_mode_sub_metrics <- c(
  "Walking" = "Walking",
  "Driver" = "Driver",
  "Passenger" = "Passenger",
  "Bicycle" = "Bicycle",
  "Bus" = "Bus",
  "Rail" = "Rail",
  "Other" = "Other"
)

ferry_measure_sub_metrics <- c(
  "Reliability" = "Reliability",
  "Punctuality" = "Punctuality",
  "Punctuality - Aberdeen routes" = "Punctuality - Aberdeen routes",
  "Punctuality - Pentland Firth" = "Punctuality - Pentland Firth"
)

# RESAS 4-fold classification mapping
resas_council_mapping_transport <- list(
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

# Function to get display name for metrics
get_transport_metric_display_name <- function(metric_name) {
  metric_info <- transport_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# Helper function to resolve file path
get_file_path <- function(filename) {
  main_dir <- dirname(file.path(getwd(), "app.R"))
  filepath <- file.path(main_dir, "transport", filename)
  if (!file.exists(filepath)) {
    filepath <- file.path("transport", filename)
    if (!file.exists(filepath)) {
      stop("File not found in either location: ", filename)
    }
  }
  return(filepath)
}
# Data Loading Functions

# Load 2-fold data from simplified CSV files 
load_transport_2fold_data_simple <- function(filename, selected_activity = NULL) {
  cat("Loading transport 2-fold data from:", filename, "\n")
  
  tryCatch({
    if(!file.exists(filename)) {
      cat("File not found:", filename, "\n")
      return(data.frame())
    }
    
    # Read CSV 
    raw_data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
    
    # Set proper column names
    colnames(raw_data) <- c("ID", "Year", "Council", "Activity", "Rest_of_Scotland", "Rural_Scotland", "All")
    
    # Filter for selected activity if specified
    if (!is.null(selected_activity)) {
      raw_data <- raw_data %>% filter(Activity == selected_activity)
    }
    
    # Remove any "Base" entries
    raw_data <- raw_data %>% filter(!grepl("Base", Activity, ignore.case = TRUE))
    
    # Process data - create records for Urban, Rural, Scotland
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      year <- as.numeric(raw_data$Year[i])
      if (is.na(year)) next
      
      activity <- raw_data$Activity[i]
      rest_scotland <- raw_data$Rest_of_Scotland[i]
      rural_scotland <- raw_data$Rural_Scotland[i]
      all_scotland <- raw_data$All[i]
      
      # Urban data
      if (!is.na(rest_scotland) && rest_scotland != "-" && rest_scotland != "*") {
        urban_row <- data.frame(
          Year = year,
          Activity = activity,
          Area = "Urban",
          Value = as.numeric(rest_scotland),
          Data_Source = "Scottish Household Survey - 2-fold",
          stringsAsFactors = FALSE
        )
        processed_data <- rbind(processed_data, urban_row)
      }
      
      # Rural data
      if (!is.na(rural_scotland) && rural_scotland != "-" && rural_scotland != "*") {
        rural_row <- data.frame(
          Year = year,
          Activity = activity,
          Area = "Rural",
          Value = as.numeric(rural_scotland),
          Data_Source = "Scottish Household Survey - 2-fold",
          stringsAsFactors = FALSE
        )
        processed_data <- rbind(processed_data, rural_row)
      }
      
      # Scotland total
      if (!is.na(all_scotland) && all_scotland != "-" && all_scotland != "*") {
        scotland_row <- data.frame(
          Year = year,
          Activity = activity,
          Area = "Scotland",
          Value = as.numeric(all_scotland),
          Data_Source = "Scottish Household Survey - 2-fold",
          stringsAsFactors = FALSE
        )
        processed_data <- rbind(processed_data, scotland_row)
      }
    }
    
    cat(paste("Loaded", nrow(processed_data), "transport 2-fold records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading transport 2-fold data:", e$message, "\n")
    return(data.frame())
  })
}

load_transport_satisfaction_6fold <- function() {
  filepath <- get_file_path("transport_satisfaction_6_fold.csv")
  cat("Loading transport satisfaction 6-fold data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    cat("Column names:", paste(colnames(raw_data), collapse = ", "), "\n")
    
    # Filter for satisfied responses only
    satisfied_data <- raw_data %>%
      filter(serv5g == "Satisfied") %>%
      filter(!is.na(Year), !is.na(Council))
    
    if (nrow(satisfied_data) == 0) {
      cat("No satisfied data found\n")
      return(data.frame())
    }
    
    # Process the data - handle column names without backticks
    processed_data <- satisfied_data %>%
      select(Year, Council, Large.urban.areas, Other.urban.areas, Accessible.small.towns, 
             Remote.small.towns, Accessible.rural, Remote.rural, All) %>%
      pivot_longer(cols = c(Large.urban.areas, Other.urban.areas, Accessible.small.towns, 
                            Remote.small.towns, Accessible.rural, Remote.rural), 
                   names_to = "Area", values_to = "Value") %>%
      mutate(
        Area = case_when(
          Area == "Large.urban.areas" ~ "Large Urban Areas",
          Area == "Other.urban.areas" ~ "Other Urban Areas", 
          Area == "Accessible.small.towns" ~ "Accessible Small Towns",
          Area == "Remote.small.towns" ~ "Remote Small Towns",
          Area == "Accessible.rural" ~ "Accessible Rural",
          Area == "Remote.rural" ~ "Remote Rural",
          TRUE ~ Area
        ),
        Value = case_when(
          Value %in% c("-", "*", "", "NA") ~ NA_real_,
          TRUE ~ as.numeric(Value)
        ),
        Data_Source = "Scottish Household Survey - Public Transport Satisfaction"
      ) %>%
      filter(!is.na(Value))
    
    # Add Scotland data from All column
    scotland_data <- satisfied_data %>%
      filter(!is.na(All), All != "-", All != "*") %>%
      select(Year, All) %>%
      mutate(
        Area = "Scotland",
        Value = as.numeric(All),
        Data_Source = "Scottish Household Survey - Public Transport Satisfaction"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source)
    
    # Combine both datasets
    final_data <- bind_rows(
      processed_data %>% select(Year, Area, Value, Data_Source),
      scotland_data
    )
    
    cat("Processed", nrow(final_data), "transport satisfaction records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading transport satisfaction data:", e$message, "\n")
    return(data.frame())
  })
}


# Road condition data - add Scotland aggregation
load_road_condition_data <- function() {
  filepath <- get_file_path("road_conditions.csv")
  cat("Loading road condition data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    
    if ("Indicator..road.network.traffic." %in% names(raw_data)) {
      names(raw_data)[names(raw_data) == "Indicator..road.network.traffic."] <- "Indicator"
    }
    
    filtered_data <- raw_data %>%
      filter(Indicator == "The Percentage Of Roads Needing Repairs (Red And Amber Classification) In Scotland") %>%
      filter(!is.na(DateCode), !is.na(Value), !is.na(FeatureName)) %>%
      mutate(
        Local_Authority = trimws(FeatureName),
        Year = as.numeric(DateCode),
        Value = as.numeric(Value)
      ) %>%
      filter(Year >= 2010, !is.na(Value))
    
    # Separate Scotland data
    scotland_data <- filtered_data %>%
      filter(Local_Authority == "Scotland") %>%
      mutate(
        Area = "Scotland",
        Data_Source = "Scottish Government - Road Condition"
      ) %>%
      select(Year, Area, Value, Data_Source)
    
    # Process non-Scotland data
    processed_data <- filtered_data %>%
      filter(Local_Authority != "Scotland") %>%
      mutate(
        Authority_Clean = case_when(
          Local_Authority == "City of Edinburgh" ~ "Edinburgh, City of",
          Local_Authority == "Edinburgh City" ~ "Edinburgh, City of",
          TRUE ~ Local_Authority
        ),
        Classification_4fold = case_when(
          Authority_Clean %in% resas_council_mapping_transport[["Islands & Remote Rural"]] ~ "Islands & Remote Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Mainly Rural"]] ~ "Mainly Rural", 
          Authority_Clean %in% resas_council_mapping_transport[["Urban with Substantial Rural"]] ~ "Urban with Substantial Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Larger Cities"]] ~ "Larger Cities",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Classification_4fold)) %>%
      group_by(Year, Classification_4fold) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(
        Area = Classification_4fold,
        Data_Source = "Scottish Government - Road Condition"
      ) %>%
      select(Year, Area, Value, Data_Source)
    
    # Combine with Scotland data
    final_data <- bind_rows(processed_data, scotland_data)
    
    cat("Processed", nrow(final_data), "road condition records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading road condition data:", e$message, "\n")
    return(data.frame())
  })
}
# Replace your load_ev_infrastructure_data function with this fixed version:

load_ev_infrastructure_data <- function(classification_type = "4-fold") {
  filepath <- get_file_path("ev_infrastructure.xlsx")
  cat("Loading EV infrastructure data from:", filepath, "\n")
  
  tryCatch({
    if (!file.exists(filepath)) {
      cat("EV infrastructure file not found at:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file with headers
    raw_data <- read_excel(filepath, col_names = TRUE, skip = 0)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    if (nrow(raw_data) == 0) {
      cat("No data found in Excel file\n")
      return(data.frame())
    }
    
    # Get column names - first two are authority code and name
    all_cols <- colnames(raw_data)
    authority_code_col <- all_cols[1]
    authority_name_col <- all_cols[2]
    date_cols <- all_cols[3:length(all_cols)]
    
    cat("Authority columns:", authority_code_col, "and", authority_name_col, "\n")
    cat("Date columns found:", length(date_cols), "\n")
    cat("Sample date columns:", paste(date_cols[1:min(5, length(date_cols))], collapse = ", "), "\n")
    
    # Group date columns by year to find the last column for each year
    year_to_latest_col <- list()
    
    for (col_name in date_cols) {
      # Extract year from column name (e.g., "Oct-19" -> "19", "Apr-25" -> "25")
      year_match <- str_extract(col_name, "\\d{2}$")
      
      if (!is.na(year_match)) {
        # Convert 2-digit year to 4-digit
        year_2digit <- as.numeric(year_match)
        year_4digit <- ifelse(year_2digit >= 19, 2000 + year_2digit, 2000 + year_2digit)
        year_key <- as.character(year_4digit)
        
        # Store the latest (rightmost) column for each year
        year_to_latest_col[[year_key]] <- col_name
      }
    }
    
    cat("Years and their latest columns:\n")
    for (year in names(year_to_latest_col)) {
      cat("  ", year, ":", year_to_latest_col[[year]], "\n")
    }
    
    processed_data <- data.frame()
    
    # Process each authority (each row)
    for (i in 1:nrow(raw_data)) {
      authority_code <- as.character(raw_data[i, authority_code_col])
      authority_name <- as.character(raw_data[i, authority_name_col])
      
      # Skip empty rows and Scotland totals (we'll calculate Scotland separately)
      if (is.na(authority_code) || is.na(authority_name) || 
          authority_name == "" || authority_name == "Scotland") {
        next
      }
      
      # Process each year using its latest available column
      for (year_str in names(year_to_latest_col)) {
        latest_col <- year_to_latest_col[[year_str]]
        year_value <- as.numeric(raw_data[i, latest_col])
        
        # Only include valid positive values
        if (!is.na(year_value) && year_value > 0) {
          processed_data <- rbind(processed_data, data.frame(
            Year = as.numeric(year_str),
            Local_Authority = trimws(authority_name),
            Authority_Code = authority_code,
            Value = year_value,
            Source_Column = latest_col,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    cat("Extracted", nrow(processed_data), "raw EV infrastructure records\n")
    
    if (nrow(processed_data) == 0) {
      cat("No valid data points extracted\n")
      return(data.frame())
    }
    
    # Clean authority names and map to RESAS classifications
    classified_data <- processed_data %>%
      mutate(
        Authority_Clean = case_when(
          Local_Authority == "City of Edinburgh" ~ "Edinburgh, City of",
          Local_Authority == "Edinburgh City" ~ "Edinburgh, City of",
          Local_Authority == "Argyll & Bute" ~ "Argyll and Bute",
          Local_Authority == "Dumfries & Galloway" ~ "Dumfries and Galloway", 
          Local_Authority == "Perth & Kinross" ~ "Perth and Kinross",
          Local_Authority == "Na h-Eileanan Siar" ~ "Na h-Eileanan Siar",
          TRUE ~ trimws(Local_Authority)
        ),
        Classification_4fold = case_when(
          Authority_Clean %in% resas_council_mapping_transport[["Islands & Remote Rural"]] ~ "Islands & Remote Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Mainly Rural"]] ~ "Mainly Rural", 
          Authority_Clean %in% resas_council_mapping_transport[["Urban with Substantial Rural"]] ~ "Urban with Substantial Rural",
          Authority_Clean %in% resas_council_mapping_transport[["Larger Cities"]] ~ "Larger Cities",
          TRUE ~ NA_character_
        ),
        Classification_2fold = case_when(
          Classification_4fold %in% c("Islands & Remote Rural", "Mainly Rural") ~ "Rural",
          Classification_4fold %in% c("Urban with Substantial Rural", "Larger Cities") ~ "Urban",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(Classification_4fold))
    
    cat("Classified", nrow(classified_data), "data points into RESAS categories\n")
    
    if (nrow(classified_data) == 0) {
      cat("No data points could be classified into RESAS categories\n")
      return(data.frame())
    }
    
    
    if (classification_type == "2-fold") {
      # 2-fold classification averages
      fold2_data <- classified_data %>%
        group_by(Year, Classification_2fold) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = Classification_2fold,
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      # Scotland average (calculated from all local authorities)
      scotland_data <- classified_data %>%
        group_by(Year) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = "Scotland",
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      final_data <- bind_rows(fold2_data, scotland_data)
      
    } else { 
      # 4-fold classification averages  
      fold4_data <- classified_data %>%
        group_by(Year, Classification_4fold) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = Classification_4fold,
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      # Scotland average (calculated from all local authorities)
      scotland_data <- classified_data %>%
        group_by(Year) %>%
        summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
        mutate(
          Area = "Scotland",
          Data_Source = "Scottish Government - EV Infrastructure"
        ) %>%
        select(Year, Area, Value, Data_Source)
      
      final_data <- bind_rows(fold4_data, scotland_data)
    }
    
    cat("Final processed data:", nrow(final_data), "records\n")
    cat("Years available:", paste(sort(unique(final_data$Year)), collapse = ", "), "\n")
    cat("Areas available:", paste(sort(unique(final_data$Area)), collapse = ", "), "\n")
    
    # Show sample of processed data
    if (nrow(final_data) > 0) {
      cat("Sample of final data:\n")
      print(head(final_data))
    }
    
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading EV infrastructure data:", e$message, "\n")
    cat("Full error traceback:\n")
    print(e)
    return(data.frame())
  })
}

# Load MIS travel costs data (3-fold UK)
load_mis_travel_costs_data <- function(selected_budget = NULL) {
  filepath <- get_file_path("mis.csv")
  cat("Loading MIS travel costs data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    expected_cols <- c("Region", "MIS budget", "2021", "2022")
    if (!all(expected_cols %in% colnames(raw_data))) {
      stop("Unexpected column names in mis.csv")
    }
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(`MIS budget`)) %>%
      mutate(
        Region = trimws(Region),
        `MIS budget` = trimws(`MIS budget`)
      ) %>%
      rename(MIS_Budget = `MIS budget`) %>%
      pivot_longer(cols = c("2021", "2022"), names_to = "Year", values_to = "Value") %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub("[£,]", "", Value)),
        Area = Region,
        Data_Source = "Scottish Government - MIS Travel Costs",
        Sub_Metric = MIS_Budget
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    if (!is.null(selected_budget) && selected_budget != "") {
      processed_data <- processed_data %>%
        filter(Sub_Metric == trimws(selected_budget))
    }
    
    cat("Processed", nrow(processed_data), "MIS travel cost records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading MIS travel costs data:", e$message, "\n")
    return(data.frame())
  })
}

# Load key service access data (3-fold only)
load_key_service_access_data <- function(selected_service = NULL) {
  filepath <- get_file_path("15min_drive.csv")
  cat("Loading key service access data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    expected_cols <- c("Region", "Service", "2006", "2009", "2012", "2016", "2020")
    if (!all(expected_cols %in% colnames(raw_data))) {
      stop("Unexpected column names in 15min_drive.csv")
    }
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Service)) %>%
      mutate(
        Region = trimws(Region),
        Service = trimws(Service)
      ) %>%
      pivot_longer(cols = c("2006", "2009", "2012", "2016", "2020"), names_to = "Year", values_to = "Value") %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub("%", "", Value)),
        Area = case_when(
          Region == "Rest of Scotland" ~ "Urban",
          Region == "Accessible Rural" ~ "Accessible Rural",
          Region == "Remote Rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Key Service Access",
        Sub_Metric = Service
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    if (!is.null(selected_service) && selected_service != "") {
      processed_data <- processed_data %>%
        filter(Sub_Metric == trimws(selected_service))
    }
    
    cat("Processed", nrow(processed_data), "key service access records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading key service access data:", e$message, "\n")
    return(data.frame())
  })
}

# Load ferry reliability data
load_ferry_reliability_data <- function(selected_measure = NULL) {
  filepath <- get_file_path("ferry_reliability.csv")
  cat("Loading ferry reliability data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    year_columns <- colnames(raw_data)[3:ncol(raw_data)]
    
    processed_data <- raw_data %>%
      filter(!is.na(Operator), !is.na(Measure)) %>%
      mutate(
        Operator = trimws(Operator),
        Measure = trimws(Measure)
      ) %>%
      pivot_longer(cols = all_of(year_columns), names_to = "Year_Range", values_to = "Value") %>%
      mutate(
        Year = as.character(Year_Range),
        Value = as.numeric(Value),
        Area = case_when(
          Operator == "CalMac" & grepl("Reliability", Measure) ~ "CalMac Reliability",
          Operator == "CalMac" & grepl("Punctuality", Measure) & !grepl("-", Measure) ~ "CalMac Punctuality",
          Operator == "NorthLink" & grepl("Aberdeen", Measure) ~ "NorthLink Punctuality - Aberdeen routes",
          Operator == "NorthLink" & grepl("Pentland", Measure) ~ "NorthLink Punctuality - Pentland Firth",
          TRUE ~ paste(Operator, Measure)
        ),
        Data_Source = "Scottish Government - Ferry Reliability",
        Sub_Metric = Measure
      ) %>%
      filter(!is.na(Value)) %>%
      mutate(
        Year_Numeric = as.numeric(paste0("20", substr(Year_Range, 6, 7)))
      ) %>%
      select(Year = Year_Numeric, Area, Value, Data_Source, Sub_Metric, Year_Range)
    
    cat("Processed", nrow(processed_data), "ferry reliability records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading ferry reliability data:", e$message, "\n")
    return(data.frame())
  })
}

# Main data loading function 
load_transport_data_simple <- function(metric_name, classification_type, selected_activity = NULL) {
  metric_info <- transport_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  # Handle each metric type
  if (metric_name == "Public transport satisfaction") {
    if (classification_type == "6-fold") {
      return(load_transport_satisfaction_6fold())
    } else {
      cat("Only 6-fold classification available for Public transport satisfaction\n")
      return(data.frame())
    }
    
  } else if (metric_name == "Transport affordability") {
    if (classification_type == "2-fold") {
      # Check for 2-fold file first
      file_2fold <- get_file_path("easy_to_afford_transport_2_fold.csv")
      if (file.exists(file_2fold)) {
        return(load_transport_2fold_data_simple(file_2fold, selected_activity))
      } else {
        # Show coming soon message
        cat("2-fold file not available for Transport affordability - showing coming soon\n")
        return(data.frame(Message = "Coming Soon"))
      }
    } else if (classification_type == "6-fold") {
      # Load from existing 6-fold file
      return(load_transport_affordability_6fold())
    } else if (classification_type == "3-fold") {
      # Create 3-fold from 6-fold (if available)
      data_6fold <- load_transport_affordability_6fold()
      if (nrow(data_6fold) > 0) {
        return(create_transport_3fold_simple(data_6fold))
      }
    }
    
  } else if (metric_name == "Mode of transport (work)") {
    if (classification_type == "2-fold") {
      # Check for 2-fold file first
      file_2fold <- get_file_path("mode_of_transport_2_fold.csv")
      if (file.exists(file_2fold)) {
        return(load_transport_2fold_data_simple(file_2fold, selected_activity))
      } else {
        # Show coming soon message
        cat("2-fold file not available for Mode of transport - showing coming soon\n")
        return(data.frame(Message = "Coming Soon"))
      }
    } else if (classification_type == "6-fold") {
      return(load_mode_of_transport_6fold(selected_activity))
    } else if (classification_type == "3-fold") {
      # Create 3-fold from 6-fold
      data_6fold <- load_mode_of_transport_6fold(selected_activity)
      if (nrow(data_6fold) > 0) {
        return(create_transport_3fold_simple(data_6fold))
      }
    }
    
  } else if (metric_name == "Road Condition") {
    if (classification_type %in% c("2-fold", "4-fold")) {
      return(load_road_condition_data())
    }
    
  } else if (metric_name == "EV Infrastructure") {
    if (classification_type %in% c("2-fold", "4-fold")) {
      return(load_ev_infrastructure_data(classification_type))  # Pass the classification_type
    }
    
  } else if (metric_name == "Weekly travel costs") {
    if (classification_type == "3-fold (UK)") {
      return(load_mis_travel_costs_data(selected_activity))
    }
    
  } else if (metric_name == "Key service access") {
    if (classification_type == "3-fold") {
      return(load_key_service_access_data(selected_activity))
    }
    
  } else if (metric_name == "Ferry reliability") {
    if (classification_type == "ferry operator") {
      return(load_ferry_reliability_data(selected_activity))
    }
  }
  
  return(data.frame())
}

# Transport affordability
load_transport_affordability_6fold <- function() {
  filepath <- get_file_path("easy_to_afford_transport.csv")
  cat("Loading transport affordability 6-fold data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    cat("Unique regions:", paste(unique(raw_data$Region), collapse = ", "), "\n")
    
    # Separate Scotland data
    scotland_data <- raw_data %>%
      filter(grepl("Scotland|All Scotland|Scotland \\(All\\)", Region, ignore.case = TRUE)) %>%
      mutate(
        Area = "Scotland",
        Value = as.numeric(Affordability),
        Data_Source = "Scottish Household Survey - Transport Affordability"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source)
    
    # Process non-Scotland data
    clean_data <- raw_data %>%
      filter(!grepl("Scotland|All Scotland|Scotland \\(All\\)", Region, ignore.case = TRUE)) %>%
      mutate(
        Region = trimws(Region),
        Year = as.numeric(Year),
        Affordability = as.numeric(Affordability)
      ) %>%
      filter(!is.na(Region), !is.na(Year), !is.na(Affordability))
    
    processed_data <- clean_data %>%
      mutate(
        Area = case_when(
          grepl("Large urban|Large Urban", Region, ignore.case = TRUE) ~ "Large Urban Areas",
          grepl("Other urban|Other Urban", Region, ignore.case = TRUE) ~ "Other Urban Areas",
          grepl("Small accessible|Accessible small", Region, ignore.case = TRUE) ~ "Accessible Small Towns",
          grepl("Small remote|Remote small", Region, ignore.case = TRUE) ~ "Remote Small Towns",
          grepl("Accessible rural", Region, ignore.case = TRUE) ~ "Accessible Rural",
          grepl("Remote rural", Region, ignore.case = TRUE) ~ "Remote Rural",
          TRUE ~ Region
        ),
        Value = as.numeric(Affordability),
        Data_Source = "Scottish Household Survey - Transport Affordability"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Area, Value, Data_Source)
    
    # Combine with Scotland data
    final_data <- bind_rows(processed_data, scotland_data) |> 
      filter(Year != 2021) # remove 2021 data as not comparable
    
    cat("Processed", nrow(final_data), "transport affordability records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading transport affordability 6-fold data:", e$message, "\n")
    return(data.frame())
  })
}

# Mode of transport
load_mode_of_transport_6fold <- function(selected_mode = NULL) {
  filepath <- get_file_path("mode_of_transport.csv")
  cat("Loading mode of transport 6-fold data from:", filepath, "\n")
  
  tryCatch({
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    # Clean column names
    names(raw_data) <- gsub("\r", "", names(raw_data))
    
    # Separate Scotland data
    scotland_data <- raw_data %>%
      filter(grepl("All Scotland|Scotland", Region, ignore.case = TRUE)) %>%
      mutate(
        Region = gsub("\r", "", Region),
        Region = trimws(Region),
        Mode = gsub("\r", "", `Mode of Travel`),
        Mode = trimws(Mode)
      ) %>%
      gather(key = "Year", value = "Value", -Region, -`Mode of Travel`, -Mode) %>%
      mutate(
        Year = as.numeric(Year),
        Value = case_when(
          Value == "#N/A" ~ NA_real_,
          Value == "" ~ NA_real_,
          TRUE ~ as.numeric(Value)
        )
      ) %>%
      filter(!is.na(Year), !is.na(Value)) %>%
      mutate(
        Area = "Scotland",
        Data_Source = "Scottish Household Survey - Mode of Transport to Work",
        Sub_Metric = Mode
      ) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    # Process non-Scotland data
    processed_data <- raw_data %>%
      filter(!grepl("All Scotland|Scotland", Region, ignore.case = TRUE)) %>%
      mutate(
        Region = gsub("\r", "", Region),
        Region = trimws(Region),
        Mode = gsub("\r", "", `Mode of Travel`),
        Mode = trimws(Mode)
      ) %>%
      gather(key = "Year", value = "Value", -Region, -`Mode of Travel`, -Mode) %>%
      mutate(
        Year = as.numeric(Year),
        Value = case_when(
          Value == "#N/A" ~ NA_real_,
          Value == "" ~ NA_real_,
          TRUE ~ as.numeric(Value)
        )
      ) %>%
      filter(!is.na(Year), !is.na(Value)) %>%
      mutate(
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban" ~ "Other Urban Areas",
          Region == "Small accessible towns" ~ "Accessible Small Towns",
          Region == "Small remote towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Household Survey - Mode of Transport to Work",
        Sub_Metric = Mode
      ) %>%
      filter(!is.na(Area)) %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    # Combine processed data with Scotland data
    final_data <- bind_rows(processed_data, scotland_data)
    
    if (!is.null(selected_mode) && selected_mode != "") {
      final_data <- final_data %>%
        filter(Sub_Metric == selected_mode)
    }
    
    cat("Processed", nrow(final_data), "mode of transport records\n")
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading mode of transport 6-fold data:", e$message, "\n")
    return(data.frame())
  })
}


# Create 3-fold from 6-fold data 
create_transport_3fold_simple <- function(data_6fold) {
  cat("Creating transport 3-fold combination\n")
  
  if(nrow(data_6fold) == 0) {
    return(data.frame())
  }
  
  # Urban from 6-fold (4 urban areas combined)
  urban_data <- data_6fold %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Rural areas from 6-fold (keep separate)
  rural_data <- data_6fold %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural"))
  
  # Combine
  combined_3fold <- bind_rows(urban_data, rural_data)
  
  cat(paste("Created", nrow(combined_3fold), "transport 3-fold records\n"))
  return(combined_3fold)
}
#  UI and Helper Functions

# Helper functions for processing and colors
simple_aggregate_transport_data <- function(processed_data, classification_type = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  # Handle both "Area" and "Area_Type" column names
  if ("Area_Type" %in% names(processed_data) && !("Area" %in% names(processed_data))) {
    cat("Converting Area_Type to Area column\n")
    processed_data <- processed_data %>%
      mutate(Area = Area_Type) %>%
      select(-Area_Type)
  }
  
  # Handle RESAS 4-fold to 2-fold aggregation
  if(classification_type == "2-fold" && "Larger Cities" %in% unique(processed_data$Area)) {
    cat("Creating 2-fold aggregation from 4-fold RESAS data\n")
    
    # Urban (Larger Cities + Urban with Substantial Rural)
    urban_data <- processed_data %>%
      filter(Area %in% c("Larger Cities", "Urban with Substantial Rural")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Urban")
    
    # Rural (Mainly Rural + Islands & Remote Rural)
    rural_data <- processed_data %>%
      filter(Area %in% c("Mainly Rural", "Islands & Remote Rural")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Rural")
    
    # Include Scotland if it exists
    scotland_data <- processed_data %>%
      filter(Area == "Scotland") %>%
      select(Year, Area, Value, Data_Source)
    
    return(bind_rows(urban_data, rural_data, scotland_data))
  }
  
  
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}
# Get key insights
get_transport_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold", selected_activity = NULL) {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  # ALWAYS try to get Scotland data directly if it exists
  scotland_direct <- latest_data %>% filter(Area == "Scotland")
  if (nrow(scotland_direct) > 0) {
    scotland_val <- scotland_direct$Value[1]
  }
  
  # Handle different area types for urban/rural
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    
  } else if (any(c("Larger Cities", "Urban with Substantial Rural", "Mainly Rural", "Islands & Remote Rural") %in% unique_areas)) {
    # RESAS 4-fold aggregation
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
    
    # If we don't have direct Scotland data, calculate it from all areas
    if (is.na(scotland_val)) {
      all_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(all_areas) > 0) {
        scotland_val <- mean(all_areas$Value, na.rm = TRUE)
      }
    }
    
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
    # 6-fold aggregation
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns"))
    if(nrow(urban_areas) > 0) {
      urban_val <- mean(urban_areas$Value, na.rm = TRUE)
    }
    
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
  }
  
  return(list(
    urban = ifelse(!is.na(urban_val), urban_val, NA),
    rural = ifelse(!is.na(rural_val), rural_val, NA),
    scotland = ifelse(!is.na(scotland_val), scotland_val, NA),
    year = latest_year
  ))
}

# Get colors 
get_transport_colors <- function(areas, classification_type, metric_name = NULL) {
  color_mapping <- list()
  
  # Special handling for metrics that keep original colors
  if (!is.null(metric_name)) {
    if (metric_name == "Weekly travel costs") {
      return(list()) # Keep original viridis colors
    }
    if (metric_name == "Ferry reliability") {
      return(list()) # Keep original viridis colors
    }
  }
  
  # Use culture module color scheme for other metrics
  if (classification_type == "6-fold") {
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    }
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
  } else if (classification_type == "4-fold") {
    color_mapping <- list("Larger Cities" = "#FDBE41", "Urban with Substantial Rural" = "#F4E470", 
                          "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")

  }
  
  # Always set Scotland to gray
  color_mapping[["Scotland"]] <- "#B2B2B2"
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display (matching culture module)
format_transport_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name == "Weekly travel costs") {
    return(paste0("£", round(value, 1)))
  } else if (metric_name == "EV Infrastructure") {
    return(round(value, 1))
  } else {
    return(paste0(round(value, 1), "%"))
  }
}

# Calculate gap between urban and rural (matching culture module)
calculate_transport_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name == "Weekly travel costs") {
    return(paste0("£", round(gap, 1)))
  } else if (metric_name == "EV Infrastructure") {
    return(round(gap, 1))
  } else {
    return(paste0(round(gap, 1), "pp"))
  }
}
# Transport Dashboard UI
transport_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      /* STANDARDIZED CSS FOR ALL DASHBOARD MODULES - EXACTLY SAME AS CULTURE */
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
      .transport-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .transport-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .transport-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .transport-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .transport-metric-description {
        color: #6c757d !important;
        font-size: 0.95em !important;
        line-height: 1.4 !important;
        margin-left: 30px !important;
      }
      "))
    ),
    
    # Main header layout
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
            h2("Transport", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("transport_classification_selector")
          ),
          
          # Top-center-right: Transport metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("transport_metric", "Transport Metric:", 
                        choices = c("Select a policy metric..." = "", names(transport_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (when applicable)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.transport_metric == 'Weekly travel costs'",
              selectInput("mis_budget", "Sub-metric:", 
                          choices = names(mis_budget_sub_metrics), selected = names(mis_budget_sub_metrics)[1], width = "180px")
            ),
            conditionalPanel(
              condition = "input.transport_metric == 'Key service access'",
              selectInput("service", "Sub-metric:", 
                          choices = names(key_service_sub_metrics), selected = names(key_service_sub_metrics)[1], width = "180px")
            ),
            conditionalPanel(
              condition = "input.transport_metric == 'Mode of transport (work)'",
              selectInput("mode", "Sub-metric:", 
                          choices = names(transport_mode_sub_metrics), selected = names(transport_mode_sub_metrics)[1], width = "180px")
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
    
    # UI SECTION
    
    # Show content only when a metric is selected
    conditionalPanel(
      condition = "input.transport_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore transport data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Transport-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Public transport satisfaction
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Public transport satisfaction', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("bus", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Public transport satisfaction"),
                      div(class = "transport-metric-description", 
                          "Satisfaction with the quality of public transport across Scotland's urban and rural areas.")
                    )
                  )
                ),
                
                # Road Condition
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Road Condition', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("road", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Road Condition"),
                      div(class = "transport-metric-description",
                          "Percentage of roads needing repairs across Scotland's local authorities.")
                    )
                  )
                ),
                
                # EV Infrastructure
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'EV Infrastructure', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("charging-station", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "EV Infrastructure"),
                      div(class = "transport-metric-description",
                          "Electric vehicle charging devices per 100,000 population by local authority.")
                    )
                  )
                ),
                
                # Weekly travel costs
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Weekly travel costs', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("pound-sign", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Weekly travel costs"),
                      div(class = "transport-metric-description",
                          "Weekly travel costs in different Minimum Income Standard (MIS) budgets across UK regions.")
                    )
                  )
                ),
                
                # Key service access
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Key service access', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("map-marker-alt", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Key service access"),
                      div(class = "transport-metric-description",
                          "Percentage of population within 15 minute drive time by public transport of key services.")
                    )
                  )
                ),
                
                # Transport affordability
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Transport affordability', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("wallet", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Transport affordability"),
                      div(class = "transport-metric-description",
                          "How easy people find it to afford transport costs across Scotland.")
                    )
                  )
                ),
                
                # Mode of transport (work)
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Mode of transport (work)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("car", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Mode of transport (work)"),
                      div(class = "transport-metric-description",
                          "How adults usually travel to work showing percentages by different transport modes.")
                    )
                  )
                ),
                
                # Ferry reliability
                div(
                  class = "transport-metrics-card",
                  onclick = "Shiny.setInputValue('transport_metric_select', 'Ferry reliability', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("ship", class = "transport-metric-icon"),
                    div(
                      div(class = "transport-metric-title", "Ferry reliability"),
                      div(class = "transport-metric-description",
                          "Percentage of Scottish lifeline ferry services that are reliable and punctual by operator.")
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
      condition = "input.transport_metric != ''",
      
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("transport_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("transport_data_summary"))
          ),
          
          # Show key insights for metrics that support them (EXCLUSION approach)
          conditionalPanel(
            condition = "input.transport_metric == 'EV Infrastructure'",
            fluidRow(
              box(title = uiOutput("transport_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("transport_urban_rate", width = 3),
                    valueBoxOutput("transport_rural_rate", width = 3),
                    valueBoxOutput("transport_scotland_rate", width = 3),
                    valueBoxOutput("transport_urban_rural_gap", width = 3)
                  ))
            )
          )
          
      ),
      
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("transport_trend_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("transport_trend_download", "Download", class = "excel-download-btn"
                            ))),
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("transport_trend_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("transport_comparison_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("transport_comparison_download", "Download", class = "excel-download-btn"
                            ))),
            status = "primary", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;", uiOutput("transport_year_selector")),
            plotlyOutput("transport_comparison_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("transport_table_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("transport_table_download", "Download", class = "excel-download-btn"
                            ))),
            status = "info", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("transport_table_year_filter")),
                  column(6, uiOutput("transport_table_area_filter"))
                )),
            DT::dataTableOutput("transport_data_table") %>% withSpinner())
      )
    )
  )
}
# Transport module server
transport_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  transport_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards (matching culture module)
  observeEvent(input$transport_metric_select, {
    updateSelectInput(session, "transport_metric", selected = input$transport_metric_select)
  })
  
  # Dynamic UI outputs (matching culture module)
  output$transport_summary_title <- renderUI({
    req(input$transport_metric, input$transport_classification_type)
    display_name <- get_transport_metric_display_name(input$transport_metric)
    classification_text <- case_when(
      input$transport_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$transport_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$transport_classification_type == "3-fold (UK)" ~ "(UK Regions)",
      input$transport_classification_type == "4-fold" ~ "(4-fold RESAS)",
      input$transport_classification_type == "6-fold" ~ "(6-fold Classification)",
      input$transport_classification_type == "ferry operator" ~ "(Ferry Operator)",
      TRUE ~ paste0("(", input$transport_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$transport_year_selector <- renderUI({
    req(transport_values$processed_data)
    if (nrow(transport_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(transport_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("transport_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$transport_classification_selector <- renderUI({
    req(input$transport_metric)
    available_classifications <- transport_metrics[[input$transport_metric]]$classifications
    choices <- list()
    
    # Special handling for Weekly travel costs
    if (input$transport_metric == "Weekly travel costs") {
      if("3-fold (UK)" %in% available_classifications) choices[["UK Regions"]] <- "3-fold (UK)"
    } else {
      if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
      if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
      if("ferry operator" %in% available_classifications) choices[["Ferry Operator"]] <- "ferry operator"
    }
    
    # Open the most complex fold first 
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("transport_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles 
  output$transport_trend_title <- renderUI({
    req(input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste("Trend Analysis for", display_name)
  })
  
  output$transport_comparison_title <- renderUI({
    req(input$transport_selected_year, input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste0("Single Year Comparison (", input$transport_selected_year, ") for ", display_name)
  })
  
  output$transport_table_title <- renderUI({
    req(input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste("Data Table for", display_name)
  })
  
  output$transport_key_insights_title <- renderUI({
    req(input$transport_metric)
    display_name <- ifelse(
      input$transport_metric == "Transport affordability",
      "How easy (very easy or fairly easy) people find it to afford transport costs",
      ifelse(
        input$transport_metric == "Public transport satisfaction",
        "Percentage of people very or fairly satisfied with the quality of public transport",
        get_transport_metric_display_name(input$transport_metric)
      )
    )
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$transport_table_year_filter <- renderUI({
    req(transport_values$processed_data)
    if(nrow(transport_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(transport_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("transport_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$transport_table_area_filter <- renderUI({
    req(transport_values$processed_data, input$transport_classification_type)
    if(nrow(transport_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("transport_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Process data reactively
  observe({
    if (is.null(input$transport_metric) || input$transport_metric == "") {
      transport_values$processed_data <- data.frame()
      transport_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$transport_classification_type)
    
    # Use simplified data loading with sub-metrics
    if (input$transport_metric == "Weekly travel costs") {
      req(input$mis_budget)
      selected_budget <- mis_budget_sub_metrics[input$mis_budget]
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type, 
        selected_budget
      )
    } else if (input$transport_metric == "Key service access") {
      req(input$service)
      selected_service <- key_service_sub_metrics[input$service]
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type, 
        selected_service
      )
    } else if (input$transport_metric == "Mode of transport (work)") {
      req(input$mode)
      selected_mode <- transport_mode_sub_metrics[input$mode]
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type, 
        selected_mode
      )
    } else {
      transport_values$processed_data <- load_transport_data_simple(
        input$transport_metric, 
        input$transport_classification_type
      )
    }
    
    transport_values$data_status <- if(nrow(transport_values$processed_data) > 0) "Transport data loaded" else "No data available"
  })
  
  # Data summary 
    output$transport_data_summary <- renderUI({
    req(transport_values$processed_data, input$transport_classification_type, input$transport_metric)
    
    if (nrow(transport_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_transport_metric_display_name(input$transport_metric)
    
    custom_insight <- transport_key_insights[[input$transport_metric]]
    custom_notes <- transport_notes[[input$transport_metric]]
    
    # Define source information
    source_info <- switch(input$transport_metric,
                          "Public transport satisfaction" = list(
                            text = "Scottish Household Survey",
                            url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
                          ),
                          "Road Condition" = list(
                            text = "SCOTS road condition surveys", 
                            url = "https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Froad-network-traffic"
                          ),
                          "EV Infrastructure" = list(
                            text = "Zapmap EV infrastructure data",
                            url = "https://www.gov.uk/government/collections/electric-vehicle-charging-infrastructure-statistics"
                          ),
                          "Weekly travel costs" = list(
                            text = "The Cost of Remoteness, MIS 2022",
                            url = "https://www.gov.scot/publications/the-cost-of-remoteness-reflecting-higher-living-costs-in-remote-rural-scotland-when-measuring-fuel-poverty/"
                          ),
                          "Key service access" = list(
                            text = "Scottish Index of Multiple Deprivation 2006, 2009, 2012, 2016, 2020",
                            url = "https://www.gov.scot/collections/scottish-index-of-multiple-deprivation-2020/"
                          ),
                          "Transport affordability" = list(
                            text = "Scottish Household Survey (Transport and Travel in Scotland)",
                            url = "https://www.transport.gov.scot/search-results/?q=transport+and+travel+in+scotland&type=publication"
                          ),
                          "Mode of transport (work)" = list(
                            text = "Scottish Household Survey (Transport and Travel in Scotland)",
                            url = "https://www.transport.gov.scot/search-results/?q=transport+and+travel+in+scotland&type=publication"
                          ),
                          "Ferry reliability" = list(
                            text = "Transport Scotland water transport statistics",
                            url = "https://www.transport.gov.scot/publication/scottish-transport-statistics-2023/chapter-9-water-transport/"
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
    } else {
      # For metrics without custom insights (managing financially and influence over local decisions)
      return(
        div(class = "comparison-highlight",
            style = "background: linear-gradient(135deg, #e3f2fd, #f0f8ff); border-left: 4px solid #2196f3; padding: 20px; border-radius: 8px;",
            div(style = "display: flex; align-items: flex-start; gap: 15px;",
                icon("info-circle", style = "font-size: 1.5em; color: #2196f3; margin-top: 3px;"),
                div(
                  h4("Data Loaded", style = "margin: 0 0 10px 0; color: #1976d2; font-weight: 600;"),
                  p(paste("Showing data for:", display_name), style = "font-size: 1.05em; line-height: 1.6; margin: 0 0 15px 0; color: #1976d2;"),
                  hr(style = "border-top: 1px solid #bbdefb; margin: 15px 0 10px 0;"),
                  div(
                    style = "font-size: 0.9em; color: #1976d2;",
                    strong("Source: ", style = "color: #1976d2;"),
                    tags$a(
                      href = source_info$url,
                      target = "_blank",
                      source_info$text,
                      style = "color: #007bff; text-decoration: none; font-weight: 500;"
                    )
                  )
                )
            )
        )
      )
    }
  })

  # Value boxes with special handling for different metrics
  output$transport_urban_rate <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    # Skip for metrics that don't support urban/rural comparison
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
      year <- ""
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- format_transport_value(key_insights$urban, input$transport_metric)
      year <- if(!is.na(key_insights$year)) key_insights$year else ""
    }
    
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
  
  output$transport_rural_rate <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
      year <- ""
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- format_transport_value(key_insights$rural, input$transport_metric)
      year <- if(!is.na(key_insights$year)) key_insights$year else ""
    }
    
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
  
  # Scotland rate with special handling for EV Infrastructure  
  output$transport_scotland_rate <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
      year <- ""
      subtitle <- "Scotland Total"
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- format_transport_value(key_insights$scotland, input$transport_metric)
      year <- if(!is.na(key_insights$year)) key_insights$year else ""
      
      # Special case for EV Infrastructure - show as "Scotland Average"
      if (input$transport_metric == "EV Infrastructure") {
        subtitle <- paste("Scotland Average", year)
      } else {
        subtitle <- paste("Scotland Total", year)
      }
    }
    
   # valueBox(value = val, subtitle = subtitle, icon = icon("flag"), color = "maroon")
    
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", subtitle),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("flag"),
      color = "aqua"
    )
    
  })
  
  output$transport_urban_rural_gap <- renderValueBox({
    req(transport_values$processed_data, input$transport_metric)
    
    if (input$transport_metric %in% c("Weekly travel costs", "Ferry reliability", "Key service access", "Public transport satisfaction")) {
      val <- "N/A"
    } else {
      key_insights <- get_transport_key_insights(transport_values$processed_data, input$transport_metric, "2-fold")
      val <- calculate_transport_gap(key_insights$urban, key_insights$rural, input$transport_metric)
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
  
  # Trend chart (matching culture module with 45-degree year labels and Scotland in grey)
  output$transport_trend_chart <- renderPlotly({
    req(transport_values$processed_data, input$transport_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      # Format data for display
      is_cost_data <- input$transport_metric == "Weekly travel costs"
      is_ev_data <- input$transport_metric == "EV Infrastructure"
      
      if (is_cost_data) {
        agg_data$Value_Display <- paste0("£", round(agg_data$Value, 1))
        y_label <- "Weekly Cost (£)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Cost: ", agg_data$Value_Display)
      } else if (is_ev_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "EV Charging per 100k"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>EV Charging: ", agg_data$Value_Rounded)
      } 
      else if (input$transport_metric == "Public transport satisfaction")  {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage very or fairly satisfied (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      else if (input$transport_metric == "Ferry reliability")  {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", paste0(agg_data$Year, "-", substr(agg_data$Year + 1, 3,4)), "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text
      
      # Create plot
      if (input$transport_metric == "Ferry reliability") {
        p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            },
            labels = function(x) {
              paste0(x - 1, "-", substr(x, 3, 4))  # e.g., 2022 becomes "2021-22"
            }
          )
        
      } else if (input$transport_metric == "Mode of transport (work)") {
        agg_data1 <- agg_data |> filter(Year < 2020)
        agg_data2 <- agg_data |> filter(Year >= 2022)
        
        p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_vline(xintercept = 2019, linetype = "dashed", color = "darkgrey", size = 0.5) +
          geom_vline(xintercept = 2022, linetype = "dashed", color = "darkgrey", size = 0.5) +
          annotate(
            "text",
            x = 2020.5,
            y = max(agg_data1$Value, na.rm = TRUE),
            label = "Data for 2020 and 2021 are impacted by COVID-19",
            angle = 90,
            hjust = 1,
            vjust = 1.1
          ) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          geom_line(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            }
          )
        
      } else {
        p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(
            breaks = function(x) {
              if (length(x) == 0 || all(is.na(x))) return(c())
              seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            }
          )
      }
      
      # Apply colors with Scotland in grey
      area_colors <- get_transport_colors(unique(agg_data$Area), input$transport_classification_type, input$transport_metric)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      } else {
        # Use viridis for metrics that keep original colors, but still show Scotland in grey if present
        if ("Scotland" %in% unique(agg_data$Area)) {
          manual_colors <- c("#B2B2B2")
          names(manual_colors) <- "Scotland"
          other_areas <- unique(agg_data$Area)[unique(agg_data$Area) != "Scotland"]
          if (length(other_areas) > 0) {
            viridis_colors <- viridis_discrete(option = "plasma", end = 0.8)(length(other_areas))
            names(viridis_colors) <- other_areas
            manual_colors <- c(manual_colors, viridis_colors)
          }
          p <- p + scale_color_manual(values = manual_colors)
        } else {
          p <- p + scale_color_viridis_d(option = "plasma", end = 0.8)
        }
      }
      
      # Format y-axis
      if (is_cost_data) {
        p <- p + scale_y_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_ev_data) {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in transport trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Comparison chart 
  output$transport_comparison_chart <- renderPlotly({
    req(transport_values$processed_data, input$transport_classification_type, input$transport_selected_year)
    
    tryCatch({
      agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
      selected_data <- agg_data %>% filter(Year == as.numeric(input$transport_selected_year))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$transport_selected_year), textfont = list(size = 16)))
      }
      
      # Format data
      is_cost_data <- input$transport_metric == "Weekly travel costs"
      is_ev_data <- input$transport_metric == "EV Infrastructure"
      
      if (is_cost_data) {
        selected_data$Value_Display <- paste0("£", round(selected_data$Value, 1))
        x_label <- "Weekly Cost (£)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Cost: ", selected_data$Value_Display)
      } else if (is_ev_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "EV Charging per 100k"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>EV Charging: ", selected_data$Value_Rounded)
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
      
      #  Apply colors with Scotland in grey
      area_colors <- get_transport_colors(unique(selected_data$Area), input$transport_classification_type, input$transport_metric)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      } else {
        # Use viridis for metrics that keep original colors, but still show Scotland in grey if present
        if ("Scotland" %in% unique(selected_data$Area)) {
          manual_colors <- c("#B2B2B2")
          names(manual_colors) <- "Scotland"
          other_areas <- unique(selected_data$Area)[unique(selected_data$Area) != "Scotland"]
          if (length(other_areas) > 0) {
            viridis_colors <- viridis_discrete(option = "plasma", end = 0.8)(length(other_areas))
            names(viridis_colors) <- other_areas
            manual_colors <- c(manual_colors, viridis_colors)
          }
          p <- p + scale_fill_manual(values = manual_colors)
        } else {
          p <- p + scale_fill_viridis_d(option = "plasma", end = 0.8)
        }
      }
      
      # Format x-axis
      if (is_cost_data) {
        p <- p + scale_x_continuous(labels = scales::dollar_format(prefix = "£"))
      } else if (is_ev_data) {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in transport comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Download handlers (matching culture module)
  # Transport Trend Download
  output$transport_trend_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$transport_metric)
      paste0("Transport_Trend_", metric, "_", input$transport_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transport_values$processed_data, input$transport_classification_type)
      
      data <- simple_aggregate_transport_data(
        transport_values$processed_data,
        input$transport_classification_type
      ) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Transport Comparison Download
  output$transport_comparison_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$transport_metric)
      paste0("Transport_Comparison_", metric, "_", input$transport_classification_type, "_", input$transport_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transport_values$processed_data, input$transport_classification_type, input$transport_selected_year)
      
      data <- simple_aggregate_transport_data(
        transport_values$processed_data,
        input$transport_classification_type
      ) %>%
        filter(Year == as.numeric(input$transport_selected_year)) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Transport Table Download
  output$transport_table_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$transport_metric)
      paste0("Transport_Table_", metric, "_", input$transport_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(transport_values$processed_data, input$transport_classification_type)
      
      data <- simple_aggregate_transport_data(
        transport_values$processed_data,
        input$transport_classification_type
      )
      
      if (!is.null(input$transport_table_year_filter) && input$transport_table_year_filter != "all") {
        data <- data %>% filter(Year == as.numeric(input$transport_table_year_filter))
      }
      if (!is.null(input$transport_table_area_filter) && input$transport_table_area_filter != "all") {
        data <- data %>% filter(Area == input$transport_table_area_filter)
      }
      
      data <- data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  # Data table 
  output$transport_data_table <- DT::renderDataTable({
    req(transport_values$processed_data, input$transport_classification_type)
    
    if(nrow(transport_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_transport_metric_display_name(input$transport_metric)
    agg_data <- simple_aggregate_transport_data(transport_values$processed_data, input$transport_classification_type)
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    # Apply filters
    filtered_data <- agg_data
    
    if(!is.null(input$transport_table_year_filter) && input$transport_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$transport_table_year_filter))
    }
    
    if(!is.null(input$transport_table_area_filter) && input$transport_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$transport_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    # Prepare table data
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    # Format value column based on metric type
    if (input$transport_metric == "Weekly travel costs") {
      value_col_name <- "Weekly Cost (£)"
      table_data$Value <- round(table_data$Value, 1)
    } else if (input$transport_metric == "EV Infrastructure") {
      value_col_name <- "EV Charging per 100k"
      table_data$Value <- round(table_data$Value, 1)
    } else {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$transport_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (input$transport_metric == "Weekly travel costs") {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "£", digits = 1)
    } else if (input$transport_metric == "EV Infrastructure") {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    } else {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    }
    
    return(dt)
  })
}
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
