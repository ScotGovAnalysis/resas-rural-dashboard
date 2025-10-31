
# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(plotly)
library(DT)

# Population metrics configuration
population_metrics <- list(
  "Median age" = list(
    file_6fold = "population/median_age.xlsx",
    file_2fold = "population/median_age_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Median age by urban rural classification"
  ),
  "Population change (count)" = list(
    file_6fold = "population/population_count.xlsx", 
    file_2fold = "population/population_count_2_fold.xlsx",
    classifications = c("2-fold", "3-fold", "6-fold"),  # Can aggregate from 6-fold
    full_name = "Population change (count) by urban rural classification",
    include_scotland = TRUE
  ),
  "Population change (%)" = list(
    file_6fold = "population/population_change_percentage.xlsx",
    file_2fold = "population/population_change_percentage_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Annual percentage change in population by urban rural classification"
  ),
  "16-19 yo in education, employment, training" = list(
    file_6fold = "population/participation_16_19.xlsx",
    file_2fold = "population/participation_16_19_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "The proportion of 16-19 year olds in Scotland participating in",
    has_sub_metrics = TRUE,
    include_scotland = TRUE
  ),
  "Skill shortage vacancies" = list(
    file_6fold = "population/skill_shortage.xlsx",
    file_2fold = "population/skill_shortage_2_fold.xlsx",
    classifications = c("2-fold"),  # 2-fold data available
    full_name = "Incidence of skill shortage vacancies (employer base)",
    include_scotland = TRUE,
    no_trend_chart = TRUE
  ),
  "Positive destinations" = list(
    file_6fold = "population/positive_destinations.xlsx",
    file_2fold = "population/positive_destinations_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Percentage of school leavers in positive destinations at 9-month follow-up",
    chart_title = "(by pupil's home address)",
    include_scotland = TRUE
  ),
  "Primary School Literacy" = list(
    file_6fold = "population/primary_literacy.xlsx",
    file_2fold = "population/primary_literacy_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Percentage of primary school pupils achieving expected CfE Levels in Literacy",
    include_scotland = TRUE
  ),
  "Primary School Numeracy" = list(
    file_6fold = "population/primary_numeracy.xlsx",
    file_2fold = "population/primary_numeracy_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Percentage of primary school pupils achieving expected CfE Levels in Numeracy",
    include_scotland = TRUE
  ),
  "Secondary School Literacy" = list(
    file_6fold = "population/secondary_literacy.xlsx",
    file_2fold = "population/secondary_literacy_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Percentage of secondary school pupils achieving expected CfE Levels in Literacy",
    include_scotland = TRUE
  ),
  "Secondary School Numeracy" = list(
    file_6fold = "population/secondary_numeracy.xlsx",
    file_2fold = "population/secondary_numeracy_2_fold.xlsx",
    classifications = c("6-fold"),  # Only 6-fold available
    full_name = "Percentage of secondary school pupils achieving expected CfE Levels in Numeracy",
    include_scotland = TRUE
  )
)

# Sub-metrics for 16-19 participation
participation_sub_metrics <- c(
  "Education" = "Education",
  "Employment" = "Employment", 
  "Training and Other Personal Development" = "Training and Other Personal Development"
)

# Static Key Insights and Notes 
population_key_insights <- list(
  "Median age" = "Remote rural areas show the highest median age as of 2022 (52 years) and large urban areas the lowest (39 years).",
  "Population change (count)" = "Approximately 4.5m people live in urban areas in urban areas of Scotland and 960 000 live in rural areas as of 2022. The population in general increased from 2001 to 2022 (5m to 5.44m).",
  "Population change (%)" = "'Between 2020 and 2021 the populations of accessible rural and remote rural areas saw 2.0% and 1.6% increases respectively.",
  "16-19 yo in education, employment, training" = "Large urban areas see the highest proportion of 16-19 year olds in education as of 2025 (73.9%) and remote small towns the lowest (61.7%).",
  "Skill shortage vacancies" = "The incidence of skill shortage vacancies was 6% in rural Scotland and 7% in the rest of Scotland as of 2024. In 2022, the incidence of skill shortage vacancies was 8% in rural Scotland and 11% in the rest of Scotland.",
  "Positive destinations" = "Percentage of school leavers in positive destinations in Scotland at 9-month follow-up has increased steadily across Scotland between 2009 and 2022 (going from 85.9% to 92.9% in 2022).",
  "Primary School Literacy" = "Remote small towns show the lowest percentage of primary school literacy (69%) and accessible rural areas show the highest (77%) as of 2023.",
  "Primary School Numeracy" = "Remote small towns show the lowest percentage of primary school numeracy (77%) and accessible rural areas show the highest (83%) as of 2023.",
  "Secondary School Literacy" = "Remote small towns show the lowest percentage of secondary school literacy (85%) and accessible rural areas show the highest (90%) as of 2023.",
  "Secondary School Numeracy" = "Remote small towns show the lowest percentage of secondary school literacy (88%) and accessible rural areas as well as remote rural areas show the highest (joint 92%) as of 2023."
)


population_notes <- list(
  "Median age" = "Figures for 2022 are based on Scotland's Census 2022. Figures for 2011 - 2021 will be revised so that they are consistent with Scotland's Census 2022. As a result, at present, the 2011-2021 figures cannot be directly compared to the 2022 figures.",
  "Population change (count)" = "Figures for 2022 are based on Scotland's Census 2022. Figures for 2011 - 2021 will be revised so that they are consistent with Scotland's Census 2022. As a result, at present, the 2011-2021 figures cannot be directly compared to the 2022 figures.",
  "Population change (%)" = "More recent data are required to see whether this upward population change trend continues in these geographical areas beyond the COVID-19 pandemic.",
  "16-19 yo in education, employment, training" = 
    "1. Not participating and unconfirmed status are not included in table.\n
    2. Total Scotland figures do not sum from region figures as unknown region category was excluded.\n
    3. The effects of the COVID-19 pandemic and associated lockdown measures is likely to have young adults' participation. The statistics from 2020 cover the period 1st April 2019 to 31st March 2020 and so are unlikely to be impacted materially.\n
    4. From January 2025, HM Revenue & Customs (HMRC) has shared data with SDS for statistical purposes, providing employment records for 16–24-year-olds in Scotland. This data covered the whole period of interest from 1 April 2024 to 31 March 2025 and was merged with the CSS dataset to produce the final APM 2025 dataset. HMRC data is not within the APM dataset prior to 2025. The methodology change introduced in the APM 2025 has enhanced data quality, reducing those with an unconfirmed status.\n
    The inclusion of HMRC data will have contributed towards an increase in the number of 16–19-year-olds reported as participating in employment and the overall participation rate.\n
  This creates a step change in the time series and comparisons to previous years should be interpreted with caution due to the impact of these methodological improvements.",
  
  "Skill shortage vacancies" = "1. Datasets for previous UK ESS surveys (2015, 2017) are only available by region but no urban rural divide is published. 2. Scottish Survey in 2020 was separate from UK ESS. Skills shortage vacancy questions in UK and Scotland surveys are the same and therefore compatible, however also provides no urban-rural divide in the published data tables. 3. The 2022 and 2024 Employer Skills Survey includes a rural Scotland / rest of Scotland split for skill shortage vacancies, available in the published background data tables for Scotland. This is not available for the previous published surveys. Published national skill shortage vacancy data is available.",
  "Positive destinations" = "For further information on how the coronavirus (COVID-19) pandemic has affected these statistics, see the School Leaver Follow-Up Destinations Methodology And Data Sources publication, 2024.",
  "Primary School Literacy" = "1. The Achievement of Curriculum for Excellence Level (ACEL) publication covered only Primary school children (P1, P4 and P7) in 2020-21. Secondary school and special school data were not collected. 2. The Achievement of Curriculum for Excellence Level (ACEL) collection and publication was cancelled in 2019-20 due to the difficulties in collecting data whilst schools were closed due to COVID-19.",
  "Primary School Numeracy" = "1. The Achievement of Curriculum for Excellence Level (ACEL) publication covered only Primary school children (P1, P4 and P7) in 2020-21. Secondary school and special school data were not collected. 2. The Achievement of Curriculum for Excellence Level (ACEL) collection and publication was cancelled in 2019-20 due to the difficulties in collecting data whilst schools were closed due to COVID-19.",
  "Secondary School Literacy" = "1. The Achievement of Curriculum for Excellence Level (ACEL) publication covered only Primary school children (P1, P4 and P7) in 2020-21. Secondary school and special school data were not collected. 2. The Achievement of Curriculum for Excellence Level (ACEL) collection and publication was cancelled in 2019-20 due to the difficulties in collecting data whilst schools were closed due to COVID-19.",
  "Secondary School Numeracy" = "1. The Achievement of Curriculum for Excellence Level (ACEL) publication covered only Primary school children (P1, P4 and P7) in 2020-21. Secondary school and special school data were not collected. 2. The Achievement of Curriculum for Excellence Level (ACEL) collection and publication was cancelled in 2019-20 due to the difficulties in collecting data whilst schools were closed due to COVID-19."
)

# Function to get display name for metrics
get_population_metric_display_name <- function(metric_name) {
  metric_info <- population_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  
  return(metric_name)
}

# Helper function for number formatting
format_population_number <- function(number) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  return(scales::comma(round(number, 0)))
}

# Main data loading function 
load_population_data_simple <- function(metric_name, classification_type, selected_activity = NULL) {
  metric_info <- population_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  # Special case: Skill shortage - load directly from 6-fold file since it has 2-fold data
  if (metric_name == "Skill shortage vacancies") {
    return(load_skill_shortage_data())
  }
  
  # Special case: 16-19 participation 
  if (metric_name == "16-19 yo in education, employment, training") {
    selected_status_code <- if (!is.null(selected_activity)) selected_activity else NULL  # Use directly, no mapping needed here
    return(load_participation_16_19_data_updated(selected_status_code))
  }
  
  # For Population change (count) - can create 2-fold and 3-fold by aggregating 6-fold
  if (metric_name == "Population change (count)") {
    data_6fold <- load_population_count_data_updated()
    
    if (classification_type == "2-fold") {
      return(create_population_2fold_from_6fold(data_6fold, "sum"))
    } else if (classification_type == "3-fold") {
      return(create_population_3fold_from_6fold(data_6fold, "sum"))
    } else {
      return(data_6fold)
    }
  }
  
  # For all other metrics - only 6-fold available
  if (classification_type == "6-fold") {
    if (metric_name == "Median age") {
      return(load_median_age_data_updated())
    } else if (metric_name == "Population change (%)") {
      return(load_population_change_percentage_data_updated())
    } else if (metric_name == "Positive destinations") {
      return(load_positive_destinations_data_updated())
    } else if (metric_name == "Primary School Literacy") {
      return(load_primary_literacy_data_updated())
    } else if (metric_name == "Primary School Numeracy") {
      return(load_primary_numeracy_data_updated())
    } else if (metric_name == "Secondary School Literacy") {
      return(load_secondary_literacy_data_updated())
    } else if (metric_name == "Secondary School Numeracy") {
      return(load_secondary_numeracy_data_updated())
    }
  }
  
  return(data.frame())
}

# 16-19 participation data loading function 
load_participation_16_19_data_updated <- function(selected_status = NULL) {
  cat("Loading 16-19 participation data...\n")
  
  tryCatch({
    possible_paths <- c(
      "population/participation_16_19.xlsx",
      "participation_16_19.xlsx",
      "population_skills/participation_16_19.xlsx"
    )
    
    file_path <- NULL
    for (path in possible_paths) {
      if (file.exists(path)) {
        file_path <- path
        break
      }
    }
    
    if (is.null(file_path)) {
      cat("16-19 participation file not found in any expected location\n")
      return(data.frame())
    }
    
    cat("Reading 16-19 participation data from:", file_path, "\n")
    
    # Read Excel file 
    raw_data <- read_excel(file_path, skip = 1)
    cat("After skip=1, dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    
    # Set proper column names
    expected_cols <- c("Region", "Status", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")
    
    if (ncol(raw_data) >= 11) {
      names(raw_data)[1:12] <- expected_cols
    } else {
      cat("ERROR: Expected at least 11 columns, got", ncol(raw_data), "\n")
      return(data.frame())
    }
    
    # Filter out header row and process data
    processed_data <- raw_data %>%
      filter(!is.na(Region), 
             !is.na(Status), 
             Region != "", 
             Status != "",
             Region != "Region",
             Status != "Status") %>%
      select(Region, Status, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`, `2025`) %>%
      gather(key = "Year", value = "Value", -Region, -Status) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),
        Region_Clean = str_trim(Region),
        Status_Clean = str_trim(Status),
        Area = case_when(
          Region_Clean == "Scotland" ~ "Scotland",
          Region_Clean == "Large urban areas" ~ "Large Urban Areas",
          Region_Clean == "Other urban areas" ~ "Other Urban Areas",
          Region_Clean == "Accessible small towns" ~ "Accessible Small Towns",
          Region_Clean == "Remote small towns" ~ "Remote Small Towns", 
          Region_Clean == "Accessible rural areas" ~ "Accessible Rural",
          Region_Clean == "Remote rural areas" ~ "Remote Rural",
          TRUE ~ Region_Clean
        ),
        Data_Source = "Scottish Government - 16-19 Participation",
        Sub_Metric = Status_Clean
      ) %>%
      filter(!is.na(Value), 
             !is.na(Area), 
             !is.na(Year), 
             !is.na(Sub_Metric),
             Value > 0,
             !is.na(Status_Clean),
             Status_Clean != "")
    
    cat("After basic processing and cleaning:", nrow(processed_data), "rows\n")
    
    # Show available statuses after cleaning
    if (nrow(processed_data) > 0) {
      cat("Available statuses after cleaning:\n")
      unique_statuses <- unique(processed_data$Sub_Metric)
      for (i in seq_along(unique_statuses)) {
        status <- unique_statuses[i]
        cat(paste0("  ", i, ". '", status, "' (", class(status), ")\n"))
      }
    }
    
    # Filter by selected status 
    if (!is.null(selected_status) && 
        length(selected_status) > 0 && 
        !is.na(selected_status) && 
        selected_status != "") {
      cat("Filtering for selected status:", selected_status, "\n")
      
      filtered_data <- processed_data %>% 
        filter(!is.na(Sub_Metric) & Sub_Metric == selected_status)
      
      cat("Direct match results:", nrow(filtered_data), "rows\n")
      
      if (nrow(filtered_data) == 0) {
        cat("No match found for status:", selected_status, ". Available statuses:\n")
        print(unique(processed_data$Sub_Metric))
        return(data.frame())
      }
      
      processed_data <- filtered_data
    } else {
      cat("No status filter applied (selected_status is NULL, NA, or empty)\n")
    }
    
    # Final data selection
    final_data <- processed_data %>%
      select(Year, Area, Value, Data_Source, Sub_Metric)
    
    cat("Final processed data:", nrow(final_data), "records\n")
    if (nrow(final_data) > 0) {
      cat("Sample final data:\n")
      print(head(final_data, 5))
      cat("Value range:", min(final_data$Value, na.rm = TRUE), "to", max(final_data$Value, na.rm = TRUE), "\n")
    }
    
    return(final_data)
    
  }, error = function(e) {
    cat("Error loading 16-19 participation data:", e$message, "\n")
    print(e)
    return(data.frame())
  })
}

# Skill shortage data loading
load_skill_shortage_data <- function() {
  cat("Loading skill shortage data...\n")
  
  tryCatch({
    file_path <- if (file.exists("population/skill_shortage.xlsx")) {
      "population/skill_shortage.xlsx"
    } else if (file.exists("skill_shortage.xlsx")) {
      "skill_shortage.xlsx"
    } else {
      stop("skill_shortage.xlsx not found")
    }
    
    raw_data <- read_excel(file_path)
    cat("Raw skill shortage data loaded, dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Year), !is.na(skill_shortage_vacancies)) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(skill_shortage_vacancies) * 100,
        Area = case_when(
          Region == "Rural" ~ "Rural",
          Region == "Rest of Scotland" ~ "Urban",
          Region == "Scotland" ~ "Scotland",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Skill Shortage Vacancies"
      ) %>%
      filter(!is.na(Value), !is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "skill shortage records\n")
    if (nrow(processed_data) > 0) {
      cat("Sample processed data:\n")
      print(processed_data)
      cat("Expected vs Actual values:\n")
      cat("Rural: Expected 8%, Got", processed_data$Value[processed_data$Area == "Rural"], "%\n")
      cat("Urban: Expected 11%, Got", processed_data$Value[processed_data$Area == "Urban"], "%\n") 
      cat("Scotland: Expected 10%, Got", processed_data$Value[processed_data$Area == "Scotland"], "%\n")
    }
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading skill shortage data:", e$message, "\n")
    return(data.frame())
  })
}

# Functions to create 2-fold and 3-fold from 6-fold data
create_population_2fold_from_6fold <- function(data_6fold, aggregation_method = "mean") {
  if (nrow(data_6fold) == 0) return(data.frame())
  
  cat("Creating 2-fold from 6-fold data using", aggregation_method, "aggregation\n")
  
  data_2fold <- data_6fold %>%
    filter(!grepl("Scotland", Area, ignore.case = TRUE)) %>%
    mutate(
      Area_2fold = case_when(
        Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns") ~ "Urban",
        Area %in% c("Accessible Rural", "Remote Rural") ~ "Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Area_2fold)) %>%
    group_by(Year, Area_2fold, Data_Source) %>%
    summarise(
      Value = if (aggregation_method == "sum") sum(Value, na.rm = TRUE) else mean(Value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(Area = Area_2fold)
  
  scotland_data <- data_6fold %>% filter(grepl("Scotland", Area, ignore.case = TRUE))
  if (nrow(scotland_data) > 0) {
    data_2fold <- bind_rows(data_2fold, scotland_data)
  }
  
  cat("Created", nrow(data_2fold), "2-fold records\n")
  return(data_2fold)
}

create_population_3fold_from_6fold <- function(data_6fold, aggregation_method = "mean") {
  if (nrow(data_6fold) == 0) return(data.frame())
  
  cat("Creating 3-fold from 6-fold data using", aggregation_method, "aggregation\n")
  
  data_3fold <- data_6fold %>%
    filter(!grepl("Scotland", Area, ignore.case = TRUE)) %>%
    mutate(
      Area_3fold = case_when(
        Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns") ~ "Urban",
        Area == "Accessible Rural" ~ "Accessible Rural",
        Area == "Remote Rural" ~ "Remote Rural",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(Area_3fold)) %>%
    group_by(Year, Area_3fold, Data_Source) %>%
    summarise(
      Value = if (aggregation_method == "sum") sum(Value, na.rm = TRUE) else mean(Value, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(Area = Area_3fold)
  
  scotland_data <- data_6fold %>% filter(grepl("Scotland", Area, ignore.case = TRUE))
  if (nrow(scotland_data) > 0) {
    data_3fold <- bind_rows(data_3fold, scotland_data)
  }
  
  cat("Created", nrow(data_3fold), "3-fold records\n")
  return(data_3fold)
}

# Loading functions for 6-fold data
load_median_age_data_updated <- function() {
  cat("Loading median age data...\n")
  
  tryCatch({
    file_path <- if (file.exists("population/median_age.xlsx")) {
      "population/median_age.xlsx"
    } else if (file.exists("median_age.xlsx")) {
      "median_age.xlsx"
    } else {
      stop("median_age.xlsx not found")
    }
    
    raw_data <- read_excel(file_path)
    
    if (grepl("Table|Median", names(raw_data)[1], ignore.case = TRUE) || 
        grepl("Table|Median", as.character(raw_data[1,1]), ignore.case = TRUE)) {
      raw_data <- read_excel(file_path, skip = 1)
    }
    
    if (ncol(raw_data) >= 5) {
      names(raw_data)[1:5] <- c("Region", "Population_Group", "2020", "2021", "2022")
    }
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), Region != "", !is.na(`2020`)) %>%
      select(Region, `2020`, `2021`, `2022`) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural areas" ~ "Accessible Rural",
          Region == "Remote rural areas" ~ "Remote Rural",
          Region == "Scotland (All)" ~ "Scotland",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Median Age"
      ) %>%
      filter(!is.na(Value), !is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "median age records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading median age data:", e$message, "\n")
    return(data.frame())
  })
}

load_population_count_data_updated <- function() {
  cat("Loading population count data...\n")
  
  tryCatch({
    raw_data <- read_excel("population/population_count.xlsx", skip = 1)
    
    year_cols <- 2001:2022
    colnames(raw_data) <- c("Region", as.character(year_cols), rep("Extra", ncol(raw_data) - 23))
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), Region != "") %>%
      select(Region, all_of(as.character(year_cols))) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),
        Area = case_when(
          Region == "Scotland (All)" ~ "Scotland",
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Population Count"
      ) %>%
      filter(!is.na(Value), !is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat("Loaded", nrow(processed_data), "population count records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading population count data:", e$message, "\n")
    return(data.frame())
  })
}

load_population_change_percentage_data_updated <- function() {
  cat("Loading population change percentage data...\n")
  
  tryCatch({
    raw_data <- read_excel("population/population_change_percentage.xlsx", skip = 1)
    
    year_cols <- 2012:2021
    colnames(raw_data) <- c("Region", as.character(year_cols), rep("Extra", ncol(raw_data) - 11))
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), Region != "") %>%
      select(Region, all_of(as.character(year_cols))) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value) * 100,
        Area = case_when(
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural" ~ "Accessible Rural",
          Region == "Remote rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Population Change Percentage"
      ) %>%
      filter(!is.na(Value), !is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat("Loaded", nrow(processed_data), "population change percentage records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading population change percentage data:", e$message, "\n")
    return(data.frame())
  })
}

load_positive_destinations_data_updated <- function() {
  cat("Loading positive destinations data...\n")
  
  tryCatch({
    raw_data <- read_excel("population/positive_destinations.xlsx", skip = 1)
    
    colnames(raw_data) <- c("Year", "Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", 
                           "Remote Small Towns", "Accessible Rural", "Remote Rural", "Total", 
                           rep("Extra", ncol(raw_data) - 8))
    
    processed_data <- raw_data %>%
      filter(!is.na(Year)) %>%
      select(Year, `Large Urban Areas`, `Other Urban Areas`, `Accessible Small Towns`, 
             `Remote Small Towns`, `Accessible Rural`, `Remote Rural`, Total) %>%
      mutate(Year_Numeric = as.numeric(str_extract(Year, "^20[0-9]{2}"))) %>%
      filter(!is.na(Year_Numeric)) %>%
      gather(key = "Region", value = "Value", -Year, -Year_Numeric) %>%
      mutate(
        Year = Year_Numeric,
        Value = as.numeric(Value),
        Area = case_when(
          Region == "Total" ~ "Scotland",
          Region == "Large Urban Areas" ~ "Large Urban Areas",
          Region == "Other Urban Areas" ~ "Other Urban Areas",
          Region == "Accessible Small Towns" ~ "Accessible Small Towns",
          Region == "Remote Small Towns" ~ "Remote Small Towns",
          Region == "Accessible Rural" ~ "Accessible Rural",
          Region == "Remote Rural" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Positive Destinations"
      ) %>%
      filter(!is.na(Value), !is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat("Loaded", nrow(processed_data), "positive destinations records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading positive destinations data:", e$message, "\n")
    return(data.frame())
  })
}

load_education_achievement_data_updated <- function(filename, data_type) {
  cat("Loading", data_type, "data from", filename, "...\n")
  
  tryCatch({
    raw_data <- read_excel(paste0("population/", filename), skip = 1)
    
    colnames(raw_data) <- c("Year", "Large urban areas", "Other urban areas", "Accessible small towns", 
                           "Remote small towns", "Accessible rural areas", "Remote rural areas", "All pupils", 
                           rep("Extra", ncol(raw_data) - 8))
    
    processed_data <- raw_data %>%
      filter(!is.na(Year)) %>%
      select(Year, `Large urban areas`, `Other urban areas`, `Accessible small towns`, 
             `Remote small towns`, `Accessible rural areas`, `Remote rural areas`, `All pupils`) %>%
      mutate(Year_Numeric = as.numeric(str_extract(Year, "^20[0-9]{2}"))) %>%
      filter(!is.na(Year_Numeric)) %>%
      gather(key = "Region", value = "Value", -Year, -Year_Numeric) %>%
      mutate(
        Year = Year_Numeric,
        Value = as.numeric(Value),
        Area = case_when(
          Region == "All pupils" ~ "Scotland",
          Region == "Large urban areas" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible small towns" ~ "Accessible Small Towns",
          Region == "Remote small towns" ~ "Remote Small Towns",
          Region == "Accessible rural areas" ~ "Accessible Rural",
          Region == "Remote rural areas" ~ "Remote Rural",
          TRUE ~ Region
        ),
        Data_Source = paste("Scottish Government -", data_type)
      ) %>%
      filter(!is.na(Value), !is.na(Area)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat("Loaded", nrow(processed_data), data_type, "records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading", data_type, "data:", e$message, "\n")
    return(data.frame())
  })
}

load_primary_literacy_data_updated <- function() {
  load_education_achievement_data_updated("primary_literacy.xlsx", "Primary School Literacy")
}

load_primary_numeracy_data_updated <- function() {
  load_education_achievement_data_updated("primary_numeracy.xlsx", "Primary School Numeracy")
}

load_secondary_literacy_data_updated <- function() {
  load_education_achievement_data_updated("secondary_literacy.xlsx", "Secondary School Literacy")
}

load_secondary_numeracy_data_updated <- function() {
  load_education_achievement_data_updated("secondary_numeracy.xlsx", "Secondary School Numeracy")
}

# Simple aggregation function
simple_aggregate_population_data <- function(processed_data, classification_type = "2-fold") {
  if (nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}

# Get key insights (only for metrics with 2-fold/3-fold classifications)
get_simple_population_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold", selected_activity = NULL) {
  if (nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  cat("Processing population key insights for:", metric_name, "\n")
  cat("Data dimensions:", nrow(processed_data), "rows\n")
  cat("Unique areas in data:", paste(unique(processed_data$Area), collapse = ", "), "\n")
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    cat("Data already in 2-fold format\n")
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
    cat("Data in 6-fold format, aggregating to 2-fold\n")
    
    urban_areas <- latest_data %>% 
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns"))
    if (nrow(urban_areas) > 0) {
      if (metric_name == "Population change (count)") {
        urban_val <- sum(urban_areas$Value, na.rm = TRUE)
      } else {
        urban_val <- mean(urban_areas$Value, na.rm = TRUE)
      }
    }
    
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if (nrow(rural_areas) > 0) {
      if (metric_name == "Population change (count)") {
        rural_val <- sum(rural_areas$Value, na.rm = TRUE)
      } else {
        rural_val <- mean(rural_areas$Value, na.rm = TRUE)
      }
    }
    
    scotland_areas <- latest_data %>% filter(Area == "Scotland")
    if (nrow(scotland_areas) > 0) {
      scotland_val <- scotland_areas$Value[1]
    } else {
      all_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if (nrow(all_areas) > 0) {
        if (metric_name == "Population change (count)") {
          scotland_val <- sum(all_areas$Value, na.rm = TRUE)
        } else {
          scotland_val <- mean(all_areas$Value, na.rm = TRUE)
        }
      }
    }
  } else if (any(c("Accessible Rural", "Remote Rural") %in% unique_areas) && "Urban" %in% unique_areas) {
    cat("Data in 3-fold format\n")
    
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if (nrow(rural_areas) > 0) {
      if (metric_name == "Population change (count)") {
        rural_val <- sum(rural_areas$Value, na.rm = TRUE)
      } else {
        rural_val <- mean(rural_areas$Value, na.rm = TRUE)
      }
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

# Create color mapping with Scotland as gray
get_simple_population_colors <- function(areas, classification_type) {
  color_mapping <- list()
  
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
  }
  
  color_mapping[["Scotland"]] <- "#B2B2B2"
  
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display
format_population_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name == "Median age") {
    return(paste0(round(value, 1), " years"))
  } else if (metric_name == "Population change (count)") {
    return(format_population_number(value))
  } else if (metric_name %in% c("Population change (%)", "16-19 yo in education, employment, training", 
                                "Skill shortage vacancies", "Positive destinations", 
                                "Primary School Literacy", "Primary School Numeracy", 
                                "Secondary School Literacy", "Secondary School Numeracy")) {
    return(paste0(round(value, 1), "%"))
  } else {
    return(round(value, 1))
  }
}

# Calculate gap between urban and rural
calculate_simple_population_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name == "Median age") {
    return(paste0(round(gap, 1), " years"))
  } else if (metric_name == "Population change (count)") {
    return(format_population_number(gap))
  } else if (metric_name %in% c("Population change (%)", "16-19 yo in education, employment, training", 
                                "Skill shortage vacancies", "Positive destinations", 
                                "Primary School Literacy", "Primary School Numeracy", 
                                "Secondary School Literacy", "Secondary School Numeracy")) {
    return(paste0(round(gap, 1), "pp"))
  } else {
    return(round(gap, 1))
  }
}

# UI Components 
population_dashboard_ui <- function(category_id) {
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
      .population-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .population-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .population-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .population-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .population-metric-description {
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
          
          div(
            style = "position: absolute; left: 50px; top: 2px;",
            h2("Population, Education & Skills", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("population_classification_selector")
          ),
          
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("population_metric", "Population Metric:", 
                        choices = c("Select a policy metric..." = "", names(population_metrics)), 
                        selected = "", width = "220px")
          ),
          
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.population_metric == '16-19 yo in education, employment, training'",
              selectInput("selected_participation_status", "Sub-metric:", 
                          choices = names(participation_sub_metrics), selected = names(participation_sub_metrics)[1], width = "180px")
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
    
    conditionalPanel(
      condition = "input.population_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore population, education and skills data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Median age', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("users", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Median age"),
                      div(class = "population-metric-description", 
                          "Median age by urban rural classification, showing demographic patterns and age distribution across Scotland's diverse communities.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Population change (count)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("chart-line", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Population change (count)"),
                      div(class = "population-metric-description",
                          "Absolute population changes by area, tracking growth and decline patterns across urban and rural Scotland.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Population change (%)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("percentage", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Population change (%)"),
                      div(class = "population-metric-description",
                          "Annual percentage change in population, showing relative growth rates across different geographic classifications.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', '16-19 yo in education, employment, training', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("graduation-cap", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "16-19 yo in education, employment, training"),
                      div(class = "population-metric-description",
                          "Youth participation rates in education, training or employment, showing opportunities available to young people across different areas.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Skill shortage vacancies', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("briefcase", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Skill shortage vacancies"),
                      div(class = "population-metric-description",
                          "Incidence of skill shortage vacancies by employer base, showing labor market challenges across urban and rural areas.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Positive destinations', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("route", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Positive destinations"),
                      div(class = "population-metric-description",
                          "Percentage of school leavers in positive destinations at 9-month follow-up, tracking post-school outcomes across different areas.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Primary School Literacy', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("book-open", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Primary School Literacy"),
                      div(class = "population-metric-description",
                          "Percentage of primary school pupils achieving expected CfE Levels in Literacy across urban and rural areas.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Primary School Numeracy', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("calculator", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Primary School Numeracy"),
                      div(class = "population-metric-description",
                          "Percentage of primary school pupils achieving expected CfE Levels in Numeracy across different geographic areas.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Secondary School Literacy', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("book", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Secondary School Literacy"),
                      div(class = "population-metric-description",
                          "Secondary literacy achievement rates showing educational outcomes across Scotland's urban-rural spectrum.")
                    )
                  )
                ),
                
                div(
                  class = "population-metrics-card",
                  onclick = "Shiny.setInputValue('population_metric_select', 'Secondary School Numeracy', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("chart-bar", class = "population-metric-icon"),
                    div(
                      div(class = "population-metric-title", "Secondary School Numeracy"),
                      div(class = "population-metric-description",
                          "Secondary numeracy achievement showing geographic variations in mathematical education outcomes.")
                    )
                  )
                )
              )
            )
          )
      )
    ),
    
    conditionalPanel(
      condition = "input.population_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("population_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("population_data_summary"))
          ),
          
          # Show key insights only for metrics with 2-fold/3-fold classifications
          conditionalPanel(
            condition = "input.population_metric == 'Population change (count)' || input.population_metric == 'Skill shortage vacancies'",
            fluidRow(
              box(title = uiOutput("population_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("population_urban_rate", width = 3),
                    valueBoxOutput("population_rural_rate", width = 3),
                    valueBoxOutput("population_scotland_rate", width = 3),
                    valueBoxOutput("population_urban_rural_gap", width = 3)
                  ))
            )
          ),
          
          conditionalPanel(
            condition = "input.population_metric != 'Skill shortage vacancies'",
            fluidRow(
              box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                              uiOutput("population_trend_title"),
                              div(style = "position: absolute; right: 20px;",
                                  downloadButton("population_trend_download", "Download", class = "excel-download-btn"
                                  ))),
                  status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("population_trend_chart") %>% withSpinner())
            )
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                            uiOutput("population_comparison_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("population_comparison_download", "Download", class = "excel-download-btn"
                                ))),
                status = "primary", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;", uiOutput("population_year_selector")),
                plotlyOutput("population_comparison_chart") %>% withSpinner())
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                            uiOutput("population_table_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("population_table_download", "Download", class = "excel-download-btn"
                                ))),
                status = "info", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;",
                    fluidRow(
                      column(6, uiOutput("population_table_year_filter")),
                      column(6, uiOutput("population_table_area_filter"))
                    )),
                DT::dataTableOutput("population_data_table") %>% withSpinner())
          )
      )
    )
  )
}

# Population module server
population_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  population_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  observeEvent(input$population_metric_select, {
    updateSelectInput(session, "population_metric", selected = input$population_metric_select)
  })
  
  output$population_summary_title <- renderUI({
    req(input$population_metric, input$population_classification_type)
    display_name <- 
      ifelse(input$population_metric == "16-19 yo in education, employment, training", 
             paste(get_population_metric_display_name(input$population_metric), input$selected_participation_status),
             get_population_metric_display_name(input$population_metric))
    classification_text <- case_when(
      input$population_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$population_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$population_classification_type == "6-fold" ~ "(6-fold Classification)",
      TRUE ~ paste0("(", input$population_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$population_year_selector <- renderUI({
    req(population_values$processed_data)
    if (nrow(population_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(population_values$processed_data$Year), decreasing = TRUE)
    if (length(available_years) > 0) {
      selectInput("population_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$population_classification_selector <- renderUI({
    req(input$population_metric)
    available_classifications <- population_metrics[[input$population_metric]]$classifications
    choices <- list()
    
    if ("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
    if ("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"  
    if ("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    
    default_selection <- if (length(choices) > 0) choices[[1]] else NULL
    
    selectInput("population_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  
  output$population_trend_title <- renderUI({
    req(input$population_metric, input$selected_participation_status)
    
    display_name <- if (input$population_metric == "16-19 yo in education, employment, training") {
      paste(get_population_metric_display_name(input$population_metric), input$selected_participation_status)
    } else if (input$population_metric == "Positive destinations") {
      paste(get_population_metric_display_name(input$population_metric), "(by pupil's home address)")
    } else {
      get_population_metric_display_name(input$population_metric)
    }})
    
  output$population_comparison_title <- renderUI({
    req(input$population_selected_year, input$population_metric,
        input$selected_participation_status )
    display_name <- if (input$population_metric == "16-19 yo in education, employment, training") {
      paste(get_population_metric_display_name(input$population_metric), input$selected_participation_status)
    } else if (input$population_metric == "Positive destinations") {
      paste(get_population_metric_display_name(input$population_metric), "(by pupil's home address)")
    } else {
      get_population_metric_display_name(input$population_metric)
    }
    paste0("Single Year Comparison (", input$population_selected_year, ") for ", display_name)
  })
  
  output$population_table_title <- renderUI({
    req(input$population_metric,
        input$selected_participation_status)
    
    display_name <- if (input$population_metric == "16-19 yo in education, employment, training") {
      paste(get_population_metric_display_name(input$population_metric), input$selected_participation_status)
    } else if (input$population_metric == "Positive destinations") {
      paste(get_population_metric_display_name(input$population_metric), "(by pupil's home address)")
    } else {
      get_population_metric_display_name(input$population_metric)
    }
    
    paste("Data Table for", display_name)
  })
  
  output$population_key_insights_title <- renderUI({
    req(input$population_metric,
        input$selected_participation_status)
    display_name <- ifelse(input$population_metric == "16-19 yo in education, employment, training", 
                           paste(get_population_metric_display_name(input$population_metric), input$selected_participation_status),
                           get_population_metric_display_name(input$population_metric))
    paste("Key Insights for", display_name)
  })
  
  output$population_table_year_filter <- renderUI({
    req(population_values$processed_data)
    if (nrow(population_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(population_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for (year in all_years) choices[[as.character(year)]] <- year
    selectInput("population_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$population_table_area_filter <- renderUI({
    req(population_values$processed_data, input$population_classification_type)
    if (nrow(population_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type)
    if (nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for (area in all_areas) choices[[area]] <- area
    selectInput("population_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
    
  # Data loading for non-16-19 metrics
  observe({
    req(input$population_metric, input$population_classification_type)
    
    if (input$population_metric == "" || input$population_metric == "16-19 yo in education, employment, training") {
      return()
    }
    
    population_values$processed_data <- load_population_data_simple(
      input$population_metric, 
      input$population_classification_type
    )
    
    population_values$data_status <- if (nrow(population_values$processed_data) > 0) "Population data loaded" else "No data available"
  })
  
  # Separate observer for 16-19 data
  observe({
    req(input$population_metric == "16-19 yo in education, employment, training",
        input$population_classification_type, 
        input$selected_participation_status)
    
    # Use double bracket to get the actual value
    selected_status_code <- participation_sub_metrics[[input$selected_participation_status]]
    
    population_values$processed_data <- load_population_data_simple(
      input$population_metric, 
      input$population_classification_type, 
      selected_status_code
    )
    
    population_values$data_status <- if (nrow(population_values$processed_data) > 0) "Population data loaded" else "No data available"
  })  
  output$population_data_summary <- renderUI({
    req(input$population_classification_type, input$population_metric)
    
    if (nrow(population_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- ifelse(input$population_metric == "16-19 yo in education, employment, training", 
                           paste(get_population_metric_display_name(input$population_metric), input$selected_participation_status),
                           get_population_metric_display_name(input$population_metric))
    custom_insight <- population_key_insights[[input$population_metric]]
    custom_notes <- population_notes[[input$population_metric]]
    
    
    source_info <- switch(input$population_metric,
                          "Median age" = list(
                            text1 = "Mid-2020 Small Area Population Estimates for 2011 Data Zones, National Records of Scotland",
                            url1 = "https://webarchive.nrscotland.gov.uk/20241128121856/https:/www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/small-area-population-estimates-2011-data-zone-based/mid-2020",
                            text2 = "Mid-2021 Small Area Population Estimates for 2011 Data Zones, National Records of Scotland",
                            url2 = "https://webarchive.nrscotland.gov.uk/20241128121855/https:/www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/small-area-population-estimates-2011-data-zone-based/mid-2021",
                            text3 = "Small area population estimates: mid-2022, National Records of Scotland",
                            url3 = "https://www.nrscotland.gov.uk/publications/small-area-population-estimates-mid-2022/"
                            
                          ),
                          "Population change (count)" = list(
                            text1 = "Mid-Year Small Area Population Estimates for 2011 Data Zones, National Records of Scotland", 
                            url1 = "https://webarchive.nrscotland.gov.uk/20241128121748/https:/www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates",
                            text2 = "Other Geographies: mid-2022 (2011 Data Zone based) - National Records of Scotland (NRS)",
                            url2 = "https://www.nrscotland.gov.uk/publications/other-geographies-mid-2022-2011-data-zone-based/"
                          ),
                          "Population change (%)" = list(
                            text = "Mid-2021 Small Area Population Estimates for 2011 Data Zones, National Records of Scotland",
                            url = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/small-area-population-estimates-2011-data-zone-based/mid-2021"
                          ),
                          "16-19 yo in education, employment, training" = list(
                            text = "Annual Participation Measure | Skills Development Scotland, August 2025",
                            url = "https://www.skillsdevelopmentscotland.co.uk/publications-statistics/statistics/annual-participation-measure"
                          ),
                          "Skill shortage vacancies" = list(
                            text = "Employer Skills Survey 2024",
                            url = "https://www.gov.scot/publications/employer-skills-survey-2024-scotland/"
                          ),
                          "Positive destinations" = list(
                            text = "Summary statistics for Follow- Up Leaver Destinations, No. 6: 2024 Edition",
                            url = "https://www.gov.scot/publications/summary-statistics-follow-up-leaver-destinations-no-6-2024-edition/"
                          ),
                          "Primary School Literacy" = list(
                            text = "Achievement of Curriculum for Excellence (CfE) Levels 2023-24",
                            url = "https://www.gov.scot/collections/school-education-statistics/"
                          ),
                          "Primary School Numeracy" = list(
                            text = "Achievement of Curriculum for Excellence (CfE) Levels 2023-24",
                            url = "https://www.gov.scot/collections/school-education-statistics/"
                          ),
                          "Secondary School Literacy" = list(
                            text = "Achievement of Curriculum for Excellence (CfE) Levels 2023-24",
                            url = "https://www.gov.scot/collections/school-education-statistics/"
                          ),
                          "Secondary School Numeracy" = list(
                            text = "Achievement of Curriculum for Excellence (CfE) Levels 2023-24",
                            url = "https://www.gov.scot/collections/school-education-statistics/"
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
      
      # Add extra sources for nrs data
      if(input$population_metric == "Population change (count)") {
      insight_content <- tagList(
        insight_content,
        hr(style = "border-top: 1px solid #c3e6cb; margin: 15px 0 10px 0;"),
        div(
          style = "font-size: 1.25em; color: #155724;",
          strong("Source - 2001-2021:", style = "color: #155724;"),
          tags$a(
            href = source_info$url1,
            target = "_blank",
            source_info$text1,
            style = "color: #007bff; text-decoration: none; font-weight: 500;"
          )
        ),
        div(
          style = "font-size: 1.25em; color: #155724;",
          strong("Source - 2022:", style = "color: #155724;"),
          tags$a(
            href = source_info$url2,
            target = "_blank",
            source_info$text2,
            style = "color: #007bff; text-decoration: none; font-weight: 500;"
          )
        )
        
      )}
      else if(input$population_metric == "Median age"){
        insight_content <- tagList(
        insight_content,
        hr(style = "border-top: 1px solid #c3e6cb; margin: 15px 0 10px 0;"),
        div(
          style = "font-size: 1.25em; color: #155724;",
          strong("Source - 2020:", style = "color: #155724;"),
          tags$a(
            href = source_info$url1,
            target = "_blank",
            source_info$text1,
            style = "color: #007bff; text-decoration: none; font-weight: 500;"
          )
        ),
        div(
          style = "font-size: 1.25em; color: #155724;",
          strong("Source - 2021:", style = "color: #155724;"),
          tags$a(
            href = source_info$url2,
            target = "_blank",
            source_info$text2,
            style = "color: #007bff; text-decoration: none; font-weight: 500;"
          )
        ),
        div(
          style = "font-size: 1.25em; color: #155724;",
          strong("Source - 2022:", style = "color: #155724;"),
          tags$a(
            href = source_info$url3,
            target = "_blank",
            source_info$text3,
            style = "color: #007bff; text-decoration: none; font-weight: 500;"
          )
        )
      )}
      else{
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
      )}
      
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
    
    return(div(h4("Population Data Loaded"), p(paste("Showing data for:", display_name))))
  })
  
  output$population_urban_rate <- renderValueBox({
    req(population_values$processed_data, input$population_metric)
    
    key_insights <- get_simple_population_key_insights(population_values$processed_data, input$population_metric, "2-fold")
    val <- format_population_value(key_insights$urban, input$population_metric)
    year <- if (!is.na(key_insights$year)) key_insights$year else ""
    
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
  
  output$population_rural_rate <- renderValueBox({
    req(population_values$processed_data, input$population_metric)
    
    key_insights <- get_simple_population_key_insights(population_values$processed_data, input$population_metric, "2-fold")
    val <- format_population_value(key_insights$rural, input$population_metric)
    year <- if (!is.na(key_insights$year)) key_insights$year else ""
    
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
  
  output$population_scotland_rate <- renderValueBox({
    req(population_values$processed_data, input$population_metric)
    
    key_insights <- get_simple_population_key_insights(population_values$processed_data, input$population_metric, "2-fold")
    val <- format_population_value(key_insights$scotland, input$population_metric)
    year <- if (!is.na(key_insights$year)) key_insights$year else ""
    
   # valueBox(value = val, subtitle = paste("Scotland Total", year), icon = icon("flag"), color = "maroon")
    valueBox(
      value = tagList(
        tags$div(style = "font-size:20pt; font-weight:normal; min-height:40px;", paste("Scotland Total", year)),
        tags$div(style = "font-size:22pt; font-weight:bold;", val)
      ),
      subtitle = NULL,
      icon = icon("flag"),
      color = "aqua"
    )
    
  })
  
  output$population_urban_rural_gap <- renderValueBox({
    req(population_values$processed_data, input$population_metric)
    
    key_insights <- get_simple_population_key_insights(population_values$processed_data, input$population_metric, "2-fold")
    val <- calculate_simple_population_gap(key_insights$urban, key_insights$rural, input$population_metric)
    
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
  
  output$population_trend_chart <- renderPlotly({
    req(population_values$processed_data, input$population_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
      
      is_count_data <- input$population_metric == "Population change (count)"
      is_age_data <- input$population_metric == "Median age"
      
      if (is_count_data) {
        agg_data$Value_Display <- sapply(agg_data$Value, format_population_number)
        y_label <- "Population Count"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Count: ", agg_data$Value_Display)
      } else if (is_age_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Age (Years)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Age: ", agg_data$Value_Rounded, " years")
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text

       
        
      if (input$population_metric == "16-19 yo in education, employment, training") {
        
        # Split data based on methodology change
        agg_data1 <- agg_data |> filter(Year < 2025)
        agg_data2 <- agg_data |> filter(Year >= 2025)
        
        # Build plot with methodology change line
        p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_vline(xintercept = 2024, linetype = "dashed",  color = "darkgrey", size = 0.5) +
          geom_vline(xintercept = 2025, linetype = "dashed",  color = "darkgrey", size = 0.5) +
          
          annotate(
            "text",
            x = 2024.5,  # align with the vertical line
            y = max(agg_data$Value, na.rm = TRUE)-0.5,  # top of the plot
            label = "Methodology change\nin 2025",  # stacked text using newline
            angle = 90,  # vertical orientation
            hjust = 1,   # right-align the text
            vjust = 1.1 # adjust vertical position slightly above the top
          )+
        
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          geom_point(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 3) +
         
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(breaks = function(x) {
            if (length(x) == 0 || all(is.na(x))) return(c())
            seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
          })
        
      }
      else if (input$population_metric == "Median age") {
        
        # Split data based on methodology change
        agg_data1 <- agg_data |> filter(Year < 2022)
        agg_data2 <- agg_data |> filter(Year >= 2022)
        
        # Build plot with methodology change line
        p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          geom_vline(xintercept = 2021, linetype = "dashed",  color = "darkgrey", size = 0.5) +
          geom_vline(xintercept = 2022, linetype = "dashed",  color = "darkgrey", size = 0.5) +
          
          annotate(
            "text",
            x = 2021.5,  # align with the vertical line
            y = max(agg_data$Value, na.rm = TRUE)-0.5,  # top of the plot
            label = "Data for 2022 are based on a different methodology",  # stacked text using newline
            angle = 90,  # vertical orientation
            hjust = 1,   # right-align the text
            vjust = 1.1 # adjust vertical position slightly above the top
          )+
          
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          geom_point(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 3) +
          
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(breaks = function(x) {
            if (length(x) == 0 || all(is.na(x))) return(c())
            seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
          })
        
      }
      else
        if (input$population_metric == "Population change (count)") {
          
          # Split data based on methodology change
          agg_data1 <- agg_data |> filter(Area !="Scotland") |>  filter(Year < 2022)
          agg_data2 <- agg_data |> filter(Area !="Scotland") |>  filter(Year == 2022) # no 2023 data - to exclude x axis label, change when 2023 data onwards are available
          
          # Build plot with methodology change line
          p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
            geom_vline(xintercept = 2021, linetype = "dashed", color = "darkgrey", size = 0.5) +
            geom_vline(xintercept = 2022, linetype = "dashed", color = "darkgrey", size = 0.5) +
            
            annotate(
              "text",
              x = 2021.2,
              y = max(agg_data1$Value, na.rm = TRUE) * 0.7,
              label = "Data for 2022\nare based on a\ndifferent methodology",
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
      
      # Default plot without methodology split
      else if(input$population_metric == "Skill shortage vacancies"){
        p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(x = "Year", y = y_label, color = "Area Type") +
        scale_x_continuous(breaks = function(x) {
          if (length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
        })
        
        
      }
    # custom year labels x axis
        else{ 
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
                paste0(x, "-", substr(x + 1, 3, 4))  # e.g., 2022 becomes "2022-23"
              }
            )
          
       
       
      }
      
      
      area_colors <- get_simple_population_colors(unique(agg_data$Area), input$population_classification_type)
      if (length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      if (is_count_data) {
        p <- p + scale_y_continuous(labels = function(x) sapply(x, format_population_number))
      } else if (!is_age_data) {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  
  
  
  
  output$population_comparison_chart <- renderPlotly({
    req(population_values$processed_data, input$population_classification_type, input$population_selected_year)
    
    tryCatch({
      agg_data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type)
      selected_data <- agg_data %>% filter(Year == as.numeric(input$population_selected_year))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$population_selected_year), textfont = list(size = 16)))
      }
      
      is_count_data <- input$population_metric == "Population change (count)"
      is_age_data <- input$population_metric == "Median age"
      
      if (is_count_data) {
        selected_data$Value_Display <- sapply(selected_data$Value, format_population_number)
        x_label <- "Population Count"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Count: ", selected_data$Value_Display)
      } else if (is_age_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "Age (Years)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Age: ", selected_data$Value_Rounded, " years")
      } else {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "Percentage (%)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Rate: ", selected_data$Value_Rounded, "%")
      }
      
      selected_data$tooltip <- tooltip_text
      
      p <- ggplot(selected_data, aes(x = Value, y = Area, fill = Area, text = tooltip)) +
        geom_bar(stat = "identity", width = 0.4) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text.y = element_text(size = 10)
        ) +
        labs(x = x_label, y = "Area")
      
      area_colors <- get_simple_population_colors(unique(selected_data$Area), input$population_classification_type)
      if (length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      if (is_count_data) {
        p <- p + scale_x_continuous(labels = function(x) sapply(x, format_population_number))
      } else if (!is_age_data) {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  output$population_data_table <- DT::renderDataTable({
    req(population_values$processed_data, input$population_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }
      
      filtered_data <- agg_data
      
      if (!is.null(input$population_table_year_filter) && input$population_table_year_filter != "all") {
        filtered_data <- filtered_data %>% filter(Year == as.numeric(input$population_table_year_filter))
      }
      
      if (!is.null(input$population_table_area_filter) && input$population_table_area_filter != "all") {
        filtered_data <- filtered_data %>% filter(Area == input$population_table_area_filter)
      }
      
      is_count_data <- input$population_metric == "Population change (count)"
      is_age_data <- input$population_metric == "Median age"
      
      if (is_count_data) {
        filtered_data$Value <- sapply(filtered_data$Value, format_population_number)
      } else if (is_age_data) {
        filtered_data$Value <- sprintf("%.1f years", filtered_data$Value)
      } else {
        filtered_data$Value <- sprintf("%.1f%%", filtered_data$Value)
      }
      
      table_data <- filtered_data %>%
        select(Year, Area, Value) %>%
        arrange(Year, Area)
      
      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          ),
          dom = 'tip'
        ),
        rownames = FALSE,
        colnames = c("Year", "Area", "Value")
      ) %>%
        formatStyle(columns = 'Value', textAlign = 'right')
      
    }, error = function(e) {
      cat("Error in data table:", e$message, "\n")
      DT::datatable(data.frame(Message = paste("Error:", e$message)))
    })
  })
  
  # Trend Download Handler
  output$population_trend_download <- downloadHandler(
    filename = function() {
      paste0("population_trend_", input$population_metric, "_", input$population_classification_type, ".csv")
    },
    content = function(file) {
      req(population_values$processed_data, input$population_classification_type)
      
      data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type)
      
      if (nrow(data) == 0) {
        showNotification("No data available to download.", type = "warning")
        return(NULL)
      }
      
      write.csv(data, file = file, row.names = FALSE)
    }
  )
  
  # Comparison Download Handler
  output$population_comparison_download <- downloadHandler(
    filename = function() {
      paste0("population_comparison_", input$population_metric, "_", input$population_selected_year, ".csv")
    },
    content = function(file) {
      req(population_values$processed_data, input$population_classification_type, input$population_selected_year)
      
      data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type) %>%
        filter(Year == as.numeric(input$population_selected_year))
      
      if (nrow(data) == 0) {
        showNotification("No data available to download.", type = "warning")
        return(NULL)
      }
      
      write.csv(data, file = file, row.names = FALSE)
    }
  )
  
  # Table Download Handler
  output$population_table_download <- downloadHandler(
    filename = function() {
      paste0("population_table_", input$population_metric, "_", input$population_classification_type, ".csv")
    },
    content = function(file) {
      req(population_values$processed_data, input$population_classification_type)
      
      data <- simple_aggregate_population_data(population_values$processed_data, input$population_classification_type)
      
      if (nrow(data) == 0) {
        showNotification("No data available to download.", type = "warning")
        return(NULL)
      }
      
      write.csv(data, file = file, row.names = FALSE)
    }
  )
}
  

# Helper function to get classification colors
get_classification_colors <- function(classification_type) {
  if (classification_type == "6-fold") {
    return(list(
      Large_Urban_Areas = "#FDBE41", # Bright yellow
      Other_Urban_Areas = "#F4E470", # Light yellow
      Accessible_Small_Towns = "#80BA27", # Olive green
      Remote_Small_Towns = "#23A845", # Medium green
      Accessible_Rural = "#00833E", # Dark green
      Remote_Rural = "#0E450B" # Very dark green
    ))
  } else if (classification_type == "3-fold") {
    return(list(
      Urban = "#FDBE41", # Bright yellow
      Accessible_Rural = "#80BA27", # Olive green
      Remote_Rural = "#0E450B" # Very dark green
    ))
  } else if (classification_type == "2-fold") {
    return(list(
      Urban = "#FDBE41", # Bright yellow
      Rural = "#0E450B" # Very dark green
    ))
  }
  return(NULL)
}             
# Module UI and Server
population_ui <- function(id) {
  ns <- NS(id)
  population_dashboard_ui(id)
}

population_server_module <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    population_server(id, values, session)
  })
}
