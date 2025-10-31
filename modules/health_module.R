
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(shiny)
library(plotly)
library(DT)

# Health metrics configuration with source information
health_metrics <- list(
  "Quality of care" = list(
    file = "care_experience.xlsx",
    file_2fold = "care_experience_2_fold.xlsx",
    file_6fold = "care_experience.xlsx",
    classifications = c("6-fold"),
    full_name = "Quality of care experience",
    include_scotland = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  ),
  "Self-assessed health" = list(
    file = "self_assessed_health.xlsx",
    file_2fold = "self_assessed_health_2_fold.xlsx", 
    file_6fold = "self_assessed_health.xlsx",
    classifications = c("6-fold"),
    full_name = "Self-assessed general health",
    include_scotland = TRUE,
    source_text = "Scottish Surveys Core Questions",
    source_url = "https://www.gov.scot/collections/scottish-surveys-core-questions/"
  ),
  "Healthy Life Expectancy (males)" = list(
    file = "healthy_male.xlsx",
    file_2fold = "healthy_male_2_fold.xlsx",
    file_6fold = "healthy_male.xlsx",
    classifications = c("6-fold"),
    full_name = "Healthy Life Expectancy (males)",
    include_scotland = TRUE,
    source_text = "Healthy Life Expectancy in Scotland",
    source_url = "https://www.nrscotland.gov.uk/publications/healthy-life-expectancy-2019-2021/#:~:text=In%202019-2021%20healthy%20life%20expectancy%20at%20birth%20for,2015-2017%20for%20males%20and%20since%202014-2016%20for%20females"
  ),
  "Healthy Life Expectancy (females)" = list(
    file = "healthy_female.xlsx", 
    file_2fold = "healthy_female_2_fold.xlsx",
    file_6fold = "healthy_female.xlsx",
    classifications = c("6-fold"),
    full_name = "Healthy Life Expectancy (females)",
    include_scotland = TRUE,
    source_text = "Healthy Life Expectancy in Scotland",
    source_url = "https://www.nrscotland.gov.uk/publications/healthy-life-expectancy-2019-2021/#:~:text=In%202019-2021%20healthy%20life%20expectancy%20at%20birth%20for,2015-2017%20for%20males%20and%20since%202014-2016%20for%20females"
  ),
  "Mental Well-being" = list(
    file = "warwick_edi.xlsx",
    file_2fold = "warwick_edi_2_fold.xlsx",
    file_6fold = "warwick_edi.xlsx",
    classifications = c("6-fold"),
    full_name = "Warwick-Edinburgh Mental Well-being Scale (WEMWBS) score",
    source_text = "Scottish Surveys Core Questions (Scottish Household Survey)",
    source_url = "https://www.gov.scot/publications/scottish-surveys-core-questions-2023/documents/"
  ),
  "GP access" = list(
    file = "gp_access.xlsx",
    file_2fold = "gp_access_2_fold.xlsx",
    file_6fold = "gp_access.xlsx",
    classifications = c("6-fold"),
    full_name = "How easy it is for people to contact their General Practice in the way they want",
    include_scotland = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  ),
  "Care impact on quality of life" = list(
    file = "care_impact.xlsx",
    file_2fold = "care_impact_2_fold.xlsx",
    file_6fold = "care_impact.xlsx",
    classifications = c("6-fold"),
    full_name = "Help, care or support improved or maintained quality of life",
    include_scotland = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  ),
  "Access to nature" = list(
    file = "5min_walk.xlsx",
    file_2fold = "5min_walk_2_fold.xlsx",
    file_6fold = "5min_walk.xlsx",
    classifications = c("6-fold"),
    full_name = "Proportion (%) of adults living within 5 minutes' walk of their nearest green or blue space",
    include_scotland = TRUE,
    source_text = "Scottish Household Survey",
    source_url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
  ),
  "Out-of-hours healthcare" = list(
    file = "out_of_hours.xlsx",
    file_2fold = "out_of_hours_2_fold.xlsx",
    file_6fold = "out_of_hours.xlsx",
    classifications = c("6-fold"),
    full_name = "Experience of out of hours healthcare",
    has_sub_metrics = TRUE,
    no_trend_chart = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  ),
  "Help with Everyday Living" = list(
    file = "everyday_living.xlsx",
    file_2fold = "everyday_living_2_fold.xlsx",
    file_6fold = "everyday_living.xlsx",
    classifications = c("6-fold"),
    full_name = "Rating of Care, Support and Help with Everyday Living",
    has_sub_metrics = TRUE,
    no_trend_chart = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  ),
  "Care-Life Balance" = list(
    file = "care_life_balance.xlsx",
    file_2fold = "care_life_balance_2_fold.xlsx",
    file_6fold = "care_life_balance.xlsx",
    classifications = c("6-fold"),
    full_name = "Rating of balance between caring and other things in life",
    has_sub_metrics = TRUE,
    no_trend_chart = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  ),
  "Support to Continue Caring" = list(
    file = "support_continue_caring.xlsx",
    file_2fold = "support_continue_caring_2_fold.xlsx",
    file_6fold = "support_continue_caring.xlsx",
    classifications = c("6-fold"),
    full_name = "Rating of feeling supported to continue caring",
    has_sub_metrics = TRUE,
    no_trend_chart = TRUE,
    source_text = "Health and Care Experience Survey (Bespoke data)",
    source_url = "https://www.gov.scot/collections/health-and-care-experience-survey/"
  )
)

# Sub-metrics for rating-based metrics
rating_sub_metrics <- c(
  "Positive" = "Positive",
  "Neutral" = "Neutral", 
  "Negative" = "Negative"
)

# Static Key Insights for each health metric (with source integration)
health_key_insights <- list(
  "Quality of care" = "As of 2023, the quality of care experience is the highest in remote rural areas (81%) and the lowest in other urban areas (64%).",
  "Self-assessed health" = "As of 2023, self-assessed health is the highest in accessible rural areas (75%) and lowest in remote small towns (68%) but the values are in general similar across the regions and cluster around the Scotland average of 72.2%.",
  "Healthy Life Expectancy (males)" = "For 2019-2021, healthy life expectancy for males was the highest in accessible rural areas (65 years) and remote rural areas (64.3 years). It was the lowest in large urban areas and other urban areas (58.8 and 58.7 years).",
  "Healthy Life Expectancy (females)" = "For 2019-2021, healthy life expectancy for females was the highest in remote rural areas (66.2 years) and the lowest in other urban areas (58.5 years).",
  "Mental Well-being" = "The results for mental well-being scores were similar for all regions in 2023.",
  "GP access" = "As of 2023, 91% of people living in remote rural areas find it easy to contact their GP in the way they want. On the other hand, only 71% people living in other urban areas find it easy to contact their GP the way they want.",
  "Care impact on quality of life" = "The percentage of people who find that help, care or support maintained their quality of life decreased across geographical classifications between 2017 and 2021. Between 2021 and 2023 the percentage continued to fall or remain the same across most areas but increased by 6 percentage points in Remote Small Towns (Up from 63% to 69%).",
  "Access to nature" = "Access to green and blue spaces varies significantly across Scotland, with rural areas generally having better access to natural environments within walking distance.",
  "Out-of-hours healthcare Positive" = "As of 2023, perceptions of out of hours healthcare were generally positive across Scotland but more so in rural areas, with the highest percentage of positive experiences in remote small towns (82%). Accessible small towns received the lowest positive rating (72%).",
  "Out-of-hours healthcare Neutral" = "[placeholder]",
  "Out-of-hours healthcare Negative" = "[placeholder]",
   "Help with Everyday Living Positive" = "In 2023, the positive rating of care, support and help with everyday living was the highest in remote small towns (73%) and the lowest in accessible small towns (61%).",
  "Help with Everyday Living Neutral" = "[placeholder]",
  "Help with Everyday Living Negative" = "[placeholder]",
  "Care-Life Balance Positive" = "In 2023, the positive rating of balance between caring and other things in life was the highest in remote rural areas (63%) and the lowest in accessible small towns (57%).",
  "Care-Life Balance Neutral" = "[placeholder]",
  "Care-Life Balance Negative" = "[placeholder]",
  "Support to Continue Caring Positive" = "In 2023, the positive rating of feeling supported to continue caring was the highest in large urban areas (32%) and the lowest in accessible small towns (29%). It is worth noting that the most popular rating of this question was 'neutral' rather than 'positive' or 'negative'.",
  "Support to Continue Caring Neutral" = "[placeholder]",
  "Support to Continue Caring Negative" = "[placeholder]"
)

health_notes <- list("Quality of care" = "",
                     "Self-assessed health" = "",
                     "Healthy Life Expectancy (males)" = "",
                     "Healthy Life Expectancy (females)" = "",
                     "Mental Well-being" = tagList( 
                       tags$p("Mental wellbeing is calculated using average scores of the",
                              tags$a(href = "https://corc.uk.net/outcome-measures-guidance/directory-of-outcome-measures/short-warwick-edinburgh-mental-wellbeing-scale-swemwbs/Shortened", 
                                     target = "_blank",  "Warwick-Edinburgh Mental Wellbeing Scale (SWEMWBS)"))),
                     "GP access" = "",
                     "Care impact on quality of life" = "",
                     "Access to nature" = "",
                     "Out-of-hours healthcare" = "",
                     "Help with Everyday Living" = "",
                     "Care-Life Balance" = "",
                     "Support to Continue Caring" = ""

)

# Function to resolve file path 
get_health_file_path <- function(filename) {
  healthcare_path <- file.path("healthcare", filename)
  if (file.exists(healthcare_path)) {
    return(healthcare_path)
  }
  if (file.exists(filename)) {
    return(filename)
  }
  return(healthcare_path)
}

# Function to get display name for metrics
get_health_metric_display_name <- function(metric_name) {
  metric_info <- health_metrics[[metric_name]]
  if (!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# MISSING FUNCTION 1: Load self-assessed health data
load_self_assessed_health_data <- function() {
  cat("Loading self-assessed health data...\n")
  
  tryCatch({
    filepath <- get_health_file_path("self_assessed_health.xlsx")
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath, skip = 1)  # Skip title row
    
    # Based on console output: Row 2 has headers: Region, Response, 2012, 2013, etc.
    colnames(raw_data) <- c("Region", "Response", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Response)) %>%
      filter(Response == "Very good/Good") %>%  # Exact match from console
      gather(key = "Year", value = "Value", -Region, -Response) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),  # Already percentage, don't multiply by 100
        Area_Type = case_when(
          Region == "Large urban area" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible Small Towns" ~ "Accessible Small Towns",
          Region == "Remote Small Towns" ~ "Remote Small Towns",
          Region == "Accessible Rural Areas" ~ "Accessible Rural",
          Region == "Remote Rural Areas" ~ "Remote Rural",
          Region == "Scotland" ~ "Scotland",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Self-assessed Health"
      ) %>%
      filter(!is.na(Value), !is.na(Year), !is.na(Area_Type), Value > 0) %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "self-assessed health records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading self-assessed health data:", e$message, "\n")
    return(data.frame())
  })
}
# Load healthy life expectancy data
load_healthy_life_expectancy_data <- function(filename, gender) {
  cat("Loading healthy life expectancy data for", gender, "from:", filename, "\n")
  
  tryCatch({
    filepath <- get_health_file_path(filename)
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath)
    
    # Life expectancy data typically has: Region, Period, Value
    # Assuming columns: Region, 2015-17, 2016-18, 2017-19, 2018-20, 2019-21
    colnames(raw_data) <- c("Region", "2015-17", "2016-18", "2017-19", "2018-20", "2019-21")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      gather(key = "Period", value = "Value", -Region) %>%
      mutate(
        Year = case_when(
          Period == "2015-17" ~ 2016,
          Period == "2016-18" ~ 2017,
          Period == "2017-19" ~ 2018,
          Period == "2018-20" ~ 2019,
          Period == "2019-21" ~ 2020,
          TRUE ~ NA_real_
        ),
        Value = as.numeric(Value),
        Region_Clean = str_trim(Region),
        Area_Type = case_when(
          Region_Clean == "Large urban areas" ~ "Large Urban Areas",
          Region_Clean == "Other urban areas" ~ "Other Urban Areas",
          Region_Clean == "Accessible small towns" ~ "Accessible Small Towns",
          Region_Clean == "Remote small towns" ~ "Remote Small Towns",
          Region_Clean == "Accessible rural" ~ "Accessible Rural",
          Region_Clean == "Remote rural" ~ "Remote Rural",
          Region_Clean == "Scotland" ~ "Scotland",
          TRUE ~ Region_Clean
        ),
        Data_Source = paste("Scottish Government - Healthy Life Expectancy", paste0("(", gender, ")"))
      ) %>%
      filter(!is.na(Value), !is.na(Year), !is.na(Area_Type)) %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "healthy life expectancy records for", gender, "\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading healthy life expectancy data:", e$message, "\n")
    return(data.frame())
  })
}

# Load WEMWBS data
load_wemwbs_data <- function() {
  cat("Loading WEMWBS mental wellbeing data...\n")
  
  tryCatch({
    filepath <- get_health_file_path("warwick_edi.xlsx")
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath, skip = 1)  # Skip title row
    
    
    colnames(raw_data) <- c("Region", "Value_2023")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Value_2023)) %>%
      filter(!grepl("^(Region|Warwick)", Region)) %>%  # Skip header rows
      mutate(
        Year = 2023,
        Value = as.numeric(Value_2023),
        Area_Type = case_when(
          Region == "Large Urban Area" ~ "Large Urban Areas",
          Region == "Other Urban Area" ~ "Other Urban Areas",
          Region == "Accessible Small Town" ~ "Accessible Small Towns",
          Region == "Remote Small Town" ~ "Remote Small Towns",
          Region == "Accessible Rural Area" ~ "Accessible Rural",
          Region == "Remote Rural Area" ~ "Remote Rural",
          Region == "Scotland" ~ "Scotland",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - Mental Well-being (WEMWBS)"
      ) %>%
      filter(!is.na(Value), !is.na(Area_Type), Value > 0) %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "WEMWBS records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading WEMWBS data:", e$message, "\n")
    return(data.frame())
  })
}

# Load care impact data
load_care_impact_data <- function() {
  cat("Loading care impact data...\n")
  
  tryCatch({
    filepath <- get_health_file_path("care_impact.xlsx")
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath)
    
    colnames(raw_data) <- c("Region", "Response", "2017-18", "2019-20", "2021-22", "2023-24")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Response)) %>%
      gather(key = "Year_Range", value = "Value", -Region, -Response) %>%
      mutate(
        Year = case_when(
          Year_Range == "2017-18" ~ 2017,
          Year_Range == "2019-20" ~ 2019,
          Year_Range == "2021-22" ~ 2021,
          Year_Range == "2023-24" ~ 2023,
          TRUE ~ NA_real_
        ),
        Value = as.numeric(Value) * 100,
        Region_Clean = str_trim(Region),
        Area_Type = case_when(
          Region_Clean == "Large urban area" ~ "Large Urban Areas",
          Region_Clean == "Other urban areas" ~ "Other Urban Areas",
          Region_Clean == "Accessible Small Towns" ~ "Accessible Small Towns",
          Region_Clean == "Remote Small Towns" ~ "Remote Small Towns",
          Region_Clean == "Accessible Rural Areas" ~ "Accessible Rural",
          Region_Clean == "Remote Rural Areas" ~ "Remote Rural",
          Region_Clean == "Scotland" ~ "Scotland",
          TRUE ~ Region_Clean
        ),
        Data_Source = "Scottish Government - Care Impact on Quality of Life"
      ) %>%
      filter(!is.na(Value), !is.na(Year), !is.na(Area_Type), 
             Response %in% c("Improved/Maintained", "Positive")) %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "care impact records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading care impact data:", e$message, "\n")
    return(data.frame())
  })
}


# Load care experience data
load_care_experience_data <- function() {
  cat("Loading care experience data...\n")
  
  tryCatch({
    filepath <- get_health_file_path("care_impact.xlsx")
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath)
    
    colnames(raw_data) <- c("Region", "Response", "2017-18", "2019-20", "2021-22", "2023-24")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region), !is.na(Response)) %>%
      gather(key = "Year_Range", value = "Value", -Region, -Response) %>%
      mutate(
        Year = case_when(
          Year_Range == "2017-18" ~ 2017,
          Year_Range == "2019-20" ~ 2019,
          Year_Range == "2021-22" ~ 2021,
          Year_Range == "2023-24" ~ 2023,
          TRUE ~ NA_real_
        ),
        Value = as.numeric(Value) * 100,
        Region_Clean = str_trim(Region),
        Area_Type = case_when(
          Region_Clean == "Large urban area" ~ "Large Urban Areas",
          Region_Clean == "Other urban areas" ~ "Other Urban Areas",
          Region_Clean == "Accessible Small Towns" ~ "Accessible Small Towns",
          Region_Clean == "Remote Small Towns" ~ "Remote Small Towns",
          Region_Clean == "Accessible Rural Areas" ~ "Accessible Rural",
          Region_Clean == "Remote Rural Areas" ~ "Remote Rural",
          Region_Clean == "Scotland" ~ "Scotland",
          TRUE ~ Region_Clean
        ),
        Data_Source = "Scottish Government - care experience on Quality of Life"
      ) %>%
      filter(!is.na(Value), !is.na(Year), !is.na(Area_Type), 
             Response %in% c("Improved/Maintained", "Positive")) %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "care experience records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading care experience data:", e$message, "\n")
    return(data.frame())
  })
}





# Load 5min walk (Access to nature) data
load_5min_walk_data <- function() {
  cat("Loading 5min walk (access to nature) data...\n")
  
  tryCatch({
    filepath <- get_health_file_path("5min_walk.xlsx")
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath, skip = 1)  # Skip header row
    
    # Debug: Check what we actually read
    cat("Raw data columns:", paste(colnames(raw_data), collapse = ", "), "\n")
    cat("First few rows:\n")
    print(head(raw_data))
    
    # Handle different possible column structures
    if ("Region" %in% colnames(raw_data)) {
      # Standard structure: Region, Year columns
      processed_data <- raw_data %>%
        filter(!is.na(Region)) %>%
        # Convert Region and Year columns to long format
        gather(key = "Year", value = "Value", -Region) %>%
        mutate(
          Year = as.numeric(Year),
          Value = as.numeric(Value),
          Region_Clean = str_trim(Region)
        ) %>%
        filter(!is.na(Year), !is.na(Value)) %>%
        # Only keep actual area types, not intermediate headers
        filter(!grepl("^(Year|Region)$", Region_Clean, ignore.case = TRUE)) %>%
        mutate(
          Area_Type = case_when(
            Region_Clean == "Large urban areas" ~ "Large Urban Areas",
            Region_Clean == "Other urban areas" ~ "Other Urban Areas",
            Region_Clean == "Accessible small towns" ~ "Accessible Small Towns",
            Region_Clean == "Remote small towns" ~ "Remote Small Towns", 
            Region_Clean == "Accessible rural" ~ "Accessible Rural",
            Region_Clean == "Remote rural" ~ "Remote Rural",
            Region_Clean == "Scotland" ~ "Scotland",
            TRUE ~ Region_Clean
          ),
          Data_Source = "Scottish Government - Access to Nature (5min walk)"
        ) %>%
        # Filter out any rows where Area_Type is still the original Region_Clean (unmapped)
        filter(Area_Type != Region_Clean | Area_Type %in% c("Large Urban Areas", "Other Urban Areas", 
                                                           "Accessible Small Towns", "Remote Small Towns",
                                                           "Accessible Rural", "Remote Rural", "Scotland")) %>%
        select(Year, Area_Type, Value, Data_Source)
    } else {
      # Alternative structure - try different approach
      # Assume first column is areas, rest are years
      colnames(raw_data)[1] <- "Region"
      year_cols <- colnames(raw_data)[-1]
      
      processed_data <- raw_data %>%
        filter(!is.na(Region)) %>%
        gather(key = "Year", value = "Value", -Region) %>%
        mutate(
          Year = as.numeric(Year),
          Value = as.numeric(Value),
          Region_Clean = str_trim(Region)
        ) %>%
        filter(!is.na(Year), !is.na(Value)) %>%
        mutate(
          Area_Type = case_when(
            Region_Clean == "Large urban areas" ~ "Large Urban Areas",
            Region_Clean == "Other urban areas" ~ "Other Urban Areas",
            Region_Clean == "Accessible small towns" ~ "Accessible Small Towns",
            Region_Clean == "Remote small towns" ~ "Remote Small Towns",
            Region_Clean == "Accessible rural" ~ "Accessible Rural", 
            Region_Clean == "Remote rural" ~ "Remote Rural",
            Region_Clean == "Scotland" ~ "Scotland",
            TRUE ~ Region_Clean
          ),
          Data_Source = "Scottish Government - Access to Nature (5min walk)"
        ) %>%
        filter(Area_Type != Region_Clean | Area_Type %in% c("Large Urban Areas", "Other Urban Areas",
                                                           "Accessible Small Towns", "Remote Small Towns", 
                                                           "Accessible Rural", "Remote Rural", "Scotland")) %>%
        select(Year, Area_Type, Value, Data_Source)
    }
    
    cat("Successfully loaded", nrow(processed_data), "access to nature records\n")
    if (nrow(processed_data) > 0) {
      cat("Sample processed data:\n")
      print(head(processed_data))
    }
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading access to nature data:", e$message, "\n")
    return(data.frame())
  })
}
load_gp_access_data <- function() {
  cat("Loading GP access data...\n")
  
  tryCatch({
    filepath <- get_health_file_path("gp_access.xlsx")
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath, skip = 1)  # Skip title row
    
    # Based on console: Region, 2017-18, 2019-20, 2021-22, 2023-24
    colnames(raw_data) <- c("Region", "2017-18", "2019-20", "2021-22", "2023-24")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      filter(!grepl("^(Region|How easy)", Region)) %>%  # Skip header rows
      gather(key = "Year_Range", value = "Value", -Region) %>%
      mutate(
        Year = case_when(
          Year_Range == "2017-18" ~ 2017,
          Year_Range == "2019-20" ~ 2019,
          Year_Range == "2021-22" ~ 2021,
          Year_Range == "2023-24" ~ 2023,
          TRUE ~ NA_real_
        ),
        Value = as.numeric(Value) * 100,  # Convert decimal to percentage
        Area_Type = case_when(
          Region == "Large urban area" ~ "Large Urban Areas",
          Region == "Other urban areas" ~ "Other Urban Areas",
          Region == "Accessible Small Towns" ~ "Accessible Small Towns",
          Region == "Remote Small Towns" ~ "Remote Small Towns",
          Region == "Accessible Rural Areas" ~ "Accessible Rural",
          Region == "Remote Rural Areas" ~ "Remote Rural",
          Region == "Scotland" ~ "Scotland",
          TRUE ~ Region
        ),
        Data_Source = "Scottish Government - GP Access"
      ) %>%
      filter(!is.na(Value), !is.na(Year), !is.na(Area_Type), Value > 0) %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "GP access records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading GP access data:", e$message, "\n")
    return(data.frame())
  })
}


# Generic rating data loader for sub-metric files
load_rating_data <- function(filename, data_source_name, selected_rating = NULL) {
  cat("Loading rating data from:", filename, "for rating:", selected_rating, "\n")
  
  tryCatch({
    filepath <- get_health_file_path(filename)
    
    if (!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath, skip = 1)  # Skip title row
    
    # Based on console: Region, Negative, Neutral, Positive (3 rating columns)
    colnames(raw_data) <- c("Region", "Negative", "Neutral", "Positive")
    
    processed_data <- raw_data %>%
      filter(!is.na(Region)) %>%
      filter(!grepl("^(Region|Table)", Region)) %>%  # Skip header/title rows
      gather(key = "Rating", value = "Value", -Region) %>%
      mutate(
        Year = 2023,  # Single year data
        Value = as.numeric(Value),  # Already percentage
        Rating_Clean = str_trim(Rating),
        Area_Type = case_when(
          Region == "Large Urban Areas" ~ "Large Urban Areas",
          Region == "Other Urban Areas" ~ "Other Urban Areas",
          Region == "Accessible Small Towns" ~ "Accessible Small Towns",
          Region == "Remote Small Towns" ~ "Remote Small Towns",
          Region == "Accessible Rural Areas" ~ "Accessible Rural",
          Region == "Remote Rural Areas" ~ "Remote Rural",
          Region == "Scotland" ~ "Scotland",
          TRUE ~ Region
        ),
        Data_Source = data_source_name
      ) %>%
      filter(!is.na(Value), !is.na(Area_Type), Value > 0)
    
    # Filter by selected rating if provided
    if (!is.null(selected_rating) && selected_rating != "") {
      processed_data <- processed_data %>%
        filter(Rating_Clean == selected_rating)
    }
    
    processed_data <- processed_data %>%
      select(Year, Area_Type, Value, Data_Source)
    
    cat("Successfully loaded", nrow(processed_data), "rating records\n")
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading rating data:", e$message, "\n")
    return(data.frame())
  })
}

# Load health data function with proper function calls
load_health_6fold_data <- function(filename, selected_rating = NULL) {
  cat("Loading 6-fold health data from:", filename, "\n")
  
  tryCatch({
    filepath <- get_health_file_path(filename)
    
    if(grepl("care_experience", filename)) {
      return(load_care_experience_data())
    } else if(grepl("self_assessed_health", filename)) {
      return(load_self_assessed_health_data())
    } else if(grepl("healthy_male", filename)) {
      return(load_healthy_life_expectancy_data(filename, "males"))
    } else if(grepl("healthy_female", filename)) {
      return(load_healthy_life_expectancy_data(filename, "females"))
    } else if(grepl("warwick_edi", filename)) {
      return(load_wemwbs_data())
    } else if(grepl("gp_access", filename)) {
      return(load_gp_access_data())
    } else if(grepl("care_impact", filename)) {
      return(load_care_impact_data())
    } else if(grepl("5min_walk", filename)) {
      return(load_5min_walk_data())
    } else if(grepl("out_of_hours", filename)) {
      return(load_rating_data(filename, "Scottish Government - Out-of-hours Healthcare", selected_rating))
    } else if(grepl("everyday_living", filename)) {
      return(load_rating_data(filename, "Scottish Government - Help with Everyday Living", selected_rating))
    } else if(grepl("care_life_balance", filename)) {
      return(load_rating_data(filename, "Scottish Government - Care-Life Balance", selected_rating))
    } else if(grepl("support_continue_caring", filename)) {
      return(load_rating_data(filename, "Scottish Government - Support to Continue Caring", selected_rating))
    }
    
    return(data.frame())
    
  }, error = function(e) {
    cat("Error loading 6-fold health data:", e$message, "\n")
    return(data.frame())
  })
}

# Load 2-fold data (preserved for future developer use)
load_health_2fold_data <- function(filename, selected_rating = NULL) {
  cat("Loading 2-fold health data from:", filename, "\n")
  
  tryCatch({
    if(!file.exists(filename)) {
      cat("File not found:", filename, "\n")
      return(data.frame())
    }
    
    if(grepl("\\.xlsx?$", filename)) {
      raw_data <- read_excel(filename, skip = 1)
    } else {
      raw_data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
    }
    
    processed_data <- data.frame()
    
    if("Urban" %in% names(raw_data) && "Rural" %in% names(raw_data)) {
      for (i in 1:nrow(raw_data)) {
        year <- as.numeric(raw_data$Year[i])
        if (is.na(year)) next
        
        urban_val <- raw_data$Urban[i]
        scotland_val <- if("Scotland" %in% names(raw_data)) raw_data$Scotland[i] else NA
        
        if (!is.na(urban_val) && urban_val != "-" && urban_val != "*") {
          urban_row <- data.frame(
            Year = year,
            Area = "Urban",
            Value = as.numeric(urban_val),
            Data_Source = "Scottish Government - 2-fold",
            stringsAsFactors = FALSE
          )
          processed_data <- rbind(processed_data, urban_row)
        }
        
        if (!is.na(rural_val) && rural_val != "-" && rural_val != "*") {
          rural_row <- data.frame(
            Year = year,
            Area = "Rural",
            Value = as.numeric(rural_val),
            Data_Source = "Scottish Government - 2-fold",
            stringsAsFactors = FALSE
          )
          processed_data <- rbind(processed_data, rural_row)
        }
        
        if (!is.na(scotland_val) && scotland_val != "-" && scotland_val != "*") {
          scotland_row <- data.frame(
            Year = year,
            Area = "Scotland",
            Value = as.numeric(scotland_val),
            Data_Source = "Scottish Government - 2-fold",
            stringsAsFactors = FALSE
          )
          processed_data <- rbind(processed_data, scotland_row)
        }
      }
    }
    
    cat(paste("Loaded", nrow(processed_data), "2-fold health records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading 2-fold health data:", e$message, "\n")
    return(data.frame())
  })
}

# Create 3-fold (preserved for future developer use)
create_health_3fold <- function(data_2fold, data_6fold) {
  cat("Creating health 3-fold combination\n")
  
  if(nrow(data_2fold) == 0 || nrow(data_6fold) == 0) {
    cat("Missing 2-fold or 6-fold health data\n")
    return(data.frame())
  }
  
  urban_data <- data_2fold %>%
    filter(Area == "Urban") %>%
    mutate(Data_Source = "Scottish Government - 3-fold (Urban from 2-fold)")
  
  rural_data <- data_6fold %>%
    filter(Area_Type %in% c("Accessible Rural", "Remote Rural")) %>%
    rename(Area = Area_Type) %>%
    select(Year, Area, Value, Data_Source) %>%
    mutate(Data_Source = "Scottish Government - 3-fold (Rural from 6-fold)")
  
  scotland_data <- data_2fold %>%
    filter(Area == "Scotland") %>%
    mutate(Data_Source = "Scottish Government - 3-fold (Scotland from 2-fold)")
  
  combined_3fold <- bind_rows(urban_data, rural_data, scotland_data)
  
  cat(paste("Created", nrow(combined_3fold), "3-fold health records\n"))
  return(combined_3fold)
}

# Main data loading function
load_health_data_simple <- function(metric_name, classification_type, selected_rating = NULL) {
  metric_info <- health_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  if (classification_type == "6-fold") {
    return(load_health_6fold_data(metric_info$file, selected_rating))
  }
  
  # Preserved 2-fold and 3-fold logic for future developer use
  if (classification_type == "2-fold") {
    if (!is.null(metric_info$file_2fold)) {
      filepath_2fold <- get_health_file_path(metric_info$file_2fold)
      if (file.exists(filepath_2fold)) {
        return(load_health_2fold_data(filepath_2fold, selected_rating))
      } else {
        return(data.frame(
          Year = 2024,
          Area = "Coming Soon",
          Value = NA,
          Data_Source = "Coming Soon",
          stringsAsFactors = FALSE
        ))
      }
    }
  } else if (classification_type == "3-fold") {
    filepath_2fold <- get_health_file_path(metric_info$file_2fold)
    if (file.exists(filepath_2fold)) {
      data_2fold <- load_health_2fold_data(filepath_2fold, selected_rating)
      data_6fold <- load_health_6fold_data(metric_info$file, selected_rating)
      return(create_health_3fold(data_2fold, data_6fold))
    } else {
      return(data.frame(
        Year = 2024,
        Area = "Coming Soon", 
        Value = NA,
        Data_Source = "Coming Soon",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(data.frame())
}

# Process data reactively
simple_aggregate_health_data <- function(processed_data, classification_type = "6-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  if("Coming Soon" %in% unique(processed_data$Area)) {
    return(processed_data)
  }
  
  # FIXED: Rename Area_Type to Area consistently
  if("Area_Type" %in% names(processed_data)) {
    processed_data <- processed_data %>%
      rename(Area = Area_Type)
  }
  
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}
# Get key insights (preserved for future developer use)
get_health_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  if("Coming Soon" %in% unique(processed_data$Area)) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  unique_areas <- unique(latest_data$Area)
  
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
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
  } else if (any(c("Accessible Rural", "Remote Rural") %in% unique_areas) && "Urban" %in% unique_areas) {
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    
    rural_areas <- latest_data %>% 
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    if(nrow(rural_areas) > 0) {
      rural_val <- mean(rural_areas$Value, na.rm = TRUE)
    }
    
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
  }
  
  return(list(
    urban = ifelse(!is.na(urban_val), urban_val, NA),
    rural = ifelse(!is.na(rural_val), rural_val, NA),
    scotland = ifelse(!is.na(scotland_val), scotland_val, NA),
    year = latest_year
  ))
}

# Create color mapping with Scotland as gray
get_health_colors <- function(areas, classification_type) {
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
  }
  
  color_mapping[["Scotland"]] <- "#B2B2B2"
  
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}

# Format values for display
format_health_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name %in% c("Healthy Life Expectancy (males)", "Healthy Life Expectancy (females)")) {
    return(paste0(round(value, 1), " years"))
  } else if (metric_name == "Mental Well-being") {
    return(round(value, 1))
  } else {
    return(paste0(round(value, 1), "%"))
  }
}

# Calculate gap between urban and rural (preserved for future developer use)
calculate_health_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name %in% c("Healthy Life Expectancy (males)", "Healthy Life Expectancy (females)")) {
    return(paste0(round(gap, 1), " years"))
  } else if (metric_name == "Mental Well-being") {
    return(round(gap, 1))
  } else {
    return(paste0(round(gap, 1), "pp"))
  }
}

# UI Components 
health_dashboard_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    tags$head(
      tags$style(HTML("
      .selectize-dropdown { z-index: 9999 !important; }
      .selectize-control.single .selectize-dropdown { z-index: 9999 !important; }
      .category-header { overflow: visible !important; }
      .category-header-overlay { overflow: visible !important; }
      
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
      
      .sidebar-collapse .category-header {
        left: 0 !important;
        width: 100vw !important;
      }
      
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
      
      .health-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .health-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .health-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .health-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .health-metric-description {
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
            h2("Health and Social Care", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("health_metric", "Health Metric:", 
                        choices = c("Select a policy metric..." = "", names(health_metrics)), 
                        selected = "", width = "220px")
          ),
          
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.health_metric == 'Out-of-hours healthcare' || input.health_metric == 'Help with Everyday Living' || input.health_metric == 'Care-Life Balance' || input.health_metric == 'Support to Continue Caring'",
              selectInput("selected_rating", "Sub-metric:", 
                          choices = names(rating_sub_metrics), selected = names(rating_sub_metrics)[1], width = "180px")
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
    
    # Health UI - Display all 12 metrics in grid instead of just 8
    
    
    conditionalPanel(
      condition = "input.health_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore health and social care data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px;",
                
                # Row 1
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Quality of care', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("heart", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Quality of care"),
                      div(class = "health-metric-description", 
                          "Experience and satisfaction with care services across Scotland's health and social care system.")
                    )
                  )
                ),
                
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Self-assessed health', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("user-md", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Self-assessed health"),
                      div(class = "health-metric-description",
                          "How people rate their own general health status across urban and rural communities.")
                    )
                  )
                ),
                
                # Row 2
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Healthy Life Expectancy (males)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("male", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Healthy Life Expectancy (males)"),
                      div(class = "health-metric-description",
                          "Average years males can expect to live in good health across different area types.")
                    )
                  )
                ),
                
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Healthy Life Expectancy (females)', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("female", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Healthy Life Expectancy (females)"),
                      div(class = "health-metric-description",
                          "Average years females can expect to live in good health across different area types.")
                    )
                  )
                ),
                
                # Row 3
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Mental Well-being', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("brain", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Mental Well-being"),
                      div(class = "health-metric-description",
                          "WEMWBS scores measuring mental wellbeing across Scotland's communities.")
                    )
                  )
                ),
                
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'GP access', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("stethoscope", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "GP access"),
                      div(class = "health-metric-description",
                          "Ease of contacting General Practice services in preferred manner across areas.")
                    )
                  )
                ),
                
                # Row 4
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Care impact on quality of life', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("hands-helping", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Care impact on quality of life"),
                      div(class = "health-metric-description",
                          "How care and support services improve or maintain people's quality of life.")
                    )
                  )
                ),
                
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Access to nature', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("tree", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Access to nature"),
                      div(class = "health-metric-description",
                          "Proportion of adults living within 5 minutes' walk of green or blue space.")
                    )
                  )
                ),
                
                # Row 5 - NEW METRICS ADDED
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Out-of-hours healthcare', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("clock", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Out-of-hours healthcare"),
                      div(class = "health-metric-description",
                          "Experience and satisfaction with out-of-hours healthcare services.")
                    )
                  )
                ),
                
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Help with Everyday Living', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("home", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Help with Everyday Living"),
                      div(class = "health-metric-description",
                          "Rating of care, support and help with everyday living activities.")
                    )
                  )
                ),
                
                # Row 6 - FINAL 2 METRICS
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Care-Life Balance', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("balance-scale", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Care-Life Balance"),
                      div(class = "health-metric-description",
                          "Rating of balance between caring responsibilities and other life activities.")
                    )
                  )
                ),
                
                div(
                  class = "health-metrics-card",
                  onclick = "Shiny.setInputValue('health_metric_select', 'Support to Continue Caring', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("hands-heart", class = "health-metric-icon"),
                    div(
                      div(class = "health-metric-title", "Support to Continue Caring"),
                      div(class = "health-metric-description",
                          "Rating of feeling supported to continue providing care to others.")
                    )
                  )
                )
              )
            )
          )
      )
    ),
    
    conditionalPanel(
      condition = "input.health_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("health_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("health_data_summary"))
          ),
          
          conditionalPanel(
            condition = "input.health_metric != 'Out-of-hours healthcare' && input.health_metric != 'Help with Everyday Living' && input.health_metric != 'Care-Life Balance' && input.health_metric != 'Support to Continue Caring' && input.health_metric != 'Mental Well-being'",
            fluidRow(
              box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                              uiOutput("health_trend_title"),
                              div(style = "position: absolute; right: 20px;",
                                  downloadButton("health_trend_download", "Download", class = "excel-download-btn"
                                  ))), 
                  status = "primary", solidHeader = TRUE, width = 12,
                  plotlyOutput("health_trend_chart") %>% withSpinner())
            )
          ),
          
          fluidRow( 
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                            uiOutput("health_comparison_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("health_comparison_download", "Download", class = "excel-download-btn"
                                ))),
                status = "primary", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;", uiOutput("health_year_selector")),
                plotlyOutput("health_comparison_chart") %>% withSpinner())
          ),
          
          fluidRow(
            box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 15px;",
                            uiOutput("health_table_title"),
                            div(style = "position: absolute; right: 20px;",
                                downloadButton("health_table_download", "Download", class = "excel-download-btn"
                                ))),
                status = "info", solidHeader = TRUE, width = 12,
                div(style = "margin-bottom: 15px;",
                    fluidRow(
                      column(6, uiOutput("health_table_year_filter")),
                      column(6, uiOutput("health_table_area_filter"))
                    )),
                DT::dataTableOutput("health_data_table") %>% withSpinner())
          )
      )
    )
  )
}

# Server Functions
health_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  health_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  observeEvent(input$health_metric_select, {
    updateSelectInput(session, "health_metric", selected = input$health_metric_select)
  })
  
  output$health_summary_title <- renderUI({
    req(input$health_metric)
    display_name <- get_health_metric_display_name(input$health_metric)
    h3(paste(display_name, "(6-fold Classification)"), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$health_year_selector <- renderUI({
    req(health_values$processed_data)
    if (nrow(health_values$processed_data) == 0) return(NULL)
    if("Coming Soon" %in% unique(health_values$processed_data$Area)) return(NULL)
    available_years <- sort(unique(health_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("health_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$health_trend_title <- renderUI({
    req(input$health_metric)
    display_name <- get_health_metric_display_name(input$health_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$health_comparison_title <- renderUI({
    req(input$health_selected_year, input$health_metric)
    display_name <- get_health_metric_display_name(input$health_metric)
    paste0("Single Year Comparison (", input$health_selected_year, ") for ", display_name)
  })
  
  output$health_table_title <- renderUI({
    req(input$health_metric)
    display_name <- get_health_metric_display_name(input$health_metric)
    paste("Data Table for", display_name)
  })
  
  output$health_table_year_filter <- renderUI({
    req(health_values$processed_data)
    if(nrow(health_values$processed_data) == 0) return(NULL)
    if("Coming Soon" %in% unique(health_values$processed_data$Area)) return(NULL)
    all_years <- sort(unique(health_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("health_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$health_table_area_filter <- renderUI({
    req(health_values$processed_data)
    if(nrow(health_values$processed_data) == 0) return(NULL)
    if("Coming Soon" %in% unique(health_values$processed_data$Area)) return(NULL)
    agg_data <- simple_aggregate_health_data(health_values$processed_data, "6-fold")
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("health_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  observe({
    if (is.null(input$health_metric) || input$health_metric == "") {
      health_values$processed_data <- data.frame()
      health_values$data_status <- "Please select a metric"
      return()
    }
    
    if (input$health_metric %in% c("Out-of-hours healthcare", "Help with Everyday Living", "Care-Life Balance", "Support to Continue Caring")) {
      req(input$selected_rating)
      selected_rating <- rating_sub_metrics[input$selected_rating]
      health_values$processed_data <- load_health_data_simple(
        input$health_metric, 
        "6-fold", 
        selected_rating
      )
    } else {
      health_values$processed_data <- load_health_data_simple(
        input$health_metric, 
        "6-fold"
      )
    }
    
    health_values$data_status <- if(nrow(health_values$processed_data) > 0) "Health data loaded" else "No data available"
  })
  
  # Data summary with source information
  output$health_data_summary <- renderUI({
    req(health_values$processed_data, input$health_metric)
    
    if (nrow(health_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_health_metric_display_name(input$health_metric)
    custom_insight <- ifelse(input$health_metric %in% c("Out-of-hours healthcare", "Help with Everyday Living", "Care-Life Balance", "Support to Continue Caring"),
                             health_key_insights[[paste(input$health_metric, input$selected_rating) ]],
                             health_key_insights[[input$health_metric]])
    custom_notes <- health_notes[[input$health_metric]]
    
    #  source information from metric configuration
    metric_info <- health_metrics[[input$health_metric]]
    source_info <- if (!is.null(metric_info$source_text) && !is.null(metric_info$source_url)) {
      list(
        text = metric_info$source_text,
        url = metric_info$source_url
      )
    } else {
      list(
        text = "Scottish Government Health and Social Care Data",
        url = "https://www.gov.scot/collections/health-and-social-care-statistics/"
      )
    }
    
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
    
    return(div(h4("Health Data Loaded"), p(paste("Showing data for:", display_name))))
  })
  
  output$health_trend_chart <- renderPlotly({
    req(health_values$processed_data)
    
    if("Coming Soon" %in% unique(health_values$processed_data$Area)) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "Coming Soon - 2-fold data files not yet available", textfont = list(size = 16)))
    }
    
    tryCatch({
      agg_data <- simple_aggregate_health_data(health_values$processed_data, "6-fold")
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      is_life_expectancy <- input$health_metric %in% c("Healthy Life Expectancy (males)", "Healthy Life Expectancy (females)")
      is_wemwbs <- input$health_metric == "Mental Well-being"
      
      if (is_life_expectancy) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Years"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Life Expectancy: ", agg_data$Value_Rounded, " years")
      } else if (is_wemwbs) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "WEMWBS Score"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Score: ", agg_data$Value_Rounded)
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text
      
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
          if(length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
        })
      
      area_colors <- get_health_colors(unique(agg_data$Area), "6-fold")
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      if (is_life_expectancy || is_wemwbs) {
        p <- p + scale_y_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in health trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  output$health_comparison_chart <- renderPlotly({
    req(health_values$processed_data, input$health_selected_year)
    
    if("Coming Soon" %in% unique(health_values$processed_data$Area)) {
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "Coming Soon - 2-fold data files not yet available", textfont = list(size = 16)))
    }
    
    tryCatch({
      agg_data <- simple_aggregate_health_data(health_values$processed_data, "6-fold")
      selected_data <- agg_data %>% filter(Year == as.numeric(input$health_selected_year))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$health_selected_year), textfont = list(size = 16)))
      }
      
      is_life_expectancy <- input$health_metric %in% c("Healthy Life Expectancy (males)", "Healthy Life Expectancy (females)")
      is_wemwbs <- input$health_metric == "Mental Well-being"
      
      if (is_life_expectancy) {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "Years"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Life Expectancy: ", selected_data$Value_Rounded, " years")
      } else if (is_wemwbs) {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "WEMWBS Score"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Score: ", selected_data$Value_Rounded)
      } else {
        selected_data$Value_Rounded <- round(selected_data$Value, 0)
        x_label <- "Percentage (%)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Rate: ", selected_data$Value_Rounded, "%")
      }
      
      selected_data$tooltip <- tooltip_text
      
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Area, Value), fill = Area, text = tooltip)) +
        geom_col(alpha = 0.8, width = 0.7) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "", x = x_label)
      
      area_colors <- get_health_colors(unique(selected_data$Area), "6-fold")
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      if (is_life_expectancy || is_wemwbs) {
        p <- p + scale_x_continuous(labels = scales::comma_format())
      } else {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1, acuracy = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in health comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
 
  # Health Trend Download
  output$health_trend_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$health_metric)
      paste0("Health_Trend_", metric, "_6-fold_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(health_values$processed_data)
      
      if ("Coming Soon" %in% unique(health_values$processed_data$Area)) {
        showNotification("Data not yet available for download", type = "warning")
        return(NULL)
      }
      
      data <- simple_aggregate_health_data(health_values$processed_data, "6-fold") %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Health Comparison Download
  output$health_comparison_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$health_metric)
      paste0("Health_Comparison_", metric, "_6-fold_", input$health_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(health_values$processed_data, input$health_selected_year)
      
      if ("Coming Soon" %in% unique(health_values$processed_data$Area)) {
        showNotification("Data not yet available for download", type = "warning")
        return(NULL)
      }
      
      data <- simple_aggregate_health_data(health_values$processed_data, "6-fold") %>%
        filter(Year == as.numeric(input$health_selected_year)) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Health Table Download
  output$health_table_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$health_metric)
      paste0("Health_Table_", metric, "_6-fold_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(health_values$processed_data)
      
      if ("Coming Soon" %in% unique(health_values$processed_data$Area)) {
        showNotification("Data not yet available for download", type = "warning")
        return(NULL)
      }
      
      data <- simple_aggregate_health_data(health_values$processed_data, "6-fold")
      
      if (!is.null(input$health_table_year_filter) && input$health_table_year_filter != "all") {
        data <- data %>% filter(Year == as.numeric(input$health_table_year_filter))
      }
      if (!is.null(input$health_table_area_filter) && input$health_table_area_filter != "all") {
        data <- data %>% filter(Area == input$health_table_area_filter)
      }
      
      data <- data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  output$health_data_table <- DT::renderDataTable({
    req(health_values$processed_data)
    
    if("Coming Soon" %in% unique(health_values$processed_data$Area)) {
      return(DT::datatable(data.frame(Message = "Coming Soon - 2-fold data files not yet available"), options = list(dom = 't')))
    }
    
    if(nrow(health_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_health_metric_display_name(input$health_metric)
    agg_data <- simple_aggregate_health_data(health_values$processed_data, "6-fold")
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    filtered_data <- agg_data
    
    if(!is.null(input$health_table_year_filter) && input$health_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$health_table_year_filter))
    }
    
    if(!is.null(input$health_table_area_filter) && input$health_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$health_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    if (input$health_metric %in% c("Healthy Life Expectancy (males)", "Healthy Life Expectancy (females)")) {
      value_col_name <- "Years"
      table_data$Value <- round(table_data$Value, 1)
    } else if (input$health_metric == "Mental Well-being") {
      value_col_name <- "WEMWBS Score"
      table_data$Value <- round(table_data$Value, 1)
    } else {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 0)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by 6-fold Classification"))
    
    dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    
    return(dt)
  })
}




