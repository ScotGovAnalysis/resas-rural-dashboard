
# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Social Justice metrics configuration
social_justice_metrics <- list(
  "Relative poverty" = list(
    file_2fold = "social_justice/poverty_adult.csv",
    classifications = c("2-fold"),
    full_name = "Proportion (%) of people who are in relative poverty"
  ),
  "Relative child poverty" = list(
    file_2fold = "social_justice/poverty_child.csv",
    classifications = c("2-fold"),
    full_name = "Proportion (%) of children who are in relative poverty"
  ),
  "Housing affordability" = list(
    file_2fold = "social_justice/affordability_ratio.xlsx",
    classifications = c("2-fold"),
    full_name = "Housing costs as percentage of earnings"
  ),
  "Influence over local decisions" = list(
    file_6fold = "social_justice/influnce_decisions_6_fold.csv",
    classifications = c("6-fold"),
    full_name = "Perceptions of influence over decisions affecting local area",
    has_sub_metrics = TRUE
  ),
  "Managing financially" = list(
    file_6fold = "social_justice/managing_financially_6_fold.csv",
    classifications = c("6-fold"),
    full_name = "How the household is managing financially"
  ),
  "Community ownership" = list(
    file_6fold = "social_justice/community_ownership.csv",
    classifications = c("2-fold", "3-fold", "6-fold"),
    full_name = "Number of assets in community ownership"
  )
)

# Static Key Insights for metrics 
social_justice_key_insights <- list(
  "Relative poverty" = "Despite a marked, although temporary, increase in relative poverty among all individuals in rural areas between 2015 and 2021, the relative poverty rate is higher in urban areas.",
  
  "Relative child poverty" = "The rate of relative child poverty remains higher in urban areas than in rural areas",
  
  "Housing affordability" = "The percentage of earnings urban households spend on their housing continues to be higher compared to rural households, although over time, all households have seen declining trend.",
  
  "Influence over local decisions" = "insight placeholder",
  
  "Managing financially" = "insight placeholder",
  
  "Community ownership" = "insight placeholder"
)

# Static Notes for metrics (now including all metrics)
social_justice_notes <- list(
  "Relative poverty" = "Estimates are averages of three years worth of data in order to reduce data volatility and show trends more accurately. Estimates for 2018-21 to 2020-23 are two-year averages due to the impact of the pandemic on the survey.",
  
  "Relative child poverty" = "Estimates are averages of three years worth of data in order to reduce data volatility and show trends more accurately. Estimates for 2018-21 to 2020-23 are two-year averages due to the impact of the pandemic on the survey.",
  
  "Housing affordability" = "notes placeholder",
  
  "Influence over local decisions" = "notes placeholder",
  
  "Managing financially" = "notes placeholder",
  
  "Community ownership" = "notes placeholder"
)

# Sub-metrics for influence over local decisions
influence_sub_metrics <- c(
  "I can influence decisions affecting my local area" = "I can influence decisions affecting my local area",
  "I would like to be more involved in decisions" = "I would like to be more involved in the decisions my council makes that affect my local area"
)

# Function to extract year from year range
extract_final_year <- function(year_string) {
  if(grepl("-", year_string)) {
    parts <- strsplit(year_string, "-")[[1]]
    if(length(parts) == 2) {
      first_year <- as.numeric(parts[1])
      last_part <- as.numeric(parts[2])
      
      if(last_part < 100) {
        century <- floor(first_year / 100) * 100
        return(century + last_part)
      } else {
        return(last_part)
      }
    }
  }
  return(as.numeric(year_string))
}

# Function to get display name for metrics
get_social_justice_metric_display_name <- function(metric_name) {
  metric_info <- social_justice_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

# Helper functions for number formatting
format_social_justice_number <- function(number, metric_name) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  
  if (metric_name == "Community ownership") {
    return(scales::comma(round(number, 0)))
  } else if (metric_name %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")) {
    return(paste0(round(number, 1), "%"))
  } else {
    return(round(number, 2))
  }
}

format_full_number <- function(number) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  return(scales::comma(round(number, 0)))
}

# Create color mapping with Scotland as gray
get_social_justice_colors <- function(areas, classification_type) {
  color_mapping <- list()
  
  # Get base colors from config.R if available (like culture module)
  if (classification_type == "6-fold") {
    colors <- get_classification_colors("6-fold")
    if (!is.null(colors)) {
      color_mapping[["Large Urban Areas"]] <- colors[["Large_Urban_Areas"]]
      color_mapping[["Other Urban Areas"]] <- colors[["Other_Urban_Areas"]]
      color_mapping[["Accessible Small Towns"]] <- colors[["Accessible_Small_Towns"]]
      color_mapping[["Remote Small Towns"]] <- colors[["Remote_Small_Towns"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    } else {
      # Fallback colors if config not available
      color_mapping[["Large Urban Areas"]] <- "#1f77b4"
      color_mapping[["Other Urban Areas"]] <- "#ff7f0e"
      color_mapping[["Accessible Small Towns"]] <- "#2ca02c"
      color_mapping[["Remote Small Towns"]] <- "#d62728"
      color_mapping[["Accessible Rural"]] <- "#9467bd"
      color_mapping[["Remote Rural"]] <- "#8c564b"
    }
  } else if (classification_type == "3-fold") {
    colors <- get_classification_colors("3-fold")
    if (!is.null(colors)) {
      color_mapping[["Urban"]] <- colors[["Urban"]]
      color_mapping[["Accessible Rural"]] <- colors[["Accessible_Rural"]]
      color_mapping[["Remote Rural"]] <- colors[["Remote_Rural"]]
    } else {
      color_mapping[["Urban"]] <- "#1f77b4"
      color_mapping[["Accessible Rural"]] <- "#9467bd"
      color_mapping[["Remote Rural"]] <- "#8c564b"
    }
  } else if (classification_type == "2-fold") {
    colors <- get_classification_colors("2-fold")
    if (!is.null(colors)) {
      color_mapping[["Urban"]] <- colors[["Urban"]]
      color_mapping[["Rural"]] <- colors[["Rural"]]
    } else {
      color_mapping[["Urban"]] <- "#1f77b4"
      color_mapping[["Rural"]] <- "#ff7f0e"
    }
  }
  
  # Always set Scotland to gray
  color_mapping[["Scotland"]] <- "#B2B2B2"
  
  # Return only colors for areas that exist in the data
  final_colors <- color_mapping[names(color_mapping) %in% areas]
  return(final_colors[!sapply(final_colors, is.null)])
}
# Part 2: Data Loading Functions

# Load poverty data (adult or child) 
load_poverty_data <- function(filename, data_type) {
  filepath <- file.path("social_justice", filename)
  cat("Loading poverty data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE)
    
    # Find the table 'a' data (first table with relative poverty rates)
    table_a_start <- which(grepl("Relative poverty rate.*Proportion", raw_data[,1]))
    
    if(length(table_a_start) == 0) {
      cat("Could not find table 'a' in", filepath, "\n")
      return(data.frame())
    }
    
    header_row <- table_a_start[1] + 1
    data_start_row <- header_row + 1
    
    headers <- as.character(raw_data[header_row, ])
    headers <- headers[headers != ""]
    
    data_rows <- raw_data[data_start_row:(data_start_row + 2), 1:length(headers)]
    colnames(data_rows) <- headers
    
    processed_data <- data_rows %>%
      filter(!is.na(Group), Group %in% c("All", "Urban", "Rural")) %>%
      gather(key = "Year_Range", value = "Value", -Group) %>%
      mutate(
        Year_Range = as.character(Year_Range),
        Value = as.numeric(gsub("%", "", Value)),
        Year = sapply(Year_Range, extract_final_year),
        Area_Type = case_when(
          Group == "Urban" ~ "Urban",
          Group == "Rural" ~ "Rural", 
          Group == "All" ~ "Scotland"
        ),
        Data_Source = paste0("Scottish Government - ", 
                           ifelse(data_type == "adult", "Adult", "Child"), 
                           " Relative Poverty")
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area = Area_Type, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "poverty records from", filepath, "\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading poverty data from", filepath, ":", e$message, "\n")
    return(data.frame())
  })
}

# Load housing affordability data from Excel 
load_housing_affordability_data <- function() {
  filepath <- file.path("social_justice", "affordability_ratio.xlsx")
  cat("Loading housing affordability data from Excel:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read_excel(filepath, sheet = "Table", col_names = FALSE)
    
    cat("Excel file dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    data_rows <- raw_data[7:51, 1:4]  # Columns A-D, rows 8-52
    colnames(data_rows) <- c("Period", "Area", "Sample", "Ratio")
    
    processed_data <- data_rows %>%
      filter(!is.na(Period), !is.na(Area), !is.na(Ratio)) %>%
      filter(Area %in% c("Urban", "Rural", "All")) %>%
      mutate(
        Period = as.character(Period),
        Area_Original = as.character(Area),
        Ratio = as.numeric(Ratio),
        Year = sapply(Period, function(p) {
          if(grepl("-", p)) {
            parts <- strsplit(p, "-")[[1]]
            if(length(parts) == 2) {
              first_year <- as.numeric(parts[1])
              last_part <- as.numeric(parts[2])
              if(last_part < 100) {
                century <- floor(first_year / 100) * 100
                return(century + last_part)
              } else {
                return(last_part)
              }
            }
          }
          return(as.numeric(p))
        }),
        Area = case_when(
          Area_Original == "Urban" ~ "Urban",
          Area_Original == "Rural" ~ "Rural",
          Area_Original == "All" ~ "Scotland"
        ),
        Data_Source = "Scottish Government - Housing Affordability Ratio (Family Resources Survey)",
        Value = round(Ratio * 100, 1)
      ) %>%
      filter(!is.na(Year), !is.na(Value)) %>%
      select(Year, Area, Value, Data_Source, Period)
    
    cat(paste("Successfully processed", nrow(processed_data), "housing affordability records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading housing affordability data:", e$message, "\n")
    return(data.frame())
  })
}

# Load managing financially data from the new 6-fold file format
load_managing_financially_data_new <- function() {
  filepath <- "social_justice/managing_financially_6_fold.csv"
  cat("Loading managing financially data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE)
    
    # Remove the first unnamed column if it exists
    if(names(raw_data)[1] == "X" || names(raw_data)[1] == "") {
      raw_data <- raw_data[, -1]
    }
    
    cat("Raw data dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
    
    # Process the data - focus on "Manages well" status and Scotland-level data
    processed_data <- raw_data %>%
      filter(!is.na(Year), Council == "Scotland", Status == "Manages well") %>%
      gather(key = "Area", value = "Value", 
             Large.urban.areas, Other.urban.areas, Accessible.small.towns, 
             Remote.small.towns, Accessible.rural, Remote.rural) %>%
      mutate(
        Area = case_when(
          Area == "Large.urban.areas" ~ "Large Urban Areas",
          Area == "Other.urban.areas" ~ "Other Urban Areas", 
          Area == "Accessible.small.towns" ~ "Accessible Small Towns",
          Area == "Remote.small.towns" ~ "Remote Small Towns",
          Area == "Accessible.rural" ~ "Accessible Rural",
          Area == "Remote.rural" ~ "Remote Rural"
        ),
        Value = as.numeric(Value),
        Data_Source = "Scottish Household Survey - Managing Financially"
      ) %>%
      filter(!is.na(Value), Value > 0) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "managing financially records\n"))
    
    if(nrow(processed_data) > 0) {
      cat("Sample data:\n")
      print(head(processed_data))
    }
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading managing financially data:", e$message, "\n")
    return(data.frame())
  })
}

# Load influence decisions data from the new 6-fold file format
load_perceptions_data_new <- function(selected_statement = NULL) {
  filepath <- "social_justice/influnce_decisions_6_fold.csv"
  cat("Loading perceptions data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read the CSV properly with standard CSV parsing (not manual line splitting)
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, header = TRUE)
    
    cat("Raw data dimensions:", nrow(raw_data), "x", ncol(raw_data), "\n")
    cat("Column names:", paste(names(raw_data), collapse = ", "), "\n")
    
    # Clean up column names (remove any extra quotes or spaces)
    names(raw_data) <- gsub('"', '', names(raw_data))
    names(raw_data) <- trimws(names(raw_data))
    
    # Set proper column names based on the file structure
    # Columns: "", Year, Council, "", Large urban areas, Other urban areas, Accessible small towns, Remote small towns, Accessible rural, Remote rural
    expected_cols <- c("ID", "Year", "Council", "Statement", "Large.urban.areas", "Other.urban.areas", 
                       "Accessible.small.towns", "Remote.small.towns", "Accessible.rural", "Remote.rural")
    
    if (ncol(raw_data) >= 10) {
      names(raw_data)[1:10] <- expected_cols
    }
    
    cat("Updated column names:", paste(names(raw_data), collapse = ", "), "\n")
    cat("Sample raw data:\n")
    print(head(raw_data, 3))
    
    # Process the data
    processed_data <- raw_data %>%
      filter(!is.na(Year), Council == "Scotland", !is.na(Statement), Statement != "") %>%
      # Clean the statement text
      mutate(Statement = trimws(gsub('"', '', Statement))) %>%
      # Filter by selected statement if specified
      {if (!is.null(selected_statement)) {
        cat("Filtering for statement:", selected_statement, "\n")
        cat("Available statements:", paste(unique(.$Statement), collapse = "; "), "\n")
        filter(., grepl(selected_statement, Statement, fixed = TRUE))
      } else .} %>%
      # Convert to long format
      gather(key = "Area", value = "Value", 
             Large.urban.areas, Other.urban.areas, Accessible.small.towns, 
             Remote.small.towns, Accessible.rural, Remote.rural) %>%
      # Clean area names and values
      mutate(
        Area = case_when(
          Area == "Large.urban.areas" ~ "Large Urban Areas",
          Area == "Other.urban.areas" ~ "Other Urban Areas", 
          Area == "Accessible.small.towns" ~ "Accessible Small Towns",
          Area == "Remote.small.towns" ~ "Remote Small Towns",
          Area == "Accessible.rural" ~ "Accessible Rural",
          Area == "Remote.rural" ~ "Remote Rural"
        ),
        Value = as.numeric(gsub('"', '', Value)),
        Year = as.numeric(gsub('"', '', Year)),
        Data_Source = "Scottish Household Survey - Local Influence"
      ) %>%
      filter(!is.na(Value), Value > 0, !is.na(Year)) %>%
      select(Year, Area, Value, Statement, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "perceptions records\n"))
    
    if(nrow(processed_data) > 0) {
      cat("Sample processed data:\n")
      print(head(processed_data, 6))
      cat("Unique statements:", paste(unique(processed_data$Statement), collapse = "; "), "\n")
      cat("Years found:", paste(sort(unique(processed_data$Year)), collapse = ", "), "\n")
    }
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading perceptions data:", e$message, "\n")
    print(e)
    return(data.frame())
  })
}

# Keep existing community ownership data loading function
load_community_ownership_data <- function() {
  filepath <- file.path("social_justice", "community_ownership.csv")
  cat("Loading community ownership data from", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE)
    
    cat("Raw data dimensions:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")
    
    headers <- as.character(raw_data[4, ])
    data_rows <- raw_data[5:nrow(raw_data), ]
    
    colnames(data_rows) <- c("Year", "Large_Urban_Areas", "Other_Urban_Areas", 
                            "Accessible_Small_Towns", "Remote_Small_Towns", 
                            "Accessible_Rural", "Remote_Rural")
    
    processed_data <- data_rows %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(!is.na(Year), Year >= 2000, Year <= 2023) %>%
      mutate(
        Large_Urban_Areas = as.numeric(Large_Urban_Areas),
        Other_Urban_Areas = as.numeric(Other_Urban_Areas),
        Accessible_Small_Towns = as.numeric(Accessible_Small_Towns),
        Remote_Small_Towns = as.numeric(Remote_Small_Towns),
        Accessible_Rural = as.numeric(Accessible_Rural),
        Remote_Rural = as.numeric(Remote_Rural)
      ) %>%
      filter(!is.na(Large_Urban_Areas), !is.na(Other_Urban_Areas), !is.na(Accessible_Rural), !is.na(Remote_Rural)) %>%
      gather(key = "Area", value = "Value", 
             Large_Urban_Areas, Other_Urban_Areas, Accessible_Small_Towns, 
             Remote_Small_Towns, Accessible_Rural, Remote_Rural) %>%
      mutate(
        Area = case_when(
          Area == "Large_Urban_Areas" ~ "Large Urban Areas",
          Area == "Other_Urban_Areas" ~ "Other Urban Areas", 
          Area == "Accessible_Small_Towns" ~ "Accessible Small Towns",
          Area == "Remote_Small_Towns" ~ "Remote Small Towns",
          Area == "Accessible_Rural" ~ "Accessible Rural",
          Area == "Remote_Rural" ~ "Remote Rural",
          TRUE ~ Area
        ),
        Data_Source = "Scottish Government - Community Ownership Assets"
      ) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Successfully processed", nrow(processed_data), "community ownership records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading community ownership data:", e$message, "\n")
    return(data.frame())
  })
}

# Part 3: Processing Functions and Key Insights

# Create 2-fold and 3-fold aggregations for Community ownership (sum, not average)
create_community_ownership_aggregations <- function(data_6fold, classification_type) {
  cat("Creating community ownership aggregations for", classification_type, "\n")
  
  if(nrow(data_6fold) == 0) {
    return(data.frame())
  }
  
  if(classification_type == "2-fold") {
    # Sum for Urban (4 urban areas)
    urban_data <- data_6fold %>%
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Urban")
    
    # Sum for Rural (2 rural areas)
    rural_data <- data_6fold %>%
      filter(Area %in% c("Accessible Rural", "Remote Rural")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Rural")
    
    # Sum for Scotland (all areas)
    scotland_data <- data_6fold %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Scotland")
    
    return(bind_rows(urban_data, rural_data, scotland_data))
    
  } else if(classification_type == "3-fold") {
    # Sum for Urban (4 urban areas)
    urban_data <- data_6fold %>%
      filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Urban")
    
    # Keep Rural areas separate (Accessible Rural and Remote Rural)
    rural_data <- data_6fold %>%
      filter(Area %in% c("Accessible Rural", "Remote Rural"))
    
    # Sum for Scotland (all areas)
    scotland_data <- data_6fold %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Scotland")
    
    return(bind_rows(urban_data, rural_data, scotland_data))
  }
  
  return(data_6fold)  # Return 6-fold as-is
}

# Main data loading function
load_social_justice_data_simple <- function(metric_name, classification_type, selected_sub_metric = NULL) {
  metric_info <- social_justice_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  # Handle each metric based on available data
  if (metric_name == "Relative poverty") {
    return(load_poverty_data("poverty_adult.csv", "adult"))
    
  } else if (metric_name == "Relative child poverty") {
    return(load_poverty_data("poverty_child.csv", "child"))
    
  } else if (metric_name == "Housing affordability") {
    return(load_housing_affordability_data())
    
  } else if (metric_name == "Influence over local decisions") {
    return(load_perceptions_data_new(selected_sub_metric))
    
  } else if (metric_name == "Managing financially") {
    return(load_managing_financially_data_new())
    
  } else if (metric_name == "Community ownership") {
    data_6fold <- load_community_ownership_data()
    
    if(classification_type == "6-fold") {
      return(data_6fold)
    } else {
      return(create_community_ownership_aggregations(data_6fold, classification_type))
    }
  }
  
  return(data.frame())
}

# Simplified aggregate function
simple_aggregate_social_justice_data <- function(processed_data, classification_type = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}

# Key insights function (only for metrics that have key insights)
get_simple_social_justice_key_insights <- function(processed_data, metric_name, classification_type_for_key_insights = "2-fold", selected_activity = NULL) {
  if(nrow(processed_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  cat("Processing social justice key insights for:", metric_name, "\n")
  cat("Data dimensions:", nrow(processed_data), "rows\n")
  cat("Unique areas in data:", paste(unique(processed_data$Area), collapse = ", "), "\n")
  
  latest_year <- max(processed_data$Year, na.rm = TRUE)
  latest_data <- processed_data %>% filter(Year == latest_year)
  
  # Initialize values
  urban_val <- NA
  rural_val <- NA
  scotland_val <- NA
  
  unique_areas <- unique(latest_data$Area)
  
  # Case 1: Already has Urban/Rural/Scotland (2-fold data)
  if (all(c("Urban", "Rural") %in% unique_areas)) {
    cat("Data already in 2-fold format\n")
    urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
    rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
    scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
    
  # Case 2: Has 6-fold areas (from original complex data)
  } else if (any(c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns", "Accessible Rural", "Remote Rural") %in% unique_areas)) {
    cat("Data in 6-fold format, aggregating to 2-fold\n")
    
    # For Community ownership: sum the values
    if(metric_name == "Community ownership") {
      # Urban = sum of 4 urban areas
      urban_areas <- latest_data %>% 
        filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns"))
      if(nrow(urban_areas) > 0) {
        urban_val <- sum(urban_areas$Value, na.rm = TRUE)
      }
      
      # Rural = sum of 2 rural areas
      rural_areas <- latest_data %>% 
        filter(Area %in% c("Accessible Rural", "Remote Rural"))
      if(nrow(rural_areas) > 0) {
        rural_val <- sum(rural_areas$Value, na.rm = TRUE)
      }
      
      # Scotland = sum of all areas
      all_areas <- latest_data %>% 
        filter(!grepl("Scotland", Area, ignore.case = TRUE))
      if(nrow(all_areas) > 0) {
        scotland_val <- sum(all_areas$Value, na.rm = TRUE)
      }
      
    } else {
      # For other metrics: average the values
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
      
      # Scotland = use existing Scotland value or calculate average
      scotland_areas <- latest_data %>% filter(Area == "Scotland")
      if(nrow(scotland_areas) > 0) {
        scotland_val <- scotland_areas$Value[1]
      } else {
        # Calculate weighted average (approximate)
        all_areas <- latest_data %>% 
          filter(!grepl("Scotland", Area, ignore.case = TRUE))
        if(nrow(all_areas) > 0) {
          scotland_val <- mean(all_areas$Value, na.rm = TRUE)
        }
      }
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

# Format values for display
format_social_justice_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name == "Community ownership") {
    return(format_full_number(value))
  } else if (metric_name %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")) {
    return(paste0(round(value, 1), "%"))
  } else {
    return(round(value, 2))
  }
}

# Calculate gap between urban and rural
calculate_simple_social_justice_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name == "Community ownership") {
    return(format_full_number(gap))
  } else if (metric_name %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")) {
    return(paste0(round(gap, 1), "pp"))
  } else {
    return(round(gap, 2))
  }
}
# Part 4: UI Components

# Create social justice dashboard UI
social_justice_dashboard_ui <- function(category_id) {
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
      .social-justice-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .social-justice-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .social-justice-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .social-justice-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .social-justice-metric-description {
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
            h2("Social Justice", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("social_justice_classification_selector")
          ),
          
          # Top-center-right: Social justice metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("social_justice_metric", "Social Justice Metric:", 
                        choices = c("Select a policy metric..." = "", names(social_justice_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (when applicable)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.social_justice_metric == 'Influence over local decisions'",
              selectInput("selected_influence_statement", "Sub-metric:", 
                          choices = names(influence_sub_metrics), selected = names(influence_sub_metrics)[1], width = "180px")
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
      condition = "input.social_justice_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore social justice data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Social justice specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Relative poverty
                div(
                  class = "social-justice-metrics-card",
                  onclick = "Shiny.setInputValue('social_justice_metric_select', 'Relative poverty', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("chart-line", class = "social-justice-metric-icon"),
                    div(
                      div(class = "social-justice-metric-title", "Relative poverty"),
                      div(class = "social-justice-metric-description", 
                          "Proportion (%) of people who are in relative poverty - measured as households with income below 60% of median UK household income.")
                    )
                  )
                ),
                
                # Relative child poverty
                div(
                  class = "social-justice-metrics-card",
                  onclick = "Shiny.setInputValue('social_justice_metric_select', 'Relative child poverty', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("child", class = "social-justice-metric-icon"),
                    div(
                      div(class = "social-justice-metric-title", "Relative child poverty"),
                      div(class = "social-justice-metric-description",
                          "Proportion (%) of children who are in relative poverty - children living in households with income below 60% of median UK household income after housing costs.")
                    )
                  )
                ),
                
                # Housing affordability
                div(
                  class = "social-justice-metrics-card",
                  onclick = "Shiny.setInputValue('social_justice_metric_select', 'Housing affordability', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("home", class = "social-justice-metric-icon"),
                    div(
                      div(class = "social-justice-metric-title", "Housing affordability"),
                      div(class = "social-justice-metric-description",
                          "Housing costs as percentage of earnings - showing the relationship between housing costs and household income across different areas.")
                    )
                  )
                ),
                
                # Influence over local decisions
                div(
                  class = "social-justice-metrics-card",
                  onclick = "Shiny.setInputValue('social_justice_metric_select', 'Influence over local decisions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("vote-yea", class = "social-justice-metric-icon"),
                    div(
                      div(class = "social-justice-metric-title", "Influence over local decisions"),
                      div(class = "social-justice-metric-description",
                          "Perceptions of influence over decisions affecting local area - community views on local decision-making processes and participation.")
                    )
                  )
                ),
                
                # Managing financially
                div(
                  class = "social-justice-metrics-card",
                  onclick = "Shiny.setInputValue('social_justice_metric_select', 'Managing financially', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("coins", class = "social-justice-metric-icon"),
                    div(
                      div(class = "social-justice-metric-title", "Managing financially"),
                      div(class = "social-justice-metric-description",
                          "How the household is managing financially - household financial management capabilities and economic wellbeing across different community types.")
                    )
                  )
                ),
                
                # Community ownership
                div(
                  class = "social-justice-metrics-card",
                  onclick = "Shiny.setInputValue('social_justice_metric_select', 'Community ownership', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("users", class = "social-justice-metric-icon"),
                    div(
                      div(class = "social-justice-metric-title", "Community ownership"),
                      div(class = "social-justice-metric-description",
                          "Number of assets in community ownership - tracking community-led initiatives and asset ownership across Scotland's communities.")
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
      condition = "input.social_justice_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("social_justice_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("social_justice_data_summary"))
          ),
          
          # Conditionally show key insights boxes only for certain metrics
          conditionalPanel(
            condition = "input.social_justice_metric == 'Relative poverty' || input.social_justice_metric == 'Relative child poverty' || input.social_justice_metric == 'Housing affordability' || input.social_justice_metric == 'Community ownership'",
            fluidRow(
              box(title = uiOutput("social_justice_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    valueBoxOutput("social_justice_urban_rate", width = 3),
                    valueBoxOutput("social_justice_rural_rate", width = 3),
                    valueBoxOutput("social_justice_scotland_rate", width = 3),
                    valueBoxOutput("social_justice_urban_rural_gap", width = 3)
                  ))
            )
          )
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("social_justice_trend_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("social_justice_trend_download", "Download", class = "excel-download-btn"
                            ))),
            status = "primary", solidHeader = TRUE, width = 12,
            plotlyOutput("social_justice_trend_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("social_justice_comparison_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("social_justice_comparison_download", "Download", class = "excel-download-btn"
                            ))), 
            status = "primary", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;", uiOutput("social_justice_year_selector")),
            plotlyOutput("social_justice_comparison_chart") %>% withSpinner())
      ),
      
      fluidRow(
        box(title = div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; padding-right: 115px;",
                        uiOutput("social_justice_table_title"),
                        div(style = "position: absolute; right: 20px;",
                            downloadButton("social_justice_table_download", "Download", class = "excel-download-btn"
                            ))),
            status = "info", solidHeader = TRUE, width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("social_justice_table_year_filter")),
                  column(6, uiOutput("social_justice_table_area_filter"))
                )),
            DT::dataTableOutput("social_justice_data_table") %>% withSpinner())
      )
    )
  )
}
# Part 5: Server Functions

# Social Justice module server
social_justice_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  social_justice_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  # Handle metric selection from the metrics cards
  observeEvent(input$social_justice_metric_select, {
    updateSelectInput(session, "social_justice_metric", selected = input$social_justice_metric_select)
  })
  
  # Dynamic UI outputs
  output$social_justice_summary_title <- renderUI({
    req(input$social_justice_metric, input$social_justice_classification_type)
    display_name <- get_social_justice_metric_display_name(input$social_justice_metric)
    classification_text <- case_when(
      input$social_justice_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$social_justice_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$social_justice_classification_type == "6-fold" ~ "(6-fold Classification)",
      TRUE ~ paste0("(", input$social_justice_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$social_justice_year_selector <- renderUI({
    req(social_justice_values$processed_data)
    if (nrow(social_justice_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(social_justice_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("social_justice_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$social_justice_classification_selector <- renderUI({
    req(input$social_justice_metric)
    available_classifications <- social_justice_metrics[[input$social_justice_metric]]$classifications
    choices <- list()
    
    # Start with most complex fold (6-fold) first
    if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
    if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"
    if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("social_justice_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles
  output$social_justice_trend_title <- renderUI({
    req(input$social_justice_metric)
    display_name <- ifelse(input$social_justice_metric == "Managing financially", "households managing well financially (very well or quite well)",
                             get_social_justice_metric_display_name(input$social_justice_metric))
    paste("Trend Analysis for", display_name)
  })
  
  output$social_justice_comparison_title <- renderUI({
    req(input$social_justice_selected_year, input$social_justice_metric)
    display_name <- get_social_justice_metric_display_name(input$social_justice_metric)
    paste0("Single Year Comparison (", input$social_justice_selected_year, ") for ", display_name)
  })
  
  output$social_justice_table_title <- renderUI({
    req(input$social_justice_metric)
    display_name <- get_social_justice_metric_display_name(input$social_justice_metric)
    paste("Data Table for", display_name)
  })
  
  output$social_justice_key_insights_title <- renderUI({
    req(input$social_justice_metric)
    display_name <- get_social_justice_metric_display_name(input$social_justice_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$social_justice_table_year_filter <- renderUI({
    req(social_justice_values$processed_data)
    if(nrow(social_justice_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(social_justice_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("social_justice_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$social_justice_table_area_filter <- renderUI({
    req(social_justice_values$processed_data, input$social_justice_classification_type)
    if(nrow(social_justice_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_social_justice_data(social_justice_values$processed_data, input$social_justice_classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("social_justice_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Process data reactively
  observe({
    if (is.null(input$social_justice_metric) || input$social_justice_metric == "") {
      social_justice_values$processed_data <- data.frame()
      social_justice_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$social_justice_classification_type)
    
    # Use simplified data loading
    if (input$social_justice_metric == "Influence over local decisions") {
      req(input$selected_influence_statement)
      selected_sub_metric <- influence_sub_metrics[input$selected_influence_statement]
      social_justice_values$processed_data <- load_social_justice_data_simple(
        input$social_justice_metric, 
        input$social_justice_classification_type, 
        selected_sub_metric
      )
    } else {
      social_justice_values$processed_data <- load_social_justice_data_simple(
        input$social_justice_metric, 
        input$social_justice_classification_type
      )
    }
    
    social_justice_values$data_status <- if(nrow(social_justice_values$processed_data) > 0) "Social justice data loaded" else "No data available"
  })
  
  # Data summary (with custom insights only for metrics that have them)
  output$social_justice_data_summary <- renderUI({
    req(social_justice_values$processed_data, input$social_justice_classification_type, input$social_justice_metric)
    
    if (nrow(social_justice_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_social_justice_metric_display_name(input$social_justice_metric)
    
    # Only show custom insights for metrics that have them (excluding managing financially and influence over local decisions)
    custom_insight <- social_justice_key_insights[[input$social_justice_metric]]
    custom_notes <- social_justice_notes[[input$social_justice_metric]]
    
    # Define source information
    source_info <- switch(input$social_justice_metric,
                          "Relative poverty" = list(
                            text = "Scottish Government Poverty Statistics",
                            url = "https://www.gov.scot/collections/poverty-statistics/"
                          ),
                          "Relative child poverty" = list(
                            text = "Scottish Government Poverty Statistics", 
                            url = "https://www.gov.scot/collections/poverty-statistics/"
                          ),
                          "Housing affordability" = list(
                            text = "Family Resources Survey",
                            url = "https://www.gov.scot/collections/family-resources-survey/"
                          ),
                          "Influence over local decisions" = list(
                            text = "Scottish Household Survey",
                            url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
                          ),
                          "Managing financially" = list(
                            text = "Scottish Household Survey",
                            url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
                          ),
                          "Community ownership" = list(
                            text = "Scottish Government Community Empowerment",
                            url = "https://www.gov.scot/policies/community-empowerment/"
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
  
  # Updated server value boxes to ALWAYS show 2-fold urban/rural
  output$social_justice_urban_rate <- renderValueBox({
    req(social_justice_values$processed_data, input$social_justice_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_simple_social_justice_key_insights(social_justice_values$processed_data, input$social_justice_metric, "2-fold")
    val <- format_social_justice_value(key_insights$urban, input$social_justice_metric)
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
  
  output$social_justice_rural_rate <- renderValueBox({
    req(social_justice_values$processed_data, input$social_justice_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_simple_social_justice_key_insights(social_justice_values$processed_data, input$social_justice_metric, "2-fold")
    val <- format_social_justice_value(key_insights$rural, input$social_justice_metric)
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
  
  output$social_justice_scotland_rate <- renderValueBox({
    req(social_justice_values$processed_data, input$social_justice_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_simple_social_justice_key_insights(social_justice_values$processed_data, input$social_justice_metric, "2-fold")
    val <- format_social_justice_value(key_insights$scotland, input$social_justice_metric)
    year <- if(!is.na(key_insights$year)) key_insights$year else ""
    
    #valueBox(value = val, subtitle = paste("Scotland Total", year), icon = icon("flag"), color = "maroon")
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
  
  output$social_justice_urban_rural_gap <- renderValueBox({
    req(social_justice_values$processed_data, input$social_justice_metric)
    
    # ALWAYS get 2-fold urban/rural values regardless of current classification
    key_insights <- get_simple_social_justice_key_insights(social_justice_values$processed_data, input$social_justice_metric, "2-fold")
    val <- calculate_simple_social_justice_gap(key_insights$urban, key_insights$rural, input$social_justice_metric)
    
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
  
  # Updated trend chart with 45-degree rotated year labels and proper colors
  output$social_justice_trend_chart <- renderPlotly({
    req(social_justice_values$processed_data, input$social_justice_classification_type)
    
    tryCatch({
      agg_data <- simple_aggregate_social_justice_data(social_justice_values$processed_data, input$social_justice_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
      cat("Trend chart - unique areas:", paste(unique(agg_data$Area), collapse = ", "), "\n")
      
      # Format data for display
      is_community_data <- input$social_justice_metric == "Community ownership"
      is_percentage_data <- input$social_justice_metric %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")
      
      if (is_community_data) {
        agg_data$Value_Display <- sapply(agg_data$Value, format_full_number)
        y_label <- "Number of Assets"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Assets: ", agg_data$Value_Display)
      } else if (is_percentage_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- ifelse(input$social_justice_metric == "Managing financially", "Percentage (%) managing well financially", "Percentage (%)")
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 2)
        y_label <- "Value"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Value: ", agg_data$Value_Rounded)
      }
      
      agg_data$tooltip <- tooltip_text
      
      # Create plot
      p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)  # 45-degree rotation
        ) +
        labs(x = "Year", y = y_label, color = "Area Type") +
        scale_x_continuous(breaks = function(x) {
          if(length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
        })
      
      # Apply colors with Scotland as gray (like culture module)
      area_colors <- get_social_justice_colors(unique(agg_data$Area), input$social_justice_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      # Format y-axis
      if (is_community_data) {
        p <- p + scale_y_continuous(labels = function(x) sapply(x, format_full_number))
      } else if (is_percentage_data) {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in trend chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Comparison chart with Scotland in gray and proper colors
  output$social_justice_comparison_chart <- renderPlotly({
    req(social_justice_values$processed_data, input$social_justice_classification_type, input$social_justice_selected_year)
    
    tryCatch({
      agg_data <- simple_aggregate_social_justice_data(social_justice_values$processed_data, input$social_justice_classification_type)
      
      selected_data <- agg_data %>% filter(Year == as.numeric(input$social_justice_selected_year))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$social_justice_selected_year), textfont = list(size = 16)))
      }
      
      cat("Comparison chart - selected_data dimensions:", nrow(selected_data), "x", ncol(selected_data), "\n")
      cat("Comparison chart - unique areas:", paste(unique(selected_data$Area), collapse = ", "), "\n")
      
      # Format data
      is_community_data <- input$social_justice_metric == "Community ownership"
      is_percentage_data <- input$social_justice_metric %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")
      
      if (is_community_data) {
        selected_data$Value_Display <- sapply(selected_data$Value, format_full_number)
        x_label <- "Number of Assets"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Assets: ", selected_data$Value_Display)
      } else if (is_percentage_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 1)
        x_label <- "Percentage (%)"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Rate: ", selected_data$Value_Rounded, "%")
      } else {
        selected_data$Value_Rounded <- round(selected_data$Value, 2)
        x_label <- "Value"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Value: ", selected_data$Value_Rounded)
      }
      
      selected_data$tooltip <- tooltip_text
      
      # Create horizontal bar chart
      p <- ggplot(selected_data, aes(x = Value, y = reorder(Area, Value), fill = Area, text = tooltip)) +
        geom_col(alpha = 0.8, width = 0.7) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(y = "", x = x_label)
      
      # Apply colors with Scotland as gray (like culture module)
      area_colors <- get_social_justice_colors(unique(selected_data$Area), input$social_justice_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      # Format x-axis
      if (is_community_data) {
        p <- p + scale_x_continuous(labels = function(x) sapply(x, format_full_number))
      } else if (is_percentage_data) {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in comparison chart:", e$message, "\n")
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })
  
  # Download handlers
  # Trend Download Handler
  output$social_justice_trend_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$social_justice_metric)
      paste0("Social_Justice_Trend_", metric, "_", input$social_justice_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(social_justice_values$processed_data, input$social_justice_classification_type)
      
      data <- simple_aggregate_social_justice_data(
        social_justice_values$processed_data,
        input$social_justice_classification_type
      ) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Year, Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Comparison Download Handler
  output$social_justice_comparison_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$social_justice_metric)
      paste0("Social_Justice_Comparison_", metric, "_", input$social_justice_classification_type, "_", input$social_justice_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(social_justice_values$processed_data, input$social_justice_classification_type, input$social_justice_selected_year)
      
      data <- simple_aggregate_social_justice_data(
        social_justice_values$processed_data,
        input$social_justice_classification_type
      ) %>%
        filter(Year == as.numeric(input$social_justice_selected_year)) %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Table Download Handler
  output$social_justice_table_download <- downloadHandler(
    filename = function() {
      metric <- gsub("[^A-Za-z0-9]", "_", input$social_justice_metric)
      paste0("Social_Justice_Table_", metric, "_", input$social_justice_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(social_justice_values$processed_data, input$social_justice_classification_type)
      
      data <- simple_aggregate_social_justice_data(
        social_justice_values$processed_data,
        input$social_justice_classification_type
      )
      
      if (!is.null(input$social_justice_table_year_filter) && input$social_justice_table_year_filter != "all") {
        data <- data %>% filter(Year == as.numeric(input$social_justice_table_year_filter))
      }
      if (!is.null(input$social_justice_table_area_filter) && input$social_justice_table_area_filter != "all") {
        data <- data %>% filter(Area == input$social_justice_table_area_filter)
      }
      
      data <- data %>%
        select(Year, Area, Value, Data_Source) %>%
        arrange(desc(Year), Area)
      
      write.csv(data, file, row.names = FALSE)
    }
  )

  
  # Data table
  output$social_justice_data_table <- DT::renderDataTable({
    req(social_justice_values$processed_data, input$social_justice_classification_type)
    
    if(nrow(social_justice_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_social_justice_metric_display_name(input$social_justice_metric)
    agg_data <- simple_aggregate_social_justice_data(social_justice_values$processed_data, input$social_justice_classification_type)
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    # Apply filters
    filtered_data <- agg_data
    
    if(!is.null(input$social_justice_table_year_filter) && input$social_justice_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$social_justice_table_year_filter))
    }
    
    if(!is.null(input$social_justice_table_area_filter) && input$social_justice_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$social_justice_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    # Prepare table data
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    # Format value column based on metric type
    if (input$social_justice_metric == "Community ownership") {
      value_col_name <- "Number of Assets"
      table_data$Value <- round(table_data$Value, 0)
    } else if (input$social_justice_metric %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")) {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    } else {
      value_col_name <- "Value"
      table_data$Value <- round(table_data$Value, 2)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$social_justice_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (input$social_justice_metric == "Community ownership") {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "", digits = 0)
    } else if (input$social_justice_metric %in% c("Relative poverty", "Relative child poverty", "Housing affordability", "Influence over local decisions", "Managing financially")) {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    } else {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 2)
    }
    
    return(dt)
  })
}
