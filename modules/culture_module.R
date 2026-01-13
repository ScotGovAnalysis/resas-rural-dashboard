
# modules/culture_module.R 

# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Culture metrics configuration
culture_metrics <- list(
  "Attendance at cultural events and visiting places of culture" = list(
    file_2fold = "culture/attendance_2_fold.csv",
    file_6fold = "culture/attendance_6_fold.csv",
    classifications = c("2-fold", "3-fold", "6-fold"),
    full_name = "Attendance at cultural events and visiting places of culture",
    has_sub_metrics = TRUE
  ),
  "Participation in any cultural activity" = list(
    file_2fold = "culture/participation_2_fold.csv",
    file_6fold = "culture/participation_6_fold.csv",
    classifications = c("2-fold", "3-fold", "6-fold"),
    full_name = "Participation in any cultural activity",
    has_sub_metrics = TRUE
  ),
  "Gaelic speakers" = list(
    file_4fold = "culture/gaelic.xlsx",
    classifications = c("2-fold", "4-fold"), 
    full_name = "Population speaking Gaelic"
  ),
  "Visits to attractions" = list(
    file_6fold = "culture/attraction_visits.csv",
    classifications = c("2-fold", "3-fold", "6-fold"),
    full_name = "Number of visits to Scotland's visitor attractions"
  )
)

culture_definitions <- list( "Attendance at cultural events and visiting places of culture" = "Attendance rates at cultural events or places of culture in the last 12 months including cinema, theatre, museums, live music events and festivals. Shows how urban and rural communities in Scotland attend cultural events and places of culture.",
   "Participation in any cultural activity" = "Participation rates in cultural activities in the last 12 months including reading, music, dance, taking part in theatrical performances, singing and photography. Shows how urban and rural communities in Scotland participate in cultural activities.",
   "Gaelic speakers" = "",
   "Visits to attractions" = ""
  
)
  
# Static Key Insights for each culture metric
culture_key_insights <- list(
  "Attendance at cultural events and visiting places of culture" = "Attendance at cultural events and visiting places of culture (including cinema) is at similar levels for adults living in ‘rural Scotland’ (77%) and adults living in ‘rest of Scotland’ (76%). Overall there has been a drop in attendance in cultural activities from 2019 to 2022 related to the COVID pandemic. However, attendance at cultural events or places of culture (including cinema) has been recovering with higher rates across all 6 categories of urban/rural classification from 2022 to 2023, except for remote small towns which saw a large drop (79% in 2022 to 66% in 2023).",
  
  "Participation in any cultural activity" = "Participation in cultural activities (including reading) in 2023 was the highest in accessible rural areas (80%) and lowest in other urban areas (71%). Adults living in ‘rural Scotland’ show generally higher levels of participation in any cultural activity (including reading) than adults living in ‘rest of Scotland’ (79% vs 73%).",
  
  "Gaelic speakers" = "The total number of Gaelic speakers in Scotland has increased from 2001 to 2022. In this period, the number of Gaelic speakers declined in Islands and Remote Rural areas but increased in all other geographic classifications. In 2022, Islands and Remote Rural areas had the lowest number of Gaelic speakers whilst in 2001 it had it had the most. Mainly Rural areas had the highest number of Gaelic speakers in 2022." ,
  
  "Visits to attractions" = "The number of visits to Scotland's visitor attractions has increased in all areas between 2020 and 2024. In 2024 attraction in large urban areas had the highest number of visits and those in remote small towns had the fewest."
)

# Static Notes for each culture metric
culture_notes <- list(
  "Attendance at cultural events and visiting places of culture" = "Due to methodology changes due to Covid-19 the results of the 2020 and 2021 SHS telephone survey were published as experimental statistics. They are not directly comparable to SHS face to-face survey results from previous years (2019 and earlier) and from 2022 onwards.",
  
  "Participation in any cultural activity" = "Due to methodology changes due to Covid-19 the results of the 2020 and 2021 SHS telephone survey were published as experimental statistics. They are not directly comparable to SHS face to-face survey results from previous years (2019 and earlier) and from 2022 onwards.",
  
  "Gaelic speakers" = "1. Totals are not additive so percentages have not been calculated 2. \"Speaks Gaelic\" category includes \"Speaks, reads and writes Gaelic\", \"Speaks but does not read or write Gaelic\" and \"Speaks and reads but does not write Gaelic\". \"Other combination of skills in Gaelic\" is excluded.", 
  "Visits to attractions" = ""
)

# Sub-metrics for attendance activities (excluding Base)
attendance_activities <- c(
  "Any including cinema" = "Any including cinema",
  "Any excluding cinema" = "Any excluding cinema", 
  "Cinema" = "Cinema",
  "Library (including mobile and online)" = "Library (including mobile and online)",
  "Live music event - e.g. traditional music, rock concert" = "Live music event - e.g. traditional music, rock concert",
  "Theatre - e.g. pantomime / musical / play" = "Theatre - e.g. pantomime / musical / play",
  "Dance show / event - e.g. ballet" = "Dance show / event - e.g. ballet",
  "Historic place - e.g. castle, stately home and grounds, battle or archaeological site" = "Historic place - e.g. castle, stately home and grounds, battle or archaeological site",
  "Museum" = "Museum",
  "Art gallery" = "Art gallery",
  "Exhibition - including art, photography and crafts" = "Exhibition - including art, photography and crafts",
  "Street arts (e.g. musical performances or art in parks, streets or shopping centre)" = "Street arts (e.g. musical performances or art in parks, streets or shopping centre)",
  "Culturally specific festival (e.g. Mela /Feis/ local Gala days)" = "Culturally specific festival (e.g. Mela /Feis/ local Gala days)",
  "Book festival" = "Book festival",
  "Archive or records office (e.g. Scotland's Family History Peoples Centre)" = "Archive or records office (e.g. Scotland's Family History Peoples Centre)",
  "Streaming of a live performance (e.g. theatre, dance, concert or comedy performance)" = "Streaming of a live performance (e.g. theatre, dance, concert or comedy performance)",
  "Classical music performance" = "Classical music performance",
  "Opera" = "Opera",
  "Comedy performance" = "Comedy performance"
)

# Sub-metrics for participation activities (excluding Base)
participation_activities <- c(
  "Any including reading" = "Any including reading",
  "Any excluding reading" = "Any excluding reading",
  "Read books, poetry or graphic novels / comics for pleasure (including on a Kindle or other mobile device)" = "Read books, poetry or graphic novels / comics for pleasure (including on a Kindle or other mobile device)",
  "Dance, either for fitness or not for fitness" = "Dance, either for fitness or not for fitness",
  "Played a musical instrument or written music" = "Played a musical instrument or written music",
  "Taken part in a theatrical performance such as a play, musical, comedy or variety performance (including production and backstage" = "Taken part in a theatrical performance such as a play, musical, comedy or variety performance (including production and backstage",
  "Sang in a singing group or choir" = "Sang in a singing group or choir",
  "Painting, drawing, printmaking or sculpture" = "Painting, drawing, printmaking or sculpture",
  "Photography as an artistic activity (not family or holiday 'snaps')" = "Photography as an artistic activity (not family or holiday 'snaps')",
  "Film- making/ video-making as an artistic activity" = "Film- making/ video-making as an artistic activity",
  "Used a computer to produce artwork or animation" = "Used a computer to produce artwork or animation",
  "Crafts such as knitting, jewellery making, pottery, etc." = "Crafts such as knitting, jewellery making, pottery, etc.",
  "Creative writing - stories, books, comics, plays or poetry" = "Creative writing - stories, books, comics, plays or poetry",
  "Viewed performances (e.g. music or dance) online on a smartphone, computer, smart tv etc" = "Viewed performances (e.g. music or dance) online on a smartphone, computer, smart tv etc",
  "Viewed cultural content online (e.g. museum or heritage collections or artist's work)" = "Viewed cultural content online (e.g. museum or heritage collections or artist's work)",
  "Shared art or creative content online that you have created yourself (such as digital art, music, dance, videos or recordings)" = "Shared art or creative content online that you have created yourself (such as digital art, music, dance, videos or recordings)",
  "Other cultural activity" = "Other cultural activity"
)

# Helper functions for number formatting
format_visits_number <- function(number) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  
  rounded <- round(number / 1000) * 1000
  
  if (rounded >= 1000000) {
    millions <- rounded / 1000000
    if (millions >= 100) {
      return(paste0(round(millions, 0), " million"))
    } else if (millions >= 10) {
      return(paste0(round(millions, 1), " million"))
    } else {
      return(paste0(round(millions, 2), " million"))
    }
  } else if (rounded >= 1000) {
    thousands <- rounded / 1000
    if (thousands >= 100) {
      return(scales::comma(round(thousands * 1000, 0)))
    } else {
      return(paste0(round(thousands, 0), "k"))
    }
  } else {
    return(scales::comma(rounded))
  }
}

format_full_number <- function(number) {
  if (is.na(number) || number == 0) {
    return("0")
  }
  return(scales::comma(round(number, 0)))
}

# Function to get display name for metrics
get_culture_metric_display_name <- function(metric_name) {
  metric_info <- culture_metrics[[metric_name]]
  if(!is.null(metric_info)) {
    return(metric_info$full_name)
  }
  return(metric_name)
}

#  DATA LOADING FUNCTIONS

# Load 2-fold data from simplified CSV files
load_2fold_data_simple <- function(filename, selected_activity = NULL) {
  cat("Loading 2-fold data from:", filename, "\n")
  
  tryCatch({
    if(!file.exists(filename)) {
      cat("File not found:", filename, "\n")
      return(data.frame())
    }
    
    # Read CSV - columns: "", Year, Council, "", Rest of Scotland, Rural Scotland, All
    raw_data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
    
    # Set proper column names based on file structure
    colnames(raw_data) <- c("ID", "Year", "Council", "Activity", "Rest_of_Scotland", "Rural_Scotland", "All")
    
    # Filter for selected activity if specified, exclude "Base" rows
    if (!is.null(selected_activity)) {
      raw_data <- raw_data %>% filter(Activity == selected_activity)
    }
    
    # Remove any "Base" entries
    raw_data <- raw_data %>% filter(!grepl("Base", Activity, ignore.case = TRUE))
    
    # Process data - create records for Urban (Rest of Scotland), Rural (Rural Scotland), Scotland (All)
    processed_data <- data.frame()
    
    for (i in 1:nrow(raw_data)) {
      year <- as.numeric(raw_data$Year[i])
      if (is.na(year)) next
      
      activity <- raw_data$Activity[i]
      rest_scotland <- raw_data$Rest_of_Scotland[i]
      rural_scotland <- raw_data$Rural_Scotland[i]
      all_scotland <- raw_data$All[i]
      
      # Urban data (Rest of Scotland)
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
      
      # Rural data (Rural Scotland)
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
      
      # Scotland total (All)
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
    
    cat(paste("Loaded", nrow(processed_data), "2-fold records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading 2-fold data:", e$message, "\n")
    return(data.frame())
  })
}

# Load 6-fold data
load_6fold_data_simple <- function(filename, selected_activity = NULL) {
  cat("Loading 6-fold data from:", filename, "\n")
  
  tryCatch({
    if(!file.exists(filename)) {
      cat("File not found:", filename, "\n")
      return(data.frame())
    }
    
    # Read CSV - columns: "", Year, Council, "", Large urban areas, Other urban areas, Accessible small towns, Remote small towns, Accessible rural, Remote rural, All
    raw_data <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE)
    
    # Set proper column names
    colnames(raw_data) <- c("ID", "Year", "Council", "Activity", "Large_Urban_Areas", 
                            "Other_Urban_Areas", "Accessible_Small_Towns", "Remote_Small_Towns", 
                            "Accessible_Rural", "Remote_Rural", "All")
    
    # Filter for selected activity if specified, exclude "Base" rows
    if (!is.null(selected_activity)) {
      raw_data <- raw_data %>% filter(Activity == selected_activity)
    }
    
    # Remove any "Base" entries
    raw_data <- raw_data %>% filter(!grepl("Base", Activity, ignore.case = TRUE))
    
    # Clean and filter data
    clean_data <- raw_data %>%
      filter(!is.na(Year), !is.na(Activity)) %>%
      mutate(Year = as.numeric(Year)) %>%
      filter(!is.na(Year)) %>%
      mutate(Activity = trimws(Activity))
    
    # Convert to long format
    processed_data <- clean_data %>%
      select(Year, Activity, Large_Urban_Areas, Other_Urban_Areas, Accessible_Small_Towns, 
             Remote_Small_Towns, Accessible_Rural, Remote_Rural, All) %>%
      gather(key = "Area", value = "Value", -Year, -Activity) %>%
      filter(!Value %in% c("-", "*", "")) %>%
      mutate(
        Value = as.numeric(Value),
        Area = case_when(
          Area == "Large_Urban_Areas" ~ "Large Urban Areas",
          Area == "Other_Urban_Areas" ~ "Other Urban Areas", 
          Area == "Accessible_Small_Towns" ~ "Accessible Small Towns",
          Area == "Remote_Small_Towns" ~ "Remote Small Towns",
          Area == "Accessible_Rural" ~ "Accessible Rural",
          Area == "Remote_Rural" ~ "Remote Rural",
          Area == "All" ~ "Scotland"
        ),
        Data_Source = "Scottish Household Survey - 6-fold"
      ) %>%
      filter(!is.na(Value)) %>%
      select(Year, Activity, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "6-fold records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading 6-fold data:", e$message, "\n")
    return(data.frame())
  })
}

# Create 3-fold by combining Urban from 2-fold + Rural areas from 6-fold + Scotland from 2-fold
create_3fold_simple <- function(data_2fold, data_6fold) {
  cat("Creating simple 3-fold combination\n")
  
  if(nrow(data_2fold) == 0 || nrow(data_6fold) == 0) {
    cat("Missing 2-fold or 6-fold data\n")
    return(data.frame())
  }
  
  # Urban from 2-fold (Rest of Scotland)
  urban_data <- data_2fold %>%
    filter(Area == "Urban") %>%
    mutate(Data_Source = "Scottish Household Survey - 3-fold (Urban from 2-fold)")
  
  # Rural areas from 6-fold (Accessible Rural and Remote Rural)
  rural_data <- data_6fold %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural")) %>%
    mutate(Data_Source = "Scottish Household Survey - 3-fold (Rural from 6-fold)")
  
  # Scotland from 2-fold (All column)
  scotland_data <- data_2fold %>%
    filter(Area == "Scotland") %>%
    mutate(Data_Source = "Scottish Household Survey - 3-fold (Scotland from 2-fold)")
  
  # Combine
  combined_3fold <- bind_rows(urban_data, rural_data, scotland_data)
  
  cat(paste("Created", nrow(combined_3fold), "3-fold records\n"))
  return(combined_3fold)
}

# FIXED: Load Gaelic data - only 4-fold
load_gaelic_data_simple <- function() {
  filepath <- "culture/gaelic.xlsx"
  cat("Loading Gaelic data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    # Read Excel file - skip the title row (row 0), header is in row 1
    raw_data <- read_excel(filepath, skip = 1)
    
    # The file structure is: Region, 2001, 2011, 2022
    # Set proper column names
    colnames(raw_data) <- c("Region", "2001", "2011", "2022", "Extra")[1:ncol(raw_data)]
    
    cat("Column names in Gaelic file:", paste(colnames(raw_data), collapse = ", "), "\n")
    cat("Number of rows in raw data:", nrow(raw_data), "\n")
    cat("Sample of raw data:\n")
    print(head(raw_data, 5))
    
    # Process like the CSV files - exclude SCOTLAND row and empty rows
    clean_data <- raw_data %>%
      filter(
        !is.na(Region), 
        Region != "", 
        !grepl("SCOTLAND", Region, ignore.case = TRUE)
      ) %>%
      select(Region, `2001`, `2011`, `2022`)
    
    # Convert to long format
    processed_data <- clean_data %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(Value),
        # Map to standard area names - exact matches from file
        Area = case_when(
          Region == "Islands & Remote Rural" ~ "Islands & Remote Rural",
          Region == "Mainly Rural" ~ "Mainly Rural", 
          Region == "Urban with Substantial Rural areas" ~ "Urban with Substantial Rural areas",
          Region == "Larger Cities" ~ "Larger Cities",
          TRUE ~ trimws(Region)
        ),
        Data_Source = "Scottish Government - Census Data"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "Gaelic 4-fold records\n"))
    
    # Debug: show what we have
    cat("Areas found:", paste(unique(processed_data$Area), collapse = ", "), "\n")
    cat("Years found:", paste(unique(processed_data$Year), collapse = ", "), "\n")
    cat("Sample processed data:\n")
    print(head(processed_data, 8))
    
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading Gaelic data:", e$message, "\n")
    print(e)
    return(data.frame())
  })
}

# Load attraction visits data
load_attraction_visits_data_simple <- function() {
  filepath <- "culture/attraction_visits.csv"
  cat("Loading attraction visits data from:", filepath, "\n")
  
  tryCatch({
    if(!file.exists(filepath)) {
      cat("File not found:", filepath, "\n")
      return(data.frame())
    }
    
    raw_data <- read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Process data - convert to long format first
    processed_data <- raw_data %>%
      select(-Sample) %>%
      gather(key = "Year", value = "Value", -Region) %>%
      mutate(
        Year = as.numeric(Year),
        Value = as.numeric(gsub('[^0-9]', '', Value)),
        Area = trimws(Region),
        Data_Source = "Scottish Government - Visitor Attraction Statistics"
      ) %>%
      filter(!is.na(Value), !is.na(Year)) %>%
      select(Year, Area, Value, Data_Source)
    
    cat(paste("Loaded", nrow(processed_data), "attraction visit 6-fold records\n"))
    return(processed_data)
    
  }, error = function(e) {
    cat("Error loading attraction visits data:", e$message, "\n")
    return(data.frame())
  })
}

# Create 2-fold aggregation for attraction visits
create_attraction_2fold <- function(attraction_6fold_data) {
  cat("Creating attraction visits 2-fold aggregation\n")
  
  if(nrow(attraction_6fold_data) == 0) {
    return(data.frame())
  }
  
  # Sum for Urban (4 urban areas)
  urban_data <- attraction_6fold_data %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Sum for Rural (2 rural areas)
  rural_data <- attraction_6fold_data %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Rural")
  
  # Sum for Scotland (all areas)
  scotland_data <- attraction_6fold_data %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Scotland")
  
  return(bind_rows(urban_data, rural_data, scotland_data))
}

# Create 3-fold aggregation for attraction visits
create_attraction_3fold <- function(attraction_6fold_data) {
  cat("Creating attraction visits 3-fold aggregation\n")
  
  if(nrow(attraction_6fold_data) == 0) {
    return(data.frame())
  }
  
  # Sum for Urban (4 urban areas)
  urban_data <- attraction_6fold_data %>%
    filter(Area %in% c("Large Urban Areas", "Other Urban Areas", "Accessible Small Towns", "Remote Small Towns")) %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Urban")
  
  # Keep Rural areas separate (Accessible Rural and Remote Rural)
  rural_data <- attraction_6fold_data %>%
    filter(Area %in% c("Accessible Rural", "Remote Rural"))
  
  # Sum for Scotland (all areas)
  scotland_data <- attraction_6fold_data %>%
    group_by(Year, Data_Source) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Area = "Scotland")
  
  return(bind_rows(urban_data, rural_data, scotland_data))
}

# Updated load_culture_data_simple to handle 2-fold Gaelic
load_culture_data_simple <- function(metric_name, classification_type, selected_activity = NULL) {
  metric_info <- culture_metrics[[metric_name]]
  if (is.null(metric_info)) {
    return(data.frame())
  }
  
  cat("Loading", metric_name, "with", classification_type, "classification\n")
  
  # Attendance and Participation
  if (metric_name %in% c("Attendance at cultural events and visiting places of culture", "Participation in any cultural activity")) {
    
    if (classification_type == "2-fold") {
      return(load_2fold_data_simple(metric_info$file_2fold, selected_activity))
    } else if (classification_type == "6-fold") {
      return(load_6fold_data_simple(metric_info$file_6fold, selected_activity))
    } else if (classification_type == "3-fold") {
      data_2fold <- load_2fold_data_simple(metric_info$file_2fold, selected_activity)
      data_6fold <- load_6fold_data_simple(metric_info$file_6fold, selected_activity)
      return(create_3fold_simple(data_2fold, data_6fold))
    }
    
    # Gaelic speakers 
  } else if (metric_name == "Gaelic speakers") {
    if (classification_type %in% c("4-fold", "2-fold")) {
      return(load_gaelic_data_simple())  # Returns 4-fold data, which gets aggregated to 2-fold if needed
    } else {
      cat("Only 4-fold and 2-fold classifications available for Gaelic speakers\n")
      return(data.frame())
    }
    
    # Visits to attractions
  } else if (metric_name == "Visits to attractions") {
    attraction_6fold <- load_attraction_visits_data_simple()
    
    if (classification_type == "6-fold") {
      return(attraction_6fold)
    } else if (classification_type == "2-fold") {
      return(create_attraction_2fold(attraction_6fold))
    } else if (classification_type == "3-fold") {
      return(create_attraction_3fold(attraction_6fold))
    }
  }
  
  return(data.frame())
}


# PROCESSING FUNCTIONS

# FIXED: Updated simple aggregate function to handle 2-fold Gaelic
simple_aggregate_culture_data <- function(processed_data, classification_type = "2-fold") {
  if(nrow(processed_data) == 0) {
    return(data.frame())
  }
  
  # For 4-fold data (Gaelic), handle 2-fold aggregation
  if(classification_type == "2-fold" && "Larger Cities" %in% unique(processed_data$Area)) {
    cat("Creating 2-fold aggregation from 4-fold Gaelic data\n")
    
    # Aggregate Urban (Larger Cities + Urban with Substantial Rural areas)
    urban_data <- processed_data %>%
      filter(Area %in% c("Larger Cities", "Urban with Substantial Rural areas")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Urban")
    
    # Aggregate Rural (Mainly Rural + Islands & Remote Rural)
    rural_data <- processed_data %>%
      filter(Area %in% c("Mainly Rural", "Islands & Remote Rural")) %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Rural")
    
    # Scotland total (all areas)
    scotland_data <- processed_data %>%
      group_by(Year, Data_Source) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Area = "Scotland")
    
    return(bind_rows(urban_data, rural_data, scotland_data))
  }
  
  # For 4-fold data (Gaelic), just pass through as-is since it's already in the right format
  if(classification_type == "4-fold") {
    return(processed_data %>%
             group_by(Year, Area, Data_Source) %>%
             summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
  }
  
  # For other classifications, data is already in the right format from loading functions
  return(processed_data %>%
           group_by(Year, Area, Data_Source) %>%
           summarise(Value = mean(Value, na.rm = TRUE), .groups = 'drop'))
}

get_simple_key_insights_2fold_only <- function(metric_name, selected_activity = NULL) {
  cat("Loading 2-fold data directly for key insights:", metric_name, "\n")
  
  # For Gaelic, we need to load 4-fold and aggregate to 2-fold
  if(metric_name == "Gaelic speakers") {
    raw_data <- load_culture_data_simple(metric_name, "4-fold", selected_activity)
    if(nrow(raw_data) == 0) {
      return(list(urban = NA, rural = NA, scotland = NA, year = NA))
    }
    
    # Aggregate 4-fold Gaelic data to 2-fold
    twofold_data <- simple_aggregate_culture_data(raw_data, "2-fold")
  } else {
    # For other metrics, load 2-fold data directly
    twofold_data <- load_culture_data_simple(metric_name, "2-fold", selected_activity)
  }
  
  if(nrow(twofold_data) == 0) {
    return(list(urban = NA, rural = NA, scotland = NA, year = NA))
  }
  
  # Get latest year from the 2-fold data
  latest_year <- max(twofold_data$Year, na.rm = TRUE)
  latest_data <- twofold_data %>% filter(Year == latest_year)
  
  # Extract Urban, Rural, Scotland values directly
  urban_val <- latest_data %>% filter(Area == "Urban") %>% pull(Value) %>% first()
  rural_val <- latest_data %>% filter(Area == "Rural") %>% pull(Value) %>% first()
  scotland_val <- latest_data %>% filter(Area == "Scotland") %>% pull(Value) %>% first()
  
  # Handle NAs
  urban_val <- ifelse(length(urban_val) == 0 || is.na(urban_val), NA, urban_val)
  rural_val <- ifelse(length(rural_val) == 0 || is.na(rural_val), NA, rural_val)
  scotland_val <- ifelse(length(scotland_val) == 0 || is.na(scotland_val), NA, scotland_val)
  
  cat("2-fold key insights for year", latest_year, "- Urban:", urban_val, "Rural:", rural_val, "Scotland:", scotland_val, "\n")
  
  return(list(
    urban = urban_val,
    rural = rural_val,
    scotland = scotland_val,
    year = latest_year
  ))
}
# Create color mapping with Scotland as gray
get_simple_colors <- function(areas, classification_type) {
  color_mapping <- list()
  
  # Get base colors from config.R if available
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

# Format values for display
format_culture_value <- function(value, metric_name) {
  if (is.na(value)) return("No Data")
  
  if (metric_name == "Visits to attractions") {
    return(format_visits_number(value))
  } else if (metric_name == "Gaelic speakers") {
    return(format_full_number(value))
  } else {
    return(paste0(round(value, 1), "%"))
  }
}

# Calculate gap between urban and rural
calculate_simple_gap <- function(urban_val, rural_val, metric_name) {
  if (is.na(urban_val) || is.na(rural_val)) return("No Data")
  
  gap <- abs(urban_val - rural_val)
  
  if (metric_name == "Visits to attractions") {
    return(format_visits_number(gap))
  } else if (metric_name == "Gaelic speakers") {
    return(format_full_number(gap))
  } else {
    return(paste0(round(gap, 1), "pp"))
  }
}

# UI COMPONENTS

culture_dashboard_ui <- function(category_id) {
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
      .culture-metrics-card {
        background: white !important;
        border: 1px solid #dee2e6 !important;
        border-radius: 6px !important;
        padding: 15px !important;
        margin-bottom: 15px !important;
        transition: all 0.2s ease !important;
        cursor: pointer !important;
      }
      
      .culture-metrics-card:hover {
        background: #f8f9fa !important;
        border-color: #007bff !important;
        transform: translateX(5px) !important;
        box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
      }
      
      .culture-metric-icon {
        color: #28a745 !important;
        font-size: 1.2em !important;
        margin-right: 15px !important;
      }
      
      .culture-metric-title {
        color: #007bff !important;
        font-size: 1.1em !important;
        font-weight: 600 !important;
        margin-bottom: 5px !important;
      }
      
      .culture-metric-description {
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
            h2("Culture and Heritage", style = "margin: 0; color: white; text-shadow: 3px 3px 6px rgba(0,0,0,0.8); font-size: 2.4em; font-weight: 700;")
          ),
          
          # Top-right: Classification dropdown
          div(
            style = "position: absolute; left: 460px; bottom: 5px; z-index: 1002;",
            uiOutput("culture_classification_selector")
          ),
          
          # Top-center-right: Cultural metric dropdown  
          div(
            style = "position: absolute; left: 220px; bottom: 5px; z-index: 1001;",
            selectInput("culture_metric", "Cultural Metric:", 
                        choices = c("Select a policy metric..." = "", names(culture_metrics)), 
                        selected = "", width = "220px")
          ),
          
          # Bottom-right: Sub-metric dropdown (when applicable)
          div(
            style = "position: absolute; left: 690px; bottom: 5px; z-index: 1003;",
            conditionalPanel(
              condition = "input.culture_metric == 'Attendance at cultural events and visiting places of culture'",
              selectInput("selected_attendance_activity", "Sub-metric:", 
                          choices = names(attendance_activities), selected = names(attendance_activities)[1], width = "180px")
            ),
            conditionalPanel(
              condition = "input.culture_metric == 'Participation in any cultural activity'",
              selectInput("selected_participation_activity", "Sub-metric:", 
                          choices = names(participation_activities), selected = names(participation_activities)[1], width = "180px")
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
      condition = "input.culture_metric == ''",
      div(style = "margin-top:200px",
          fluidRow(
            box(
              title = "Please select a metric:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 12,
              
              p("Click on any metric below to explore culture and heritage data across Scotland's urban and rural areas.", 
                style = "font-size: 1.1em; margin-bottom: 25px; color: #495057;"),
              
              # Culture-specific metrics list
              div(
                style = "display: grid; grid-template-columns: 1fr; gap: 15px;",
                
                # Attendance at cultural events
                div(
                  class = "culture-metrics-card",
                  onclick = "Shiny.setInputValue('culture_metric_select', 'Attendance at cultural events and visiting places of culture', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("calendar-alt", class = "culture-metric-icon"),
                    div(
                      div(class = "culture-metric-title", "Attendance at cultural events and visiting places of culture"),
                      div(class = "culture-metric-description", 
                          "Attendance rates across 18 different cultural activities including cinema, theatre, museums, live music, and festivals. Shows how urban and rural communities engage with Scotland's cultural offerings.")
                    )
                  )
                ),
                
                # Participation in cultural activity
                div(
                  class = "culture-metrics-card",
                  onclick = "Shiny.setInputValue('culture_metric_select', 'Participation in any cultural activity', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("palette", class = "culture-metric-icon"),
                    div(
                      div(class = "culture-metric-title", "Participation in any cultural activity"),
                      div(class = "culture-metric-description",
                          "Active participation in creative and cultural activities including reading, music, dance, visual arts, crafts, and digital content creation. Tracks how communities actively create and share culture.")
                    )
                  )
                ),
                
                # Gaelic speakers
                div(
                  class = "culture-metrics-card",
                  onclick = "Shiny.setInputValue('culture_metric_select', 'Gaelic speakers', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("language", class = "culture-metric-icon"),
                    div(
                      div(class = "culture-metric-title", "Gaelic speakers"),
                      div(class = "culture-metric-description",
                          "Population that can speak Scottish Gaelic, tracking the preservation and vitality of Scotland's indigenous language across different regional classifications.")
                    )
                  )
                ),
                
                # Visits to attractions
                div(
                  class = "culture-metrics-card",
                  onclick = "Shiny.setInputValue('culture_metric_select', 'Visits to attractions', {priority: 'event'});",
                  div(
                    style = "display: flex; align-items: flex-start;",
                    icon("map-marked-alt", class = "culture-metric-icon"),
                    div(
                      div(class = "culture-metric-title", "Visits to attractions"),
                      div(class = "culture-metric-description",
                          "Annual visitor numbers to Scotland's cultural and heritage attractions, showing tourism patterns and cultural site usage across urban and rural areas.")
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
      condition = "input.culture_metric != ''",
      
      div(style = "margin-top:200px",
          fluidRow(
            box(title = uiOutput("culture_summary_title"), status = "success", solidHeader = TRUE, width = 12,
                uiOutput("culture_data_summary"))
          ),
          
          fluidRow(
            box(title = uiOutput("culture_key_insights_title"), status = "info", solidHeader = TRUE, width = 12,
                fluidRow(
                  valueBoxOutput("culture_urban_rate", width = 3),
                  valueBoxOutput("culture_rural_rate", width = 3),
                  valueBoxOutput("culture_scotland_rate", width = 3),
                  valueBoxOutput("culture_urban_rural_gap", width = 3)
                ))
          )
      ),
      
      fluidRow(
        box(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            uiOutput("culture_trend_title"),  # leaves space for button
            downloadButton("culture_trend_download", "Download", class = "excel-download-btn",
                           style = "position: absolute; right: 0")
          ),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("culture_trend_chart") %>% withSpinner()
        )
      ),
      
      # Culture Comparison Section
      fluidRow(
        box(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            uiOutput("culture_comparison_title"),
            downloadButton("culture_comparison_download", "Download", class = "excel-download-btn",
                           style = "position: absolute; right: 0")
          ),
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(style = "margin-bottom: 15px;", uiOutput("culture_year_selector")),
          plotlyOutput("culture_comparison_chart") %>% withSpinner()
        )
      ),
      
      # Culture Table Section
      fluidRow(
        box(
          title = div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            uiOutput("culture_table_title"),
            downloadButton("culture_table_download", "Download", class = "excel-download-btn",
                           style = "position: absolute; right: 0")
          ),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            div(style = "margin-bottom: 15px;",
                fluidRow(
                  column(6, uiOutput("culture_table_year_filter")),
                  column(6, uiOutput("culture_table_area_filter"))
                )),
            DT::dataTableOutput("culture_data_table") %>% withSpinner()
          )
        ))
  
   
  )
}

# SERVER FUNCTIONS

# Culture module server
culture_server <- function(id, values, parent_session = NULL) {
  if (!is.null(parent_session)) {
    session <- parent_session
    input <- parent_session$input
    output <- parent_session$output
  }
  
  culture_values <- reactiveValues(
    processed_data = NULL,
    data_status = "Loading..."
  )
  
  #Handle metric selection from the metrics cards
  observeEvent(input$culture_metric_select, {
    updateSelectInput(session, "culture_metric", selected = input$culture_metric_select)
  })
  
  # Dynamic UI outputs
  output$culture_summary_title <- renderUI({
    req(input$culture_metric, input$culture_classification_type)
    display_name <- get_culture_metric_display_name(input$culture_metric)
    classification_text <- case_when(
      input$culture_classification_type == "2-fold" ~ "(Urban/Rural)",
      input$culture_classification_type == "3-fold" ~ "(3-fold Classification)",
      input$culture_classification_type == "4-fold" ~ "(4-fold RESAS)",
      input$culture_classification_type == "6-fold" ~ "(6-fold Classification)",
      TRUE ~ paste0("(", input$culture_classification_type, ")")
    )
    h3(paste(display_name, classification_text), style = "margin: 0; font-weight: 600; color: white;")
  })
  
  output$culture_year_selector <- renderUI({
    req(culture_values$processed_data)
    if (nrow(culture_values$processed_data) == 0) return(NULL)
    available_years <- sort(unique(culture_values$processed_data$Year), decreasing = TRUE)
    if(length(available_years) > 0) {
      selectInput("culture_selected_year", "Select Year for Comparison:", choices = available_years, selected = available_years[1], width = "200px")
    }
  })
  
  output$culture_classification_selector <- renderUI({
    req(input$culture_metric)
    available_classifications <- culture_metrics[[input$culture_metric]]$classifications
    choices <- list()
    
    # Special labeling for Gaelic
    if (input$culture_metric == "Gaelic speakers") {
      if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if("2-fold" %in% available_classifications) choices[["2-fold (RESAS)"]] <- "2-fold"  # FIXED: Added this
    } else {
      if("6-fold" %in% available_classifications) choices[["6-fold (Most Detailed)"]] <- "6-fold"
      if("4-fold" %in% available_classifications) choices[["4-fold (RESAS)"]] <- "4-fold"
      if("3-fold" %in% available_classifications) choices[["3-fold (Detailed)"]] <- "3-fold"
      if("2-fold" %in% available_classifications) choices[["2-fold (Urban/Rural)"]] <- "2-fold"
    }
    
    default_selection <- if(length(choices) > 0) choices[[1]] else NULL
    
    selectInput("culture_classification_type", "Geographic Classification:", 
                choices = choices, selected = default_selection, width = "200px")
  })
  
  # Dynamic titles
  output$culture_trend_title <- renderUI({
    req(input$culture_metric)
    display_name <- get_culture_metric_display_name(input$culture_metric)
    paste("Trend Analysis for", display_name)
  })
  
  output$culture_comparison_title <- renderUI({
    req(input$culture_selected_year, input$culture_metric)
    display_name <- get_culture_metric_display_name(input$culture_metric)
    paste0("Single Year Comparison (", input$culture_selected_year, ") for ", display_name)
  })
  
  output$culture_table_title <- renderUI({
    req(input$culture_metric)
    display_name <- get_culture_metric_display_name(input$culture_metric)
    paste("Data Table for", display_name)
  })
  
  output$culture_key_insights_title <- renderUI({
    req(input$culture_metric)
    display_name <- get_culture_metric_display_name(input$culture_metric)
    paste("Key Insights for", display_name)
  })
  
  # Table filters
  output$culture_table_year_filter <- renderUI({
    req(culture_values$processed_data)
    if(nrow(culture_values$processed_data) == 0) return(NULL)
    all_years <- sort(unique(culture_values$processed_data$Year), decreasing = TRUE)
    choices <- list("All Years" = "all")
    for(year in all_years) choices[[as.character(year)]] <- year
    selectInput("culture_table_year_filter", "Filter by Year:", choices = choices, selected = "all", width = "100%")
  })
  
  output$culture_table_area_filter <- renderUI({
    req(culture_values$processed_data, input$culture_classification_type)
    if(nrow(culture_values$processed_data) == 0) return(NULL)
    agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
    if(nrow(agg_data) == 0) return(NULL)
    all_areas <- sort(unique(agg_data$Area))
    choices <- list("All Areas" = "all")
    for(area in all_areas) choices[[area]] <- area
    selectInput("culture_table_area_filter", "Filter by Area:", choices = choices, selected = "all", width = "100%")
  })
  
  # Process data reactively
  observe({
    if (is.null(input$culture_metric) || input$culture_metric == "") {
      culture_values$processed_data <- data.frame()
      culture_values$data_status <- "Please select a metric"
      return()
    }
    
    req(input$culture_classification_type)
    
    # Use simplified data loading
    if (input$culture_metric == "Attendance at cultural events and visiting places of culture") {
      req(input$selected_attendance_activity)
      selected_activity <- attendance_activities[input$selected_attendance_activity]
      culture_values$processed_data <- load_culture_data_simple(
        input$culture_metric, 
        input$culture_classification_type, 
        selected_activity
      )
    } else if (input$culture_metric == "Participation in any cultural activity") {
      req(input$selected_participation_activity)
      selected_activity <- participation_activities[input$selected_participation_activity]
      culture_values$processed_data <- load_culture_data_simple(
        input$culture_metric, 
        input$culture_classification_type, 
        selected_activity
      )
    } else {
      culture_values$processed_data <- load_culture_data_simple(
        input$culture_metric, 
        input$culture_classification_type
      )
    }
    
    culture_values$data_status <- if(nrow(culture_values$processed_data) > 0) "Culture data loaded" else "No data available"
  })
  
  # Data summary
  output$culture_data_summary <- renderUI({
    req(culture_values$processed_data, input$culture_classification_type, input$culture_metric)
    
    if (nrow(culture_values$processed_data) == 0) {
      return(div(class = "no-data-message",
                 h4("No Data Available"),
                 p("No data available for selected metric and classification.")))
    }
    
    display_name <- get_culture_metric_display_name(input$culture_metric)
    custom_definition <- culture_definitions[[input$culture_metric]]
    custom_insight <- culture_key_insights[[input$culture_metric]]
    custom_notes <- culture_notes[[input$culture_metric]]
    
    # Define source information
    source_info <- switch(input$culture_metric,
                          "Attendance at cultural events and visiting places of culture" = list(
                            text = "Scottish Household Survey",
                            url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
                          ),
                          "Participation in any cultural activity" = list(
                            text = "Scottish Household Survey", 
                            url = "https://www.gov.scot/collections/scottish-household-survey-publications/"
                          ),
                          "Gaelic speakers" = list(
                            text = "Scotland's Census",
                            url = "https://www.scotlandscensus.gov.uk/"
                          ),
                          "Visits to attractions" = list(
                            text = "Bespoke data from Moffat Centre for Travel and Tourism Business Development",
                            url = "https://moffatcentre.uk/"
                          ),
                          list(text = "Data Source", url = "#")
    )
    if (!is.null(custom_insight) && !is.null(custom_definition) && custom_definition != "") {
      insight_content <- tagList( 
        div(
          h4("Key Finding", style = "font-size: 1.75em; margin: 0 0 10px 0; color: #155724; font-weight: 600;"),
          p(custom_insight, style = "font-size: 1.5em; line-height: 1.6; margin: 0 0 15px 0; color: #155724;")
        ),
        div(
          h4("Definition", style = "font-size: 1.75em; margin: 0 0 10px 0; color: #155724; font-weight: 600;"),
          p(custom_definition, style = "font-size: 1.5em; line-height: 1.6; margin: 0 0 15px 0; color: #155724;")
        )
       
      )}
    
      if (!is.null(custom_insight)  && custom_definition == "") {
        insight_content <- tagList( 
          div(
            h4("Key Finding", style = "font-size: 1.75em; margin: 0 0 10px 0; color: #155724; font-weight: 600;"),
            p(custom_insight, style = "font-size: 1.5em; line-height: 1.6; margin: 0 0 15px 0; color: #155724;")
          )
          
        )}
        
    
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
    
    
    return(
      div(
        h4("Culture Data Loaded"),
        p(
          paste("Showing data for:", display_name),
          tags$br(),
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
    ))
    
  })
  # Updated server value boxes to ALWAYS use 2-fold data directly
  output$culture_urban_rate <- renderValueBox({
    req(input$culture_metric)
    
    # Determine selected activity for metrics that need it
    selected_activity <- NULL
    if(input$culture_metric == "Attendance at cultural events and visiting places of culture" && !is.null(input$selected_attendance_activity)) {
      selected_activity <- attendance_activities[input$selected_attendance_activity]
    } else if(input$culture_metric == "Participation in any cultural activity" && !is.null(input$selected_participation_activity)) {
      selected_activity <- participation_activities[input$selected_participation_activity]
    }
    
    # ALWAYS load 2-fold data directly from 2-fold files
    key_insights <- get_simple_key_insights_2fold_only(input$culture_metric, selected_activity)
    val <- format_culture_value(key_insights$urban, input$culture_metric)
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
  output$culture_rural_rate <- renderValueBox({
    req(input$culture_metric)
    
    # Determine selected activity for metrics that need it
    selected_activity <- NULL
    if(input$culture_metric == "Attendance at cultural events and visiting places of culture" && !is.null(input$selected_attendance_activity)) {
      selected_activity <- attendance_activities[input$selected_attendance_activity]
    } else if(input$culture_metric == "Participation in any cultural activity" && !is.null(input$selected_participation_activity)) {
      selected_activity <- participation_activities[input$selected_participation_activity]
    }
    
    # ALWAYS load 2-fold data directly from 2-fold files
    key_insights <- get_simple_key_insights_2fold_only(input$culture_metric, selected_activity)
    val <- format_culture_value(key_insights$rural, input$culture_metric)
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
  
  output$culture_scotland_rate <- renderValueBox({
    req(input$culture_metric)
    
    # Determine selected activity for metrics that need it
    selected_activity <- NULL
    if(input$culture_metric == "Attendance at cultural events and visiting places of culture" && !is.null(input$selected_attendance_activity)) {
      selected_activity <- attendance_activities[input$selected_attendance_activity]
    } else if(input$culture_metric == "Participation in any cultural activity" && !is.null(input$selected_participation_activity)) {
      selected_activity <- participation_activities[input$selected_participation_activity]
    }
    
    # ALWAYS load 2-fold data directly from 2-fold files
    key_insights <- get_simple_key_insights_2fold_only(input$culture_metric, selected_activity)
    val <- format_culture_value(key_insights$scotland, input$culture_metric)
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
  
  output$culture_urban_rural_gap <- renderValueBox({
    req(input$culture_metric)
    
    # Determine selected activity for metrics that need it
    selected_activity <- NULL
    if(input$culture_metric == "Attendance at cultural events and visiting places of culture" && !is.null(input$selected_attendance_activity)) {
      selected_activity <- attendance_activities[input$selected_attendance_activity]
    } else if(input$culture_metric == "Participation in any cultural activity" && !is.null(input$selected_participation_activity)) {
      selected_activity <- participation_activities[input$selected_participation_activity]
    }
    
    # load 2-fold data directly from 2-fold files
    key_insights <- get_simple_key_insights_2fold_only(input$culture_metric, selected_activity)
    val <- calculate_simple_gap(key_insights$urban, key_insights$rural, input$culture_metric)
    
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
  
  # FIXED: Updated trend chart with 45-degree rotated year labels
  output$culture_trend_chart <- renderPlotly({
    req(culture_values$processed_data, input$culture_classification_type)
    
    # Add error handling and debugging
    tryCatch({
      agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
      
      if (nrow(agg_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 16)))
      }
      
      cat("Trend chart - agg_data dimensions:", nrow(agg_data), "x", ncol(agg_data), "\n")
      cat("Trend chart - unique areas:", paste(unique(agg_data$Area), collapse = ", "), "\n")
      
      # Format data for display
      is_visitor_data <- input$culture_metric == "Visits to attractions"
      is_gaelic_data <- input$culture_metric == "Gaelic speakers"
      
      if (is_visitor_data) {
        agg_data$Value_Display <- sapply(agg_data$Value, format_visits_number)
        y_label <- "Number of Visits"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Visits: ", agg_data$Value_Display)
      } else if (is_gaelic_data) {
        agg_data$Value_Rounded <- round(agg_data$Value, 0)
        y_label <- "Number of Speakers"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Speakers: ", sapply(agg_data$Value_Rounded, format_full_number))
      } else {
        agg_data$Value_Rounded <- round(agg_data$Value, 1)
        y_label <- "Percentage (%)"
        tooltip_text <- paste0("Year: ", agg_data$Year, "<br>Area: ", agg_data$Area, "<br>Rate: ", agg_data$Value_Rounded, "%")
      }
      
      agg_data$tooltip <- tooltip_text
      
      
      
      if (input$culture_metric %in% c("Attendance at cultural events and visiting places of culture",
                                      "Participation in any cultural activity"
                                      )) {
        
        agg_data1 <- agg_data |> filter(Year< 2022)
        agg_data2 <- agg_data |> filter(Year>= 2022)
        p <- ggplot(agg_data1, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
          
          
          geom_vline(xintercept = 2019, linetype = "dashed",  color = "darkgrey", size = 0.5) +
          geom_vline(xintercept = 2022, linetype = "dashed",  color = "darkgrey", size = 0.5) +
          
          annotate(
            "text",
            x = 2020.5,  # align with the vertical line
            y = max(agg_data$Value, na.rm = TRUE)-0.5,  # top of the plot
            label = "Data for 2020 and 2021 are not presented due to methodology changes",  # stacked text using newline
            angle = 90,  # vertical orientation
            hjust = 1,   # right-align the text
            vjust = 1.1 # adjust vertical position slightly above the top
          )+
          
          geom_line(size = 1.2, alpha = 0.8) +
         geom_point(size = 3) +
          geom_line(data = agg_data2, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip), size = 1.2,
                    alpha = 0.8) +
          
          
          # Add points for 2022 and 2023
          geom_point(
            data = agg_data2,
            aes(x = Year, y = Value, color = Area, group = Area, text = tooltip),
            size = 3
          ) +
          theme_minimal() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)  # FIXED: 45-degree rotation
          ) +
          labs(x = "Year", y = y_label, color = "Area Type") +
          scale_x_continuous(breaks = function(x) {
            # FIXED: Handle case where x might be empty or have issues
            if(length(x) == 0 || all(is.na(x))) return(c())
            seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
            
            
          }  
          )
      
          
      } else{
      # Create plot
      p <- ggplot(agg_data, aes(x = Year, y = Value, color = Area, group = Area, text = tooltip)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(size = 3) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)  # FIXED: 45-degree rotation
        ) +
        labs(x = "Year", y = y_label, color = "Area Type") +
        scale_x_continuous(breaks = function(x) {
          # FIXED: Handle case where x might be empty or have issues
          if(length(x) == 0 || all(is.na(x))) return(c())
          seq(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)), by = 1)
        
          
        }  
        )
      }
      
      # Apply simple colors with Scotland as gray
      area_colors <- get_simple_colors(unique(agg_data$Area), input$culture_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_color_manual(values = unlist(area_colors))
      }
      
      # Format y-axis
      if (is_visitor_data) {
        p <- p + scale_y_continuous(labels = function(x) sapply(x, format_visits_number))
      } else if (is_gaelic_data) {
        p <- p + scale_y_continuous(labels = function(x) sapply(x, format_full_number))
      } else {
        p <- p + scale_y_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in trend chart:", e$message, "\n")
      print(e)
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })  
      # Comparison chart with Scotland in gray
  output$culture_comparison_chart <- renderPlotly({
    req(culture_values$processed_data, input$culture_classification_type, input$culture_selected_year)
    
    # Add error handling and debugging
    tryCatch({
      agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
      selected_data <- agg_data %>% filter(Year == as.numeric(input$culture_selected_year))
      
      if (nrow(selected_data) == 0) {
        return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("No data available for", input$culture_selected_year), textfont = list(size = 16)))
      }
      
      cat("Comparison chart - selected_data dimensions:", nrow(selected_data), "x", ncol(selected_data), "\n")
      cat("Comparison chart - unique areas:", paste(unique(selected_data$Area), collapse = ", "), "\n")
      
      # Format data
      is_visitor_data <- input$culture_metric == "Visits to attractions"
      is_gaelic_data <- input$culture_metric == "Gaelic speakers"
      
      if (is_visitor_data) {
        selected_data$Value_Display <- sapply(selected_data$Value, format_visits_number)
        x_label <- "Number of Visits"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Visits: ", selected_data$Value_Display)
      } else if (is_gaelic_data) {
        selected_data$Value_Rounded <- round(selected_data$Value, 0)
        x_label <- "Number of Speakers"
        tooltip_text <- paste0("Area: ", selected_data$Area, "<br>Speakers: ", sapply(selected_data$Value_Rounded, format_full_number))
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
      
      # Apply simple colors with Scotland as gray
      area_colors <- get_simple_colors(unique(selected_data$Area), input$culture_classification_type)
      if(length(area_colors) > 0) {
        p <- p + scale_fill_manual(values = unlist(area_colors))
      }
      
      # Format x-axis
      if (is_visitor_data) {
        p <- p + scale_x_continuous(labels = function(x) sapply(x, format_visits_number))
      } else if (is_gaelic_data) {
        p <- p + scale_x_continuous(labels = function(x) sapply(x, format_full_number))
      } else {
        p <- p + scale_x_continuous(labels = scales::percent_format(scale = 1))
      }
      
      ggplotly(p, tooltip = "text")
      
    }, error = function(e) {
      cat("Error in comparison chart:", e$message, "\n")
      print(e)
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message), textfont = list(size = 12))
    })
  })  
  
  # Trend Download Handler
  output$culture_trend_download <- downloadHandler(
    filename = function() {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$culture_metric)
      paste0("Culture_Trend_", metric_name, "_", input$culture_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(culture_values$processed_data, input$culture_classification_type)
      
      agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
      
      if (nrow(agg_data) > 0) {
        download_data <- agg_data %>%
          select(Year, Area, Value, Data_Source) %>%
          arrange(Year, Area)
        
        write.csv(download_data, file, row.names = FALSE)
      }
    }
  )
  
  # Comparison Download Handler
  output$culture_comparison_download<- downloadHandler(
    filename = function() {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$culture_metric)
      paste0("Culture_Comparison_", metric_name, "_", input$culture_classification_type, "_", input$culture_selected_year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(culture_values$processed_data, input$culture_classification_type, input$culture_selected_year)
      
      agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
      selected_data <- agg_data %>% filter(Year == as.numeric(input$culture_selected_year))
      
      if (nrow(selected_data) > 0) {
        download_data <- selected_data %>%
          select(Year, Area, Value, Data_Source) %>%
          arrange(Area)
        
        write.csv(download_data, file, row.names = FALSE)
      }
    }
  )
  
  # Table Download Handler
  output$culture_table_download<- downloadHandler(
    filename = function() {
      metric_name <- gsub("[^A-Za-z0-9]", "_", input$culture_metric)
      paste0("Culture_Table_", metric_name, "_", input$culture_classification_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(culture_values$processed_data, input$culture_classification_type)
      
      agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
      
      if (nrow(agg_data) > 0) {
        filtered_data <- agg_data
        
        if (!is.null(input$culture_table_year_filter) && input$culture_table_year_filter != "all") {
          filtered_data <- filtered_data %>% filter(Year == as.numeric(input$culture_table_year_filter))
        }
        
        if (!is.null(input$culture_table_area_filter) && input$culture_table_area_filter != "all") {
          filtered_data <- filtered_data %>% filter(Area == input$culture_table_area_filter)
        }
        
        download_data <- filtered_data %>%
          select(Year, Area, Value, Data_Source) %>%
          arrange(desc(Year), Area)
        
        write.csv(download_data, file, row.names = FALSE)
      }
    }
  )
  output$test_download <- downloadHandler(
    filename = function() { "test.csv" },
    content = function(file) {
      write.csv(data.frame(x = 1:5, y = letters[1:5]), file, row.names = FALSE)
    }
  )
  
  
  # Data table
  output$culture_data_table <- DT::renderDataTable({
    req(culture_values$processed_data, input$culture_classification_type)
    
    if(nrow(culture_values$processed_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected metric"), options = list(dom = 't')))
    }
    
    display_name <- get_culture_metric_display_name(input$culture_metric)
    agg_data <- simple_aggregate_culture_data(culture_values$processed_data, input$culture_classification_type)
    
    if(nrow(agg_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data available for selected classification"), options = list(dom = 't')))
    }
    
    # Apply filters
    filtered_data <- agg_data
    
    if(!is.null(input$culture_table_year_filter) && input$culture_table_year_filter != "all") {
      filtered_data <- filtered_data %>% filter(Year == as.numeric(input$culture_table_year_filter))
    }
    
    if(!is.null(input$culture_table_area_filter) && input$culture_table_area_filter != "all") {
      filtered_data <- filtered_data %>% filter(Area == input$culture_table_area_filter)
    }
    
    if(nrow(filtered_data) == 0) {
      return(DT::datatable(data.frame(Message = "No data matches the selected filters"), options = list(dom = 't')))
    }
    
    # Prepare table data
    table_data <- filtered_data %>%
      arrange(desc(Year), Area) %>%
      select(Year, Area, Value, Data_Source)
    
    # Format value column based on metric type
    if (input$culture_metric == "Visits to attractions") {
      value_col_name <- "Number of Visits"
      table_data$Value <- round(table_data$Value, 0)
    } else if (input$culture_metric == "Gaelic speakers") {
      value_col_name <- "Number of Speakers"
      table_data$Value <- round(table_data$Value, 0)
    } else {
      value_col_name <- "Percentage (%)"
      table_data$Value <- round(table_data$Value, 1)
    }
    
    names(table_data)[names(table_data) == "Value"] <- value_col_name
    
    dt <- DT::datatable(table_data, 
                        options = list(pageLength = 15, scrollX = TRUE),
                        caption = paste("Data for:", display_name, "by", input$culture_classification_type, "Classification"))
    
    # Apply appropriate formatting
    if (input$culture_metric %in% c("Visits to attractions", "Gaelic speakers")) {
      dt <- dt %>% formatCurrency(columns = value_col_name, currency = "", digits = 0)
    } else {
      dt <- dt %>% formatRound(columns = value_col_name, digits = 1)
    }
    
    return(dt)
  })
}
