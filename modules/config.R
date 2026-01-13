
# modules/config.R - Configuration and shared data structures



# last_update - change here 
last_update <- "December 2025"
# All 9 categories with their information and metrics
CATEGORIES <- list(
  "economy_digital" = list(
    name = "Economy and Digital",
    icon = "chart-line",
    description = "Economic vitality and digital connectivity drive modern rural development. This category examines business growth, employment patterns, digital infrastructure, and economic opportunities across Scotland's regions.",
    bg_image = "economy_img.jpg",
    metrics = c("Gross Value Added", "Business Openings", "Business Closures",
                "Abroad Owned Businesses", "Business Registrations", 
                "Jobs in Abroad Owned Businesses", "High Growth Businesses",
                "Employment", "Economic Activity", "Patterns of Working", 
                "Annual Net Income", "Business Optimism", "Rural Business and Net Zero",
                "Rural Businesses and Efficiency", "Internet Access",
                "Broadband Download Speed", "4G coverage"),
    data_available = TRUE
  ),
  "housing" = list(
    name = "Housing",
    icon = "home",
    description = "Housing availability, quality, and affordability directly impact rural community sustainability. This category tracks household tenure types, housing market dynamics, and residential patterns across urban and rural Scotland.",
    bg_image = "housing_img.jpg",
    metrics = c("Owner occupied", 
                "Owner occupied - owned outright",
                "Owner occupied - buying with loan or mortgage",
                "Private rented",
                "Social rented",
                "Social rented - local authority"),
    data_available = TRUE
  ),
  "transport" = list(
    name = "Transport",
    icon = "route",
    description = "Effective transport networks are crucial for connecting rural communities. This category examines public transport satisfaction, road infrastructure quality, ferry services, and accessibility across Scotland's diverse geography.",
    bg_image = "transport_img.jpg",
    metrics = c("Satisfaction with the quality of public transport", 
                "Local authority road network condition",
                "Ferry reliability"),
    data_available = TRUE
  ),
  "agriculture_marine" = list(
    name = "Agriculture and Marine",
    icon = "tractor",
    description = "Scotland's agricultural and marine sectors form the backbone of rural economies. This category monitors sustainable farming practices, woodland management, marine resource utilization, and environmental stewardship across different geographic regions.",
    bg_image = "agriculture_img.jpg",
    metrics = c("Area of land covered by higher-level or targeted agri-environment schemes",
                "Woodland area certified as sustainably managed", 
                "Regional Gross Value Added",
                "Greenhouse Gas Emissions"),
    data_available = TRUE
  ),
  "environment_climate" = list(
    name = "Environment and Climate Change",
    icon = "leaf",
    description = "Environmental sustainability and climate action are critical for Scotland's future. This category tracks recycling rates, biodiversity conservation, water quality, and environmental protection measures across different communities.",
    bg_image = "environment_img.jpg",
    metrics = c("Percentage of household waste recycled", "Biodiversity measures", "Fresh water condition"),
    data_available = TRUE
  ),
  
  "health_social" = list(
    name = "Health and Social Care",
    icon = "heartbeat",
    description = "Health outcomes and social care quality are fundamental to community wellbeing. This category monitors healthcare access, service quality, health outcomes, and social care provision across urban and rural Scotland.",
    bg_image = "health_img.jpg",
    metrics = c("Quality of care experience", "Self-assessed general health", "Experience of out of hours healthcare"),
    data_available = TRUE
  ),
  
  "population_skills" = list(
    name = "Population, Education and Skills",
    icon = "graduation-cap",
    description = "Demographic trends and educational outcomes shape Scotland's future. This category monitors population changes, age distributions, educational participation, and skill development opportunities across urban and rural areas.",
    bg_image = "education_img.jpg",
    metrics = c("Population by age", "Population change", 
                "The proportion of 16-19 year olds in Scotland participating in education, training or employment"),
    data_available = TRUE
  ),

  "social_justice" = list(
    name = "Social Justice",
    icon = "balance-scale",
    description = "Social equity and community empowerment are essential for inclusive development. This category tracks poverty levels, community ownership, civic participation, and financial wellbeing across different population groups.",
    bg_image = "https://images.unsplash.com/photo-1582213782179-e0d53f98f2ca?ixlib=rb-4.0.3&auto=format&fit=crop&w=1200&q=80",
    metrics = c("Relative poverty", "Relative child poverty", "Transport affordability", 
                "Housing costs as a percentage of earnings",
                "Level of fuel poverty", "Perceptions of influence over decisions affecting local area",
                "Household financial wellbeing", "Number of assets in community ownership"),
    data_available = TRUE
  ),
  

  "culture" = list(
    name = "Culture and Heritage",
    icon = "theater-masks",
    description = "Scotland's rich cultural landscape encompasses traditional arts, language preservation, and visitor attractions. This category tracks participation in cultural activities, attendance at cultural events, Gaelic language retention, and tourism patterns across urban and rural communities.",
    bg_image = "culture_img.jpg",
    metrics = c("Attendance at cultural events and visiting places of culture", 
                "Participation in any cultural activity",
                "Population that can speak Gaelic", 
                "Number of visits to Scotland's visitor attractions"),
    data_available = TRUE
  )

)

# Urban/Rural classification mapping 
URBAN_RURAL_MAPPING <- list(
  "6-fold" = c("Large_Urban_Areas", "Other_Urban_Areas", "Accessible_Small_Towns", 
               "Remote_Small_Towns", "Accessible_Rural", "Remote_Rural"),
  "3-fold" = c("Urban", "Accessible_Rural", "Remote_Rural"),
  "2-fold" = c("Urban", "Rural")
)

# CHANGE: Universal color scheme for all classifications
CLASSIFICATION_COLORS <- list(
  "6-fold" = c("Large_Urban_Areas" = "#FDBE41", "Other_Urban_Areas" = "#F4E470", 
               "Accessible_Small_Towns" = "#80BA27", "Remote_Small_Towns" = "#23A845", 
               "Accessible_Rural" = "#00833E", "Remote_Rural" = "#0E450B"),
  "3-fold" = c("Urban" = "#FDBE41", "Accessible_Rural" = "#80BA27", "Remote_Rural" = "#0E450B"),
  "2-fold" = c("Urban" = "#FDBE41", "Rural" = "#0E450B"),
  "RESAS" = c("Larger Cities" = "#FDBE41", "Urban with Substantial Rural" = "#F4E470", 
              "Mainly Rural" = "#80BA27", "Islands & Remote Rural" = "#0E450B")
)

# Scottish council regions with coordinates 
SCOTTISH_COUNCILS <- list(
  "Aberdeen City" = list(lat = 57.1497, lng = -2.0943),
  "Aberdeenshire" = list(lat = 57.2804, lng = -2.5111),
  "Angus" = list(lat = 56.7156, lng = -2.9667),
  "Argyll & Bute" = list(lat = 56.3333, lng = -5.5),
  "Edinburgh, City of" = list(lat = 55.9533, lng = -3.1883),
  "Clackmannanshire" = list(lat = 56.1194, lng = -3.7503),
  "Dumfries & Galloway" = list(lat = 55.1667, lng = -3.8333),
  "Dundee City" = list(lat = 56.4620, lng = -2.9707),
  "East Ayrshire" = list(lat = 55.4667, lng = -4.2167),
  "East Dunbartonshire" = list(lat = 55.9333, lng = -4.2333),
  "East Lothian" = list(lat = 55.9167, lng = -2.75),
  "East Renfrewshire" = list(lat = 55.7667, lng = -4.3833),
  "Falkirk" = list(lat = 56.0018, lng = -3.7839),
  "Fife" = list(lat = 56.2167, lng = -3.1667),
  "Glasgow City" = list(lat = 55.8642, lng = -4.2518),
  "Highland" = list(lat = 57.4778, lng = -4.2247),
  "Inverclyde" = list(lat = 55.9167, lng = -4.75),
  "Midlothian" = list(lat = 55.8167, lng = -3.0833),
  "Moray" = list(lat = 57.5333, lng = -3.2),
  "North Ayrshire" = list(lat = 55.6333, lng = -4.75),
  "North Lanarkshire" = list(lat = 55.8667, lng = -3.95),
  "Orkney Islands" = list(lat = 59.0000, lng = -3.0000),
  "Perth & Kinross" = list(lat = 56.3950, lng = -3.4310),
  "Renfrewshire" = list(lat = 55.8333, lng = -4.5833),
  "Scottish Borders" = list(lat = 55.5333, lng = -2.7833),
  "Shetland Islands" = list(lat = 60.1544, lng = -1.1544),
  "South Ayrshire" = list(lat = 55.4167, lng = -4.6167),
  "South Lanarkshire" = list(lat = 55.6667, lng = -3.9167),
  "Stirling" = list(lat = 56.1165, lng = -3.9369),
  "West Dunbartonshire" = list(lat = 55.9333, lng = -4.5667),
  "West Lothian" = list(lat = 55.8667, lng = -3.5167),
  "Na h-Eileanan Siar" = list(lat = 57.6956, lng = -7.3089)
)

# CHANGE 3: Common CSS styles 
COMMON_CSS <- "
  .real-data-badge {
    background: #28a745;
    color: white;
    padding: 4px 12px;
    border-radius: 15px;
    font-size: 0.85em;
    margin-left: 10px;
  }
  .coming-soon-badge {
    background: #ffc107;
    color: #212529;
    padding: 4px 12px;
    border-radius: 15px;
    font-size: 0.85em;
    margin-left: 10px;
  }
  
  /* CHANGE 3: Universal blue color #8DEAFD for key insights - KEPT ORIGINAL GREEN for comparison highlights */
  .comparison-highlight {
    background: #e8f5e8;
    border: 1px solid #28a745;
    border-radius: 5px;
    padding: 15px;
    margin: 10px 0;
  }
  
  /* CHANGE 3: ADDED - Key insights now use universal blue color */
  .key-insights-highlight {
    background: #8DEAFD !important;
    border: 1px solid #8DEAFD !important;
    border-radius: 5px;
    padding: 15px;
    margin: 10px 0;
    color: black !important;
  }
  
  .no-data-message {
    background: #f8d7da;
    border: 1px solid #f5c6cb;
    border-radius: 5px;
    padding: 20px;
    margin: 20px 0;
    text-align: center;
  }
  .coming-soon-message {
    background: #fff3cd;
    border: 1px solid #ffeaa7;
    border-radius: 5px;
    padding: 20px;
    margin: 20px 0;
    text-align: center;
  }
"

# CHANGE 1: Generic header UI function 
create_category_header <- function(category_id, additional_controls = NULL) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    # --- Dev banner (appears on category pages) ---
    div(
      id = "dev-banner",
      role = "region",
      `aria-label` = "Notice",
      style = "display: none;",  # JS will show on category view
      span("⚠️ This is a development environment. Some features may be unavailable."),
      span(class = "close", `aria-label` = "Close", title = "Close", HTML("&times;"))
    ),
    
  div(
    class = "category-header",
    style = paste0("background-image: url('", cat_info$bg_image, "'); 
                   background-size: cover; 
                   background-position: center; 
                   background-repeat: no-repeat; 
                   position: relative; 
                   min-height: 120px; 
                   border-radius: 8px; 
                   margin-bottom: 20px;"),
    div(
      style = "background: linear-gradient(135deg, rgba(0,0,0,0.7), rgba(0,0,0,0.4)); 
               position: absolute; 
               top: 0; left: 0; right: 0; bottom: 0; 
               padding: 20px; 
               display: flex; 
               flex-direction: column; 
               justify-content: center; 
               align-items: center; 
               color: white;",
      div(
        style = "display: flex; align-items: center; gap: 20px; flex-wrap: wrap; width: 100%; justify-content: space-between;",
        div(
          style = "text-align: center; flex-grow: 1;",
          icon(cat_info$icon, style = "font-size: 2.5em; margin-bottom: 10px;"),
          h2(cat_info$name, style = "margin: 0; text-shadow: 2px 2px 4px rgba(0,0,0,0.5); font-size: 2.2em;"),
          span(ifelse(cat_info$data_available, "Real Data", "Coming Soon"), 
               class = ifelse(cat_info$data_available, "real-data-badge", "coming-soon-badge"))
        ),
        if(!is.null(additional_controls)) {
          div(style = "display: flex; gap: 15px;", additional_controls)
        }
      ),
      div(
        style = "margin-top: 15px; width: 100%;",
        actionButton("back_to_categories", "← Back to Categories", 
                     class = "btn btn-light")
      )
    )
  )
  )
}

# Generic coming soon UI function 
create_coming_soon_ui <- function(category_id) {
  cat_info <- CATEGORIES[[category_id]]
  
  tagList(
    create_category_header(category_id),
    
    fluidRow(
      box(
        title = paste(cat_info$name, "- Coming Soon"),
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        div(
          class = "coming-soon-message",
          h3("📊 Data Integration in Progress"),
          p("This category is being prepared for real government data integration."),
          
          h4("Planned Metrics:"),
          tags$ul(
            lapply(cat_info$metrics, function(metric) tags$li(metric))
          ),
          
          h4("About this Category:"),
          p(cat_info$description),
          
          br(),
          p("Check back soon for real data analysis in this important policy area!")
        )
      )
    )
  )
}

# CHANGE 8: Function to create Excel download button that's always visible
create_excel_download_button <- function(outputId, filename = "data.xlsx", label = "Download Excel") {
  tags$a(
    class = "excel-download-btn",
    href = "#",
    onclick = paste0("Shiny.setInputValue('", outputId, "', Math.random());"),
    icon("file-excel"),
    label
  )
}

# CHANGE: Function to get colors for classification
get_classification_colors <- function(classification_type) {
  return(CLASSIFICATION_COLORS[[classification_type]])
}

# CHANGE: Function to apply colors to plotly charts
apply_classification_colors <- function(plot, data, classification_type, color_column = "Area") {
  colors <- get_classification_colors(classification_type)
  if (!is.null(colors) && color_column %in% names(data)) {
    # Get unique areas in the data
    unique_areas <- unique(data[[color_column]])
    # Map colors to areas that exist in the data
    plot_colors <- colors[names(colors) %in% unique_areas]
    if (length(plot_colors) > 0) {
      plot <- plot %>% 
        layout(colorway = unname(plot_colors))
    }
  }
  return(plot)
}
