<<<<<<< HEAD
# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(htmltools)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(readxl)
library(stringr)
library(tidyr)
library(scales)
library(openxlsx)
library(later)

# Load configuration
source("modules/config.R")
source("modules/culture_module.R")
source("modules/economy_digital_module.R")
source("modules/housing_module.R")
source("modules/transport_module.R")
source("modules/social_justice_module.R")
source("modules/classification_module.R")
source("modules/agriculture_module.R")
source("modules/population_module.R")
source("modules/health_module.R")
source("modules/environment_module.R")

# Load all module files
# Source all category modules


COMPLETE_METRICS <- list(
 
  "economy_digital" = list(
    name = "Economy and Digital",
    icon = "chart-line",
    data_available = TRUE,
    metrics = list(
      "Regional Gross Value Added" = "Regional Gross Value Added (£ millions)",
      "Number of VAT/PAYE Businesses" = "Number of VAT/PAYE Businesses",
      "High growth private businesses" = "Number of high growth registered private sector businesses",
      "Economic inactivity" = "Economic inactivity rates for population aged 16 to 64",
      "Broadband coverage" = "Residential broadband coverage by service type/download speeds",
      "4G coverage" = "Percentage 4G geographic coverage for at least one Mobile Network Operator (MNO)"
    )
  ),
  "housing" = list(
    name = "Housing",
    icon = "home",
    data_available = TRUE,
    metrics = list(
      "Median Property Price" = "Median Property Price",
      "Second Homes" = "Proportion of all dwellings that are second homes",
      "Vacant Homes" = "Proportion of all dwellings that are vacant homes",
      "New Build Completions" = "New Build Completions",
      "AHSP Completions" = "AHSP Completions",
      "Housing Conditions" = "Dwellings with urgent disrepair to critical elements",
      "EPC rating" = "Proportion of households rated EPC C or above"
    )
  ),
  "transport" = list(
    name = "Transport",
    icon = "route",
    data_available = TRUE,
    metrics = list(
      "Public transport satisfaction" = "Satisfaction with the quality of public transport",
      "Cycle Training" = "Proportion of Primary Schools Delivering On-Road Cycle Training",
      "Road Condition" = "The Percentage Of Roads Needing Repairs (Red And Amber Classification) In Scotland",
      "EV Infrastructure" = "Publicly available electric vehicle charging devices at all speeds by local authority per 100,000 population",
      "Weekly travel costs" = "Weekly travel costs in different Minimum Income Standard (MIS) budgets",
      "Key service access" = "Percentage of population within 15 minute drive time by public transport of key service",
      "Transport affordability" = "How easy or difficult people find it to afford transport costs",
      "Mode of transport (work)" = "How adults usually travel to work (percentages)",
      "Ferry reliability" = "Percentage of Scottish lifeline ferry services that are reliable and punctual by operator"
    )
  ),
  "agriculture_marine" = list(
    name = "Agriculture and Marine",
    icon = "tractor", 
    data_available = TRUE,
    metrics = list(
      "Agri-Environment Schemes" = "Area of land covered by higher-level or targeted agri-environment schemes in England, Wales, Scotland and Northern Ireland",
      "Sustainably Managed Woodland" = "Percentage of UK woodland area certified as sustainably managed by country", 
      "GVA (Agri, Forestry, Fishing)" = "Regional Gross Value Added (GVA) (balanced) in pounds, million by Agriculture, Forestry and Fishing sectors",
      "Employment (Agri, Forestry, Fishing)" = "Percentage (%) Employment by sector: Agriculture, Forestry and Fishing sector",
      "Fish Stocks" = "Estimated % of commercial stocks fished at sustainable levels",
      "Greenhouse Gas Emissions" = "Greenhouse Gas Emissions from Agriculture and LULUCF",
      "Area of woodland creation and new woodland creation" = "New woodland creation",
      "Farm Business Income" = "Average Farm business income by farm type",
      "Diversified Activity And Incomes" = "Average diversified income (% of farm business income - FBI)  and amount of diversified income of farms with diversified activity",
      "New To Crofting" = "New entrants to crofting",
      "Resident Crofters" = "Percentage of crofts with resident crofters",
      "Agritourism Value" = "Value of agritourism"
    )
  ),
  
  "environment_climate" = list(
    name = "Environment and Climate Change",
    icon = "leaf",
    data_available = TRUE,
    metrics = list(
      "Percentage of household waste recycled" = "Percentage (%) of household waste recycled",
      "Renewable Electricity" = "Renewable Electricity Generation (MWh per household)",
      "Clean Seas" = "Clean Seas: the percentage of biogeographic regions with acceptably low levels of contaminants",
      "Biodiversity: Scottish species" = "Biodiversity: combination of three indices of Scottish species - abundance of marine species, abundance of terrestrial species and occupancy of terrestrial species",
      "Fresh water" = "Fresh Water condition: The percentage of river and loch waterbodies achieving 'Good' or better status in terms of four metrics; water quality, water resources (flows and levels), access to fish migration and physical condition",
      "Restored peatland" = "Hectares of restored peatland: Cumulative area of peatland restored since 2012 (thousands hectares)",
      "Biodiversity awareness" = "Awareness, understanding and support for biodiversity conservation",
      "Rare and threatened species" = "Status of rare and threatened species (UK Red List Index)",
      "Forests and woodlands" = "Woodland area by forest type and ownership (thousand hectares)"
    )
  ),
  
  
  "health_social" = list(
    name = "Health and Social Care",
    icon = "heartbeat",
    data_available = TRUE,
    metrics = list(
      "Quality of care" = "Quality of care experience",
      "Self-assessed health" = "Self-assessed general health",
      "Healthy Life Expectancy (males)" = "Healthy Life Expectancy (males)",
      "Healthy Life Expectancy (females)" = "Healthy Life Expectancy (females)",
      "Mental Well-being" = "Warwick-Edinburgh Mental Well-being Scale (WEMWBS) score",
      "GP access" = "How easy it is for people to contact their General Practice in the way they want",
      "Care impact on quality of life" = "Help, care or support improved or maintained quality of life",
      "Access to nature" = "Proportion (%) of adults living within 5 minutes' walk of their nearest green or blue space",
      "Out-of-hours healthcare" = "Experience of out of hours healthcare",
      "Help with Everyday Living" = "Rating of Care, Support and Help with Everyday Living",
      "Care-Life Balance" = "Balancing caring responsibilities and other aspects of life",
      "Support to Continue Caring" = "Feeling supported to continue caring"
    )
  ),
  
  
  "population_skills" = list(
    name = "Population, Education and Skills",
    icon = "graduation-cap",
    data_available = TRUE,
    metrics = list(
      "Median age" = "Median age by urban rural classification",
      "Population change (count)" = "Population change (count) by urban rural classification",
      "Population change (%)" = "Annual percentage change in population by urban rural classification",
      "16-19 yo in education, employment, training" = "The proportion of 16-19 year olds in Scotland participating in education, training or employment",
      "Skill shortage vacancies" = "Incidence of skill shortage vacancies (employer base)",
      "Positive destinations" = "Percentage of school leavers in positive destinations at 9-month follow-up",
      "Primary School Literacy" = "Percentage of primary school pupils achieving expected CfE Levels in Literacy",
      "Primary School Numeracy" = "Percentage of primary school pupils achieving expected CfE Levels in Numeracy",
      "Secondary School Literacy" = "Percentage of secondary school pupils achieving expected CfE Levels in Literacy",
      "Secondary School Numeracy" = "Percentage of secondary school pupils achieving expected CfE Levels in Numeracy"
    )
  ),

  
  "social_justice" = list(
    name = "Social Justice",
    icon = "balance-scale",
    data_available = TRUE,
    metrics = list(
      "Relative poverty" = "Proportion (%) of people who are in relative poverty",
      "Relative child poverty" = "Proportion (%) of children who are in relative poverty",
      "Transport affordability" = "How easy or difficult people find it to afford transport costs",
      "Housing affordability" = "Housing costs as percentage of earnings",
      "Level of fuel poverty" = "Proportion (%) of homes in fuel poverty",
      "Influence over local decisions" = "Perceptions of influence over decisions affecting local area",
      "Managing financially" = "How the household is managing financially",
      "Community ownership" = "Number of assets in community ownership"
    )
  ),
 
  "culture" = list(
    name = "Culture and Heritage",
    icon = "theater-masks",
    data_available = TRUE,
    metrics = list(
      "Attendance at cultural events and visiting places of culture" = "Attendance at cultural events and visiting places of culture",
      "Participation in any cultural activity" = "Participation in any cultural activity",
      "Gaelic speakers" = "Population speaking Gaelic",
      "Visits to attractions" = "Number of visits to Scotland's visitor attractions"
    )
  )
  

)

# Function to reset module states
reset_module_states <- function(session) {
  # Reset all module-specific inputs to their default states
  updateSelectInput(session, "environment_metric", selected = "")
  updateSelectInput(session, "environment_classification_type", selected = NULL)
  updateSelectInput(session, "culture_metric", selected = "")
  updateSelectInput(session, "culture_classification_type", selected = NULL)
  updateSelectInput(session, "transport_metric", selected = "")
  updateSelectInput(session, "transport_classification_type", selected = NULL)
  updateSelectInput(session, "housing_metric", selected = "")
  updateSelectInput(session, "housing_classification_type", selected = NULL)
  updateSelectInput(session, "social_justice_metric", selected = "")
  updateSelectInput(session, "social_justice_classification_type", selected = NULL)
  updateSelectInput(session, "agriculture_metric", selected = "")
  updateSelectInput(session, "agriculture_classification_type", selected = NULL)
  updateSelectInput(session, "population_metric", selected = "")
  updateSelectInput(session, "population_classification_type", selected = NULL)
  updateSelectInput(session, "health_metric", selected = "")
  updateSelectInput(session, "health_classification_type", selected = NULL)
  updateSelectInput(session, "economy_metric", selected = "")
  updateSelectInput(session, "economy_classification_type", selected = NULL)
  
  # Reset sub-metric dropdowns
  updateSelectInput(session, "selected_attendance_activity", selected = NULL)
  updateSelectInput(session, "selected_participation_activity", selected = NULL)
  updateSelectInput(session, "selected_woodlands_activity", selected = NULL)
}
# Main UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Rural Scotland Data Dashboard",
    tags$li(class = "dropdown", id = "sidebar-toggle-header", style = "display: none;")
  ),

  dashboardSidebar(
    sidebarMenu(
      id = 'sidebar',
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Policy Areas", tabName = "categories", icon = icon("th-large")),
      menuItem("List of Policy Metrics", tabName = "metrics_list", icon = icon("list")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    div(
      class = "sidebar-logos",
      img(src = "SG_logo.png", alt = "Scottish Government Logo"),
      img(src = "RESAS_logo.png", alt = "RESAS Logo")
    )
 

  ),
 
  
  dashboardBody(
    tags$head(
      tags$style(HTML(paste0(COMMON_CSS, "
        /* CHANGE: Keep header visible permanently and make sidebar sticky */
        .main-header {
          display: none !important;        
        }
        /* CHANGE: Make sidebar sticky/scrollable */
        .main-sidebar {
          position: fixed !important;
          top: 50px !important;
          bottom: 0 !important;
          left: 0 !important;
          overflow-y: auto !important;
          z-index: 1050 !important;
        }
        
        /* CHANGE: Remove gap at top of sidebar */
        .main-sidebar .sidebar {
          padding-top: 0 !important;
          margin-top: 0 !important;
        }
        
        .main-sidebar .sidebar-menu {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }
        /* SIDEBAR HEADER WITH GREEN BACKGROUND */
        .main-sidebar::before {
          content: 'Rural Scotland Data Dashboard';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          background: #0E450B !important;
          color: white !important;
          padding: 15px 20px;
          font-weight: 600;
          font-size: 14px;
          text-align: center;
          z-index: 1000;
          border-bottom: 1px solid rgba(255,255,255,0.1);
          display: block !important;
        }
        
        /* ADJUST SIDEBAR MENU TO ACCOUNT FOR HEADER */
        .main-sidebar {
          position: fixed !important;
          top: 0 !important;
          bottom: 0 !important;
          left: 0 !important;
          overflow-y: auto !important;
          z-index: 1050 !important;
        }
        
        .main-sidebar .sidebar-menu {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }
        /*  Completely remove gap - target all possible sources */
        .content-wrapper {
          margin-top: 0 !important;
          padding: 0 !important;
          margin-left: 230px !important;
        }
               /* SIDEBAR LOGOS - AT BOTTOM */
        .sidebar-logos {
          position: fixed !important;
          bottom: 0 !important;
          left: 0 !important;
          width: 230px !important;
          background: #222d32 !important;
          z-index: 1000 !important;
          border-top: 1px solid rgba(255,255,255,0.1) !important;
          padding: 0 !important;
          margin: 0 !important;
        }

        .sidebar-logos img {
          width: 100% !important;
          height: auto !important;
          display: block !important;
          background: white !important;
          margin: 0 !important;
          border-radius: 0 !important;
          box-shadow: none !important;
        }
             
                /* Adjust sidebar menu to account for logos */
        .main-sidebar .sidebar-menu {
          padding-bottom: 120px !important; /* Adjust based on logo heights */
        }
        
        /* When sidebar is collapsed, hide logos */
        .sidebar-collapse .sidebar-logos {
          display: none !important;
        }

        /* CONDITIONAL CONTENT PADDING - ONLY WHEN CATEGORY HEADER IS PRESENT */
        
        /* Default state - no extra padding */
        .content-wrapper .content {
          padding: 0 !important;
          margin: 0 !important;
        }
        
        /* Only add padding when category header is present */
        .category-header + .row {
          margin-top: 200px !important;
        }
        
        /* More specific: only add padding to content that comes after category header */
        .category-header ~ * {
          margin-top: 200px !important;
        }
        
        /* Ensure home, categories selection, and metrics list pages have no extra padding */
        #home .content,
        #categories .content:not(.category-view),
        #metrics_list .content,
        #about .content {
          padding-top: 0 !important;
          margin-top: 0 !important;
        }

        /* Override any inherited padding for non-category pages */
        .content-wrapper:not(.category-view) .content {
          padding-top: 0 !important;
        }
        
        /* Specific reset for tab content */
        .tab-content .tab-pane {
          padding-top: 0 !important;
        }        
        /* Target AdminLTE specific classes */
        .skin-blue .content-wrapper,
        .skin-blue .right-side {
          background-color: #ecf0f5 !important;
        }
        
        /* Remove any box margins at the top */
        .content > .row:first-child {
          margin-top: 0 !important;
        }
        
        .content > .row:first-child > .col-sm-12:first-child > .box:first-child {
          margin-top: 0 !important;
        }
        
        /* Adjust for collapsed sidebar */
        .sidebar-collapse .content-wrapper {
          margin-left: 0 !important;
        }
        
        /* Orange buttons for main page */
        #explore_policy_areas,
        #explore_policy_metrics {
          background-color: #FDBE41 !important;
          color: black !important;
          border-color: #FDBE41 !important;
        }
        
        #explore_policy_areas:hover,
        #explore_policy_metrics:hover {
          background-color: #E6A835 !important;
          color: black !important;
          border-color: #E6A835 !important;
        }
        
        /* Hide dashboard header when in category view */
        .category-view .main-header,
        .main-header {
          display: none !important;
        }
        


        /*  Category header base styles - SHORTENED from 180px to 120px */
        .category-header {
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          min-height: 120px;
          border-radius: 8px;
          overflow: visible;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          margin: 15px;
          transition: all 0.3s ease;
          position: relative;
          z-index: 100;
        }
        
        /* Sticky state */
        .category-header.sticky {
          position: fixed !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          width: 100vw !important;
          margin: 0 !important;
          border-radius: 0 !important;
          z-index: 1050 !important;
          box-shadow: 0 2px 15px rgba(0,0,0,0.3) !important;
        }
        
        /* Account for sidebar when sticky */
        .category-header.sticky {
          margin-left: 230px !important;
          width: calc(100vw - 230px) !important;
        }
        
        /* When sidebar is collapsed */
        .sidebar-collapse .category-header.sticky {
          margin-left: 0 !important;
          width: 100vw !important;
        }
        
        /* Compact state when scrolled - SHORTENED from 80px to 60px */
        .category-header.compact {
          min-height: 60px !important;
        }
        
        .category-header.compact .category-header-overlay {
          min-height: 60px !important;
          padding: 15px 25px !important;
          flex-direction: row !important;
          justify-content: space-between !important;
          align-items: center !important;
        }
        
        .category-header.compact h2 {
          font-size: 1.8em !important;
          margin: 0 !important;
        }
        
        .category-header.compact .custom-sidebar-toggle {
          position: absolute !important;
          top: 2px !important;
          left: 2px !important;
          transform: translateY(-50%) !important;
        }
        
        /*  Placeholder to maintain layout when header becomes fixed - SHORTENED */
        .header-placeholder {
          height: 0;
          margin: 15px;
          display: none;
          transition: height 0.3s ease;
        }
        
        .header-placeholder.active {
          display: block;
          height: 120px;
        }
        
        /* Header overlay - SHORTENED */
        .category-header-overlay {
          background: linear-gradient(135deg, rgba(0,0,0,0.7), rgba(0,0,0,0.4));
          position: relative;
          padding: 25px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          color: white;
          border-radius: 8px;
          min-height: 120px;
          transition: all 0.3s ease;
        }
        
        /*  ALL DROPDOWN BUTTONS - NON-TRANSPARENT WHITE WITH BLACK TEXT */
        .selectize-control.single .selectize-input,
        .selectize-control.single .selectize-input.focus,
        .form-control,
        select.form-control {
          background: white !important;
          color: black !important;
          border: 1px solid #ccc !important;
          opacity: 1 !important;
        }
        
        .selectize-dropdown {
          background: white !important;
          border: 1px solid #ccc !important;
          color: black !important;
        }
        
        .selectize-dropdown .option {
          color: black !important;
          background: white !important;
        }
        
        .selectize-dropdown .option.active,
        .selectize-dropdown .option:hover {
          background: #f0f0f0 !important;
          color: black !important;
        }
        
        /* : UNIVERSAL GREEN COLOUR WITH BLACK TEXT for charts, data table, key insights */
        .box-primary > .box-header,
        .box-info > .box-header,
        .box-success > .box-header {
          background: #0E450B !important;
          color: white !important;
          border-bottom: 1px solid #0E450B !important;
        }
        
        .box-primary > .box-header .box-title,
        .box-info > .box-header .box-title,
        .box-success > .box-header .box-title,
        .box-header h3,
        .box-header h4 {
          color: white !important;
          font-weight: 600 !important;
        }
        
        /*  Data table styling  */
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          color: black !important;
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #0E450B !important;
          border-color: #0E450B !important;
          color: white !important;
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: #0E450B !important;
          border-color: #0E450B !important;
          color: white !important;
        }


        /* Fixed banner below the header */
        #dev-banner {
          position: fixed;
          top: 50px;             /* typical header height in shinydashboard */
          left: 230px;
          right: 0;
          z-index: 1030;         /* above body, below modals */
          background: #fff4d6;   /* warm notice background */
          border-bottom: 1px solid #e0c78a;
          color: #5f4b00;
          padding: 10px 16px;
          font-size: 14px;
        }
        #dev-banner .close {
          float: right;
          font-size: 16px;
          line-height: 1;
          color: #5f4b00;
          opacity: 0.6;
        }
        #dev-banner .close:hover { opacity: 1; }

        /* Push the content down when banner is visible */
        body.has-dev-banner .content-wrapper,
        body.has-dev-banner .right-side {
          padding-top: 52px;     /* match banner height (approx.) */
        }

          
        /* CHART DOWNLOAD BUTTONS -  */
        .plotly .modebar {
          opacity: 1 !important;
          visibility: visible !important;
          position: relative !important;
          right: auto !important;
          top: auto !important;
        }
        
        .plotly .modebar-container {
          opacity: 1 !important;
          visibility: visible !important;
        }
        
        .plotly .modebar-group {
          background: white !important;
          border: 1px solid #ccc !important;
          border-radius: 4px !important;
        }
        
        .plotly .modebar-btn {
          color: #666 !important;
          background: white !important;
        }
        
        .plotly .modebar-btn:hover {
          background: #0E450B !important;
          color: white !important;
        }
        /* Force category headers to be edge-to-edge at very top */
        .category-header,
        .category-header.sticky,
        .category-header.compact,
        .category-header.header-processed {
          position: fixed !important;
          top: 0 !important;              /* ← CHANGED: From 50px to 0 */
          left: 230px !important;         /* ← CHANGED: From 245px to 230px (sidebar width) */
          right: 0 !important;            /* ← CHANGED: From 15px to 0 */
          z-index: 1040 !important;
          margin: 0 !important;
          border-radius: 0 !important;
          min-height: 180px !important;
          max-height: 180px !important;
          width: calc(100vw - 230px) !important;  /* ← CHANGED: Full viewport width minus sidebar */
        }
        
        /* Account for collapsed sidebar */
        .sidebar-collapse .category-header,
        .sidebar-collapse .category-header.sticky,
        .sidebar-collapse .category-header.compact {
          left: 0 !important;             /* ← CHANGED: From 15px to 0 */
          width: 100vw !important;        /* ← CHANGED: Full viewport width */
        }        
        /* Add spacing to prevent content overlap */
        .category-header + * {
          margin-top: 200px !important;
        }
        
        /* Disable all placeholder behavior */
        .header-placeholder,
        .header-placeholder.active {
          display: none !important;
          height: 0 !important;
        }
        /*  Excel download button styling - always visible */
        .excel-download-btn {
          background: white !important;
          color: black !important;
          border: 1px solid #ccc !important;
          border-radius: 4px !important;
          padding: 8px 12px !important;
          margin: 10px 0 !important;
          display: inline-block !important;
          text-decoration: none !important;
          font-size: 14px !important;
        }
        
        .excel-download-btn:hover {
          background: #0E450B !important;
          color: white !important;
          border-color: #0E450B !important;
          text-decoration: none !important;
        }
        
        .excel-download-btn i {
          margin-right: 5px !important;
        }
        
        /* CHANGE 9: DISABLE WARNINGS CSS */
        .shiny-notification-warning,
        .shiny-notification-error {
          display: none !important;
        }
        
        /*  SIDEBAR TOGGLE BUTTON - TOP LEFT OF CATEGORY HEADER */
        .custom-sidebar-toggle {
          background: rgba(255, 255, 255, 0.9) !important;
          border: none !important;
          border-radius: 4px !important;
          padding: 0 !important;
          color: black !important;
          font-size: 14px !important;
          transition: all 0.3s ease !important;
          cursor: pointer !important;
          position: absolute !important;
          left: 0px !important;
          top: 0px !important;
          z-index: 2001 !important;
          width: 32px !important;
          height: 32px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.2) !important;
        }
        
        .custom-sidebar-toggle:hover {
          background: rgba(255, 255, 255, 1) !important;
          transform: scale(1.05) !important;
          color: black !important;
          box-shadow: 0 4px 8px rgba(0,0,0,0.3) !important;
        }
        
        .custom-sidebar-toggle:focus {
          outline: none !important;
          box-shadow: 0 0 0 3px rgba(0,0,0,0.2) !important;
        }
        
        /* CREATE CUSTOM HAMBURGER LINES  */
        .custom-sidebar-toggle::before {
          content: '' !important;
          position: absolute !important;
          width: 18px;
          height: 2px;
          background: black;
          box-shadow: 
            0 -6px 0 black,
            0 6px 0 black;
          display: block;
        }
        
        /* HIDE ANY FA ICON IF PRESENT */
        .custom-sidebar-toggle i {
          display: none !important;
        }        
        /* ENSURE CATEGORY HEADER HAS RELATIVE POSITIONING FOR ABSOLUTE CHILD */
        .category-header {
          position: relative !important;
        }
        
        .category-header-overlay {
          position: relative !important;
        }        
          /* Hide default dashboard header toggle */
        .main-header .navbar-toggle {
          display: none !important;
        }
        
        /*  Reduce gaps between policy area tiles */
        .category-card {
          background: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          padding: 5px;
          margin: 5px; /* CHANGED: Reduced from 10px to 5px */
          cursor: pointer;
          transition: all 0.3s ease;
          text-align: center;
        }
        .category-card:hover {
          background: #e9ecef;
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        .category-btn {
          width: 100% !important;
          height: 100% !important;
          border: none !important;
          background: transparent !important;
          box-shadow: none !important;
          padding: 15px !important;
        }
        .category-btn:hover {
          background: transparent !important;
        }
        .featured-category {
          border: 2px solid #007bff !important;
          background: #f0f8ff !important;
        }
        .coming-soon-category {
          opacity: 0.6;
          border: 2px dashed #ccc !important;
        }
        
        /*  Center text for specific category tiles */
        .category-card[data-category='population_skills'] h4,
        .category-card[data-category='environment_climate'] h4 {
          text-align: center !important;
        }
        
        .hero-section {
          box-shadow: 0 8px 16px rgba(0,0,0,0.2);
        }
        .hero-section h1 {
          animation: fadeInUp 1s ease-out;
        }
        .hero-section h3 {
          animation: fadeInUp 1s ease-out 0.3s both;
        }
        .hero-section .btn {
          animation: fadeInUp 1s ease-out 0.6s both;
          transition: all 0.3s ease;
        }
        .hero-section .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.3);
        }
        .classification-link:hover {
          color: #23527c !important;
          text-decoration: underline !important;
        }
        .metrics-card:hover {
          background-color: #e3f2fd !important;
          border-color: #007bff !important;
          transform: translateX(5px);
          box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
        }
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        .content-section {
          margin-top: 20px;
        }
        .box {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
        .classification-tabs .nav-tabs {
          margin-bottom: 20px;
        }
        .classification-tabs .nav-tabs > li > a {
          font-size: 14px;
          padding: 8px 12px;
        }
      ")))
    ),
    
    # JavaScript to disable warnings
    tags$script(HTML("
      // DISABLE SHINY WARNINGS
      $(document).ready(function() {
        // Hide all existing warnings
        $('.shiny-notification-warning, .shiny-notification-error').hide();
        
        // Monitor for new warnings and hide them
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            mutation.addedNodes.forEach(function(node) {
              if (node.nodeType === 1) {
                var warnings = $(node).find('.shiny-notification-warning, .shiny-notification-error');
                if (warnings.length > 0) {
                  warnings.hide();
                }
                if ($(node).hasClass('shiny-notification-warning') || $(node).hasClass('shiny-notification-error')) {
                  $(node).hide();
                }
              }
            });
          });
        });
        
        observer.observe(document.body, {
          childList: true,
          subtree: true
        });
        
        console.log('Warning suppression activated');
      });
    ")),
    
    tags$script(HTML("
      $(document).ready(function() {
        console.log('Improved sticky header script loading...');
        
        var stickyHeader = {
          init: function() {
            this.bindEvents();
            this.setupHeaders();
          },
          
          bindEvents: function() {
            var self = this;
            
            $(document).on('click', '#custom-sidebar-toggle', function(e) {
              e.preventDefault();
              e.stopPropagation();
              console.log('Custom sidebar toggle clicked');
              $('body').toggleClass('sidebar-collapse');
              
              setTimeout(function() {
                $(window).trigger('resize');
              }, 350);
            });
            
            $(document).on('shiny:value shiny:inputchanged', function(event) {
              setTimeout(function() {
                self.setupHeaders();
              }, 300);
            });
          },
          
          setupHeaders: function() {
            var self = this;
            $('.category-header').each(function() {
              var $header = $(this);
              
              if (!$header.hasClass('header-processed')) {
                console.log('Setting up header...');
                $header.addClass('header-processed');
                self.addSidebarToggle($header);
              }
            });
          },
          
          addSidebarToggle: function($header) {
            var $overlay = $header.find('.category-header-overlay');
            if ($overlay.length && !$overlay.find('.custom-sidebar-toggle').length) {
              var toggleBtn = $('<button class=custom-sidebar-toggle id=custom-sidebar-toggle><i class=fa fa-bars></i></button>');
              $overlay.append(toggleBtn);
            }
          }
        };
        
        stickyHeader.init();
        console.log('Improved sticky header script loaded successfully');
      });
    ")),
    
    
    # --- Banner placeholder
    uiOutput('dev_banner'),
    
    tabItems(
      tabItem(tabName = "home",
              # Hero section with background image
              fluidRow(
                div(
                  class = "hero-section",
                  style = "background-image: url('home_page_img.jpg');
                           background-size: cover;
                           background-position: center;
                           background-repeat: no-repeat;
                           position: relative;
                           min-height: 400px;
                           border-radius: 8px;
                           margin-bottom: 30px;
                           overflow: hidden;",
                  div(
                    style = "background: linear-gradient(135deg, rgba(0,0,0,0.6), rgba(0,0,0,0.3));
                             position: absolute;
                             top: 0; left: 0; right: 0; bottom: 0;
                             padding: 40px;
                             display: flex;
                             flex-direction: column;
                             justify-content: center;
                             align-items: center;
                             color: white;
                             text-align: center;",
                    h1("Rural Scotland Data Dashboard", 
                       style = "font-size: 3.5em; margin-bottom: 20px; text-shadow: 2px 2px 4px rgba(0,0,0,0.7); font-weight: bold;"),
                    # h3("Supporting the Rural Delivery Plan",
                    #    style = "margin-bottom: 30px; text-shadow: 1px 1px 2px rgba(0,0,0,0.7);"),
                    # CHANGE: Replace single button with two side-by-side buttons
                    div(
                      style = "display: flex; gap: 20px; justify-content: center; flex-wrap: wrap; margin-top: 20px;",
                      actionButton("explore_policy_areas", "Explore Key Policy Areas", 
                                   class = "btn btn-primary btn-lg",
                                   icon = icon("th-large"),
                                   style = "padding: 15px 30px; font-size: 1.2em;"),
                      actionButton("explore_policy_metrics", "Explore Key Policy Metrics", 
                                   class = "btn btn-success btn-lg",
                                   icon = icon("list"),
                                   style = "padding: 15px 30px; font-size: 1.2em;")
                    )
                  )
                )
              ),
              
              # Main content section 
              fluidRow(
                column(12,
                       box(
                         title = "Introduction", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = 12,
                         
                         p("This dashboard compiles existing data on a range of issues that concern rural Scotland. The data is organised according to nine thematic policy areas, each containing a range of metrics. Where possible a geographic breakdown of the data is included so that comparisons can be drawn between different types of rural areas within Scotland, as well as Scotland as a whole. The dashboard is designed to compliment the Rural Delivery Plan (see the About section for more information). An explanation of the geography is available below."),
                         
                         h4("Dashboard Categories:"),
                         
                         tags$ul(
                           lapply(names(CATEGORIES), function(cat_id) {
                             cat_info <- CATEGORIES[[cat_id]]
                             tags$li(
                               actionLink(paste0("cat_link_", cat_id), cat_info$name, 
                                          style = "color: #007bff; font-weight: bold; text-decoration: none;")
                             )
                           })
                         )
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       box(
                         title = "Rural Scotland geographic classifications", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = 12,
                         div(
                        #    style = "line-height: 1.6;",
                        #    p("Rural areas constitute 98% of Scotland's landmass and 17% of its population. The Scottish Government core definition of rurality classifies areas with a population of fewer than 3,000 people to be rural.",
                        #      style = "font-size: 1.1em; margin-bottom: 20px;"),
                        #    
                        #    h4("Presenting data on rural Scotland", style = "color: #337ab7; margin-bottom: 15px;"),
                        #    
                        #    p("Data in this dashboard use the most appropriate available classification for rural and urban areas.", 
                        #    style = "margin-bottom: 20px;"),
                        #    p("Where possible it uses the Scottish Government Urban Rural Classification 2020 which provide a consistent way of defining urban and rural areas across Scotland.  It is based on two main criteria:", 
                        #  style = "margin-bottom: 20px;"),
                        # ul("1. Population as defined by the National Records of Scotland", 
                        #    style = "margin-bottom: 20px;"),
                         
                           
                           style = "line-height: 1.6;",
                           
                           p("Data in this dashboard use the most appropriate available classification for rural and urban areas."),
                           
                           # Paragraph with hyperlink and bold text
                           tags$p(
                             "Where possible it uses the ",
                             tags$a(
                               href = "https://www.gov.scot/publications/scottish-government-urban-rural-classification-2020/documents/",
                               target = "_blank",
                               tags$strong("Scottish Government Urban Rural Classification 2020")
                             ),
                             " which provides a consistent way of defining urban and rural areas across Scotland. It is based on two main criteria:"
                           ),
                           
                           # Ordered list
                           tags$ol(
                             tags$li("Population as defined by the National Records of Scotland"),
                             tags$li("Accessibility based on drive time to a Settlement with a population of 10,000 or more.")
                           ),
                           
                           p("These criteria can be used to aggregate data at twofold, threefold, sixfold and eightfold classifications."),
                           p("The Scottish Government Urban Rural Classification is updated on occasion to reflect changes to Settlement populations and Settlement boundaries and, to a lesser extent, changes to the road network and the datasets on travel speed. Some older data may use an earlier version of the classification system and more recent data may use the ",
                             tags$a(
                               href = "https://www.gov.scot/publications/scottish-government-urban-rural-classification-2022/pages/changes-with-this-release/",
                               target = "_blank",
                               tags$strong("Scottish Government Urban Rural Classification 2022")
                             ),
                             " which was released in 2024."),
                           p("On occasions where data cannot be categorised by population and accessibility the data may be categorised using the Scottish Government’s ",
                             tags$a(
                               href = "https://www.gov.scot/publications/understanding-scottish-rural-economy/pages/13/",
                               target = "_blank",
                               tags$strong(" RESAS Classification of Urban and Rural Local Authorities")
                             ),
                           " . This clusters local authorities according to their level of rurality and establishes four different groups, which are labelled as ‘larger cities’, ‘urban with substantial rural areas’, ‘mainly rural’ and ‘islands and remote.’"),
                           p("In some instances it may not be possible to provide a geographic breakdown of the data for metrics in this dashboard, or it may not be appropriate or relevant to do so."),
                           p("The Rural Delivery Plan focuses on mainland rural Scotland only whilst the National Islands Plan sets out a plan for island communities. It should be noted that data in this dashboard includes data from the Scottish islands as this aligns with the main collection and analysis modes. A geographic classification for mainland rural Scotland, excluding the islands, does not currently exist and would duplicate most if not all of the data in this dashboard."),
                           p("Click on the classifications below to learn more:"),
                           
                           # Classification tabs within the panel
                           div(
                             class = "classification-tabs",
                             tabsetPanel(
                               id = "classification_tabs",
                               type = "tabs",
                               tabPanel("2-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 2-fold"),
                                                p("The Scottish Government core definition of rurality classifies areas with a population of fewer than 3,000 people to be rural. The Scottish Government Urban Rural Classification can be collapsed to this core definition, to create a 2-fold classification:"),
                                                tags$ol(
                                                  tags$li(strong("Rest of Scotland:"), "(1) Large Urban Areas, (2) Other Urban Areas, (3) Accessible Small Towns, and (4) Remote Small Towns."),
                                                  tags$li(strong("Rural Scotland:"), "(5) Accessible Rural and (6) Remote Rural Areas.")
                                                ),
                                                div(class = "alert alert-info", style = "margin-top: 15px;",
                                                    p(strong("Note:"), "On slides where a further breakdown of the data is not possible (for example, because the sample is too small), figures are presented in two categories: 'Rural' and 'Urban'. In this classification, the 'Rural' category includes accessible and remote rural areas.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "2_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "2-fold Classification Map")
                                            )
                                        )
                               ),
                               tabPanel("3-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 3-fold"),
                                                p("The 3-fold classification simplifies the rural categories:"),
                                                tags$ol(
                                                  tags$li(strong("Rest of Scotland:"), "All urban areas including large urban areas, other urban areas, and small towns."),
                                                  tags$li(strong("Accessible Rural:"), "Rural areas within 30 minutes drive time of settlements of 10,000 or more people."),
                                                  tags$li(strong("Remote Rural:"), "Rural areas with drive time of over 30 minutes to settlements of 10,000 or more people.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "3_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "3-fold Classification Map")
                                            )
                                        )
                               ),
                               tabPanel("6-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 6-fold"),
                                                p("The 6-fold classification provides the most detailed breakdown of urban and rural areas in Scotland:"),
                                                tags$ol(
                                                  tags$li(strong("Large Urban Areas:"), "Settlements of 125,000 people and over."),
                                                  tags$li(strong("Other Urban Areas:"), "Settlements of 10,000 to 124,999 people."),
                                                  tags$li(strong("Accessible Small Towns:"), "Settlements of 3,000 to 9,999 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Small Towns:"), "Settlements of 3,000 to 9,999 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Accessible Rural Areas:"), "Areas with a population of less than 3,000 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Rural Areas:"), "Areas with a population of less than 3,000 people, and with a drive time of over 30 minutes but less than or equal to 60 minutes to a Settlement of 10,000 or more.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "6_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "6-fold Classification Map")
                                            )
                                            
                                        )
                               ),
                               tabPanel("8-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 8-fold"),
                                                p("The 8-fold classification provides the most detailed breakdown including very remote categories:"),
                                                tags$ol(
                                                  tags$li(strong("Large Urban Areas:"), "Settlements of 125,000 people and over."),
                                                  tags$li(strong("Other Urban Areas:"), "Settlements of 10,000 to 124,999 people."),
                                                  tags$li(strong("Accessible Small Towns:"), "Settlements of 3,000 to 9,999 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Small Towns:"), "Settlements of 3,000 to 9,999 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Very Remote Small Towns:"), "Settlements of 3,000 to 9,999 people, and with a drive time of over 60 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Accessible Rural Areas:"), "Areas with a population of less than 3,000 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Rural Areas:"), "Areas with a population of less than 3,000 people, and with a drive time of over 30 minutes but less than or equal to 60 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Very Remote Rural Areas:"), "Areas with a population of less than 3,000 people, and with a drive time of over 60 minutes to a Settlement of 10,000 or more.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "8_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "8-fold Classification Map")
                                            )
                                        )
                               ),
                               tabPanel("RESAS Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Rural & Environmental Science and Analytical Services (RESAS) Classification"),
                                                p("This classification classifies local authorities according to their level of rurality and establishes four different groups as well as a broader urban-rural grouping:"),
                                                
                                                h5("Rural Group:"),
                                                tags$ol(
                                                  tags$li(strong("Islands and Remote Rural:"), "Local authorities with significant island populations or very remote mainland areas."),
                                                  tags$li(strong("Mainly Rural:"), "Local authorities where rural areas dominate the landscape and population.")
                                                ),
                                                
                                                h5("Urban Group:"),
                                                tags$ol(start = 3,
                                                        tags$li(strong("Urban with Substantial Rural:"), "Local authorities with significant urban centers but also substantial rural populations."),
                                                        tags$li(strong("Larger Cities:"), "Local authorities dominated by major urban centers and cities.")
                                                )
                                            ),
                                            # Image (right side) - Note the PNG extension for this one
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "4_fold_image.png", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "RESAS 4-fold Classification Map")
                                            )
                                        )
                               )
                             )                      
                           )
                         )
                       )
                )
              )
      ),
      
      # Categories Selection Tab
      tabItem(tabName = "categories",
              uiOutput("categories_main_content")
      ),
      
      # List of Metrics Tab 
      tabItem(tabName = "metrics_list",
              fluidRow(
                box(
                  title = "Complete List of Policy Metrics by Policy Area", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  p("Click on any policy metric name to navigate directly to that metric within its policy area.", 
                    style = "font-size: 1.1em; margin-bottom: 20px;"),
                  
                  # Generate metrics list dynamically using actual module definitions
                  div(
                    style = "margin-top: 20px;",
                    lapply(names(COMPLETE_METRICS), function(cat_id) {
                      cat_info <- COMPLETE_METRICS[[cat_id]]
                      
                      div(
                        style = "margin-bottom: 35px; border-left: 4px solid #007bff; padding-left: 20px; background: #f8f9fa; border-radius: 8px; padding: 20px;",
                        
                        # Category header with icon and status
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px; border-bottom: 2px solid #e9ecef; padding-bottom: 15px;",
                          div(
                            style = "display: flex; align-items: center;",
                            icon(cat_info$icon, style = "font-size: 2em; margin-right: 15px; color: #007bff;"),
                            h3(cat_info$name, style = "margin: 0; color: #007bff; font-weight: 600;")
                          )
                        ),
                        
                        # Metrics list for this category - show clickable links with full names
                        div(
                          style = "display: grid; grid-template-columns: 1fr; gap: 12px;",
                          lapply(names(cat_info$metrics), function(metric_key) {
                            full_name <- cat_info$metrics[[metric_key]]
                            # Create a unique action button for each metric
                            metric_id <- paste0("metric_link_", gsub("[^A-Za-z0-9]", "_", paste(cat_id, metric_key, sep = "_")))
                            
                            div(
                              class = "metrics-card",
                              style = "background: white; border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; transition: all 0.2s ease;",
                              actionLink(
                                inputId = metric_id,
                                label = div(
                                  style = "display: flex; align-items: flex-start; justify-content: space-between;",
                                  div(
                                    div(
                                      style = "display: flex; align-items: center; margin-bottom: 5px;",
                                      icon("chart-bar", style = "margin-right: 10px; color: #28a745; font-size: 1.1em;"),
                                      strong(metric_key, style = "color: #007bff; font-size: 1.05em;")
                                    ),
                                    div(
                                      style = "margin-left: 25px; color: #6c757d; font-size: 0.95em; line-height: 1.4;",
                                      full_name
                                    )
                                  ),
                                  icon("external-link-alt", style = "color: #6c757d; font-size: 1em; margin-top: 2px;")
                                ),
                                style = "text-decoration: none; display: block; width: 100%;"
                              )
                            )
                          })
                        )
                      )
                    })
                  )
                )
              )
      ),
      
      # About Tab 
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Dashboard", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  h4("The Rural Scotland Data Dashboard 2025"),
                  p("This dashboard compiles existing data on a range of issues that concern rural Scotland across nine key policy areas."),
                  p("The Scottish Government committed to publishing a Rural Delivery Plan showing how it is delivering for rural Scotland, by 2026. The plan is for mainland rural Scotland only.  A Rural Scotland Data Dashboard was initially produced in 2023 to inform the development of the Rural Delivery Plan. It can be viewed here:", tags$a(
                    href = "https://www.gov.scot/publications/rural-scotland-data-dashboard-overview/",
                    target = "_blank",
                    tags$strong("Rural Scotland Data Dashboard (2023)"))),
                  p("Ahead of the publication of the Rural Delivery Plan, the Rural Data Dashboard has been redeveloped and updated to align with the thematic areas and objectives of the Plan."),
                  p("This version, the Rural Scotland Data Dashboard (2025), has been developed in R Shiny. It includes a refined set of metrics and will contribute to ongoing monitoring of the Plan. The data can be explored by thematic area and includes the option to download the data. The data has been updated to reflect updated sources. Data presented in the Rural Scotland Data Dashboard (2025) comes from a range of existing sources, such as Scotland’s Census, the Scottish Household Survey and the Scottish House Condition Survey, the Health and Care Experience survey and the Family Resources Survey Scotland, among others."),
                  p(tags$strong("It should be noted that the 2023 version of the dashboard is no longer being updated."), paste0("This version is in beta.", " It was last updated in ", last_update, ".")),
                  p("If you have any feedback please contact:", tags$a(href = "mailto:socialresearch@gov.scot",target = "_blank","socialresearch@gov.scot"), "."),
                  
                  # h4("Data Sources:"),
                  # tags$ul(
                  #   tags$li(tags$strong("Scottish Household Survey:"), "Cultural attendance and participation data, transport satisfaction"),
                  #   tags$li(tags$strong("Scotland's Census:"), "Population and demographic data"),
                  #   tags$li(tags$strong("Sub-Scotland Economic Statistics Database:"), "Economic, business, and employment data"),
                  #   tags$li(tags$strong("Local Authority Data:"), "Council-level performance metrics")
                  # ),
                  h4("Technical Architecture:"),
                  p("The dashboard is built with a modular architecture, with separate modules for each policy category. This allows for:"),
                  tags$ul(
                    tags$li("Independent development of category-specific analysis"),
                    tags$li("Addition of new data sources and metrics"),
                    tags$li("Maintainable and scalable codebase"),
                    tags$li("Consistent user experience across categories")
                  )
                )
              )
      )
    )
  )
) 
# Main Server
server <- function(input, output, session) {
  # Reactive values for overall app state
  values <- reactiveValues(
    current_category = NULL,
    current_classification = NULL,
    app_data_status = "Ready",
    navigation_source = "categories"
  )
  culture_server("culture", values, session)
  agriculture_server("agriculture_marine", values, session)
  transport_server("transport", values, session)
  housing_server("housing", values, session)
  population_server("population_skills", values, session)
  economy_server("economy_digital", values, session)
  social_justice_server("social_justice", values, session)
  health_server("health_social", values, session)
  environment_server("environment_climate", values, session)
  
  # Dynamic back button text based on navigation source
  output$back_button_text <- renderText({
    if(values$navigation_source == "metrics_list") {
      "← Back to List of Metrics"
    } else {
      "← Back to Categories"
    }
  })
  
  # Reset module inputs when category changes
  observeEvent(values$current_category, {
    cat("Category changed to:", values$current_category, "\n")
    if (is.null(values$current_category)) {
      cat("Resetting all module inputs due to no category selected\n")
      # Reset all modules when returning to category selection
      updateSelectInput(session, "culture_metric", selected = "")
      updateSelectInput(session, "agriculture_metric", selected = "")
      updateSelectInput(session, "transport_metric", selected = "")
      updateSelectInput(session, "housing_metric", selected = "")
      updateSelectInput(session, "population_metric", selected = "")
      updateSelectInput(session, "economy_metric", selected = "")
      updateSelectInput(session, "social_justice_metric", selected = "")
      updateSelectInput(session, "health_metric", selected = "")
      updateSelectInput(session, "environment_metric", selected = "")
      updateSelectInput(session, "environment_classification_type", selected = "")
      updateSelectInput(session, "selected_woodlands_activity", selected = names(woodlands_sub_metrics)[1])
      updateSelectInput(session, "agriculture_classification_type", selected = "")
      updateSelectInput(session, "selected_diversified_metric", selected = names(diversified_activity_sub_metrics)[1])
      # Add resets for other modules' sub-metric dropdowns if applicable
    } else {
      # Reset specific module inputs based on category
      if (values$current_category == "environment_climate") {
        cat("Resetting Environment module inputs\n")
        updateSelectInput(session, "environment_metric", selected = "")
        updateSelectInput(session, "environment_classification_type", selected = "")
        updateSelectInput(session, "selected_woodlands_activity", selected = names(woodlands_sub_metrics)[1])
      } else if (values$current_category == "agriculture_marine") {
        cat("Resetting Agriculture module inputs\n")
        updateSelectInput(session, "agriculture_metric", selected = "")
        updateSelectInput(session, "agriculture_classification_type", selected = "")
        updateSelectInput(session, "selected_diversified_metric", selected = names(diversified_activity_sub_metrics)[1])
      } else if (values$current_category == "culture") {
        cat("Resetting Culture module inputs\n")
        updateSelectInput(session, "culture_metric", selected = "")
        updateSelectInput(session, "culture_classification_type", selected = "")
        # Reset sub-metric dropdowns for Culture if applicable
      } else if (values$current_category == "transport") {
        cat("Resetting Transport module inputs\n")
        updateSelectInput(session, "transport_metric", selected = "")
        # Add classification or sub-metric resets if applicable
      } else if (values$current_category == "housing") {
        cat("Resetting Housing module inputs\n")
        updateSelectInput(session, "housing_metric", selected = "")
      } else if (values$current_category == "population_skills") {
        cat("Resetting Population module inputs\n")
        updateSelectInput(session, "population_metric", selected = "")
      } else if (values$current_category == "economy_digital") {
        cat("Resetting Economy module inputs\n")
        updateSelectInput(session, "economy_metric", selected = "")
      } else if (values$current_category == "social_justice") {
        cat("Resetting Social Justice module inputs\n")
        updateSelectInput(session, "social_justice_metric", selected = "")
      } else if (values$current_category == "health_social") {
        cat("Resetting Health module inputs\n")
        updateSelectInput(session, "health_metric", selected = "")
      }
    }
  })
  
  # Handle navigation
  observeEvent(input$explore_policy_areas, {
    updateTabItems(session, "sidebar", selected = "categories")
    values$navigation_source <- "categories"
    values$current_category <- NULL
  })
  
  observeEvent(input$explore_policy_metrics, {
    updateTabItems(session, "sidebar", selected = "metrics_list")
    values$navigation_source <- "categories"
    values$current_category <- NULL
  })
  
  # Handle category link clicks from home page - UPDATED WITH STATE RESET
  lapply(names(CATEGORIES), function(cat_id) {
    observeEvent(input[[paste0("cat_link_", cat_id)]], {
      # Reset all module states before switching
      reset_module_states(session)
      
      # Small delay to ensure resets are processed
      later::later(function() {
        values$current_category <- cat_id
        values$navigation_source <- "categories"
        updateTabItems(session, "sidebar", selected = "categories")
      }, delay = 0.1)
    })
  })
  
  # Handle back button with context awareness - UPDATED WITH STATE RESET
  observeEvent(input$back_to_categories, {
    # Reset all module states when going back
    reset_module_states(session)
    
    if(values$navigation_source == "metrics_list") {
      updateTabItems(session, "sidebar", selected = "metrics_list")
    } else {
      updateTabItems(session, "sidebar", selected = "categories")
    }
    
    # Reset category selection after a small delay
    later::later(function() {
      values$current_category <- NULL
      values$navigation_source <- "categories"
    }, delay = 0.1)
  })
  
  # Main categories content
  output$categories_main_content <- renderUI({
    if (is.null(values$current_category)) {
      fluidRow(
        box(
          title = "Select a Policy Area to Explore", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 15px;",
            lapply(names(CATEGORIES), function(cat_id) {
              cat_info <- CATEGORIES[[cat_id]]
              is_featured <- cat_info$data_available
              is_available <- cat_info$data_available
              div(
                class = paste0("category-card", 
                               ifelse(is_featured, " featured-category", ""),
                               ifelse(!is_available, " coming-soon-category", "")),
                `data-category` = cat_id,
                style = "padding: 20px;",
                actionButton(
                  inputId = paste0("cat_", cat_id),
                  label = div(
                    icon(cat_info$icon, style = "font-size: 2em; margin-bottom: 10px;"),
                    h4(case_when(
                      cat_id == "population_skills" ~ "Population, Education & Skills",
                      cat_id == "environment_climate" ~ "Environment & Climate",
                      cat_id == "agriculture_marine" ~ "Agriculture & Marine",
                      cat_id == "economy_digital" ~ "Economy & Digital",
                      cat_id == "health_social" ~ "Health & Social Care",
                      TRUE ~ cat_info$name
                    ), style = paste0("margin: 10px 0;", 
                                      ifelse(is_featured, " color: #007bff;", ""),
                                      if(cat_id %in% c("population_skills", "environment_climate")) " text-align: center;" else ""))
                  ),
                  class = "btn btn-link category-btn",
                  style = "width: 100%; height: 100%; border: none; background: transparent; text-decoration: none;"
                )
              )
            })
          )
        )
      )
    } else {
      cat("Switching to category:", values$current_category, "\n")
      switch(values$current_category,
             "culture" = culture_dashboard_ui(values$current_category),
             "agriculture_marine" = agriculture_dashboard_ui(values$current_category),
             "transport" = transport_dashboard_ui(values$current_category),
             "housing" = housing_dashboard_ui(values$current_category),
             "population_skills" = population_dashboard_ui(values$current_category),
             "environment_climate" = {
               cat("Rendering environment_dashboard_ui\n")
               environment_dashboard_ui(values$current_category)
             },
             "economy_digital" = economy_dashboard_ui(values$current_category),
             "health_social" = health_dashboard_ui(values$current_category),
             "social_justice" = social_justice_dashboard_ui(values$current_category),
             fluidRow(
               box(
                 title = "Policy Area Not Found",
                 status = "warning",
                 solidHeader = TRUE,
                 width = 12,
                 p("The selected policy area is not available.")
               )
             )
      )
    }
  })
  
  # Handle category button clicks (from categories page) - UPDATED WITH STATE RESET
  lapply(names(CATEGORIES), function(cat_id) {
    observeEvent(input[[paste0("cat_", cat_id)]], {
      # Reset all module states before switching
      reset_module_states(session)
      
      # Small delay to ensure resets are processed
      later::later(function() {
        values$current_category <- cat_id
        values$navigation_source <- "categories"
      }, delay = 0.1)
    })
  })
  
  # Handle metric link clicks - UPDATED WITH STATE RESET
  lapply(names(COMPLETE_METRICS), function(cat_id) {
    cat_info <- COMPLETE_METRICS[[cat_id]]
    lapply(names(cat_info$metrics), function(metric_key) {
      metric_id <- paste0("metric_link_", gsub("[^A-Za-z0-9]", "_", paste(cat_id, metric_key, sep = "_")))
      
      observeEvent(input[[metric_id]], {
        # Reset all module states before switching
        reset_module_states(session)
        
        # Navigate to the category first
        values$current_category <- cat_id
        values$navigation_source <- "metrics_list"
        updateTabItems(session, "sidebar", selected = "categories")
        
        # Set the metric after a delay
        later::later(function() {
          if(cat_id == "culture") {
            updateSelectInput(session, "culture_metric", selected = metric_key)
          } else if(cat_id == "agriculture_marine") {
            updateSelectInput(session, "agriculture_metric", selected = metric_key)
          } else if(cat_id == "transport") {
            updateSelectInput(session, "transport_metric", selected = metric_key)
          } else if(cat_id == "housing") {
            updateSelectInput(session, "housing_metric", selected = metric_key)
          } else if(cat_id == "economy_digital") {
            updateSelectInput(session, "economy_metric", selected = metric_key)
          } else if(cat_id == "population_skills") {
            updateSelectInput(session, "population_metric", selected = metric_key)
          } else if(cat_id == "social_justice") {
            updateSelectInput(session, "social_justice_metric", selected = metric_key)
          } else if(cat_id == "health_social") {
            updateSelectInput(session, "health_metric", selected = metric_key)
          } else if(cat_id == "environment_climate") {
            updateSelectInput(session, "environment_metric", selected = metric_key)
          }
        }, delay = 1.2)  # Increased delay for metric selection
      })
    })
  })
  
  
  
  # Reactive control for display (could be tied to a feature flag or user role)
  show_banner <- reactiveVal(TRUE)
  
  # Render the banner when enabled
  output$dev_banner <- renderUI({
    if (!isTRUE(show_banner())) {
      session$sendCustomMessage('devBannerVisible', FALSE)
      return(NULL)
    }
    # Notify body to add a top padding class
    session$sendCustomMessage('devBannerVisible', TRUE)
    
    div(
      id = 'dev-banner',
      role = 'status',
      tags$strong('This is a beta version. Content is in development.'),
      HTML(paste('Last updated', last_update)),
      # Dismiss button (session-only)
      tags$a(
        href = '#', class = 'close', `aria-label` = 'Close',
        onclick = "Shiny.setInputValue('dismiss_dev_banner', Date.now()); return false;",
        HTML('&times;')
      )
    )
  })
  
  # Handle dismissal
  observeEvent(input$dismiss_dev_banner, {
    show_banner(FALSE)
    session$sendCustomMessage('devBannerVisible', FALSE)
  })

  
  
  # Module server functions
}

# Run the application
shinyApp(ui = ui, server = server)
=======
# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(htmltools)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(readxl)
library(stringr)
library(tidyr)
library(scales)
library(openxlsx)
library(later)

# Load configuration
source("modules/config.R")
source("modules/culture_module.R")
source("modules/economy_digital_module.R")
source("modules/housing_module.R")
source("modules/transport_module.R")
source("modules/social_justice_module.R")
source("modules/classification_module.R")
source("modules/agriculture_module.R")
source("modules/population_module.R")
source("modules/health_module.R")
source("modules/environment_module.R")

# Load all module files
# Source all category modules


COMPLETE_METRICS <- list(
 
  "agriculture_marine" = list(
    name = "Agriculture and Marine",
    icon = "tractor", 
    data_available = TRUE,
    metrics = list(
      "Agri-Environment Schemes" = "Area of land covered by higher-level or targeted agri-environment schemes in England, Wales, Scotland and Northern Ireland",
      "Sustainably Managed Woodland" = "Percentage of UK woodland area certified as sustainably managed by country", 
      "GVA (Agri, Forestry, Fishing)" = "Regional Gross Value Added (GVA) (balanced) in pounds, million by Agriculture, Forestry and Fishing sectors",
      "Employment (Agri, Forestry, Fishing)" = "Percentage (%) Employment by sector: Agriculture, Forestry and Fishing sector",
      "Fish Stocks" = "Estimated % of commercial stocks fished at sustainable levels",
      "Greenhouse Gas Emissions" = "Greenhouse Gas Emissions from Agriculture and LULUCF",
      "New Planting" = "New woodland creation",
      "Farm Business Income" = "Average Farm business income by farm type",
      "Diversified Activity And Incomes" = "Average diversified income (% of farm business income - FBI)  and amount of diversified income of farms with diversified activity",
      "New To Crofting" = "New entrants to crofting",
      "Resident Crofters" = "Percentage of crofts with resident crofters",
      "Agritourism Value" = "Value of agritourism"
    )
  ),
  "economy_digital" = list(
    name = "Economy and Digital",
    icon = "chart-line",
    data_available = TRUE,
    metrics = list(
      "Regional Gross Value Added" = "Regional Gross Value Added (£ millions)",
      "Number of VAT/PAYE Businesses" = "Number of VAT/PAYE Businesses",
      "High growth private businesses" = "Number of high growth registered private sector businesses",
      "Economic inactivity" = "Economic inactivity rates for population aged 16 to 64",
      "Broadband coverage" = "Residential broadband coverage by service type/download speeds",
      "4G coverage" = "Percentage 4G geographic coverage for at least one Mobile Network Operator (MNO)"
    )
  ),
  "population_skills" = list(
    name = "Population, Education and Skills",
    icon = "graduation-cap",
    data_available = TRUE,
    metrics = list(
      "Median age" = "Median age by urban rural classification",
      "Population change (count)" = "Population change (count) by urban rural classification",
      "Population change (%)" = "Annual percentage change in population by urban rural classification",
      "16-19 yo in education, employment, training" = "The proportion of 16-19 year olds in Scotland participating in education, training or employment",
      "Skill shortage vacancies" = "Incidence of skill shortage vacancies (employer base)",
      "Positive destinations" = "Percentage of school leavers in positive destinations at 9-month follow-up",
      "Primary School Literacy" = "Percentage of primary school pupils achieving expected CfE Levels in Literacy",
      "Primary School Numeracy" = "Percentage of primary school pupils achieving expected CfE Levels in Numeracy",
      "Secondary School Literacy" = "Percentage of secondary school pupils achieving expected CfE Levels in Literacy",
      "Secondary School Numeracy" = "Percentage of secondary school pupils achieving expected CfE Levels in Numeracy"
    )
  ),
  "housing" = list(
    name = "Housing",
    icon = "home",
    data_available = TRUE,
    metrics = list(
      "Median Property Price" = "Median Property Price",
      "Second Homes" = "Proportion of all dwellings that are second homes",
      "Vacant Homes" = "Proportion of all dwellings that are vacant homes",
      "New Build Completions" = "New Build Completions",
      "AHSP Completions" = "AHSP Completions",
      "Housing Conditions" = "Dwellings with urgent disrepair to critical elements",
      "EPC rating" = "Proportion of households rated EPC C or above"
    )
  ),
  "health_social" = list(
    name = "Health and Social Care",
    icon = "heartbeat",
    data_available = TRUE,
    metrics = list(
      "Quality of care" = "Quality of care experience",
      "Self-assessed health" = "Self-assessed general health",
      "Healthy Life Expectancy (males)" = "Healthy Life Expectancy (males)",
      "Healthy Life Expectancy (females)" = "Healthy Life Expectancy (females)",
      "Mental Well-being" = "Warwick-Edinburgh Mental Well-being Scale (WEMWBS) score",
      "GP access" = "How easy it is for people to contact their General Practice in the way they want",
      "Care impact on quality of life" = "Help, care or support improved or maintained quality of life",
      "Access to nature" = "Proportion (%) of adults living within 5 minutes' walk of their nearest green or blue space",
      "Out-of-hours healthcare" = "Experience of out of hours healthcare",
      "Help with Everyday Living" = "Rating of Care, Support and Help with Everyday Living",
      "Care-Life Balance" = "Balancing caring responsibilities and other aspects of life",
      "Support to Continue Caring" = "Feeling supported to continue caring"
    )
  ),
  
  "environment_climate" = list(
    name = "Environment and Climate Change",
    icon = "leaf",
    data_available = TRUE,
    metrics = list(
      "Percentage of household waste recycled" = "Percentage (%) of household waste recycled",
      "Renewable Electricity" = "Renewable Electricity Generation (MWh per household)",
      "Clean Seas" = "Clean Seas: the percentage of biogeographic regions with acceptably low levels of contaminants",
      "Biodiversity: Scottish species" = "Biodiversity: combination of three indices of Scottish species - abundance of marine species, abundance of terrestrial species and occupancy of terrestrial species",
      "Fresh water" = "Fresh Water condition: The percentage of river and loch waterbodies achieving 'Good' or better status in terms of four metrics; water quality, water resources (flows and levels), access to fish migration and physical condition",
      "Restored peatland" = "Hectares of restored peatland: Cumulative area of peatland restored since 2012 (thousands hectares)",
      "Biodiversity awareness" = "Awareness, understanding and support for biodiversity conservation",
      "Rare and threatened species" = "Status of rare and threatened species (UK Red List Index)",
      "Forests and woodlands" = "Woodland area by forest type and ownership (thousand hectares)"
    )
  ),
  "transport" = list(
    name = "Transport",
    icon = "route",
    data_available = TRUE,
    metrics = list(
      "Public transport satisfaction" = "Satisfaction with the quality of public transport",
      "Cycle Training" = "Proportion of Primary Schools Delivering On-Road Cycle Training",
      "Road Condition" = "The Percentage Of Roads Needing Repairs (Red And Amber Classification) In Scotland",
      "EV Infrastructure" = "Publicly available electric vehicle charging devices at all speeds by local authority per 100,000 population",
      "Weekly travel costs" = "Weekly travel costs in different Minimum Income Standard (MIS) budgets",
      "Key service access" = "Percentage of population within 15 minute drive time by public transport of key service",
      "Transport affordability" = "How easy or difficult people find it to afford transport costs",
      "Mode of transport (work)" = "How adults usually travel to work (percentages)",
      "Ferry reliability" = "Percentage of Scottish lifeline ferry services that are reliable and punctual by operator"
    )
  ),

  "culture" = list(
    name = "Culture and Heritage",
    icon = "theater-masks",
    data_available = TRUE,
    metrics = list(
      "Attendance at cultural events and visiting places of culture" = "Attendance at cultural events and visiting places of culture",
      "Participation in any cultural activity" = "Participation in any cultural activity",
      "Gaelic speakers" = "Population speaking Gaelic",
      "Visits to attractions" = "Number of visits to Scotland's visitor attractions"
    )
  ),
  
  "social_justice" = list(
    name = "Social Justice",
    icon = "balance-scale",
    data_available = TRUE,
    metrics = list(
      "Relative poverty" = "Proportion (%) of people who are in relative poverty",
      "Relative child poverty" = "Proportion (%) of children who are in relative poverty",
      "Housing affordability" = "Housing costs as percentage of earnings",
      "Influence over local decisions" = "Perceptions of influence over decisions affecting local area",
      "Managing financially" = "How the household is managing financially",
      "Community ownership" = "Number of assets in community ownership"
    )
  )

)

# Function to reset module states
reset_module_states <- function(session) {
  # Reset all module-specific inputs to their default states
  updateSelectInput(session, "environment_metric", selected = "")
  updateSelectInput(session, "environment_classification_type", selected = NULL)
  updateSelectInput(session, "culture_metric", selected = "")
  updateSelectInput(session, "culture_classification_type", selected = NULL)
  updateSelectInput(session, "transport_metric", selected = "")
  updateSelectInput(session, "transport_classification_type", selected = NULL)
  updateSelectInput(session, "housing_metric", selected = "")
  updateSelectInput(session, "housing_classification_type", selected = NULL)
  updateSelectInput(session, "social_justice_metric", selected = "")
  updateSelectInput(session, "social_justice_classification_type", selected = NULL)
  updateSelectInput(session, "agriculture_metric", selected = "")
  updateSelectInput(session, "agriculture_classification_type", selected = NULL)
  updateSelectInput(session, "population_metric", selected = "")
  updateSelectInput(session, "population_classification_type", selected = NULL)
  updateSelectInput(session, "health_metric", selected = "")
  updateSelectInput(session, "health_classification_type", selected = NULL)
  updateSelectInput(session, "economy_metric", selected = "")
  updateSelectInput(session, "economy_classification_type", selected = NULL)
  
  # Reset sub-metric dropdowns
  updateSelectInput(session, "selected_attendance_activity", selected = NULL)
  updateSelectInput(session, "selected_participation_activity", selected = NULL)
  updateSelectInput(session, "selected_woodlands_activity", selected = NULL)
}
# Main UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Rural Scotland Data Dashboard",
    tags$li(class = "dropdown", id = "sidebar-toggle-header", style = "display: none;")
  ),

  dashboardSidebar(
    sidebarMenu(
      id = 'sidebar',
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Policy Areas", tabName = "categories", icon = icon("th-large")),
      menuItem("List of Policy Metrics", tabName = "metrics_list", icon = icon("list")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    div(
      class = "sidebar-logos",
      img(src = "SG_logo.png", alt = "Scottish Government Logo"),
      img(src = "RESAS_logo.png", alt = "RESAS Logo")
    )
 

  ),
 
  
  dashboardBody(
    tags$head(
      tags$style(HTML(paste0(COMMON_CSS, "
        /* CHANGE: Keep header visible permanently and make sidebar sticky */
        .main-header {
          display: none !important;        
        }
        /* CHANGE: Make sidebar sticky/scrollable */
        .main-sidebar {
          position: fixed !important;
          top: 50px !important;
          bottom: 0 !important;
          left: 0 !important;
          overflow-y: auto !important;
          z-index: 1050 !important;
        }
        
        /* CHANGE: Remove gap at top of sidebar */
        .main-sidebar .sidebar {
          padding-top: 0 !important;
          margin-top: 0 !important;
        }
        
        .main-sidebar .sidebar-menu {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }
        /* SIDEBAR HEADER WITH GREEN BACKGROUND */
        .main-sidebar::before {
          content: 'Rural Scotland Data Dashboard';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          background: #0E450B !important;
          color: white !important;
          padding: 15px 20px;
          font-weight: 600;
          font-size: 14px;
          text-align: center;
          z-index: 1000;
          border-bottom: 1px solid rgba(255,255,255,0.1);
          display: block !important;
        }
        
        /* ADJUST SIDEBAR MENU TO ACCOUNT FOR HEADER */
        .main-sidebar {
          position: fixed !important;
          top: 0 !important;
          bottom: 0 !important;
          left: 0 !important;
          overflow-y: auto !important;
          z-index: 1050 !important;
        }
        
        .main-sidebar .sidebar-menu {
          margin-top: 0 !important;
          padding-top: 0 !important;
        }
        /*  Completely remove gap - target all possible sources */
        .content-wrapper {
          margin-top: 0 !important;
          padding: 0 !important;
          margin-left: 230px !important;
        }
               /* SIDEBAR LOGOS - AT BOTTOM */
        .sidebar-logos {
          position: fixed !important;
          bottom: 0 !important;
          left: 0 !important;
          width: 230px !important;
          background: #222d32 !important;
          z-index: 1000 !important;
          border-top: 1px solid rgba(255,255,255,0.1) !important;
          padding: 0 !important;
          margin: 0 !important;
        }

        .sidebar-logos img {
          width: 100% !important;
          height: auto !important;
          display: block !important;
          background: white !important;
          margin: 0 !important;
          border-radius: 0 !important;
          box-shadow: none !important;
        }
             
                /* Adjust sidebar menu to account for logos */
        .main-sidebar .sidebar-menu {
          padding-bottom: 120px !important; /* Adjust based on logo heights */
        }
        
        /* When sidebar is collapsed, hide logos */
        .sidebar-collapse .sidebar-logos {
          display: none !important;
        }

        /* CONDITIONAL CONTENT PADDING - ONLY WHEN CATEGORY HEADER IS PRESENT */
        
        /* Default state - no extra padding */
        .content-wrapper .content {
          padding: 0 !important;
          margin: 0 !important;
        }
        
        /* Only add padding when category header is present */
        .category-header + .row {
          margin-top: 200px !important;
        }
        
        /* More specific: only add padding to content that comes after category header */
        .category-header ~ * {
          margin-top: 200px !important;
        }
        
        /* Ensure home, categories selection, and metrics list pages have no extra padding */
        #home .content,
        #categories .content:not(.category-view),
        #metrics_list .content,
        #about .content {
          padding-top: 0 !important;
          margin-top: 0 !important;
        }

        /* Override any inherited padding for non-category pages */
        .content-wrapper:not(.category-view) .content {
          padding-top: 0 !important;
        }
        
        /* Specific reset for tab content */
        .tab-content .tab-pane {
          padding-top: 0 !important;
        }        
        /* Target AdminLTE specific classes */
        .skin-blue .content-wrapper,
        .skin-blue .right-side {
          background-color: #ecf0f5 !important;
        }
        
        /* Remove any box margins at the top */
        .content > .row:first-child {
          margin-top: 0 !important;
        }
        
        .content > .row:first-child > .col-sm-12:first-child > .box:first-child {
          margin-top: 0 !important;
        }
        
        /* Adjust for collapsed sidebar */
        .sidebar-collapse .content-wrapper {
          margin-left: 0 !important;
        }
        
        /* Orange buttons for main page */
        #explore_policy_areas,
        #explore_policy_metrics {
          background-color: #FDBE41 !important;
          color: black !important;
          border-color: #FDBE41 !important;
        }
        
        #explore_policy_areas:hover,
        #explore_policy_metrics:hover {
          background-color: #E6A835 !important;
          color: black !important;
          border-color: #E6A835 !important;
        }
        
        /* Hide dashboard header when in category view */
        .category-view .main-header,
        .main-header {
          display: none !important;
        }
        
        /*  Category header base styles - SHORTENED from 180px to 120px */
        .category-header {
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          min-height: 120px;
          border-radius: 8px;
          overflow: visible;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          margin: 15px;
          transition: all 0.3s ease;
          position: relative;
          z-index: 100;
        }
        
        /* Sticky state */
        .category-header.sticky {
          position: fixed !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          width: 100vw !important;
          margin: 0 !important;
          border-radius: 0 !important;
          z-index: 1050 !important;
          box-shadow: 0 2px 15px rgba(0,0,0,0.3) !important;
        }
        
        /* Account for sidebar when sticky */
        .category-header.sticky {
          margin-left: 230px !important;
          width: calc(100vw - 230px) !important;
        }
        
        /* When sidebar is collapsed */
        .sidebar-collapse .category-header.sticky {
          margin-left: 0 !important;
          width: 100vw !important;
        }
        
        /* Compact state when scrolled - SHORTENED from 80px to 60px */
        .category-header.compact {
          min-height: 60px !important;
        }
        
        .category-header.compact .category-header-overlay {
          min-height: 60px !important;
          padding: 15px 25px !important;
          flex-direction: row !important;
          justify-content: space-between !important;
          align-items: center !important;
        }
        
        .category-header.compact h2 {
          font-size: 1.8em !important;
          margin: 0 !important;
        }
        
        .category-header.compact .custom-sidebar-toggle {
          position: absolute !important;
          top: 2px !important;
          left: 2px !important;
          transform: translateY(-50%) !important;
        }
        
        /*  Placeholder to maintain layout when header becomes fixed - SHORTENED */
        .header-placeholder {
          height: 0;
          margin: 15px;
          display: none;
          transition: height 0.3s ease;
        }
        
        .header-placeholder.active {
          display: block;
          height: 120px;
        }
        
        /* Header overlay - SHORTENED */
        .category-header-overlay {
          background: linear-gradient(135deg, rgba(0,0,0,0.7), rgba(0,0,0,0.4));
          position: relative;
          padding: 25px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          color: white;
          border-radius: 8px;
          min-height: 120px;
          transition: all 0.3s ease;
        }
        
        /*  ALL DROPDOWN BUTTONS - NON-TRANSPARENT WHITE WITH BLACK TEXT */
        .selectize-control.single .selectize-input,
        .selectize-control.single .selectize-input.focus,
        .form-control,
        select.form-control {
          background: white !important;
          color: black !important;
          border: 1px solid #ccc !important;
          opacity: 1 !important;
        }
        
        .selectize-dropdown {
          background: white !important;
          border: 1px solid #ccc !important;
          color: black !important;
        }
        
        .selectize-dropdown .option {
          color: black !important;
          background: white !important;
        }
        
        .selectize-dropdown .option.active,
        .selectize-dropdown .option:hover {
          background: #f0f0f0 !important;
          color: black !important;
        }
        
        /* : UNIVERSAL GREEN COLOUR WITH BLACK TEXT for charts, data table, key insights */
        .box-primary > .box-header,
        .box-info > .box-header,
        .box-success > .box-header {
          background: #0E450B !important;
          color: white !important;
          border-bottom: 1px solid #0E450B !important;
        }
        
        .box-primary > .box-header .box-title,
        .box-info > .box-header .box-title,
        .box-success > .box-header .box-title,
        .box-header h3,
        .box-header h4 {
          color: white !important;
          font-weight: 600 !important;
        }
        
        /*  Data table styling  */
        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          color: black !important;
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #0E450B !important;
          border-color: #0E450B !important;
          color: white !important;
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: #0E450B !important;
          border-color: #0E450B !important;
          color: white !important;
        }
        
        /* CHART DOWNLOAD BUTTONS -  */
        .plotly .modebar {
          opacity: 1 !important;
          visibility: visible !important;
          position: relative !important;
          right: auto !important;
          top: auto !important;
        }
        
        .plotly .modebar-container {
          opacity: 1 !important;
          visibility: visible !important;
        }
        
        .plotly .modebar-group {
          background: white !important;
          border: 1px solid #ccc !important;
          border-radius: 4px !important;
        }
        
        .plotly .modebar-btn {
          color: #666 !important;
          background: white !important;
        }
        
        .plotly .modebar-btn:hover {
          background: #0E450B !important;
          color: white !important;
        }
        /* Force category headers to be edge-to-edge at very top */
        .category-header,
        .category-header.sticky,
        .category-header.compact,
        .category-header.header-processed {
          position: fixed !important;
          top: 0 !important;              /* ← CHANGED: From 50px to 0 */
          left: 230px !important;         /* ← CHANGED: From 245px to 230px (sidebar width) */
          right: 0 !important;            /* ← CHANGED: From 15px to 0 */
          z-index: 1040 !important;
          margin: 0 !important;
          border-radius: 0 !important;
          min-height: 180px !important;
          max-height: 180px !important;
          width: calc(100vw - 230px) !important;  /* ← CHANGED: Full viewport width minus sidebar */
        }
        
        /* Account for collapsed sidebar */
        .sidebar-collapse .category-header,
        .sidebar-collapse .category-header.sticky,
        .sidebar-collapse .category-header.compact {
          left: 0 !important;             /* ← CHANGED: From 15px to 0 */
          width: 100vw !important;        /* ← CHANGED: Full viewport width */
        }        
        /* Add spacing to prevent content overlap */
        .category-header + * {
          margin-top: 200px !important;
        }
        
        /* Disable all placeholder behavior */
        .header-placeholder,
        .header-placeholder.active {
          display: none !important;
          height: 0 !important;
        }
        /*  Excel download button styling - always visible */
        .excel-download-btn {
          background: white !important;
          color: black !important;
          border: 1px solid #ccc !important;
          border-radius: 4px !important;
          padding: 8px 12px !important;
          margin: 10px 0 !important;
          display: inline-block !important;
          text-decoration: none !important;
          font-size: 14px !important;
        }
        
        .excel-download-btn:hover {
          background: #0E450B !important;
          color: white !important;
          border-color: #0E450B !important;
          text-decoration: none !important;
        }
        
        .excel-download-btn i {
          margin-right: 5px !important;
        }
        
        /* CHANGE 9: DISABLE WARNINGS CSS */
        .shiny-notification-warning,
        .shiny-notification-error {
          display: none !important;
        }
        
        /*  SIDEBAR TOGGLE BUTTON - TOP LEFT OF CATEGORY HEADER */
        .custom-sidebar-toggle {
          background: rgba(255, 255, 255, 0.9) !important;
          border: none !important;
          border-radius: 4px !important;
          padding: 0 !important;
          color: black !important;
          font-size: 14px !important;
          transition: all 0.3s ease !important;
          cursor: pointer !important;
          position: absolute !important;
          left: 0px !important;
          top: 0px !important;
          z-index: 2001 !important;
          width: 32px !important;
          height: 32px !important;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          box-shadow: 0 2px 4px rgba(0,0,0,0.2) !important;
        }
        
        .custom-sidebar-toggle:hover {
          background: rgba(255, 255, 255, 1) !important;
          transform: scale(1.05) !important;
          color: black !important;
          box-shadow: 0 4px 8px rgba(0,0,0,0.3) !important;
        }
        
        .custom-sidebar-toggle:focus {
          outline: none !important;
          box-shadow: 0 0 0 3px rgba(0,0,0,0.2) !important;
        }
        
        /* CREATE CUSTOM HAMBURGER LINES  */
        .custom-sidebar-toggle::before {
          content: '' !important;
          position: absolute !important;
          width: 18px;
          height: 2px;
          background: black;
          box-shadow: 
            0 -6px 0 black,
            0 6px 0 black;
          display: block;
        }
        
        /* HIDE ANY FA ICON IF PRESENT */
        .custom-sidebar-toggle i {
          display: none !important;
        }        
        /* ENSURE CATEGORY HEADER HAS RELATIVE POSITIONING FOR ABSOLUTE CHILD */
        .category-header {
          position: relative !important;
        }
        
        .category-header-overlay {
          position: relative !important;
        }        
          /* Hide default dashboard header toggle */
        .main-header .navbar-toggle {
          display: none !important;
        }
        
        /*  Reduce gaps between policy area tiles */
        .category-card {
          background: #f8f9fa;
          border: 1px solid #dee2e6;
          border-radius: 8px;
          padding: 5px;
          margin: 5px; /* CHANGED: Reduced from 10px to 5px */
          cursor: pointer;
          transition: all 0.3s ease;
          text-align: center;
        }
        .category-card:hover {
          background: #e9ecef;
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        .category-btn {
          width: 100% !important;
          height: 100% !important;
          border: none !important;
          background: transparent !important;
          box-shadow: none !important;
          padding: 15px !important;
        }
        .category-btn:hover {
          background: transparent !important;
        }
        .featured-category {
          border: 2px solid #007bff !important;
          background: #f0f8ff !important;
        }
        .coming-soon-category {
          opacity: 0.6;
          border: 2px dashed #ccc !important;
        }
        
        /*  Center text for specific category tiles */
        .category-card[data-category='population_skills'] h4,
        .category-card[data-category='environment_climate'] h4 {
          text-align: center !important;
        }
        
        .hero-section {
          box-shadow: 0 8px 16px rgba(0,0,0,0.2);
        }
        .hero-section h1 {
          animation: fadeInUp 1s ease-out;
        }
        .hero-section h3 {
          animation: fadeInUp 1s ease-out 0.3s both;
        }
        .hero-section .btn {
          animation: fadeInUp 1s ease-out 0.6s both;
          transition: all 0.3s ease;
        }
        .hero-section .btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.3);
        }
        .classification-link:hover {
          color: #23527c !important;
          text-decoration: underline !important;
        }
        .metrics-card:hover {
          background-color: #e3f2fd !important;
          border-color: #007bff !important;
          transform: translateX(5px);
          box-shadow: 0 4px 12px rgba(0,123,255,0.15) !important;
        }
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        .content-section {
          margin-top: 20px;
        }
        .box {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
        .classification-tabs .nav-tabs {
          margin-bottom: 20px;
        }
        .classification-tabs .nav-tabs > li > a {
          font-size: 14px;
          padding: 8px 12px;
        }
      ")))
    ),
    
    # JavaScript to disable warnings
    tags$script(HTML("
      // DISABLE SHINY WARNINGS
      $(document).ready(function() {
        // Hide all existing warnings
        $('.shiny-notification-warning, .shiny-notification-error').hide();
        
        // Monitor for new warnings and hide them
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            mutation.addedNodes.forEach(function(node) {
              if (node.nodeType === 1) {
                var warnings = $(node).find('.shiny-notification-warning, .shiny-notification-error');
                if (warnings.length > 0) {
                  warnings.hide();
                }
                if ($(node).hasClass('shiny-notification-warning') || $(node).hasClass('shiny-notification-error')) {
                  $(node).hide();
                }
              }
            });
          });
        });
        
        observer.observe(document.body, {
          childList: true,
          subtree: true
        });
        
        console.log('Warning suppression activated');
      });
    ")),
    
    tags$script(HTML("
      $(document).ready(function() {
        console.log('Improved sticky header script loading...');
        
        var stickyHeader = {
          init: function() {
            this.bindEvents();
            this.setupHeaders();
          },
          
          bindEvents: function() {
            var self = this;
            
            $(document).on('click', '#custom-sidebar-toggle', function(e) {
              e.preventDefault();
              e.stopPropagation();
              console.log('Custom sidebar toggle clicked');
              $('body').toggleClass('sidebar-collapse');
              
              setTimeout(function() {
                $(window).trigger('resize');
              }, 350);
            });
            
            $(document).on('shiny:value shiny:inputchanged', function(event) {
              setTimeout(function() {
                self.setupHeaders();
              }, 300);
            });
          },
          
          setupHeaders: function() {
            var self = this;
            $('.category-header').each(function() {
              var $header = $(this);
              
              if (!$header.hasClass('header-processed')) {
                console.log('Setting up header...');
                $header.addClass('header-processed');
                self.addSidebarToggle($header);
              }
            });
          },
          
          addSidebarToggle: function($header) {
            var $overlay = $header.find('.category-header-overlay');
            if ($overlay.length && !$overlay.find('.custom-sidebar-toggle').length) {
              var toggleBtn = $('<button class=custom-sidebar-toggle id=custom-sidebar-toggle><i class=fa fa-bars></i></button>');
              $overlay.append(toggleBtn);
            }
          }
        };
        
        stickyHeader.init();
        console.log('Improved sticky header script loaded successfully');
      });
    ")),
    tabItems(
      tabItem(tabName = "home",
              # Hero section with background image
              fluidRow(
                div(
                  class = "hero-section",
                  style = "background-image: url('home_page_img.jpg');
                           background-size: cover;
                           background-position: center;
                           background-repeat: no-repeat;
                           position: relative;
                           min-height: 400px;
                           border-radius: 8px;
                           margin-bottom: 30px;
                           overflow: hidden;",
                  div(
                    style = "background: linear-gradient(135deg, rgba(0,0,0,0.6), rgba(0,0,0,0.3));
                             position: absolute;
                             top: 0; left: 0; right: 0; bottom: 0;
                             padding: 40px;
                             display: flex;
                             flex-direction: column;
                             justify-content: center;
                             align-items: center;
                             color: white;
                             text-align: center;",
                    h1("Rural Scotland Data Dashboard", 
                       style = "font-size: 3.5em; margin-bottom: 20px; text-shadow: 2px 2px 4px rgba(0,0,0,0.7); font-weight: bold;"),
                    h3("Supporting the Rural Delivery Plan",
                       style = "margin-bottom: 30px; text-shadow: 1px 1px 2px rgba(0,0,0,0.7);"),
                    # CHANGE: Replace single button with two side-by-side buttons
                    div(
                      style = "display: flex; gap: 20px; justify-content: center; flex-wrap: wrap; margin-top: 20px;",
                      actionButton("explore_policy_areas", "Explore Key Policy Areas", 
                                   class = "btn btn-primary btn-lg",
                                   icon = icon("th-large"),
                                   style = "padding: 15px 30px; font-size: 1.2em;"),
                      actionButton("explore_policy_metrics", "Explore Key Policy Metrics", 
                                   class = "btn btn-success btn-lg",
                                   icon = icon("list"),
                                   style = "padding: 15px 30px; font-size: 1.2em;")
                    )
                  )
                )
              ),
              
              # Main content section 
              fluidRow(
                column(12,
                       box(
                         title = "About the Dashboard", 
                         status = "primary", 
                         solidHeader = TRUE,
                         width = 12,
                         
                         p("This dashboard integrates most recent data from a wide range of Scottish Government data sources. It is designed to compliment the Rural Delivery Plan, bringing together a range of data to illustrate the experience of rural communities and the impact of government action over time."),
                         p("Data is organized across 9 key policy areas affecting rural Scotland."),
                         
                         h4("Dashboard Categories:"),
                         
                         tags$ul(
                           lapply(names(CATEGORIES), function(cat_id) {
                             cat_info <- CATEGORIES[[cat_id]]
                             tags$li(
                               actionLink(paste0("cat_link_", cat_id), cat_info$name, 
                                          style = "color: #007bff; font-weight: bold; text-decoration: none;")
                             )
                           })
                         )
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       box(
                         title = "About Rural Scotland", 
                         status = "info", 
                         solidHeader = TRUE,
                         width = 12,
                         div(
                           style = "line-height: 1.6;",
                           p(strong("Rural areas constitute 98% of Scotland's landmass and 17% of its population. The Scottish Government core definition of rurality classifies areas with a population of fewer than 3,000 people to be rural."),
                             style = "font-size: 1.1em; margin-bottom: 20px;"),
                           
                           h4("Presenting data on rural Scotland", style = "color: #337ab7; margin-bottom: 15px;"),
                           
                           p(strong("This dashboard presents data at several different classification levels. Click on any classification below to learn more:"), 
                             style = "margin-bottom: 20px;"),
                           
                           # Classification tabs within the panel
                           div(
                             class = "classification-tabs",
                             tabsetPanel(
                               id = "classification_tabs",
                               type = "tabs",
                               tabPanel("2-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 2-fold"),
                                                p("The Scottish Government core definition of rurality classifies areas with a population of fewer than 3,000 people to be rural. The Scottish Government Urban Rural Classification can be collapsed to this core definition, to create a 2-fold classification:"),
                                                tags$ol(
                                                  tags$li(strong("Rest of Scotland:"), "(1) Large Urban Areas, (2) Other Urban Areas, (3) Accessible Small Towns, and (4) Remote Small Towns."),
                                                  tags$li(strong("Rural Scotland:"), "(5) Accessible Rural and (6) Remote Rural Areas.")
                                                ),
                                                div(class = "alert alert-info", style = "margin-top: 15px;",
                                                    p(strong("Note:"), "On slides where a further breakdown of the data is not possible (for example, because the sample is too small), figures are presented in two categories: 'Rural' and 'Urban'. In this classification, the 'Rural' category includes accessible and remote rural areas.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "2_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "2-fold Classification Map")
                                            )
                                        )
                               ),
                               tabPanel("3-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 3-fold"),
                                                p("The 3-fold classification simplifies the rural categories:"),
                                                tags$ol(
                                                  tags$li(strong("Rest of Scotland:"), "All urban areas including large urban areas, other urban areas, and small towns."),
                                                  tags$li(strong("Accessible Rural:"), "Rural areas within 30 minutes drive time of settlements of 10,000 or more people."),
                                                  tags$li(strong("Remote Rural:"), "Rural areas with drive time of over 30 minutes to settlements of 10,000 or more people.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "3_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "3-fold Classification Map")
                                            )
                                        )
                               ),
                               tabPanel("6-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 6-fold"),
                                                p("The 6-fold classification provides the most detailed breakdown of urban and rural areas in Scotland:"),
                                                tags$ol(
                                                  tags$li(strong("Large Urban Areas:"), "Settlements of 125,000 people and over."),
                                                  tags$li(strong("Other Urban Areas:"), "Settlements of 10,000 to 124,999 people."),
                                                  tags$li(strong("Accessible Small Towns:"), "Settlements of 3,000 to 9,999 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Small Towns:"), "Settlements of 3,000 to 9,999 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Accessible Rural Areas:"), "Areas with a population of less than 3,000 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Rural Areas:"), "Areas with a population of less than 3,000 people, and with a drive time of over 30 minutes but less than or equal to 60 minutes to a Settlement of 10,000 or more.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "6_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "6-fold Classification Map")
                                            )
                                            
                                        )
                               ),
                               tabPanel("8-fold Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Scottish Government Urban Rural Classification, 8-fold"),
                                                p("The 8-fold classification provides the most detailed breakdown including very remote categories:"),
                                                tags$ol(
                                                  tags$li(strong("Large Urban Areas:"), "Settlements of 125,000 people and over."),
                                                  tags$li(strong("Other Urban Areas:"), "Settlements of 10,000 to 124,999 people."),
                                                  tags$li(strong("Accessible Small Towns:"), "Settlements of 3,000 to 9,999 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Small Towns:"), "Settlements of 3,000 to 9,999 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Very Remote Small Towns:"), "Settlements of 3,000 to 9,999 people, and with a drive time of over 60 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Accessible Rural Areas:"), "Areas with a population of less than 3,000 people, and within a 30 minute drive time of a Settlement of 10,000 or more."),
                                                  tags$li(strong("Remote Rural Areas:"), "Areas with a population of less than 3,000 people, and with a drive time of over 30 minutes but less than or equal to 60 minutes to a Settlement of 10,000 or more."),
                                                  tags$li(strong("Very Remote Rural Areas:"), "Areas with a population of less than 3,000 people, and with a drive time of over 60 minutes to a Settlement of 10,000 or more.")
                                                )
                                            ),
                                            # Image (right side)
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "8_fold_image.jpg", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "8-fold Classification Map")
                                            )
                                        )
                               ),
                               tabPanel("RESAS Classification",
                                        div(style = "margin-top: 20px; display: flex; gap: 20px;",
                                            # Text content (left side)
                                            div(style = "flex: 1;",
                                                h4("Rural & Environmental Science and Analytical Services (RESAS) Classification"),
                                                p("This classification classifies local authorities according to their level of rurality and establishes four different groups as well as a broader urban-rural grouping:"),
                                                
                                                h5("Rural Group:"),
                                                tags$ol(
                                                  tags$li(strong("Islands and Remote Rural:"), "Local authorities with significant island populations or very remote mainland areas."),
                                                  tags$li(strong("Mainly Rural:"), "Local authorities where rural areas dominate the landscape and population.")
                                                ),
                                                
                                                h5("Urban Group:"),
                                                tags$ol(start = 3,
                                                        tags$li(strong("Urban with Substantial Rural:"), "Local authorities with significant urban centers but also substantial rural populations."),
                                                        tags$li(strong("Larger Cities:"), "Local authorities dominated by major urban centers and cities.")
                                                )
                                            ),
                                            # Image (right side) - Note the PNG extension for this one
                                            div(style = "flex: 0 0 300px;",
                                                img(src = "4_fold_image.png", 
                                                    style = "width: 100%; height: auto; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                                                    alt = "RESAS 4-fold Classification Map")
                                            )
                                        )
                               )
                             )                      
                           )
                         )
                       )
                )
              )
      ),
      
      # Categories Selection Tab
      tabItem(tabName = "categories",
              uiOutput("categories_main_content")
      ),
      
      # List of Metrics Tab 
      tabItem(tabName = "metrics_list",
              fluidRow(
                box(
                  title = "Complete List of Policy Metrics by Policy Area", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  p("Click on any policy metric name to navigate directly to that metric within its policy area.", 
                    style = "font-size: 1.1em; margin-bottom: 20px;"),
                  
                  # Generate metrics list dynamically using actual module definitions
                  div(
                    style = "margin-top: 20px;",
                    lapply(names(COMPLETE_METRICS), function(cat_id) {
                      cat_info <- COMPLETE_METRICS[[cat_id]]
                      
                      div(
                        style = "margin-bottom: 35px; border-left: 4px solid #007bff; padding-left: 20px; background: #f8f9fa; border-radius: 8px; padding: 20px;",
                        
                        # Category header with icon and status
                        div(
                          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 20px; border-bottom: 2px solid #e9ecef; padding-bottom: 15px;",
                          div(
                            style = "display: flex; align-items: center;",
                            icon(cat_info$icon, style = "font-size: 2em; margin-right: 15px; color: #007bff;"),
                            h3(cat_info$name, style = "margin: 0; color: #007bff; font-weight: 600;")
                          )
                        ),
                        
                        # Metrics list for this category - show clickable links with full names
                        div(
                          style = "display: grid; grid-template-columns: 1fr; gap: 12px;",
                          lapply(names(cat_info$metrics), function(metric_key) {
                            full_name <- cat_info$metrics[[metric_key]]
                            # Create a unique action button for each metric
                            metric_id <- paste0("metric_link_", gsub("[^A-Za-z0-9]", "_", paste(cat_id, metric_key, sep = "_")))
                            
                            div(
                              class = "metrics-card",
                              style = "background: white; border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; transition: all 0.2s ease;",
                              actionLink(
                                inputId = metric_id,
                                label = div(
                                  style = "display: flex; align-items: flex-start; justify-content: space-between;",
                                  div(
                                    div(
                                      style = "display: flex; align-items: center; margin-bottom: 5px;",
                                      icon("chart-bar", style = "margin-right: 10px; color: #28a745; font-size: 1.1em;"),
                                      strong(metric_key, style = "color: #007bff; font-size: 1.05em;")
                                    ),
                                    div(
                                      style = "margin-left: 25px; color: #6c757d; font-size: 0.95em; line-height: 1.4;",
                                      full_name
                                    )
                                  ),
                                  icon("external-link-alt", style = "color: #6c757d; font-size: 1em; margin-top: 2px;")
                                ),
                                style = "text-decoration: none; display: block; width: 100%;"
                              )
                            )
                          })
                        )
                      )
                    })
                  )
                )
              )
      ),
      
      # About Tab 
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This Dashboard", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  h4("Rural Scotland Performance Indicators"),
                  p("This dashboard provides comprehensive insights into rural Scotland's performance across 9 key policy areas."),
                  
                  h4("Data Sources:"),
                  tags$ul(
                    tags$li(tags$strong("Scottish Household Survey:"), "Cultural attendance and participation data, transport satisfaction"),
                    tags$li(tags$strong("Scotland's Census:"), "Population and demographic data"),
                    tags$li(tags$strong("Sub-Scotland Economic Statistics Database:"), "Economic, business, and employment data"),
                    tags$li(tags$strong("Local Authority Data:"), "Council-level performance metrics")
                  ),
                  
                  h4("Technical Architecture:"),
                  p("The dashboard is built with a modular architecture, with separate modules for each policy category. This allows for:"),
                  tags$ul(
                    tags$li("Independent development of category-specific analysis"),
                    tags$li("Easy addition of new data sources and metrics"),
                    tags$li("Maintainable and scalable codebase"),
                    tags$li("Consistent user experience across categories")
                  )
                )
              )
      )
    )
  )
) 
# Main Server
server <- function(input, output, session) {
  # Reactive values for overall app state
  values <- reactiveValues(
    current_category = NULL,
    current_classification = NULL,
    app_data_status = "Ready",
    navigation_source = "categories"
  )
  culture_server("culture", values, session)
  agriculture_server("agriculture_marine", values, session)
  transport_server("transport", values, session)
  housing_server("housing", values, session)
  population_server("population_skills", values, session)
  economy_server("economy_digital", values, session)
  social_justice_server("social_justice", values, session)
  health_server("health_social", values, session)
  environment_server("environment_climate", values, session)
  
  # Dynamic back button text based on navigation source
  output$back_button_text <- renderText({
    if(values$navigation_source == "metrics_list") {
      "← Back to List of Metrics"
    } else {
      "← Back to Categories"
    }
  })
  
  # Reset module inputs when category changes
  observeEvent(values$current_category, {
    cat("Category changed to:", values$current_category, "\n")
    if (is.null(values$current_category)) {
      cat("Resetting all module inputs due to no category selected\n")
      # Reset all modules when returning to category selection
      updateSelectInput(session, "culture_metric", selected = "")
      updateSelectInput(session, "agriculture_metric", selected = "")
      updateSelectInput(session, "transport_metric", selected = "")
      updateSelectInput(session, "housing_metric", selected = "")
      updateSelectInput(session, "population_metric", selected = "")
      updateSelectInput(session, "economy_metric", selected = "")
      updateSelectInput(session, "social_justice_metric", selected = "")
      updateSelectInput(session, "health_metric", selected = "")
      updateSelectInput(session, "environment_metric", selected = "")
      updateSelectInput(session, "environment_classification_type", selected = "")
      updateSelectInput(session, "selected_woodlands_activity", selected = names(woodlands_sub_metrics)[1])
      updateSelectInput(session, "agriculture_classification_type", selected = "")
      updateSelectInput(session, "selected_diversified_metric", selected = names(diversified_activity_sub_metrics)[1])
      # Add resets for other modules' sub-metric dropdowns if applicable
    } else {
      # Reset specific module inputs based on category
      if (values$current_category == "environment_climate") {
        cat("Resetting Environment module inputs\n")
        updateSelectInput(session, "environment_metric", selected = "")
        updateSelectInput(session, "environment_classification_type", selected = "")
        updateSelectInput(session, "selected_woodlands_activity", selected = names(woodlands_sub_metrics)[1])
      } else if (values$current_category == "agriculture_marine") {
        cat("Resetting Agriculture module inputs\n")
        updateSelectInput(session, "agriculture_metric", selected = "")
        updateSelectInput(session, "agriculture_classification_type", selected = "")
        updateSelectInput(session, "selected_diversified_metric", selected = names(diversified_activity_sub_metrics)[1])
      } else if (values$current_category == "culture") {
        cat("Resetting Culture module inputs\n")
        updateSelectInput(session, "culture_metric", selected = "")
        updateSelectInput(session, "culture_classification_type", selected = "")
        # Reset sub-metric dropdowns for Culture if applicable
      } else if (values$current_category == "transport") {
        cat("Resetting Transport module inputs\n")
        updateSelectInput(session, "transport_metric", selected = "")
        # Add classification or sub-metric resets if applicable
      } else if (values$current_category == "housing") {
        cat("Resetting Housing module inputs\n")
        updateSelectInput(session, "housing_metric", selected = "")
      } else if (values$current_category == "population_skills") {
        cat("Resetting Population module inputs\n")
        updateSelectInput(session, "population_metric", selected = "")
      } else if (values$current_category == "economy_digital") {
        cat("Resetting Economy module inputs\n")
        updateSelectInput(session, "economy_metric", selected = "")
      } else if (values$current_category == "social_justice") {
        cat("Resetting Social Justice module inputs\n")
        updateSelectInput(session, "social_justice_metric", selected = "")
      } else if (values$current_category == "health_social") {
        cat("Resetting Health module inputs\n")
        updateSelectInput(session, "health_metric", selected = "")
      }
    }
  })
  
  # Handle navigation
  observeEvent(input$explore_policy_areas, {
    updateTabItems(session, "sidebar", selected = "categories")
    values$navigation_source <- "categories"
    values$current_category <- NULL
  })
  
  observeEvent(input$explore_policy_metrics, {
    updateTabItems(session, "sidebar", selected = "metrics_list")
    values$navigation_source <- "categories"
    values$current_category <- NULL
  })
  
  # Handle category link clicks from home page - UPDATED WITH STATE RESET
  lapply(names(CATEGORIES), function(cat_id) {
    observeEvent(input[[paste0("cat_link_", cat_id)]], {
      # Reset all module states before switching
      reset_module_states(session)
      
      # Small delay to ensure resets are processed
      later::later(function() {
        values$current_category <- cat_id
        values$navigation_source <- "categories"
        updateTabItems(session, "sidebar", selected = "categories")
      }, delay = 0.1)
    })
  })
  
  # Handle back button with context awareness - UPDATED WITH STATE RESET
  observeEvent(input$back_to_categories, {
    # Reset all module states when going back
    reset_module_states(session)
    
    if(values$navigation_source == "metrics_list") {
      updateTabItems(session, "sidebar", selected = "metrics_list")
    } else {
      updateTabItems(session, "sidebar", selected = "categories")
    }
    
    # Reset category selection after a small delay
    later::later(function() {
      values$current_category <- NULL
      values$navigation_source <- "categories"
    }, delay = 0.1)
  })
  
  # Main categories content
  output$categories_main_content <- renderUI({
    if (is.null(values$current_category)) {
      fluidRow(
        box(
          title = "Select a Policy Area to Explore", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 15px;",
            lapply(names(CATEGORIES), function(cat_id) {
              cat_info <- CATEGORIES[[cat_id]]
              is_featured <- cat_info$data_available
              is_available <- cat_info$data_available
              div(
                class = paste0("category-card", 
                               ifelse(is_featured, " featured-category", ""),
                               ifelse(!is_available, " coming-soon-category", "")),
                `data-category` = cat_id,
                style = "padding: 20px;",
                actionButton(
                  inputId = paste0("cat_", cat_id),
                  label = div(
                    icon(cat_info$icon, style = "font-size: 2em; margin-bottom: 10px;"),
                    h4(case_when(
                      cat_id == "population_skills" ~ "Population, Education & Skills",
                      cat_id == "environment_climate" ~ "Environment & Climate",
                      cat_id == "agriculture_marine" ~ "Agriculture & Marine",
                      cat_id == "economy_digital" ~ "Economy & Digital",
                      cat_id == "health_social" ~ "Health & Social Care",
                      TRUE ~ cat_info$name
                    ), style = paste0("margin: 10px 0;", 
                                      ifelse(is_featured, " color: #007bff;", ""),
                                      if(cat_id %in% c("population_skills", "environment_climate")) " text-align: center;" else ""))
                  ),
                  class = "btn btn-link category-btn",
                  style = "width: 100%; height: 100%; border: none; background: transparent; text-decoration: none;"
                )
              )
            })
          )
        )
      )
    } else {
      cat("Switching to category:", values$current_category, "\n")
      switch(values$current_category,
             "culture" = culture_dashboard_ui(values$current_category),
             "agriculture_marine" = agriculture_dashboard_ui(values$current_category),
             "transport" = transport_dashboard_ui(values$current_category),
             "housing" = housing_dashboard_ui(values$current_category),
             "population_skills" = population_dashboard_ui(values$current_category),
             "environment_climate" = {
               cat("Rendering environment_dashboard_ui\n")
               environment_dashboard_ui(values$current_category)
             },
             "economy_digital" = economy_dashboard_ui(values$current_category),
             "health_social" = health_dashboard_ui(values$current_category),
             "social_justice" = social_justice_dashboard_ui(values$current_category),
             fluidRow(
               box(
                 title = "Policy Area Not Found",
                 status = "warning",
                 solidHeader = TRUE,
                 width = 12,
                 p("The selected policy area is not available.")
               )
             )
      )
    }
  })
  
  # Handle category button clicks (from categories page) - UPDATED WITH STATE RESET
  lapply(names(CATEGORIES), function(cat_id) {
    observeEvent(input[[paste0("cat_", cat_id)]], {
      # Reset all module states before switching
      reset_module_states(session)
      
      # Small delay to ensure resets are processed
      later::later(function() {
        values$current_category <- cat_id
        values$navigation_source <- "categories"
      }, delay = 0.1)
    })
  })
  
  # Handle metric link clicks - UPDATED WITH STATE RESET
  lapply(names(COMPLETE_METRICS), function(cat_id) {
    cat_info <- COMPLETE_METRICS[[cat_id]]
    lapply(names(cat_info$metrics), function(metric_key) {
      metric_id <- paste0("metric_link_", gsub("[^A-Za-z0-9]", "_", paste(cat_id, metric_key, sep = "_")))
      
      observeEvent(input[[metric_id]], {
        # Reset all module states before switching
        reset_module_states(session)
        
        # Navigate to the category first
        values$current_category <- cat_id
        values$navigation_source <- "metrics_list"
        updateTabItems(session, "sidebar", selected = "categories")
        
        # Set the metric after a delay
        later::later(function() {
          if(cat_id == "culture") {
            updateSelectInput(session, "culture_metric", selected = metric_key)
          } else if(cat_id == "agriculture_marine") {
            updateSelectInput(session, "agriculture_metric", selected = metric_key)
          } else if(cat_id == "transport") {
            updateSelectInput(session, "transport_metric", selected = metric_key)
          } else if(cat_id == "housing") {
            updateSelectInput(session, "housing_metric", selected = metric_key)
          } else if(cat_id == "economy_digital") {
            updateSelectInput(session, "economy_metric", selected = metric_key)
          } else if(cat_id == "population_skills") {
            updateSelectInput(session, "population_metric", selected = metric_key)
          } else if(cat_id == "social_justice") {
            updateSelectInput(session, "social_justice_metric", selected = metric_key)
          } else if(cat_id == "health_social") {
            updateSelectInput(session, "health_metric", selected = metric_key)
          } else if(cat_id == "environment_climate") {
            updateSelectInput(session, "environment_metric", selected = metric_key)
          }
        }, delay = 1.2)  # Increased delay for metric selection
      })
    })
  })
  
  # Module server functions
}

# Run the application
shinyApp(ui = ui, server = server)
>>>>>>> a88732c399431e9b684a6e02de83638eef2a8ee4
