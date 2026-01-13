
# modules/classification_module.R - Urban Rural Classification Information Pages

# Classification data structures
classification_6fold <- list(
  title = "Scottish Government Urban Rural Classification, 6-fold",
  categories = list(
    list(
      number = "1",
      name = "Large Urban Areas", 
      description = "Settlements of 125,000 people and over.",
      color = "#2E8B57"
    ),
    list(
      number = "2", 
      name = "Other Urban Areas",
      description = "Settlements of 10,000 to 124,999 people.",
      color = "#32CD32"
    ),
    list(
      number = "3",
      name = "Accessible Small Towns", 
      description = "Settlements of 3,000 to 9,999 people, and within a 30 minute drive time of a Settlement of 10,000 or more.",
      color = "#90EE90"
    ),
    list(
      number = "4",
      name = "Remote Small Towns",
      description = "Settlements of 3,000 to 9,999 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more.",
      color = "#FFD700"
    ),
    list(
      number = "5", 
      name = "Accessible Rural Areas",
      description = "Areas with a population of less than 3,000 people, and within a 30 minute drive time of a Settlement of 10,000 or more.",
      color = "#FFA500"
    ),
    list(
      number = "6",
      name = "Remote Rural Areas", 
      description = "Areas with a population of less than 3,000 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more.",
      color = "#FF6347"
    )
  )
)

classification_8fold <- list(
  title = "Scottish Government Urban Rural Classification, 8-fold",
  categories = list(
    list(
      number = "1",
      name = "Large Urban Areas",
      description = "Settlements of 125,000 people and over.",
      color = "#2E8B57"
    ),
    list(
      number = "2",
      name = "Other Urban Areas", 
      description = "Settlements of 10,000 to 124,999 people.",
      color = "#32CD32"
    ),
    list(
      number = "3",
      name = "Accessible Small Towns",
      description = "Settlements of 3,000 to 9,999 people, and within a 30 minute drive time of a Settlement of 10,000 or more.",
      color = "#90EE90"
    ),
    list(
      number = "4", 
      name = "Remote Small Towns",
      description = "Settlements of 3,000 to 9,999 people, and with a drive time of over 30 minutes to a Settlement of 10,000 or more.",
      color = "#98FB98"
    ),
    list(
      number = "5",
      name = "Very Remote Small Towns",
      description = "Settlements of 3,000 to 9,999 people, and with a drive time of over 60 minutes to a Settlement of 10,000 or more.",
      color = "#FFD700"
    ),
    list(
      number = "6",
      name = "Accessible Rural Areas", 
      description = "Areas with a population of less than 3,000 people, and within a 30 minute drive time of a Settlement of 10,000 or more.",
      color = "#FFA500"
    ),
    list(
      number = "7",
      name = "Remote Rural Areas",
      description = "Areas with a population of less than 3,000 people, and with a drive time of over 30 minutes but less than or equal to 60 minutes to a Settlement of 10,000 or more.",
      color = "#FF7F50"
    ),
    list(
      number = "8", 
      name = "Very Remote Rural Areas",
      description = "Areas with a population of less than 3,000 people, and with a drive time of over 60 minutes to a Settlement of 10,000 or more.",
      color = "#FF6347"
    )
  )
)

classification_3fold <- list(
  title = "Scottish Government Urban Rural Classification, 3-fold",
  description = "The Scottish Government 3-fold Urban Rural Classification includes three categories:",
  categories = list(
    list(
      name = "Accessible Rural",
      description = "Rural areas within 30 minutes drive time of settlements of 10,000 or more people",
      color = "#32CD32"
    ),
    list(
      name = "Remote Rural", 
      description = "Rural areas with drive time of over 30 minutes to settlements of 10,000 or more people",
      color = "#FF6347"
    ),
    list(
      name = "Rest of Scotland",
      description = "All urban areas including large urban areas, other urban areas, and small towns",
      color = "#4682B4"
    )
  )
)

classification_2fold <- list(
  title = "Scottish Government Urban Rural Classification, 2-fold",
  description = "The Scottish Government core definition of rurality classifies areas with a population of fewer than 3,000 people to be rural. The Scottish Government Urban Rural Classification can be collapsed to this core definition, to create a 2-fold classification.",
  categories = list(
    list(
      name = "Rest of Scotland",
      description = "(1) Large Urban Areas, (2) Other Urban Areas, (3) Accessible Small Towns, and (4) Remote Small Towns.",
      color = "#4682B4"
    ),
    list(
      name = "Rural Scotland", 
      description = "(5) Accessible Rural and (6) Remote Rural Areas.",
      color = "#32CD32"
    )
  ),
  note = "On slides where a further breakdown of the data is not possible (for example, because the sample is too small), figures are presented in two categories: 'Rural' and 'Urban'. In this classification, the 'Rural' category includes accessible and remote rural areas."
)

classification_resas <- list(
  title = "Rural & Environmental Science and Analytical Services (RESAS) Classification of Local Authorities",
  description = "This classification classifies local authorities according to their level of rurality and establishes four different groups as well as a broader urban-rural grouping:",
  rural_group = list(
    name = "Rural",
    categories = list(
      list(
        number = "1",
        name = "Islands and Remote Rural",
        description = "Local authorities with significant island populations or very remote mainland areas",
        color = "#FF6347"
      ),
      list(
        number = "2", 
        name = "Mainly Rural",
        description = "Local authorities where rural areas dominate the landscape and population",
        color = "#FFA500"
      )
    )
  ),
  urban_group = list(
    name = "Urban",
    categories = list(
      list(
        number = "3",
        name = "Urban with Substantial Rural",
        description = "Local authorities with significant urban centers but also substantial rural populations",
        color = "#90EE90"
      ),
      list(
        number = "4",
        name = "Larger Cities", 
        description = "Local authorities dominated by major urban centers and cities",
        color = "#4682B4"
      )
    )
  )
)

# UI function for classification pages
create_classification_ui <- function(classification_type) {
  classification_data <- switch(classification_type,
    "2fold" = classification_2fold,
    "3fold" = classification_3fold, 
    "6fold" = classification_6fold,
    "8fold" = classification_8fold,
    "resas" = classification_resas
  )
  
  tagList(
    # Header section
    fluidRow(
      div(
        class = "classification-header",
        style = "background: linear-gradient(135deg, #2E8B57, #32CD32);
                 color: white;
                 padding: 30px;
                 border-radius: 8px;
                 margin-bottom: 30px;
                 text-align: center;",
        h1(classification_data$title, style = "margin-bottom: 15px; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);"),
        if(!is.null(classification_data$description)) {
          p(classification_data$description, style = "font-size: 1.2em; margin-bottom: 20px;")
        },
        actionButton("back_to_home", "← Back to Home", 
                     class = "btn btn-light",
                     style = "margin-right: 15px;"),
        actionButton("explore_data", "Explore Data Categories", 
                     class = "btn btn-warning",
                     icon = icon("chart-bar"))
      )
    ),
    
    # Content section
    if(classification_type == "resas") {
      create_resas_content(classification_data)
    } else {
      create_standard_classification_content(classification_data)
    }
  )
}

# Standard classification content (6-fold, 8-fold, 3-fold, 2-fold)
create_standard_classification_content <- function(classification_data) {
  fluidRow(
    # Categories list
    column(8,
      box(
        title = "Classification Categories",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        div(
          class = "categories-list",
          lapply(classification_data$categories, function(category) {
            div(
              class = "category-item",
              style = paste0("background: ", category$color, "20; 
                            border-left: 5px solid ", category$color, ";
                            padding: 20px;
                            margin-bottom: 15px;
                            border-radius: 5px;"),
              div(
                style = "display: flex; align-items: center; gap: 15px;",
                if(!is.null(category$number)) {
                  div(
                    style = paste0("background: ", category$color, "; 
                                  color: white; 
                                  width: 40px; 
                                  height: 40px; 
                                  border-radius: 50%; 
                                  display: flex; 
                                  align-items: center; 
                                  justify-content: center; 
                                  font-weight: bold;
                                  font-size: 1.2em;"),
                    category$number
                  )
                },
                div(
                  h4(category$name, style = "margin: 0; color: #333;"),
                  p(category$description, style = "margin: 5px 0 0 0; color: #666; line-height: 1.4;")
                )
              )
            )
          })
        ),
        if(!is.null(classification_data$note)) {
          div(
            class = "alert alert-info",
            style = "margin-top: 20px;",
            h5("Important Note:"),
            p(classification_data$note)
          )
        }
      )
    ),
    
    # Visualization
    column(4,
      box(
        title = "Visual Overview",
        status = "info", 
        solidHeader = TRUE,
        width = 12,
        plotOutput("classification_chart", height = "400px")
      )
    )
  )
}

# RESAS specific content
create_resas_content <- function(classification_data) {
  fluidRow(
    column(12,
      # Rural group
      box(
        title = paste("1.", classification_data$rural_group$name),
        status = "success",
        solidHeader = TRUE,
        width = 6,
        lapply(classification_data$rural_group$categories, function(category) {
          div(
            class = "category-item",
            style = paste0("background: ", category$color, "20; 
                          border-left: 5px solid ", category$color, ";
                          padding: 15px;
                          margin-bottom: 10px;
                          border-radius: 5px;"),
            h5(paste(category$number, ".", category$name), style = "margin: 0 0 10px 0; color: #333;"),
            p(category$description, style = "margin: 0; color: #666; line-height: 1.4;")
          )
        })
      ),
      
      # Urban group  
      box(
        title = paste("2.", classification_data$urban_group$name),
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        lapply(classification_data$urban_group$categories, function(category) {
          div(
            class = "category-item", 
            style = paste0("background: ", category$color, "20;
                          border-left: 5px solid ", category$color, ";
                          padding: 15px;
                          margin-bottom: 10px;
                          border-radius: 5px;"),
            h5(paste(category$number, ".", category$name), style = "margin: 0 0 10px 0; color: #333;"),
            p(category$description, style = "margin: 0; color: #666; line-height: 1.4;")
          )
        })
      )
    ),
    
    # Visualization for RESAS
    column(12,
      box(
        title = "RESAS Classification Overview",
        status = "info",
        solidHeader = TRUE, 
        width = 12,
        plotOutput("resas_chart", height = "300px")
      )
    )
  )
}

# Server function for classification pages
classification_server <- function(input, output, session, classification_type) {
  
  classification_data <- switch(classification_type,
    "6fold" = classification_6fold,
    "8fold" = classification_8fold,
    "3fold" = classification_3fold, 
    "2fold" = classification_2fold,
    "resas" = classification_resas
  )
  
  # Standard classification chart
  output$classification_chart <- renderPlot({
    if(classification_type != "resas") {
      library(ggplot2)
      
      # Create data frame for visualization
      chart_data <- data.frame(
        Category = sapply(classification_data$categories, function(x) x$name),
        Population = if(classification_type == "3fold") c(850000, 500000, 3500000) else 
                    if(classification_type == "2fold") c(4350000, 500000) else
                    rep(1, length(classification_data$categories)),
        Color = sapply(classification_data$categories, function(x) x$color),
        stringsAsFactors = FALSE
      )
      
      # Create a simple bar chart or pie chart
      if(classification_type %in% c("3fold", "2fold")) {
        ggplot(chart_data, aes(x = "", y = Population, fill = Category)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          scale_fill_manual(values = chart_data$Color) +
          theme_void() +
          theme(legend.position = "bottom",
                legend.text = element_text(size = 8)) +
          labs(title = "Population Distribution")
      } else {
        ggplot(chart_data, aes(x = reorder(Category, -row_number()), y = Population, fill = Category)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = chart_data$Color) +
          coord_flip() +
          theme_minimal() +
          theme(legend.position = "none",
                axis.text.y = element_text(size = 8),
                axis.title = element_blank()) +
          labs(title = "Classification Categories")
      }
    }
  })
  
  # RESAS specific chart
  output$resas_chart <- renderPlot({
    if(classification_type == "resas") {
      library(ggplot2)
      
      # Create hierarchical data
      chart_data <- data.frame(
        Group = c(rep("Rural", 2), rep("Urban", 2)),
        Category = c("Islands & Remote Rural", "Mainly Rural", 
                    "Urban with Substantial Rural", "Larger Cities"),
        Value = c(1, 1, 1, 1),
        Color = c("#FF6347", "#FFA500", "#90EE90", "#4682B4")
      )
      
      ggplot(chart_data, aes(x = Group, y = Value, fill = Category)) +
        geom_bar(stat = "identity", position = "stack", width = 0.6) +
        scale_fill_manual(values = chart_data$Color) +
        theme_minimal() +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank()) +
        labs(title = "RESAS Classification Structure",
             fill = "Local Authority Types")
    }
  })
}
