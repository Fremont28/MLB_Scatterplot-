install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "DT"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
install.packages("ggrepel")
library(ggrepel)


# Example dataset (replace with your own)
# For sports data, load something like CSV or API response
df<- read.csv("~/Documents/R Shiny Montana/2019_2025_statcast.csv")

# ---- Player name handling ----
df$player_name <- df$last_name..first_name

# Extract LAST name only (before comma)
df$last_name <- sub(",.*", "", df$player_name)

# ---- SAFELY detect season column ----
season_col <- intersect(
  c("season", "year", "game_year", "Season"),
  names(df)
)[1]

if (is.na(season_col)) {
  stop("No season/year column found in dataset.")
}

df$season <- as.character(df[[season_col]])

# ---- Numeric variables only ----
numeric_vars <- names(df)[
  sapply(df, is.numeric) &
    !names(df) %in% c("player_id","year")
]


# ---- UI ----
ui <- fluidPage(
  titlePanel("2019-2025 MLB Player Metrics"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x_var", "X Variable:", numeric_vars),
      selectInput("y_var", "Y Variable:", numeric_vars),
      sliderInput(
        "z_thresh",
        "Z-score threshold:",
        min = 1.5, max = 4, value = 2.5, step = 0.1
      )
    ),
    mainPanel(
      plotlyOutput("scatterPlot", height = "550px")
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  plot_data <- reactive({
    req(input$x_var, input$y_var)
    
    df %>%
      filter(
        !is.na(.data[[input$x_var]]),
        !is.na(.data[[input$y_var]])
      ) %>%
      mutate(
        x_z = as.numeric(scale(.data[[input$x_var]])),
        y_z = as.numeric(scale(.data[[input$y_var]])),
        outlier = ifelse(
          abs(x_z) > input$z_thresh |
            abs(y_z) > input$z_thresh,
          "Outlier", "Normal"
        ),
        label = ifelse(
          pmax(abs(x_z), abs(y_z)) >= 3,
          paste0(last_name, " (", season, ")"),
          NA
        ),
        tooltip = paste0(
          "<b>", player_name, "</b><br>",
          "Season: ", season, "<br>",
          input$x_var, ": ", round(.data[[input$x_var]], 2), "<br>",
          input$y_var, ": ", round(.data[[input$y_var]], 2)
        )
      )
    
  })
  
  output$scatterPlot <- renderPlotly({
    
    d <- plot_data()
    
    p <- plot_ly(
      data = d,
      x = ~get(input$x_var),
      y = ~get(input$y_var),
      type = "scatter",
      mode = "markers",
      color = ~outlier,
      colors = c("Normal" = "grey70", "Outlier" = "orange"),
      text = ~tooltip,
      hoverinfo = "text",
      marker = list(size = 9, opacity = 0.8)
    )
    
    # Labels for OUTLIERS ONLY
    outliers <- d %>% filter(outlier == "Outlier")
    
    p <- p %>%
      add_text(
        data = outliers,
        x = ~get(input$x_var),
        y = ~get(input$y_var),
        text = ~label,
        textposition = "top center",
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = input$x_var),
        yaxis = list(title = input$y_var)
      )
    
    p
  })
  
}

shinyApp(ui, server)





