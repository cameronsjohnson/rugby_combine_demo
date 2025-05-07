################################################################################
# Combine sport app
# May 2025
################################################################################

################################################################################
# Set up
################################################################################
library(tidyverse)
library(shiny)
library(readxl)
library(bslib)
library(fmsb)

################################################################################
# Example data
################################################################################
# Load combine data
combine_data <- read_csv("data/nfl_combine_2010_to_2023.csv")

combine_data <- combine_data |>
  rename(Forty = `40yd`)

# Convert height (e.g. "6-3") to inches
combine_data$HeightInches <- sapply(combine_data$Height, function(h) {
  if (is.na(h) || !grepl("-", h)) return(NA)
  parts <- strsplit(h, "-")[[1]]
  as.numeric(parts[1]) * 12 + as.numeric(parts[2])
})

################################################################################
# App
################################################################################
# Define UI
ui <- navbarPage(
  title = "🏈 NFL Combine Result Comparator",
  theme = bs_theme(bootswatch = "lux"),
  
  tabPanel("Compare",
           sidebarLayout(
             sidebarPanel(
               div(class = "sidebar-panel",
                   h4("Enter Your Result"),
                   selectInput("position", "Position", choices = sort(unique(combine_data$Pos))),
                   numericInput("height", "Height (inches)", value = 72, min = 60, max = 85),
                   numericInput("weight", "Weight (lbs)", value = 200, min = 150, max = 350),
                   numericInput("forty", "40-Yard Dash Time (sec)", value = 4.5, min = 4.0, max = 6.0, step = 0.01),
                   numericInput("vertical", "Vertical Jump (inches)", value = 32, min = 20, max = 45),
                   numericInput("bench", "Bench Reps", value = 20, min = 0, max = 50),
                   numericInput("shuttle", "20-Yard Shuttle (sec)", value = 4.2, min = 3.8, max = 6.0, step = 0.01),
                   actionButton("submit", "Compare", class = "btn-primary btn-lg")
               )
             ),
             mainPanel(
               div(class = "main-panel",
                   h4("Comparison to Historic Data"),
                   verbatimTextOutput("summary"),
                   plotOutput("plot_forty"),
                   plotOutput("plot_vertical"),
                   plotOutput("plot_bench")
               )
             )
           )
  ),
  
  tabPanel("Player Profile",
           fluidRow(
             column(12,
                    h4("Radar Profile Compared to Positional Average"),
                    plotOutput("radar_plot", height = "500px")
             )
           )
  )
)

# Server
server <- function(input, output) {
  new_player <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    new_player(data.frame(
      Player = "You",
      Pos = input$position,
      HeightInches = input$height,
      Weight = input$weight,
      Forty = input$forty,
      Vertical = input$vertical,
      Bench = input$bench,
      Shuttle = input$shuttle
    ))
  })
  
  output$summary <- renderPrint({
    req(new_player())
    df <- combine_data %>% filter(Pos == input$position)
    
    # Use ecdf() safely with NA handling
    get_percentile <- function(var, value) {
      x <- df[[var]]
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA)
      ecdf(x)(value) * 100
    }
    
    p <- new_player()
    cat("Results for Position:", input$position, "\n\n")
    cat("• 40-Yard Dash: faster than", round(get_percentile("Forty", p$Forty), 1), "%\n")
    cat("• Vertical Jump: higher than", round(get_percentile("Vertical", p$Vertical), 1), "%\n")
    cat("• Bench Press: more reps than", round(get_percentile("Bench", p$Bench), 1), "%\n")
    cat("• Shuttle: faster than", round(get_percentile("Shuttle", p$Shuttle), 1), "%\n")
  })
  
  plot_hist <- function(var, color, input_val) {
    df <- combine_data %>% filter(Pos == input$position)
    df <- df[!is.na(df[[var]]), ]
    ggplot(df, aes_string(var)) +
      geom_histogram(binwidth = 0.25, fill = color, color = "white") +
      geom_vline(xintercept = input_val, color = "red", size = 1.5) +
      labs(x = var, y = "Count") +
      theme_minimal()
  }
  
  output$plot_forty <- renderPlot({
    req(new_player())
    plot_hist("Forty", "#3498db", input$forty)
  })
  
  output$plot_vertical <- renderPlot({
    req(new_player())
    plot_hist("Vertical", "#9b59b6", input$vertical)
  })
  
  output$plot_bench <- renderPlot({
    req(new_player())
    plot_hist("Bench", "#e67e22", input$bench)
  })
  
  output$radar_plot <- renderPlot({
    req(new_player())
    
    # Select relevant data
    df <- combine_data %>%
      filter(Pos == input$position) %>%
      select(Forty, Vertical, Bench, Shuttle)
    
    # Clean NAs
    df_clean <- df %>% na.omit()
    if (nrow(df_clean) < 5) return(NULL)  # not enough data
    
    # Average values
    avg <- colMeans(df_clean, na.rm = TRUE)
    
    # Player input
    p <- new_player()[, c("Forty", "Vertical", "Bench", "Shuttle")]
    names(p) <- names(avg)
    
    # Normalize and invert where lower is better
    scale_metric <- function(x, invert = FALSE) {
      rng <- range(df_clean[[x]], na.rm = TRUE)
      val <- c(p[[x]], avg[[x]])
      norm <- (val - rng[1]) / diff(rng)
      if (invert) norm <- 1 - norm
      return(norm)
    }
    
    radar_df <- data.frame(
      row.names = c("You", "Average"),
      Forty   = scale_metric("Forty", invert = TRUE),
      Vertical = scale_metric("Vertical"),
      Bench    = scale_metric("Bench"),
      Shuttle  = scale_metric("Shuttle", invert = TRUE)
    )
    
    # Add max/min for fmsb
    radar_df <- rbind(
      rep(1, ncol(radar_df)),  # max
      rep(0, ncol(radar_df)),  # min
      radar_df
    )
    
    fmsb::radarchart(
      radar_df,
      axistype = 1,
      pcol = c("red", "blue"), pfcol = c(scales::alpha("red", 0.5), scales::alpha("blue", 0.5)),
      plwd = 2,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey30",
      vlcex = 1.1,
      title = paste("Radar Profile: You vs Avg", input$position)
    )
    
    legend("topright", legend = c("You", "Average"), col = c("red", "blue"),
           lty = 1, lwd = 2, bty = "n")
  })
}

shinyApp(ui = ui, server = server)