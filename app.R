library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)

# Import data
insulin <- read_csv("insulin.csv")

# Convert to data frame
tdd_df <- as.data.frame(insulin)

# Format Time column, create date column
tdd_df$Time <- as.POSIXct(tdd_df$Time, format = "%d/%m/%Y %H:%M", tz=Sys.timezone())
tdd_df$date = as.Date(tdd_df$Time, format = "%Y-%m-%d")


str(tdd_df)

# Rename tdd variable
tdd_df <- tdd_df %>%
  rename(total_daily_dose = "Total daily dose",
         total_daily_basal = "Total daily basal")

# Filter out rows with null values in the tdd variable  
tdd_date <- filter(tdd_df, (!is.na(tdd_df$total_daily_dose))) 

tdd_date$percent <- (tdd_date$total_daily_basal / tdd_date$total_daily_dose)
tdd_date$percent <- (tdd_date$percent*100)
tdd_date$percent <- round(tdd_date$percent, digits = 0)

str(tdd_date)
head(tdd_date)


# Create app user interface
ui <- fluidPage(
  titlePanel("Diabetes Tracker"),
  
  sidebarLayout(
    sidebarPanel( 
      #date selector goes here 
      dateRangeInput("range_date", "Select date range",
                     start = min(tdd_date$date),
                     end = max(tdd_date$date),
                     min = min(tdd_date$date),
                     max = max(tdd_date$date),
                     format = "dd/mm/yyyy")),
    
    mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%")), 
                       plotlyOutput("Plot"), 
                       plotlyOutput("Plot2")) #plot output goes here
    )
  )
)

server <- function(input, output, session)({
  
  output$Plot <- renderPlotly({
    data1 <- tdd_date[tdd_date$date>=input$range_date[1] & tdd_date$date<=input$range_date[2],]
    ggplot(data1, 
           aes(x = `date`, y = `total_daily_dose`)) +
      geom_point(alpha = .25) +
      geom_smooth(method=lm, col='grey', size=.5) +
      scale_x_date() +
      xlab("date") + ylab("tdd") +
      ylim(0,16) +
      labs(title = "Total daily dose (TDD)")})
  
  output$Plot2 <- renderPlotly({
    data1 <- tdd_date[tdd_date$date>=input$range_date[1] & tdd_date$date<=input$range_date[2],]
    ggplot(data1,
           aes(x = `date`, y = `percent`)) + 
      geom_col(fill="coral1", col = "coral1") +
      geom_smooth(method=lm, col='black', size=.5, se=FALSE) +
      ylim(0,100) +
      xlab("date") + ylab("% basal") +
      labs(title = "Basal as a % of TDD")
    
  })
  
})

# Launch app
shinyApp(ui = ui, server = server)