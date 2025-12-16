#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

source("source.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Bird surveillance dashboard"),
  tabsetPanel(tabPanel(
    "Overview",
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(sliderInput(
        "bins",
        "Number of bins:",
        min = 1,
        max = 50,
        value = 30
      )),
      
      # Show a plot of the generated distribution
      mainPanel(plotOutput("distPlot"))
    )
  ),tabPanel(
    "Swagger",
    sidebarLayout(
      sidebarPanel(),
      # Show a plot of the generated distribution
      mainPanel(plotlyOutput("Map1"))
    )
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    output$Map1 <- renderPlotly({
      total_map <- base_map +
        geom_point(
          data = live_free_range,
          aes(
            x = Long_rep, y = Lat_rep, size = cluster_size,
            text = paste("Number of Birds:", cluster_size)
          ),
          fill = "blue", alpha = 0.6, shape = 21, color = "transparent", stroke = 0.4
        ) +
        geom_text(
          data = live_free_range,
          aes(
            x = Long_rep,
            y = Lat_rep,
            label = ifelse(cluster_size>1000,cluster_size,'')
          ),
          size = 2
        ) +
        scale_size_continuous(name = "Number of Birds", range = c(0.5, 12)) +
        labs(title = " ")
      
      ggplotly(total_map, tooltip = "text") %>%
        layout(
          xaxis = list(autorange = TRUE),
          yaxis = list(autorange = TRUE),
          margin = list(l = 0, r = 0, b = 0, t = 20, pad = 0)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
