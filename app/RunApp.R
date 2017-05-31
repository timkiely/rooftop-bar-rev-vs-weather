


rev_ts <- readRDS("weather-revenue-data.rds")

ui <- 
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput("obs", "Number of observations:", min = 5, max = 500, value = 100)
      ),
      mainPanel(plotOutput("distPlot"))
    )
  )





server <- 
  function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs), col = 'skyblue', border = 'white')
    })
  }






shinyApp(ui = ui, server = server)

