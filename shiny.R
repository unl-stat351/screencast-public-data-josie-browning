library(shiny)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Crop Energy/Health Bar Chart"),

  fluidRow(
    column(
      4,
      selectInput("measure", "Select Measure:", choices = c("Energy", "Health")),
      selectInput("season", "Season:", c("All", unique(crops_df_long$Season)))
    )
  ),

  plotlyOutput("bar_plot", height = "500px")
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    df <- crops_df_long

    if (input$season != "All") {
      df <- df %>% filter(Season == input$season,
                          Quality == "iridium",
                          Type == input$measure)
    }

    df
  })


  output$bar_plot <- renderPlotly({
    df <- filtered_data()

    plot_ly(
      data = df,
      x = ~Crops,
      y = ~Value,
      type = 'bar',
      hoverinfo = 'text',
      marker = list(color = 'skyblue')
    ) %>%
      layout(
        yaxis = list(title = input$measure),
        xaxis = list(title = "Crop"),
        title = paste(input$measure, "by Crop")
      )
  })
}

shinyApp(ui = ui, server = server)


