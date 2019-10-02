library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(

  #STATIC OUTPUT
  titlePanel('Iris k-means clustering'),
  
  fluidRow(column(3,
   wellPanel(numericInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 6),
             selectInput(inputId = 'xcol', label = 'X Variable', 
                         choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
             selectInput(inputId = 'ycol', label = 'Y Variable', 
                         choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), 
                         selected = "Sepal.Width"),
             actionButton(inputId = "summaryButton", "Get summary")
             )               
                  ),
   column(9, tags$img(src="irisData.png"))),
  
  #DYNAMIC OUTPUT
  plotOutput(outputId = 'plot1')
  
)

# Define server logic
server <- function(input, output, session) {
  
  #GET THE DATA
  data("iris")
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  #ANALYSE THE DATA
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  #OUTPUT PLOT
  output$plot1 <- renderPlot({

    colNames = colnames(selectedData())
    ggplot(selectedData(), aes_string(x = colNames[1],
                               y = colNames[2],
                               colour = factor(clusters()$cluster))) +
      geom_point(size = 5) + theme_minimal() +  theme(legend.position = "none")

  })
  
  #This is another way of implementing the ggplot
  
  # output$plot1 <- renderPlot({
  # 
  #   ggplot(selectedData(), aes(x = selectedData()[,1], 
  #                            y = selectedData()[,2], 
  #                            colour = factor(clusters()$cluster))) + 
  #     geom_point(size = 5) + theme_minimal() +  theme(legend.position = "none")
  # 
  # })
  
  observeEvent(input$summaryButton, {
    showModal(modalDialog(
      paste("There are", nrow(selectedData()), "points, divided into", input$clusters, "clusters")
    ))
  })
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

