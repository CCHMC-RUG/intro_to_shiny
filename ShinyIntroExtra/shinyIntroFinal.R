#https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(

  #STATIC OUTPUT
  titlePanel('Iris k-means clustering'),
  
  #INPUT
  numericInput(inputId = 'nClusters', 
               label = 'Number of clusters', 
               value = 3, min = 1, max = 10),
  
  #DYNAMIC OUTPUT
  plotOutput(outputId = 'plot1')
  
)

# Define server logic
server <- function(input, output, session) {
  
  #GET THE DATA
  data("iris")
  selectedData <- iris[, c("Sepal.Length", "Sepal.Width")]
  
  #OUTPUT PLOT
  output$plot1 <- renderPlot({

    #ANALYSE THE DATA
    clusters <- kmeans(selectedData, input$nClusters)
    
    #OUTPUT PLOT
    ggplot(selectedData, aes(x = Sepal.Length, 
                                      y = Sepal.Width, 
                                      colour = factor(clusters$cluster))) + 
      geom_point(size = 5) + theme_minimal() +  theme(legend.position = "none")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

