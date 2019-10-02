### CLUSTER IRIS DATA ###

library(ggplot2)

#GET THE DATA
data("iris")
selectedData <- iris[, c("Sepal.Length", "Sepal.Width")]

#ANALYSE THE DATA
clusters <- kmeans(selectedData, 3)

#OUTPUT PLOT
myPlot = ggplot(selectedData, 
                aes(x = Sepal.Length,
                    y = Sepal.Width,
                    colour = factor(clusters$cluster))) + 
  geom_point(size = 5) + 
  theme_minimal() + 
  theme(legend.position = "none")

#Plot it...
myPlot
