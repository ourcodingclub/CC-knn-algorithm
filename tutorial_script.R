library(ggplot2)
library(dplyr)
library(class)
library(gridExtra)
library(gmodels)

# Loading iris dataset
iris.data <- iris

# Viewing iris dataset structure and attributes
str(iris.data)

# Scatter plot visualising petal width and length grouped by species
ggplot(iris.data, aes(x = Petal.Width, y = Petal.Length, color = Species)) +
  geom_point() +
  theme_classic()

# Boxplot visualising variation in petal width between species
ggplot(iris.data, aes(x = Species, y = Petal.Width, color = Species)) +
  geom_boxplot() +
  theme_classic()

# Building a normalisation function
normalise <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# normalise data frame
iris.norm <- as.data.frame(lapply(iris[1:4], normalise))

# Generating random number
set.seed(1234)

# Randomly generating our training and test sampels with a respective ratio of 2/3 and 1/3
datasample <- sample(2, nrow(iris.norm), replace = TRUE, prob = c(0.67, 0.33))

# Generate training set
iris.training <- iris.norm[datasample == 1, 1:4]

# Generate test set 
iris.test <- iris.norm[datasample == 2, 1:4]

# Generate training labels
irisTraining.labels <- iris[datasample == 1, 5]

# Generate test labels
irisTest.labels <- iris[datasample == 2, 5]

# Building our knn classifier
iris.knn <- knn(train = iris.training, test = iris.test, cl = irisTraining.labels, k = 3)

# creating a dataframe from known (true) test labels
test.labels <- data.frame(irisTest.labels)

# combining predicted and known species classes
class.comparison <- data.frame(iris.knn, test.labels)

# giving appropriate column names
names(class.comparison) <- c("Predicted Species", "Observed Species")

# checking results
class.comparison

# confusion matrix
CrossTable(x = iris.testLabels, y = iris.knn, prop.chisq = FALSE)