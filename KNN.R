library(class)

#loading dataset from drive 
iris<- read.csv('D:/wine.csv',header = TRUE,sep = ',')
iris

#EUCLIDEAN

# Split the dataset into training and test sets
set.seed(123)  # Set seed for reproducibility
indices <- sample(1:nrow(iris), nrow(iris) * 0.7)  # 70% for training, 30% for testing
train_data <- iris[indices, 1:4]
train_labels <- iris[indices, 5]
test_data <- iris[-indices, 1:4]
true_labels <- iris[-indices, 5]

# Function to calculate Euclidean distance between two points
euclidean_distance <- function(point1, point2) {
  sqrt(sum((point1 - point2)^2))
}

# Function to perform KNN classification
knn <- function(train_data, test_data, train_labels, k) {
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  predictions <- vector("character", length = n_test)
  
  for (i in 1:n_test) {
    distances <- vector("numeric", length = n_train)
    
    for (j in 1:n_train) {
      distances[j] <- euclidean_distance(test_data[i, ], train_data[j, ])
    }
    
    # Sort the distances and get the indices of the k nearest neighbors
    nearest_neighbors <- order(distances)[1:k]
    
    # Get the labels of the k nearest neighbors
    nearest_labels <- train_labels[nearest_neighbors]
    
    # Find the most common label among the nearest neighbors
    predictions[i] <- names(which.max(table(nearest_labels)))
  }
  
  return(predictions)
}

# Function to calculate accuracy
calculate_accuracy <- function(predictions, true_labels) {
  correct <- sum(predictions == true_labels)
  total <- length(true_labels)
  accuracy <- correct / total
  return(accuracy)
}

# Perform KNN classification with k = 5 using Euclidean distance
k <- 7
knn_predictions <- knn(train_data, test_data, train_labels, k)

# Calculate accuracy
accuracy <- calculate_accuracy(knn_predictions, true_labels)
print(paste("Accuracy:", accuracy))







#MANHATTAN


# Split the dataset into training and test sets
set.seed(123)  # Set seed for reproducibility
indices <- sample(1:nrow(iris), nrow(iris) * 0.7)  # 70% for training, 30% for testing
train_data <- iris[indices, 1:4]
train_labels <- iris[indices, 5]
test_data <- iris[-indices, 1:4]
true_labels <- iris[-indices, 5]

# Function to calculate Manhattan distance between two points
manhattan_distance <- function(point1, point2) {
  sum(abs(point1 - point2))
}

# Function to perform KNN classification
knn <- function(train_data, test_data, train_labels, k) {
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  predictions <- vector("character", length = n_test)
  
  for (i in 1:n_test) {
    distances <- vector("numeric", length = n_train)
    
    for (j in 1:n_train) {
      distances[j] <- manhattan_distance(test_data[i, ], train_data[j, ])
    }
    
    # Sort the distances and get the indices of the k nearest neighbors
    nearest_neighbors <- order(distances)[1:k]
    
    # Get the labels of the k nearest neighbors
    nearest_labels <- train_labels[nearest_neighbors]
    
    # Find the most common label among the nearest neighbors
    predictions[i] <- names(which.max(table(nearest_labels)))
  }
  
  return(predictions)
}

# Function to calculate accuracy
calculate_accuracy <- function(predictions, true_labels) {
  correct <- sum(predictions == true_labels)
  total <- length(true_labels)
  accuracy <- correct / total
  return(accuracy)
}

# Perform KNN classification with k = 5 using Manhattan distance
k <- 5
knn_predictions <- knn(train_data, test_data, train_labels, k)

# Calculate accuracy
accuracy <- calculate_accuracy(knn_predictions, true_labels)
print(paste("Accuracy:", accuracy))


#MAXIMUM DIMENSION DISTANCE


# Split the dataset into training and test sets
set.seed(123)  # Set seed for reproducibility
indices <- sample(1:nrow(iris), nrow(iris) * 0.7)  # 70% for training, 30% for testing
train_data <- iris[indices, 1:4]
train_labels <- iris[indices, 5]
test_data <- iris[-indices, 1:4]
true_labels <- iris[-indices, 5]

# Function to calculate Maximum Dimension distance between two points
max_dimension_distance <- function(point1, point2) {
  max(abs(point1 - point2))
}

# Function to perform KNN classification
knn <- function(train_data, test_data, train_labels, k) {
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  predictions <- vector("character", length = n_test)
  
  for (i in 1:n_test) {
    distances <- vector("numeric", length = n_train)
    
    for (j in 1:n_train) {
      distances[j] <- max_dimension_distance(test_data[i, ], train_data[j, ])
    }
    
    # Sort the distances and get the indices of the k nearest neighbors
    nearest_neighbors <- order(distances)[1:k]
    
    # Get the labels of the k nearest neighbors
    nearest_labels <- train_labels[nearest_neighbors]
    
    # Find the most common label among the nearest neighbors
    predictions[i] <- names(which.max(table(nearest_labels)))
  }
  
  return(predictions)
}

# Function to calculate accuracy
calculate_accuracy <- function(predictions, true_labels) {
  correct <- sum(predictions == true_labels)
  total <- length(true_labels)
  accuracy <- correct / total
  return(accuracy)
}

# Perform KNN classification with k = 5 using Maximum Dimension distance
k <- 5
knn_predictions <- knn(train_data, test_data, train_labels, k)

# Calculate accuracy
accuracy <- calculate_accuracy(knn_predictions, true_labels)
print(paste("Accuracy:", accuracy))

