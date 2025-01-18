
# install.packages
install.packages("caTools")
install.packages("ggplot2")
install.packages('rpart')
install.packages('rpart.plot')



# Load necessary libraries
library(caTools)
library(ggplot2)
library(class)
library(rpart)
library(rpart.plot)


# Load the data
data <- read.csv(file.choose(), header=T)
View(data)

# Check the structure of the data
str(data)



###############################################################################################
##Preprocessing Dataset

# Convert numerical columns to numeric if necessary
data$bmi <- as.numeric(data$bmi)
data$age <- as.numeric(data$age)
data$avg_glucose_level <- as.numeric(data$avg_glucose_level)
data$hypertension <- as.numeric(data$hypertension)
data$heart_disease <- as.numeric(data$heart_disease)

## NA values Handling-------------------------------

# Replace NA values in the dataset with median for numeric columns
data$bmi[is.na(data$bmi)] <- median(data$bmi, na.rm = TRUE)
data$hypertension[is.na(data$hypertension)] <- median(data$hypertension, na.rm = TRUE)
data$heart_disease[is.na(data$heart_disease)] <- median(data$heart_disease, na.rm = TRUE)

# Convert "Unknown" in smoking_status to mode
mode_value <- names(sort(table(data$smoking_status), decreasing = TRUE))[1]
data$smoking_status[data$smoking_status == "Unknown"] <- mode_value

## Clean Outliers-----------------

boxplot(data$bmi, 
        main = "Boxplot of BMI", 
        ylab = "BMI", 
        col = "lightblue")

# Identify outliers
outliers <- boxplot(data$bmi, plot=FALSE)$out
outliers

# Show rows with outliers in bmi
data[which(data$bmi %in% outliers),]

# Remove rows with outliers in bmi
data <- data[-which(data$bmi %in% outliers),]

# Generate boxplot for filtered data, or show warning if empty
if (nrow(data) > 0) {
  boxplot(data$bmi,
          main = "Boxplot of BMI", 
          ylab = "BMI", 
          col = "blue")
} else {
  warning("Filtered data is empty. No boxplot to display.")
}
boxplot(data$age, 
        main = "Boxplot of AGE", 
        ylab = "Age", 
        col = "lightgreen")

# Converting to levels (strings -> factors)
data$stroke <- as.factor(data$stroke)

## Split data into training and testing sets

# Set a seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
split <- sample.split(data$stroke, SplitRatio = 0.7)
training_set <- subset(data, split == TRUE)
testing_set <- subset(data, split == FALSE)

# Preview the dataset
View(training_set)





###############################################################################################
##Logistic Regression

# Visualize the relationship between Age and Stroke occurrence
ggplot(training_set, aes(x = age, y = stroke)) + 
  geom_jitter(height = .05, alpha = .1) + 
  labs(title = "Scatter Plot of Age vs Stroke Occurrence",
       x = "Age",
       y = "Stroke (0 = No, 1 = Yes)")

# Creating a logistic regression model using age, bmi, and avg_glucose_level
model <- glm(stroke ~ age + bmi + avg_glucose_level, data = training_set, family = "binomial")
summary(model)

# Visualization of logistic regression fit    
ggplot(training_set, aes(x = age, y = stroke)) + 
  geom_jitter(height = .05, alpha = .1) + 
  geom_smooth(method = 'glm', method.args = list(family = "binomial")) +
  labs(title = "Logistic Regression Fit for Age vs Stroke",
       x = "Age",
       y = "Stroke (0 = No, 1 = Yes)")

# Train the logistic regression model
lg_model <- glm(stroke ~ age + bmi + avg_glucose_level, data = training_set, family = "binomial")
summary(lg_model)

# Make predictions on the test set
predictions <- predict(lg_model, newdata = testing_set, type = "response")

# Convert prediction probabilities to class labels
predicted_classes <- ifelse(predictions > 0.5, "stroke", "No Stroke")
head(predicted_classes)

# Create a confusion matrix
conf_matrix <- table(testing_set$stroke, predicted_classes)
print(conf_matrix)

# Calculate the accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy(logistic regression):", accuracy))




###############################################################################################
##Decision Tree

# Build the decision tree using relevant features

tree <-rpart(stroke  ~.,testing_set,method="class")
# Summary of the decision tree model
summary(tree)


#plotiing the decision tree
r= rpart.plot(tree)
#extra =3 (nodes)
prp(tree,extra=3)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy(decision tree):", accuracy))





###############################################################################################
##Knn

# Feature scaling 
train_scale <- scale(training_set[, c("age", "avg_glucose_level", "bmi")])
test_scale <- scale(testing_set[, c("age", "avg_glucose_level", "bmi")])

# Determine the optimal value of k
k <- sqrt(NROW(training_set))

k

# Apply k-NN algorithm 
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = training_set$stroke,
                      k = k)

# Create a confusion matrix to evaluate the model's performance
cm <- table(testing_set$stroke, classifier_knn, dnn = c("Actual", "Predicted"))
print("Confusion Matrix:")
print(cm)

# Calculate Misclassification Error and Accuracy
misClassError <- mean(classifier_knn != testing_set$stroke)
accuracy <- 1 - misClassError
print(paste("Accuracy(KNN) =", accuracy))





###############################################################################################
##k means 

# Perform k-means clustering (exclude non-numeric or irrelevant columns)
results <- kmeans(training_set[, c("age", "avg_glucose_level")], 3)

# Add the cluster assignments to the dataset
training_set$Cluster <- as.factor(results$cluster)  

# Plot age vs avg_glucose_level colored by cluster
plot1 <- ggplot(training_set, aes(x = age, y = avg_glucose_level, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Age vs Average Glucose Level - Clustered",
    x = "Age",
    y = "Average Glucose Level"
  ) +
  theme_minimal()

print(plot1)




# Plot age vs BMI colored by cluster
plot2 <- ggplot(training_set, aes(x = age, y = bmi, color = as.factor(stroke))) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Age vs BMI",
    x = "Age",
    y = "BMI",
    color = "Stroke"
  ) +
  theme_minimal()
  
print(plot2)



