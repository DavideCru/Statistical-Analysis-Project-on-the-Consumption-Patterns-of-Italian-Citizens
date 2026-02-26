######################################################################################
#                                                                                    #
#     Statistical Analysis Project on the Consumption Patterns of Italian Citizens   #
#                                                                                    #
######################################################################################                                                  


# Check if libraries are already installed, otherwise install them
packages_needed <- c("corrplot", "lmtest", "regclass", "haven", "ggplot2", "precrec", "PRROC", "ROCR")
new_packages <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(packages_needed, library, character.only = TRUE)

# Data description for dataset 'm4'
summary(m4) 

# SIMPLE REGRESSION MODEL
# C: Consumption
# Y.x: Net Available Income

# Create a custom dataset for the variables C and Y.x
data_regression_simple <- na.omit(m4[, c("C", "Y.x")])

# Check the structure of the dataset
colnames(data_regression_simple)
dim(data_regression_simple)
str(data_regression_simple)

# Disable scientific notation on axis labels for easier interpretation
options(scipen = 10)

# Define the simple linear regression model
mod_Rls = lm(formula = C ~ Y.x, data = data_regression_simple)
summary(mod_Rls)

# Scatter plot with regression line
plot(data_regression_simple$Y.x, data_regression_simple$C, 
     xlab = "Net Available Income (€)", 
     ylab = "Consumption (€)", 
     main = "Relationship between Income and Consumption", 
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Semi-transparent points
abline(lm(C ~ Y.x, data = data_regression_simple), col = "red", lwd = 2)     

# Save regression model results to CSV
write.csv(summary(mod_Rls)$coefficients, "results_simple_regression.csv")

# Save the scatter plot as a PNG image
png("scatter_plot.png")
plot(data_regression_simple$Y.x, data_regression_simple$C, 
     xlab = "Net Available Income (€)", 
     ylab = "Consumption (€)", 
     main = "Relationship between Income and Consumption", 
     pch = 16, col = rgb(0, 0, 1, 0.3))  
abline(lm(C ~ Y.x, data = data_regression_simple), col = "red", lwd = 2)
dev.off()

# Calculate the correlation between the two variables
cor(data_regression_simple$Y.x, data_regression_simple$C)

# Calculate model fit statistics
n = nrow(data_regression_simple)  # Number of observations
SSTOT = var(data_regression_simple$Y.x)  # Total sum of squares
SSR = (1 / (n - 1)) * sum((mod_Rls$fitted.values - mean(data_regression_simple$Y.x))^2)  # Regression sum of squares
SSE = var(mod_Rls$residuals)  # Error sum of squares

# Print calculated statistics (Optional, for clarity)
cat("Total Sum of Squares (SSTOT):", SSTOT, "\n")
cat("Regression Sum of Squares (SSR):", SSR, "\n")
cat("Error Sum of Squares (SSE):", SSE, "\n")

# Calculate R² for simple regression
r_squared <- cor(data_regression_simple$Y.x, data_regression_simple$C)^2
cat("R-squared for simple regression:", round(r_squared, 4), "\n")



# MULTIPLE REGRESSION
# C: Consumption
# Y.x: Net Available Income
# NCOMP: Number of family members
# ireg: Region of residence
# CD1: Transportation expenses
# apqual: Employment status
# NPERC: Number of income earners in the household

# Create dataset for multiple regression
data.Rlm <- na.omit(m4[, c("C", "Y.x", "NCOMP", "ireg", "CD1", "apqual", "NPERC")])
colnames(data.Rlm)
dim(data.Rlm)
str(data.Rlm)


# Data transformation for multiple regression: Convert variables to numeric or factors as needed
data.Rlm$Y.x = as.numeric(data.Rlm$Y.x)   # Net available income
data.Rlm$C = as.numeric(data.Rlm$C)         # Consumption
data.Rlm$NCOMP = as.numeric(data.Rlm$NCOMP) # Number of family members
data.Rlm$ireg = as.factor(data.Rlm$ireg)    # Region of residence (categorical variable)
data.Rlm$CD1 = as.numeric(data.Rlm$CD1)     # Transportation expenses
data.Rlm$apqual = as.factor(data.Rlm$apqual) # Employment status (categorical variable)
data.Rlm$NPERC = as.numeric(data.Rlm$NPERC) # Number of income earners in the household


# Check the levels of the regions, setting the reference category to "Lazio" (code "12")
levels(data.Rlm$ireg)  
data.Rlm$ireg <- relevel(data.Rlm$ireg, ref = "12")

# Check the levels of employment status, setting the reference category to "Student" (code "17")
levels(data.Rlm$apqual)  
data.Rlm$apqual <- relevel(data.Rlm$apqual, ref = "17")

# Defining the multiple linear regression model
mod_RM = lm(formula = C ~ Y.x + NCOMP + NPERC + CD1 + ireg + apqual, data = data.Rlm)
summary(mod_RM)

# Correlation between the variables and the correlation matrix
cor(data.Rlm$Y.x + data.Rlm$NCOMP + data.Rlm$CD1 + data.Rlm$NPERC, data.Rlm$C)

# Create a correlation matrix for relevant variables
matr_cor = cor(data.Rlm[, c("C", "Y.x", "NCOMP", "CD1", "NPERC")])
print(matr_cor)

# Visualize the correlation matrix using number and color methods
corrplot(matr_cor, method = "number")
corrplot(matr_cor, method = "color")



# LOGISTIC REGRESSION
# dislav_bin: Experiences of unemployment longer than 6 months
# spesecon: Regular monthly expenses in cash
# asnonoc: Previous employment status before pension/unemployment

# Creazione di un dataset personale per le variabili considerate nel modello
# Transforming the 'dislav' variable into a binary variable
data.log <- na.omit(m4[,c("dislav", "asnonoc", "spesecon")])
data.log$dislav_bin <- ifelse(data.log$dislav == 1, 1, 0)

# Logistic regression model
mod_log = glm(dislav_bin ~ spesecon + asnonoc, family = binomial, data = data.log)
summary(mod_log)

# Predicting probabilities
predict_probs = predict(mod_log, type = "response")

# Converting probabilities to predicted classes (0 or 1) based on a threshold of 0.5
# If the predicted probability is greater than or equal to 0.5, classify as 1 (indicating the event occurred)
# Otherwise, classify as 0 (event did not occur)
predicted_classes = ifelse(predict_probs >= 0.5, 1, 0)

# Saving the results
write.csv(predicted_classes, "predicted_classes_logistic.csv")



# Calculate the confusion matrix
conf.mat = table(Predicted = predicted_classes,
                 Osserved = as.numeric(data.log$dislav) - 1)

print(conf.mat)   # Display the calculated confusion matrix

# Extract values from the confusion matrix
TN = conf.mat[0, 0]  # True Negatives
TP = conf.mat[1, 1]  # True Positives
FN = conf.mat[1, 1]  # False Negatives
FP = conf.mat[1, 0]  # False Positives

# Sensitivity = True Positive Rate
sensitivity = TP / (TP + FN)
cat("Sensitivity (True Positive Rate):", sensitivity, "\n")

# Specificity = True Negative Rate
specificity = TN / (TN + FP)
cat("Specificity (True Negative Rate):", specificity, "\n")

# Accuracy
accuracy = (TP + TN) / (TP + TN + FP + FN)
cat("Accuracy:", accuracy, "\n")

# Calculate and visualize the ROC curve
pred <- prediction(predict_probs, data.log$dislav_bin)
perf <- performance(pred, "tpr", "fpr")

# ROC curve plot with AUC label
plot(perf, col = "blue", lwd = 2, main = "ROC Curve - Model Performance")
abline(a = 0, b = 1, lty = 2, col = "black")

# Calculate and display the AUC
auc_value <- performance(pred, "auc")@y.values[[1]]
polygon(c(0, x_vals, 1), c(0, y_vals, 0), col = rgb(0.1, 0.4, 0.9, 0.1), border = NA)
text(0.6, 0.2, paste("AUC =", round(auc_value, 4)), cex = 1.2)




# CLUSTERING
# C: Consumption
# Y.x: Net Available Income

# Select numeric variables
data_regression_simple <- na.omit(m4[, c("C", "Y.x")])

# K-means clustering on consumption and net income
set.seed(123)
kmeans_result <- kmeans(data_regression_simple, centers = 3)

# Adding clusters to dataset and visualizing
data_regression_simple$cluster <- kmeans_result$cluster
plot(data_regression_simple$C, data_regression_simple$Y.x, 
     col = data_regression_simple$cluster, pch = 16, 
     xlab = "Consumption (€)", ylab = "Net Available Income (€)", 
     main = "K-means Clustering: Consumption vs Income")


# Boxplot for consumption by region
# C: Consumption
# AREA3: Geographical area of residence
boxplot(C ~ AREA3, data = m4, names = c("North", "Center", "South"), 
        xlab = "Geographical Area", ylab = "Consumption (€)", 
        main = "Boxplot of Consumption by Region")

