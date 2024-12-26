# Install necessary libraries
if (!requireNamespace("tensorflow", quietly = TRUE)) install.packages("tensorflow")
if (!requireNamespace("keras", quietly = TRUE)) install.packages("keras")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")

install.packages("openxlsx")
library(openxlsx)
library(tensorflow)
library(keras)
library(dplyr)
library(caret)

# Load the dataset
file_path <- "C:\\Users\\mh319\\Desktop\\VS Code\\Github Repo\\Protien Regression Model\\Data\\Proteomedb Serratia sp. FGI94.xlsx"
dataset <- read.xlsx(file_path)

# Preprocessing
# One-hot encode the protein Name
dataset <- dataset %>% 
  mutate(Name = as.factor(Name)) %>%
  mutate(Name_Encoded = as.integer(Name))

# Extract additional features from sequences
dataset <- dataset %>%
  mutate(
    AA_Length = nchar(AA_Sequence), 
    Nt_Length = nchar(Nt_Sequence)
  )

# Normalize numerical features
numerical_cols <- c("No_of_residues", "Mol_weight", "AA_Length", "Nt_Length")
preProc <- preProcess(dataset[, numerical_cols], method = c("center", "scale"))
normalized_data <- predict(preProc, dataset[, numerical_cols])

# Combine preprocessed data
model_data <- cbind(normalized_data, Name_Encoded = dataset$Name_Encoded)

# Split data into training and testing
set.seed(123)
trainIndex <- createDataPartition(model_data$Mol_weight, p = 0.8, list = FALSE)
train_data <- model_data[trainIndex, ]
test_data <- model_data[-trainIndex, ]

# Separate features and target
train_x <- train_data %>% select(-Mol_weight) %>% as.matrix()
train_y <- train_data$Mol_weight

test_x <- test_data %>% select(-Mol_weight) %>% as.matrix()
test_y <- test_data$Mol_weight


install.packages("reticulate")
library(reticulate)

reticulate::install_python(version = "3.8")
reticulate::virtualenv_create("r-reticulate")
reticulate::virtualenv_install("r-reticulate", packages = c("tensorflow", "keras"))


# Define the regression model
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)  # Output layer for regression

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "mse",  # Mean Squared Error for regression
  metrics = c("mae")  # Mean Absolute Error
)

# Train the model
history <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model on test data
model %>% evaluate(test_x, test_y)

# Make predictions
predictions <- model %>% predict(test_x)

# View predictions vs actual
results <- data.frame(Actual = test_y, Predicted = as.vector(predictions))
print(results)
