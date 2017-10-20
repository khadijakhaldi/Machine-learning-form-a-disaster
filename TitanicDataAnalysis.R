# Copyright 2012 Dave Langer
#    
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.




# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
#by creating an oher df = test+ survived column
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$survived <- as.factor(data.combined$survived)
data.combined$pclass <- as.factor(data.combined$pclass)

# Take a look at gross survival rates
table(data.combined$survived)


# Distribution across classes
table(data.combined$pclass)


# Load up ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higer rate
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


# Examine the first few names in the training data set
head(as.character(train$name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))


# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
#which => from data.combined i want the name column where the name is duplicated
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]
#so as you can see the 4th persons are different so there's no problem

# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g., sibsp)?
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]


# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(data.combined$sex == "male"), ]
males[1:5,]

# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.=> how 'title' is related to `Survived` and `Pclass`
#So we need to add an ther column => Title !!!!


# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

#Loop over all the rows
#ech time  call extractTitle by giving it the name of the passenger in  row i
#The results of the extractTitle function add it to titles using c function

titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)

# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")






