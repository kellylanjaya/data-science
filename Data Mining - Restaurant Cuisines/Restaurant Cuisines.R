cuisine <- read.csv("chefmozcuisine.csv", fileEncoding = 'UTF-8-BOM')
geoplaces <- read.csv("geoplaces2.csv")
rating <- read.csv("rating_final.csv", fileEncoding = 'UTF-8-BOM')

na.omit(cuisine)
na.omit(geoplaces)
na.omit(rating)

# 1. Data Visualization

# a. Show cuisines distribution where the cuisines are provided by more than 5 restaurants.

cuisine_data <- merge(cuisine, geoplaces, by = "placeID")

cuisine_distribution <- table(cuisine_data$Rcuisine)

cuisine_distribution <- cuisine_distribution[cuisine_distribution > 5]

pie(
  cuisine_distribution,
  main = "Cuisines Distribution"
)

# b. Show the frequency of restaurant cuisines count.

cuisine_count <- table(cuisine_data$placeID)

hist(
  cuisine_count,
  main = "Cuisines Count Frequency based on Restaurant",
  col = "lightblue",
  xlab = "Cuisines Count",
  ylab = "Frequency"
)

# c. Show restaurant state distribution that the average rating is above 1.2.

data_rating <- merge(rating, geoplaces,by="placeID")
data_rating["average_rating"] <- (data_rating$rating + data_rating$food_rating+ data_rating$service_rating) / 3
data_rating <- data_rating[data_rating$average_rating > 1.2,]

#data cleaning
data_rating$state <- tolower(data_rating$state)
data_rating$state <- ifelse(data_rating$state=="s.l.p.", "slp",
                            ifelse(data_rating$state=="san luis potos","slp",
                                   ifelse(data_rating$state=="san luis potosi","slp", data_rating$state)))

data_3 <- table(data_rating$average_rating, data_rating$state)

barplot(
  data_3, 
  beside = TRUE, 
  col= c("red","blue","pink"),
  main="Average Ratings Distribution\nbased on the State of Restaurant"
)

legend(
  "top",
  legend = row.names(data_3), 
  fill = c("red","blue","pink"),
  cex = 0.8
)

# a.	Data Preprocessing
# In the Data Preprocessing phase, there are some data that can’t be used for the further analysis. Do the following task to cleanse the data:
# •	Remove all restaurants that is a franchise restaurant.

data_preprocessing <- cuisine_data[cuisine_data$franchise=='f', ]

# •	Remove all restaurants that provides other services in the restaurant.

data_preprocessing <- data_preprocessing[data_preprocessing$other_services == 'none', ]

# •	Remove all restaurants that the country is not defined.

data_preprocessing <- data_preprocessing[data_preprocessing$country != '?', ]

# •	Replace underscore (‘_’) in the cuisine name to space (‘ ’).
data_preprocessing$Rcuisine <- gsub("_", " ", data_preprocessing$Rcuisine)

# b. Data Transformation
# In this phase, you need to change the data, so it is suitable to be used in the Apriori analysis. Prepared the cuisines data in term of the cuisine name.

apriori_analysis <- split(data_preprocessing$Rcuisine, data_preprocessing$placeID)

# c. Data Mining
# •	Show frequent cuisines using Apriori algorithm with minimum support: 0.008 based on the data that have already pre-processed.

library(arules)

frequent_itemset <- apriori(apriori_analysis, parameter = list(support = 0.008, target = "frequent itemsets"))

inspect(frequent_itemset)

# •	Show the association rules using minimum confidence: 0.8 based on the frequent cuisines that resulted from step above.

assoc_rule <- ruleInduction(frequent_itemset, confidence=0.8)

inspect(assoc_rule)

