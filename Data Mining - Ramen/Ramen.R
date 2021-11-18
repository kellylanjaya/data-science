spicy <- read.csv('ramen-spicy.csv', fileEncoding = 'UTF-8-BOM')
ratings <- read.csv('ramen-ratings.csv')

# 1. Data Visualization
# a. Show the frequency of ramen review based on their rating (stars)

ramen_frequency <- table(ratings$Stars)

plot(
  ramen_frequency,
  type = 'l',
  main = "Number of ramen review based on rating (stars)",
  xlab = "Rate",
  ylab = "Frequency"
)

# b. Show number of ramen review based on their brands, where the count of each review is more than 24.

ramen_review <- table(ratings$Brand)

ramen_review <- ramen_review[table(ratings$Brand) > 24]

pie(
  ramen_review,
  main = "Number of ramen review based on brands"
)

# c.	Show number of ramen review based on Ratings. The ratings are separate into 3 categories:
# o	Top: stars are greater than equals to 4.
# o	Average: stars are greater than equals to 2 and less than 4.
# o	Bad: stars are less than 2.

top <- ratings[ratings$Stars >= 4, "Stars"] 
average <- ratings[ratings$Stars >= 2 & ratings$Stars < 4, "Stars"] 
bad <- ratings[ratings$Stars < 2, "Stars"]  

top_count <- as.matrix(top) 
average_count <- as.matrix(average) 
bad_count <- as.matrix(bad)  

ramen_ratings <- data.frame('Top' = as.vector(nrow(top_count)),
                            'Average' = as.vector(nrow(average_count)),
                            'Bad' = as.vector(nrow(bad_count)))  

ramen_ratings_matrix <- as.matrix(ramen_ratings)  

barplot(   
  ramen_ratings_matrix,   
  main = "Number of ramen review based on Ratings",   
  xlab = "Rating Categories",   
  ylab = "Number of Ramen",
  col = c("red", "green", "blue")
)

# 2. Frequent Pattern Analysis

# a.	Data pre-processing
# •	Remove all Country that is India.

data_preprocessing <- spicy[spicy$Country != 'India', ]

# •	Remove all missing data.

data_preprocessing <- na.omit(data_preprocessing)

# •	Remove all duplicated data for the analysis.

data_preprocessing <- data_preprocessing[!duplicated(data_preprocessing), ]

# b.	Data Transformation
# In this phase, you need to change the data, so it is suitable to be used in the apriori analysis. Prepare the data in terms of the spicy level.

apriori_data <-split(data_preprocessing$Spicy, data_preprocessing$Country)

# c. Data Mining
# •	Show frequent spicy levels using apriori algorithm with minimum support: 0.4 based on the data that have already pre-processed.

library(arules)

frequent_itemset <- apriori(apriori_data, parameter=list(support=0.4, target="frequent itemsets"))

inspect(frequent_itemset)

# •	Show the association rules using minimum confidence: 0.9 based on the frequent spicy levels that resulted from step above.

assoc_rule <- ruleInduction(frequent_itemset, confidence=0.9)

inspect(assoc_rule)

