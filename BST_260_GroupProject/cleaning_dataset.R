## Datacleaning R
library(dplyr)
library(tidyr)


### make sure not to read strings in as factors
tmp <- read.csv("C:/Users/Keya Joshi/Downloads/Yelp_Brookline_Data_AM_120119.csv", stringsAsFactors = F)
head(tmp)

colnames(dataset)

tmp_use <- tmp %>% select(resturaunt, num_reviews, food_type, description_1, description_2, price, Mon_hrs, Tues_hrs, Wed_hrs, Thu_hrs, Fri_hrs, Sat_hrs, Sun_hrs, full_address, Latitude, Longitude, rating)

tmp_use %>% select(rating) %>% unique()

## fix the rating column 
## first remove all characters after the space
## then change this to numeric 

tmp_use$rating_num <- as.numeric(gsub('([0-9]+) .*', '\\1', tmp_use$rating))
table(tmp_use$rating_num)

tmp_use %>% select(price) %>% unique()
class(tmp_use$price)
summary(as.factor(tmp_use$price))
## this will just take the dollar signs and turn them 1-3
tmp_use$price_num <- as.numeric(as.factor(tmp_use$price))
table(tmp_use$price_num) ## will see that there are 28 1s which correspond to 28 $ restaurants 

## trying to get special characters removed 
tmp_use$restaurant_name <-iconv(tmp_use$resturaunt, to = "ASCII//TRANSLIT")

tmp_use %>% select(food_type) %>% unique()
tmp_use %>% select(description_1) %>% unique()
## remove all the commas 
tmp_use$description <- gsub(",", "", tmp_use$description_1)
## remove all commas 
tmp_use %>% select(description_2) %>% unique()
tmp_use$description2 <- gsub(",", "", tmp_use$description_2)

## do same thing with number of reviews
tmp_use$N_reviews<- as.numeric(gsub('([0-9]+) .*', '\\1', tmp_use$num_reviews))

## select relevant columns now:
cleaned <- tmp_use %>% select(restaurant_name, N_reviews, food_type, description, description2, price_num, Mon_hrs, Tues_hrs, Wed_hrs, Thu_hrs, Fri_hrs, Sat_hrs, Sun_hrs, full_address, Latitude, Longitude, rating_num, price)

## rename columns to be relevant
colnames(cleaned) <- c("Name", "N_reviews", "Type", "Keyword1", "Keyword2", "Price", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Address", "Latitude", "Longitude", "Rating", "Price_factor")

colnames(cleaned)


write.csv(cleaned, "cleaned.csv", row.names = F)
