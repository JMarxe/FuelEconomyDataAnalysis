# Fuel Economy Data Analysis (R)

## Introduction

#### This Dataset contains Vehicle Fuel Economy statistics thats provides a reliable basis for comparing vehicles expenses and details. The fuel economy values provided by the EPA are accurate predictions of the fuel economy a typical driver will experience under typical driving conditions and serve as a useful benchmark when contrasting different vehicles. Your fuel economy could, however, differ slightly from the EPA's predictions. Fuel efficiency varies, sometimes significantly, depending on the road, the driver, and other aspects.

#### Fuel Economy Data was retrieved by the Environmental Protection Agency's National Vehicle and Fuel Emissions Laboratory, located in Ann Arbor, Michigan, that conducted a vehicle testing, and vehicle manufacturers, under the agency's supervision, produce fuel economy data.

```{r echo=T, results = 'hide', error=FALSE, warning=FALSE, message=FALSE}

library(tidyverse)
library(tidymodels)
library(caret)
library(broom)
library(modelr)
library(scales)
library(lemon)
library(rpart)
library(rpart.plot)
library(cluster)
library(factoextra)
library(readr)
library(plotly)
library(lubridate)
library(dplyr)
library(DAAG)

fuel <- read_csv("Fuel Economy Data.csv")
view(fuel)

```

<!-- Special Comment (Hidden when Knit) -->

## Part 1: Exploratory Analysis 5 Charts + 1

<!-- 3rd Bar shows an error that says "Gasoline or E85", when it should be only saying "E85"  -->

### 1st Chart

```{r}

fuel %>%
  count(`Fuel Type`) %>%
  arrange(desc(n)) %>%  # Arrange function let us reorder the rows of a tibble in this case in descending order
  head(5) %>% # Head function displays the first n rows present in the input data frame
  ggplot(aes(x = reorder(`Fuel Type`, n), y = n, fill = `Fuel Type`)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  coord_flip() +  # Flip Cartesian coordinates so that Vertical becomes Horizontal and Backwards.
  labs(x = "Fuel Type", y = "Number of Vehicles Count", title = "Most Used Fuel Type", subtitle = "Per Number of Vehicles", caption = "Horizontal Bar Chart represents the Number of Vehicles and Type of Fuel they use")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221848318-97a5122f-0db8-4672-af47-554b7092a55f.png" height="80%" width="80%"/>
<br />

#### In this Visual Chart, we can see that the majority of the vehicles use Regular Fuel, also having a significant difference and being more than twice as big as the 2nd most used (Premium).

### 2nd Chart

```{r}

fuel %>%
  group_by(Make) %>%  # Groups rows by column values in the Data Frame
  summarize(MPG_Average = mean(`City MPG (FT1)`)) %>% # Summarize the data frame into just one value or vector
  arrange(desc(MPG_Average)) %>% # Arrange function let us reorder the rows of a tibble in this case in descending order
  head(10) %>%
  mutate(Make = fct_reorder(Make, MPG_Average)) %>% # fct_reorder for displaying where the factor is mapped to position
  ggplot(aes(y = Make, x= MPG_Average, fill = Make)) +
  geom_col(show.legend = FALSE) +
  labs(x = "MPG Average", y = "Manufacturer", title = "Most Fuel-Efficient Manufacturers", subtitle = "Considering Average MPA (Miles Per Gallon)", captions = "Higher Miles Per Gallon is Better")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221848569-5af5859b-d6a9-4a56-b59e-6059725fd532.png" height="80%" width="80%"/>
<br />

#### This visual displays which the top 10 makes whose vehicles have a high MPG average. Tesla is the highest with CODA Automotive at second, which is interesting because they are both eletric vehicle manufacturers. This could mean that electric vehicles have better MPG than gasoline vehicles.

### 3rd Chart

```{r}
  
fuel %>%
  ggplot(aes(x = `Annual Fuel Cost (FT1)`, y = `Annual Consumption in Barrels (FT1)`)) +
  geom_point(col = "darkolivegreen3")+
  labs(x = "Annual Fuel Cost (Dollars)", y = "Annual Consumption (Barrels)", title = "Correlation between Annual Fuel Cost and Annual Consumption", subtitle = "In Barrels", caption = "Scatterplot that shows the comparison")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221848853-57f7219e-2cae-47d6-8244-24d6671b519c.png" height="80%" width="80%"/>
<br />

#### This visual allow us to see a positive correlation between the Annual Fuel Cost and the Annual Consumption as Fuel is a primary need for transportation.

### 4th Chart

```{r}

annual_fuel_cost <- fuel %>%
  ggplot(aes(x = `Annual Fuel Cost (FT1)`)) +
  geom_histogram(bins = 30, fill = "lightBlue", col = "blue") +
  labs(x = "Annual Fuel Cost", y = "Count of Vehicles", title = "Annual Fuel Cost", subtitle = "Per Vehicle", caption = "This Histogram represents how much are the fuel expenses per vehicle")

ggplotly(annual_fuel_cost) %>% 
  layout(title = list(text = paste0('Annual Fuel Cost', # Code added to be able to show the subtitle on the graph (ggplotly)
                                    '<br>',
                                    '<sup>',
                                    'Per Vehicle')))
  
```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221849308-bbb1063f-01d5-42d2-baf3-06799efcece4.png" height="100%" width="100%"/>
<br />
ㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤㅤThis Histogram represents how much are the fuel expenses per vehicle

#### The histogram displays a simple visual of the annual fuel cost for every vehicle. We can see that the majority of vehicles seem to gravitate towards an annual fuel cost of about 2000 with very only a few outliers surpassing 4000. It can be inferred that most if not all vehicles in this data set have a very economic annual fuel cost.

### 5th Chart

```{r}

fuel %>%
  filter(Year >= 2000) %>%
  group_by(Year) %>%
  count(Year) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_line(col = "maroon2") +
  labs(x = "Years", y = "Unique Model Count", title = "Unique Car Models Manufactured",  subtitle = "From 2000 to above", captions = "Quantity of Models produced each Year")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221850154-47f5d600-e704-43c4-871a-6694da65c73c.png" height="80%" width="80%"/>
<br />

#### The Line Graph represents the amount of unique cars produced after the year 2000.

### 6th Chart

```{r}

fuel %>%
  ggplot(aes(x = `Engine Cylinders`, fill = Drive)) +
  geom_boxplot() +
  labs(x = "Engine Cylinders", y = "Drive Axle Type", fill = "Axle Type", title = "Common configuration of Drive Axle Type", subtitle = "According to the Engine Cylinders", caption = "This Box Plot shows the types of axle and its available Engine Cylinders distribution")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221850404-96c1e80e-2eee-4d32-b73e-43ec3dc23f41.png" height="80%" width="80%"/>
<br />

#### As it is shown in the Graph, the 2 wheel drive axle type and the 4 wheel or all wheel drive allows to have a 4 or 8 engine cylinders in their configuration


## Part 2

### K-Means Clustering

<!-- Missing comments throughout that explain what is being done in the code -->

```{r}

fuel_revised <- fuel %>%
  select(`City MPG (FT1)`, `Annual Fuel Cost (FT1)`, Make, Model) %>%
  filter(Make == "BMW") 

set.seed(1) # Creates reproducible results when writing code that involves creating variables that take on random values.
fuel_cluster <- kmeans(fuel_revised[,1:2], centers = 3) # Unsupervised Non-linear algorithm that cluster data based on similarity or similar groups

fuel_cluster

```

```{r}

options(scipen = 999)
fviz_nbclust(fuel_revised[,1:2], kmeans, method = "wss") +  # wss is Within Sum of Squares from the previous part.
labs(title = "Optimal Number of Cluster")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221850671-d526033e-cf6c-480d-b8bf-99d0a6a9adaa.png" height="80%" width="80%"/>
<br />

```{r}

fuel_cluster_df <- fuel_revised %>%
  mutate(cluster_number = as.character(fuel_cluster$cluster)) # Changes to a factor makes it

fuel_cluster_df %>%
  ggplot(aes( x = `City MPG (FT1)`, y = `Annual Fuel Cost (FT1)`, color = cluster_number))+
  geom_point() +
  labs(title = "Fuel Cluster for BMW", color = "Cluster №")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221850869-f0134595-ece8-43b1-8106-f78b1ab441f1.png" height="80%" width="80%"/>
<br />

```{r}

fuel_cluster_df <- fuel_revised%>%
  mutate(cluster_number = as.character(fuel_cluster$cluster))

ggplot() +
  geom_point(data = fuel_cluster_df, mapping = aes( x = `City MPG (FT1)`, y = `Annual Fuel Cost (FT1)`, color = cluster_number)) +
  geom_point(mapping = aes(x = fuel_cluster$centers[, 1],
                                  y = fuel_cluster$centers[, 2]), color = "red", size = 5) +
  labs(title = "Centroids for BMW Fuel Clusters", color = "Cluster №")

```

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221851024-b42deefb-5e1c-469e-b334-8bcde43aff96.png" height="80%" width="80%"/>
<br />

#### We decided to create a k means cluster for BMW vehicles because we wanted to understand the pattern between city MPG and annual fuel cost for a specific make and see if we could find something useful if we were to create customer segmentations. We at first tried using 3 clusters as it had a 78% but after creating our elbow plot we decided to stick with 4 clusters since it had a higher percentage of 84%. It was a notable difference in internal cohesion that we decided to keep considering our clusters of 305 and 343 are the only ones truly tightly packed. We managed to minimize the sum of squared distances as you can see our centroids are closely packed together with only 3 notable outliers. Based on this analysis we feel that for a make like BMW, this car would be best suitable for high income individuals because annual fuel cost has a trend going upwards with city MPG not going past 50, excluding the 3 outliers. This could mean that a lower MPG could equate to a higher annual fuel cost which would make sense since that would mean that your vehicle does not get a high number of miles per gallon. We feel that that applying this analysis to other types of vehicle makes we could find patterns to see if they are economic or costly.

## Option 1

### Highway MPG Model and Prediction

```{r}

fuel_updated <- fuel %>%
  select( `Highway MPG (FT1)`,`City MPG (FT1)`, `Engine Cylinders`, `Engine Displacement`, `Combined MPG (FT1)`) %>%
  drop_na()

## Partition our data
set.seed(1) # Setting the random number seed's initial value for the random-number functions
fuel_split <- initial_split(fuel_updated, prop = 0.70) # Specifying that our training will have 70%, with the remaining going to testing (we do that below)
fuel_training <- training(fuel_split)
fuel_testing <- testing(fuel_split)

nrow(fuel_training)
nrow(fuel_testing)
nrow(fuel_training) + nrow(fuel_testing) # Gives the total count of the rows from the original data

# Linear model
highway_model <- lm (`Highway MPG (FT1)` ~ `Engine Cylinders` + `Engine Displacement` + `Combined MPG (FT1)`, data = fuel_training)
   
summary( highway_model)

# Visual
fuel_training %>%
  ggplot(aes( x = highway_model$residuals)) +
  geom_histogram(bins = 30, fill = "olivedrab2", col = "olivedrab3") +
  labs(x = "Residuals", y = "Count", title = "Highway MPG Prediction")

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221851228-14b8d0d4-09ec-430c-9b14-1257617146f0.png" height="80%" width="80%"/>
<br />

# Prediction Model
highway_prediction <- as.data.frame(predict(highway_model, new_data = fuel_testing))

```

## City MPG Prediction

```{r}

fuel_updated <- fuel %>%
  select( `Highway MPG (FT1)`,`City MPG (FT1)`, `Engine Cylinders`, `Engine Displacement`, `Combined MPG (FT1)`)%>%
  drop_na()

## Partition our data
set.seed(1) # Setting the random number seed's initial value for the random-number functions
fuel_split <- initial_split(fuel_updated, prop = 0.70) # Specifying that our training will have 70%, with the remaining going to testing (we do that below)
fuel_training <- training(fuel_split)
fuel_testing <- testing(fuel_split)

# Linear model
city_model <- lm (`City MPG (FT1)` ~ `Engine Cylinders` + `Engine Displacement` + `Combined MPG (FT1)`, data = fuel_training)
   
summary(city_model)

# Visual
fuel_training %>%
  ggplot(aes(x = city_model$residuals)) +
  geom_histogram(bins = 30, fill = "lightgoldenrod", col = "lightgoldenrod3")+
  labs(x = "Residuals", y = "Count", title = "City MPG Prediction")

<p align="center">
<img src="https://user-images.githubusercontent.com/114123232/221851480-1398d129-a28e-4f74-afdb-7c7ccca2ac82.png" height="80%" width="80%"/>
<br />

# Prediction Model
city_prediction <- as.data.frame(predict(city_model, new_data = fuel_testing))
view(city_prediction)

```

### Based on the two models created(highway_model and city_model), the three columns : Engine Cylinders, Engine Displacement and Combined Displacement are statiscally significant to predict the MPG for the highway or the city. The models are statistically significant with both variables having values close to zero. The proportion of variation is equal to R-squared which in this case are 0.9408 and  0.9717. This means that there is a moderate relationship between the two variables.
