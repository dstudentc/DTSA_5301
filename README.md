# Data Science as a Field - NYC shooting incidents
Repo contains the markdown file, markdown pdf, and the presentation pdf for the NYC shooting incidents analysis

For this assignment we are exploring historical shooting incidents that occurred in New York City from 2006 - 2020. The data comes from data.gov website and was collected by the New York City police department. 

We will explore the data to determine dangerous boroughs in New York City and also see if we are able to model the number of gun related deaths.

## Datasets:

1) Historical shooting incidents NYC (2006 - 2020); source: data.gov
2) Population data for each NYC borough; source: Census Data - Google


## Data Ingestion

First let's import the library we will be using:

```{r import, message=FALSE}
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)
library(formatR)
library(prophet)
```

```{r setup2, include=FALSE}
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)
```

Now let's grab our data from the gov website:

```{r getting data}
# URL with our data
nypd_url <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"
# Reading in our data, and specifying that strings become factors
shooting_data <- read.csv(nypd_url, stringsAsFactors = TRUE)
# Converting to a tibble
shooting_data <- as_tibble(shooting_data)
```

## Data Exploration

Now let's view the data:

```{r viewing data}
# Looking at our data
head(shooting_data)
# Taking a look at our columns / data types
str(shooting_data)
```

We can see that each shooting incident is recorded per row. We also can see that some columns have the wrong data type
Like date which is coded as a factor type. We will fix this issue a little further down. 

For now, let's take a look at the summary of our data 

```{r viewing data2}
summary(shooting_data)
```

Now we have a good idea of what the data represents, let's start to clean our dataset. 

## Data Cleaning

First, let's see if there is any missing data:

```{r missing data}
missing_data <- shooting_data %>% mutate_all(na_if,"")
data.frame(sapply(missing_data, function(x) sum(is.na(x))))
```

We can see that some columns are missing a lot of data.
let's clean up our data, by removing columns that have a lot of missing data. 
We also have a lot of redundant columns (like Lat and longitude) that won't be used in our analysis so we will remove them:

```{r clean missing}
columns_with_missing_data <- c("LOCATION_DESC", "PERP_AGE_GROUP", "PERP_SEX", "PERP_RACE")
unnecessary_columns <- c("INCIDENT_KEY", "PRECINCT", "VIC_AGE_GROUP", "VIC_SEX", "VIC_RACE", "X_COORD_CD", "Y_COORD_CD", "Latitude", "Longitude", "Lon_Lat", "JURISDICTION_CODE")
# Removing those columns
shooting_data_clean <- shooting_data %>% select(-c(all_of(columns_with_missing_data), all_of(unnecessary_columns)))
```

Now we will clean our date and time columns:

```{r date_clean}
shooting_data_clean <- shooting_data_clean %>% mutate(OCCUR_DATE = mdy(OCCUR_DATE)) %>% mutate(OCCUR_TIME = hms(OCCUR_TIME))
```

We will add year and month columns:

```{r month_clean}
shooting_data_clean <- shooting_data_clean %>% mutate(YEAR = year(OCCUR_DATE)) %>% mutate(MONTH = month(OCCUR_DATE))
```

And finally we will add a count of our shooting incidents, and transform the murder flag to binary values (0, 1). This flag represents the number of shooting related deaths. 


```{r shooting_clean}
shooting_data_clean$INCIDENTS <- 1
# Transforming flag to binary
shooting_data_clean$STATISTICAL_MURDER_FLAG <- as.integer(as.logical(shooting_data_clean$STATISTICAL_MURDER_FLAG))
```

Now let's take a look at our data now after cleaning:

```{r show clean data }
head(shooting_data_clean)
```

## Data Visualization and Analysis

It looks like we have a good clean dataset to work with. Now let's do some data exploration.
Let's create a new dataframe where we group the shooting incidents by year:

```{r group shooting incidents year}
shooting_data_year <- shooting_data_clean %>% group_by(YEAR) %>% summarize(INCIDENTS = sum(INCIDENTS), DEATHS = sum(STATISTICAL_MURDER_FLAG))
shooting_data_year
```

Now that we have this dataset, let's graph the number of incidents over the years:

```{r group shooting plot year}
ggplot(shooting_data_year, aes(x = YEAR, y = INCIDENTS)) + geom_line(size=1) + geom_point(size=3) + ggtitle("NYC Shooting Incidents by Year (2006-2020)")
```

It looks like the number of shooting incidents were decreasing year over year, until we hit 2020. Before starting the analysis I thought 2020 would have much fewer incidents because of COVID (see bias section further down for more elaboration). 

Now that we looked at the incidents over the years, let's create a new data set to see the incidents by NYC borough:

```{r group shooting incidents borough}
# Group by borough
shooting_data_borough <- shooting_data_clean %>% group_by(BORO) %>% summarize(INCIDENTS = sum(INCIDENTS), DEATHS = sum(STATISTICAL_MURDER_FLAG))
shooting_data_borough 
```

It looks like some boroughs have more incidents than others, let's take a look:

```{r group shooting plot borough}
ggplot(shooting_data_borough, aes(x = reorder(BORO, -INCIDENTS), y = INCIDENTS)) + geom_bar(stat="identity", fill="steelblue") + xlab("BOROUGHS") + ggtitle("NYC Shooting Incidents by Borough (2006-2020)")
```

It looks like Brooklyn has the most incidents... But does having the most incident mean that it the most "gun violent" borough in NYC?

I decided to look at the population data of each of the boroughs (information taken from Google). Let's take a look:

```{r pop stats}
# Got population of each borough from Google
population_boroughs <- data.frame(BORO=c("QUEENS", "MANHATTAN", "BRONX", "BROOKLYN", "STATEN ISLAND"), POPULATION = c(2287000, 1632000, 1435000, 2590000, 474893))
population_boroughs
```

So it seems that Brooklyn has the highest population. So let's take a look at the number of incidents by population.
First let's merge our new dataset with our boroughs data, and then divide the number of incidents by population:

```{r merging data}
# Merging our datasets together into boroughs_comnplete.
boroughs_complete <- merge(x=shooting_data_borough, y=population_boroughs, by="BORO")
boroughs_complete$INCIDENTS_VS_POPULATION  <-  percent(boroughs_complete$INCIDENTS / boroughs_complete$POPULATION)
boroughs_complete
```

So it looks like the Bronx has the most number of incidents by population. Let's visualize the data:

```{r merging plot}
ggplot(boroughs_complete, aes(x = reorder(BORO, -INCIDENTS), y = INCIDENTS_VS_POPULATION)) + geom_bar(stat="identity", fill="steelblue") + xlab("BOROUGHS") + ylab("INCIDENTS / POPULATION") + ggtitle("NYC Shooting Incidents/Population by Borough (2006-2020)")
```

So while Brooklyn has the most number of incidents, it seems that the Bronx has more gun incidents / population and is more dangerous. 

## Data Modeling 

Previously our dataset had the number of shooting incidents and deaths. Let's see if we can use the # of shooting incidents to predict how many deaths we will have in NYC per year. We will use a linear model:

```{r modeling}
model <- lm(DEATHS ~ INCIDENTS, data=shooting_data_year)
shooting_data_year_final <- shooting_data_year %>% mutate(PRED = predict(model))
shooting_data_year_final
```
We have a model that predicts the number of deaths, let's visualize the predictions vs the actual death related count


```{r model plot}
shooting_data_year_final %>% ggplot() + geom_point(aes(x=INCIDENTS, y=DEATHS, colour="DEATHS"), color="blue", size=2, show.legend=TRUE) + geom_point(aes(x=INCIDENTS, y=PRED, colour="PRED"), color="red", size=2, show.legend=TRUE) + ggtitle("Modeling the number of shooting related deaths by shooting incidents")
```

It looks like we are able to create a fairly accurate model to represent the number of gun related deaths using the # of gun shooting incidents. 

Let's also try a special Forecasting Library called Prophet that will allow us to predict the number of future gun related incidents in NYC. We first have to manipulate the data so that Prophet is able to read it. 


```{r model prophet manipulation}
# making prophet fit
shooting_data_year_prophet <- shooting_data_year_final %>% select(c(YEAR, INCIDENTS)) 
shooting_data_year_prophet$ds <- as.Date(paste(as.character(shooting_data_year_prophet$YEAR), "01", "01", sep = "-"))
shooting_data_year_prophet <- shooting_data_year_prophet %>% select(c(ds, INCIDENTS)) %>% rename(y = 'INCIDENTS')
shooting_data_year_prophet
```

Now that we have it in the format that Prophet requires, let's format use it to forecast out 10 years into the future to see what will happen with the # of shooting incidents. 

```{r model prophet plot}
# making prophet fit
m <- prophet(shooting_data_year_prophet)
future <- make_future_dataframe(m, periods = 10, freq = "year")
# forecasting out the future
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast, main="Main titles", xlab="Date",  ylab="Number of Shooting Incidents", sub="Sub-title")
```

The black dots are the actual values and the blue line is the prediction. The lower and upper confidence bounds are given by the shaded blue region. It looks like the model took previous years data (before 2020) and continues the pre-COVID trend. 


## Bias Identification

Personal Bias: 
As previously mentioned, one of my personal biases was the belief that there would be less shooting related incidents during 2020 because of Covid. I assumed that people would be locked down at home. From examining the data, that assumption was proven to be false. Covid and potentially other factors (2020 was also a hotly contested election year) actually increased the number of shooting incidents. 

Information Bias: 
Another bias that I noticed would potentially be the time the shooting incident was recorded. Since these observations are human dependent, I wanted to remove it entirely from my dataset. 
I also wanted to remove any factors involving race. There have been several algorithms which ended up being inherently racist.
See this MIT article describing police specific algorithms:
https://www.technologyreview.com/2020/07/17/1005396/predictive-policing-algorithms-racist-dismantled-machine-learning-bias-criminal-justice/

## Conclusion

We cleaned, examined, visualized, analyzed and modeled the data from the NYPD. It looks like the number of shooting incidents has been steadily decreasing year of year, until we hit 2020. Covid (and other factors) seem to have had a negative impact and has lead to an increase in the number of shooting incidents. The Bronx seems to be the most violent borough in NYC with the highest number of shooting incidents / population. Finally, we showed that we are able to model the number of shooting related deaths by using the # of shooting incidents. 


```{r session info}
sessionInfo()
```
