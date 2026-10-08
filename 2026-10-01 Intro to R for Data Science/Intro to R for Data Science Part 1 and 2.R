##### INTRODUCTION TO R FOR DATA SCIENCE #####
### October 1, 2026
### Presented by Keiko Bridwell

##### SETTING UP YOUR R SESSION #####

# install packages (only need to do one time)
# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")

# load packages you will be using in this session (need to do every time you open R)
library(readr)
library(dplyr)

# check your working directory
getwd()
# set your working directory to the folder your data is located in:
# Session > Set working directory > Choose directory

# read in the file and assign it to a variable
movies <- read_csv("movies2013.csv")



##### EXPLORING THE DATA #####

# get an idea of what the data looks like (dimensions, list of variables, their types, preview of content)
movies
glimpse(movies)

# look at the whole dataset in a different window
View(movies)

# see summary statistics for individual variables
summary(movies)
summary(movies$year)
summary(movies$genre5)

# convert categorical variables to factors
movies$genre5 <- factor(movies$genre5)
movies$rated <- factor(movies$rated, levels = c("G","PG","PG-13","R","NC-17"), ordered = TRUE)

# convert date variables to date
movies$release_date <- as.Date(movies$release_date, "%m/%d/%Y")

# convert dollar amounts to millions of dollars
movies$budget_M <- movies$budget/1000000
movies$us_gross_M <- movies$us_gross/1000000
movies$int_gross_M <- movies$int_gross/1000000

# look at how the summary has changed
summary(movies)


##### SUMMARIZING USING DPLYR

# introducing: the pipe!
# Mac users: Command + Shift + M
# Windows users: Ctrl + Shift + M
# two different versions: %>% and |>

# the two lines below work the same way, but the second one with the pipe can be more useful 
# if you need to perform a lot of actions on a variable in a row
count(movies, rated)
movies |> count(rated)
movies |> count(genre5)

# can count things that aren't factors
movies |> count(director)
movies |> count(year)

# summarize() for numerical variables
movies |> summarize(mean(us_gross_M))
movies |> summarize(median(us_gross_M))
movies |> summarize(min(us_gross_M), max(us_gross_M))

# group_by() is used with summarize() to report the summarized value for each level of a categorical variable
movies |> group_by(genre5) |> summarize(mean(budget_M))
movies |> group_by(genre5) |> summarize(mean(imdb_rating))
movies |> group_by(rated) |> summarize(mean(imdb_rating))


##### PART 2 #####
##### MANIPULATING THE DATA WITH DPLYR #####

# filter() extracts rows that meet logical criteria
movies |> filter(imdb_rating < 4)

# can't see titles very well, so let's pick out just a few columns to look at with select()
movies |> filter(imdb_rating < 4) |> select(title, year, imdb_rating)

# trying some other filters
movies |> filter(runtime > 3) |> select(title, year, runtime, imdb_rating)
movies |> filter(genre5 == "Action")
movies |> filter(genre5 != "Action")

# filtering by multiple variables
movies |> filter(genre5 == "Action", us_gross_M > 1000) # over 1 billion
movies |> filter(genre5 == "Action", us_gross_M > 500)

# arrange(): order rows from least to greatest in a column
# by year
movies |> 
  filter(genre5 == "Action", us_gross_M > 500) |> 
  arrange(year) |> 
  select(title, year, us_gross_M)

# by us_gross_M, least to greatest
movies |> 
  filter(genre5 == "Action", us_gross_M > 500) |> 
  arrange(us_gross_M) |> 
  select(title, year, us_gross_M)

# by us_gross_M, greatest to least
movies |> 
  filter(genre5 == "Action", us_gross_M > 500) |> 
  arrange(desc(us_gross)) |> 
  select(title, year, us_gross_M)

# filtering with the purpose of summarizing
# count the movies made in the 1990s
movies |> filter(year >= 1990, year < 2000) |> count()

# count the 90s movies by rating
movies |> filter(year >= 1990, year < 2000) |> count(rated)

# sum of the amount they made internationally
movies |> 
  filter(year >= 1990, year < 2000) |> 
  summarize(sum(int_gross))

# average IMDB rating and runtime
movies |> 
  filter(year >= 1990, year < 2000) |> 
  summarize(mean(imdb_rating), mean(runtime))

# average IMDB rating and runtime, with column titles
movies |> 
  filter(year >= 1990, year < 2000) |> 
  summarize(mean_rating = mean(imdb_rating), 
            mean_runtime = mean(runtime))

# average IMDB rating and runtime by genre
movies |> 
  filter(year >= 1990, year < 2000) |> 
  group_by(genre5) |> 
  summarize(mean_rating = mean(imdb_rating), 
            mean_runtime = mean(runtime))


##### CREATING NEW COLUMNS #####

# calculate runtime in minutes
movies <- movies |> 
  mutate(runtime_min = round(runtime*60,0))

# create a profit variable
movies <- movies |> 
  mutate(profit = int_gross - budget,
         profit_M = round(profit / 1000000,3)) # or, profit_M = profit/1000000

# create a variable using mutate(), then pipe it directly into summarize()
movies |> 
  mutate(men_lines100 = men_lines*100) |> 
  group_by(genre5) |> 
  summarize(mean(men_lines100))

# create a categorical variable for decade
movies_plus <- movies_plus |> mutate(decade = case_when(year >= 1970 & year <= 1979 ~ "1970s",
                                                        year >= 1980 & year <= 1989 ~ "1980s",
                                                        year >= 1990 & year <= 1999 ~ "1990s",
                                                        year >= 2000 & year <= 2009 ~ "2000s",
                                                        year >= 2010 & year <= 2019 ~ "2010s"))

# the fast way to round to the previous 10s without using case_when()
movies <- movies |> 
  mutate(decade2 = floor(year/10)*10)

# number of movies per decade
movies |> count(decade)

# average and total profit by decade
movies |> group_by(decade) |> summarize(mean(profit_M), sum(profit_M))


##### BONUS: GGPLOT #####

# scatterplot of profit by date, with line of best fit
ggplot(movies, aes(x = release_date, y = profit_M)) +
  geom_point(color = "purple", alpha = 0.25) + 
  geom_smooth(method = "lm") +
  xlab("Release Date") + ylab("Profit (millions)") +
  ggtitle("Movies Profit 1970-2013") +
  theme_minimal() +
  theme()

# bar chart of average profit by genre, with each bar a different color
ggplot(movies, aes(x = genre5, y = profit_M, fill = genre5)) +
  geom_bar(stat = "summary", fun=mean) +
  xlab("Genre") + ylab("Profit (millions)") +
  ggtitle("Movies Profit by Genre") +
  theme_minimal() +
  theme(legend.position = "none")

# preset themes: theme_bw, theme_classic, theme_gray/grey, theme_light, theme_linedraw, theme_minimal, theme_void

# Why is the "Other" category so profitable?
movies |> 
  filter(genre5 == "Other") |> 
  select(title, year, profit_M)