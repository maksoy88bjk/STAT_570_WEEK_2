# Lecture 3 ---------------------------------------------------------------

rm(list=ls())
# We will be working on Data Transformation for this lecture

# Packages ----------------------------------------------------------------

library(tidyverse)
library(nycflights13)

# Descriptives  -----------------------------------------------------------

short_flights<- flights |>
  filter(air_time < 60)

# Data Check ----------------------------------------------------

head(flights)
str(flights)
glimpse(flights)

# Data Transformations ----------------------------------------------------

### destination MIA olan ayni gundeki gecikmelerin ortalamalarini aldik
flights |> 
  filter(dest == "MIA") |> 
  group_by(year,month, day) |> 
  summarize(
    gecikme = mean(arr_delay, na.rm = TRUE)
  )

### 11 Ocak'daki United Airlines Ucuslari 
jan11 <- flights |> 
  filter(month == 1 & day == 11 & carrier == "UA")

### %in% means and & or 

flights |> 
  filter(month %in% c(1, 2))

### 1 veya 2 

flights |> 
  filter(month == 1 | month == 2)

### arrange
## The arrange() reorder rows, either ascending (default) or descending

flights |> 
  arrange(year, month, day, arr_time)

flights |> 
  arrange(desc(dep_delay))







