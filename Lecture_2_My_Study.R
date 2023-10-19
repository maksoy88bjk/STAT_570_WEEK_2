# Lecture 3 ---------------------------------------------------------------

rm(list=ls())
# We will be working on Data Transformation for this lecture

# Packages ----------------------------------------------------------------

library(tidyverse)
library(nycflights13)

# Descriptives  -----------------------------------------------------------

short_flights<- flights |>
  filter(air_time < 60)
edsdsds