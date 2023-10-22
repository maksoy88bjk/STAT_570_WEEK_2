# Week-3 ------------------------------------------------------------------
rm(list = ls())


# Packages ----------------------------------------------------------------

library(tidyverse)
library(nycflights13) 

# Descriptives ------------------------------------------------------------

## library(styler) r kodu düzenlemek için bir kütüphane styler:::style_active_file()
## |> ctrl + shift + m    *** shortcut
# ctrl + shift + r farklı descriptives yaratıyoruz boylece

short_flights <- flights |> filter(air_time<60)

# Filtering Exmple METU Class------------------------------------------------------------

flights|>
  filter(dest=="IAH")|>
  group_by(year,month,day)|>
  summarize(n=n(),
            delay=mean(arr_delay,na.rm=TRUE))|>
  filter(n>10)

flights|>
  filter(carrier=="UA",dest%in%c("IAH","HOU"),sched_dep_time>0900,sched_arr_time<2000)|>
  group_by(flight)|>
  summarize(
    delay=mean(arr_delay,na.rm=TRUE),cancelled=sum(is.na(arr_delay)),n=n())|>
  filter(n>10)

# Data Transformation -----------------------------------------------------
head(flights); str(flights)
glimpse(flights)

filter_flight_example<-
  flights |> 
  filter(dest == "IND") |> 
  group_by(year,month,day)|>
  summarize(
    arr_delay = mean(arr_delay, na.rm=TRUE)
  )

# filter ------------------------------------------------------------------

jan1<- flights |> 
  filter(month == 1 & day ==1)

flights |> 
  filter(month == 1 | month == 2)

### %in% equal and or equal

flights |> 
  filter(month %in% c(1,2))

# arrange -----------------------------------------------------------------
flights |> 
  arrange(year,month,day, dep_time)

# distinct unique yapmak icin cok daha hizli ayni zamanda
flights |> 
  distinct(origin, dest, .keep_all = TRUE)

flights |> 
  distinct(origin, dest, sort = TRUE)

# mutate()
##before yazınca dataframe'in basina atti aynisini after da yapabiliriz, 1 de zorunludegil
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

#select
flights |> 
  select(!year:day)

flights |> 
  select(where(is.character))

# starts_with ("abc")
# end_with ("ad")
# contains("")

# var olan sutunu baska isimle atama
flights |> 
  select(tail_num = tailnum)

flights |> 
  rename(tail_num = tailnum)

flights |> 
  relocate(year:dep_time, .after= time_hour)

flights |> 
  relocate(starts_with("arr"), .before = dep_time)

###
table1
table2

table1 |> 
  mutate(rate = cases/population*10000)

table1 |> 
  group_by(year) |> 
  summarize(total_cases = sum(cases))

ggplot(table1, aes(year,cases)) + 
  geom_line(aes(group= country), color ="grey50") + 
  geom_point(aes(color= country, shape = country)) +
  scale_x_continuous(breaks = c(1999,2000)) + theme_classic()

# Lengthening Data --------------------------------------------------------

##bilboard data sets https://www.billboard.com/ veriler buradan cekilmis; top 100'e giren sarkılar
view(billboard)

billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )

billboard_longer<- billboard |> 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )


billboard_longer |> 
  ggplot(aes(week, rank, group=track)) + 
  geom_line(alpha=0.25) + 
  scale_y_reverse()

df <- tribble(
  ~id,~bp1, ~bp2,
  "A",100,122,
  "B",140,115,
  "C", 120,125
)

df_longer<- 
  df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to =  "measurument",
    values_to = "value"
  )

view(who2)
who2_longer<- 
  who2 |> 
  pivot_longer(
    cols = !(country: year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

view(household)
household |> 
  pivot_longer(
    cols = !family,
    names_to = c(".value","child"),
    names_sep = "_",
    values_drop_na = TRUE
  )


