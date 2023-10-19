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

### unique values
flights |> 
  distinct()

flights |> 
  distinct(origin, dest, .keep_all = TRUE)

flights |> 
  count(origin, dest, sort = TRUE)

# mutate()

# before = 1 birinci satırdan once sonucu yaz
flights |> 
  mutate(
    gain = flights$dep_delay - flights$arr_delay,
    speed = flights$distance / flights$air_time * 60,
    .before = 1 
  )


flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day 
  )

### sadece kullanilanlari sonuc olarak ver
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

# select()

### only year, month and day
flights |> 
  select(year, month, day)

### from year to day
flights |> 
  select(year:day)

### remove from year to day
flights |>
  select(!year:day)

### only characters
flights |> 
  select(where(is.character))

### kolon adi degistirip sectik
flights |> 
  select(tail_num = tailnum)

### kolon adi degistirdik
flights_rename <- flights |> 
                    rename(tail_num = tailnum)

# select icinde kullanilabilecek komutlar
# starts_with("abc")
# ends_with("..")
# contains("")

# relocate

### relocate kolon konumu degistirir burada tablonun basina aldi iki kolonu
flights |> 
  relocate(time_hour, air_time)

flights |> 
  relocate(year:dep_time, .after = time_hour)

### "arr" ile baslayan kolonlari dep_time kolonunun onune attik    
flights |> 
  relocate(starts_with("arr"), .before = dep_time)


# Data Tidying: Pivotting -------------------------------------------------
# Reference: R for Data Science, 2nd Edition

table1
table2
table3

table1 |> 
  mutate(rate = cases / population * 10000)

table1 |> 
  group_by(year) |> 
  summarize(total_cases = sum(cases))

ggplot(table1, aes(x=year, y=cases)) + 
  geom_line(aes(group= country), color="darkgray", linewidth=2) + 
  geom_point(aes(color = country, shape = country), size=4) + 
  scale_x_continuous(breaks = c(1999,2000))

# Lengthening Data --------------------------------------------------------

dim(billboard)
# 317  79

### billboard; 1999 6. ayindan 2000 sonuna kadar hafta hafta top100 listesi
### verinin kolon sayisi 317; bunu daha cok satir olacak sekilde duzenliyoruz
### amac aslinda daha cok ggplot'a uygun hale getirmek

###kolon isimleri wk ile baslayanlari sectik
### sutun adlarini week kolonuna atadik
### iceriklerini (value) de rank kolonuna atadik
### parse_number ile week kolonunu rakamlara dondurduk

billboard_longer <- 
  billboard |> 
  pivot_longer(
    cols= starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) |> 
  mutate(
    week = parse_number(week)
  )

billboard_longer |> 
  ggplot(aes(week, rank, group= track)) + 
  geom_line(alpha=0.3) + 
  scale_y_reverse()

df <- tribble(
  ~id, ~bp1, ~bp2,
  "A", 100,  120,
  "B", 140,  115,
  "C", 120,  125
)

df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

### who2 kolon isimleri (sp_m_014) her ayracta (_) dgnss, gndr, age
who2
View(who2)

who2 |> 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count"
  )

household
View(household)

household |> 
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )




