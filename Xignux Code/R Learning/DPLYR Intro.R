library(dbplyr)
library(dplyr)
library(nycflights13)
dim(flights)
flights
filter(flights, month == 1, day == 1)
flights[flights$month == 1 & flights$day == 1, ]
#row 5 should work but it doesn't
#should be the same result as row 6
#dplyr was missing. Had installes dbplyr
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))
#use desc for descending order
select(flights, year, month, day)
#selects columns by name
select(flights, year:day)
#select all columns between year and day, inclusive
select(flights, -(year:day))
#select all columns except for year those bwtween to day
rename(flights, departure = dep_time)
#new name equals old name
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance/air_time * 60)
#adds two columns to the table
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time/60))
#you can refer to columns you just created
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time/60))
#would give you a table with only the new variables
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))
#reduces it to one row.
sample_n(flights, 10)
#random sample of x rows
sample_frac(flights, 0.01)
#random sample of fraction of database
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
#grouped by plane
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()
#plotted the graph
destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n())
#grouping by destination and then checking how many
#flights and specific planes go to a destination
