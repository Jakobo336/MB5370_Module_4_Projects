#In this section, you will be introduced to tidy data and the accompanying tools in the tidyr package. tidyr is part of the core tidyverse, which you should now be quite familiar with. Before starting this section, make sure the tidyverse is loaded.

install.packages("tidyverse")   # only once, to install the whole tidyverse
library(dplyr)                  # for mutate(), filter(), count()
library(tidyr)

#There are many ways to display a given data set, but not every way is easy to use for analysis. For example, the format of a field datasheet might be convenient for data collection and entry, but it might not be a useful way to display the data when you go to analyse it. The process of rearranging your data to a format more appropriate for analysis is the process of “tidying.”
#Let’s look at an example below:

table1

#Each row is one observation: the TB cases and population for a single country–year pair

#Table 1 is already tidy, number of cases per country per year and matching population per country per year is already tidy.
#To divide cases by population:
table1 %>% 
  mutate(rate = cases / population * 10000)

table2

#Table 2 is not tidy. Each row is not an observation, instead each row is a piece of a larger observation. The TB cases and population for a single country–year pair are spread over two rows.

cases <- filter(table2, type == "cases")
pop   <- filter(table2, type == "population")

joined <- inner_join(cases, pop, by = c("country", "year"))

joined %>% 
  mutate(rate = count.x / count.y * 10000)

table3

#Table 3 is not tidy. Each row is an observation, but a single variable (rate) contains two pieces of information that should be split into two separate variables (cases and population).

table3 %>% 
  separate(col = rate, into = c("cases", "population"), sep = "/") %>% 
  mutate(cases = as.numeric(cases),
         population = as.numeric(population)) %>% 
  mutate(rate = cases / population * 10000)

#Each table displays the exact same dataset, but only table1 is “tidy.”

# Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)

# Compute cases per year
table1 %>% 
  count(year, wt = cases)

# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#We’ll start with pivoting our data frame longer because this is the most common tidying issue you will likely face within a given dataset. pivot_longer() makes datasets “longer” by increasing the number of rows and decreasing the number of columns, solving those common problems of data values in the variable name (e.g wk1, wk2, wk3, etc.).

#Let’s see this in action by using pivot_longer() in an example given by R4DS:
#The billboard dataset records the billboard rank of songs in the year 2000
billboard

#In this dataset, each observation is a song. The first three columns (artist, track and date.entered) are variables that describe the song. Then we have 76 columns (wk1-wk76) that describe the rank of the song in each week. Here, the column names are one variable (the week) and the cell values are another (the rank). To tidy the billboard dataset we will use pivot_longer().
#This is the case where actual data values (wk1, wk2 etc.)  are in the column name, with each observation (row of data) being a song. We need to have the data in a format where each row is an observation (so-called long format).

billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )

#Notice the NA values in the output above? It looks like “Baby Don’t Cry” by 2 Pac was only in the top 100 for 7 out of 76 weeks. Therefore, when we lengthened the data, the weeks where it wasn't on the charts became ‘NA.’ These NA’s were forced to exist because of the structure of the dataset not because they are actually unknown. Therefore, we can simply ask pivot_longer to remove them by adding the argument values_drop_na = TRUE as shown below:

billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )

#4.5.2 Pivoting longer

df <- tribble(
  ~id,  ~bp1, ~bp2,
  "A",  100,  120,
  "B",  140,  115,
  "C",  120,  125
)

#Here, all we have done is created a dataset called ‘df’ with 3 variables and their associated values. 
#However, we want our new (tidy) dataset to have three variables: 
#1. id (which already exists)
#2. measurement (the column names) 
#3. value (the cell values)

#To make this happen we need to pivot df longer:
df |> 
  pivot_longer(
    cols = bp1:bp2,
    names_to = "measurement",
    values_to = "value"
  )

#4.5.3 Widening datasets

#We’ll use an example from R4DS to explore pivot_wider() looking at the cms_patient_experience dataset from the Centers of Medicare and Medicaid.

cms_patient_experience

#The core unit being studied is an organization. But in this format, each organization is spread across six rows with one row for each measurement taken in the survey organization. We can see the complete set of values for measure_cd and measure_title by using distinct():

cms_patient_experience |> 
  distinct(measure_cd, measure_title)

#pivot_wider() has the opposite interface to pivot_longer(): instead of choosing new column names, we need to provide the existing columns that define the values (values_from) and the column name (names_from):

cms_patient_experience |> 
  pivot_wider(
    names_from = measure_cd,
    values_from = prf_rate
  )

cms_patient_experience |> 
  pivot_wider(
    id_cols = starts_with("org"),
    names_from = measure_cd,
    values_from = prf_rate
  )

#4.5.4 Pivoting wider
#To understand what pivot_wider() does to our data, let’s once again use a simple example. This time we have two patients with id s A and B, and we have three blood pressure (bp) measurements from patient A and two from patient B:

df <- tribble(
  ~id, ~measurement, ~value,
  "A",        "bp1",    100,
  "B",        "bp1",    140,
  "B",        "bp2",    115, 
  "A",        "bp2",    120,
  "A",        "bp3",    105
)

#We’ll take the names from the measurement column using the names_from() argument and the values from the value column using the values_from() argument:

df |> 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

#To start the pivoting process, pivot_wider() needs to first figure out what will go in the rows and columns. The new column names will be the unique values of measurement.

df |> 
  distinct(measurement) |> 
  pull()

df |> 
  select(-measurement, -value) |> 
  distinct()

#pivot_wider() then combines these results to generate an empty dataframe:

df |> 
  select(-measurement, -value) |> 
  distinct() |> 
  mutate(x = NA, y = NA, z = NA)

#Exercise 4.5.5

#1 Why pivot_longer() and pivot_wider() are not perfectly symmetrical
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half   = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

#Pivot wider then longer again
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")

#Fix by forcing type back to numeric
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return",
               names_ptypes = list(year = double()))


#2 Why the table4a code fails
#tries to find columns "1999" and "2000" as positions
#table4a %>% pivot_longer(c(1999, 2000), names_to = "year", values_to = "cases")

#Fix: quote/backtick column names or use tidyselect helpers
table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")


#3 Preg tibble: making it tidy
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

# Variables here are: pregnant status, sex, and count
# The data is wide, so we pivot_longer() to make it tidy
preg_long <- preg %>% 
  pivot_longer(cols = c(male, female), names_to = "sex", values_to = "count")

preg_long

#4.5.6 Seperating and Uniting data tables

table3

#We need to split the rate column up into two variables: 1) cases and 2) population. separate() will take the name of the column we want to split and the names of the columns we want it split into.

table3 %>% 
  separate(rate, into = c("cases", "population"))

#By default, separate() will split values wherever it sees non-alphanumeric characters. In this case, it’s splitting on the “/” in the rate column. If you want to split on a specific character, you can use the sep argument:

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

#This is a default of using separate(). However, since the values in those columns are actually numbers, we want to ask separate() to convert them to better types using convert = TRUE. Now you can see they are listed as integer types(<int>)

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

#Using unite()

table5 %>% 
  unite(new, century, year, sep = "")

#4.6 Filling missing data

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

treatment |>
  fill(everything())

#4.6.2 Fixed values
#Sometimes missing values represent some fixed and known value, most commonly 0. You can use dplyr::coalesce() to replace them:
x <- c(1, 4, 5, 7, NA)
coalesce(x, 0)

x <- c(1, 4, 5, 7, -99)
na_if(x, -99)

#4.6.3 NaN
x <- c(NA, NaN)
x * 10
#> [1]  NA NaN
x == 1
#> [1] NA NA
is.na(x)
#> [1] TRUE TRUE

#4.6.4 Implicit missing values

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

#Here’s another example where if we pivot stocks wider to put the quarter in the columns, both missing values become explicit:

stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  )

#4.7.1 CSV files
#The example below shows that we have a data file called students.csv and it lives in a folder called ‘data’ on your computer. Note this isn’t a real file you have, just an example so this code won’t work.

? read_csv
students <- read_csv("C://data/students.csv")
#> Rows: 6 Columns: 5

library(readr)
students <- read_csv("https://pos.it/r4ds-students-csv")

#In the favourite.food column, there are a bunch of food items, and then the character string N/A, which should have been a real NA that R will recognize as “not available”. This is something we can address using the na argument. By default, read_csv() only recognizes empty strings ("") in this dataset as NAs, we want it to also recognize the character string "N/A".

students <- read_csv("data/students.csv", na = c("N/A", ""))

students

#You might also notice that the Student ID and Full Name columns are surrounded by backticks. That’s because they contain spaces, breaking R’s usual rules for variable names; they’re non-syntactic names (think back to our intro to programming workshop!). To refer to these variables, you need to surround them with backticks, `:

students |> 
  rename(
    student_id = `Student ID`,
    full_name = `Full Name`
  )

#4.7.3 Exercises

#Identify what is wrong with each of the following inline CSV files. What happens when you run the code?

#1. Too many values in a row (extra column created)
read_csv("a,b\n1,2,3\n4,5,6")
# -> Warning about extra columns, creates column ...3

#2. Too few and too many values (NA for missing, ...4 for extra)
read_csv("a,b,c\n1,2\n1,2,3,4")
# -> Fills missing with NA, creates extra column ...4

#3. Unclosed quote (parse error)
# This will fail with an error: "Unclosed quoted string"
read_csv("a,b\n\"1")

#4. Mixed data types (coerces to character)
read_csv("a,b\n1,2\na,b")
# -> Both columns read as character because of mixed types

#5. Wrong delimiter (semicolon treated as text, not separator)
read_csv("a;b\n1;3")
# -> Everything becomes one column. Fix with:
read_delim("a;b\n1;3", delim = ";")
# or use read_csv2() which defaults to semicolons

#4.8.1 Relational Data
#Let’s explore relational data using the nycflight13 package dataset. You should already be familiar with this dataset from the Tibbles section (5.3). Make sure tidyverse and nycflights13 are loaded into your R library for this session.

library(tidyverse)
library(nycflights13)

#airlines lets you look up the full carrier name from its abbreviated code:

airlines
#> # A tibble: 16 × 2
#>   carrier name                    
#>   <chr>   <chr>                   
#> 1 9E      Endeavor Air Inc.       
#> 2 AA      American Airlines Inc.  
#> 3 AS      Alaska Airlines Inc.    
#> 4 B6      JetBlue Airways         
#> 5 DL      Delta Air Lines Inc.    
#> 6 EV      ExpressJet Airlines Inc.
#> # ... with 10 more rows

#airports gives information about each airport, identified by the FAA airport code:

airports
#> # A tibble: 1,458 × 8
#>   faa   name                             lat   lon   alt    tz dst   tzone      
#>   <chr> <chr>                          <dbl> <dbl> <dbl> <dbl> <chr> <chr>      
#> 1 04G   Lansdowne Airport               41.1 -80.6  1044    -5 A     America/Ne...
#> 2 06A   Moton Field Municipal Airport   32.5 -85.7   264    -6 A     America/Ch...
#> 3 06C   Schaumburg Regional             42.0 -88.1   801    -6 A     America/Ch...
#> 4 06N   Randall Airport                 41.4 -75.4   523    -5 A     America/Ne...
#> 5 09J   Jekyll Island Airport           31.1 -81.4    11    -5 A     America/Ne...
#> 6 0A9   Elizabethton Municipal Airport  36.4 -82.2  1593    -5 A     America/Ne...
#> # ... with 1,452 more rows

#planes gives information about each plane, identified by its tail number:

planes
#> # A tibble: 3,322 × 9
#>   tailnum  year type                    manuf...¹ model engines seats speed engine
#>   <chr>   <int> <chr>                   <chr>   <chr>   <int> <int> <int> <chr> 
#> 1 N10156   2004 Fixed wing multi engine EMBRAER EMB-...       2    55    NA Turbo...
#> 2 N102UW   1998 Fixed wing multi engine AIRBUS... A320...       2   182    NA Turbo...
#> 3 N103US   1999 Fixed wing multi engine AIRBUS... A320...       2   182    NA Turbo...
#> 4 N104UW   1999 Fixed wing multi engine AIRBUS... A320...       2   182    NA Turbo...
#> 5 N10575   2002 Fixed wing multi engine EMBRAER EMB-...       2    55    NA Turbo...
#> 6 N105UW   1999 Fixed wing multi engine AIRBUS... A320...       2   182    NA Turbo...
#> # ... with 3,316 more rows, and abbreviated variable name ¹​manufacturer

#weather gives hourly meteorological data for each airport:

weather
#> # A tibble: 26,115 × 15
#>   origin  year month   day  hour  temp  dewp humid wind_dir wind_speed wind_gust
#>   <chr>  <int> <int> <int> <int> <dbl> <dbl> <dbl>    <dbl>      <dbl>     <dbl>
#> 1 EWR     2013     1     1     1  39.0  26.1  59.4      270      10.4         NA
#> 2 EWR     2013     1     1     2  39.0  27.0  61.6      250       8.06        NA
#> 3 EWR     2013     1     1     3  39.0  28.0  65.4      240      11.5         NA
#> 4 EWR     2013     1     1     4  39.9  28.0  62.2      250      12.7         NA
#> 5 EWR     2013     1     1     5  39.0  28.0  65.4      260      12.7         NA
#> 6 EWR     2013     1     1     6  37.9  28.0  67.2      240      11.5         NA
#> # ... with 26,109 more rows, and 4 more variables: precip <dbl>, pressure <dbl>,
#> #   visib <dbl>, time_hour <dttm>

#So how can we actually join together our datasets? By identifying the keys. 
#A key is a variable (or set of variables) that uniquely identifies an observation. In simple cases, a single variable is sufficient to identify an observation. For example, each plane is uniquely identified by its tailnum. In other cases, multiple variables may be needed. For example, to identify an observation in weather you need five variables: year, month, day, hour, origin.

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)
#> # A tibble: 0 × 2
#> # ... with 2 variables: tailnum <chr>, n <int>

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)
#> # A tibble: 3 × 6
#>    year month   day  hour origin     n
#>   <int> <int> <int> <int> <chr>  <int>
#> 1  2013    11     3     1 EWR        2
#> 2  2013    11     3     1 JFK        2
#> 3  2013    11     3     1 LGA        2

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)
#> # A tibble: 29,768 × 5
#>    year month   day flight     n
#>   <int> <int> <int>  <int> <int>
#> 1  2013     1     1      1     2
#> 2  2013     1     1      3     2
#> 3  2013     1     1      4     2
#> 4  2013     1     1     11     3
#> 5  2013     1     1     15     2
#> 6  2013     1     1     21     2
#> # ... with 29,762 more rows

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)
#> # A tibble: 64,928 × 5
#>    year month   day tailnum     n
#>   <int> <int> <int> <chr>   <int>
#> 1  2013     1     1 N0EGMQ      2
#> 2  2013     1     1 N11189      2
#> 3  2013     1     1 N11536      2
#> 4  2013     1     1 N11544      3
#> 5  2013     1     1 N11551      2
#> 6  2013     1     1 N12540      2
#> # ... with 64,922 more rows

#4.6.2 Mutating joins
#Mutating joins are a great tool we can use for combining a pair of tables.
#A mutating join allows you to combine variables from two tables. It first matches observations by their keys, then copies across variables from one table to the other.

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
#> # A tibble: 336,776 × 8
#>    year month   day  hour origin dest  tailnum carrier
#>   <int> <int> <int> <dbl> <chr>  <chr> <chr>   <chr>  
#> 1  2013     1     1     5 EWR    IAH   N14228  UA     
#> 2  2013     1     1     5 LGA    IAH   N24211  UA     
#> 3  2013     1     1     5 JFK    MIA   N619AA  AA     
#> 4  2013     1     1     5 JFK    BQN   N804JB  B6     
#> 5  2013     1     1     6 LGA    ATL   N668DN  DL     
#> 6  2013     1     1     5 EWR    ORD   N39463  UA     
#> # ... with 336,770 more rows

#Now we will see what happens when we use the mutating function left_join()

flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")
#> # A tibble: 336,776 × 7
#>    year month   day  hour tailnum carrier name                  
#>   <int> <int> <int> <dbl> <chr>   <chr>   <chr>                 
#> 1  2013     1     1     5 N14228  UA      United Air Lines Inc. 
#> 2  2013     1     1     5 N24211  UA      United Air Lines Inc. 
#> 3  2013     1     1     5 N619AA  AA      American Airlines Inc.
#> 4  2013     1     1     5 N804JB  B6      JetBlue Airways       
#> 5  2013     1     1     6 N668DN  DL      Delta Air Lines Inc.  
#> 6  2013     1     1     5 N39463  UA      United Air Lines Inc. 
#> # ... with 336,770 more rows

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)])
#> # A tibble: 336,776 × 7
#>    year month   day  hour tailnum carrier name                  
#>   <int> <int> <int> <dbl> <chr>   <chr>   <chr>                 
#> 1  2013     1     1     5 N14228  UA      United Air Lines Inc. 
#> 2  2013     1     1     5 N24211  UA      United Air Lines Inc. 
#> 3  2013     1     1     5 N619AA  AA      American Airlines Inc.
#> 4  2013     1     1     5 N804JB  B6      JetBlue Airways       
#> 5  2013     1     1     6 N668DN  DL      Delta Air Lines Inc.  
#> 6  2013     1     1     5 N39463  UA      United Air Lines Inc. 
#> # ... with 336,770 more rows

#Let’s dive into how mutating joins work in detail. Visual representations are a handy tool for conceptualising these joins. 

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

#The output of an inner join is a new data frame that contains the key, the x values, and the y values. We use ‘by’ to tell dplyr which variable is the key:

x %>% 
  inner_join(y, by = "key")
#> # A tibble: 2 × 3
#>     key val_x val_y
#>   <dbl> <chr> <chr>
#> 1     1 x1    y1   
#> 2     2 x2    y2

#One table has duplicate keys. This is useful when you want to add in additional information as there is typically a one-to-many relationship.
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")
#> # A tibble: 4 × 3
#>     key val_x val_y
#>   <dbl> <chr> <chr>
#> 1     1 x1    y1   
#> 2     2 x2    y2   
#> 3     2 x3    y2   
#> 4     1 x4    y1

#2. Both tables have duplicate keys. This is usually an error because in neither table do the keys uniquely identify an observation. When you join duplicate keys, you get all possible combinations, the Cartesian product:
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")
#> # A tibble: 6 × 3
#>     key val_x val_y
#>   <dbl> <chr> <chr>
#> 1     1 x1    y1   
#> 2     2 x2    y2   
#> 3     2 x2    y3   
#> 4     2 x3    y2   
#> 5     2 x3    y3   
#> 6     3 x4    y4

#So far, the pairs of tables have always been joined by a single variable, and that variable has the same name in both tables. That constraint was encoded by by = "key". You can use other values for by to connect the tables in other ways:
flights2 %>% 
  left_join(weather)
#> Joining, by = c("year", "month", "day", "hour", "origin")
#> # A tibble: 336,776 × 18
#>    year month   day  hour origin dest  tailnum carrier  temp  dewp humid wind_...¹
#>   <int> <int> <int> <dbl> <chr>  <chr> <chr>   <chr>   <dbl> <dbl> <dbl>   <dbl>
#> 1  2013     1     1     5 EWR    IAH   N14228  UA       39.0  28.0  64.4     260
#> 2  2013     1     1     5 LGA    IAH   N24211  UA       39.9  25.0  54.8     250
#> 3  2013     1     1     5 JFK    MIA   N619AA  AA       39.0  27.0  61.6     260
#> 4  2013     1     1     5 JFK    BQN   N804JB  B6       39.0  27.0  61.6     260
#> 5  2013     1     1     6 LGA    ATL   N668DN  DL       39.9  25.0  54.8     260
#> 6  2013     1     1     5 EWR    ORD   N39463  UA       39.0  28.0  64.4     260
#> # ... with 336,770 more rows, 6 more variables: wind_speed <dbl>,
#> #   wind_gust <dbl>, precip <dbl>, pressure <dbl>, visib <dbl>,
#> #   time_hour <dttm>, and abbreviated variable name ¹​wind_dir


flights2 %>% 
  left_join(planes, by = "tailnum")
#> # A tibble: 336,776 × 16
#>   year.x month   day  hour origin dest  tailnum carrier year.y type      manuf...¹
#>    <int> <int> <int> <dbl> <chr>  <chr> <chr>   <chr>    <int> <chr>     <chr>  
#> 1   2013     1     1     5 EWR    IAH   N14228  UA        1999 Fixed wi... BOEING 
#> 2   2013     1     1     5 LGA    IAH   N24211  UA        1998 Fixed wi... BOEING 
#> 3   2013     1     1     5 JFK    MIA   N619AA  AA        1990 Fixed wi... BOEING 
#> 4   2013     1     1     5 JFK    BQN   N804JB  B6        2012 Fixed wi... AIRBUS 
#> 5   2013     1     1     6 LGA    ATL   N668DN  DL        1991 Fixed wi... BOEING 
#> 6   2013     1     1     5 EWR    ORD   N39463  UA        2012 Fixed wi... BOEING 
#> # ... with 336,770 more rows, 5 more variables: model <chr>, engines <int>,
#> #   seats <int>, speed <int>, engine <chr>, and abbreviated variable name
#> #   ¹​manufacturer

flights2 %>% 
  left_join(airports, c("dest" = "faa"))
#> # A tibble: 336,776 × 15
#>    year month   day  hour origin dest  tailnum carrier name      lat   lon   alt
#>   <int> <int> <int> <dbl> <chr>  <chr> <chr>   <chr>   <chr>   <dbl> <dbl> <dbl>
#> 1  2013     1     1     5 EWR    IAH   N14228  UA      George...  30.0 -95.3    97
#> 2  2013     1     1     5 LGA    IAH   N24211  UA      George...  30.0 -95.3    97
#> 3  2013     1     1     5 JFK    MIA   N619AA  AA      Miami ...  25.8 -80.3     8
#> 4  2013     1     1     5 JFK    BQN   N804JB  B6      <NA>     NA    NA      NA
#> 5  2013     1     1     6 LGA    ATL   N668DN  DL      Hartsf...  33.6 -84.4  1026
#> 6  2013     1     1     5 EWR    ORD   N39463  UA      Chicag...  42.0 -87.9   668
#> # ... with 336,770 more rows, and 3 more variables: tz <dbl>, dst <chr>,
#> #   tzone <chr>

flights2 %>% 
  left_join(airports, c("origin" = "faa"))
#> # A tibble: 336,776 × 15
#>    year month   day  hour origin dest  tailnum carrier name      lat   lon   alt
#>   <int> <int> <int> <dbl> <chr>  <chr> <chr>   <chr>   <chr>   <dbl> <dbl> <dbl>
#> 1  2013     1     1     5 EWR    IAH   N14228  UA      Newark...  40.7 -74.2    18
#> 2  2013     1     1     5 LGA    IAH   N24211  UA      La Gua...  40.8 -73.9    22
#> 3  2013     1     1     5 JFK    MIA   N619AA  AA      John F...  40.6 -73.8    13
#> 4  2013     1     1     5 JFK    BQN   N804JB  B6      John F...  40.6 -73.8    13
#> 5  2013     1     1     6 LGA    ATL   N668DN  DL      La Gua...  40.8 -73.9    22
#> 6  2013     1     1     5 EWR    ORD   N39463  UA      Newark...  40.7 -74.2    18
#> # ... with 336,770 more rows, and 3 more variables: tz <dbl>, dst <chr>,
#> #   tzone <chr>

#Let’s explore pipes using code to tell a kids story about a little bunny named Foo Foo:
#Little bunny Foo Foo
#Went hopping through the forest
#Scooping up the field mice
#And bopping them on the head

foo_foo <- little_bunny()

#And we’ll use a function for each key verb: hop(), scoop(), and bop(). Using this object and these verbs, there are (at least) four ways we could retell the story in code:
#Save each intermediate step as a new object.
#Overwrite the original object many times.
#Compose functions.
#Use the pipe.

foo_foo_1 <- hop(foo_foo, through = forest)
foo_foo_2 <- scoop(foo_foo_1, up = field_mice)
foo_foo_3 <- bop(foo_foo_2, on = head)

#Overwrite the original object instead of creating intermediate objects at each step.

foo_foo <- hop(foo_foo, through = forest)
foo_foo <- scoop(foo_foo, up = field_mice)
foo_foo <- bop(foo_foo, on = head)

#String the function calls together
bop(
  scoop(
    hop(foo_foo, through = forest),
    up = field_mice
  ), 
  on = head
)

#Use the pipe
foo_foo %>%
  hop(through = forest) %>%
  scoop(up = field_mice) %>%
  bop(on = head)






