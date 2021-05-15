library(dplyr)
library(readr)
library(stringr)

##Load CSV files 
Property_value <- read.csv('Property_value.csv')
County_income <- read.csv('Income_per_county.csv')
Spend_per_pupil <- read.csv('Spend_per_pupil.csv')
NC_county <- read.csv('NC_counties.csv')

##mutate string to numeric 
School_performance <- read.csv('School Performance.csv')%>%
  mutate(Low_Income_perc = as.numeric(Low_Income_perc), Proficiency_18 = as.numeric(Grade_level_proficiency_18), Proficiency_19 = as.numeric(Grade_level_proficiency_19))

AB_School <- School_performance%>%
  group_by(County_Name)%>%
  filter(Grade_18 %in% c('A', 'B'))%>% 
  count(Grade_18)%>%
  summarize(School_AB_grade = sum(n))

Total_schools <- School_performance%>% count(County_Name)%>%
  inner_join(AB_School, by = 'County_Name')%>%
  mutate(A_ratio = School_AB_grade/n * 100)

Property_NC <- Property_value%>% 
  mutate(median_property_value = X5.31.2018)%>%
  select(RegionName, State, median_property_value)
#

##
NC_counties <- Property_value%>% 
  mutate(median_property_value = X5.31.2018)%>%
  select(RegionName, State, median_property_value)%>%
  inner_join(County_income, by = c('RegionName' = 'County_Name'))%>%
  rename(County_Name = RegionName)%>%
  inner_join(Spend_per_pupil, by = 'County_Name')%>%
  full_join(Total_schools, by = 'County_Name')

Perfomance_x_poverty <- School_performance%>%
  filter(Proficiency_18 > 70 & Proficiency_19 > 70)

Perfomance_and_low_income <- School_performance%>%
  filter(Grade_18 %in% c('A', 'B') & Grade_19 %in% c('A', 'B'))

Performance_x_property <-Property_NC%>%
  inner_join(School_performance, by = c('RegionName' = 'County_Name'))

NC_counties_TopLocal <- NC_counties%>%
  select(County_Name, median_property_value, Income, Local.PPE, School_AB_grade)%>%
  top_n(Local.PPE, 10)