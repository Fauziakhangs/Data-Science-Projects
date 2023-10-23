getwd()
mydata1 <- read.csv("C:/Users/HP/Downloads/population_even.csv")
mydata2 <- read.csv("C:/Users/HP/Downloads/population_odd.csv")

View(mydata1)
View(mydata2)

names(mydata1)
names(mydata2)

#Ques2a)
library(dplyr)
df <- mydata1 %>% inner_join(mydata2, by = c("STATE", "NAME")) %>% head()
df

df1 <- mydata1 %>% inner_join(mydata2, by = c("STATE", "NAME"))
df1
#Ques2b.a)
library(dplyr)
df <- mydata1 %>% inner_join(mydata2, by = c("STATE", "NAME")) %>% head()
df
names (df)

#Ques2b.b)
df_rename <- df1 %>% 
  select (STATE, NAME, POPESTIMATE2010, POPESTIMATE2012, 
  POPESTIMATE2014, POPESTIMATE2016, POPESTIMATE2018,
  POPESTIMATE2011,POPESTIMATE2013, POPESTIMATE2015, 
   POPESTIMATE2017, POPESTIMATE2019) %>%
  rename ("2010" = POPESTIMATE2010, "2012" = POPESTIMATE2012, 
"2014" = POPESTIMATE2014, "2016" = POPESTIMATE2016,  "2018" = POPESTIMATE2018, "2011" = POPESTIMATE2011,  "2013" = POPESTIMATE2013, "2015" = POPESTIMATE2015, "2017" = POPESTIMATE2017, "2019" = POPESTIMATE2019)

names(df_rename)

#Ques2b.c)
df_rename <- df_rename %>%
   select (STATE, NAME, "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
names(df_rename)


#Ques2c)
library(dplyr)
summary(df_rename)
View(df_rename)

exm <- sum(mean(df_rename$"2010", na.rm=TRUE), 
           mean (df_rename$"2012", na.rm = TRUE))
df_rename$"2011" <- df_rename$"2011" %>%
replace_na(round(exm/2))

exm1 <- sum(mean (df_rename$"2012", na.rm = TRUE), 
            mean (df_rename$"2014", na.rm = TRUE))
df_rename$"2013" <- df_rename$"2013" %>%
replace_na(round(exm1/2))

exm2 <- sum(mean (df_rename$"2014", na.rm = TRUE),
            mean (df_rename$"2016", na.rm = TRUE))
df_rename$"2015" <- df_rename$"2015" %>%
replace_na(round (exm2/2))


exm3 <- sum (mean (df_rename$"2016", na.rm = TRUE),
             mean (df_rename$"2018", na.rm = TRUE))

df_rename$"2017" <- df_rename$"2017"%>%
replace_na(round(exm3/2))


df_rename$"2019" <- df_rename$"2019" %>%
replace_na(round(mean(df_rename$"2018", na.rm = TRUE)))

summary(df_rename)
View(df_rename)

#Ques2d.a)
library(dplyr)
max_population <- df_rename %>%
   select(-c("STATE")) %>%
  rowwise() %>%
  summarise(NAME, max_pop = 
  max(`2010`, `2011`, '2012', '2013', '2014', '2015', 
  '2016', '2017', '2018', '2019'))

 max_population
 
#Ques2d.b)
 total_population <- df_rename %>%
    rowwise() %>%
  summarise(NAME, TotalPop = sum(`2012`, `2013`, `2014`, `2015`, 
                           `2016`, `2017`, `2018`, `2019`))
total_population 
 
#Ques2e)
 sum (df_rename$'2010')
 sum (df_rename$'2011')
 sum (df_rename$'2012')
 sum (df_rename$'2013')
 sum (df_rename$'2014')
 sum (df_rename$'2015') 
 sum (df_rename$'2016') 
 sum (df_rename$'2017') 
 sum (df_rename$'2018')
 sum (df_rename$'2019')
 