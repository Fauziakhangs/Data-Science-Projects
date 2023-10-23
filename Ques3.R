
#Ques03

library(dplyr)
library(tidyr)
df1 <- mydata1 %>% inner_join(mydata2, by = c("STATE", "NAME"))
df1
df_rename <- df1 %>% 
  select (STATE, NAME, POPESTIMATE2010, POPESTIMATE2012, 
          POPESTIMATE2014, POPESTIMATE2016, POPESTIMATE2018,
          POPESTIMATE2011,POPESTIMATE2013, POPESTIMATE2015, 
          POPESTIMATE2017, POPESTIMATE2019) %>%
  rename ("2010" = POPESTIMATE2010, "2012" = POPESTIMATE2012, 
          "2014" = POPESTIMATE2014, "2016" = POPESTIMATE2016,  "2018" = POPESTIMATE2018, "2011" = POPESTIMATE2011,  "2013" = POPESTIMATE2013, "2015" = POPESTIMATE2015, "2017" = POPESTIMATE2017, "2019" = POPESTIMATE2019)

df_rename <- df_rename %>%
  select (STATE, NAME, "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
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

library(ggplot2)

df_renameGraph <- df_rename %>%
  filter(NAME %in% c('New York', 'California', 'Texas')) %>%
  pivot_longer(cols = c("2010", "2011", "2012", "2013", "2014", "2015", 
                        "2016", "2017", "2018", "2019"), 
               names_to = "YEAR", values_to = "population")
df_renameGraph


plt <- ggplot(df_renameGraph, aes (x=YEAR, y=population, col = NAME, group = NAME))
plt + geom_line()
