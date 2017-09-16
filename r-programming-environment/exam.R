library(readr)
library(dplyr)
library(tidyr)
library(readxl)

## pollution <- read_csv("data/daily_SPEC_2014.csv.bz2")
## site_meta <- read_excel("data/aqs_sites.xlsx")

q1_bromine_WN<- pollution %>%
    filter(`State Name` == "Wisconsin" & `Parameter Name` == "Bromine PM2.5 LC") %>%
    summarize(q1 = mean(`Arithmetic Mean`, na.rm = TRUE))

print(q1_bromine_WN)

avg_by_chemical <- pollution %>%
    group_by(`Parameter Name`) %>%
    summarize(avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
    arrange(desc(avg))

q2 <- avg_by_chemical %>%
    filter(`Parameter Name` %in% c("EC2 PM2.5 LC",
                                   "OC CSN Unadjusted PM2.5 LC TOT",
                                   "Sulfur PM2.5 LC",
                                   "Sodium PM2.5 LC"))

print(q2)

avg_by_site_sulfate <- pollution %>%
    filter(`Parameter Name` == "Sulfate PM2.5 LC") %>%
    group_by(`State Code`, `County Code`, `Site Num`) %>%
    summarize(avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
    arrange(desc(avg))

print(avg_by_site_sulfate)

q4 <- pollution %>%
    filter(`State Name` %in% c("California", "Arizona")
           & `Parameter Name` == "EC PM2.5 LC TOR") %>%
    group_by(`State Name`) %>%
    summarize(avg = mean(`Arithmetic Mean`, na.rm = TRUE))

q4 <- abs(q4[1,2]-q4[2,2])
print(q4)

q5 <- pollution %>%
    filter(Longitude < -100.0
           & `Parameter Name` == "OC PM2.5 LC TOR") %>%
    summarize(q5_med = median(`Arithmetic Mean`, na.rm = TRUE))

print(q5)

q6 <- site_meta %>%
    filter(`Land Use`== "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
    summarize(cnt = n())

print(q6)

site_tag <- site_meta %>%
    select(c("State Code", "County Code", "Site Number", "Land Use", "Location Setting"))

pollution <- pollution %>%
    mutate(`State Code` = as.numeric(`State Code`)) %>%
    mutate(`County Code` = as.numeric(`County Code`)) %>%
    mutate(`Site Number` = as.numeric(`Site Num`))

pollution2 <- pollution %>% left_join(site_tag)

q7 <- pollution2 %>%
    filter(Longitude >= -100.0
           & `Parameter Name` == "EC PM2.5 LC TOR") %>%
    filter(`Land Use`== "RESIDENTIAL" & `Location Setting` == "SUBURBAN") %>%
    summarize(med = median(`Arithmetic Mean`, na.rm = TRUE))

print(q7)

library(lubridate)
q8 <- pollution2  %>%
    filter(`Land Use`=="COMMERCIAL", `Parameter Name`=="Sulfate PM2.5 LC") %>%
    group_by(month(`Date Local`)) %>%
    summarise(avg = mean(`Arithmetic Mean`)) %>%
    arrange(desc(avg))
q8
