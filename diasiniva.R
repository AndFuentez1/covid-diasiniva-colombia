##DIA SIN IVA & COVID
#Author: @jorgegalindo

#Please read the following Twitter thread (ES) for reference: https://twitter.com/JorgeGalindo/status/1287540300945199104

rm(list = ls())
library(tidyverse)


##Extract original data - 

#download your csv from https://www.datos.gov.co/Salud-y-Protecci-n-Social/Estado-de-Casos-de-Coronavirus-COVID-19-en-Colombi/6c4c-msrp

#Read dataset

individual_daily_df <- read.csv("casos.csv", sep =",",
                                col.names=c("id_case","date_incl","city_num","city","dept","place","age","sex","type","status","country_origin","date_symptoms","date_death","date_diag","date_recovery","date_web","rec_type","dept_code","cntry_code","ethnic_belong","ethnic_name"), 
                                colClasses = c("city_num"="character"))

individual_daily_df <- individual_daily_df %>% 
  mutate(date_incl=as.Date(date_incl,format="%Y-%m-%d"),
         date_symptoms=as.Date(date_symptoms,format="%Y-%m-%d"),
         date_death=as.Date(date_death,format="%Y-%m-%d"),
         date_diag=as.Date(date_diag,format="%Y-%m-%d"),
         date_recovery=as.Date(date_recovery,format="%Y-%m-%d"),
         date_detect=as.Date(date_web,format="%Y-%m-%d"),
         case=as.integer(1),
         death=case_when(
           is.na(date_death) ~ 0,
           TRUE ~ 1
         )
  ) 

#Translate & Setup dataframes and vectors

citycodes <- individual_daily_df %>%
  select(city,city_num) %>%
  group_by(city,city_num) %>%
  summarize()

diditrain_df <- read.csv("lluviadiasiniva.csv") %>% left_join(citycodes) #Data copied from official IDEAM figures; some cities (Florencia, San José del Guaviare, Armenia) lacked data so I had to check with locals!
rain_v <- diditrain_df$city_num
big_cities_v <- c("05001","11001","76001","08001","08758","25754","13001","73001","68001") #Largest cities in CO

#Create DF


daily_sym_df <- individual_daily_df %>%
  group_by(city_num,date_symptoms) %>%
  summarize(
    deaths = sum(death),
    cases = sum(case)
  ) 


##Hypothesis 1: large cities should have seen further increases in contagion after Día Sin IVA, and these should be larger than in any random day

#Plot -- note that plots are quite spartan

bigcities_df <- daily_sym_df %>%
  select(city_num,date_symptoms,cases) %>%
  filter(city_num %in% big_cities_v)

bigcities_plot <- ggplot(data=subset(bigcities_df,date_symptoms>"2020-06-10")) +
  geom_col(aes(x=date_symptoms,y=cases)) +
  facet_wrap(vars(city_num), scales = "free")

#Diff-indiff analysis

  #1. Día sin IVA

diff_indiff_bigcities <- daily_sym_df %>%
  filter(date_symptoms>"2020-06-01") %>%
  mutate(time=case_when(
    date_symptoms>"2020-06-19" ~ 1,
    TRUE ~ 0
  ),
  treated=case_when(
    city_num %in% big_cities_v ~ 1,
    TRUE ~ 0
  ),
  interaction=time*treated)

didreg_bigcities = lm(cases ~ treated + time + interaction, data = diff_indiff_bigcities)

  #2. Any random day

diff_indiff_bigcities_random <- daily_sym_df %>%
  filter(date_symptoms<"2020-06-01") %>% #Change this figure to any other day
  mutate(time=case_when(
    date_symptoms>"2020-05-14" ~ 1, #Change this figure to any other day
    TRUE ~ 0
  ),
  treated=case_when(
    city_num %in% big_cities_v ~ 1,
    TRUE ~ 0
  ),
  interaction=time*treated)

didreg_bigcities_random = lm(cases ~ treated + time + interaction, data = diff_indiff_bigcities_random)  # Results are larger than for the Día Sin IVA - hypothesis discarded

##Hypothesis 2: large or midsized cities with no rain on the 19th of June should have seen further increases in contagion after Día Sin IVA

#Plot

rain_df_initial <- daily_sym_df %>%
  filter(city_num %in% rain_v, date_symptoms>"2020-06-10",date_symptoms<"2020-07-01") %>% #Change the dates accordingly, but keep a reasonable window re: reporting
  left_join(diditrain_df) %>%
  mutate(rain_dummy=case_when(
    rain>3 ~ "Yes", #take a look at the rain data and make a decision here
    TRUE ~ "No"
  )) 

norain <- rain_df_initial %>%
  filter(rain_dummy=="No") %>%
  select(city_num)

norain_v <- dplyr::pull(norain, city_num)

rain_df <- rain_df_initial %>%
  group_by(rain_dummy,date_symptoms) %>%
  summarize(cases=sum(cases))

rain_plot <- ggplot(rain_df) + geom_line(aes(x=date_symptoms,y=cases,color=rain_dummy))

#Diff indiff analysis

diff_indiff_rain <- daily_sym_df %>%
  filter(city_num %in% rain_v) %>%
  mutate(time=case_when(
    date_symptoms>"2020-06-19" ~ 1,
    TRUE ~ 0
  ),
         treated=case_when(
           city_num %in% norain_v ~ 1,
          TRUE ~ 0
           ),
  interaction=time*treated)

didreg_rain = lm(cases ~ treated + time + interaction, data = diff_indiff_rain)
