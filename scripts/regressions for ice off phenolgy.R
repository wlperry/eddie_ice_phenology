# Eddie Module Ice Phenology ------

# Load libraries -----
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(broom)

# Read in files separately ------
baikal.df    <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Baikal") %>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))

cazenovia.df <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Cazenovia")%>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))

mendota.df   <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Mendota") %>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))

monona.df    <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Monona") %>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))

sunapee.df   <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Sunapee") %>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))

oneida.df    <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Oneida") %>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))

wingra.df    <- read_excel("data/lake_ice_phenology_module-.xls", sheet="Wingra") %>%
  mutate(ice_on_date = mdy(ice_on_date),
         ice_off_date = mdy(ice_off_date),
         ice_on_julian = yday(ice_on_date),
         ice_off_julian = yday(ice_off_date),
         pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970"))


# Bind all lakes into one file -----
lakes.df <- bind_rows(baikal.df, cazenovia.df, mendota.df, monona.df, 
                      sunapee.df, oneida.df, wingra.df)


# Whole time regressions -----
# Work with files separately to calculate regression coefficients
baikal.model<- lm(ice_off_julian ~ start_year, data = baikal.df)
tidy(summary(baikal.model))

baikal.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")



cazenovia.model<- lm(ice_off_julian ~ start_year, data = cazenovia.df)
tidy(summary(cazenovia.model))

cazenovia.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")


mendota.model<- lm(ice_off_julian ~ start_year, data = mendota.df)
tidy(summary(mendota.model))

mendota.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")



monona.model<- lm(ice_off_julian ~ start_year, data = monona.df)
tidy(summary(monona.model))

monona.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")



sunapee.model<- lm(ice_off_julian ~ start_year, data = sunapee.df)
tidy(summary(sunapee.model))

sunapee.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")



oneida.model<- lm(ice_off_julian ~ start_year, data = oneida.df)
tidy(summary(oneida.model))

oneida.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")



wingra.model<- lm(ice_off_julian ~ start_year, data = wingra.df)
tidy(summary(wingra.model))

wingra.df %>% ggplot(aes(start_year, ice_off_julian)) +
  geom_point() + 
  geom_smooth(method="lm")


# Pre post 1970 regressions----
baikal_pre_post.model <- baikal.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

baikal_pre_post.coef <- tidy(baikal_pre_post.model, pre_post.model)
baikal_pre_post.coef


baikal.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")



cazenovia_pre_post.model <- cazenovia.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

cazenovia_pre_post.coef <- tidy(cazenovia_pre_post.model, pre_post.model)
cazenovia_pre_post.coef

cazenovia.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")



mendota_pre_post.model <- mendota.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

mendota_pre_post.coef <- tidy(mendota_pre_post.model, pre_post.model)
mendota_pre_post.coef


mendota.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")



monona_pre_post.model <- monona.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

monona_pre_post.coef <- tidy(monona_pre_post.model, pre_post.model)
monona_pre_post.coef


monona.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")



oneida_pre_post.model <- oneida.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

oneida_pre_post.coef <- tidy(oneida_pre_post.model, pre_post.model)
oneida_pre_post.coef


oneida.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")



sunapee_pre_post.model <- sunapee.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

sunapee_pre_post.coef <- tidy(sunapee_pre_post.model, pre_post.model)
sunapee_pre_post.coef


sunapee.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")



wingra_pre_post.model <- wingra.df %>% group_by(pre_post_1970) %>%
  do(pre_post.model = lm(ice_off_julian ~ start_year, data = .))

wingra_pre_post.coef <- tidy(wingra_pre_post.model, pre_post.model)
wingra_pre_post.coef
 
wingra.df %>% ggplot(aes(start_year, ice_off_julian, color=pre_post_1970, 
                         shape=pre_post_1970,
                         linetype=pre_post_1970)) +
  geom_point() + 
  geom_smooth(method="lm")
  




