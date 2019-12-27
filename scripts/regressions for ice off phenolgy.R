# Eddie Module Ice Phenology ------
# this is an adaption of the eddie module on ice phenology to 
# examine regressions of ice on off and duration of ice cover from lakes
# 
# The first is to use a demonstration of Lake Sunapee, New Hampshire, USA
# then the script will allow students to adapt the code to be use to examine 
# other lakes

# Install libraries------
# Install issing libraries for use to exame data using tidyverse
# this only has to be done one time and a # can be put in front of 
# the code to comment it out so it does not run in the future as now
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("broom")

# Load libraries -----
# Load the libraries each time you run a script or program
library(tidyverse) # loads a lot of libraries in this one to add more functionality
library(readxl) # allows you to read in excel files
library(janitor) # cleans up column names and removes empty columns if wanted
library(lubridate) # allows easy conversion of varaibles to date format
library(broom) # cleans up output for easy presentation

# Read file ------
# note since they are all in different sheets you can change
# sheet = "name of lake" to the name you are interested in
# to read in that lake

# Read in Lake Sunapee -----
# stores in the sunapee dataframe in the environment tab
sunapee.df <- read_excel("data/lake_ice_phenology_module-.xls", # identifies the file to read in
                           sheet="Sunapee", # identifies the sheet to read in
                           guess_max = 1000) # allows more rows to guess the type of data


# Convert Dates to proper date format -----
# Behind this date is the number of seconds since 1970 or number of days
sunapee.df  <- sunapee.df  %>% # tells R to use the sunapee dataframe and passes to next command
  mutate(ice_on_date = mdy(ice_on_date), # converts the date to date using mdy as Month Day Year format 
         ice_off_date = mdy(ice_off_date), # converts the date to date using mdy as Month Day Year format 
         ice_on_julian = yday(ice_on_date), # converts the date to date using mdy as Month Day Year format 
         ice_off_julian = yday(ice_off_date))


# Create new varaible for break point analsys -----
# if_else tells r to evaluate a column and do things
# if true is the first option if false is the second option
# can cahge 1970 to another year if you want a different break point
sunapee.df  <- sunapee.df  %>% # tells R to use the sunapee dataframe and passes to next command
         mutate(
           pre_post_1970 = ifelse(start_year <= 1970, "pre_1970", "post_1970") # if else to make breakpoint
           ) 


# Regression of ice_off_doy -----
# Work with lakes separately to calculate regression coefficients
# the first line of code below stores a linear model in an object called sunapee.model
# lm = linear model of ice_off_julian day is a function of start year 
# using the data from sunapee.df
sunapee.model <- lm(ice_off_julian ~ start_year, data = sunapee.df)

# Model Summary -----
# To see the data from the model you run this below
summary(sunapee.model)

# the output looks like 
# Call:
#   lm(formula = ice_off_julian ~ start_year, data = sunapee.df)
# 
# Residuals: # this is an estimate of the statistics of the residuals 
#   Min      1Q  Median      3Q     Max 
# -27.297  -5.122   1.268   6.250  16.644 
# 
# these are the coefficients 
# (Intercept) = the y intercept but since years are used the intercept is not very useful
# start_year = the slope of the line
# the (>|t|) is the probability the value is not 0 = means nothing really
# Coefficients:
#               Estimate    Std. Error  t value   Pr(>|t|)    
#   (Intercept) 294.35842     35.03231    8.402   3.82e-14 ***
#   start_year   -0.09269      0.01804   -5.138   8.87e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.187 on 144 degrees of freedom
# Multiple R-squared:  0.1549,	Adjusted R-squared:  0.1491 
# F-statistic:  26.4 on 1 and 144 DF,  p-value: 8.872e-07
#
# the above is important. 
# the F-statistic determines if the model is significant or not and this one is
# the square root of the R-stquared gives you the regression coefficient of r = 

# Condensed output----
# To get a reduced amount of infomation you can run the tidy version using Broom
tidy(summary(sunapee.model))

# Create a graph of the data-----
sunapee.df %>% # takes the dataframe and feeds it into the plot
  ggplot(aes(x = start_year, y = ice_off_julian)) + # tells ggplot to use variables for x and y (note the x=/y= are usually not necessary)
  geom_point() + # adds points for individual observations
  geom_smooth(method="lm") + # performs a linear model on the data with a 95% confidence interval shaded
  labs(y = "Ice off day (Julian Day)", # creates the y axis label
       x = "Year of observation" ) # creates the x axis label
 


# A prettier plot ----
# You can save the plot to an object as well
# the graph can be exported using the export button in the graph window or as below
sunapee.plot <- sunapee.df %>% # takes the dataframe and feeds it into the plot
  ggplot(aes(x = start_year, y = ice_off_julian)) + # tells ggplot to use variables for x and y (note the x=/y= are usually not necessary)
  geom_point(size = 3, color="darkblue", fill = "red", shape = 25) + # adds points for individual observations
  geom_smooth(method="lm") + # performs a linear model on the data with a 95% confidence interval shaded
  scale_x_continuous(
    limits = c(1860, 2020), # creates the limits of the axis - note will remove data not in range
    breaks = seq(1860, 2020, by = 20) # sets the lables on the axis in a sequence from and to with ticks by = 
  ) +
  labs(y = "Ice off day (Julian Day)", # creates the y axis label
       x = "Year of observation", # creates the x axis label
       title = "Lake Sunapee" # title
  ) + 
  theme( # this allows modification of the look of the graph with common settings
    axis.line = element_line(size = 0.4, linetype = "solid"), # color and size of axis lines
    axis.ticks = element_line(colour = "black",  size = 0.9),  # color and size of ticks
    axis.title = element_text(family = "Helvetica", size = 15, face = "bold"), #size of axis tit#les
    axis.text.x = element_text(family = "Helvetica", size = 16),  # size of x axis text overall
    axis.text.y = element_text(family = "Helvetica", size = 16),  # size of y axis text overall
    plot.title = element_text(family = "Helvetica", size = 19))  # if there is a title these are the settings

sunapee.plot

ggsave("figures/sunapee ice off doy vs year plot.pdf", # save the plot to this file in figures
       sunapee.plot, # which plot to save - change this to change the plot saved
       device = "pdf", # can export as "tiff", "jpeg", "pdf", and others
       width = 6, # width of the plot in units
       height = 6, # height of the plot in units
       units = "in" ) # units to be used




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 1970 Break point analysis -----
# there are a few ways to do this... 
# 1 create separate dataframes for pre and post and then do the above approach

# Extract only sunapee pre 1970 data ----
sunapee_pre1970.df <- sunapee.df %>% # use the sunapee data set 
  filter( # filter out some of the data to retain 
    pre_post_1970 == "pre_1970") # this filters out only pre_1970 data in the pre_post_1970 column
# all of this is saved in the sunapee_pre1970.df dataframe

# Extract only sunapee post 1970 data ----
sunapee_post1970.df <- sunapee.df %>% # use the sunapee data set 
  filter( # filter out some of the data to retain 
    pre_post_1970 == "post_1970") # this filters out only pre_1970 data in the pre_post_1970 column
# all of this is saved in the sunapee_pre1970.df dataframe

# Sunapee pre 1970 model ----
sunapee_pre1970.model <- lm(ice_off_julian ~ start_year, data = sunapee_pre1970.df)

# Model Summary -----
# To see the data from the model you run this below
summary(sunapee_pre1970.model)

# Call:
#   lm(formula = ice_off_julian ~ start_year, data = sunapee_pre1970.df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -26.992  -5.216   1.440   6.430  16.605 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 314.04384   61.73621   5.087 1.71e-06 ***
#   start_year   -0.10310    0.03216  -3.206  0.00181 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.563 on 100 degrees of freedom
# Multiple R-squared:  0.0932,	Adjusted R-squared:  0.08413 
# F-statistic: 10.28 on 1 and 100 DF,  p-value: 0.001808

# Sunapee pre 1970 model ----
sunapee_post1970.model <- lm(ice_off_julian ~ start_year, data = sunapee_post1970.df)

# Model Summary -----
# To see the data from the model you run this below
summary(sunapee_post1970.model)

# Call:
#   lm(formula = ice_off_julian ~ start_year, data = sunapee_post1970.df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -24.1395  -4.9908   0.5587   6.0430  15.9264 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 641.81466  191.59047   3.350  0.00172 **
#   start_year   -0.26674    0.09615  -2.774  0.00823 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.099 on 42 degrees of freedom
# Multiple R-squared:  0.1549,	Adjusted R-squared:  0.1347 
# F-statistic: 7.695 on 1 and 42 DF,  p-value: 0.008225


# 2 - the second way to do this without splititng the dataframe----
sunapee_pre_post.model <- sunapee.df %>% 
  group_by(pre_post_1970) %>% # this grpups the data into the groups listed in this variable
  do(pre_post.model = lm(ice_off_julian ~ start_year, # this does the model in two steps 
                         data = .)) # tells it to use the data fred into the model above

# this saves the coefficients out to a new dataframe
# and does teh summary of he model
# what model and the column to use of teh groups
sunapee_pre_post.coef <- tidy(sunapee_pre_post.model, pre_post.model) 
sunapee_pre_post.coef # prints out the dataframe

# Plotting pre and post 1970----
# you can see the graph with both lines
sunapee.df %>% 
  ggplot(aes(start_year, ice_off_julian, 
             color=pre_post_1970, # this maps a color to the points based on the column
             shape=pre_post_1970, # this maps a symbol shape to the column
             linetype=pre_post_1970)) + # this changes the line time based on the column
  geom_point() + # add in points
  geom_smooth(method="lm") # add in a smoothed line







# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# This is code to read in all the lake names as you want
# This pipes or strings all the commands into one set of lines
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
# this could make one file of all the lakes and can be easier to work with 
# will show how to work with all of the data at once below
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
  




