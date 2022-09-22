
#Load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(mosaicData)) install.packages("mosaicData", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(arsenal)) install.packages("arsenal", repos = "http://cran.us.r-project.org")
if(!require(rlang)) install.packages("rlang", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tidymodels)) install.packages("tidymodels", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")
if(!require(tictoc)) install.packages("tictoc", repos = "http://cran.us.r-project.org")
if(!require(vip)) install.packages("vip", repos = "http://cran.us.r-project.org")
if(!require(remotes)) install.packages("remotes", repos = "http://cran.us.r-project.org")
if(!require(tinytext)) install.packages("tinytext", repos = "http://cran.us.r-project.org")
options(tinytex.verbose = TRUE)

library(readr)
library(dplyr)
library(skimr)
library(ggplot2)
library(mosaicData) #for correlation plot
library(ggcorrplot) #for correlation
library(arsenal)
library(mosaicData)
library(rlang)
library(stringr)
library(rmarkdown)
library(knitr)
library(tidymodels)
library(forcats)
library(doParallel)
library(tictoc)  
library(vip)
library(remotes)
library(tinytex)
options( tinytex.verbose = TRUE)


#Load arabica
arabica <- read_csv("arabica_data_cleaned.csv")
dim(arabica)

#Count total missing values in each column
sapply(arabica, function(x) sum(is.na(x)))

#Calculate percentage of missing values
colSums(is.na(arabica)) / nrow(arabica)

#Lot.Number has 0.7940503432 NAs, this is too high so it must be removed
arabica_clean <- arabica[ , ! names(arabica) %in% c("Lot.Number")]  

arabica_clean <- arabica_clean %>% 
  mutate(id = row_number()) %>% #creata id column
  select(id, everything()) 
arabica_clean <- subset(arabica_clean, select = -c(...1 ))

#explore the dataset
skim(arabica_clean)
summary(arabica_clean)
colnames(arabica_clean)
dim(arabica_clean)

#Fix altitude because the summary function shows max mean altidue = 190164, this is not
#possible since the highest mountains in the world are not that high, for example the height of Mount Everest is 8,848 (Wikipedia Contributors, 2019).
coffees_altitude <- arabica_clean %>% 
  select(altitude_mean_meters, Total.Cup.Points) %>% 
  filter(altitude_mean_meters <= 10000) %>%  
  group_by(altitude_mean_meters) %>% 
  summarise(cup_points = mean(Total.Cup.Points)) %>% 
  arrange(cup_points) %>% 
  arrange(desc(cup_points))
coffees_altitude

altitude_points <- coffees_altitude %>%
  ggplot(aes(altitude_mean_meters, cup_points)) +
  geom_point(aes(color=cup_points), size = 4) +
  ggtitle("Altitude Mean Meters compared to Average Cup Points")+
  theme_gray()+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
altitude_points
#I would not say that altitude is super relevant, but most coffees rated between 80 and 85 are produced at less than 2000 meters and some high rated coffees are produced between 2000 and 3000 meters.

#As a general approach, I want to check if there is any relationship between coffee Variety and Total.Cup.Points. 

#Create scatterplot of variety vs. Total.Cup.Points, but most values are higher than 75 and there is an obvious outlier.
p <- ggplot(data=arabica_clean, aes(x= Variety, y= Total.Cup.Points, color= Variety)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p


#Check boxplots, and here we see that some varieties stand out in terms of total cup points
l <- ggplot(data=arabica_clean, aes(x= Variety, y= Total.Cup.Points, color = Variety)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
l


#Barplot of Varieties versus count of total cup points
m <- ggplot(arabica_clean, aes(x= Variety, fill= Total.Cup.Points))+
  geom_bar(fill = "brown")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
m

#To make things easier, I rename the dataset
arab_coffee <- arabica_clean


#RATINGS
#Total.Cup.Points (quality rating 0-100) is relevant since it is a standard metric for coffee reviews. For example, according to Coffeereview website (n.d.), explains that coffee ratings higher than 97 equal to the best cups, while ratings of 85/86 are barely acceptable.Here I plot the outliers (three zeros) we have previously identified.

p <- arab_coffee %>%
  ggplot(aes(x = Total.Cup.Points)) +
  geom_boxplot(y = 0, fill = "brown", outlier.shape = NA) +
  geom_jitter(aes(y = 1), color = "brown",
              alpha = 0.3, height = 0.3, width = 0) 
p

arab_coffee  %>%
  filter(Total.Cup.Points == min(Total.Cup.Points)) %>%
  glimpse()

arab_coffee <- arab_coffee %>% filter(Total.Cup.Points > 0)
p %+% arab_coffee

arab_coffee %>%
  ggplot(aes(x = Moisture)) +
  geom_boxplot(y = 0, fill = "brown", outlier.shape = NA) +
  geom_jitter(aes(y = 1), color = "brown",
              alpha = 0.3, height = 0.3, width = 0) 

#Run a correlation to identify correlations between coffee variables
# select numeric variables
df <- dplyr::select_if(arab_coffee, is.numeric)

#Calculate the correlations
r <- cor(df, use="complete.obs")
round(r,2)

ggcorrplot(r)

#There are relevant positive and negative correlations, especially related to Total.Cup.Points and aroma, flavor, aftertaste acidity, body, and balance. Therefore, I
#try to understand other relationships with high ratings and country of production.

coffees_reduced <- arab_coffee %>% select(Species, Country.of.Origin, Number.of.Bags,Total.Cup.Points) %>% 
  group_by(Country.of.Origin) %>% 
  summarise(cup_points = mean(Total.Cup.Points)) %>% 
  print(n=37) %>% 
  arrange(cup_points)

coffees_reduced <- na.omit(coffees_reduced) 


coffee_points <- coffees_reduced %>%
  ggplot(aes(Country.of.Origin, cup_points)) +
  geom_point(aes(color=cup_points), size = 4) +
  ggtitle("Country of Origin compared to Average Cup Points")+
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
coffee_points
#Coffee beans produced in Etiopia, Ecuador, Japan, Panama, Papua New Guinea, United Statese, Kenya, Panama, and Uganda have the highest ratings. I did not know that Japan produces coffee, but according to Taylor (2020), the japanese coffee industry is one of the best in the world.

#Check if Country.Partner is related to high ratings. Meta D agricultural development plc is the most appreciated partner and they have received high ratings.
coffees_partners <- arab_coffee %>% 
  select(In.Country.Partner, Total.Cup.Points) %>% 
  group_by(In.Country.Partner) %>% 
  summarise(cup_points = mean(Total.Cup.Points)) %>% 
  print(n=37) %>% 
  arrange(cup_points)

partners_points <- coffees_partners %>%
  ggplot(aes(In.Country.Partner, cup_points)) +
  geom_point(aes(color=cup_points), size = 4) +
  ggtitle("In Country Partners compared to Average Cup Points")+
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
partners_points

#Coffee beans colors; blue-green and bluish-green beans seem slightly better than green #beans.
coffees_color <- arab_coffee %>% 
  select(Color, Total.Cup.Points) %>% 
  group_by(Color) %>% 
  summarise(cup_points = mean(Total.Cup.Points)) %>% 
  arrange(cup_points)
coffees_color <- na.omit(coffees_color)

color_points <- coffees_color %>%
  ggplot(aes(Color, cup_points)) +
  geom_point(aes(color=cup_points), size = 4) +
  ggtitle("Coffee Beans Color compared to Average Cup Points")+
  theme_gray()+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
color_points

#Harvest_years are grouped, split them.
arab_coffee <- arab_coffee %>%
  mutate(
    harvest_year_num = Harvest.Year %>%
      str_extract("\\d{4}") %>%
      as.numeric()
  )
arab_coffee %>%
  count(Harvest.Year, harvest_year_num, sort = T) %>%
  paged_table()

#Processing method and coffee cup points; the "pulped natural/honey" method has high ratings.
coffees_method <- arab_coffee %>% 
  select(Processing.Method, Total.Cup.Points) %>% 
  group_by(Processing.Method) %>% 
  summarise(cup_points = mean(Total.Cup.Points)) %>% 
  arrange(cup_points)
coffees_method

coffees_method <- na.omit(coffees_method) #remove NAs

method_points <- coffees_method %>%
  ggplot(aes(Processing.Method, cup_points)) +
  geom_point(aes(color=cup_points), size = 4) +
  ggtitle("Processing Method compared to Average Cup Points")+
  theme_gray()+
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
method_points


#Comparison between number of bags produced and quality or ratings. There is not a clear #relationship between number of bags produced and ratings, since the average amount produced #is below 300 bags (with a few exceptions of around 600 and more than 900 bags).
coffees_bags <- arab_coffee %>% 
  select(Number.of.Bags, Total.Cup.Points) %>%
  group_by(Number.of.Bags) %>% 
  summarise(cup_points = mean(Total.Cup.Points)) %>% 
  arrange(cup_points)
coffees_bags

bags_points <- coffees_bags %>%
  ggplot(aes(Number.of.Bags, cup_points)) +
  geom_point(aes(color=cup_points), size = 4) +
  ggtitle("Number of Bags produced compared to Average Cup Points")+
  theme_gray()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
bags_points

#Models

set.seed(74)
sample(c("Aroma", "Flavor", "Aftertaste", "Acidity", "Body", "Balance","Cupper_Points"),
       size = 1)

#Load tidymodels and split the dataset using train/test functions. We do this to avoid over #optimistic predictions (overfitting); this problem might occur after using the same dataset to make predictions. We know that a model is accurate when error rate is low (Theobald, 2017 p. 48).


arab_coffee <- arab_coffee %>% #to give missing values factor level
  mutate(
    Variety = fct_explicit_na(Variety),
    across(where(is.character), factor))

#Theobald recommmends splittings of 70/30 or 80/2, also considering the size of #the dataset; so I choose 75/25 (2017, p. 46). 

set.seed(42)
coffee_split <- initial_split(arab_coffee, prop = 3/4, strata = Flavor)
coffee_train <- training(coffee_split)
coffee_test <- testing(coffee_split)

#In Tidymodels the vfold_cv splits data into V equal groups
coffee_resamples <- vfold_cv(coffee_train, v = 5, strata = Flavor)


#Recipe. In Tidymodels, recipes are used for feature engineering, to prepare data before using them (Silge & Kuhn, 2022, chapter 8). I choose the variables as per correlation plot, with flavor among the most important ones together with aroma and aftertaste.
coffee_rec <-
  recipe(
    Flavor ~
      Country.of.Origin + Processing.Method + Color +
      In.Country.Partner + Variety + Aroma + Aftertaste + Acidity + Body +
      Balance + Uniformity + Clean.Cup + Sweetness + Moisture +
      altitude_mean_meters,
    data = coffee_train
  ) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_other(Country.of.Origin, Variety, In.Country.Partner, Processing.Method,
             threshold = 0.05) %>%
  step_impute_mean(altitude_mean_meters) %>%
  step_normalize(all_numeric_predictors()) %>% #normalize variables
  step_ns(altitude_mean_meters, deg_free = 4) %>%
  step_dummy(all_nominal_predictors())
coffee_rec
#(Dunn, 2020)

#Use prep to explore the processing and to apply the preprocessing to the datasets (Silge & Kuhn, 2022, chapter 16.4).
coffee_baked <- bake(prep(coffee_rec), new_data = NULL)
coffee_baked %>% paged_table()

#Use DoParallel to register execution of R code
n_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(n_cores - 1)
registerDoParallel(cl)


lm_spec <- linear_reg() %>%
  set_engine("lm")

lm_workflow <- workflow() %>%
  add_recipe(coffee_rec) %>%
  add_model(lm_spec)

lm_fit_train <- lm_workflow %>%
  fit(data = coffee_train)

lm_fit <- last_fit(lm_fit_train, coffee_split) #final fit

collect_metrics(lm_fit)

#Comparison between prediction and actual data
collect_predictions(lm_fit) %>%
  ggplot(aes(x = Flavor, y = .pred)) +
  geom_point(color = "brown", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, size = 1.5)

#Since the result from the Linear regression is good, I go ahead with Random Forest.


#Random Forest

ranger_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")

ranger_workflow <- workflow() %>%
  add_recipe(coffee_rec) %>%
  add_model(ranger_spec)

set.seed(12)

#tic toc functions from tictoc package, to nest and timing my function.
tic()
ranger_tune <-
  tune_grid(ranger_workflow, resamples = coffee_resamples, grid = 11)
toc()

#Tune function to find optimal paramethers
show_best(ranger_tune, metric = "rmse")

autoplot(ranger_tune) 

#Result are consistent with the previous RMSE and model.
ranger_best <- ranger_workflow %>%
  finalize_workflow(select_best(ranger_tune, metric = "rmse"))

ranger_fit <- last_fit(ranger_best, coffee_split)
collect_metrics(ranger_fit)

#According to this model, the following graph shows the most important variables.
ranger_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme(legend.position = c(0.3, 0.3))

#LASSO

#Formula is: Lasso regression error = Regression error + L1 norm

lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")


lasso_lambda_grid <- grid_regular(penalty(), levels = 50) #specify the amount of regularization to use (Silge & Kuhn, 2022)

lasso_workflow <- workflow() %>%
  add_recipe(coffee_rec) %>%
  add_model(lasso_spec)


tic()
lasso_tune <-
  tune_grid(
    lasso_workflow,
    resamples = coffee_resamples,
    grid = lasso_lambda_grid
  )
toc()

show_best(lasso_tune, metric = "rmse")

#RMSE and RSQ using LASSO
collect_metrics(lasso_tune) %>%
  #filter(!is.na(std_err)) %>%
  ggplot(aes(x = penalty, y = mean)) +
  geom_line(size = 1, color = "brown") +
  geom_point(color = "brown") +
  geom_ribbon(aes(ymin = mean - std_err, ymax = mean + std_err),
              alpha = 0.5, fill = "brown") +
  facet_wrap(~.metric, ncol = 1, scales = "free_y") +
  scale_x_log10()

#Results
lasso_best_workflow <- lasso_workflow %>%
  finalize_workflow(select_best(lasso_tune, metric = "rmse"))
lasso_fit <- last_fit(lasso_best_workflow, coffee_split)

collect_metrics(lasso_fit)

#Check again which variables are the most important according to LASSO Model. The first #results are similar to the ones I have obtained using Random Forest, but this model adds
#a negative correlation with Colombia as a Country of Origin, and poisitive correlations #with the partner called ""In Country Partner Asociacion Del Café", with clean cup, and #Mexico as a country of origin.

lasso_fit %>%
  extract_fit_engine() %>%
  vi(
    lambda =  select_best(lasso_tune, metric = "rmse")$penalty
  ) %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme(legend.position = c(0.3, 0.3))


