
library(tidyverse)
library(tidymodels) 
library(vroom)
library(forecast)
library(patchwork)

store_train <- vroom("C:/Users/Josh/Documents/stat348/item_store_demand/train.csv")
store_test <- vroom("C:/Users/Josh/Documents/stat348/item_store_demand/test.csv")

storeItem <- store_train %>%
  filter(store==1,item==1)

plot1 <- storeItem %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE) +
  ggtitle("Sales Over Time Item 1 1") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot2 <- storeItem %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("1 month ACF Plot Item 1 1") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot3 <- storeItem %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) +
  ggtitle("2 year ACF Plot Item 1 1") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

storeItem2 <- store_train %>%
  filter(store==2,item==11)

plot4 <- storeItem2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE) +
  ggtitle("Sales Over Time Item 2 11") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot5 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ggtitle("1 month ACF Plot Item 2 11") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot6 <- storeItem2 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) +
  ggtitle("1 month ACF Plot Item 2 11") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

hw27 <- (plot1 + plot2 + plot3) / (plot4 + plot5 + plot6)
ggsave("C:/Users/Josh/Documents/stat348/item_store_demand/hw27.png")

## hw 28 (start testing some models on individual items)
first_store_recipe <- recipe(sales~.,data=storeItem) %>%
  step_date(date, features="doy") %>%
  step_date(date, features="dow") %>%
  step_mutate(date_dow = factor(date_dow)) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=cos(date_doy)) %>%
  step_rm(store,item)

install.packages("ranger")
store_rf <- rand_forest(mtry = tune(),
                        min_n=tune(),
                        trees=1000) %>%
                        set_engine("ranger") %>%
                        set_mode("regression")

# set workflow
rf_wf <- workflow() %>%
  add_recipe(first_store_recipe) %>%
  add_model(store_rf)

# grid of values to tune over
grid_rf <- grid_regular(mtry(range=c(1,10)),
                              min_n(),
                              levels = 5)

# split data for CV
rf_folds <- vfold_cv(storeItem, v = 10, repeats=1)

# Run the CV
rf_CV_results <- rf_wf %>%
  tune_grid(resamples=rf_folds,
            grid=grid_rf,
            metrics=metric_set(smape))

best_rf_model <- rf_CV_results %>%
  show_best(metric = "smape")

best_rf_model

