
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
  step_date(date, features = "month") %>%
  step_mutate(date_month = factor(date_month)) %>%
  step_mutate(date_dow = factor(date_dow)) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(cosDOY=cos(date_doy)) %>%
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

## ARIMA model
library(modeltime)
library(timetk)

## pick out my items
storeItemTrain <- store_train %>%
  filter(store==1,item==11)
storeItemTest <- store_test %>%
  filter(store==1,item==11)
storeItemTrain2 <- store_train %>%
  filter(store==2, item == 12)
storeItemTest2 <- store_test %>%
  filter(store==2, item == 12)

# Define the ARIMA model
arima_cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)
arima_cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

# create a recipe
arima_recipe <- recipe(sales~.,data=storeItemTrain) %>%
  step_date(date, features="doy") %>%
  step_date(date, features="dow") %>%
  step_date(date, features = "month") %>%
  step_rm(item,store)

## define the ARIMA model
arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5,
                         non_seasonal_ma=5,
                         seasonal_ar=2,
                         seasonal_ma=2,
                         non_seasonal_differences=2,
                         seasonal_differences=2
                         ) %>%
  set_engine("auto_arima")

# put into a single workflow and fit to the training data
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(arima_cv_split))

# tune the models
arima_cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(arima_cv_split))

## visualize the results
p1 <- arima_cv_results %>%
  modeltime_forecast(
    new_data = testing(arima_cv_split),
    actual_data = training(arima_cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## tune to whole dataset
fullfit <- arima_cv_results %>%
  modeltime_refit(data=storeItemTrain)

## predict for the whole dataset
p2 <- fullfit %>%
  modeltime_forecast(
    new_data = storeItemTest,
    actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)

# let's do this again for another store item combo
# Define the ARIMA model
arima_cv_split2 <- time_series_split(storeItemTrain2, assess="3 months", cumulative = TRUE)
arima_cv_split2 %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

# create a recipe
arima_recipe2 <- recipe(sales~.,data=storeItemTrain2) %>%
  step_date(date, features="doy") %>%
  step_date(date, features="dow") %>%
  step_date(date, features = "month") %>%
  step_rm(store, item)

# put into a single workflow and fit to the training data
arima_wf <- workflow() %>%
  add_recipe(arima_recipe2) %>%
  add_model(arima_model) %>%
  fit(data=training(arima_cv_split2))

# tune the models
arima_cv_results2 <- modeltime_calibrate(arima_wf,
                                        new_data = testing(arima_cv_split2))

## visualize the results
p3 <- arima_cv_results2 %>%
  modeltime_forecast(
    new_data = testing(arima_cv_split2),
    actual_data = training(arima_cv_split2)
  ) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## tune to whole dataset
fullfit <- arima_cv_results2 %>%
  modeltime_refit(data=storeItemTrain2)

## predict for the whole dataset
p4 <- fullfit %>%
  modeltime_forecast(
    new_data = storeItemTest2,
    actual_data = storeItemTrain2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## let's put the plots together
plotly::subplot(p1,p3,p2,p4,nrows=2)



## hw 31 facebook prophet model
prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(arima_cv_split))

# tune the models
prophet_arima_cv_results <- modeltime_calibrate(prophet_model,
                                        new_data = testing(arima_cv_split))

## visualize the results
prophet_p1 <- prophet_arima_cv_results %>%
  modeltime_forecast(
    new_data = testing(arima_cv_split),
    actual_data = training(arima_cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## tune to whole dataset
prophet_fullfit <- prophet_arima_cv_results %>%
  modeltime_refit(data=storeItemTrain)

## predict for the whole dataset
prophet_p2 <- prophet_fullfit %>%
  modeltime_forecast(
    new_data = storeItemTest,
    actual_data = storeItemTrain) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## now for the other store item combo we'll do the same thing
prophet_model2 <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(arima_cv_split2))

# tune the models
prophet_arima_cv_results2 <- modeltime_calibrate(prophet_model2,
                                                new_data = testing(arima_cv_split2))
## visualize the results
prophet_p3 <- prophet_arima_cv_results2 %>%
  modeltime_forecast(
    new_data = testing(arima_cv_split2),
    actual_data = training(arima_cv_split2)
  ) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## tune to whole dataset
prophet_fullfit2 <- prophet_arima_cv_results2 %>%
  modeltime_refit(data=storeItemTrain2)

## predict for the whole dataset
prophet_p4 <- prophet_fullfit2 %>%
  modeltime_forecast(
    new_data = storeItemTest2,
    actual_data = storeItemTrain2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## let's put the plots together
hw31 <- plotly::subplot(prophet_p1,prophet_p3,prophet_p2,prophet_p4,nrows=2)


