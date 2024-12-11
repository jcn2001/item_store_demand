
library(tidyverse)
library(tidymodels) 
library(vroom)
library(forecast)
library(patchwork)
library(modeltime)
library(timetk)

train <- vroom("C:/Users/Josh/Documents/stat348/item_store_demand/train.csv")
test <- vroom("C:/Users/Josh/Documents/stat348/item_store_demand/test.csv")


nStores <- max(train$store)
nItems <- max(train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    ## split
    prophet_split <- time_series_split(storeItemTrain, assess="3 months", cumulative = TRUE)
    
    ## fit the model here 
    prophet_model <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(sales ~ date, data = training(prophet_split))
    
    # tune the models
    prophet_results <- modeltime_calibrate(prophet_model,
                                              new_data = testing(prophet_split))
    
    ## tune to whole dataset
    prophet_fullfit <- prophet_results %>%
      modeltime_refit(data=storeItemTrain)

    ## predict the storeitem sales
    preds <- prophet_fullfit %>%
      modeltime_forecast(
        new_data = storeItemTest,
        actual_data = storeItemTrain
      ) %>%
      filter(!is.na(.model_id)) %>%
      mutate(id=storeItemTest$id) %>%
      select(id, .value) %>%
      rename(sales=.value)
    
  ## save the preds
  if(s==1 & i ==1){
    all_preds <- preds
  } else {
    all_preds <- bind_rows(all_preds, preds)
  }
  }
}

vroom_write(all_preds, file = "./submission.csv", delim =",")

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(lightgbm)
library(bonsai)

## Read in the data
train <- vroom::vroom("/kaggle/input/demand-forecasting-kernels-only/train.csv")
test <- vroom::vroom("/kaggle/input/demand-forecasting-kernels-only/test.csv")
n.stores <- max(item$store)
n.items <- max(item$item)

## set up the model and workflow
item_recipe <- recipe(sales~.,data=train) %>%
  step-date(date, features=c("dow", "month", "decimal", "doy","year")) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())

boosted_model <- boost_tree(tree_depth = 2,
                            trees = 1000,
                            learn_rate=0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

boost_wf <- workflow() %>%
  add_recipe(item_recipe) %>%
  add_model(boosted_model)

for(s in 1:n.store){
  for(i in 1:n.items){
    
    # pick out one item store combo
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    # fit the data and forecast
    fitted_wf <- boost_wf %>%
      fit(data=storeItemTrain)
    
    preds <- predict(fitted_wf, new_data=storeItemTest) %>%
      bind_cols(storeItemTest) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    ## save the preds
    if(s==1 & i ==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

vroom_write(all_preds, file = "./submission.csv", delim =",")
