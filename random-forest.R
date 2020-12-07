#decision trees palmer penguins
library(tidymodels)
library(palmerpenguins)

penguins <- palmerpenguins::penguins %>%
  drop_na() %>%
  select(-year,-sex, -island)


set.seed(123)
p_split <- penguins %>%
  initial_split(prop=0.75)

p_train <- training(p_split)
p_test  <- testing(p_split)

# para hacer validación cruzada
so_folds <- vfold_cv(p_train, strata = species)


#especifico el modelo 
set.seed(123)
vanilla_tree_spec <-
  decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

#modelo vanilla sin tunning
set.seed(123)
vanilla_tree_spec %>% 
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()

#tenemos 3 parametros para hacer tuneo
#tree_depth
#min_n
#cost_complexity


#tunning de hiperparametros
trees_spec <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(min_n = 20, cost_complexity = 0.1)

trees_spec %>%
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()

--------------------------------------------#vainilla random forest-------------------------
# random forest

rf_spec <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

set.seed(123)

rf_spec %>% 
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()

------------------------------------------#random forest----------------------------------------------------
# con mtry = 3

rf3_spec <- rf_spec %>% 
  set_args(mtry = 3)
set.seed(123)
rf3_spec %>% 
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()


# con mtry = 2

rf2_spec <- rf_spec %>% 
  set_args(mtry = 2)

set.seed(123)

rf2_spec %>% 
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()



# con mtry = 4

rf4_spec <- rf_spec %>% 
  set_args(mtry = 4)

set.seed(123)

rf4_spec %>% 
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()

----------------------------------#ejercicio para ellos----------------------------------


----------------------------------#random forest tuneo automatico-------------------------

p_recipe <- training(p_split) %>%
  recipe(species~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()


tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


tune_wf <- workflow() %>%
  add_recipe(p_recipe) %>%
  add_model(tune_spec)

set.seed(234)
trees_folds <- vfold_cv(p_train, strata = species)

doParallel::registerDoParallel()

set.seed(123)
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res


#veamos graficamente el tuneo de estos hiperparámetros
#esto para tener idea los rangos de variación para hacer un tuneo más fino
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


#seleccionamos el mejor modelo

best_auc <- select_best(tune_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

#importancia de las variables
library(vip)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(species ~ .,
      data = juice(p_recipe)) %>%
  vip(geom = "point")
    

final_wf <- workflow() %>%
  add_recipe(p_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(p_split)

final_res %>%
  collect_metrics()

#hemos obtenido el 98% de accuracy en el test set
--------------------------------#aca finaliza el modelo----------------------------------

----------------------------#decision trees--------------------------------------------
set.seed(123)
trees_0_5 <- decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(min_n = 5, cost_complexity = 0)

trees_20_1 %>%
  fit_resamples(species ~ ., 
                resamples = so_folds) %>% 
  collect_metrics()

--------------------------#testeo------------------------------------------------------
#copiar el workflow

recipe_dt <- training(p_split) %>%
  recipe(species~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
recipe_dt

tree_wf_0_5 <- workflow() %>%
  add_recipe(recipe_dt) %>%
  add_model(trees_0_5)
tree_wf_0_5

set.seed(123)
final_fit_dt <- last_fit(
  tree_wf_0_5,
  split = p_split
)
final_fit_dt %>%
  collect_metrics()

final_fit_dt %>%
  collect_predictions() %>%
  conf_mat(species, .pred_class)  