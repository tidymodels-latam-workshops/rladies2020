#PCA

library(palmerpenguins)

library(tidymodels)
library(tidyverse)
penguins <- palmerpenguins::penguins 
# la variable cualitativa es species
penguins <- penguins %>%
  select(-sex, -year, -island)%>%
  drop_na()
library(recipes)

pca_rec <- recipe(~., data = penguins) %>%
  update_role(species, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

pca_prep

bake(pca_prep,new_data = penguins) %>%
  ggplot(aes(PC1, PC2, label = species)) +
  geom_point(aes(color = species), alpha = 0.7, size = 2) +
  labs(title="PCA from Palmer Penguins")

#UMAP
library(embed)

umap_rec <- recipe(~., data = penguins) %>%
  update_role(species, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

umap_prep

bake(umap_prep,new_data=penguins) %>%
  ggplot(aes(umap_1, umap_2, label = species)) +
  geom_point(aes(color = species), alpha = 0.7, size = 2) +
  labs(title="UMAP from Palmer Penguins")


# 