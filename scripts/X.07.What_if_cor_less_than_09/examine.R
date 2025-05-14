library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

# setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/07.What_if_cor_less_than_09/')

# devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
# devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

load_all("/home/yc85_illinois_edu/BirdFlowPipeline") # if only r script is changed, you can do it. Otherwise reinstall.
load_all("/home/yc85_illinois_edu/BirdFlowR") # if only r script is changed, you can do it. Otherwise reinstall.

source('../02.Summarize_validation_preliminary/load_data_functions.R')

## load data
res <- load_raw_validation_all_sp()
raw_combined <- res[['raw_combined']]
raw_combined_with_tracking <-  res[['raw_combined_with_tracking']]

res <- load_best_models_validation_all_sp(raw_combined, raw_combined_with_tracking)
all_res <- res[['all_res']]
all_res_with_tracking <- res[['all_res_with_tracking']]

## with cor-S&T > 0.9 and. < 0.9, will the model perform differently?
library(ggpubr)
tmp <- all_res[all_res['method']=='LL',]
tmp$level_end_traverse_cor <- as.factor(ifelse(tmp$end_traverse_cor>0.9,'High cor','Low cor'))
ggplot(data=tmp, aes(x=level_end_traverse_cor, y=weighted_mean_ll_improvement)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", label = "p.format")


## Amount of data and corST? Is the low cor with S&T due to overfitting?
tmp <- all_res[all_res['method']=='LL',]
tmp$level_end_traverse_cor <- as.factor(ifelse(tmp$end_traverse_cor>0.9,'High cor','Low cor'))
ggplot(data=tmp, aes(x=level_end_traverse_cor, y=training_n_intervals)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test", label = "p.format")
# So not overfiting? but maybe it's because of ranges? for Canada and Alaska we may not have banding or MOTUS data? Should we do ST09+LL? Or ST09+Energy_Score?

## Using LL or energy_socre -- does it make a difference?
hist(all_res[all_res['method']=='energy_score',]$end_traverse_cor - 
       all_res[all_res['method']=='LL',]$end_traverse_cor, breaks=50)
# So LL is doing better for corS&T, actually. But not much.


#### look into specific species
##
tmp <- all_res[all_res['method']=='LL',] ## LL as model selection method
tmp <- tmp[tmp$end_traverse_cor < 0.8,]

for (line_count in 1:dim(tmp)[1]) {
  sp <- tmp[line_count, ]$sp
  print(sp)
  # model <- tmp[line_count, ]$model
  # model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model}')
  # model <- import_birdflow(model_path)
  # my_routes <- route(model, n=100)
  # p <- plot(my_routes)
  # print(p)
}

###
tmp <- all_res[all_res['method']=='LL',] ## LL as model selection method
tmp <- tmp[(tmp$end_traverse_cor > 0.8) & (tmp$end_traverse_cor < 0.9),]

for (line_count in 1:dim(tmp)[1]) {
  sp <- tmp[line_count, ]$sp
  print(sp)
  # model <- tmp[line_count, ]$model
  # model_path <- glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/{sp}/{sp}_150km/{model}')
  # model <- import_birdflow(model_path)
  # my_routes <- route(model, n=100)
  # p <- plot(my_routes)
  # print(p)
}






