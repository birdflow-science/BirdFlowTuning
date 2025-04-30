library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/10.Check_pit_score/')

devtools::install_local("/home/yc85_illinois_edu/BirdFlowR", force = T, dependencies = FALSE) # if the BirdFlowR is updated, we need to reinstall it, so that i can be used in BirdFlowPipeline!
devtools::install_local("/home/yc85_illinois_edu/BirdFlowPipeline", force = T, dependencies = FALSE)

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

##
# tmp <- all_res_with_tracking[all_res_with_tracking['method']=='LL',] ## LL as model selection method
tmp <- all_res_with_tracking[all_res_with_tracking['method']=='ST_and_LL_log',] ## LL as model selection method
