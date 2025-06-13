library(dplyr)

# Load data
res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
res <- res[res$method=='ST098_and_LL',]
res <- res[res$training_n_intervals>20,]

### First, data composition
print(paste0('Total species modeled: ', length(res$sp |> unique())))
print(paste0('Total orders covered: ', length(res$ORDER1_eBird |> unique())))
print(paste0('Total families covered: ', length(res$FAMILY1_eBird |> unique())))

res <- res |> group_by(sp) |> slice_head(n=1) # Remove redundant lines

print(paste0('Min training transitions: ', min(res$training_n_intervals)))
print(paste0('Max training transitions: ', max(res$training_n_intervals)))
print(paste0('Mean training transitions: ', mean(res$training_n_intervals)))

print(paste0('Banding proportional contribution across species: ', mean(res$train_n_banding / res$training_n_intervals)))
print(paste0('Banding proportional contribution across species, min max: ', min(res$train_n_banding / res$training_n_intervals), ' - ',
             max(res$train_n_banding / res$training_n_intervals)))
print(paste0('Banding total contribution: ', sum(res$train_n_banding) / sum(res$training_n_intervals)))

print(paste0('Motus proportional contribution across species: ', mean(res$train_n_motus / res$training_n_intervals)))
print(paste0('Motus proportional contribution across species, min max: ', min(res$train_n_motus / res$training_n_intervals), ' - ',
             max(res$train_n_motus / res$training_n_intervals)))
print(paste0('Motus total contribution: ', sum(res$train_n_motus) / sum(res$training_n_intervals)))

print(paste0('Tracking proportional contribution across species: ', mean(res$train_n_tracking / res$training_n_intervals)))
print(paste0('Tracking proportional contribution across species, min max: ', min(res$train_n_tracking / res$training_n_intervals), ' - ',
             max(res$train_n_tracking / res$training_n_intervals)))
print(paste0('Tracking total contribution: ', sum(res$train_n_tracking)/ sum(res$training_n_intervals)))

## Species that use Tracking data as the majority data:
res |> dplyr::group_by(sp) |> 
  dplyr::slice_sample(n=1) |> 
  dplyr::ungroup() |> 
  dplyr::filter(train_n_tracking>0) |> 
  dplyr::mutate(train_prop = train_n_tracking / training_n_intervals) |> 
  dplyr::pull(train_prop)


### Second, LL compared to null_LL
res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
res <- res[res$method=='ST098_and_LL',]
res <- res[res$training_n_intervals>20,]
res <- res |> group_by(sp) |> slice_head(n=1)

print(paste0('Avergae LL improvement: ', mean(res$weighted_mean_ll_improvement)))
print(paste0('Min LL improvement: ', min(res$weighted_mean_ll_improvement)))
print(paste0('Max LL improvement: ', max(res$weighted_mean_ll_improvement)))
print(paste0('quantile LL improvement: ', quantile(res$weighted_mean_ll_improvement, c(0.025, 0.975))))

print(paste0('Avergae win distance: ', mean(res$weighted_mean_win_distance)))
print(paste0('Min win distance: ', min(res$weighted_mean_win_distance)))
print(paste0('Max win distance: ', max(res$weighted_mean_win_distance)))
print(paste0('quantile LL improvement: ', quantile(res$weighted_mean_win_distance, c(0.025, 0.975))))


### Third, LOO
res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
# res <- res[res$method=='ST098_and_LL',]
res <- res[res$training_n_intervals>20,]
# res <- res |> group_by(sp) |> slice_head(n=1)

LOO_summary <- res |>
  group_by(sp) |>
  summarize(
    LL_sp_specific_over_LOO =
      first(weighted_mean_ll_improvement[method == "ST098_and_LL"]) -
      first(weighted_mean_ll_improvement[method == "LOO_ST098_and_LL"]),
    LL_familly_LOO_over_LOO =
      first(weighted_mean_ll_improvement[method == "LOO_FAMILY_ST098_and_LL"]) -
      first(weighted_mean_ll_improvement[method == "LOO_ST098_and_LL"]),
    LL_order_LOO_over_LOO =
      first(weighted_mean_ll_improvement[method == "LOO_ORDER_ST098_and_LL"]) -
      first(weighted_mean_ll_improvement[method == "LOO_ST098_and_LL"]),
    distance_gain_sp_specific_over_LOO =
      first(weighted_mean_win_distance[method == "ST098_and_LL"]) -
                 first(weighted_mean_win_distance[method == "LOO_ST098_and_LL"]),
    distance_gain_familly_LOO_over_LOO =
      first(weighted_mean_win_distance[method == "LOO_FAMILY_ST098_and_LL"]) -
                 first(weighted_mean_win_distance[method == "LOO_ST098_and_LL"]),
    distance_gain_order_LOO_over_LOO =
      first(weighted_mean_win_distance[method == "LOO_ORDER_ST098_and_LL"]) -
                 first(weighted_mean_win_distance[method == "LOO_ST098_and_LL"]),
    rela_distance_gain_sp_specific_over_LOO =
      first(weighted_mean_win_distance_fraction[method == "ST098_and_LL"]) -
                 first(weighted_mean_win_distance_fraction[method == "LOO_ST098_and_LL"]),
    rela_distance_gain_familly_LOO_over_LOO =
      first(weighted_mean_win_distance_fraction[method == "LOO_FAMILY_ST098_and_LL"]) -
                 first(weighted_mean_win_distance_fraction[method == "LOO_ST098_and_LL"]),
    rela_distance_gain_order_LOO_over_LOO =
      first(weighted_mean_win_distance_fraction[method == "LOO_ORDER_ST098_and_LL"]) -
                 first(weighted_mean_win_distance_fraction[method == "LOO_ST098_and_LL"]),
    
    both_better_in_LL_and_distance_gain_sp_specific_over_LOO = 
      (first(weighted_mean_ll_improvement[method == "ST098_and_LL"]) -
                  first(weighted_mean_ll_improvement[method == "LOO_ST098_and_LL"]) > 0) & 
      (first(weighted_mean_win_distance_fraction[method == "ST098_and_LL"]) -
                  first(weighted_mean_win_distance_fraction[method == "LOO_ST098_and_LL"]) > 0),
    both_better_in_LL_and_distance_gain_familly_LOO_over_LOO = 
      (first(weighted_mean_ll_improvement[method == "LOO_FAMILY_ST098_and_LL"]) -
                  first(weighted_mean_ll_improvement[method == "LOO_ST098_and_LL"]) > 0) & 
      (first(weighted_mean_win_distance_fraction[method == "LOO_FAMILY_ST098_and_LL"]) -
                  first(weighted_mean_win_distance_fraction[method == "LOO_ST098_and_LL"]) > 0),
    both_better_in_LL_and_distance_gain_order_LOO_over_LOO = 
      (first(weighted_mean_ll_improvement[method == "LOO_ORDER_ST098_and_LL"]) -
                  first(weighted_mean_ll_improvement[method == "LOO_ST098_and_LL"]) > 0) & 
      (first(weighted_mean_win_distance_fraction[method == "LOO_ORDER_ST098_and_LL"]) -
                  first(weighted_mean_win_distance_fraction[method == "LOO_ST098_and_LL"]) > 0)
  ) |>
  ungroup()

# print('Comparing LOO methods:')
# print(LOO_summary[,-1] |> colMeans())
# print(LOO_summary[,-1] |> colSums())

print(paste0('Number of species species-specific > ALL-LOO: ', sum(LOO_summary$LL_sp_specific_over_LOO |> na.omit() > 0)))
print(paste0('Proportion of species species-specific > ALL-LOO: ', mean(LOO_summary$LL_sp_specific_over_LOO |> na.omit() > 0)))

print(paste0('Number of species Family-LOO > ALL-LOO: ', sum(LOO_summary$LL_familly_LOO_over_LOO |> na.omit()> 0)))
print(paste0('Proportion of species Family-LOO > ALL-LOO: ', mean(LOO_summary$LL_familly_LOO_over_LOO |> na.omit()> 0)))

print(paste0('Number of species Order-LOO > ALL-LOO: ', sum(LOO_summary$LL_order_LOO_over_LOO |> na.omit()> 0)))
print(paste0('Proportion of species Order-LOO > ALL-LOO: ', mean(LOO_summary$LL_order_LOO_over_LOO|> na.omit() > 0)))


print(paste0('LL improvement: species species-specific - ALL-LOO: ', mean(LOO_summary$LL_sp_specific_over_LOO |> na.omit())))
print(paste0('LL improvement range: species species-specific - ALL-LOO: ', 
             min(LOO_summary$LL_sp_specific_over_LOO |> na.omit()), ' - ',
             max(LOO_summary$LL_sp_specific_over_LOO |> na.omit())))
print(paste0('LL improvement quantile: species species-specific - ALL-LOO: ', 
             quantile(LOO_summary$LL_sp_specific_over_LOO |> na.omit(), c(0.025, 0.975))))

print(paste0('Reliative distance gain: species species-specific - ALL-LOO: ', mean(LOO_summary$rela_distance_gain_sp_specific_over_LOO |> na.omit())))
print(paste0('Reliative distance gain range: species species-specific - ALL-LOO: ', 
             min(LOO_summary$rela_distance_gain_sp_specific_over_LOO |> na.omit()), ' - ',
             max(LOO_summary$rela_distance_gain_sp_specific_over_LOO |> na.omit())))
print(paste0('Reliative distance gain quantile: species species-specific - ALL-LOO: ', 
             quantile(LOO_summary$rela_distance_gain_sp_specific_over_LOO |> na.omit(), c(0.025, 0.975))))


print(paste0('LL improvement: Family-LOO - ALL-LOO: ', mean(LOO_summary$LL_familly_LOO_over_LOO |> na.omit())))
print(paste0('LL improvement range: Family-LOO - ALL-LOO: ', 
             min(LOO_summary$LL_familly_LOO_over_LOO |> na.omit()), ' - ',
             max(LOO_summary$LL_familly_LOO_over_LOO |> na.omit())))
print(paste0('LL improvement quantile: Family-LOO - ALL-LOO: ', 
             quantile(LOO_summary$LL_familly_LOO_over_LOO |> na.omit(), c(0.025, 0.975))))

print(paste0('Reliative distance gain: Family-LOO - ALL-LOO: ', mean(LOO_summary$rela_distance_gain_familly_LOO_over_LOO |> na.omit())))
print(paste0('Reliative distance gain range: Family-LOO - ALL-LOO: ', 
             min(LOO_summary$rela_distance_gain_familly_LOO_over_LOO |> na.omit()), ' - ',
             max(LOO_summary$rela_distance_gain_familly_LOO_over_LOO |> na.omit())))
print(paste0('Reliative distance gain quantile: Family-LOO - ALL-LOO: ', 
             quantile(LOO_summary$rela_distance_gain_familly_LOO_over_LOO |> na.omit(), c(0.025, 0.975))))


print(paste0('LL improvement: Order-LOO - ALL-LOO: ', mean(LOO_summary$LL_order_LOO_over_LOO |> na.omit())))
print(paste0('LL improvement range: Order-LOO - ALL-LOO: ', 
             min(LOO_summary$LL_order_LOO_over_LOO |> na.omit()), ' - ',
             max(LOO_summary$LL_order_LOO_over_LOO |> na.omit())))
print(paste0('LL improvement quantile: Order-LOO - ALL-LOO: ', 
             quantile(LOO_summary$LL_order_LOO_over_LOO |> na.omit(), c(0.025, 0.975))))


print(paste0('Reliative distance gain: Order-LOO - ALL-LOO: ', mean(LOO_summary$rela_distance_gain_order_LOO_over_LOO |> na.omit())))
print(paste0('Reliative distance gain range: Order-LOO - ALL-LOO: ', 
             min(LOO_summary$rela_distance_gain_order_LOO_over_LOO |> na.omit()), ' - ',
             max(LOO_summary$rela_distance_gain_order_LOO_over_LOO |> na.omit())))
print(paste0('Reliative distance gain quantile: Order-LOO - ALL-LOO: ', 
             quantile(LOO_summary$rela_distance_gain_order_LOO_over_LOO |> na.omit(), c(0.025, 0.975))))



### Model decay
data <- read.csv('../../data/04.Sumamrize_prediction_metrics/02.06.regression_LL_elapsed_km.csv')
data <- data |> group_by(sp) |> slice(1)
data <- data[data$x_intercept<=10000,]
mean(data$x_intercept |> na.omit())
quantile(data$x_intercept |> na.omit(), c(0.025, 0.975))
print(paste0('Mean one week transition LL:', mean((data |> na.omit())$A + (data |> na.omit())$C)))

data <- read.csv('../../data/04.Sumamrize_prediction_metrics/02.04.regression_relative_distance_gain_elapsed_km.csv')
data <- data |> group_by(sp) |> slice(1)
data <- data[data$x_intercept<=10000,]
mean(data$x_intercept |> na.omit())
quantile(data$x_intercept |> na.omit(), c(0.025, 0.975))
print(paste0('Mean one week transition relative distance gain:', mean((data |> na.omit())$A + (data |> na.omit())$C)))

### Data Composition
res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
res <- res[res$method=='ST098_and_LL',]
res <- res[res$training_n_intervals>20,]
res <- res |> group_by(sp) |> slice_head(n=1)

data_type_missing_for_each_sp <- sign(res$train_n_banding == 0) + sign(res$train_n_motus == 0) + sign(res$train_n_tracking == 0)


mean(data_type_missing_for_each_sp == 2)

mean(data_type_missing_for_each_sp == 1)

mean(data_type_missing_for_each_sp == 0)

## Missing banding?
1 - (res[(res$train_n_motus > 0) | (res$train_n_tracking > 0),]$sp|>unique()|>length() / nrow(res))
1 - (res[(res$train_n_motus > 0) | (res$train_n_tracking > 0),]$FAMILY1_eBird|>unique()|>length() / res$FAMILY1_eBird |> unique() |> length())
1 - (res[(res$train_n_motus > 0) | (res$train_n_tracking > 0),]$ORDER1_eBird|>unique()|>length() / res$ORDER1_eBird |> unique() |> length())

## Missing motus?
1 - (res[(res$train_n_banding > 0) | (res$train_n_tracking > 0),]$sp|>unique()|>length() / nrow(res))
1 - (res[(res$train_n_banding > 0) | (res$train_n_tracking > 0),]$FAMILY1_eBird|>unique()|>length() / res$FAMILY1_eBird |> unique() |> length())
1 - (res[(res$train_n_banding > 0) | (res$train_n_tracking > 0),]$ORDER1_eBird|>unique()|>length() / res$ORDER1_eBird |> unique() |> length())


## Missing tracking?
1 - (res[(res$train_n_banding > 0) | (res$train_n_motus > 0),]$sp|>unique()|>length() / nrow(res))
1 - (res[(res$train_n_banding > 0) | (res$train_n_motus > 0),]$FAMILY1_eBird|>unique()|>length() / res$FAMILY1_eBird |> unique() |> length())
1 - (res[(res$train_n_banding > 0) | (res$train_n_motus > 0),]$ORDER1_eBird|>unique()|>length() / res$ORDER1_eBird |> unique() |> length())

##
res$n_datasource_missing <- data_type_missing_for_each_sp
(res[res$n_datasource_missing==2,]$test_n_banding > 0) |> mean()
(res[res$n_datasource_missing==2,]$test_n_banding > 0) |> mean()
(res[res$n_datasource_missing==2,]$test_n_banding > 0) |> mean()







