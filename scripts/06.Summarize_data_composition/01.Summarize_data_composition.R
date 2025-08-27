library(ggplot2)
library(BirdFlowR)
library(BirdFlowPipeline)
library(devtools)

setwd('/home/yc85_illinois_edu/BirdFlow_Validation_Project/scripts/06.Summarize_data_composition/')
source('../plotting_params/plotting_params.R')
# source('../02.Summarize_validation_preliminary/load_data_functions.R')

all_res <- read.csv('../../data/03.All_validation_summary/validation_final_summary.csv')
all_res <- all_res |> dplyr::group_by(.data[['sp']], .data[['method']]) |> dplyr::slice(1) |> dplyr::ungroup()

## 01. Load data
tmp <- all_res[all_res['method']=='ST098_and_LL',] ## ST098_and_LL as model selection method
tmp$total_training_n_intervals <- tmp$train_n_banding + tmp$train_n_tracking + tmp$train_n_motus
tmp <- tmp |> dplyr::arrange(-.data[['total_training_n_intervals']])


## 02. Transform data
df2 <- tmp |>
  # compute total & log‐thickness
  dplyr::mutate(
    total  = train_n_banding + train_n_motus + train_n_tracking,
    logtot = total**(1/2), #log(total, base=1.001)
  ) |> 
  dplyr::arrange(-.data[['total']]) |>
  tidyr::pivot_longer(
    cols      = dplyr::starts_with("train_n_"),
    names_to  = "data_type",
    values_to = "n"
  ) |>
  dplyr::mutate(
    data_type = dplyr::recode(data_type,
                       train_n_banding  = "Banding",
                       train_n_motus    = "Motus",
                       train_n_tracking = "Tracking")
  ) |>
  dplyr::arrange(.data[['total']], common_name, data_type) |>
  dplyr::mutate(
    sp_index = match(common_name, unique(common_name))
  )

y_df <- df2 |> dplyr::group_by(common_name) |> dplyr::slice(1) |> 
  dplyr::arrange(.data[['total']], common_name) |> 
  dplyr::ungroup() |>
  dplyr::mutate(
    # bar center & edges: thickness = logtot
    y_ctr = cumsum(logtot) + 0.05,
    ymin  = y_ctr - logtot/2,
    ymax  = y_ctr + logtot/2
  )

x_df <- df2 |> dplyr::group_by(common_name) |>
  dplyr::mutate(
    # stacking positions (100% total)
    prop = n / sum(n),
    xmin = lag(cumsum(prop), default = 0),
    xmax = cumsum(prop)
  )


df2 <- df2 |> merge(y_df[,c('common_name', 'ymin', 'ymax')],
            by.x = 'common_name', by.y='common_name', all.x=T) |> 
  merge(x_df[,c('common_name', 'data_type', 'prop', 'xmin', 'xmax')],
        by.x = c('common_name', 'data_type'), by.y=c('common_name', 'data_type'), all.x=T) |>
  dplyr::arrange(sp_index, data_type)


## 03. Plot
y_df$new_label <- ifelse(y_df$sp_index > (max(y_df$sp_index) - 50), y_df$common_name, '.')

p <- ggplot(df2) +
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax,
                fill = data_type),
            linewidth=0.1,
            color = "lightgray") +
  scale_y_continuous(
    breaks = y_df$y_ctr, #tail(y_df$y_ctr, 20), #unique(df2$sp_index),
    labels = y_df$new_label #tail(y_df$new_label, 20)
  ) +
  scale_fill_manual(
    name   = "Data type",
    values = c(
      Banding  = "steelblue2",
      Motus    = "chartreuse4",
      Tracking = "orange2"
    )
  ) +
  coord_cartesian(expand = FALSE) +
  my_plotting_params[['theme']] +
  my_plotting_params[['my_plotting_params']] +
  labs(
    x     = "Proportion of transitions",
    y     = NULL,
    fill  = "Data type"
  ) +
  my_plotting_params[['theme']] +
  theme(
    axis.text.y = element_text(size = 16)
  )

cairo_pdf('../../data/07.Summarize_data_composition/data_composition.pdf',
          width = my_plotting_params[['single_plot_width']]*0.7, height = my_plotting_params[['single_plot_height']]*1.6, family = my_plotting_params[['font']])
print(p)
dev.off()

### 04. save data
prop_summary <- list(
  mean_banding_prop = mean(tmp$train_n_banding / tmp$total_training_n_intervals),
  std_banding_prop = sd(tmp$train_n_banding / tmp$total_training_n_intervals),
  quantile_025_banding_prop = as.numeric(quantile(tmp$train_n_banding / tmp$total_training_n_intervals, probs=0.25)),
  quantile_075_banding_prop = as.numeric(quantile(tmp$train_n_banding / tmp$total_training_n_intervals, probs=0.75)),
  mean_motus_prop = mean(tmp$train_n_motus / tmp$total_training_n_intervals),
  std_motus_prop = sd(tmp$train_n_motus / tmp$total_training_n_intervals),
  quantile_025_motus_prop = as.numeric(quantile(tmp$train_n_motus / tmp$total_training_n_intervals, probs=0.25)),
  quantile_075_motus_prop = as.numeric(quantile(tmp$train_n_motus / tmp$total_training_n_intervals, probs=0.75)),
  mean_tracking_prop = mean(tmp$train_n_tracking / tmp$total_training_n_intervals),
  std_tracking_prop = sd(tmp$train_n_tracking / tmp$total_training_n_intervals),
  quantile_025_tracking_prop = as.numeric(quantile(tmp$train_n_tracking / tmp$total_training_n_intervals, probs=0.25)),
  quantile_075_tracking_prop = as.numeric(quantile(tmp$train_n_tracking / tmp$total_training_n_intervals, probs=0.75))
)

prop_summary <- as.data.frame(prop_summary)
write.csv(prop_summary, '../../data/07.Summarize_data_composition/data_composition.csv')





