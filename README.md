# BirdFlowTuning

Code for:

Integrating multi-source data to infer population-level migration movement of North America's migratory birds

Data and tuned models are available at: https://doi.org/10.5281/zenodo.16985511

--------

## Structure

- `scripts/01.Run_validation_more_species`
   
Train and validate BirdFlow models using BirdFlowPipelne package.

- `scripts/02.Summarize_validation_preliminary`

After fitting the 225 grid search and validate each model, summarize the validation results and pull them into a single dataframe.

- `scripts/03.Summarize_species_specific_tuning_prediction_metrics`

Summarize the predictive metrids. Including log likelihood improvement and distance gain, and the performance decay along space and time.

- `scripts/04.Summarize_species_specific_tuning_bio_metrics`

Sumamrize the biological features of the tuned models. Including the validation of migraiton speed, straightness, and number of stopovers.

- `scripts/05.Compare_species_specific_and_LOO`

Investigate the performance of parameter transferring using leave-one-out (LOO) method, including All-LOO, Family-LOO, and order LOO.

- `scripts/06.Summarize_data_composition`

Summarize the data composition for tracking, banding, and Motus for each species.

- `scripts/07.Regression_for_hyperparameters`

Regress the selected hyperparameters using morphological and distributional traits. 

- `scripts/08.Demonstration_map`

Simulate migration routes using BirdFlow and the case study of phenology. To give a taste of what BirdFlow can do.

- `scripts/10.Calcualte_important_result_numbers`

Calculate some numbers for the Result part of the manuscript.



