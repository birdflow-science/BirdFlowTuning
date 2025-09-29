# BirdFlowTuning

Code for:

Population-level migration modeling of North America’s birds through data integration with BirdFlow

Intermediate data and tuned models are available at: https://doi.org/10.5281/zenodo.16985511

--------

## LICENSE

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg

--------

## Structure

- `scripts/01.Run_validation_more_species`
   
Train and validate BirdFlow models using [BirdFlowPipelne](https://github.com/birdflow-science/BirdFlowPipeline) package. 
`BirdFlowPipeline` package is the workhorse to fit and validate those models.
The rest of the steps in this repository is mainly for summarizing the results and making figures.

- `scripts/02.Summarize_validation_preliminary`

After fitting the 225 grid search and validate each model, summarize the validation results and pull them into a single dataframe.

- `scripts/03.Summarize_species_specific_tuning_prediction_metrics`

Summarize the predictive metrics. Including log likelihood improvement and distance gain, and the performance decay along space and time.

- `scripts/04.Summarize_species_specific_tuning_bio_metrics`

Summarize the biological features of the tuned models. Including the validation of migration speed, straightness, and number of stopovers.

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



