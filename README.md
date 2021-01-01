# Evaluation of EpiNow2 using approach of Huisman, Scire et al.

This repository adapts the code and method from ["Estimation and worldwide monitoring of the effective reproductive number of SARS-CoV-2"](https://doi.org/10.1101/2020.11.26.20239368) to compare the performance of `EpiNow2` to the Huisman, Scire et al. approach using their simulated scenarios and evaluation metrics.

## Reproducibility

1. Clone the repository.

```
git clone --recursive https://github.com/seabbs/huisman.epinow2.evaluation.git
```

2. Set the repository as your working directory.

```
cd huisman.epinow2.evaluation
```
2. Install the dependencies.

```
Rscript -e 'install.packages("remotes"); remotes::install_dev_deps()`
```

3. Update the summary evaluation from Huisman, Scire et al.

```
Rscript code/interactiveSimulations.R
```

## Code outline from Huisman, Scire et al.

- `generateSimulations.R` contains functions to generate simulations, and estimate Re from simulated observations
- `compareSimulations.R` contains functions to calculate performance metrics on simulated data
- `interactiveSimulations.R` is used to generate Fig. 1A
- `paperSimulationGrid.R` is used to generate Fig. 1B and supplementary Figs.  (since the simulations take some time to compute, the summarised files needed for plotting can be found in the simulations folder)
- `empiricalPaper.R` is used to generate Table 1, S1; and Fig S10, S11
- `interventionImpact.R` is used to generate Fig 3b, 4, S12, and tables S3, S4

## Additional code

- `epinow2-interactive.R` contains the `EpiNow2` code to estimate the simulations found in `interactiveSimulation.R`.
- `epinow2-grid.R` contains the `EpiNow2` code to estimate the simulation grid found in `paperSimulationGrid.R`.
- 