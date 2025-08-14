# DOC and Biomass Data Processing and Visualization

## Overview
This repository contains the R code and data used to calculate **Dissolved Organic Carbon (DOC) consumption** (inlet - outlet) and visualize **biomass cell counts** for our paper. The workflow includes data cleaning, consumption calculations, and plot generation.

---

## Project Structure
├── data/
│   ├── DOC_consumption.csv    # Calculated DOC consumption (inlet - outlet)
│   └── DOC_per_column.csv     # Raw DOC measurements (inlet/outlet)
├── output/                    # Generated plots and processed data
├── R/                         # R scripts for processing and plotting
├── LICENSE
└── README.md

## Data

### Inputs
- **`data/DOC_final_pretreated_all.csv`**: Pretreated raw DOC measurements (µg C/L) for inlet/outlet samples across columns and time points.
- **`data/Cell_Counts_standardized.csv`**: Standardized biomass cell counts (10⁶ cells/L) for each column and time point.

### Outputs
- Processed data and plots are saved in the `output/` directory.

---

## Workflow

### 1. Data Processing and DOC Consumption Calculation
**Script**: [`R/Column_DOC_Calculations.R`](R/Column_DOC_Calculations.R)
- Loads `data/DOC_final_pretreated_all.csv` and `data/Cell_Counts_standardized.csv`.
- Calculates DOC consumption: `consumption = inlet - outlet`.
- Merges DOC consumption with biomass data.
- Saves processed data to `output/`.
- Generates faceted boxplots for:
  - DOC consumption vs. time.
  - Biomass vs. time.
- Adds annotations (a–j) to each facet for reference in the paper.
- Saves plots to `output/`.

---

### 2. Plotting
**Script**: [`R/plotting_functions.R`](R/plotting_functions.R)
- Holds the necessary functions to convert the labels and apply the global coloring scheme

---
