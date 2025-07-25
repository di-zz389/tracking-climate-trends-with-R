# Tracking Climate Trends with R
This repo explores R-based statistical analysis of climate data to identify long-term environmental trends.

### Datasets

- **global-temp-annual.csv** – NASA GISS Surface Temperature Analysis (GISTEMP)  
- **owid-co2-data.csv** – CO₂ emissions data from Our World in Data (OWID)

### Methodology

The analysis workflow includes:

- Data preprocessing  
- Univariate statistical analysis  
- Correlation analysis  
- Cross-dataset analysis, including:
  - Correlation matrix
  - Time series analysis
  - Scatter plot visualization
  - Linear regression modeling
  - Lag analysis
  - Rate-of-change analysis

All results and visualizations are documented in the `r-notebook.md` file.

### Key Findings

The analysis reveals a consistent rise in global temperatures, with distinct periods of accelerated warming that closely correspond to increased CO₂ emissions. Furthermore, the results indicate climate inertia: temperature changes are more strongly correlated with CO₂ emissions from preceding years than with emissions from the current year.