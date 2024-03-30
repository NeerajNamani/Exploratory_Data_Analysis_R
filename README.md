# Exploratory Data Analysis (EDA) in R

The EDA process for the Plant Ecosystem Analysis project involved a comprehensive examination of the "PlantData.txt" dataset using R. This dataset contains 137 observations of 9 variables related to various geographical and environmental gradients impacting plant ecosystems. The primary objective was to explore the relationship between Native plant species Richness (NR) and ecological/environmental factors such as Area, Latitude, Elevation, Distance to significant landmarks (Dist), Soil types, Age of ecosystems (Years), Time since last deglaciation (Deglac), and adjacent Human population densities (Human.pop).

## Initial Data Loading and Inspection

- **Data Import**: The dataset was loaded into R using the `read.csv` function, specifying the path to the "PlantData.txt" file.
- **Initial Peeking**: Utilized the `head()` function to examine the first few rows of the dataset, ensuring proper loading and to get an initial sense of the data structure.

## Data Structure and Summary

- **Structure Inspection**: The `str()` function revealed the dataset as a data frame with 137 observations and 9 variables, with all variables being numerical.
- **Summary Statistics**: Employed `summary()` to obtain a descriptive summary of each variable, including measures of central tendency and dispersion.

## Missing Values and Unique Counts

- Checked for missing values using `colSums(is.na(Data))` and confirmed the dataset had no missing values.
- Calculated unique value counts for each column to determine the diversity of data and assess if any categorical variables were present.

## Pairwise Relationships and Correlations

- Used the `GGally` package to generate pairwise relationship plots with `ggpairs()`, aiding in visualizing correlations and potential relationships between variables.
- Constructed a correlation matrix to quantify the strength of relationships between variables, identifying particularly strong correlations worth further investigation.

## Distribution Analysis

- Visualized the distribution of each numerical variable using histograms, noting the distribution shape, skewness, and potential outliers.
- Generated boxplots for each variable to further assess data spread and identify outliers.

## Correlation Plot

- Created a correlation plot using the `corrplot` package, visually representing the correlation matrix and highlighting significant correlations.

## Transformations and Diagnostics

- Applied transformations to certain variables (e.g., log transformation to Human.pop) to address skewness and improve model fitting.
- Conducted diagnostics using plots of residuals, Q-Q plots, and other checks to evaluate model assumptions and fit.

## Model Fitting and Selection

- Explored multiple linear regression models, starting with a full model including all predictors and progressively refining the model based on statistical significance and model diagnostics.
- Utilized backward selection, AIC, BIC, and adjusted R-squared values to identify the most parsimonious and informative model.

## Final Model and Interpretation

- The selected final model incorporated log-transformed predictors and demonstrated the best fit based on adjusted R-squared and residual diagnostics.
- Key predictors identified as significantly impacting Native plant species Richness included Area, Elevation, and Soil types.

This detailed EDA process laid a solid foundation for subsequent analysis and modeling, providing valuable insights into the complex relationships within the plant ecosystem data.
