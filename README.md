# How much does skiing in Europe cost?

The goal of this project is to answer the above question based on several features such as number of runs, location, height of the mountain, etc. 

## Code Used

**R version:** R 4.0.2 

**Python version:** Python 3.8.5

Please note, Python was only used for web scraping

**Python packages:** Beautiful Soup

**R packages:** tidyverse, tidymodels, ggcorrplot, ggrepel

## Data Cleaning

- parsed numeric data out of numeric variables
- imputted missing values using knn
- made new column elevation_length by subtracting lowest point from highest_point

## EDA

<p float="left">
  <img src="https://user-images.githubusercontent.com/65564135/102079439-038cfb00-3e0d-11eb-98a5-a2093d76de23.png" width = 400/>
  <img src="https://user-images.githubusercontent.com/65564135/102079434-012aa100-3e0d-11eb-9b93-f6a7fe362321.png" width = 400/>
</p>