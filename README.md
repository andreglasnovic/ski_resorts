# How much does skiing in Europe cost?

The goal of this project is to answer the above question based on several features such as number of runs, location, height of the mountain, etc. I will be predicting the cost of ski resorts to help skiers and snowboarders get the idea on what to expect from European resorts.

## Code Used

**R version:** R 4.0.2 

**Python version:** Python 3.8.5

Please note, Python was only used for web scraping

**R packages:** tidyverse, tidymodels, ggcorrplot, ggrepel

**Python packages:** Beautiful Soup

## Data Cleaning

- trimmed columns, inserted NAs to empty cells, replaced "None" with NA
- parsed numeric data out of numeric variables
- imputted missing values using knn
- made new column elevation_length by subtracting lowest point from highest_point

## EDA

Below are highlights from my EDA. Since I am trying to predict price, I mainly focused on showing the correlation between price and other variables

<p float="left">
  <img src="https://user-images.githubusercontent.com/65564135/102079444-04be2800-3e0d-11eb-9ed3-5419372da513.png" width = 500/>
  <img src="https://user-images.githubusercontent.com/65564135/102079450-07b91880-3e0d-11eb-869c-2f4215ab3b33.png" width = 500/>
  <img src="https://user-images.githubusercontent.com/65564135/102079446-05ef5500-3e0d-11eb-9aed-550da2827a17.png" width = 600/>
</p>

## Model Building

- Dropped variables that would not be included in the model
- Split data into train and test set with a test size od 20%
- Centered and scaled the data
- Transformed categorical variables into dummy variables

I tried three different models and evaluated them using MAE, RMSE, RSQ.

Models:

- Linear Regression
- Lasso Regression
- XGBoost

## Model Performance:

<img src="https://user-images.githubusercontent.com/65564135/102087083-51a7fb80-3e19-11eb-8923-d76935630153.jpg" width = 300/> 

<img src="https://user-images.githubusercontent.com/65564135/102079434-012aa100-3e0d-11eb-9b93-f6a7fe362321.png" width = 700/>

