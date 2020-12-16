As a skiing enthusiast, I though it'd be be interesting to know what factors might impact the cost of ski resorts. Those insights might help me in choosing my next destination. 

The goal of this project is to determine the cost of ski resorts to help me and other skiers and snowboarders get the idea on what to expect from European resorts. The first step of the project was to collect the data, so I went to https://www.skiresort.info and scraped 2450 resorts using python and beautiful soup. The project also includes data cleaning, statistical analysis, data visualization and machine learning modeling. All the files, including scraping script, are uploaded to my github. 

P.S. cover photo taken by me in february 2020 :)
## Table of Content
- [Data Cleaning](#data-cleaning)
- [Missing Values](#missing-values)
  - [Imputing Missing Values](#imputing-missing-values)
- [Feature Engeneering](#feature-engeneering)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Data Preprocessing](#data-preprocessing)
- [Building Models](#building-models)
- [Model Results](#model-results)
- [Model Fitting](#model-fitting)

## Data Cleaning

``` r
# loading libraries
library(tidyverse)
library(tidymodels)
# loading data
resorts <- read.csv('ski_resorts_eu.csv') %>% 
  select(-X)
```

Let’s first see what columns and data types we’re working with

``` r
str(resorts)
```

    ## 'data.frame':    2450 obs. of  11 variables:
    ##  $ resort_name  : chr  " Ischgl/​Samnaun – Silvretta Arena " " Carezza Ski " " Silvretta Montafon " " Obertauern " ...
    ##  $ country      : chr  "Austria" "Italy" "Austria" "Austria" ...
    ##  $ province     : chr  "Tyrol (Tirol)" "Trentino-Alto Adige (Trentino-Südtirol)" "Vorarlberg" "Salzburg (Salzburger Land)" ...
    ##  $ rating       : num  4.8 4 4.5 4.5 4.3 4.2 3.8 4.2 4 4.7 ...
    ##  $ lowest_point : chr  "1360 m" "1182 m" "700 m" "1630 m" ...
    ##  $ highest_point: chr  "2872 m" "2337 m" "2430 m" "2313 m" ...
    ##  $ blue_runs    : chr  "47 km" "19 km" "60 km" "61 km" ...
    ##  $ red_runs     : chr  "143 km" "16.2 km" "50 km" "35 km" ...
    ##  $ black_runs   : chr  "49 km" "4.8 km" "8 km" "4 km" ...
    ##  $ ski_lifts    : chr  "41 ski lifts" "12 ski lifts" "34 ski lifts" "26 ski lifts" ...
    ##  $ price        : chr  "€ 53.50" "€ 49.50" "€ 57,-" "€ 49,-" ...

When I was scraping the data I remember I inserted “None” to the places
where data wasn’t available. Let’s deal with that first before I forget.
They should all be replaced by NA

How many "None" cells do we have?

``` r
resorts %>% 
  summarise_all(list(~sum(str_detect(.,"^None"))))
```

    ##   resort_name country province rating lowest_point highest_point blue_runs
    ## 1           0       0       50      0            3             3        22
    ##   red_runs black_runs ski_lifts price
    ## 1       22         22         3     0

Ok. There’s quite a few. I’ll replace “None” with NA to be able
to impute or remove them later

``` r
resorts <- resorts %>% 
  mutate(across(everything(), ~na_if(.,"None")))
```

I’d use `mutate_at()`, but dplyr is telling me it’s been superseded by
`across()` function, so let’s use `across()` instead

Another thing I made note of while collecting data was some extra spaces
and empty cells. Let’s check

``` r
# checking for extra space
resorts %>% 
  summarise_all(list(~sum(str_detect(.,"^\\s"))))
```

    ##   resort_name country province rating lowest_point highest_point blue_runs
    ## 1        2393       0       NA      0           NA            NA        NA
    ##   red_runs black_runs ski_lifts price
    ## 1       NA         NA        NA     0

There’s a great stringr function `str_trim()` to clean the extra space

``` r
resorts <- resorts %>% 
  mutate(across(where(is.character), str_trim)) 
```

``` r
# checking for empty cells
resorts %>% 
  summarise_all(list(~sum(. == "")))
```

    ##   resort_name country province rating lowest_point highest_point blue_runs
    ## 1          57       0       NA      0           NA            NA        NA
    ##   red_runs black_runs ski_lifts price
    ## 1       NA         NA        NA   471

Only price and resort\_name columns have empty cells. Let’s fix that

``` r
resorts <- resorts %>% 
  mutate(across(c(price,resort_name),~na_if(.,"")))
```

Now, price variable looks very messy. It contains different currencies
and other characters. I’ll remove all but price in euros

``` r
resorts <- resorts %>% 
  mutate(price = gsub("(.*€ )|(,.*)","", price))
```

Units of measurement are still left to remove for some numeric
variables. I’ll replace everything that’s not a digit with blank space

``` r
# removing units of measurement from columns 5 to 10 
resorts <- resorts %>% 
  mutate(across(5:10, ~gsub("\\D","",.)))
```

What are we working with now?

``` r
str(resorts)
```

    ## 'data.frame':    2450 obs. of  11 variables:
    ##  $ resort_name  : chr  "Ischgl/​Samnaun – Silvretta Arena" "Carezza Ski" "Silvretta Montafon" "Obertauern" ...
    ##  $ country      : chr  "Austria" "Italy" "Austria" "Austria" ...
    ##  $ province     : chr  "Tyrol (Tirol)" "Trentino-Alto Adige (Trentino-Südtirol)" "Vorarlberg" "Salzburg (Salzburger Land)" ...
    ##  $ rating       : num  4.8 4 4.5 4.5 4.3 4.2 3.8 4.2 4 4.7 ...
    ##  $ lowest_point : chr  "1360" "1182" "700" "1630" ...
    ##  $ highest_point: chr  "2872" "2337" "2430" "2313" ...
    ##  $ blue_runs    : chr  "47" "19" "60" "61" ...
    ##  $ red_runs     : chr  "143" "162" "50" "35" ...
    ##  $ black_runs   : chr  "49" "48" "8" "4" ...
    ##  $ ski_lifts    : chr  "41" "12" "34" "26" ...
    ##  $ price        : chr  "53.50" "49.50" "57" "49" ...

Nice\! Variables look much cleaner now, one last thing in this section is
to turn numbers into numerics

``` r
resorts <- resorts %>% 
  mutate(across(4:11,as.numeric))
```

## Missing Values

I like to start this section with the percentage of missing values…

``` r
resorts %>% 
  summarise_all(list(~ sum(is.na(.)) / n() * 100))
```

    ##   resort_name country province rating lowest_point highest_point blue_runs
    ## 1    2.326531       0 2.040816      0     0.122449      0.122449 0.8979592
    ##    red_runs black_runs ski_lifts    price
    ## 1 0.8979592  0.8979592  0.122449 19.22449

The variable that first catches my attention is price. There’s almost
20% of values missing. It’ll be among first things to take care of.

Now, province variable will be disregarded. I’ll explain why in data
preprocessing section, so please keep reading trough\!

### Imputing Missing Values

I’ll first get indexes of rows where price is missing to be able to
compare price distribution before and after the imputation

``` r
price_na <- resorts %>% 
  mutate(row = seq(1:length(price))) %>% 
  filter(is.na(price)) %>% 
  pull(row)
```

I"ll use the k nearest neighbor model to impute the missing
values

``` r
resorts <- recipe(price ~ ., data = resorts) %>% 
  step_knnimpute(lowest_point, highest_point, blue_runs, red_runs, black_runs, ski_lifts, price) %>% 
  prep() %>% 
  juice()
```

Since I used the recipes package, all my characters were turned into
factors. Freed two birds with one key\!

Let’s now compare price distributions before and after. I’ll only do
this for price since it had the significant amount of missing values

``` r
ggplot(NULL, aes(price)) +
  geom_density(data = resorts[-price_na,], fill = "green", alpha = 0.5)+
  geom_density(data = resorts, fill = "red", alpha = 0.5)+
  scale_x_log10()+
  labs(title = "Price Distribution", subtitle = "green = before\nred = after")
```

<img src=figure-gfm/unnamed-chunk-17-1.png>

Good\! The distribution is fairly similar although I imputed 20% of the
data. We can keep on going

## Feature Engeneering

There’s lowest and highest point of the ski resort in the dataframe. It
might be meaningful to calculate the difference between highest and
lowest point

``` r
resorts <- resorts %>% 
  mutate(elevation_length = highest_point - lowest_point)
```
That's all for feature engineering. I think we have enough features 

## Exploratory Data Analysis

The first step of exploring would be to see what numeric variables are
correlated with price the most

``` r
library(ggcorrplot)

resorts %>% 
  na.omit() %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  ggcorrplot(hc.order = TRUE, outline.color = "white", lab = T)+
  labs(title = "Correlation Map")
```

<img src=figure-gfm/unnamed-chunk-19-1.png>
Now that we have the correlation info, it should help us understand what
features might make the biggest impact on our model.

Let’s make some graphs now!

I’ll plot price as a function of several variables that show the highest
correlation and then group by country

``` r
resorts %>%
  mutate(total_runs = red_runs + blue_runs + black_runs) %>% 
  select(elevation_length, ski_lifts, highest_point, country, price, total_runs) %>%
  mutate(country = fct_lump(country,4)) %>%
  filter(country != "Other") %>% 
  gather(key = "key", value = "value", -price,-country) %>% 
  ggplot(aes(value,price, color = country))+
  geom_point(alpha = 0.5)+
  geom_smooth(method = "lm",colour ="blue")+
  facet_wrap(~key, scales = "free")+
  scale_y_log10()+
  scale_x_log10()+
  labs(fill = "Country", title = "Price vs impactful features", subtitle = "Clustered by country")
```

<img src=figure-gfm/unnamed-chunk-20-1.png>

`fct_lump()` is a great function to lump less frequent factor levels
into “other”. Namely, it would be messy to have all the countries on the
graph. I also summed all the runs into total runs to get the idea of how
price reacts to an increase of runs.

Tip of the day: Log scale is a great way to widen the space between data points.

Now I’ll show how price and rating are related. Please note, I won’t use
rating in my model simply because it’s inconvenient for the end users.
More on this in data preprocessing section

``` r
library(ggrepel)

resorts %>% 
  group_by(country) %>% 
  summarise(n = n(),
            rating = mean(rating),
            price = mean(price,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(n > 15) %>% 
  ggplot(aes(price,rating)) +
  geom_point(aes(colour = country),alpha = 0.7, size = 5)+
  geom_text_repel(aes(label = country), size = 3) +
  labs(size = "# of resorts", title = "Rating vs Price", subtitle = "Grouped by country", colour = "")+
  theme(legend.position = "none")
```

<img src=figure-gfm/unnamed-chunk-21-1.png>

## Data Preprocessing

I’ll start preprocessing with removing certain variables

- province:
  - Having to select the province might be too specific for the
    end users; country should be enough for them to get the general idea
    of the price
    
- lowest\_point:
  - I only want to leave highest point and elevation
length. I believe end users are less interested in lowest point
- rating: 
  - Rating is just inconvenient for the end users to obtain, and
it’s also subjective
- resort\_name:
  - If users are specifically interested in one resort they
could easily look up its price

<!-- end list -->

``` r
resorts <- resorts %>% 
  select(-resort_name, -rating, -province, -lowest_point)

resorts %>% 
  head()
```

    ## # A tibble: 6 x 8
    ##   country highest_point blue_runs red_runs black_runs ski_lifts price
    ##   <fct>           <dbl>     <dbl>    <dbl>      <dbl>     <dbl> <dbl>
    ## 1 Austria          2872        47      143         49        41  53.5
    ## 2 Italy            2337        19      162         48        12  49.5
    ## 3 Austria          2430        60       50          8        34  57  
    ## 4 Austria          2313        61       35          4        26  49  
    ## 5 Austria          2150        35       30         10        19  50.5
    ## 6 Austria          2025       259      537        135        43  51  
    ## # … with 1 more variable: elevation_length <dbl>

Now that my dataframe contains only columns that will go into the models
let’s split the data into train and test sets with proportions of 80%
for training and remaining 20% for testing

``` r
split <- initial_split(resorts, prop = .8, strata = price)

train <- training(split)
test <- testing(split)
```

In this next step I’ll use the recipes package to preprocess data for
modeling

``` r
train_rec <- recipe(price~., data = train) %>% 
  step_zv(all_numeric(),-all_outcomes()) %>% 
  step_normalize(all_numeric(),-all_outcomes()) %>% 
  step_dummy(all_nominal())

train_rec
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##    outcome          1
    ##  predictor          7
    ## 
    ## Operations:
    ## 
    ## Zero variance filter on all_numeric(), -all_outcomes()
    ## Centering and scaling for all_numeric(), -all_outcomes()
    ## Dummy variables from all_nominal()

`train_rec` output shows the defined steps in a recipe

## Building Models

Now that the data is preprocessed we can move on to constructing models.
I want to try several models and then evaluate them to see which
performs the best

``` r
set.seed(365)

# Linear Regression(Least Squares)
linear_model <- linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

# Lasso Regression
lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_mode("regression") %>% 
  set_engine("glmnet")

# XGBoost
xgboost_model <- boost_tree(trees = tune(), learn_rate = tune(), tree_depth = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("xgboost")

linear_model
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

``` r
lasso_model
```

    ## Linear Regression Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   penalty = tune()
    ##   mixture = 1
    ## 
    ## Computational engine: glmnet

``` r
xgboost_model
```

    ## Boosted Tree Model Specification (regression)
    ## 
    ## Main Arguments:
    ##   trees = tune()
    ##   tree_depth = tune()
    ##   learn_rate = tune()
    ## 
    ## Computational engine: xgboost

Models are now defined, but are not yet fitted to any data

### Define cross-validtion

I’ll use k-folds cross-validation for resampling

Resampling will only be done on XGBoost and Lasso. Basic linear model doesn't require resampling

``` r
folds <- vfold_cv(train, strata = price, v = 10)
```

### Define Grids

I’ll define grids now. Grids are used for testing different parameters

``` r
lasso_grid <- grid_regular(penalty(), levels = 10)

xgboost_grid <- grid_regular(parameters(xgboost_model), levels = 3, filter = c(trees > 1))
```

### Define Metrics

Below metrics will be used to evaluate models

``` r
model_metrics <- metric_set(mae, rmse, rsq)
model_metrics
```

    ##   metric          class direction
    ## 1    mae numeric_metric  minimize
    ## 2   rmse numeric_metric  minimize
    ## 3    rsq numeric_metric  maximize

### Define Workflow

`workflow()` will help hold recipe and model together.

Workflow allows you to process any dataset through the assigned
recipe. The recipe within a workflow will prepare a new dataset for
model fitting

``` r
wf <- workflow() %>% 
  add_recipe(train_rec)
wf
```

    ## ══ Workflow ════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════
    ## Preprocessor: Recipe
    ## Model: None
    ## 
    ## ── Preprocessor ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## 3 Recipe Steps
    ## 
    ## ● step_zv()
    ## ● step_normalize()
    ## ● step_dummy()

The same recipe will be used for all models, which is why I only added
recipe to the workflow for now and not the model

### Parameter Tuning

Now, all the above defined variables will be used for actual tuning.
I’ll make sure to save predictions with `control_grid()` function

``` r
# doParallel package allows me to use all my cores for faster computing. Tuning could take awhile :/
doParallel::registerDoParallel()

lasso_tune <- tune_grid(wf %>% add_model(lasso_model),
                        grid = lasso_grid,
                        resamples = folds,
                        metrics = model_metrics,
                        control = control_grid(save_pred = TRUE))

xgboost_tune <- tune_grid(wf %>%  add_model(xgboost_model),
                          grid = xgboost_grid,
                          resamples = folds,
                          metrics = model_metrics,
                          control = control_grid(save_pred = TRUE))
```
Models are resampled and fitted with different parameters. Let’s evaluate them

### Evaluating Parameters

#### Lasso

``` r
lasso_tune %>% 
  collect_metrics() %>% 
  pivot_wider(.config, names_from = .metric, values_from = mean) %>% 
  arrange(rmse) 
```

    ## # A tibble: 10 x 4
    ##    .config   mae  rmse   rsq
    ##    <chr>   <dbl> <dbl> <dbl>
    ##  1 Model01  5.04  8.45 0.664
    ##  2 Model02  5.04  8.45 0.664
    ##  3 Model03  5.04  8.45 0.664
    ##  4 Model04  5.04  8.45 0.664
    ##  5 Model05  5.04  8.45 0.664
    ##  6 Model06  5.04  8.45 0.664
    ##  7 Model07  5.04  8.45 0.664
    ##  8 Model08  5.04  8.45 0.664
    ##  9 Model09  5.04  8.45 0.663
    ## 10 Model10  5.98  9.39 0.593

#### XGBoost

``` r
xgboost_tune %>% 
  collect_metrics() %>% 
  pivot_wider(.config, names_from = .metric, values_from = mean) %>% 
  arrange(rmse)
```

    ## # A tibble: 18 x 4
    ##    .config   mae  rmse   rsq
    ##    <chr>   <dbl> <dbl> <dbl>
    ##  1 Model06  4.84  8.28 0.675
    ##  2 Model05  4.86  8.31 0.672
    ##  3 Model17  5.04  8.78 0.636
    ##  4 Model18  5.04  8.78 0.636
    ##  5 Model11  4.94  9.39 0.614
    ##  6 Model12  4.94  9.39 0.614
    ##  7 Model16 26.3  29.9  0.557
    ##  8 Model10 26.3  29.9  0.540
    ##  9 Model04 26.3  29.9  0.356
    ## 10 Model15 26.4  30.0  0.554
    ## 11 Model09 26.4  30.0  0.538
    ## 12 Model03 26.4  30.0  0.357
    ## 13 Model14 26.5  30.1  0.553
    ## 14 Model08 26.5  30.1  0.537
    ## 15 Model02 26.5  30.1  0.357
    ## 16 Model13 26.5  30.1  0.553
    ## 17 Model07 26.5  30.1  0.536
    ## 18 Model01 26.5  30.1  0.357

Parameter tuning can significantly increase model's success. Next step is to select the best tune

### Best Tune

`select_best()` function just selects the parameters that resulted in
the best performing model

``` r
best_tune_lasso <- lasso_tune %>% 
  select_best("mae")

best_tune_xgboost <- xgboost_tune %>% 
  select_best("mae")
```

### Finalize Workflow

`finalize_workflow()` assigns the best parameters to the model

``` r
lasso_final_wf <- finalize_workflow(wf %>% add_model(lasso_model), best_tune_lasso)

xgboost_final_wf <- finalize_workflow(wf %>% add_model(xgboost_model), best_tune_xgboost)
```

## Model Results

And now fitting models to the testing set. We can use previously defined metrics

``` r
lasso_fit <- last_fit(lasso_final_wf, split, metrics = model_metrics)

xgboost_fit <- last_fit(xgboost_final_wf, split, metrics = model_metrics)

linear_fit <- last_fit(wf %>% add_model(linear_model), split, metrics = model_metrics)
# we didn't have to finalize_workflow() for basic linear model because there was no tuning for it
```

Which model performs the best?

Let’s collect metrics

``` r
lasso_res <- lasso_fit %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "lasso") %>% 
  select(model, rmse, rsq, mae)

linear_res <- linear_fit %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "linear") %>% 
  select(model, rmse, rsq, mae)

xgboost_res <- xgboost_fit %>% 
  collect_metrics() %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "xgboost") %>% 
  select(model, rmse, rsq, mae)

bind_rows(lasso_res,linear_res,xgboost_res) %>% 
  arrange(mae)
```

    ## # A tibble: 3 x 4
    ##   model    rmse   rsq   mae
    ##   <chr>   <dbl> <dbl> <dbl>
    ## 1 xgboost  7.34 0.652  5.06
    ## 2 lasso    7.42 0.643  5.31
    ## 3 linear   7.43 0.644  5.32

And the Oscar goes to… ***XGBoost*** :)

Let’s collect predictions and visualize them…

``` r
lasso_pred <- lasso_fit %>% 
  collect_predictions() %>% 
  mutate(model = "lasso")

linear_pred <- linear_fit %>% 
  collect_predictions() %>% 
  mutate(model = "linear")

xgboost_pred <- xgboost_fit %>% 
  collect_predictions() %>% 
  mutate(model = "xgboost")

bind_rows(lasso_pred,xgboost_pred,linear_pred) %>% 
  ggplot()+
  geom_density(aes(price), fill = "black", alpha = 0.7)+
  geom_density(aes(.pred, fill = model), alpha = 0.6)+
  facet_wrap(~model)+
  scale_x_log10()+
  theme(legend.position = "")+
  labs(title = "Model Results", subtitle = "true price = black")
```

<img src=figure-gfm/unnamed-chunk-36-1.png>

Above figure clearly shows how XGBoost outperforms the rest of the models
## Model Fitting

Our models are production ready. One last final step is to fit the
entire dataset to the best performing model using the final workflow

``` r
final_xgboost_model <- fit(xgboost_final_wf, resorts)

# saving the model
saveRDS(final_xgboost_model, "resorts_model.rds")

```
Please check out an [app](https://andreglasnovic.com/project/ski_resorts_app/) that deploys the best performing model from this project.

Thanks for taking the time to read through my project. Any feedback and
comments are welcome and greatly appreciated.

