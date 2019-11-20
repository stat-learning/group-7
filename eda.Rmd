---
title: "Exploratory Data Analysis"
author: 
- "Alice Chang"
- "Ben Thomas"
- "Olek Wojcik"
date: "11/19/2019"
output: pdf_document
---

### Data description

Our group is trying to explore why students of different colleges/universities have different employment outcomes. Specifically, we're interested in both (1) students' employment rate after graduation and (2) students' salaries after graduation. To approach this problem, we're using data from the U.S. Department of Education: the "College Scorecard." 
  
The College Scorecard (found here: https://collegescorecard.ed.gov/data/) has an absolutely huge amount of data, across many years. We've started our analysis with the 2014-15 dataset, which is more complete than more recent datasets. Before cleaning the data, we have 1,977 predictor variables and 7,703 observations, which represent individual colleges and universities. After cleaning the data (below), we have 1,593 predictor variables and 5,492 observations. This large decrease is primarily due to (1) removing the variables which only contained `NA` values and (2) removing rows for which our dependent variable (`COUNT_NWNE_P10`) was `NA`. 

### Data exploration

Before diving into the exploration, we first need to actually get the data. 

```{r Read the data}
college.data <- read.csv("MERGED2014_15_PP.csv", header = TRUE)
```

Now, let's take a look at this data. How much is missing? To do this, I'll use the `naniar` package. Note that I've changed the "NULL" and "PrivacySuppressed" values to "NA" so that they show up in this visualization. 

```{r Visualize missingness}
library(naniar)
library(tidyverse)

college.data <- college.data %>%
  replace(.=="NULL", NA) %>%
  replace(.== "PrivacySuppressed", NA)
  
vis_miss(college.data, show_perc_col = FALSE, warn_large_data = FALSE)
```

61% of the data in this dataset is missing; not ideal. Let's now clean this data and get it in workable form. 

```{r Clean the data}
not_all_na <- function(x) any(!is.na(x))

# Remove intro variables and other earnings variables
college.data.section <- college.data %>%
  select(MAIN:COUNT_NWNE_P10, + DEBT_MDN_SUPP:OMENRUP_PARTTIME_POOLED_SUPP)
  
# Drop NAs in response
college.data.section <- college.data.section %>% 
  drop_na(COUNT_NWNE_P10)

# Remove columns with only NA values
college.data.section <- college.data.section %>% 
  select_if(not_all_na)

# Make all columns numeric
college.data.section[, c(1:ncol(college.data.section))] <- sapply(college.data.section[, c(1:ncol(college.data.section))], as.numeric)

```

To get an initial idea as to which variables we should be considering here, I've created a boosted regression tree and displayed the variable importances. 

```{r Boosted tree}
library(gbm)
employment.tree <- gbm(COUNT_NWNE_P10 ~ ., 
                       data = college.data.section, 
                       n.trees = 150, 
                       interaction.depth = 1,
                       shrinkage = 0.1)
summary(employment.tree)
```
