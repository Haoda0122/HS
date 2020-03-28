# Verifying Theoretical Equations

---
title: "Verifying Theoretical Equations"
author: "Haoda Song, Bretho Danzy, and Yuqi Zhang"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  html_document:
    code_folding: hide
    toc: yes
    toc_float:
      collapsed: no
      smooth_scroll: no
  word_document:
    toc: yes
---
<!-- note this is best viewed as an .html -->

```{r setup, include=FALSE}
# default R stuff
knitr::opts_chunk$set(echo = TRUE)

# load necessary libraries
library(tidyverse)        # how R works
library(ggplot2)          # how plotting in R works
library(readxl)           # how R reads .xlsx's
library(nlme)             # how R does mixed linear models
library(kableExtra)       # add-on to knitr
library(knitr)            # producing nice html tables in R
library(xtable)           # make nice summary tables

# load the data
# file
file_location <- "/Users/stephen/Desktop/STA475/Capstone\ Group\ Project/Copy\ of\ F19\ Sections\ ABC\ DEF\ Names\ Removed.xlsx"
# image
image_location <-"/Users/stephen/Desktop/STA475/Capstone\ Group\ Project/workpiece_graphic.png"
raw_data <- read_excel(file_location)

# clean the data 
clean_data <- raw_data %>% 
  # create a sorting variables
  mutate(row_id = 1:54, workpiece_id = rep(1:18, each = 3)) %>% 
  # group by rows to perform row-wise calculations
  group_by(row_id) %>% 
  # perform row-wise calculations
  mutate(
    # formula for theoretical turned diameter
    turned_ideal = `Faced Diameter (in)` - 2*`DOC (in)`,
    # average chuck diameter measurements
    diameter_chuck = mean(`Chuck end diameter 1`,
                          `Chuck end diameter 2`,
                          `Chuck end diameter 3`),
    # average tip diameter measurements
    diameter_tip = mean(`Tip-end diameter 1 (in)`,
                        `Tip-end diameter 2`,
                        `Tip-end diameter 3`),
    # average two previous diameter measurements
    diameter_mean = mean(diameter_tip,diameter_chuck),
    # formula for diametral error (empirical)
    diametral_error = diameter_mean - turned_ideal,
    # average overhang distance
    overhang = mean(`From [approx distance] (close to  the chuck)`,
                    `to [approx distance] (close to the tip)`)) %>% 
  # ungroup by row_id
  ungroup() %>% 
  # clean up variable names & calculate predictors
  mutate(feed_per_rev = `f (in/rev)`,
         roughness = `Roughness (micro-in)`*.000001,
         depth_of_cut = `DOC (in)`,
         workpiece_diameter = `Faced Diameter (in)`,
         roughness_predictor = feed_per_rev^2,
         diametral_error_predictor = 
           (depth_of_cut*feed_per_rev*overhang^3)/workpiece_diameter^4) %>% 
  # group by workpiece_id to sort by overhang distance and denote region id
  group_by(workpiece_id) %>% 
  # develop region id system 
  mutate(region = as.character(order(overhang,decreasing = F))) %>% 
  ungroup() %>% 
  # select variables to keep
  select(region, workpiece_id,diametral_error,roughness,
         diametral_error_predictor, roughness_predictor)

# filter data for clean_data_roughness
clean_data_roughness <- 
  clean_data %>% 
  filter(!is.na(roughness))

clean_data_diametral <- 
  clean_data %>% 
  filter(!is.na(diametral_error)) %>% 
  # outlier consideration
  filter(abs(diametral_error) < .1)

# build regression models
# model form (no intercept) - roughness
  roughness_formula <- formula(roughness ~ 0 + roughness_predictor)

# model form (no intercept) - diametral error
  diametral_formula <- formula(diametral_error ~ 0 +
                                 diametral_error_predictor:region)
# simple linear model- roughness
  lm_roughness <- lm(roughness_formula,data = clean_data_roughness)

# mixed effect model - roughness
  lme_model <- lme(fixed = roughness_formula,
      data = clean_data_roughness,
      random = ~ 1 | workpiece_id,
      correlation =  
        corGaus(form = ~ 1 | workpiece_id))
  
# linear model - diametral error
  lm_model <- lm(formula = diametral_formula,
                 data = clean_data_diametral)
```

## Executive Summary

We sought to verify two theoretical formulas used in manufacturing engineering to measure roughness and diametral error.  Using empirical data collected by an MME 231 course at Miami University we are given a dataset with 54 observations: 51 roughness measurements and 16 diametral error measurement taken on 3 regions (Region 1: Chuck-end, Region 2: Mid-workpiece, Region 3: Tip-end).  Implementing linear regression methods we used this data to build no-intercept models that would best emulate the structure given in the theoretical formulas.  Once these models were computed we compared these *predicted coefficients* to those given in the as *truth coefficients* in the theoretical formula.  We then calculated a 95% confidence interval to see if it would capture are *truth coefficient* in said interval. 

```{r executive summary chart}
# create columns
names <- c("Roughness", 
           "Diametral Error, Region 1",
           "Diametral Error, Region 2",
           "Diametral Error, Region 3")
truth <- c(1.0272,
           rep(0.1706667,3))
preds <- c(coefficients(lme_model)[1,1],
           coefficients(lm_model)[[1]],
           coefficients(lm_model)[[2]],
           coefficients(lm_model)[[3]])
c_low <- c(pmin(intervals(object = lme_model, which = "fixed")$fixed[1],0), 
           pmin(confint(object = lm_model)[[1]],0),
           pmin(confint(object = lm_model)[[2]], 0),
           pmin(confint(object = lm_model)[[3]],0))
c_his <- c(intervals(object = lme_model, which = "fixed")$fixed[3], 
           confint(object = lm_model)[[4]],
           confint(object = lm_model)[[5]],
           confint(object = lm_model)[[6]])

# create tibble put it in kable
tibble("Experiment" = names,
       "Truth Coeff." = round(truth, digits = 3),
       "Pred. Coeff." = round(preds, digits = 3),
       "Lower Bound" = round(c_low, digits = 3),
       "Upper Bound" = round(c_his, digits = 3)) %>% 
  kable(caption = "Table 1: A brief chart comparing coefficients from theoretical formulas and predicted formulas from MME 231 data.") %>%
  add_header_above(header = c(" " = 3, "95% Confidence Interval" = 2)) %>% 
  kable_styling(full_width = TRUE, bootstrap_options = 'striped')
```

As seen in Table 1, we found the theoretical roughness truth coefficient fell outside of the confidence interval - we are unable to verify the equation with the given data obtained from this experiment.  As we observe the theoretical diametral error truth coefficients fell within the confidence interval for every region - verifying the equation. However, one should be circumspect with these results and conclusions due to the high variability in our data. Also, note in the cases of predicted diametral error coefficients of `r round(preds[2], digits = 4)` and `r round(preds[4], digits = 4)` are far from the truth coefficient of `r round(truth[2], digits = 4)` and our 95% confidence intervals are so wide we cannot effectively discern this coefficient from  a negative value within the interval. 

## Introduction

### Background

A lab was conducted in an MME 231 course to demonstrate the effects of different conditions on the quality of the finished product. The students operated three manual lathes where they recorded data on lathe parameters and characteristics of the finished workpiece. This recorded data should support manufacturing and machining theory which we will explore throughout this report. 

### Motivation 

When given theoretical equations we often see the information gleaned from them as the truth.  These equation hold true when certain assumptions and ideal conditions are met, but how do they preform when outside of a controlled environment.  Will these theoretical equations be shown to be self evident if when we reverse engineer them?  

### Formulas of Interest

We are given two theoretical formulas to verify:

#### Roughness Formula

$$
\text{Roughness} = 0.0321\cdot \frac{(\text{Feed per rev})^2}{\text{(Tool nose radius)}}, \hspace{2mm}\text{where (Tool nose radius) = 1/32}
$$

#### Diametral Error Formula

$$
\text{Diametral Error} = 
\frac{128}{3} \cdot
\frac{Y\cdot(\text{Radial Depth of Cut})\cdot
(\text{Feed per rev})\cdot (\text{Overhang})^3}
{E\cdot (\text{Workpiece Diameter})^4}
, \hspace{2mm}\text{where }
\left\{
\begin{array}{l}
      E = 10,000{,}000\\
      Y = 40{,}000\\
\end{array} 
\right.
$$

We want to show this with the given data. 

#### Data Structure

The raw data we received initial had 54 observations and 21 variables.  Here is transposed sample of the first six observations.

```{r raw data first five obs.}
# filter the data
raw_sample <- t(head(raw_data, 6))[1:21,]
# organize data
organized <- bind_cols(
  "Variables" = as.character(unlist(dimnames(raw_sample)[1])),
  "Obs. 1" = raw_sample[,1], "Obs. 2" = raw_sample[,2],
  "Obs. 3" = raw_sample[,3], "Obs. 4" = raw_sample[,4],
  "Obs. 5" = raw_sample[,5], "Obs. 6" = raw_sample[,6])

# make a kable
organized %>%  
  kable(caption = "Table 2: A sample of the raw data.") %>% 
  kable_styling(c("bordered")) %>% 
  add_header_above(c(" " = 1, "Group 1" = 3, "Group 2" = 3))
```

In `R` we take the raw data and assign a `workpiece_id` for each group, as seen in Table 2, for a total of 18 groups.  We then make a series of observation-wise calculations where we calculate sums such as `turned_ideal` (Faced Diameter (in) - 2&#8901;DOC (in)) and `diametral_error` (Average of all Diameter Measurements - Turned Ideal). We then perform some transformations to convert the roughness that was originally measured in raw dataset as micro-inches to decimal fractions of an inch.  We also compute our `roughness_predictor` and `diametral_error_predictor` that will be explained in detail in our methods section. We lastly assign a `region` variable that denotes the region on the workpiece the observation was taken from. These are all denoted by proximity to the chuck, see Graphic 1. 

![Graphic 1: Workpiece Diagram with denoted regions.](`r image_location`)

After we organized the data we had to address missingness in our data. That left us with a dataset with 51 observation for roughness and 3 regions of 16 groups or 48 observations for diametral error.  In regards to outliers, we found six observations that had an a diametral error greater than a &#8530; of an inch. We believe it is reasonable to consider these to be a result of misrecorded values so we will leave them out of our analysis.  We show a short sample below.  

```{r clean_data sample}
head(clean_data, 6) %>% 
  kable(caption = "Table 3: A sample of the cleaned data.") %>% 
  kable_styling(c("bordered"))
```

#### Brief Data Dictionary 

`region`: Region of workpiece. Region 1 = Chuck-end, Region 2 = Mid-workpiece, and Region 3 = Tip-end.  
`workpiece_id`: Workpiece or "group" the observation belongs to.  
`diametral_error`: calculated value of diametral error from data.  
`roughness`: calculate value of roughness from data.  
`diametral_error_predictor`: all non-constants in diametral error formula.  
`roughness_predictor`: all non-constants in roughness formula. 

## Methods

### Theoretical Formula Adaptations

We approached the verification of these two equations through the comparison of coefficients via simple linear regression and linear mixed-effects model. We reduced the problem by merging all given constants into the coefficient. 

#### Simplified Roughness Formula

$$
\text{Roughness} = \underbrace{1.0272}_{\text{Truth Coeff.}} 
\cdot (\text{Roughness Predictor})
$$

Here the we have combined all the known constants into what we call the *truth coefficient*. In this case the coefficent is comprised of $\frac{0.0321}{\text{Tool nose radius}}$ where $\text{Tool nose radius} = \frac{1}{32}$. Simplifying this value we calculate our truth coefficient of `r round(truth[1], digits = 4)`. What we have left as our $\text{Roughness Predictor}$ is essentially an alias of $\text{(Feed per rev)}^2$ in this case.

#### Simplified Diametral Error Formula

$$
\text{Diametral Error} =
\underbrace{0.1706667}_{\text{Truth Coeff.}} 
\cdot (\text{Diametral Error Predictor})
$$

Similar to the Simplified Roughness Formula, we have combined all known constants into the *truth coefficient*. In this case that coefficient is comprised of $\frac{128\cdot Y}{3 \cdot E}$ where $E = 10{,}000{,}000$ and $Y = 40{,}000$. This value simplifies to the *truth coefficient* value of `r round(truth[2], digits = 4)`. The $\text{Diametral Error Predictor}$ is comprised of a complex mult-factor value comprised of the following, $\frac{(\text{Radial Depth of Cut})\cdot(\text{Feed per ref})\cdot (\text{Overhang})^3}{ (\text{Workpiece Diameter})^4}$, but is drastically simplified in its current form. 

### Modeling

In both modeling approaches we dropped the intercept term and performed regression through the origin as this best emulated our theoretical equations.The models we used to estimate the *predicted coefficients* are derived from the following:

#### Roughness Linear Mixed-Effects Model

$$
\text{Roughness}_{ij} = \beta
\cdot (\text{Roughness Predictor}_{ij}) + 
\varepsilon_{ij}+
\gamma_{i}
$$

We assume the error term, $\varepsilon$, is identically and independently normally distributed with a mean of 0 and some given variance of $\sigma^2$. Our random effect is defined as $\gamma$, on which we impose a normal structure with a mean of 0 and and some variance of $\sigma^2_{\gamma}$. The subscripts $i$ and $j$ represent the workpiece and regions within that workpiece respectively. ($i = 1,2,3,\dots,14,15,16$ for the 16 workpieces, and $j=1,2,3$ for the respective region.)

We chose a linear mixed-effects model to verify Roughness equation due to the suspected correlation within workpieces we would expect amongst roughness measurements on the same groups. This methodology will allow us to better account for this behavior. (see Appendix, for further explanation of *mixed-effects model*).

#### General Diametral Error Linear Model

$$
\text{Diametral Error} = \beta
\cdot (\text{Diametral Error Predictor}) + 
\varepsilon
$$
We define the error term, $\varepsilon$, to be indentically and independently normally distributed with a mean of 0 and some given variance of $\sigma^2$. This equation is computed for every region on the work piece so there are essentially three equations derived using this methodology. 

## Results

### Exploratory Data Analysis

```{r exploratory data analysis - roughness, warning=F}
# plot roughness
ggplot() +
  geom_point(aes(x = roughness_predictor, y = roughness),
              data = clean_data) +
  labs(title = "Plot 1: Exploratory Roughness Plot",
       subtitle = "Linear trend. Discrete predictor values.",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Roughness Predictor",
       y = "Roughness (in)") +
  theme_minimal()
```

We see a general positive relationship in the data. We also observe the discrete nature of our $\text{Roughness Predictor}$, which we know from the methods section is $\text{(Feed per rev)}^2$. 

```{r exploratory data analysis - diametral error, warning=F}
# plot diametral error
ggplot() +
  geom_point(aes(x = diametral_error_predictor, y = diametral_error, 
                 color = region),
              data = clean_data) +
  labs(title = "Plot 2: Exploratory Diametral Error Plot",
       subtitle = "Clear outliers.",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametral Error", 
       color = "Region") +
  theme_minimal()
```

Its hard to visualize any relationship in the data due to the strong outliers. We see some possible grouping by region, but it's hard to discern.  As we explained in our introduction, these outliers with a diametral error greater than &#8530; of an inch are understood to be from mismeasured or misrecorded data. 

```{r exploratory data analysis - diametral error (outliers removed), warning=F}
# plot diametral error w/o outliers
ggplot() +
  geom_point(aes(x = diametral_error_predictor, y = diametral_error, 
                 color = region),
              data = clean_data_diametral) +
  labs(title = "Plot 3: Exploratory Diametral Error Plot",
       subtitle = "No strong linear relationship. Grouping of our predictor.",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametral Error", 
       color = "Region") +
  theme_minimal()
```

In Plot 3, we have now removed outliers and we see no strong linear relationship.  The lack of linear relationship may inhibit the accuracy of a linear models. However, we do see some grouping in our diametral error predictor. This tells us there is relationship between region and diametral error.  In other words, as diametral error is measured toward the chuck-end the predictor approaches 0.  

### Verification of Roughness Eqn

One immediate concern that we had about the modeling of this equation is the assumption of independence in order to build proper confidence intervals. Ideally, to make an inference on a confidence interval we often develop an experiment that generates data in an independent manner, but in the case of this experiment there is going to be a lack of independence due to the `workpiece` the data was collected on. To account for this lack of independence we must build a more complex model than simple linear regression that will take into account this `workpiece` effect. We used a linear mixed effect model which accounts for `workpiece` effects (or the correlation within `workpiece`) to better estimate roughness.

```{r verify table - roughness}
# make nice 95% CI table
head(tibble("Experiment" = names, "True Coeff." = truth, "Pred. Coeff." = preds,
       "Lower Bound" = c_low, "Upper Bound" = c_his),1) %>% 
  kable(caption = "Table 4: Summary of Roughness values.") %>%
  add_header_above(header = c(" " = 3, "95% Confidence Interval" = 2)) %>% 
  kable_styling(full_width = TRUE, bootstrap_options = 'striped')
```

After we considered the grouping effect, the above confidence interval (`r round(c_low[1], digits = 4)`, `r round(c_his[1], digits = 4)`) for the predicted coefficient of roughness shows we're 95% confident that the true coefficient is in this interval. However, our theoretical coefficient, which derived from the theoretical roughness equation is `r round(truth[1], digits = 4)`. This value falls outside of our confidence interval which means we would be unable to verify this equations using our current methodology and the given data.


```{r verify plot - roughness, warning=FALSE}
# standard subtitle 
subtitle_verify <- "Theoretical line in Red. Fitted line in Blue."
# plot roughness
ggplot() +
  geom_smooth(aes(x = roughness_predictor, y = roughness),
              method = "lm",na.rm = TRUE, 
              formula = y ~ x + 0,
              data = clean_data_roughness) +
  geom_point(aes(x = roughness_predictor, y = roughness), data = clean_data) +
  geom_line(aes(x = range(clean_data$roughness_predictor),
                y = 1.0272*range(clean_data$roughness_predictor)), 
            color = "red") +
  labs(title = "Plot 4: Plot of Roughness",
       subtitle = subtitle_verify,
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Roughness Predictor",
       y = "Roughness (in)") +
  theme_bw()

```

In Plot 4 above, each dot represents one estimated roughness based on the given data compared to the theoretical roughness. We can see that the slope of the fitted regression line (<font color='blue'>blue</font>) based on the roughness model is different than the slope of the theoretical line (<font color='red'>red</font>) based on the roughness equation. 

In our analysis, there are no outliers, the grey shadow area represents number of reasonable values of the slope parameter. It means if the theoretical line falls in the grey shadow area, we should have confidence that one of reasonable values of slope parameter based on our model and data is reasonable to be closed to the theoretical slope parameter. However, it is obvious that the theoretical roughness line (<font color='blue'>blue</font>) doesn’t fall in the grey shadow area, which means we fail to verify the roughness equation.

### Verification of Diametral Error Eqn

#### Region 1
```{r verify plot - diametral error - region 1,warning=F, message=F}
# plot region I
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "1"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "1"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data_diametral, region == "1"),
            color = "red",
            size = 1,
            alpha = .8) +
  labs(title = "Plot 5: Plot of Diametral Error in Region 1",
       subtitle = subtitle_verify,
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()
```

#### Region 2
```{r,warning=FALSE,echo=FALSE}
# plot region II
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "2"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "2"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data_diametral, region == "2"),
            color = "red",
            size = 1,
            alpha = .8) +
  labs(title = "Plot 6: Plot of Diametral Error in Region 2",
       subtitle = subtitle_verify,
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw() 
```

As we can see, there exists two high leverage points in region 1 and 2 which are far away from other points in the direction of "diametral error predictor" (x-axis). We could consider these points as high leverage points in statistics (See Appendix about the term, "leverage"). We believe our estimated coefficient (<font color='blue'>blue</font>) which is an reasonably good estimation of theoretical parameter (<font color='red'>red</font>). In graph, it is shown as blue lines are very closed to red line in different regions. The reason that we have some high leverage points is that the fact that the experiment was not truly designed to properly evaluate these theoretical equations. Even if we removed these high leverage points, result and conclusion would be the same as what we found. (See Appendix,for further outputs and results after we removed high leverage points.)

#### Region 3
```{r,warning=FALSE,echo=FALSE}
# plot region III
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "3"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "3"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data_diametral, region == "3"),
            color = "red",
            size = 1,
            alpha = .8) +
  labs(title = "Plot 7: Plot of Diametral Error in Region 3",
       subtitle = subtitle_verify,
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()
```

```{r}
# make nice 95% CI table
all_table <- tibble("Experiment" = names, "True Coeff." = truth, "Pred. Coeff." = preds,
       "Lower Bound" = c_low, "Upper Bound" = c_his)
diametral_table2 <- all_table[2,]
diametral_table3 <- all_table[3,]
diametral_table4 <- all_table[4,]
diametral_table <- rbind(diametral_table2,diametral_table3,diametral_table4)
 diametral_table %>% 
  kable(caption = "Table 5: Summary of Diametral Error values.") %>%
  add_header_above(header = c(" " = 3, "95% Confidence Interval" = 2)) %>% 
  kable_styling(full_width = TRUE, bootstrap_options = 'striped')
```

Based on the model, we can see that all estimated diametral errors in three zones (<font color='blue'>blue lines</font>) matched the theoretical lines (<font color='red'>red lines</font>) pretty well.Also, the grey shadow areas represents confidence intervals. It means any appropriate fitted lines in these areas could be reasonably considered as good estimation of our theoretical line. However, this model is so uncertainty (We will explain why the model is so uncertainty in the next paragraph.) that it is unsurprising that our the theoretical lines are inside confidence intervals in three regions and *truth coefficients* seems like a plausible value. 

However, the main concern is from small R-squares--R square: 0.0078 and Adjusted R square:-0.068, of our model, although we have pretty good form of the model which matched the form of equation perfectly. (The R square is the percentage of the response variable variation that a linear model explains. More information about R squared can be found in Appendix). We have pretty low R-squared approaching zero which means our model based on the data predicts diametral error poorly (The summary output is attached in Appendix). Although the theoretical line is inside the shaded area, we still have large uncertainty because of low R squared or the terrible model.

Therefore, all conclusions based on this model are unreasonable. We still fail to verify the diametral error equation using the given experimental data.

## Discussion

### Conclusion

In this project, we verified the two given equations, built specific models and presented the result of these models. Utilizing linear regression and mixed-effects methods we build no-intercept models that would best emulate the structure given in the theoretical formulas.  Once these models were built we compared these *predicted coefficients* to those given in the as *truth coefficients* in the theoretical formula.  We then calculated a 95% confidence interval to see if it would capture are *truth coefficient* in the interval. 

To verify roughness equation, we used the linear mixed-effects model. We omitted missing observations in the given data, then took the average of the chuck and tip end diametral error for each region so that overhang distance could be counted in our models. In the process, we made a variable which is “roughness_predictor”, it consists of the square of roughness and “diametral error predictor” consists of “Faced Diameter”, “Depth of Cut”, “Overhang Distance” and “Workpiece Diameter”. 

We worried about whether grouping variables affect the model and decided to use the linear mixed effect model (LME) to test it. The linear mixed effect model consists of two part: fixed effect and random effect. Random effect part represents residuals and follows the identically independent normal distribution, which is an effect caused by grouping; “Roughness predictor” as the fixed effect. Then we construct 95% confidence interval about the roughness equation, which is (0.35, 0.56); however, the estimate of the parameter does not fall into the confidence interval. The coefficient of our model is reasonably considered as different coefficient of our theoretical equation. We have confidence to conclude that we fail to verify the equation.

For the diametral error equation, we built simple linear regression without intercept in each region. At first, we separated the workpiece into three zones as the clients' experiment designed and made scatterplots in each region. 

Based on our visualization, we found outliers in all three regions and high leverage points in region 1 and 2 (How we define outliers and leverage points could be found in  "Appendix"). We removed outliers and kept high leverage points. These high leverage points doesn't quitely affect our result of model analysis (The output before and after removing high leverage points could be found in "Appendix"). And we believe this happened because the experiment was not designed to test the theoretical equations. We found the theoretical diametral error truth coefficient fell within the interval of the confidence interval, which means we should consider the estimation of coefficient based on the data is very closed to the coefficient of the theoretical equation. However, we have very small R squares. It means the model based on the data predicts diametral error poorly and all conclusions based on this model are unreasonable. We still fail to verify our diametral error equation based on the given experimental data.

### Concerns
One issue is the theoretical equation doesn't explain enough of variation in the experimental data. We may suggest to design a new experiment to verify these equation. This experiment may have the predictors spread out. In order to reduce the variability, the experiment may need to control several factors in the theoretical equations.

Another issue is about high leverage points. We suggest clients may design the experiment for the purpose that to verify the theoretical equations. If observations could be independent without grouping, results of analysis might change.


## References 

R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Hadley Wickham (2017), package 'tidyverse', R package version 1.3.0. https://CRAN.R-project.org/package=tidyverse

Hadley Wickham, Winston Chang, Lionel Henry, Thomas Lin Pedersen, Kohske Takahashi, Claus Wilke, Kara Woo, Hiroaki Yutani (2019), package 'ggplot2',
R package version 3.2.1, https://cran.r-project.org/web/packages/ggplot2/index.html

Hadley Wickham, Jennifer Bryan, RStudio, Marcin Kalicinski, Komarov Valery , Christophe Leitienne, Bob Colbert, David Hoerl, Evan Miller(2019),package 'readxl', R package version 1.3.1, https://cran.r-project.org/web/packages/readxl/

José Pinheiro, Douglas Bates, Saikat DebRoy, Deepayan Sarkar, Siem Heisterkamp, Bert Van Willigen, R-core, 03/04/2020, package 'nlme', R package version 3.1-145,https://cran.r-project.org/web/packages/nlme/

Hao Zhu, Thomas Travison, Timothy Tsai, Will Beasley, Yihui Xie, GuangChuang Yu , Stéphane Laurent, Rob Shepherd, Yoni Sidi, Brian Salzer, George Gui, Yeliang Fan, Duncan Murdoch, 03/16/2019, package 'kableExtra', R package version 1.1.0, https://cran.r-project.org/web/packages/kableExtra/


HOME. (n.d.). Retrieved from https://stats.idre.ucla.edu/other/mult-pkg/introduction-to-linear-mixed-models/
9.1 - Distinction Between Outliers and High Leverage Observations. (n.d.). Retrieved from https://online.stat.psu.edu/stat462/node/170/

Fuqua School of Business. (n.d.). Retrieved from https://people.duke.edu/~rnau/rsquared.htm

Data source: Meet Google Drive – One place for all your files. (n.d.). Retrieved from https://drive.google.com/file/d/1s_OcuRFQD532Dsh9OP0H99rVGPwxxLaS/view

Model Source: Applied Linear Statistical Model (Fifth edition) Micheal H. Kutner

## Appendix

### Linear Mixed Effect Model
"Linear mixed-effects models are extensions of linear regression models for data that are collected and summarized in groups. These models describe the relationship between a response variable and independent variables, with coefficients that can vary with respect to one or more grouping variables. A mixed-effects model consists of two parts, fixed effects and random effects. Fixed-effects terms are usually the conventional linear regression part, and the random effects are associated with individual experimental units drawn at random from a population. The random effects have prior distributions whereas fixed effects do not. Mixed-effects models can represent the covariance structure related to the grouping of data by associating the common random effects to observations that have the same level of a grouping variable.The standard form of a linear mixed-effects model is: 


$$
\text{y} = \underbrace{X \beta}_{\text{fixed effect}} +
\underbrace{Z b}_{\text{random effect}} +
\underbrace{\epsilon}_{\text{error}} 
$$

where
y is the n-by-1 response vector, and n is the number of observations.  
X is an n-by-p fixed-effects design matrix.  
β is a p-by-1 fixed-effects vector.  
Z is an n-by-q random-effects design matrix.  
b is a q-by-1 random-effects vector.  
ε is the n-by-1 observation error vector."  

Citation:https://www.mathworks.com/help/stats/linear-mixed-effects-models.html

### Outlier and High Leverage
Outliers:
"An outlier is a data point whose response y does not follow the general trend of the rest of the data."

High leverage:
"A data point has high leverage if it has "extreme" predictor x values. With a single predictor, an extreme x value is simply one that is particularly high or low. With multiple predictors, extreme x values may be particularly high or low for one or more predictors, or may be "unusual" combinations of predictor values (e.g., with two predictors that are positively correlated, an unusual combination of predictor values might be a high value of one predictor paired with a low value of the other predictor)."

Citation and Examples:
https://online.stat.psu.edu/stat462/node/170/

### $R^2$ and Adjusted $R^2$

*R squared*:  
"R-squared is the “percent of variance explained” by the model. That is, R-squared is the fraction by which the variance of the errors is less than the variance of the dependent variable.  (The latter number would be the error variance for a constant-only model, which merely predicts that every observation will equal the sample mean.) It is called R-squared because in a simple regression model it is just the square of the correlation between the dependent and independent variables, which is commonly denoted by “r”." 

*Adjusted R Squared*:  
Generally it is better to look at adjusted R-squared rather than R-squared and to look at the standard error of the regression rather than the standard deviation of the errors.  These are unbiased estimators that correct for the sample size and numbers of coefficients estimated. Adjusted R-squared is always smaller than R-squared, but the difference is usually very small unless you are trying to estimate too many coefficients from too small a sample in the presence of too much noise. Specifically, adjusted R-squared is equal to 1 minus (n - 1)/(n – k - 1) times 1-minus-R-squared, where n is the sample size and k is the number of independent variables.   (It is possible that adjusted R-squared is negative if the model is too complex for the sample size and/or the independent variables have too little predictive value, and some software just reports that adjusted R-squared is zero in that case.) Adjusted R-squared bears the same relation to the standard error of the regression that R-squared bears to the standard deviation of the errors: one necessarily goes up when the other goes down for models fitted to the same sample of the same dependent variable.

Citation:
https://people.duke.edu/~rnau/rsquared.htm


### Diametral Error Summary Output (Used in Analysis)
```{r,warning=FALSE}
summary(lm_model)
```

### Summary Output and Plots (Removed high leverage points)

```{r,warning=FALSE}
data_r1 <- clean_data_diametral %>% filter(region == "1") %>%
  filter(diametral_error_predictor < 0.0075)
data_r2 <- clean_data_diametral %>% filter(region == "2") %>%
  filter(diametral_error_predictor < 0.015)
data_r3 <- clean_data_diametral %>% filter(region == "3")
no_leverage_data <- rbind(data_r1,data_r2,data_r3)

# plot region I without leverage
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "1"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "1"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(no_leverage_data, region == "1"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region I (no leverage)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw() 

# plot region II without leverage
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "2"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "2"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(no_leverage_data, region == "2"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region II (no leverage)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw() 

lm_no_leverage <- lm(formula = diametral_error ~
                       diametral_error_predictor:region + 0,
                     data = no_leverage_data)
summary(lm_no_leverage)

```

Our main concern isn't solved. We still have small R squares, even less than our previous model without remove high leverage points due to remove observations.

### Code Used
```
# load necessary libraries
library(tidyverse)        # how R works
library(ggplot2)          # how plotting in R works
library(readxl)           # how R reads .xlsx's
library(nlme)             # how R does mixed linear models
library(kableExtra)       # add-on to knitr
library(knitr)            # producing nice html tables in R

# load the data
raw_data <- read_excel("/Users/stephen/Desktop/STA475/Capstone\ Group\ Project/Copy\ of\ F19\ Sections\ ABC\ DEF\ Names\ Removed.xlsx")

# clean the data 
clean_data <- raw_data %>% 
  # create a sorting variables
  mutate(row_id = 1:54, workpiece_id = rep(1:18, each = 3)) %>% 
  # group by rows to perform row-wise calculations
  group_by(row_id) %>% 
  # perform row-wise calculations
  mutate(
    # formula for theoretical turned diameter
    turned_ideal = `Faced Diameter (in)` - 2*`DOC (in)`,
    # average chuck diameter measurements
    diameter_chuck = mean(`Chuck end diameter 1`,
                          `Chuck end diameter 2`,
                          `Chuck end diameter 3`),
    # average tip diameter measurements
    diameter_tip = mean(`Tip-end diameter 1 (in)`,
                        `Tip-end diameter 2`,
                        `Tip-end diameter 3`),
    # average two previous diameter measurements
    diameter_mean = mean(diameter_tip,diameter_chuck),
    # formula for diametral error (empirical)
    diametral_error = diameter_mean - turned_ideal,
    # average overhang distance
    overhang = mean(`From [approx distance] (close to  the chuck)`,
                    `to [approx distance] (close to the tip)`)) %>% 
  # ungroup by row_id
  ungroup() %>% 
  # clean up variable names & calculate predictors
  mutate(feed_per_rev = `f (in/rev)`,
         roughness = `Roughness (micro-in)`*.000001,
         depth_of_cut = `DOC (in)`,
         workpiece_diameter = `Faced Diameter (in)`,
         roughness_predictor = feed_per_rev^2,
         diametral_error_predictor = 
           (depth_of_cut*feed_per_rev*overhang^3)/workpiece_diameter^4) %>% 
  # group by workpiece_id to sort by overhang distance and denote region id
  group_by(workpiece_id) %>% 
  # develop region id system 
  mutate(region = as.character(order(overhang,decreasing = F))) %>% 
  ungroup() %>% 
  # select variables to keep
  select(region, workpiece_id,diametral_error,roughness,
         diametral_error_predictor, roughness_predictor)

# filter data for clean_data_roughness
clean_data_roughness <- 
  clean_data %>% 
  filter(!is.na(roughness))

clean_data_diametral <- 
  clean_data %>% 
  filter(!is.na(diametral_error))%>%
  filter(diametral_error > -0.1)

# build regression models
# model form (no intercept) - roughness
  roughness_formula <- formula(roughness ~ 0 + roughness_predictor)

# model form (no intercept) - diametral error
  diametral_formula <- formula(diametral_error ~ 0 +
                                 diametral_error_predictor:region)

# mixed effect model - roughness
  lme_model <- lme(fixed = roughness_formula,
      data = clean_data_roughness,
      random = ~ 1 | workpiece_id,
      correlation =  
        corGaus(form = ~ 1 | workpiece_id))
  
# linear model - diametral error
  diametral_error_lm <- lm(formula = diametral_formula,
                 data = clean_data_diametral)
# create columns
names <- c("Roughness", 
           "Diametral Error, Region 1",
           "Diametral Error, Region 2",
           "Diametral Error, Region 3")
truth <- c(1.0272,
           rep(0.1706667,3))
preds <- c(coefficients(lme_model)[1,1],
           coefficients(diametral_error_lm)[[1]],
           coefficients(diametral_error_lm)[[2]],
           coefficients(diametral_error_lm)[[3]])
c_low <- c(intervals(object = lme_model, which = "fixed")$fixed[1], 
           confint(object = diametral_error_lm)[[1]],
           confint(object = diametral_error_lm)[[2]],
           confint(object = diametral_error_lm)[[3]])
c_his <- c(intervals(object = lme_model, which = "fixed")$fixed[3], 
           confint(object = diametral_error_lm)[[4]],
           confint(object = diametral_error_lm)[[5]],
           confint(object = diametral_error_lm)[[6]])

# create tibble put it in kable
tibble("Experiments" = names, "True Coeff." = truth, "Pred. Coeff." = preds,
       "Lower Bound" = c_low, "Upper Bound" = c_his) %>% 
  kable(caption = "Table 1: A brief chart comparing coefficients from theoretical formulas and predicted formulas from MME 231 data.") %>%
  add_header_above(header = c(" " = 3, "95% Confidence Interval" = 2)) %>% 
  kable_styling(full_width = TRUE, bootstrap_options = 'striped')
# filter the data
raw_sample <- t(head(raw_data, 6))[1:21,]
# organize data
organized <- bind_cols(
  "Variables" = as.character(unlist(dimnames(raw_sample)[1])),
  "Obs. 1" = raw_sample[,1], "Obs. 2" = raw_sample[,2],
  "Obs. 3" = raw_sample[,3], "Obs. 4" = raw_sample[,4],
  "Obs. 5" = raw_sample[,5], "Obs. 6" = raw_sample[,6])

# make a kable
organized %>%  
  kable(caption = "Table 2: A sample of the raw data.") %>% 
  kable_styling(c("bordered")) %>% 
  add_header_above(c(" " = 1, "Group 1" = 3, "Group 2" = 3))
head(clean_data, 6) %>% 
  kable(caption = "Table 3: A sample of the cleaned data.") %>% 
  kable_styling(c("bordered"))

##EDA
# plot region I
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data, region == "1"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data, region == "1"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data, region == "1"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error Region I (Original)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()

# plot region II
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data, region == "2"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data, region == "2"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data, region == "2"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error Region II (Original)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()

# plot region III
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data, region == "3"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data, region == "3"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data, region == "3"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error Region III (Original)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()

##Result
## To verify roughness equation
roughness_lm <- lm(formula = roughness ~ roughness_predictor + 0, 
                   data = clean_data)
confint(roughness_lm)

roughness_formula <- formula(roughness ~ 0 + roughness_predictor )


lme_model <- lme(fixed = roughness_formula,
    data = clean_data_diametral,
    random = ~ 1| workpiece_id,
    correlation =  
      corGaus(form = ~ 1 | workpiece_id))

intervals(lme_model, which = "fixed")

ggplot() +
  geom_smooth(aes(x = roughness_predictor, y = roughness),
              method = "lm",na.rm = TRUE, 
              formula = y ~ x + 0,
              data = clean_data) +
  geom_point(aes(x = roughness_predictor, y = roughness), data = clean_data) +
  geom_line(aes(x = range(clean_data$roughness_predictor),
                y = 1.0272*range(clean_data$roughness_predictor)), 
            color = "yellow") +
  labs(title = "Plot of Roughness",
       subtitle = "Theoretical line in Yellow; Fitted line in Blue",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Roughness Predictor",
       y = "Roughness (in)") +
  theme_bw()

##To verify diametral error equation
diametral_error_lm <- 
  lm(formula = diametral_error ~ diametral_error_predictor:region + 0
     , data = clean_data_diametral)
#plot region I
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "1"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "1"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data_diametral, region == "1"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region I",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()

# plot region II
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "2"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "2"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data_diametral, region == "2"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region II",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw() 

# plot region III
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "3"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(clean_data_diametral, region == "3"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(clean_data_diametral, region == "3"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region III",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw()

summary(diametral_error_lm)$r.squared
summary(diametral_error_lm)$adj.r.squared

##Appendix
summary(diametral_error_lm)

data_r1 <- clean_data_diametral %>% filter(region == "1") %>%
  filter(diametral_error_predictor < 0.0075)
data_r2 <- clean_data_diametral %>% filter(region == "2") %>%
  filter(diametral_error_predictor < 0.015)
data_r3 <- clean_data_diametral %>% filter(region == "3")
no_leverage_data <- rbind(data_r1,data_r2,data_r3)
# plot region I without leverage
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "1"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "1"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(no_leverage_data, region == "1"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region I (no leverage)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw() 

# plot region II without leverage
ggplot() +
  geom_smooth(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "2"),
              method = "lm", formula = y ~ x +0,
              color = "blue") +
    geom_point(aes(x = diametral_error_predictor, y = diametral_error), 
              data = filter(no_leverage_data, region == "2"),
             color = "black",
             alpha = .5) +
  geom_line(aes(x = diametral_error_predictor, y = 0.1706667*diametral_error_predictor),
            data =  filter(no_leverage_data, region == "2"),
            color = "yellow",
            size = 1,
            alpha = .8) +
  labs(title = "Plot of Diametral Error in Region II (no leverage)",
       subtitle = "Theoretical line in yellow",
       caption = "Data: Copy of F19 Sections ABC DEF Names Removed",
       x = "Diametral Error Predictor",
       y = "Diametrial Error") +
  theme_bw() 

lm_no_leverage <- lm(formula = diametral_error ~
                       diametral_error_predictor:region + 0,
                     data = no_leverage_data)
summary(lm_no_leverage)
```
