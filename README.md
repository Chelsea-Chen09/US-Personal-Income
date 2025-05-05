# Income Determinants Analysis Using Regression in R

This project uses regression modeling in R to analyze how individual characteristics—such as education, citizenship status, age, and work hours—influence total income. The data is drawn from the Integrated Public Use Microdata Series (IPUMS) and cleaned to focus on representative observations.

## Key Features

- Cleaned and preprocessed U.S. microdata (IPUMS) for analysis
- Re-coded categorical variables and filtered outliers using Cook’s Distance
- Performed multiple linear regression and hierarchical linear modeling (HLM)
- Explored interaction effects and conducted diagnostic checks (linearity, normality, homoscedasticity)
- Evaluated model fit with AIC and ANOVA comparisons
- Final model includes level-1 and level-2 variables with random slopes by family unit

## Technologies Used

- R  
- `ipumsr`, `car`, `nlme`, `stargazer`, `ggplot2`  
- Linear modeling (`lm`) and multilevel modeling (`lme`)

## Outcome

The analysis identified education, work hours, and number of children as key predictors of income, with interaction terms and multilevel modeling improving interpretability and model fit.

## Author

Chelsea Chen

This work was completed as part of a regression assignment and received full academic credit for rigorous analysis and modeling.
