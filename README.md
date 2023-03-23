Logistic Regression for Small Sample

## Author
- Tra T. Le | Tilburg University

## Description
A synthesis of different logistic regression methods to deal with a small sample. The paper is written as a literature review and R tutorials demonstrating different techniques for social scientists. 

The repository consists of the codes for the **Simulation Study** to compare the three methods: Exact Logistic Regression, Permutation of Regressor Residual test, and Firth's Penalized Logistic Regression. 

## Simulation Study
The population model
$$log(\frac{\pi_i}{1-\pi_i}) = -1 + \beta_1*x_1 + 1*x_2$$

Conditions
- Sample size: 20
- Type of conditioning variable $x_2$: binary and continuous
- Population value of parameter $\beta_1$: 0.075, 2, 3
