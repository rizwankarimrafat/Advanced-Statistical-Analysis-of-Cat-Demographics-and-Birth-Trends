# Advanced Statistical Analysis of Cat Demographics and Birth Trends

This project explores the statistical analysis of cat demographics and birth trends, with a focus on identifying patterns related to cat breeds, birth locations, and factors influencing litter sizes. The dataset provides insights into how various factors such as family income, gestation period, and geographical location impact cat births.

## Key Analyses Performed

1. **Seasonal Patterns in Cat Births:** The project investigates the times of the year with the highest number of cat births. By modeling the density of births throughout the year, it was found that cat births exhibit a bimodal distribution, with peaks occurring during March-April and October-November. A mixture model was applied to estimate the parameters governing these trends, providing a mathematical framework to predict birth patterns based on the time of year.

2. **Breed and Income Correlation:** The analysis examined whether certain cat breeds are associated with higher family incomes, potentially indicating social status symbols. By estimating the probability of each breed conditioned on family income, the project identified that Shorthair cats are associated with the highest median incomes, followed by Burmese and Bengal cats. The findings suggest a possible link between cat breed choice and socio-economic status.

3. **Gestation Period Influences:** The relationship between family income and gestation period was explored to understand how socio-economic factors might influence a cat's development in the womb. A regression mixture model was fitted to the data, revealing clusters that suggest varying gestation periods depending on income levels. This analysis opens up discussions on how unobserved variables, such as diet or environment, might play a role in gestation duration.

4. **Birth Location Density Modeling:** Finally, the project modeled the geographical distribution of cat births to identify areas with higher birth densities. By comparing locations like Sydney and Parramatta, it was determined that Parramatta is more likely to have a higher density of cat births. This conclusion was drawn using a multivariate normal mixture model, which provided the best fit for the data.

## Conclusion

This project offers a comprehensive statistical analysis of factors affecting cat demographics and birth trends. Through the application of advanced statistical models, the project not only uncovers key trends and correlations but also provides predictive models that can be used to anticipate future patterns in cat populations.
