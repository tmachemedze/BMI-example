# BMI-example from the Stata course 

In this exercise, I follow the BMI examples in the Stata course.

We have the following variables:

`hhid`	 - "Household identifier"
`pid`	- "Individual identifier"
`w1_a_best_age_yrs`	- "Best age in years"
`w1_a_b2`	- "Gender"
`w1_a_n1_1`	- "Height measure one"
`w1_a_n2_1`	- "Weight measure one"


# import the data
```{r}
nids<-read.csv("./data/nids.csv")
```

## Exploration

```{r}
head(nids, n = 10L)
```

```{r}
tail(nids, n = 10L)
```