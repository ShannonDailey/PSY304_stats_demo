## Personality ratings

**Social participation**: mean rating = `r round(mean(data$soc_particip, na.rm = TRUE), 2)`/5, standard deviation = `r round(sd(data$soc_particip, na.rm = TRUE), 2)`, range of `r min(data$soc_particip, na.rm = TRUE)`--`r max(data$soc_particip, na.rm = TRUE)`/5

```{r}
hist(data$soc_particip)
```

**Bossiness**: mean rating = `r round(mean(data$bossy, na.rm = TRUE), 2)`/3, standard deviation = `r round(sd(data$bossy, na.rm = TRUE), 2)`, range of `r min(data$bossy, na.rm = TRUE)`--`r max(data$bossy, na.rm = TRUE)`/3

```{r}
hist(data$bossy)
```

**Positive affect**: mean rating = `r round(mean(data$pos_affect, na.rm = TRUE), 2)`/4, standard deviation = `r round(sd(data$pos_affect, na.rm = TRUE), 2)`, range of `r min(data$pos_affect, na.rm = TRUE)`--`r max(data$pos_affect, na.rm = TRUE)`/4

```{r}
hist(data$pos_affect)
```

**Socially awkward**: mean rating = `r round(mean(data$awkward, na.rm = TRUE), 2)`/3, standard deviation = `r round(sd(data$awkward, na.rm = TRUE), 2)`, range of `r min(data$awkward, na.rm = TRUE)`--`r max(data$awkward, na.rm = TRUE)`/3

```{r}
hist(data$awkward)
```