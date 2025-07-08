
## behaviors one by one
### stealing

```{r, fig.width = 4, fig.height = 6 }
sjPlot::tab_model(lme4::lmer(rating ~ treatment + (1|ID2), data = data_punish_steal))
```


### behav 2: phone in class

```{r}


sjPlot::tab_model(lme4::lmer(rating ~ treatment + (1|ID2), data = data_punish_phone))

```

### behav 3: interrupt

```{r}


sjPlot::tab_model(lme4::lmer(rating ~ treatment + (1|ID2), data = data_punish_interrupt))

```

### behav 4: insult


## corr violation \* punishment






sjPlot::tab_model(lme4::lmer(rating ~ treatment + (1|ID2), data = data_punish_insult))

```

### behav 5: exclude_chat

```{r}


sjPlot::tab_model(lme4::lmer(rating ~ treatment + (1|ID2), data = data_punish_online))

```


## Interactions

###

```{r}
sjPlot::tab_model(lme4::lmer(rating ~ treatment * punishment + (1|ID2), data = data_punish_steal))
```

###

```{r}
sjPlot::tab_model(lme4::lmer(rating ~ treatment * punishment + (1|ID2), data = data_punish_phone))


```

###

```{r}
sjPlot::tab_model(lme4::lmer(rating ~ treatment * punishment + (1|ID2), data = data_punish_interrupt))

```

###

```{r}
sjPlot::tab_model(lme4::lmer(rating ~ treatment * punishment + (1|ID2), data = data_punish_insult))

```
###

```{r}
sjPlot::tab_model(lme4::lmer(rating ~ treatment * punishment + (1|ID2), data = data_punish_online))

```

