
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

```{r}

data_punish_mean <- data_punish_all %>% 
  group_by(ID2, domain, treatment) %>% 
  summarize(mean_rating = mean(rating))

data_app_all_mean <- data_app_all %>% 
  select(ID2, treatment, rating )

data_cor <- merge(data_punish_mean,data_app_all_mean, by = c('ID2', 'treatment' ))

ggplot(data = data_cor, aes(fill = treatment)) +
  geom_half_boxplot(aes(x= as_factor(rating), y = mean_rating)) + 
  geom_half_point(aes(x= as_factor(rating), y = mean_rating)) +
  geom_smooth(aes(x= rating + 1, y = mean_rating),method = "lm") +
  labs(x = "violation",
       y = "punishment") +
  theme_cowplot() +
  theme(strip.background = element_rect(fill = "lightgrey")) +
  scale_fill_brewer(palette = "Set2")  + 
  scale_color_brewer(palette = "Set2")  

ggplot(data = data_cor, aes(fill = treatment)) +
  geom_half_boxplot(aes(x= as_factor(rating), y = mean_rating)) + 
  geom_half_point(aes(x= as_factor(rating), y = mean_rating)) +
  geom_smooth(aes(x= rating + 1, y = mean_rating),method = "lm") +
  labs(x = "violation",
       y = "punishment") +
  theme_cowplot() +
  theme(strip.background = element_rect(fill = "lightgrey")) +
  scale_fill_brewer(palette = "Set2")  + 
  scale_color_brewer(palette = "Set2")  +
  facet_wrap(~domain)

cor(data_cor$mean_rating, data_cor$rating)

```

```{r}


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

