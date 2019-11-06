# Overig

### Counts

We created profiles for patients based on their HADS anxiety and depression score at baseline and after 2 years. For example, the profile: "non - non" means patients who were a non-case at baseline and are still non-case after 2 years.

Below the number of occurences of each profile. It is clear that most patients have a non - none profile. Also the profiles "case - case", "doubtful - doubtful" and "doughtful" only occur very few times. This makes it very difficult to perform a statistical analysis.

```{r, echo=TRUE}

df_profiles %>% 
  distinct() %>%
  count(profile_anx_labels)

df_profiles %>% 
  distinct() %>%
  count(profile_depr_labels)

```

Number of occurences of depressions/anxiety at baseline

```{r}
df_long %>%
  count(baseline_depr_labels)


df_long %>%
  count(baseline_anx_labels)

```





### NDI over time for different depression and anxiety profiles

NDI scores over time for the different depression and anxiety profiles


```{r}
p1b <-  ggplot(data = df_long, aes(x = time, y = ndi, col = id))  +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = id)) +
  ggtitle("Raw data: NDI score over time for different depression profiles") + 
  ylab("NDI score") + 
  xlab("Time after surgery in weeks") + theme(legend.position = "none") + 
  facet_grid(~profile_depr_labels) + 
  theme



plot(p1b)


p1c <-  ggplot(data = df_long, aes(x = time, y = ndi, col = id))  +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = id)) +
  ggtitle("Raw data: NDI score over time for different anxiety profiles") + 
  ylab("NDI score") + 
  xlab("Time after surgery in weeks") + theme(legend.position = "none") + 
  facet_grid(~profile_anx_labels) + 
  theme


plot(p1c)


```


## A few more graphs

```{r}
p1 <-  ggplot(data = df_all, aes(x = time, y = ndi, col = id))  +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = id)) +
  ggtitle("NDI score over time per surgery type") + 
  ylab("NDI score") + 
  xlab("Time after surgery in weeks") + theme(legend.position = "none") + 
  facet_grid(~surgery_labels) +
  theme

plot(p1)


p1a <-  ggplot(data = df_all, aes(x = time, y= ndi, col=id, na.rm = T))  +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = id)) +
  ggtitle("NDI score over time with HADS depression at baseline") + 
  ylab("NDI score") + 
  xlab("Time after surgery in weeks") + theme(legend.position = "none") + 
  facet_grid(~baseline_depr_labels) + 
  theme

plot(p1a)

p1b <-  ggplot(data = df_long, aes(x = time, y = ndi, col = id))  +
  geom_point(size = 4, alpha = 0.7) +
  geom_line(aes(group = id)) +
  ggtitle("NDI score over time with HADS anxiety at baseline") + 
  ylab("NDI score") + 
  xlab("Time after surgery in weeks") + theme(legend.position = "none") + 
  facet_grid(~baseline_anx_labels) +
  theme


plot(p1b)


```

