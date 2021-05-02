Bayesian vs. Frequentist Data Analysis
================
Joshua Cook
5/3/2021

``` r
real_intercept <- 1
real_slope <- 0.25
real_sigma <- 0.2

N <- 25
a <- rnorm(N, mean = real_intercept, sd = real_sigma)
b <- rnorm(N, mean = real_intercept + real_slope, sd = real_sigma)

data <- tibble(A = a, B = b) %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "value")
head(data)
```

    ## # A tibble: 6 x 2
    ##   group value
    ##   <chr> <dbl>
    ## 1 A     1.25
    ## 2 B     1.35
    ## 3 A     0.935
    ## 4 B     1.47
    ## 5 A     1.27
    ## 6 B     1.11

``` r
data %>%
  ggplot(aes(x = group, y = value, color = group, fill = group)) +
  geom_quasirandom() +
  geom_boxplot(alpha = 0.25, outlier.color = NA) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme(
    text = element_text(color = "white"),
    line = element_line(color = "white"),
    axis.ticks = element_line(color = "white"),
    axis.ticks.x = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.line = element_line(color = "white"),
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(size = 12, face = "bold", color = "white"),
    panel.background = element_rect(fill = BACKGROUND_BLUE, color = NA),
    plot.background = element_rect(fill = BACKGROUND_BLUE, color = NA)
  ) +
  labs(y = "measured value")
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggsave("images/boxplot.png", width = 2.4, height = 3, units = "in", dpi = 300)
```

``` r
t.test(value ~ group, data = data)
```

    ##
    ##  Welch Two Sample t-test
    ##
    ## data:  value by group
    ## t = -4.6523, df = 47.999, p-value = 2.606e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.3488867 -0.1383230
    ## sample estimates:
    ## mean in group A mean in group B
    ##        1.007984        1.251589

``` r
summary(aov(value ~ group, data = data))
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)
    ## group        1 0.7418  0.7418   21.64 2.61e-05 ***
    ## Residuals   48 1.6451  0.0343
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
freq_model <- lm(value ~ group, data = data)
summary(freq_model)
```

    ##
    ## Call:
    ## lm(formula = value ~ group, data = data)
    ##
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max
    ## -0.31597 -0.12615 -0.04789  0.14449  0.47295
    ##
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  1.00798    0.03703  27.224  < 2e-16 ***
    ## groupB       0.24360    0.05236   4.652 2.61e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ##
    ## Residual standard error: 0.1851 on 48 degrees of freedom
    ## Multiple R-squared:  0.3108, Adjusted R-squared:  0.2964
    ## F-statistic: 21.64 on 1 and 48 DF,  p-value: 2.606e-05

``` r
confint(freq_model)
```

    ##                 2.5 %    97.5 %
    ## (Intercept) 0.9335383 1.0824293
    ## groupB      0.1383230 0.3488866

``` r
bayes_model <- stan_glm(value ~ group, data = data)
```

    ##
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
    ## Chain 1:
    ## Chain 1: Gradient evaluation took 0.000136 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.36 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1:
    ## Chain 1:
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1:
    ## Chain 1:  Elapsed Time: 0.040141 seconds (Warm-up)
    ## Chain 1:                0.062232 seconds (Sampling)
    ## Chain 1:                0.102373 seconds (Total)
    ## Chain 1:
    ##
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
    ## Chain 2:
    ## Chain 2: Gradient evaluation took 1.8e-05 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2:
    ## Chain 2:
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2:
    ## Chain 2:  Elapsed Time: 0.046132 seconds (Warm-up)
    ## Chain 2:                0.054443 seconds (Sampling)
    ## Chain 2:                0.100575 seconds (Total)
    ## Chain 2:
    ##
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
    ## Chain 3:
    ## Chain 3: Gradient evaluation took 1.8e-05 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.18 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3:
    ## Chain 3:
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3:
    ## Chain 3:  Elapsed Time: 0.047349 seconds (Warm-up)
    ## Chain 3:                0.059956 seconds (Sampling)
    ## Chain 3:                0.107305 seconds (Total)
    ## Chain 3:
    ##
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
    ## Chain 4:
    ## Chain 4: Gradient evaluation took 1.6e-05 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.16 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4:
    ## Chain 4:
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4:
    ## Chain 4:  Elapsed Time: 0.046677 seconds (Warm-up)
    ## Chain 4:                0.054337 seconds (Sampling)
    ## Chain 4:                0.101014 seconds (Total)
    ## Chain 4:

``` r
summary(bayes_model)
```

    ##
    ## Model Info:
    ##  function:     stan_glm
    ##  family:       gaussian [identity]
    ##  formula:      value ~ group
    ##  algorithm:    sampling
    ##  sample:       4000 (posterior sample size)
    ##  priors:       see help('prior_summary')
    ##  observations: 50
    ##  predictors:   2
    ##
    ## Estimates:
    ##               mean   sd   10%   50%   90%
    ## (Intercept) 1.0    0.0  1.0   1.0   1.1
    ## groupB      0.2    0.1  0.2   0.2   0.3
    ## sigma       0.2    0.0  0.2   0.2   0.2
    ##
    ## Fit Diagnostics:
    ##            mean   sd   10%   50%   90%
    ## mean_PPD 1.1    0.0  1.1   1.1   1.2
    ##
    ## The mean_ppd is the sample average posterior predictive distribution of the outcome variable (for details see help('summary.stanreg')).
    ##
    ## MCMC diagnostics
    ##               mcse Rhat n_eff
    ## (Intercept)   0.0  1.0  3513
    ## groupB        0.0  1.0  3533
    ## sigma         0.0  1.0  3826
    ## mean_PPD      0.0  1.0  3877
    ## log-posterior 0.0  1.0  1758
    ##
    ## For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

``` r
describe_posterior(bayes_model, ci = 0.95)
```

    ## Summary of Posterior Distribution
    ##
    ## Parameter   | Median |       95% CI |   pd |          ROPE | % in ROPE |  Rhat |     ESS
    ## ----------------------------------------------------------------------------------------
    ## (Intercept) |   1.01 | [0.94, 1.08] | 100% | [-0.02, 0.02] |        0% | 1.000 | 3513.00
    ## groupB      |   0.24 | [0.14, 0.35] | 100% | [-0.02, 0.02] |        0% | 1.000 | 3533.00

``` r
posteriors <- as.data.frame(bayes_model) %>%
  as_tibble() %>%
  janitor::clean_names()
head(posteriors)
```

    ## # A tibble: 6 x 3
    ##   intercept group_b sigma
    ##       <dbl>   <dbl> <dbl>
    ## 1     1.02    0.265 0.182
    ## 2     1.01    0.227 0.206
    ## 3     1.04    0.232 0.169
    ## 4     0.974   0.257 0.199
    ## 5     0.993   0.182 0.183
    ## 6     1.07    0.255 0.199

``` r
post_description <- as_tibble(describe_posterior(bayes_model, ci = 0.95)) %>%
  janitor::clean_names() %>%
  mutate(parameter = case_when(
    parameter == "(Intercept)" ~ "intercept",
    parameter == "groupB" ~ "slope"
  ))

poster_ci <- post_description %>%
  select(parameter, ci_low, ci_high) %>%
  pivot_longer(-parameter)
```

``` r
posteriors %>%
  mutate(sample_id = row_number()) %>%
  select(-sigma) %>%
  pivot_longer(
    -sample_id,
    names_to = "parameter",
    values_to = "posterior_sample"
  ) %>%
  mutate(parameter = case_when(
    parameter == "group_b" ~ "slope",
    TRUE ~ parameter
  )) %>%
  ggplot(aes(x = posterior_sample)) +
  facet_wrap(vars(parameter), nrow = 1, scales = "free") +
  geom_density(color = "#970E53", fill = "#970E53", ) +
  geom_vline(
    aes(xintercept = median),
    data = post_description,
    color = "#56C1FF",
    alpha = 0.7
  ) +
  geom_vline(
    aes(xintercept = value),
    data = poster_ci,
    linetype = 2,
    color = "#56C1FF",
    alpha = 0.7
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.02))) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(color = "white", face = "bold"),
    text = element_text(color = "white"),
    line = element_line(color = "white"),
    axis.ticks = element_line(color = "white"),
    legend.position = "none",
    axis.line = element_line(color = "white"),
    axis.text = element_text(color = "white"),
    panel.background = element_rect(fill = BACKGROUND_BLUE, color = NA),
    plot.background = element_rect(fill = BACKGROUND_BLUE, color = NA)
  ) +
  labs(x = "posterior estimate", y = "density")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
ggsave("images/posteriors.png", width = 6, height = 2, units = "in")
```
