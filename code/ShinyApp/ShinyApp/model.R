#### Model ####
##### Age Distribution ##### 
age.pop.2022 <- c(9.48, 10.23, 10.7, 11.08, 11.6, 11.35, 11.84, 11.3, 10.82, 9.84, 10.43, 10.37, 10.3, 8.97, 7.04, 4.91, 2.83, 2.28)
ages <- seq(2, 87, by = 5)

##### Model Result ##### 
## Bootstrapping
samples1970 = c(0,0,0,0,10,26,17,21,60,32,25,21,15,12,8,0,1,0)
populations2022 = c(9.48, 10.23, 10.7, 11.08, 11.6, 11.35, 11.84, 11.3, 10.82, 9.84, 10.43, 10.37, 10.3, 8.97, 7.04, 4.91, 2.83, 2.28)

df_weight = df
# remove the sample at age 81
df = df[-which(df$IDNO==79),]

# new samples and populations by age group
pop1970 = c(10,26,17,21,60,32,25,21,15,12,8)
pop2022 = c(11.6, 11.35, 11.84, 11.3, 10.82, 9.84, 10.43, 10.37, 10.3, 8.97, 7.04)
# the 1970 and 2022 age distributions
dist1970 = pop1970 / sum(pop1970)
dist2022 = pop2022 / sum(pop2022)

set.seed(123)

# separate 1970 data set by age groups
df_mod = df
df24 = df_mod[df_mod$AGE<=24,]
df29 = df_mod[df_mod$AGE>24&df_mod$AGE<=29,]
df34 = df_mod[df_mod$AGE>29&df_mod$AGE<=34,]
df39 = df_mod[df_mod$AGE>34&df_mod$AGE<=39,]
df44 = df_mod[df_mod$AGE>39&df_mod$AGE<=44,]
df49 = df_mod[df_mod$AGE>44&df_mod$AGE<=49,]
df54 = df_mod[df_mod$AGE>49&df_mod$AGE<=54,]
df59 = df_mod[df_mod$AGE>54&df_mod$AGE<=59,]
df64 = df_mod[df_mod$AGE>59&df_mod$AGE<=64,]
df69 = df_mod[df_mod$AGE>64&df_mod$AGE<=69,]
df74 = df_mod[df_mod$AGE>69&df_mod$AGE<=74,]

# random selection process. Note that age group 24- has only 10 samples, no need to randomly select.
rd29 <- sample(1:length(df29[,1]), 10, replace = FALSE)
rd34 <- sample(1:length(df34[,1]), 10, replace = FALSE)
rd39 <- sample(1:length(df39[,1]), 9, replace = FALSE)
rd44 <- sample(1:length(df44[,1]), 9, replace = FALSE)
rd49 <- sample(1:length(df49[,1]), 8, replace = FALSE)
rd54 <- sample(1:length(df54[,1]), 9, replace = FALSE)
rd59 <- sample(1:length(df59[,1]), 9, replace = FALSE)
rd64 <- sample(1:length(df64[,1]), 9, replace = FALSE)
rd69 <- sample(1:length(df69[,1]), 8, replace = FALSE)
rd74 <- sample(1:length(df74[,1]), 6, replace = FALSE)

# new data set
df_sample = rbind(df24,df29[rd29,],df34[rd34,],df39[rd39,],df44[rd44,],df49[rd49,],df54[rd54,],df59[rd59,],df64[rd64,],df69[rd69,],df74[rd74,])

# new data set above
df_sample_comb = df_sample

for (i in 1:4){
  rd29 <- sample(1:length(df29[,1]), 10, replace = FALSE)
  rd34 <- sample(1:length(df34[,1]), 10, replace = FALSE)
  rd39 <- sample(1:length(df39[,1]), 9, replace = FALSE)
  rd44 <- sample(1:length(df44[,1]), 9, replace = FALSE)
  rd49 <- sample(1:length(df49[,1]), 8, replace = FALSE)
  rd54 <- sample(1:length(df54[,1]), 9, replace = FALSE)
  rd59 <- sample(1:length(df59[,1]), 9, replace = FALSE)
  rd64 <- sample(1:length(df64[,1]), 9, replace = FALSE)
  rd69 <- sample(1:length(df69[,1]), 8, replace = FALSE)
  rd74 <- sample(1:length(df74[,1]), 6, replace = FALSE)
  df_temp = rbind(df24,df29[rd29,],df34[rd34,],df39[rd39,],
                  df44[rd44,],df49[rd49,],df54[rd54,],df59[rd59,],
                  df64[rd64,],df69[rd69,],df74[rd74,])
  df_sample_comb = rbind(df_sample_comb,df_temp)
}

# remove index
df_sample = df_sample[,-1]

# remove density, effectively same as body fat
df_sample = df_sample[,-2]

model4 = lm(BODYFAT~ABDOMEN, data=df_sample)

# remove index
df_sample_comb = df_sample_comb[,-1]

# remove density, effectively same as body fat
df_sample_comb = df_sample_comb[,-2]

model5 = lm(BODYFAT~ABDOMEN, data=df_sample_comb)


## Weighted Linear Regression
body.fat = df_weight
sample.cnt <- body.fat |>
  group_by(AGE) |>
  summarise(count = n())

used.age <- ages[c(5:(length(ages)-3), length(ages)-1)]

j <- 1
i <- 1
sample.med <- c()
while (i <= dim(sample.cnt)[1]){
  if ((sample.cnt$AGE[i] <= used.age[j]+2) & (sample.cnt$AGE[i] >= used.age[j]-2)){
    sample.med[i] <- used.age[j]
    i <- i+1
  }else{j <- j+1}
}

df.use <- sample.cnt |>
  left_join(data.frame(AGE = sample.cnt$AGE, age.med = sample.med))
new.count <- df.use |>
  group_by(age.med) |>
  summarise(new.count = sum(count)) |>
  pull(new.count)

compare.age <- data.frame(age.med = seq(2,87,by=5), distrib2022 = age.pop.2022, distrib1970 = c(0,0,0,0,10,26,17,21,60,32,25,21,15,12,8,0,1,0))

diff.df <- compare.age |>
  summarise(
    age.med = age.med,
    prob22 = distrib2022/sum(distrib2022),
    prob70 = distrib1970/sum(distrib1970),
    diffs = prob22 / prob70
  )

weights <- diff.df |> pull(diffs)
### cuz we don't have some age intervals in our dataset, so we remove those intervals
used.weight <- weights[c(5:(length(ages)-3), length(ages)-1)]

weights.use <- c()

j <- 1
i <- 1
while(i <= dim(sample.cnt)[1]){
  if ((sample.cnt$AGE[i] <= used.age[j]+2) & (sample.cnt$AGE[i] >= used.age[j]-2)){
    wi <- rep(used.weight[j], sample.cnt$count[i])
    weights.use <-  append(weights.use, wi)
    i <- i+1
  }else{j <- j+1}
}

## model
l2.wt <- lm(BODYFAT ~ ABDOMEN, data = body.fat, weights = weights.use)

##### Plot Functions #####
# Model Result Plot Function
model_res_plot = function(model){
  xrange <- seq(60, 130, length.out = 100)
  if (model == "Weighted (Default)"){
    dff = body.fat
    model_used = l2.wt
    Title = "Weighted"
  }else if (model == "Bootstrap"){
    dff = df_sample
    model_used = model4
    Title = "Bootstrap"
  }else{
    dff = df_sample_comb
    model_used = model5
    Title = "Bootstrap"
  }
  
  predicts_ab <- predict(model_used, 
                         newdata = data.frame(ABDOMEN = xrange), 
                         interval = "prediction", 
                         se.fit = TRUE)
  
  predictions <- data.frame(
    ABDOMEN = xrange,
    BODYFAT = predicts_ab$fit[,1],
    lwr = predicts_ab$fit[,2],
    upr = predicts_ab$fit[,3]
  )

  mrp = ggplot(dff, aes(x = ABDOMEN, y = BODYFAT)) +
    geom_point(col = "royalblue") +
    geom_line(data = predictions, aes(y = BODYFAT), color = "red") +
    geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr), fill = "black", alpha = 0.2) +
    labs(
      x = "Abdomen Circumference",
      y = "Bodyfat Percentage",
      paste0(Title, " Regression Plot with Prediction Interval")
    )
  
  return(mrp)
}

# Residual Plot Function with
# "res vs fit" and "qqplot of res"
model_residual_plot = function(model){
  if (model == "Weighted (Default)"){
    model_used = l2.wt
  }else if (model == "Bootstrap"){
    model_used = model4
  }else{
    model_used = model5
  }
  df_res <- data.frame(
    Fitted = fitted(model_used),
    Residuals = resid(model_used)
  )
  # res vs fit
  mrsp <- ggplot(df_res, aes(x = Fitted, y = Residuals)) +
    geom_point(col = "royalblue") +
    geom_hline(yintercept = 0, color = "black") +
    labs(
      x = "Bodyfat Percentage",
      y = "Residuals"
    )
  # qqplot of res
  qq_plot <- ggplot(df_res, 
                    aes(sample = Residuals)) +
    stat_qq(col = "royalblue") +
    stat_qq_line()
  
    return(list(res_fit = ggplotly(mrsp),
              res_norm = (qq_plot)))
}

# Show R^2 of the selected model
text_R_sqr = function(model){
  if (model == "Weighted (Default)"){
    model_used = l2.wt
  }else if (model == "Bootstrap"){
    model_used = model4
  }else{
    model_used = model5
  }
  return(summary(model_used)$r.square)
}
