#### Calculator ####
## Calculate Predicted Bodyfat in the format of Data Table
cal_calculate = function(abd, model){
  if (model == "Weighted (Default)"){
    model_used = l2.wt
  }else if (model == "Bootstrap"){
    model_used = model4
  }else{
    model_used = model5
  }
  predicts_ab <- predict(model_used, 
                         newdata = data.frame(ABDOMEN = abd), 
                         interval = "prediction", se.fit = TRUE)
  
  predictions <- data.frame(
    Abdomen = abd,
    Predicted_Bodyfat = paste0(round(predicts_ab$fit[,1],2), "%"),
    Lower_Boundary = paste0(round(predicts_ab$fit[,2],2), "%"),
    Upper_Boundary = paste0(round(predicts_ab$fit[,3],2), "%")
  )
  return(predictions)
}

## Visualize Model Result with Marked new point
cal_plot = function(abd, model){
  xrange <- seq(60, 155, length.out = 100)
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
  
  # Add Type to distinguish which point is new
  dff_used = dff %>%
    select(ABDOMEN, BODYFAT) %>%
    mutate(Type = "origin")
  pred_bodyfat = 
    predict(model_used, newdata = data.frame(ABDOMEN = abd))
  dff_used[dim(dff_used)[1],-3] = 
    c(abd, pred_bodyfat)
  dff_used[dim(dff_used)[1],3] = "new"
  dff_used = dff_used %>% 
    mutate(Alpha = if_else(Type == "origin", 0.5, 1),
           Size = if_else(Type == "origin", 1, 1.5))
  
  # How many people exceeded by the predicted bodyfat
  where = sum(pred_bodyfat > dff$BODYFAT) / dim(dff)[1] * 100
  
  # Create Plot
  mrp = ggplot(dff_used, aes(x = ABDOMEN, y = BODYFAT)) +
    geom_point(aes(col = Type,
                   alpha = Alpha,
                   size = Size,
                   text = paste("Abdomen", ABDOMEN,
                                "<br>Bodyfat:", BODYFAT, 
                                "<br>Type:", Type))) +
    geom_line(data = predictions, aes(y = BODYFAT), color = "darkred") +
    geom_ribbon(data = predictions, aes(ymin = lwr, ymax = upr), fill = "black", alpha = 0.2) +
    annotate("text", 
             x = 110, y = 50, 
             label = paste0("Exceeds ",
                            round(where,2), "% people approximately."), 
             size = 4, color = "black") +
    labs(
      x = "Abdomen Circumference",
      y = "Bodyfat Percentage",
      title = paste0(Title, " Regression Plot with Prediction Interval")
    ) +
    scale_size_continuous(range = c(2, 3)) + 
    scale_alpha_continuous(range = c(0.5, 1)) + 
    scale_color_manual(values = c("red", "royalblue"))
  
  plotly_mrp = ggplotly(mrp, tooltip = "text")
  
  # Change the title of the legend
  plotly_mrp$x$layout$legend$title$text = "Type"
  return(plotly_mrp)
}