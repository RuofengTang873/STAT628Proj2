#### Data Cleaning ####
df = read.csv('https://raw.githubusercontent.com/WXYS1209/STAT628/main/BodyFat.csv')
##### Density vs Bodyfat #####
df_1 = df %>% 
  mutate(diff1 = abs(495/DENSITY - 450 - BODYFAT))

# body fat 0, try recover with density, which is
# negative value -3.6, cannot use, should delete.
df = df[-which(df$IDNO==182),] 
dff = df %>% 
  mutate(diff1 = abs(495/DENSITY - 450 - BODYFAT))
##### Weight_Height vs BMI #####
df_2 = df %>% 
  mutate(diff2 = abs(703 * WEIGHT / (HEIGHT^2) - ADIPOSITY))

# Observe that the height 29.5 is abnormal.
# Try recover it with weight and adiposity.
df[which(df$IDNO==42),'HEIGHT'] = 
  sqrt(df[which(df$IDNO==42),'WEIGHT'] * 703 / df[which(df$IDNO==42),'ADIPOSITY'])
df_3 = df %>% 
  mutate(diff2 = abs(703 * WEIGHT / (HEIGHT^2) - ADIPOSITY))

##### Outliers #####
df_4 = df

# Up and down quarter quantiles
quartiles = df_4 %>%
  pivot_longer(-IDNO, names_to = "Var") %>% 
  group_by(Var) %>%
  summarise(Up = quantile(value, 0.75), Down = quantile(value, 0.25))
quartiles$Var_num <- as.numeric(as.factor(quartiles$Var))
df_4_long = df_4 %>% 
  filter(IDNO %in% c(39, 41, 172, 216)) %>% 
  pivot_longer(-IDNO, names_to = "Var")

# Delete wrong bodyfat (216) and super high bodyfat (39)
# and abnormal bodyfat (172)
df = df[-which(df$IDNO %in% c(39, 172, 216)),]

##### Plot Functions #####
# Data Cleaning: Plot
dc_plot = function(pov){
  if (pov == "Bodyfat vs Density"){
    dcp = df_1 %>% 
      ggplot(aes(x=IDNO, y=diff1)) + 
      geom_point(color = "royalblue")
  }else if (pov == "Weight_Height vs BMI"){
    dcp = df_2 %>% 
      ggplot(aes(x=IDNO, y=diff2)) + 
      geom_point(color = "royalblue")
  }else{
    dcp = ggplot(df_4_long, aes(x = Var, y = value, 
                                group = IDNO, color = IDNO))+
      geom_line() + 
      geom_ribbon(data = quartiles, aes(x = Var_num, ymin = Down, ymax = Up), 
                  fill = "red", alpha = 0.5, inherit.aes = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(dcp)
}

# Data Cleaning: Data Table
dc_dt = function(pov){
  if (pov == "Bodyfat vs Density"){
    dcDT = df_1 %>% 
      filter(diff1 %in% boxplot.stats(df_1$diff1)$out) %>% 
      select(IDNO, diff1, everything()) %>% 
      arrange(-diff1)
  }else if (pov == "Weight_Height vs BMI"){
    dcDT = df_2 %>% 
      filter(diff2 %in% boxplot.stats(df_2$diff2)$out) %>% 
      select(IDNO, diff2, HEIGHT, WEIGHT,
             ADIPOSITY, everything()) %>% 
      arrange(-diff2)
  }else{
    dcDT = df_4 %>% 
      filter(IDNO %in% c(39, 41, 172, 216))
  }
  datatable(
    dcDT,
    rownames = F,
    options = list(scrollY = 200,
                   scrollX = 500,
                   deferRender = TRUE
    )
  )
}




