server =
  function(session, input, output){
    #### Data Cleaning ####
    # After Selection
    observeEvent(input$dc_check, {
      ## Plot
      output$data_clean_plot = renderPlotly({
        ggplotly(dc_plot(input$dc_check))
      })
      ## Data Table
      output$data_clean_dt = renderDataTable(
        dc_dt(input$dc_check)
      )
    })

    # After Clicking Button
    observeEvent(input$dc_fix, {
      if (input$dc_check == "Weight_Height vs BMI"){
        ## Update Plot
        output$data_clean_plot = renderPlotly({
          dcp = df_3 %>% 
            ggplot(aes(x=IDNO, y=diff2)) + 
            geom_point(color = "royalblue")
        })
        ## Update Data Table
        output$data_clean_dt = renderDataTable(
          datatable(
            df_3 %>% 
              filter(diff2 %in% boxplot.stats(df_3$diff2)$out) %>% 
              select(IDNO, diff2, HEIGHT, WEIGHT,
                     ADIPOSITY, everything()) %>% 
              arrange(-diff2),
            rownames = F,
            options = list(scrollY = 200,
                           scrollX = 500,
                           deferRender = TRUE
            )
          )
        )
      }else if (input$dc_check == "Outliers"){
        ## Update Plot
        output$data_clean_plot = renderPlotly({
          dcp = ggplot(df_4_long %>% filter(!IDNO %in% c(39, 172, 216)), 
                       aes(x = Var, y = value, 
                           group = IDNO, color = IDNO))+
            geom_line() + 
            geom_ribbon(data = quartiles, aes(x = Var_num, ymin = Down, ymax = Up), 
                        fill = "red", alpha = 0.5, inherit.aes = FALSE) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        })
        ## Update Data Table
        output$data_clean_dt = renderDataTable(
          datatable(
            df_4 %>% 
              filter(IDNO %in% c(41)),
            rownames = F,
            options = list(scrollY = 200,
                           scrollX = 500,
                           deferRender = TRUE
            )
          )
        )
      }else{
        ## Update Plot
        output$data_clean_plot = renderPlotly({
          dff %>% 
            ggplot(aes(x=IDNO, y=diff1)) + 
            geom_point(color = "royalblue")
        })
        
        ## Update Data Table
        output$data_clean_dt = renderDataTable(
          datatable(
            dff %>% 
              filter(diff1 %in% boxplot.stats(dff$diff1)$out) %>% 
              select(IDNO, diff1, everything()) %>% 
              arrange(-diff1)
          )
        )
      }
      
    })
    
    #### Model ####
    ## Age Distribution
    output$age_distr = renderPlot({
      adp = ggplot(data.frame(age = ages, pop = age.pop.2022), aes(x = age, y = pop)) +
        geom_text_repel(aes(label = pop), nudge_y = 0.9, segment.size = 0.2) +
        geom_col(fill = "royalblue", alpha = 0.7) +
        labs(x = "Age", y = "Population (millions)")
      adp
    })
    
    ## Model Result 
    output$model_result = renderPlotly({
      model_res_plot(input$model)
    })
    
    ## Residual Plot
    output$res_plot = renderPlotly({
      model_residual_plot(input$model)$res_fit
    })
    
    ## Residual Norm
    output$res_norm = renderPlotly({
      model_residual_plot(input$model)$res_norm
    })
    ## R Square Text
    output$Rsqr = renderText(
      paste0("The R^2 of the model is ", text_R_sqr(input$model), ".")
    )
    
    #### Calculator ####
    # Show empty Result Table
    output$cal_dt <- renderDataTable(
      datatable(
        data.frame(
          Abdomen = NA,
          Predicted_Bodyfat = NA,
          Lower_Boundary = NA,
          Upper_Boundary = NA
        )
      )
    )
    # Show empty Plot
    output$cal_plot = renderPlotly({
      ggplot(df, aes(x = c(0,1), y = c(0,1))) + 
        annotate("text",x=0.5,y=0.5,label="Please enter Abdomen value in cm.") + 
        labs(x = "", y = "") + 
        theme(plot.caption = element_text(size = 10))
    })
    # After clicking the button
    observeEvent(input$cal_do, {
      user_input <- as.double(input$cal_input)
      model_used = input$cal_model
      # If the input is valid
      if (!is.na(user_input) & 
          user_input <= 150 & 
          user_input >= 65) {
        # Update Result Table
        output$cal_dt <- renderDataTable({
          datatable(cal_calculate(user_input, model_used))
        })
        # Update Plot
        output$cal_plot = renderPlotly({
          cal_plot(user_input, model_used)
        })
      }else{
        # Show alert
        shinyalert("Warning", 
                   paste0("Please enter the right Abdomen value, which should be a number and range from 65 to 150."), 
                   type = "info")
        # Clean the Plot
        output$cal_plot = renderPlotly({
          ggplot(df, aes(x = c(0,1), y = c(0,1))) + 
            annotate("text",x=0.5,y=0.5,label="Please enter correct Abdomen value.") + 
            labs(x = "", y = "") + 
            theme(plot.caption = element_text(size = 10))
        })
      }
    })
  }