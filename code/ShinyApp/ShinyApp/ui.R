#### Data Cleaning Tab ####
data_cleaning = 
  tabPanel(
    "Data Cleaning",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          "dc_check",
          "In View of",
          choices = c("Bodyfat vs Density",
                      "Weight_Height vs BMI",
                      "Outliers"),
          multiple = F,
          selected = "Bodyfat vs Density"
        ),
        actionButton(
          "dc_fix",
          "Fix"
        )
      ),
      mainPanel(
        plotlyOutput("data_clean_plot", height = "300px")
      )
    ),
    h3("Suspicious Samples"),
    dataTableOutput("data_clean_dt")
  )

#### Model Tab ####
model = 
  tabPanel(
    "Model",
    sidebarLayout(
      sidebarPanel(
        pickerInput(
          "model",
          "Select Model",
          choices = c("Weighted (Default)",
                      "Bootstrap",
                      "Bootstrap_Combined"),
          multiple = F,
          selected = "Weighted (Default)"
        ),
        textOutput("Rsqr")
      ),
      mainPanel(
        h3("Age Distribution"),
        plotOutput("age_distr", height = "200px")
      )
    ),
    h3("Model Result"),
    plotlyOutput("model_result"),
    column(6,
           h3("Residual Normality"),
           plotlyOutput("res_norm")),
    column(6,
           h3("Residual Plot"),
           plotlyOutput("res_plot"))

  )

#### Calculator Tab ####
calculator = 
  tabPanel(
    "Calculator",
    sidebarLayout(
      sidebarPanel(
        h3("Instruction"),
        helpText("Please select a model and enter your Abdomen circumference in the below box."),
        helpText("Note that the number entered should between 65 and 150, which is the possible range."), 
        helpText("After entering, click the 'Calculate Bodyfat' button to get predicted results."),
        helpText("If the input is not correct, there will be a warning pop-ups. To close it, hit enter or 'OK'."),
        helpText("In the result table, the 'Lower_Boundary' and 'Upper_Boundary' show the prediction interval. In the plot, the red point represents the you."),
        helpText("Moreover, the dark red line in the plot is the regression line, and the grey area is the prediction interval."),
        
        pickerInput(
          "cal_model",
          "Select Model",
          choices = c("Weighted (Default)",
                      "Bootstrap",
                      "Bootstrap_Combined"),
          multiple = F,
          selected = "Weighted"
        ),
        textInput("cal_input", 
                  "Enter your Abdomen",
                  value = NULL
                  ),
        actionButton(
          "cal_do",
          "Calculate Bodyfat"
        )
      ),
      mainPanel(
        h3("Predicted Results"),
        dataTableOutput("cal_dt"),
        h3("Find Yourself!"),
        plotlyOutput("cal_plot")
      )
    )
  )

#### Combine ####
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "STAT628 Module2 Group3",
    calculator,
    data_cleaning,
    model
  )
)

