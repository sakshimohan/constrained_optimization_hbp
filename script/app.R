library(shiny)
library(ggplot2)
library(readxl)

# Load data
#bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)
setwd("C:/Users/sm2511/Dropbox/York/Research Projects/Malawi EHP/Analysis/")
#df <- read_excel("2 data/EHP Tool_19Jan22.xlsx", sheet = "final_intervention_list",col_names = TRUE,col_types=NULL,na="",skip=3)

#Create user interface
ui <- fluidPage(
  titlePanel("Constrained optimization for Malawi EHP"),
  
  sidebarLayout(
    sidebarPanel(
      # Data file
      fileInput(
        "excel_file","Upload data file",
        multiple = FALSE,
        accept = ".xlsx",
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      
      # Consumables budget
      numericInput(
        inputId = "drug_budget_input",
        label = "Consumables budget (2021 USD)",
        value = 226000000,
        min = 0, max = 999999999999999
      ),
      
      # Health workforce scale
      # //placeholder// 
      
      # Cost-effectiveness Threshold
      sliderInput(
        "cet", "Cost-effectiveness threshold",
        min = 0, max = 500,
        value = 65, pre = "$"
      ),
      
      # Task-shifting
      radioButtons("task_shifting_pharm","Allow task-shifting of phamaceutical and nutrition tasks to nurses",
                 choices = c("Yes", "No"), selected = "Yes"         
      ),
      
      # Constraints to include
      checkboxGroupInput("constraints", "Constraints to include",
        choices = c("Drug budget", "Health worker capacity", "Demand constraint"),
        selected = c("Drug budget", "Health worker capacity", "Demand constraint"),
        inline = FALSE,
        width = NULL,
        # //placeholder// add choiceNames and choiceValues here
      )
     
    
    ),
    
    mainPanel(
      # Summary - # of interventions, net DALYs averted, highest ICER
      tableOutput("result_sum"),
      
      br(), br(),
      
      # Resource use graph
      plotOutput("resourceuse_plot")
    )
    
  )
)

# Create server
server <- function(input, output){
  # Increase max file size to 25MB
  options(shiny.maxRequestSize=25*1024^2)

  
  ############
  # ANALYSIS #
  ############
  # Load dataset using observe
  df <- reactive({
    file = input$excel_file
    
    if (is.null(file))
      return(NULL)
    
    df <- read_excel(input$excel_file$datapath, sheet = "final_intervention_list",col_names = TRUE,col_types=NULL,na="",skip=2)
    df <- na.omit(df)  
  })
  #df <- reactive({
  #  na.omit(df) # drop rows containing missing values 
  #  
  #})
  
  # sHOW DATA IN A TABLE
  output$result_sum <- renderTable({
    newData <- df()
    head(newData,5)
  })
  
  # Show graph
  output$resourceuse_plot <- renderPlot({
    #ggplot(data = df, aes(x = ce_dalys, y = ce_cost))
    newdata <- df()
    plot(newdata$ce_dalys, newdata$ce_cost)
  })
  
  
}
shinyApp(ui = ui, server = server)