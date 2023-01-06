####################################################################

# Loading all required packages:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
# library(rsconnect) # To publish your Shiny app.R file.
library(shiny)     # The Shiny package for presentations.
library(bslib)     # Allows choosing different Shiny themes.

# Reading the summary tables:

# Online:
url_tr = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_cri.rds?raw=true"
tr = try(readRDS(url(url_tr)), TRUE)
# Locally:
# tr = readRDS("tab_cri.rds")

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# reg = c("PH", "PO", "YP")
# cor = c(0.25, 0.50, 0.75)
# gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
# gbsl = c("EW", "W")
fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("W", "BP", "PE")

# Loading the user interface to apply boxplots and tables:

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Creating a box for the combination choice:
  headerPanel("AIC Results for Survival Copula Models"),
  fluidRow(
    column(3, offset=2, selectInput(
      inputId="reg", choices=c("PH", "PO", "YP"),
      # label="Generated/Fitted Regression Class",
      label="Regression Class",
      selected="PH"),
    ),
    column(3, selectInput(
      inputId="cor", choices=c(0.25, 0.5, 0.75),
      label="Correlation", selected = 0.25),
    ),
    column(3, selectInput(
      inputId="gcops",
      choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
      label="Generated Copula", selected="AMH"),
    ),
    # column(3, selectInput(
    #   inputId="fcops",
    #   choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    #   label="Fitted Copula", selected="AMH"),
    # ),
    
    # Inserting the boxplots for the chosen input:
    titlePanel("AIC Values for Fitted Survival Copula Models"),
    fluidRow(column(width=2, offset=1,
                    # checkboxGroupInput(
                    #   inputId="gbsls", label="Generated Baseline",
                    #   choices=gbsl, selected="W", inline=T),
                    checkboxGroupInput(
                      inputId="fcops", label="Fitted Copula",
                      choices=fcop, selected=fcop, inline=T),
                    checkboxGroupInput(
                      inputId="fbsls", label="Fitted Baseline",
                      choices=fbsl, selected=fbsl, inline=T),
    ),
    column(width=8, mainPanel(plotOutput("aicPlots"))),
    
    # Summary tables for the chosen input:
    titlePanel("Monte Carlo Results"),
    fluidRow(column(width=8, offset=3,
                    mainPanel(dataTableOutput("aicResults"))))
    )
  )
)

# Loading the corresponding server:

server = function(input, output, session){
  # Reading online data (make the project public later!):
  url = "https://github.com/wrmfstat/simsurvcop/blob/main/res_cri.rds?raw=true"
  
  # Choosing results' data from a given input:
  bigdata = reactive({
    try(readRDS(url(url)), TRUE)
  })
  
  # Choosing summary table data from a given input:
  sumdata = reactive({
    tr
  })
  
  # First, open the output environments!
  
  output$aicPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = bigdata()
    
    # Filtering our data according to selected options:
    df = df %>% filter(
      Reg.Class %in% input$reg, TrueTau %in% input$cor,
      G.Copula %in% input$gcops, # G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops, F.Baseline %in% input$fbsls)
    
    # Plotting the boxplots for the relative bias:
    ggplot(df, aes(x=F.Copula, y=AIC)) + geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color="black") +
      # facet_wrap(~ G.Baseline + F.Baseline) +
      facet_wrap(~F.Baseline) +
      labs(x="Fitted Copula", y="AIC") +
      theme(legend.position="bottom")
  },
  width = 900, height = 400)
  
  output$aicResults = renderDataTable({
    # Fixing on another object to allow updates:
    dft = sumdata() %>% filter(
      Reg.Class %in% input$reg, TrueTau %in% input$cor,
      G.Copula %in% input$gcops, # G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops, F.Baseline %in% input$fbsls)
    dft = dft[,-c(1,5,7)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  options=list(pageLength=15, lengthMenu=c(3, 15))
  )
}

# Running all data set at a single application:

shinyApp(ui=ui, server=server)