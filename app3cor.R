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
url_tr = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_tau.rds?raw=true"
tr = try(readRDS(url(url_tr)), TRUE)
# Locally:
# tr = readRDS("tab_tau.rds")

colnames(tr)[8] = "ARB"

# Recoding all parameter names, to their corresponding "unicode", in
# the summary table:

tr$Parameter = case_when(tr$Parameter=="tau" ~ "\u03C4")

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
  headerPanel("RB and MC Results for Survival Copula Models"),
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
    titlePanel(
      "Relative Bias for Correlation Parameter Estimates"),
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
    column(width=8, mainPanel(plotOutput("rbPlots"))),
    
    # Summary tables for the chosen input:
    titlePanel("Monte Carlo Results"),
    fluidRow(column(width=7, offset=3,
                    mainPanel(dataTableOutput("tabResults"))))
    )
  )
)

# Loading the corresponding server:

server = function(input, output, session){
  # Reading online data:
  url = "https://github.com/wrmfstat/simsurvcop/blob/main/res_tau.rds?raw=true"
  
  # Choosing results' data from a given input:
  bigdata = reactive({
    try(readRDS(url(url)), TRUE)
  })
  
  # Choosing summary table data from a given input:
  sumdata = reactive({
    tr
  })
  
  # First, open the output environments!
  
  output$rbPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = bigdata()
    
    # Saving separately those new names:
    coef = c("tau")
      
    # Filtering our data according to selected options:
    df = df %>% filter(
      TrueTau %in% input$cor,
      G.Copula %in% input$gcops, # G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
      Reg.Class %in% input$reg, Parameter %in% coef)
      
    # Plotting the boxplots for the relative bias:
    ggplot(df, aes(x=F.Copula, y=RB)) + geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color="black") +
      # facet_wrap(~ G.Baseline + F.Baseline) +
      facet_wrap(~F.Baseline) +
      labs(x="Fitted Copula", y="Relative Bias (%)") +
      # scale_x_discrete(labels = c(expression(paste(tau)))) +
      theme(legend.position="bottom")
  },
  width = 900, height = 400)
  
  output$tabResults = renderDataTable({
    # Saving separatedely those new names:
    coef_uc = c("\u03C4")
    # Fixing on another object to allow updates:
    dft = sumdata() %>% filter(
      TrueTau %in% input$cor,
      G.Copula %in% input$gcops, # G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
      Reg.Class %in% input$reg, Parameter %in% coef_uc)
    dft = dft[,-c(1,5)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  options=list(pageLength=15, lengthMenu=c(3, 15)))
}

# Running all data set at a single application:

shinyApp(ui=ui, server=server)