####################################################################

# Loading all required packages:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
# library(rsconnect) # To publish your Shiny app.R file.
library(shiny)     # The Shiny package for presentations.
library(bslib)     # Allows choosing different Shiny themes.

# Reading all crossing time estimation results:

url_tct = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_ct/tab_ct.rds?raw=true"
tct = try(readRDS(url(url_tct)), TRUE)
tct = as.data.frame(tct)

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# reg = c("PH", "PO", "YP")
# cor = c(0.25, 0.50, 0.75)
# gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
gbsl = c("EW", "W")
# fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("BP", "PE", "W")

# Loading the user interface to apply the summary tables:

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Creating a box for the combination choice:
  headerPanel("Estimated Crossing Times for Survival Copula Models"),
  fluidRow(
    column(width=3, offset=3, selectInput(
      inputId="margs",
      choices=c("M1", "M2"),
      label="Margin", selected="M1"),
    ),
    column(3, selectInput(
      inputId="gcops",
      choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
      label="Generated Copula", selected="AMH"),
    ),
    # column(3, selectInput(
    #   inputId="cor", choices=c(0.25, 0.5, 0.75),
    #   label="Correlation", selected = 0.25),
    # ),
    # column(3, selectInput(
    #   inputId="fcops",
    #   choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    #   label="Fitted Copula", selected="AMH"),
    # ),
    # column(3, selectInput(
    #   inputId="greg", choices=c("PH", "PO", "YP"),
    #   label="Generated Class", selected="PH"),
    # ),
    # column(3, selectInput(
    #   inputId="freg", choices=c("PH", "PO"),
    #   label="Fitted Class", selected="PH"),
    # ),
    
    # Inserting the boxplots for the chosen input:
    titlePanel("Relative Bias for Crossing Time Estimates"),
    fluidRow(column(width=2, # offset=1,
                    # checkboxGroupInput(
                    #   inputId="gcops", label="Generated Copula",
                    #   choices=gcop, selected=gcop, inline=T),
                    checkboxGroupInput(
                      inputId="gbsls", label="Generated Baseline",
                      choices=gbsl, selected="W", inline=T),
                    # checkboxGroupInput(
                    #   inputId="fcops", label="Fitted Copula",
                    #   choices=fcop, selected=fcop, inline=T),
                    checkboxGroupInput(
                      inputId="fbsls", label="Fitted Baseline",
                      choices=fbsl, selected=fbsl, inline=T),
                    # checkboxGroupInput(
                    #   inputId="coefs", label="Model Parameters",
                    #   selected=coef, inline=T,
                    #   choiceNames=coef_uc, choiceValues=coef),
    ),
    
    column(width=8, mainPanel(plotOutput("ctPlots"))),
    
    # Summary tables for the chosen input:
    titlePanel("Monte Carlo Results"),
    column(width=8, offset=2,
           mainPanel(dataTableOutput("ctResults"))))
  )
)

# Loading the corresponding server:

server = function(input, output, session){
  # Reading online data:
  url_ct = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_ct/res_ct.rds?raw=true"
  
  bigdata = reactive({
    try(readRDS(url(url_ct)), TRUE)
  })
  
  # Choosing summary table data from a given input:
  sumdata = reactive({
    tct
  })
  
  # First, open the output environments!
  
  output$ctPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = bigdata()
    
    # Filtering our data according to selected options:
    df = df %>% filter(
      Margin %in% input$margs, Copula %in% input$gcops,
      G.Baseline %in% input$gbsls, F.Baseline %in% input$fbsls)
    
    # Plotting the boxplots for the crossing time estimates:
    ggplot(df) + geom_boxplot(aes(x=Copula, y=RB)) +
      xlab("Copula") + ylab("Relative Bias") +
      facet_wrap(~ G.Baseline + F.Baseline) +
      geom_hline(yintercept=0, linetype="dashed",
                 color="black") +
      theme(legend.position="bottom")
    },
    width = 900, height = 400
  )
  
  output$ctResults = renderDataTable({
    # Fixing on another object to allow updates:
    dft = sumdata() %>% filter(
      Margin %in% input$margs,
      Copula %in% input$gcops, # TrueTau %in% input$cor,
      # G.Reg.Class %in% input$greg, F.Reg.Class %in% input$freg,
      G.Baseline %in% input$gbsls, F.Baseline %in% input$fbsls)
    dft = dft[,-c(2,3,4,5,9)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  # options=list(pageLength=3, lengthMenu=c(3, 6, 30)))
  options=list(pageLength=3, lengthMenu=c(3, 6)))
}

# Running all data set at a single application:

shinyApp(ui=ui, server=server)