####################################################################

# Loading all required packages:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
# library(rsconnect) # To publish your Shiny app.R file.
library(shiny)     # The Shiny package for presentations.
library(bslib)     # Allows choosing different Shiny themes.

# Reading partitioned tables with LR test results by each regression
# model class used for generation:

# Online:
url = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_lrt.rds?raw=true"
tlrt = try(readRDS(url(url)), TRUE)
# Locally:
# tlrt = readRDS(file="tab_lrt.rds")

# Since it is a grouped data frame, we need to "ungroup" it first in
# order to use it:
class(tlrt)
tlrt = ungroup(tlrt)
class(tlrt)

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# reg = c("PH", "PO", "YP")
# cor = c(0.25, 0.50, 0.75)
# gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
gbsl = c("EW", "W")
# fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("W", "BP", "PE")

freg = c("PH", "PO") # Only for LR results.

# Loading the user interface to apply the summary tables:

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Creating a box for the combination choice:
  headerPanel("LR Tests for Nested Survival Copula Models"),
  fluidRow(
    column(3, offset=2, selectInput(
      inputId="gcops",
      choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
      label="Generated Copula", selected="AMH"),
    ),
    column(3, selectInput(
      inputId="cor", choices=c(0.25, 0.5, 0.75),
      label="Correlation", selected = 0.25),
    ),
    # column(3, selectInput(
    #   inputId="fcops",
    #   choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    #   label="Fitted Copula", selected="AMH"),
    # ),
    column(3, selectInput(
      inputId="greg", choices=c("PH", "PO", "YP"),
      label="Generated Class", selected="PH"),
    ),
    # column(3, selectInput(
    #   inputId="freg", choices=c("PH", "PO"),
    #   label="Fitted Class, Nested to YP", selected="PH"),
    # ),
    
    # Summary tables for the chosen input:
    # titlePanel("LR Tests for Fitted Survival Copula Models"),
    fluidRow(column(width=2, offset=1,
                    # checkboxGroupInput(
                    #   inputId="gbsls", label="Generated Baseline",
                    #   choices=gbsl, selected="W", inline=T),
                    checkboxGroupInput(
                      inputId="fbsls", label="Fitted Baseline",
                      choices=fbsl, selected=fbsl, inline=T),
                    checkboxGroupInput(
                      inputId="freg",
                      label="Fitted Class, Nested to YP",
                      choices=freg, selected=freg, inline=T),
    ),
    column(width=8, # offset=1,
           mainPanel(dataTableOutput("lrResults"))))
  )
)

# Loading the corresponding server:

server = function(input, output, session){
  # Choosing summary table data from a given input:
  sumdata = reactive({
    tlrt
  })
  
  # First, open the output environments!
  
  output$lrResults = renderDataTable({
    # Fixing on another object to allow updates:
    dft = sumdata() %>% filter(
      TrueTau %in% input$cor, Copula %in% input$gcops,
      G.Reg.Class %in% input$greg, F.Reg.Class %in% input$freg,
      # G.Baseline %in% input$gbsls,
      F.Baseline %in% input$fbsls)
    dft = dft[,-1] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  # options=list(pageLength=3, lengthMenu=c(3, 6, 30)))
  options=list(pageLength=6, lengthMenu=c(2,6)))
}

# Running all data set at a single application:

shinyApp(ui=ui, server=server)