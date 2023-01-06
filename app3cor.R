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
url_tr = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/tab_cor.rds?raw=true"
tr = try(readRDS(url(url_tr)), TRUE)
# Locally:
# tr = readRDS("res_exc_cop/tab_cor.rds")

tr = tr[-c(1351:1356),]
colnames(tr)[11] = "ARB"

# Recoding all parameter names, to their corresponding "unicode", in
# the summary table:

tr$Parameter = case_when(tr$Parameter=="tau" ~ "\u03C4")

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# reg = c("PH", "PO", "YP")
# cor = c(0.25, 0.50, 0.75)
# gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
gbsl = c("EW", "W")
fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("BP", "PE", "W")

# Loading the user interface to apply boxplots and tables:

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Creating a box for the combination choice:
  headerPanel("RB and MC Results for Survival Copula Models"),
  fluidRow(
    column(3, offset=2, selectInput(
      inputId="reg", choices=c("PH", "PO", "YP"),
      label="Generated/Fitted Regression Class", selected="PH"),
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
    fluidRow(column(width=2,
                    # checkboxGroupInput(
                    #   inputId="gcops", label="Generated Copula",
                    #   choices=gcop, selected=gcop, inline=T),
                    checkboxGroupInput(
                      inputId="gbsls", label="Generated Baseline",
                      choices=gbsl, selected="W", inline=T),
                    checkboxGroupInput(
                      inputId="fcops", label="Fitted Copula",
                      choices=fcop, selected=fcop, inline=T),
                    checkboxGroupInput(
                      inputId="fbsls", label="Fitted Baseline",
                      choices=fbsl, selected=fbsl, inline=T),
                    # checkboxGroupInput(
                    #   inputId="coefs", label="Model Parameters",
                    #   selected=coef, inline=T,
                    #   choiceNames=coef_uc, choiceValues=coef),
    ),
    column(width=8, mainPanel(plotOutput("rbPlots"))),
    
    # Summary tables for the chosen input:
    titlePanel("Monte Carlo Results"),
    fluidRow(column(width=8, offset=3,
                    mainPanel(dataTableOutput("tabResults"))))
    )
  )
)

# Loading the corresponding server:

server = function(input, output, session){
  # Reading online data (make the project public later!):
  url_PH_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_ph025.rds?raw=true"
  url_PO_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_po025.rds?raw=true"
  url_YP_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_yp025.rds?raw=true"
  url_PH_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_ph050.rds?raw=true"
  url_PO_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_po050.rds?raw=true"
  url_YP_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_yp050.rds?raw=true"
  url_PH_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_ph075.rds?raw=true"
  url_PO_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_po075.rds?raw=true"
  url_YP_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_cor_yp075.rds?raw=true"
  
  # Choosing results' data from a given input:
  bigdata = reactive({
    if(input$reg=="PH"){
      switch(input$cor,
        # If reading online:
        `0.25` = try(readRDS(url(url_PH_25)), TRUE),
        `0.5` = try(readRDS(url(url_PH_50)), TRUE),
        `0.75` = try(readRDS(url(url_PH_75)), TRUE)
      )
    }
    else if(input$reg=="PO"){
      switch(input$cor,
        # If reading online:
        `0.25` = try(readRDS(url(url_PO_25)), TRUE),
        `0.5` = try(readRDS(url(url_PO_50)), TRUE),
        `0.75` = try(readRDS(url(url_PO_75)), TRUE)
      )
    }
    else{
      switch(input$cor,
        # If reading online:
        `0.25` = try(readRDS(url(url_YP_25)), TRUE),
        `0.5` = try(readRDS(url(url_YP_50)), TRUE),
        `0.75` = try(readRDS(url(url_YP_75)), TRUE)
      )
    }
  })
  
  # Choosing summary table data from a given input:
  sumdata = reactive({
    if(input$reg=="PH"){
      switch(input$cor,
        `0.25` = tr %>% filter(Reg.Class=="PH" & TrueTau==0.25),
        `0.5` = tr %>% filter(Reg.Class=="PH" & TrueTau==0.5),
        `0.75` = tr %>% filter(Reg.Class=="PH" & TrueTau==0.75)
      )
    }
    else if(input$reg=="PO"){
      switch(input$cor,
        `0.25` = tr %>% filter(Reg.Class=="PO" & TrueTau==0.25),
        `0.5` = tr %>% filter(Reg.Class=="PO" & TrueTau==0.5),
        `0.75` = tr %>% filter(Reg.Class=="PO" & TrueTau==0.75)
      )
    }
    else{
      switch(input$cor,
        `0.25` = tr %>% filter(Reg.Class=="YP" & TrueTau==0.25),
        `0.5` = tr %>% filter(Reg.Class=="YP" & TrueTau==0.5),
        `0.75` = tr %>% filter(Reg.Class=="YP" & TrueTau==0.75)
      )
    }
  })
  
  # First, open the output environments!
  
  output$rbPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = bigdata()
    
    # Saving separately those new names:
    coef = c("tau")
      
    # Filtering our data according to selected options:
    df = df %>% filter(
      G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
      # Parameter %in% input$coefs)
      Parameter %in% coef)
      
    # Plotting the boxplots for the relative bias:
    ggplot(df, aes(x=F.Copula, y=RB)) + geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color="black") +
      facet_wrap(~ G.Baseline + F.Baseline) +
      labs(x="Fitted Copula", y="Relative Bias (%)") +
      # scale_x_discrete(labels = c(expression(paste(tau)))) +
      theme(legend.position="bottom")
  },
  # width = 1000, height = 450)
  width = 900, height = 400)
  
  output$tabResults = renderDataTable({
    # Saving separatedely those new names:
    coef_uc = c("\u03C4")
    # Fixing on another object to allow updates:
    dft = sumdata() %>% filter(
      G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
      F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
      Parameter %in% coef_uc)
    dft = dft[,-c(1,2,3,8,12,13,14)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  # options=list(pageLength=3, lengthMenu=c(3, 6, 30)))
  options=list(pageLength=15, lengthMenu=c(15, 30)))
}

# Running all 9 data sets at a single application:

shinyApp(ui=ui, server=server)