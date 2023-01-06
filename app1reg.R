#########################################################################

# Loading all required packages:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
# library(rsconnect) # To publish your Shiny app.R file.
library(shiny)     # The Shiny package for presentations.
library(bslib)     # Allows choosing different Shiny themes.

# Reading the summary tables:

# Online:
url_tr = "https://github.com/wrmfstat/simsurvcop/blob/master/tab_reg.rds?raw=true"
tr = try(readRDS(url(url_tr)), TRUE)
# Locally:
# tr = readRDS("res_exc_cop/tab_reg.rds")

tr = tr[-c(7201:7203),]

# Recoding all parameter names, to their corresponding "unicode", in
# the summary table:

tr$Parameter = case_when(
  tr$Parameter=="psi[1,1]" &
    tr$Reg.Class!="YP"~"\u03B2\U2081\U2081",
  tr$Parameter=="psi[1,2]" &
    tr$Reg.Class!="YP"~"\u03B2\U2081\U2082",
  tr$Parameter=="psi[2,1]" &
    tr$Reg.Class!="YP"~"\u03B2\U2082\U2081",
  tr$Parameter=="psi[2,2]" &
    tr$Reg.Class!="YP"~"\u03B2\U2082\U2082",
  tr$Parameter=="psi[1,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2081\U2081",
  tr$Parameter=="psi[1,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2081\U2082",
  tr$Parameter=="psi[2,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2082\U2081",
  tr$Parameter=="psi[2,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(S)\U2082\U2082",
  tr$Parameter=="phi[1,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2081\U2081",
  tr$Parameter=="phi[1,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2081\U2082",
  tr$Parameter=="phi[2,1]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2082\U2081",
  tr$Parameter=="phi[2,2]" &
    tr$Reg.Class=="YP"~"\u03B2^(L)\U2082\U2082")

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# tau = c(0.25, 0.50, 0.75)
gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
gbsl = c("EW", "W")
fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("BP", "PE", "W")
# reg = c("PH", "PO", "YP")

# Loading the user interface to apply boxplots and tables:

ui = fluidPage(
  # Defining the theme:
  theme = bs_theme(bootswatch="darkly"),
  
  # Creating a box for the combination choice:
  headerPanel("RB and MC Results for Survival Copula Models"),
  fluidRow(
    column(3, selectInput(
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
    column(3, selectInput(
      inputId="fcops",
      choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
      label="Fitted Copula", selected="AMH"),
    ),
    
    # Inserting the boxplots for the chosen input:
    titlePanel("Relative Bias for Regression Parameter Estimates"),
    fluidRow(column(width=2,
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
    column(width=8, mainPanel(plotOutput("rbPlots"))),

    # Summary tables for the chosen input:
    titlePanel("Monte Carlo Results"),
    fluidRow(column(width=8, offset=1,
                    mainPanel(dataTableOutput("tabResults"))))
    )
  )
)

# Loading the corresponding server:

server = function(input, output, session){
  # Reading online data (make the project public later!):
  url_PH_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_ph025.rds?raw=true"
  url_PO_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_po025.rds?raw=true"
  url_YP_25 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_yp025.rds?raw=true"
  url_PH_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_ph050.rds?raw=true"
  url_PO_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_po050.rds?raw=true"
  url_YP_50 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_yp050.rds?raw=true"
  url_PH_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_ph075.rds?raw=true"
  url_PO_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_po075.rds?raw=true"
  url_YP_75 = "https://github.com/wrmfstat/shinyCopRegEst/blob/master/res_exc_cop/res_reg_yp075.rds?raw=true"
  
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
    
    # Recoding all parameter names:
    df$Parameter = case_when(
      df$Parameter=="psi[1,1]" & df$Reg.Class!="YP"~"beta[1,1]",
      df$Parameter=="psi[1,2]" & df$Reg.Class!="YP"~"beta[1,2]",
      df$Parameter=="psi[2,1]" & df$Reg.Class!="YP"~"beta[2,1]",
      df$Parameter=="psi[2,2]" & df$Reg.Class!="YP"~"beta[2,2]",
      df$Parameter=="psi[1,1]" & df$Reg.Class=="YP"~"beta^(S)[1,1]",
      df$Parameter=="psi[1,2]" & df$Reg.Class=="YP"~"beta^(S)[1,2]",
      df$Parameter=="psi[2,1]" & df$Reg.Class=="YP"~"beta^(S)[2,1]",
      df$Parameter=="psi[2,2]" & df$Reg.Class=="YP"~"beta^(S)[2,2]",
      df$Parameter=="phi[1,1]" & df$Reg.Class=="YP"~"beta^(L)[1,1]",
      df$Parameter=="phi[1,2]" & df$Reg.Class=="YP"~"beta^(L)[1,2]",
      df$Parameter=="phi[2,1]" & df$Reg.Class=="YP"~"beta^(L)[2,1]",
      df$Parameter=="phi[2,2]" & df$Reg.Class=="YP"~"beta^(L)[2,2]")
    
    # If we want PH or PO models:
    # if(sum(which(df$Reg.Class=="YP"))==0){
    # if(input$comb=="PH x 0.25" | input$comb=="PO x 0.25" |
    #    input$comb=="PH x 0.5" | input$comb=="PO x 0.5" |
    #    input$comb=="PH x 0.75" | input$comb=="PO x 0.75"){
    if(input$reg=="PH" | input$reg=="PO"){
      # Saving separately those new names:
      coef = c("beta[1,1]", "beta[2,1]",
               "beta[1,2]", "beta[2,2]")
      
      # Filtering our data according to selected options:
      df = df %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        # Parameter %in% input$coefs)
        Parameter %in% coef)
      
      # Plotting the boxplots for the relative bias:
      ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
        geom_abline(intercept=0, slope=0,
                    linetype="dashed", color="black") +
        facet_wrap(~ G.Baseline + F.Baseline) +
        labs(x="Coefficients", y="Relative Bias (%)") +
        scale_x_discrete(labels = c(
          expression(paste(beta[11])), expression(paste(beta[21])),
          expression(paste(beta[12])), expression(paste(beta[22])))) +
        theme(legend.position="bottom")
    }
    # If we want YP models:
    else{
      # Saving separately those new names:
      coef = c("beta^(S)[1,1]", "beta^(S)[2,1]",
               "beta^(S)[1,2]", "beta^(S)[2,2]",
               "beta^(L)[1,1]", "beta^(L)[2,1]",
               "beta^(L)[1,2]", "beta^(L)[2,2]")
      
      # Filtering our data according to selected options:
      df = df %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        # Parameter %in% input$coefs)
        Parameter %in% coef)
      
      # Plotting the boxplots for the relative bias:
      ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
        geom_abline(intercept=0, slope=0,
                    linetype="dashed", color = "black") +
        facet_wrap(~ G.Baseline + F.Baseline) +
        labs(x="Coefficients", y="Relative Bias (%)") +
        scale_x_discrete(labels = c(
          expression(paste(beta[11]^"(S)")),
          expression(paste(beta[21]^"(S)")),
          expression(paste(beta[12]^"(S)")),
          expression(paste(beta[22]^"(S)")),
          expression(paste(beta[11]^"(L)")),
          expression(paste(beta[21]^"(L)")),
          expression(paste(beta[12]^"(L)")),
          expression(paste(beta[22]^"(L)")))) +
        theme(legend.position="bottom")
    }
  },
  # width = 1000, height = 450)
  width = 900, height = 400)
  
  output$tabResults = renderDataTable({
    # If we want PH or PO models:
    # if(sum(which(df$Reg.Class=="YP"))==0){
    # if(input$comb=="PH x 0.25" | input$comb=="PO x 0.25" |
    #    input$comb=="PH x 0.5" | input$comb=="PO x 0.5" |
    #    input$comb=="PH x 0.75" | input$comb=="PO x 0.75"){
    if(input$reg=="PH" | input$reg=="PO"){
      # Saving separately those new names:
      coef_uc = c("\u03B2\U2081\U2081", "\u03B2\U2082\U2081",
                  "\u03B2\U2081\U2082", "\u03B2\U2082\U2082")
      # Fixing on another object to allow updates:
      dft = sumdata() %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        Parameter %in% coef_uc)
      dft = dft[,-c(1,2,3,8)] %>% mutate(
        across(where(is.numeric), round, 4))
    }
    else{
      # Saving separately those new names:
      coef_uc = c("\u03B2^(S)\U2081\U2081","\u03B2^(S)\U2082\U2081",
                  "\u03B2^(S)\U2081\U2082","\u03B2^(S)\U2082\U2082",
                  "\u03B2^(L)\U2081\U2081","\u03B2^(L)\U2082\U2081",
                  "\u03B2^(L)\U2081\U2082","\u03B2^(L)\U2082\U2082")
      
      # Summary tables:
      dft = sumdata() %>% filter(
        G.Copula %in% input$gcops, G.Baseline %in% input$gbsls,
        F.Copula %in% input$fcops, F.Baseline %in% input$fbsls,
        Parameter %in% coef_uc)
      dft = dft[,-c(1,2,3,8)] %>% mutate(
        across(where(is.numeric), round, 4))
    }
  },
  # options=list(pageLength=12, lengthMenu=c(12, 24, 120))
  options=list(pageLength=24, lengthMenu=c(24, 48, 240)))
}

# Running all 9 data sets at a single application:

shinyApp(ui=ui, server=server)