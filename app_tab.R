####################################################################

# Loading all required packages:

library(ggplot2)   # Enhanced graphical exhibition.
# library(tidyverse) # A hub of packages for data manipulation.
library(dplyr)     # Main package for data manipulation.
# library(rsconnect) # To publish your Shiny app.R file.
library(shiny)     # The Shiny package for presentations.
library(bslib)     # Allows choosing different Shiny themes.

# Loading the package that allows dashboards for Shiny:

library(shinydashboard)

# Reading the summary tables:

# Online:
url_tr = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_reg.rds?raw=true"
tr = try(readRDS(url(url_tr)), TRUE)
url_tc = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_cri.rds?raw=true"
tc = try(readRDS(url(url_tc)), TRUE)
url_tt = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_tau.rds?raw=true"
tt = try(readRDS(url(url_tt)), TRUE)
url_tlrt = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_lrt.rds?raw=true"
tlrt = try(readRDS(url(url_tlrt)), TRUE)
url_tct = "https://github.com/wrmfstat/simsurvcop/blob/main/tab_ct.rds?raw=true"
tct = try(readRDS(url(url_tct)), TRUE)

# Correcting some of the loaded tables:

colnames(tr)[14] = "CP"
colnames(tt)[8] = "ARB"
tlrt = ungroup(tlrt)
colnames(tct)[8] = "ARB"

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

tt$Parameter = case_when(tt$Parameter=="tau" ~ "\u03C4")

# Vectors for simulation scenarios (note that results are divided by
# regression class and true correlation):

# reg = c("PH", "PO", "YP")
# cor = c(0.25, 0.50, 0.75)
# gcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
# gbsl = c("EW", "W")
fcop = c("AMH", "Clayton", "Frank", "GH", "Joe")
fbsl = c("BP", "PE", "W")

freg = c("PH", "PO") # Only for LR results.

# Building each sub-interface for each tab of results:

fr_tab1 = fluidRow(
  column(width=2, offset=1, selectInput(
    inputId="reg1", choices=c("PH", "PO", "YP"),
    # label="Generated/Fitted Regression Class",
    label="Regression Class", selected="PH"),
  ),
  column(2, selectInput(
    inputId="cor1", choices=c(0.25, 0.5, 0.75),
    label="Correlation", selected = 0.25),
  ),
  column(2, selectInput(
    inputId="mar1", choices=c("M1", "M2"),
    label="Margin", selected="M1"),
  ),
  column(2, selectInput(
    inputId="gcop1",
    choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    label="Generated Copula", selected="AMH"),
  ),
  column(2, selectInput(
    inputId="fcop1",
    choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    label="Fitted Copula", selected="AMH"),
  ),
  
  # Inserting the boxplots for the chosen input:
  # titlePanel("Relative Bias for Regression Parameter Estimates"),
  fluidRow(column(
    width=2, offset=1,
    # checkboxGroupInput(
    #   inputId="gbsl1", label="Generated Baseline",
    #   choices=gbsl, selected="W", inline=T),
    checkboxGroupInput(
      inputId="fbsl1", label="Fitted Baseline",
      choices=fbsl, selected=fbsl, inline=T),
  ),
  column(width=8, mainPanel(plotOutput("reg_rbPlots"))),
  
  # Summary tables for the chosen input:
  # titlePanel("Monte Carlo Results"),
  fluidRow(column(width=9, offset=1,
                  mainPanel(dataTableOutput("reg_tabResults"))))
  )
)

fr_tab2 = fluidRow(
  column(3, offset=2, selectInput(
    inputId="reg3", choices=c("PH", "PO", "YP"),
    label="Regression Class", selected="PH"),
  ),
  column(3, selectInput(
    inputId="cor3", choices=c(0.25, 0.5, 0.75),
    label="Correlation", selected = 0.25),
  ),
  column(3, selectInput(
    inputId="gcop3",
    choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    label="Generated Copula", selected="AMH"),
  ),
  # column(3, selectInput(
  #   inputId="fcop3",
  #   choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
  #   label="Fitted Copula", selected="AMH"),
  # ),
  
  # Inserting the boxplots for the chosen input:
  # titlePanel("Relative Bias for Correlation Parameter Estimates"),
  fluidRow(column(width=2, offset=1,
    # checkboxGroupInput(
    #   inputId="gbsl3", label="Generated Baseline",
    #   choices=gbsl, selected="W", inline=T),
    checkboxGroupInput(inputId="fcop3", label="Fitted Copula",
                       choices=fcop, selected=fcop, inline=T),
    checkboxGroupInput(inputId="fbsl3", label="Fitted Baseline",
                       choices=fbsl, selected=fbsl, inline=T),
  ),
  column(width=8, mainPanel(plotOutput("cor_rbPlots"))),
  
  # Summary tables for the chosen input:
  # titlePanel("Monte Carlo Results"),
  fluidRow(column(width=7, offset=3,
                  mainPanel(dataTableOutput("cor_tabResults"))))
  )
)

fr_tab3 = fluidRow(
  column(3, offset=2, selectInput(
    inputId="reg2", choices=c("PH", "PO", "YP"),
    # label="Generated/Fitted Regression Class",
    label="Regression Class",
    selected="PH"),
  ),
  column(3, selectInput(
    inputId="cor2", choices=c(0.25, 0.5, 0.75),
    label="Correlation", selected = 0.25),
  ),
  column(3, selectInput(
    inputId="gcop2",
    choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    label="Generated Copula", selected="AMH"),
  ),
  # column(3, selectInput(
  #   inputId="fcop2",
  #   choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
  #   label="Fitted Copula", selected="AMH"),
  # ),
  
  # Inserting the boxplots for the chosen input:
  # titlePanel("AIC Values for Fitted Survival Copula Models"),
  fluidRow(column(width=2, offset=1,
    # checkboxGroupInput(
    #   inputId="gbsl2", label="Generated Baseline",
    #   choices=gbsl, selected="W", inline=T),
    checkboxGroupInput(inputId="fcop2", label="Fitted Copula",
                       choices=fcop, selected=fcop, inline=T),
    checkboxGroupInput(inputId="fbsl2", label="Fitted Baseline",
                       choices=fbsl, selected=fbsl, inline=T),
  ),
  column(width=8, mainPanel(plotOutput("aicPlots"))),
  
  # Summary tables for the chosen input:
  # titlePanel("Monte Carlo Results"),
  fluidRow(column(width=8, offset=3,
                  mainPanel(dataTableOutput("aicResults"))))
  )
)

fr_tab4 = fluidRow(
  column(3, offset=2, selectInput(
    inputId="gcop4",
    choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    label="Generated Copula", selected="AMH"),
  ),
  column(3, selectInput(
    inputId="cor4", choices=c(0.25, 0.5, 0.75),
    label="Correlation", selected = 0.25),
  ),
  # column(3, selectInput(
  #   inputId="fcop4",
  #   choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
  #   label="Fitted Copula", selected="AMH"),
  # ),
  column(3, selectInput(
    inputId="greg4", choices=c("PH", "PO", "YP"),
    label="Generated Class", selected="PH"),
  ),
  # column(3, selectInput(
  #   inputId="freg4", choices=c("PH", "PO"),
  #   label="Fitted Class, Nested to YP", selected="PH"),
  # ),
  
  # Summary tables for the chosen input:
  # titlePanel("LR Tests for Fitted Survival Copula Models"),
  fluidRow(column(width=2, offset=1,
    # checkboxGroupInput(
    #   inputId="gbsl4", label="Generated Baseline",
    #   choices=gbsl, selected="W", inline=T),
    checkboxGroupInput(inputId="fbsl4", label="Fitted Baseline",
                       choices=fbsl, selected=fbsl, inline=T),
    checkboxGroupInput(inputId="freg4",
                       label="Fitted Class, Nested to YP",
                       choices=freg, selected=freg, inline=T),
  ),
  column(width=8, # offset=1,
         mainPanel(dataTableOutput("lrResults"))))
)

fr_tab5 = fluidRow(
  column(width=4, offset=2, selectInput(
    inputId="margs", choices=c("M1", "M2"),
    label="Margin", selected="M1"),
  ),
  column(width=4, selectInput(
    inputId="gcops",
    choices=c("AMH", "Clayton", "Frank", "GH", "Joe"),
    label="Generated Copula", selected="AMH"),
  ),
  
  # Inserting the boxplots for the chosen input:
  # titlePanel("AIC Values for Fitted Survival Copula Models"),
  fluidRow(column(width=2, offset=1,
    # checkboxGroupInput(
    #   inputId="gbsls", label="Generated Baseline",
    #   choices=gbsl, selected="W", inline=T),
    checkboxGroupInput(
      inputId="fbsls", label="Fitted Baseline",
      choices=fbsl, selected=fbsl, inline=T),
  ),
  column(width=8, mainPanel(plotOutput("ct_rbPlots"))),
  
  # Summary tables for the chosen input:
  # titlePanel("Monte Carlo Results"),
  fluidRow(column(width=8, offset=2,
                  mainPanel(dataTableOutput("ct_tabResults"))))
  )
)

# Loading the user interface (dashboard) to all boxplots and tables:

ui = dashboardPage(
  dashboardHeader(
    title="Monte Carlo Results for Survival Copula Models"),
  dashboardSidebar(
    sidebarMenu(
      # Menu items for each tab:
      menuItem(text="Regression Estimates",
               tabName="reg_est", icon=icon("database")),
      menuItem(text="Correlation Estimates",
               tabName="cor_est", icon=icon("database")),
      menuItem(text="AIC Values",
               tabName="aic_cri", icon=icon("database")),
      menuItem(text="Likelihood Ratio Tests",
               tabName="lrt_tes", icon=icon("database")),
      menuItem(text="Crossing Time Estimates",
               tabName="cst_est", icon=icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      # Loading the interface for each tab with the 1st/main title:
      tabItem(
        tabName="reg_est",
        h2("Relative Bias Boxplots and Monte Carlo Statistics for Regression Estimates"),
        fr_tab1),
      tabItem(
        tabName="cor_est",
        h2("Relative Bias Boxplots and Monte Carlo Statistics for Correlation Estimates"),
        fr_tab2),
      tabItem(
        tabName="aic_cri",
        h2("Akaike Information Criteria for Fitted Models"),
        fr_tab3),
      tabItem(
        tabName="lrt_tes",
        h2("Likelihood Ratio Tests for Nested Classes against YP"),
        fr_tab4),
      tabItem(
        tabName="cst_est",
        h2("Relative Bias Boxplots and Monte Carlo Statistics for Bootstrapped Crossing Time Estimates"),
        fr_tab5)
    )
  ),
  skin = "black"
)

server = function(input, output){
  # Code for output of 1st tab:
  
  # Reading online data:
  reg_PH_25 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_ph025.rds?raw=true"
  reg_PO_25 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_po025.rds?raw=true"
  reg_YP_25 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_yp025.rds?raw=true"
  reg_PH_50 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_ph050.rds?raw=true"
  reg_PO_50 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_po050.rds?raw=true"
  reg_YP_50 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_yp050.rds?raw=true"
  reg_PH_75 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_ph075.rds?raw=true"
  reg_PO_75 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_po075.rds?raw=true"
  reg_YP_75 = "https://github.com/wrmfstat/simsurvcop/blob/main/res_reg_yp075.rds?raw=true"
  
  # Choosing results' data from a given input:
  reg_bigdata = reactive({
    if(input$reg1=="PH"){
      switch(input$cor1,
        `0.25` = try(readRDS(url(reg_PH_25)), TRUE),
        `0.5` = try(readRDS(url(reg_PH_50)), TRUE),
        `0.75` = try(readRDS(url(reg_PH_75)), TRUE)
      )
    }
    else if(input$reg1=="PO"){
      switch(input$cor1,
        `0.25` = try(readRDS(url(reg_PO_25)), TRUE),
        `0.5` = try(readRDS(url(reg_PO_50)), TRUE),
        `0.75` = try(readRDS(url(reg_PO_75)), TRUE)
      )
    }
    else{
      switch(input$cor1,
        `0.25` = try(readRDS(url(reg_YP_25)), TRUE),
        `0.5` = try(readRDS(url(reg_YP_50)), TRUE),
        `0.75` = try(readRDS(url(reg_YP_75)), TRUE)
      )
    }
  })
  
  # Choosing summary table data from a given input:
  reg_sumdata = reactive({
    if(input$reg1=="PH"){
      switch(input$cor1,
        `0.25` = tr %>% filter(Reg.Class=="PH" & TrueTau==0.25),
        `0.5` = tr %>% filter(Reg.Class=="PH" & TrueTau==0.5),
        `0.75` = tr %>% filter(Reg.Class=="PH" & TrueTau==0.75)
      )
    }
    else if(input$reg1=="PO"){
      switch(input$cor1,
        `0.25` = tr %>% filter(Reg.Class=="PO" & TrueTau==0.25),
        `0.5` = tr %>% filter(Reg.Class=="PO" & TrueTau==0.5),
        `0.75` = tr %>% filter(Reg.Class=="PO" & TrueTau==0.75)
      )
    }
    else{
      switch(input$cor1,
        `0.25` = tr %>% filter(Reg.Class=="YP" & TrueTau==0.25),
        `0.5` = tr %>% filter(Reg.Class=="YP" & TrueTau==0.5),
        `0.75` = tr %>% filter(Reg.Class=="YP" & TrueTau==0.75)
      )
    }
  })
  
  # First, open the output environments!
  
  output$reg_rbPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = reg_bigdata()
    
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
    if(input$reg1=="PH" | input$reg1=="PO"){
      # Saving separatedely those new names:
      coef = c("beta[1,1]", "beta[2,1]",
               "beta[1,2]", "beta[2,2]")
      
      # Filtering our data according to selected options:
      df = df %>% filter(
        Margin %in% input$mar1,
        G.Copula %in% input$gcop1, # G.Baseline %in% input$gbsl1,
        F.Copula %in% input$fcop1, F.Baseline %in% input$fbsl1,
        # Parameter %in% input$coef1)
        Parameter %in% coef)
      
      # Plotting the boxplots for the relative bias:
      if(input$mar1=="M1"){
        ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
          geom_abline(intercept=0, slope=0,
                      linetype="dashed", color="black") +
          # facet_wrap(~ G.Baseline + F.Baseline) +
          facet_wrap(~F.Baseline) +
          labs(x="Coefficients", y="Relative Bias (%)") +
          scale_x_discrete(labels = c(
            expression(paste(beta[11])), expression(paste(beta[21])))) +
          theme(legend.position="bottom")
      }
      else{
        ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
          geom_abline(intercept=0, slope=0,
                      linetype="dashed", color="black") +
          # facet_wrap(~ G.Baseline + F.Baseline) +
          facet_wrap(~F.Baseline) +
          labs(x="Coefficients", y="Relative Bias (%)") +
          scale_x_discrete(labels = c(
            expression(paste(beta[12])), expression(paste(beta[22])))) +
          theme(legend.position="bottom")
      }
    }
    # If we want YP models:
    else{
      # Saving separatedely those new names:
      coef = c("beta^(S)[1,1]", "beta^(S)[2,1]",
               "beta^(S)[1,2]", "beta^(S)[2,2]",
               "beta^(L)[1,1]", "beta^(L)[2,1]",
               "beta^(L)[1,2]", "beta^(L)[2,2]")
      
      # Filtering our data according to selected options:
      df = df %>% filter(
        Margin %in% input$mar1,
        G.Copula %in% input$gcop1, # G.Baseline %in% input$gbsl1,
        F.Copula %in% input$fcop1, F.Baseline %in% input$fbsl1,
        # Parameter %in% input$coef1)
        Parameter %in% coef)
      
      # Plotting the boxplots for the relative bias:
      if(input$mar1=="M1"){
        ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
          geom_abline(intercept=0, slope=0,
                      linetype="dashed", color = "black") +
          # facet_wrap(~ G.Baseline + F.Baseline) +
          facet_wrap(~F.Baseline) +
          labs(x="Coefficients", y="Relative Bias (%)") +
          scale_x_discrete(labels = c(
            expression(paste(beta[11]^"(S)")),
            expression(paste(beta[21]^"(S)")),
            expression(paste(beta[11]^"(L)")),
            expression(paste(beta[21]^"(L)")))) +
          theme(legend.position="bottom")
      }
      else{
        ggplot(df, aes(x=Parameter, y=RB)) + geom_boxplot() +
          geom_abline(intercept=0, slope=0,
                      linetype="dashed", color = "black") +
          # facet_wrap(~ G.Baseline + F.Baseline) +
          facet_wrap(~ F.Baseline) +
          labs(x="Coefficients", y="Relative Bias (%)") +
          scale_x_discrete(labels = c(
            expression(paste(beta[12]^"(S)")),
            expression(paste(beta[22]^"(S)")),
            expression(paste(beta[12]^"(L)")),
            expression(paste(beta[22]^"(L)")))) +
          theme(legend.position="bottom")
      }
    }
  },
  # width = 900, height = 400)
  width = 750, height = 360)
  
  output$reg_tabResults = renderDataTable({
    # If we want PH or PO models:
    if(input$reg1=="PH" | input$reg1=="PO"){
      # Saving separatedely those new names:
      coef_uc = c("\u03B2\U2081\U2081", "\u03B2\U2082\U2081",
                  "\u03B2\U2081\U2082", "\u03B2\U2082\U2082")
      # Fixing on another object to allow updates:
      dft = reg_sumdata() %>% filter(
        Margin %in% input$mar1,
        G.Copula %in% input$gcop1, # G.Baseline %in% input$gbsl1,
        F.Copula %in% input$fcop1, F.Baseline %in% input$fbsl1,
        Parameter %in% coef_uc)
      dft = dft[,-c(1,2,6)] %>% mutate(
        across(where(is.numeric), round, 4))
    }
    else{
      # Saving separatedely those new names:
      coef_uc = c("\u03B2^(S)\U2081\U2081","\u03B2^(S)\U2082\U2081",
                  "\u03B2^(S)\U2081\U2082","\u03B2^(S)\U2082\U2082",
                  "\u03B2^(L)\U2081\U2081","\u03B2^(L)\U2082\U2081",
                  "\u03B2^(L)\U2081\U2082","\u03B2^(L)\U2082\U2082")
      
      # Summary tables:
      dft = reg_sumdata() %>% filter(
        Margin %in% input$mar1,
        G.Copula %in% input$gcop1, # G.Baseline %in% input$gbsl1,
        F.Copula %in% input$fcop1, F.Baseline %in% input$fbsl1,
        Parameter %in% coef_uc)
      dft = dft[,-c(1,2,6)] %>% mutate(
        across(where(is.numeric), round, 4))
    }
  },
  options=list(pageLength=6, lengthMenu=c(2, 6)))
  
  # Code for output of 2nd tab:
  
  # Reading online data (make the project public later!):
  cri = "https://github.com/wrmfstat/simsurvcop/blob/main/res_cri.rds?raw=true"
  
  # Choosing results' data from a given input:
  cri_bigdata = reactive({
    try(readRDS(url(cri)), TRUE)
  })
  
  # Choosing summary table data from a given input:
  cri_sumdata = reactive({
    tc
  })
  
  # First, open the output environments!
  
  output$aicPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = cri_bigdata()
    
    # Filtering our data according to selected options:
    df = df %>% filter(
      Reg.Class %in% input$reg2, TrueTau %in% input$cor2,
      G.Copula %in% input$gcop2, # G.Baseline %in% input$gbsl2,
      F.Copula %in% input$fcop2, F.Baseline %in% input$fbsl2)
    
    # Plotting the boxplots for the relative bias:
    ggplot(df, aes(x=F.Copula, y=AIC)) + geom_boxplot() +
      geom_abline(intercept=0, slope=0,
                  linetype="dashed", color="black") +
      # facet_wrap(~ G.Baseline + F.Baseline) +
      facet_wrap(~F.Baseline) +
      labs(x="Fitted Copula", y="AIC") +
      theme(legend.position="bottom")
  },
  # width = 900, height = 400)
  width = 750, height = 360)
  
  output$aicResults = renderDataTable({
    # Fixing on another object to allow updates:
    dft = cri_sumdata() %>% filter(
      Reg.Class %in% input$reg2, TrueTau %in% input$cor2,
      G.Copula %in% input$gcop2, # G.Baseline %in% input$gbsl2,
      F.Copula %in% input$fcop2, F.Baseline %in% input$fbsl2)
    dft = dft[,-c(1,5,7)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  options=list(pageLength=15, lengthMenu=c(3, 15)))
  
  # Code for output of 3rd tab:
  
  # Reading online data:
  corr = "https://github.com/wrmfstat/simsurvcop/blob/main/res_tau.rds?raw=true"
  
  # Choosing results' data from a given input:
  cor_bigdata = reactive({
    try(readRDS(url(corr)), TRUE)
  })
  
  # Choosing summary table data from a given input:
  cor_sumdata = reactive({
    tt
  })
  
  # First, open the output environments!
  
  output$cor_rbPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = cor_bigdata()
    
    # Saving separately those new names:
    coef = c("tau")
    
    # Filtering our data according to selected options:
    df = df %>% filter(
      TrueTau %in% input$cor3,
      G.Copula %in% input$gcop3, # G.Baseline %in% input$gbsl3,
      F.Copula %in% input$fcop3, F.Baseline %in% input$fbsl3,
      Reg.Class %in% input$reg3, Parameter %in% coef)
    
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
  # width = 900, height = 400)
  width = 750, height = 360)
  
  output$cor_tabResults = renderDataTable({
    # Saving separatedely those new names:
    coef_uc = c("\u03C4")
    # Fixing on another object to allow updates:
    dft = cor_sumdata() %>% filter(
      TrueTau %in% input$cor3,
      G.Copula %in% input$gcop3, # G.Baseline %in% input$gbsl3,
      F.Copula %in% input$fcop3, F.Baseline %in% input$fbsl3,
      Reg.Class %in% input$reg3, Parameter %in% coef_uc)
    dft = dft[,-c(1,5)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  options=list(pageLength=15, lengthMenu=c(3, 15)))
  
  # Code for output of 4th tab:
  
  # Choosing summary table data from a given input:
  lrt_sumdata = reactive({
    tlrt
  })
  
  # First, open the output environments!
  
  output$lrResults = renderDataTable({
    # Fixing on another object to allow updates:
    dft = lrt_sumdata() %>% filter(
      Copula %in% input$gcop4, TrueTau %in% input$cor4,
      G.Reg.Class %in% input$greg4, F.Reg.Class %in% input$freg4,
      # G.Baseline %in% input$gbsl4,
      F.Baseline %in% input$fbsl4)
    dft = dft[,-1] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  options=list(pageLength=6, lengthMenu=c(2, 6)))
  
  # Code for output of 5th tab:
  
  # Reading online data (make the project public later!):
  ct = "https://github.com/wrmfstat/simsurvcop/blob/main/res_ct.rds?raw=true"
  
  # Choosing results' data from a given input:
  ct_bigdata = reactive({
    try(readRDS(url(ct)), TRUE)
  })
  
  # Choosing summary table data from a given input:
  ct_sumdata = reactive({
    tct
  })
  
  # First, open the output environments!
  
  output$ct_rbPlots = renderPlot({
    # Fixing on another object to allow updates:
    df = ct_bigdata()
    
    # Filtering our data according to selected options:
    df = df %>% filter(
      Margin %in% input$margs, Copula %in% input$gcops,
      # G.Baseline %in% input$gbsls,
      F.Baseline %in% input$fbsls)
    
    # Plotting the boxplots for the relative bias:
    ggplot(df) + geom_boxplot(aes(x=Copula, y=RB)) +
      xlab("Copula") + ylab("Relative Bias") +
      # facet_wrap(~ G.Baseline + F.Baseline) +
      facet_wrap(~F.Baseline) +
      geom_hline(yintercept=0, linetype="dashed", color="black") +
      theme(legend.position="bottom")
  },
  # width = 900, height = 400)
  width = 750, height = 360)
  
  output$ct_tabResults = renderDataTable({
    # Fixing on another object to allow updates:
    dft = ct_sumdata() %>% filter(
      Margin %in% input$margs,
      Copula %in% input$gcops, # TrueTau %in% input$cor,
      # G.Reg.Class %in% input$greg, F.Reg.Class %in% input$freg,
      # G.Baseline %in% input$gbsls,
      F.Baseline %in% input$fbsls)
    dft = dft[,-c(2,3,6)] %>% mutate(
      across(where(is.numeric), round, 4))
  },
  options=list(pageLength=3, lengthMenu=3))
}

# Running all tabs in a single dashboard:

shinyApp(ui=ui, server=server)