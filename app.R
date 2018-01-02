# Load packages ----
library(shiny)
library(tidyverse)
library(rmarkdown)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  navbarPage("Kidney",  
             tabPanel("Calculate eGFR and BSA",
                      sidebarLayout(
                        sidebarPanel(fluidRow(column(6, 
                                                     numericInput("age", 
                                                                  label = h4("Age", br(), "(years old)"), 
                                                                  value = 40)), 
                                              column(6, 
                                                     numericInput("cre", 
                                                                  label = h4("Creatinine", br(), "(mg/dL)"), 
                                                                  value = 1)) 
                        ), 
                        fluidRow(column(6,
                                        numericInput("bw", label = h4("Body Weight", br(), "(kg)"), 
                                                     value = 60)), 
                                 column(6,
                                        numericInput("height", label = h4("Height", br(), "(cm)"),                                    
                                                     value = 160))      
                        ), 
                        fluidRow(column(6,  
                                        radioButtons("sex", label = h4("Sex"), 
                                                     choices = list("male" = "male", 
                                                                    "female" = "female"), 
                                                     selected = "male")), 
                                 column(6, 
                                        radioButtons("race", label = h4("Race"), 
                                                     choices = list("Caucasians" = "Caucasians", 
                                                                    "Blacks" = "Blacks", 
                                                                    "Japan" = "Japan")))
                        ), 
                        radioButtons("adj_BSA", label = h4("BSA using for adjust"), 
                                     choices = list("Du bois" = "Du bois", 
                                                    "Shintani" = "Shintani", 
                                                    "Fujimoto" = "Fujimoto"), 
                                     selected = "Du bois"), 
                        br(), 
                        actionButton("calculate", "Calculate!"), 
                        br(), br(), br(), 
                        img(src = "Kidney.png", width = 375, height = 250)
                        ), 
                        mainPanel(fluidRow(column(12, h2(strong("BSA")))
                        ), 
                        fluidRow(column(4, h3("Du bois"), 
                                        verbatimTextOutput("DB")), 
                                 column(4, h3("Shintani"), 
                                        verbatimTextOutput("Shin")), 
                                 column(4, h3("Fujimoto"), 
                                        verbatimTextOutput("Fuji"))
                        ), 
                        br(), br(), hr(),
                        fluidRow(column(12, h2(strong("eGFR")))
                        ), 
                        fluidRow(column(4, h3("Cockcroft-Gault"), 
                                        verbatimTextOutput("Cockcroft")), 
                                 column(4, h3("CCr-Sanford"), 
                                        verbatimTextOutput("sanford"))
                        ), 
                        br(), br(), hr(), 
                        fluidRow(column(4, h3("MDRD-Jaffe"), 
                                        verbatimTextOutput("mdrd_j")), 
                                 column(4, h3("MDRD-Jaffe(adj BSA)"), 
                                        verbatimTextOutput("adj_jaffe"))
                        ), 
                        br(), br(), hr(), 
                        fluidRow(column(4, h3("MDRD-enzyme"), 
                                        verbatimTextOutput("mdrd_e")), 
                                 column(4, h3("MDRD-enzyme(adj BSA)"), 
                                        verbatimTextOutput("adj_enzyme"))
                        ), 
                        br(), br(), hr(), 
                        fluidRow(column(4, h3("JSN guideline"), 
                                        verbatimTextOutput("jgfr")), 
                                 column(4, h3("JSN guideline(adj BSA)"), 
                                        verbatimTextOutput("adj_jgfr"))
                        ), 
                        br(), br(), hr()
                        ) 
                      )
             ), 
             tabPanel("Urine Test", 
                      sidebarLayout(
                        sidebarPanel(fluidRow(column(6,
                                                     numericInput("bw2", label = h4("Body Weight", br(), "(kg)"), 
                                                                  value = 60)), 
                                              column(6,
                                                     numericInput("height2", label = h4("Height", br(), "(cm)"),                                    
                                                                  value = 160))      
                        ), 
                        numericInput("UV", label = h4("Urine Volume (mL)"), 
                                     value = NULL), 
                        fluidRow(column(6, 
                                        numericInput("SC", label = h4("Serum Creatinine", 
                                                                      br(), "(mg/dL)"), value = NULL)), 
                                 column(6, 
                                        numericInput("UC", label = h4("Urine Creatinine", 
                                                                      br(), "(mg/dL)"), value = NULL))
                        ), 
                        fluidRow(column(6, 
                                        numericInput("SX", label = h4("Serum X", 
                                                                      br(), "(unit any)"), value = NULL)), 
                                 column(6, 
                                        numericInput("UX", label = h4("Urine X", 
                                                                      br(), "(unit any)"), value = NULL))
                        ), 
                        radioButtons("adj_BSA2", label = h4("BSA using for adjust"), 
                                     choices = list("Du bois" = "Du bois2", 
                                                    "Shintani" = "Shintani2", 
                                                    "Fujimoto" = "Fujimoto2"), 
                                     selected = "Du bois2"), 
                        br(), 
                        actionButton("calculate2", "Calculate!"), 
                        br(), br(), 
                        img(src = "renal.png", width = 375, height = 375)
                        ), 
                        mainPanel(h3("Creatine Clearance (mL/min)"), 
                                  verbatimTextOutput("CCR"), 
                                  br(), br(), hr(), 
                                  h3("Fractional Excertion of X (FEX) (%)"), 
                                  verbatimTextOutput("FEX"), 
                                  br(), br(), hr() 
                        )
                      )
             ),
             navbarMenu("More", 
                        tabPanel("Cockcroft-Gault", 
                                 withMathJax(includeMarkdown("Cock.Rmd"))), 
                        tabPanel("Sanford-formula", 
                                 withMathJax(includeMarkdown("Sanford.Rmd"))), 
                        tabPanel("MDMR-Jaffe", 
                                 withMathJax(includeMarkdown("MDRD.Rmd"))), 
                        tabPanel("MDMR-enzyme", 
                                 withMathJax(includeMarkdown("MDRD_E.Rmd"))), 
                        tabPanel("Japanese formula", 
                                 withMathJax(includeMarkdown("JSN_formula.Rmd"))), 
                        tabPanel("Stageing of CKD (Japanese)", 
                                 includeMarkdown("staging.Rmd"))
             )
  )
)

# Server logic----

server <- function(input, output) {
  DB_input <- eventReactive(input$calculate, {
    Du_bois(input$bw, input$height)
  })
  
  Shin_input <- eventReactive(input$calculate, {
    Shintani(input$bw, input$height)
  })
  
  Fuji_input <- eventReactive(input$calculate, {
    Fujimoto(input$bw, input$height)
  })
  
  output$DB <- renderText({
    DB_input()
  })
  
  output$Shin <- renderText({
    Shin_input()
  })
  
  output$Fuji <- renderText({
    Fuji_input()
  })
  
  ccr_cock <- eventReactive(input$calculate, {
    cockcroft(input$bw, input$age, input$cre, input$sex)
  })
  
  ccr_san <- eventReactive(input$calculate, {
    sanford(input$bw, input$height, input$age, input$cre, input$sex)
  })
  
  ccr_mdrd_jef <- eventReactive(input$calculate, {
    mdrd_jaffe(input$age, input$cre, input$sex, input$race)
  })
  
  ccr_mdrd_jef_adj <- eventReactive(input$calculate, {
    mdrd_jaffe_adj(input$bw, input$height, input$age, input$cre, input$sex, input$race, input$adj_BSA)
  })
  
  ccr_mdrd_enz <- eventReactive(input$calculate, {
    mdrd_enzyme(input$age, input$cre, input$sex, input$race)
  })
  
  ccr_mdrd_enz_adj <- eventReactive(input$calculate, {
    mdrd_enzyme_adj(input$bw, input$height, input$age, input$cre, input$sex, input$race, input$adj_BSA)
  })
  
  ccr_j <- eventReactive(input$calculate, {
    j_gfr(input$age, input$cre, input$sex, input$race)
  })
  
  ccr_j_adj <- eventReactive(input$calculate, {
    j_gfr_adj(input$bw, input$height, input$age, input$cre, input$sex, input$race, input$adj_BSA)
  })
  
  output$Cockcroft <- renderText({
    ccr_cock()
  })
  
  output$sanford <- renderText({
    ccr_san()
  })
  
  output$mdrd_j <- renderText({
    ccr_mdrd_jef()
  })
  
  output$adj_jaffe <- renderText({
    ccr_mdrd_jef_adj()
  })
  
  output$mdrd_e <- renderText({
    ccr_mdrd_enz()
  })
  
  output$adj_enzyme <- renderText({
    ccr_mdrd_enz_adj()
  })
  
  output$jgfr <- renderText({
    ccr_j()
  })
  
  output$adj_jgfr <- renderText({
    ccr_j_adj()
  })
  
  CCR_U <- eventReactive(input$calculate2, {
    U_CRE(input$bw2, input$height2, input$SC, input$UC, input$UV, input$adj_BSA2)
  })
  
  FE_X <- eventReactive(input$calculate2, {
    (input$UX * input$SC) / (input$SX * input$UC) * 100
  })
  
  output$CCR <- renderText({
    CCR_U()
  })
  
  output$FEX <- renderText({
    FE_X()
  })
}

# Run the app
shinyApp(ui, server)
