library(shiny)
library(bslib)

ui <- page_fluid(
  tags$head(tags$style(HTML("body {overflow-x: hidden;}"))),
  title = "Variance and bias of Normal estimators",
  fluidRow(column(width=3,""),column(width=6,sliderInput("nsamp", "N (sample size)", min=5, max=200, value=25)),column(width=3,"")),
  fluidRow(column(width=12,plotOutput("distPlot1"))),
  fluidRow(column(width=12,plotOutput("distPlot2"))))

server <- function(input, output) {
  x <- reactive({matrix(rnorm(10000*input$nsamp,100,20),nrow=10000)})
  xbar <- reactive({apply(x(),1,mean)})
  xmed <- reactive({apply(x(),1,median)})
  xsd <- reactive({apply(x(),1,sd)})
  xrmse <- reactive({sqrt(xsd()^2*(input$nsamp-1)/input$nsamp)})
  output$distPlot1 <- renderPlot({hist(xbar(), breaks=seq(min(xbar(),xmed()),max(xbar(),xmed()),length.out=31), main='10,000 estimates for the mean', xlab='Mu-hat', ylab='Frequency', col='#ff000080'); hist(xmed(), breaks=seq(min(xbar(),xmed()),max(xbar(),xmed()),length.out=31), col='#0000ff80', add=TRUE); legend(x='topright', legend=c('Sample mean','Sample median'), fill=c('#ff000080','#0000ff80'), bty='n')})
  output$distPlot2 <- renderPlot({hist(xrmse(), breaks=seq(min(xrmse(),xsd()),max(xrmse(),xsd()),length.out=31), main='10,000 estimates for the std dev', xlab='Sigma-hat', ylab='Frequency', col='#ff000080'); hist(xsd(), breaks=seq(min(xrmse(),xsd()),max(xrmse(),xsd()),length.out=31), col='#0000ff80', add=TRUE); legend(x='topright', legend=c('Uncorrected: /(n)','Corrected: /(n-1)'), fill=c('#ff000080','#0000ff80'), bty='n')})
}

shinyApp(ui = ui, server = server)
