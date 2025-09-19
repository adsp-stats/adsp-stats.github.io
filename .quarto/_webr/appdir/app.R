library(shiny)
library(bslib)

set.seed(1628)
hetdat <- data.frame(X=runif(200,min=0,max=10))
hetdat$Y <- 10 + 1.5*hetX + 0.5*hetX*rnorm(200)
hetLM <- lm(Y~X,hetdat)
hetint <- predict(hetLM,newdata=data.frame(X=seq(0,10,0.1)),
                  interval='prediction',level=0.8)
  
ui <- page_fluid(
  tags$head(tags$style(HTML("body {overflow-x: hidden;}"))),
  title = "80% CIs for predicted new observations",
  fluidRow(sliderInput("x", "x-level of new observations", min=0, max=10, value=5, step=0.1)),
  fluidRow(plotOutput("distPlot1")))

server <- function(input, output) {
  index <- reactive({input$x*10+1})
  output$distPlot1 <- renderPlot({
    plot(NA,NA,xlab='X',ylab='Y',xlim=c(-0.1,10.1),ylim=c(0,40))
    rect(xleft=input$x-0.1,ybottom=hetint[index(),2],
         xright=input$x+0.1,ytop=hetint[index(),3],
         border=NA,density=NA,col='#0000ff50')
    points(hetdat$X,hetdat$Y,)})
}

shinyApp(ui = ui, server = server)
