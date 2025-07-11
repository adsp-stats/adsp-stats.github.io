library(shiny)
library(bslib)

ui <- page_fluid(
      tags$head(tags$style(HTML("body {overflow-x: hidden;}"))),
  title = "Normal distribution PDF",
  fluidRow(plotOutput("distPlot")),
  fluidRow(column(width=6,sliderInput("mu", "Mean (mu)", min=-10, max=10, value=0)),
           column(width=6,sliderInput("sigma", "Std Dev (sigma)", min=0.01, max=10, value=1))))

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x <- seq(input$mu-3*input$sigma,input$mu+3*input$sigma,input$sigma/100)
    y <- dnorm(x,input$mu,input$sigma)
    xlims <- c(mean(c(-3,x[1])),mean(c(3,x[601])))
    ylims <- c(0,mean(c(dnorm(0),y[301])))
    plot(x=x,y=y,main=NULL,xlab='x',ylab='Density',type='l',lwd=2,
         xlim=xlims,ylim=ylims)
  })
}

shinyApp(ui = ui, server = server)
