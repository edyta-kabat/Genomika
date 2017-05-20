require(dplyr)
require(gdata)
library(gdata)
library(gplots)
library(foreign)
require(foreign)
require(RCurl)
require(shiny)
library(survival)
data(package = "survival")
library(rms)




lung$SurvObj <- with(lung, Surv(time, status == 2))
lung$div <- with(lung, lung$pat.karno - lung$ph.karno )
  
  
  ui = fluidPage(
    selectInput("input1", "Wybierz",
                c("Płeć"="sex", 
                  "Centrum leczniczo-badawcze"="inst")),
    textInput("input2", "Dolna grsanica wieku", 39),
    textInput("input3", "Górna granica wieku", 82),
    
    #tableOutput("data"),
    plotOutput("curve"),
    plotOutput('samopoczucie')
    
  )
  
  

  
  server = function(input, output) {
    
    output$m <- renderPrint ({input$input2})
    output$M <- renderPrint({input$input3})

    
    output$curve <- renderPlot({survplot(npsurv(formula = Surv(time,status == 2) ~ sex, 
                                               data = lung[lung$age > input$input2 & lung$age < input$input3, ]))
    conf = c("none", "bands", "bars")[1]
    xlab = ""
    ylab = "Survival"
    label.curves = TRUE
    levels.only = FALSE
    abbrev.label = FALSE
    loglog =FALSE
    logt = FALSE
    time.inc = 100
    dots  = TRUE
    n.risk = TRUE
    xlab = "Czas"
    ylab = "Prawdopodobieństwo przeżycia"
 
      })
    output$samopoczucie <- renderPlot( { hist(lung[lung$age > input$input2 & lung$age < input$input3, ]$div,
                                              main = "Histogram samopoczucia pacjenyów" ,
                                              xlab =  "Ocena stanu zdrowia wg (pacjęta - lekarza)",
                                              ylab = "Ilość wystąpień",
                                              )})
    
  }
  
  


 
  shinyApp(ui,server)
  