
library(Stat2Data)
data(Film)
library(shiny)
library(tidyverse)
library(ggplot2)


var_list=c("Time","Cast","Rating")

Film=Film%>% mutate(Origin = replace(Origin, Origin==0, "USA"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==1, "GB"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==2, "FR"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==3, "ITL"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==4, "CANADA"))


ui <- navbarPage("Film data in R Shinny App",
                 tabPanel("Data set description",
                          includeText("description.txt")),
                 tabPanel("Descriptive statistics",
                     fluidPage(
                     sidebarLayout(sidebarPanel(
                     selectInput("select", label = h3("Univariante statistic"), 
                                     choices = var_list,
                                     selected = 1)),
                    
                     mainPanel(tableOutput('table'))
                    
                     ))),
                 
                 navbarMenu("Plot graphs",
                            tabPanel("Graph1",
                            fluidPage(
                                    h3("Plots"),
                                    plotOutput(outputId = "p1")
                                )
                 ),
                                
                            tabPanel("Graph2")),
                 tabPanel("References",
                          includeMarkdown("references.md"))
)

server <- function(input, output){
    dt_f <- reactive({
        x<-as.array(summary(Film[,input$select]))
    })
    output$table <- renderTable(dt_f())
}


# Run the application 
shinyApp(ui = ui, server = server)