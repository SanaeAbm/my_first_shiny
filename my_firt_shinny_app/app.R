
library(Stat2Data)
data(Film)
library(shiny)
library(tidyverse)
library(ggplot2)


var_list=c("Time","Cast","Rating")

Film$Good[which(Film$Good == 0)] <- 'Bad'
Film$Good[which(Film$Good == 1)] <- 'Good'
Film$Good <- as.factor(Film$Good)

Film=Film%>% mutate(Origin = replace(Origin, Origin==0, "USA"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==1, "GB"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==2, "FR"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==3, "ITL"))
Film=Film%>% mutate(Origin = replace(Origin, Origin==4, "CANADA"))


ui <- navbarPage("Film data in a Shinny App",
                 tabPanel("Data set description",
                          includeText("description.txt")),
                 tabPanel("Descriptive statistics",
                     fluidPage(
                     sidebarLayout(sidebarPanel(
                     selectInput("select", label = h3("Main descriptive statistics per variable"), 
                                     choices = var_list,
                                     selected = 1)),
                    
                     mainPanel(tableOutput('table'))
                    
                     ))),
                 
                 navbarMenu("Plot graphs",
                            tabPanel("Year-time scatterplot",
                                     basicPage(
                                       plotOutput("p1", click = "click"),
                                       verbatimTextOutput("info")
                                     )
                 ),
                                
                            tabPanel("Time Histogram",
                                     fluidPage(  
                                         titlePanel( "Time Distribution Histogram" ),  
                                          sidebarLayout( 
                                             sidebarPanel(
                                                 sliderInput(
                                                      "bins",
                                                      "Bin Width:",
                                                      min = 5,
                                                      max = 50,
                                                      value = 20 )
                                                ),
                                             mainPanel(   
                                                  plotOutput( "p2" )
                                                )
                                           )
                                        )
                                     )),
                 tabPanel("References",
                          includeMarkdown("references.md"))

)




server <- function(input, output){
    df <- reactive({
        x<-as.data.frame.table(as.array(summary(Film[,input$select])))
        names(x)<-c("Statistic","Value")
        return(x)
    })
    
    output$table <- renderTable(df(),colnames = TRUE)
    
    output$p1 <- renderPlot({
      ggplot(data =Film) + geom_point(mapping = aes(x = Year, y = Time,color=Good),size=4)+ ggtitle( "Duration per year scatterplot")
    })
    
    
    output$info <- renderText({
      a=strtrim(input$click$x,4)
      b= strtrim(input$click$y,5)
      paste0("Year=",a, "\nRunning time=",b)
    })
    
    output$p2 <- renderPlot( {
           ggplot( data=Film, aes( x=Time ) ) + geom_histogram( binwidth=input$bins, colour="white", fill="lightblue", alpha=0.7 ) + scale_y_continuous( breaks=seq( 0, length( Film$Time ), by=2 ) ) + ggtitle( "Movie duration distribution" )
        } )
}



# Run the application 
shinyApp(ui = ui, server = server)