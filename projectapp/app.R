library(shinydashboard)
library(fpp3)
library(readr)
library(shinyWidgets)
library(shiny)
library(feasts)
library(ggfortify)


sidebar <- dashboardSidebar(
    sidebarUserPanel("Value Seeker",
                     subtitle = a(href = "#", icon("cog", class = "text-success"), "Seeking")
                    ),
    
    sidebarMenu(
        id = "tabs",
        menuItem("Average Prescription Prices", tabName = "dashboard", icon = icon("bar-chart-o")),
        menuItem("Predictive Modeling for Prescription Prices", tabName = "models", icon = icon("table")),
        menuItem("Price Prediction", tabName = "budget", icon = icon("table")
        )
    )
)
body <- dashboardBody(
    tabItems(
        tabItem("dashboard",
              fluidRow(
                box(title="All Prescription Types (Inputs: Plot Type & Time Interval)",
                    width=6,
                    status = "primary",
                    solidHeader = TRUE,
                    plotOutput("savingsplot")),
                box(title="Your Prescription Type (Inputs: Time Interval & Prescription Type)",
                    width=6,
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("diplot"))),
              fluidRow(
                column(width = 4,
                       box(
                         title = "Country",
                         status = "success",
                         solidHeader = TRUE,
                         width = NULL,
                         "Only for Australian prescription data."
                ),
                box(
                  title = "Plot Type",
                  status="warning",
                  solidHeader = TRUE,
                  width=NULL,
                  awesomeRadio("plotty",
                               label = "Choose your plot type.",
                               choices = c("seasonality","autocorrelation","decomposition"))
                ),
                
                box(title="Seasonality Intrepretation",
                    status="danger",
                    solidHeader = TRUE,
                    background = "navy",
                    width = NULL,
                    collapsible=TRUE,
                    "You are looking for changes in the pattern of seasonality over time. So, if there is a consistent pattern over time then average prescription prices have stayed relatively consistent in seasonality. "),
                box(title="Autocorrelation Interpretation",
                    status="danger",
                    solidHeader = TRUE,
                    background = "navy",
                    width = NULL,
                    collapsible=TRUE,
                    "Autocorrelation shows you the correlation between these observations of average prescription price and the average prices that came before them. You are looking for cases of autocorrelation, where the black lines are consistently abive the dotted blue line. Autocorrelation would suggest that the average prescription prices contain a very consistent trend.        "),
                box(title="Decomposition Interpretation",
                    status="danger",
                    solidHeader = TRUE,
                    background = "navy",
                    width = NULL,
                    collapsible=TRUE,
                    "You are looking for what component of the decomposition, trend or seasonality, is changing the most over time. Once you get that answer you can start to come up with conclusions as to how average prescription prices change.          "),
                ),
                column(width=4,
                       box(
                         title="Time Interval",
                         status="primary",
                         width=NULL,
                         "Dates range from July 1991 to June 2008",
                         dateInput("Start", label = "Select a starting date:", value = "1991-07-01"),
                         dateInput("End", label = "Select an ending date:", value = "2008-06-01")
                       )
                ),
                column(width = 4,
                       box(
                         title = "Country",
                         status = "success",
                         solidHeader = TRUE,
                         width = NULL,
                         "Only for Australian prescription data."
                         ),
                         box(
                           title = "Prescription Type (ATC)", 
                           background = "black",
                           width = NULL,
                           selectInput(
                             inputId =  "atc1", 
                             label = "Select chemical index (level 1):", 
                             choices = unique(PBS$ATC1)),
                           selectInput(
                             inputId =  "atc2", 
                             label = "Select chemical index (level 2):", 
                             choices = unique(PBS$ATC2)
                           )
                         )
                       ))
              
        ),
        tabItem("models",fluidRow(
          box(
            title = "Modeled Prescription Prices",
            status = "success",
            solidHeader = TRUE,
            width = 4,
            numericInput(inputId = "price2",
                         value = 10,
                         step = .5,
                         label = "Type in your monthly prescription price."),
            box(
              title = "Prescription Type (ATC)", 
              background = "black",
              width = NULL,
              selectInput(
                inputId =  "atc5", 
                label = "Select chemical index (level 1):", 
                choices = unique(PBS$ATC1)),
              selectInput(
                inputId =  "atc6", 
                label = "Select chemical index (level 2):", 
                choices = unique(PBS$ATC2)))
      
            )
                ),
          fluidRow(
            box(title=" Models!",
                width=6,
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                            inputId = "modelc",
                            label="Choose between Naive, Seasonal Naive, Mean, Drift, Auto Arima, Holts, and Holts/Winters.",
                            choices = c("Naive", "SeasonalNaive", "Mean", "Drift", "AutoArima","Holts","HoltsWinters")
                ),
                plotOutput("simplot"))
        
                  
            
          ),
          fluidRow(
            box(title = "Manual Arima",
                width=8,
                status="success",
                solidHeader = TRUE,
                collapsible = TRUE,
                numericInput(
                  inputId = "paramp",
                  value = 2,
                  step=1,
                  label = "Desired p of Arima Model"),
                numericInput(
                  inputId = "paramd",
                  value = 0,
                  step=1,
                  label = "Desired d of Arima Model"),
                numericInput(
                  inputId = "paramq",
                  value = 1,
                  step=1,
                  label = "Desired q of Arima Model"),
                plotOutput(("manplot")))
          )),
         
          
          
        tabItem("budget",
          
           fluidRow(
             box(
             title = "Type the amount of time, in months, you want to get a future estimated prescription price.",
             status = "success",
             solidHeader = TRUE,
             width = 4,
             
             numericInput(inputId = "time3",
                          value=1,
                          step=1,label = "Months"
                          )
           ),
           box(
             title = "ATC", 
             background = "black",
             width = 4,
             selectInput(
               inputId =  "atc3", 
               label = "Select chemical index (level 1):", 
               choices = unique(PBS$ATC1)),
             selectInput(
               inputId =  "atc4", 
               label = "Select chemical index (level 2):", 
               choices = unique(PBS$ATC2)
             )
           )), fluidRow(
             box(
             title = "Estimated Prescription Price.",
             status="primary",
             solidHeader = TRUE,
             width = 6,
             "The result below is the predicted prescription price for the number at months specified using an Auto Arima Model.",
             verbatimTextOutput("compared")
              )),
           fluidRow(
             column(width=6,
                    box(title="Value > 0",
                        status="info",
                        solidHeader = TRUE,
                        width = NULL,
                        "Compare to your current prescription price!")
             ))
                
        )
    ))




ui <- dashboardPage(skin='green',
      dashboardHeader(title = "Med-Econ"),
      dashboardSidebar(sidebar),
      dashboardBody(body)
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$savingsplot <- renderPlot({
      scrip<-subset(PBS,as.Date(Month,format = "%d-%m-%Y")>=as.Date(input$Start,format = "%d-%m-%Y")&as.Date(Month,format = "%d-%m-%Y")<=as.Date(input$End,format = "%d-%m-%Y"),select = c(Month,Scripts,Cost))
      if (input$plotty=="seasonality") {scrip%>%gg_season(Cost/Scripts) } else {
        if (input$plotty=="autocorrelation") {scrip %>% ACF(Cost/Scripts, lag_max = 9) %>% autoplot()} else{
        if (input$plotty=="decomposition") {
          scrip %>% filter(!are_duplicated(scrip,index=Month))%>%
            model(STL(Cost/Scripts ~ season(window=9), robust=TRUE)) %>%
            components() %>% autoplot()
          
          
          
        }}}
 
    })
    output$diplot <- renderPlot({
      scrip1<-subset(PBS,as.Date(Month,format = "%d-%m-%Y")>=as.Date(input$Start,format = "%d-%m-%Y")&as.Date(Month,format = "%d-%m-%Y")<=as.Date(input$End,format = "%d-%m-%Y"),select = c(Month,Scripts,Cost,ATC1,ATC2))
      scrip1<-subset(scrip1,ATC1==input$atc1&ATC2==input$atc2,select=c(Month,Scripts,Cost))
      scrip1%>%
        autoplot(Cost/Scripts)
    })
    output$simplot <- renderPlot({
      scrip3<-subset(PBS,as.Date(Month,format = "%d-%m-%Y")>=as.Date(input$Start,format = "%d-%m-%Y")&as.Date(Month,format = "%d-%m-%Y")<=as.Date(input$End,format = "%d-%m-%Y"),select = c(Month,Scripts,Cost,ATC1,ATC2))
      scrip3<-subset(scrip3,ATC1==input$atc5&ATC2==input$atc6,select=c(Month,Scripts,Cost))
      fitm<-scrip3%>%filter(!are_duplicated(scrip3,index=Month))%>%
        model(        Naive=        NAIVE(Cost/Scripts),
                      SeasonalNaive= SNAIVE(Cost/Scripts),
                      Mean=         MEAN(Cost/Scripts),
                      Drift=        RW(Cost/Scripts~drift()),
                      AutoArima=    ARIMA(Cost/Scripts,stepwise = TRUE),
                      Holts =       ETS(Cost/Scripts ~ error("A") +
                                              trend("A") + season("N")),
                      HoltsWinters= ETS(Cost/Scripts ~ error("M") + trend("Ad") + season("M"))
                      )
      fitm%>%select(input$modelc)%>%gg_tsresiduals()
  
    })
    output$manplot <- renderPlot({
      scrip4<-subset(PBS,as.Date(Month,format = "%d-%m-%Y")>=as.Date(input$Start,format = "%d-%m-%Y")&as.Date(Month,format = "%d-%m-%Y")<=as.Date(input$End,format = "%d-%m-%Y"),select = c(Month,Scripts,Cost,ATC1,ATC2))
      scrip4<-subset(scrip4,ATC1==input$atc5&ATC2==input$atc6,select=c(Month,Scripts,Cost))
      fitm2<-scrip4%>%filter(!are_duplicated(scrip4,index=Month))%>%
        model(
                      Arima=    ARIMA(Cost/Scripts~pdq(input$paramp,input$paramd,input$paramq),stepwise = TRUE)
                    
        )
      fitm2%>%gg_tsresiduals()
    })
    output$compared<-renderPrint({ 
    scrip2<-subset(PBS,ATC1==input$atc3&ATC2==input$atc4,select=c(Month,Scripts,Cost))
    fitc<-scrip2 %>% filter(!are_duplicated(scrip2,index=Month))%>%
      model(Arima=        ARIMA(Cost/Scripts,stepwise = TRUE))
      ffitc<-fitc%>%forecast(h=input$time3)
      head(ffitc$.mean[input$time3])
       })
}

# Run the application 
shinyApp(ui = ui, server = server)
