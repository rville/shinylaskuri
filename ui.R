#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("common.R", local = TRUE)


ui <- fluidPage(


    fluidRow (
# splitLayout(
 #  verticalLayout(

   column( 2, titlePanel(title=h3("2035-hiilineutraalius", align="center")),


    sliderInput("num", label="Valitse päästöt/nielu hiilineutraaliusvuonna, Mt CO2-ekv. :",min = 0, max = 40,step=.1,value=c(20)),

    sliderInput("num2", label="Valitse paastojen vertailuutaso, Mt CO2-ekv. :",min = 0, max = 60,step=.1,value=c(ghgs)),
   sliderInput("num3", label="Valitse nielun vertailuutaso, Mt CO2-ekv. :",min = 0, max = 40,step=.1,value=c(-1*lulus)),
# splitLayout(
  radioButtons("lahto", "Aseta päästöjen ja nielujen vertailutaso lähtövuodelle:",
               c("Käytä vuoden 2018 trendiarvoja päästöjen ja nielujen vertailutasoiksi" = "nor",
                 "Käytä vuoden 2018 toteutuneita arvoja päästöjen ja nielujen vertailutasoiksi" = "nol"
               )),



sliderInput("num4", label ="Valitse vertailuvuosi, josta eteenpäin hiilibudjetit lasketaan sekä hiilineutraaliusvuosi", min = 2018, max = 2035, value = c(2018, 2035)),



   checkboxInput("somevalue", "Näytä menneiden päästöjen trendikäyrät", FALSE)),


column(8,
#        div( style = "position:relative", plotOutput("plot2", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),uiOutput("hover_info")  , width = "60%")

   plotOutput("plot2" , height= 900, width=1200)

    )

  ))


# Run the application
shinyUI(ui)

# Define UI for application that draws a histogram
