#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(tidyr)


ghg = c(70244,  75571,  77988,  85607,  81907,  69892,  81259,  79595,  71481,  67857,  75709,  67836,  62442,  62880,  58728,  55104,  58060,  55341, 56373)
lulucf = c(-18898, -20774, -21844, -22335, -23503, -24430, -30538, -22556, -21282, -33853, -22462, -22734, -25450, -19042, -21343, -18873, -16754, -17142, -9804)
year = 2000:2018

suom = data.table(year, ghg, lulucf)
suom[, net := ghg+lulucf]

suomic <- gather(suom, kaasu, maara, ghg:net)

suomic$sector = c("tot")
suomic$vaihe = c("a")

suomic = as.data.table(suomic)
suomic[(kaasu %in% c("ghg")), ':='(sector =c("ghg"))]
suomic[(kaasu %in% c("net")), ':='(sector =c("net"))]
suomic[(kaasu %in% c("lulucf")), ':='(sector =c("lulucf"))]


#suomic$year = suomic$vuosi

suomic = suomic[, c("year", "maara", "sector")]

suomic$year = as.numeric(suomic$year)

suomic = suomic[year %in% 2000:2018, ]


paas = "#d67c1c"
tav ="#28b8ce"
lul = "#045a1c"

lul = "#76b72a"
tav ="#ffdd00"

har = "#e3e3e3"


ghg = "ghg"
lulucf = "lulucf"
net = "net"


year = 2000:2018
suomig = copy(suomic)

suomig$maara = suomig$maara/1000

modelghg <- loess(maara ~ year, data = subset(copy(suomig),(sector %in% "ghg")))
fitghg <- data.frame(predict(modelghg, se =FALSE))

modellul <- loess(maara ~ year, data = subset(copy(suomig),(sector %in% "lulucf")))
fitlul <- data.frame(predict(modellul, se =FALSE))

fittav = fitghg+fitlul

fitghg = as.data.table(fitghg)
fitghg[, sector := "ghgfit"]
fitghg[, year :=year]

fitlul = as.data.table(fitlul)
fitlul[, sector := "lulucffit"]
fitlul[, year :=year]

fittav = as.data.table(fittav)
fittav[, sector := "tavfit"]
fittav[, year :=year]

names(fitghg)<-c("maara", "sector", "year")
names(fitlul)<-c("maara", "sector", "year")
names(fittav)<-c("maara", "sector", "year")


suomig = rbind(suomig, fitghg)
suomig = rbind(suomig, fitlul)
suomig = rbind(suomig, fittav)
#suomig[sector %in% "tavfit", maara:=0]

gd= suomig[sector%in%"ghgfit",maara]
gb=suomig[sector%in%"lulucffit", maara]

suomig[sector %in% "tavfit", maara:=gd+gb]

#suomig[sector %in% "tavfit", maara:=(suomig[sector%in%"fitght",maara]+suomig[sector%in%"fitlul", maara])]

bla = "white"


year = 2019:2035

ghgs = suomig[sector %in% "ghgfit" & year %in% 2018 ,maara]
lulus = suomig[sector %in% "lulucffit" & year %in% 2018 ,maara]

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
