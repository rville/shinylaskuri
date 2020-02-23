#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)

source("common.R", local = TRUE)


ghgsa = suomig[sector %in% "ghg" & year %in% 2018 ,maara]
lulusa = suomig[sector %in% "lulucf" & year %in% 2018 ,maara]

tavs = suomig[sector %in% "tavfit" & year %in% 2018 ,maara]

ghg1 = data.frame(year)
ghg1$sector = "ghg"

lul1 = data.frame(year)
lul1$sector = "lulucf"

tav1 = data.frame(year)
tav1$sector = "tav"


suomix = rbind(ghg1, lul1, tav1)



#tavu <- seq(tavs, input$num, length.out=18)
tavu <- seq(tavs, 0, length.out=18)

tavk =tavu[-1]

suomix$maara = 0
suomix=as.data.table(suomix)

suomix[sector %in% "tav", maara:=tavk]


server <- function(input,output, session){

 
  
  observeEvent(input$lahto, {
    if (input$lahto == "nor")
    {
      updateSliderInput(
        session = session,
        inputId = 'num2',
        value = ghgs
      ) # updateSliderInput
    }#if
    
    
  })
  observeEvent(input$lahto, {
    if (input$lahto == "nol")
    {
      updateSliderInput(
        session = session,
        inputId = 'num2',
        value = ghgsa
      ) # updateSliderInput
    }#if
    
    
  })
  
  
  observeEvent(input$lahto, {
    if (input$lahto == "nor")
    {
      updateSliderInput(
        session = session,
        inputId = 'num3',
        value = c(-1*lulus)
      ) # updateSliderInput
    }#if
    
    
  })
  
  observeEvent(input$lahto, {
    if (input$lahto == "nol")
    {
      updateSliderInput(
        session = session,
        inputId = 'num3',
        value = -1*lulusa
      ) # updateSliderInput
    }#if
    
    
  }) 
  

  
  
  dat1 <- reactive({
    
    minv <- input$num4[1] 
    maxv <- input$num4[2] 
    test <- suomix[sector %in% "ghg",]
    paa <- seq(input$num2,input$num, length.out=maxv-minv+1)
    paak = paa[-1]
    test =  test[year %in% (minv+1):maxv, ]
    test[sector %in% "ghg", maara:=paak]

    
    test
  })
  
  dat2 <- reactive({
    
    minv <- input$num4[1] 
    maxv <- input$num4[2] 
    test <- suomix[sector %in% "lulucf",]
    nie <- seq(-1*input$num3,-1*input$num, length.out=maxv-minv+1)
    niek = nie[-1]
    test =  test[year %in% (minv+1):maxv, ]
  
    test[sector %in% "lulucf", maara:=niek]
    test = as.data.frame(test)
    test
  })
  
  dat4 <- reactive({
    
    minv <- input$num4[1] 
    maxv <- input$num4[2] 
    test <- suomix[sector %in% "tav",]
    paa <- seq(input$num2,input$num, length.out=maxv-minv+1)
    paak = paa[-1]
    nie <- seq(-1*input$num3,-1*input$num, length.out=maxv-minv+1)
    niek = nie[-1] 
  net = paak + niek 
  sumnet = sum(net)
  test =  test[year %in% (minv+1):maxv, ]
  test[sector %in% "tav", maara:=net]
  test[sector %in% "tav", sumnet:=sumnet]
  test[sector %in% "tav",minv:=minv]
  test[sector %in% "tav",maxv:=maxv]
  
    test = as.data.frame(test)
    test
  })
  
  dat3 <- reactive({
    test <- suomig
    test = as.data.frame(test)
    test
  })
  
  dat5 <- reactive({
    test <- suomix
    
    test = as.data.frame(test)
    test
  })
  
  bla = "black"
  "white" = "white"
  
  dat6 <- reactive({
    
    minv <- input$num4[1] 
    maxv <- input$num4[2] 
    test <- suomix[sector %in% "ghg",]
    num<- input$num
    num2<- input$num2
    num3<- input$num3
    
    test =  test[year %in% maxv, ]
    test[sector %in% "ghg", num:=num]
    test[sector %in% "ghg", num2:=num2]
    test[sector %in% "ghg", num3:=num3]
    test[sector %in% "ghg",minv:=minv]
    test[sector %in% "ghg",maxv:=maxv]
    
    
    test
  })
  
  #format(round(maara, 1), nsmall =1, decimal.mark=","))
  
  output$plot2<-renderPlot({
    if (input$somevalue== FALSE)  {
     ggplot(NULL) +
      
   
        
        geom_text(data=subset(dat6()), aes(y=num, x=2040, label= format(round(num, 1), nsmall =1, decimal.mark=",")), size=10,  color=paas, hjust=1) +
        geom_text(data=subset(dat6()), aes(y=-num, x=2040, label= format(round(-num, 1), nsmall =1, decimal.mark=",")), size=10,  color=lul, hjust=1) +
        
        geom_text(data=subset(dat6()), aes(y=num2, x=2040, label= format(round(num2, 1), nsmall =1, decimal.mark=",")), size=10,  color=paas, hjust=1) +
        geom_text(data=subset(dat6()), aes(y=-num3, x=2040, label= format(round(-num3, 1), nsmall =1, decimal.mark=",")), size=10,  color=lul, hjust=1) +
        
         
      geom_col(data=dat1(),aes(y=maara, x=year), fill=paas, alpha=0.9, width=.9, color="white") +
      geom_col(data=dat2(), aes(y=maara, x=year), fill=lul, alpha=0.9, width=.9, color="white") +
      geom_col(data=dat4(), aes(y=maara, x=year+.1), fill=tav, alpha=0.9, width=.7, color="black") +
      
      
      geom_col(data=subset(dat3(),((sector %in% "ghg"))), aes(y=maara, x=year), fill=paas, alpha=0.9, width=.9, color="white") +
      geom_col(data=subset(dat3(),((sector %in% "net"))), aes(y=maara, x=year+.1), fill=tav, alpha=0.9, width=.7, color="black") +
      geom_col(data=subset(dat3(),((sector %in% "lulucf"))), aes(y=maara, x=year), fill=lul, alpha=0.9, width=.9, color="white") +
      
      geom_text(data=dat1(), aes(y=maara+1, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=9, angle=90, color=paas, hjust=0) +
      geom_text(data=dat2(), aes(y=maara-1.3, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=9, angle=90, color=lul, hjust=1) +
      geom_text(data=subset(dat4()), aes(y=maara+1, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=8, angle=90, color=tav, hjust=0) +
      
  
      
        geom_text(data=subset(dat3(),((sector %in% "net"))), aes(y=maara+5, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=7, angle=90, color=tav) +
        geom_text(data=subset(dat3(),((sector %in% "lulucf"))), aes(y=maara-5, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=8, angle=90, color=lul) +
        geom_text(data=subset(dat3(),((sector %in% "ghg"))), aes(y=maara+5, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=8, angle=90, color=paas) +
        
        geom_text(data=subset(dat4(),((year %in% maxv))), aes(y=70, x=2020, 
                label=paste0("Suomen lämmitysvaikutus \nvuosilta ", minv+1,"-",maxv,": ", format(round(sumnet, 1), nsmall =1, decimal.mark=","), " Mt CO2e")), size=9, color=tav, hjust=0) +
        
        
    geom_label(data=subset(dat3(),((sector %in% "net"))), aes(y=65, x=2001, label="Kokonaispäästöt "), color=paas, fill=bla, size=11, hjust=0) +
      geom_label(data=subset(dat3(),((sector %in% "net"))), aes(y=15, x=2001, label="Nettopäästöt "), color=tav, fill=bla, size=11, hjust=0) +
      geom_label(data=subset(dat3(),((sector %in% "net"))), aes(y=-10, x=2001, label="LULUCF-nielu "), color=lul, fill=bla, size=11, hjust=0) +
      
      geom_text(data=subset(dat3(),((sector %in% "net"))), aes(y=83, x=2017.5, label="Menneet\npäästöt"), color="white", size=13, hjust=1, lineheight=.8) +
      
      geom_text(data=subset(dat3(),((sector %in% "net"))), aes(y=83, x=2019.5, label="Tulevat\npäästöt(päästöbudjetit)"), color="white", size=13, hjust=0, lineheight=.8) +
      
      geom_text(data=dat3(), aes(y=96, x=year, label=c(year)), size=10, angle=90, color="white") +
      geom_text(data=dat5(), aes(y=96, x=year, label=c(year)), size=10, angle=90, color="white") +
      
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv, y=num, xend=2038, yend=num), size=.5, color="white", linetype="dashed") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv, y=-num, xend=2038, yend=-num), size=.5, color="white", linetype="dashed") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv, y=num2, xend=2038, yend=num2), size=.5, color="white", linetype="dashed") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv, y=-num3, xend=2038, yend=-num3), size=.5, color="white", linetype="dashed") + 
        
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv-.5, y=num, xend=maxv+.5, yend=num), size=2, color="white", linetype="solid") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv-.5, y=-num, xend=maxv+.5, yend=-num), size=2, color="white", linetype="solid") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv-.5, y=num2, xend=minv+.5, yend=num2), size=2, color="white", linetype="solid") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv-.5, y=-num3, xend=minv+.5, yend=-num3), size=2, color="white", linetype="solid") +
   
        labs(x = "vuosi", y="miljoonaa CO2-ekvivalenttitonnia") +
        
      geom_segment(data=subset(dat3()),mapping=aes(x=2018.5, y=-45, xend=2018.5, yend=99), size=2, color="white", linetype="dashed") + 
      

      scale_y_continuous(breaks = c(-40, -30, -20, -10, 0, 10,20,30,40,50,60,70,80,90)) +      
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035)) +      
      
      coord_cartesian(xlim=c(2000, 2039), ylim = c(-41, 99)) +
      
      theme(strip.text.x = element_text(size=25, vjust=0),
            plot.background = element_rect(fill = hsv(200/360,.1,.3)),
            panel.background = element_rect(fill = hsv(200/360,.1,.3), colour = hsv(200/360,.4,.5), size = 1, linetype = "solid"),
            strip.text.y = element_blank(),
            strip.background =element_rect(fill=hsv(250/360,.2,.9)),
            strip.text = element_text(size=25,colour = 'white', hjust=0.1),
            axis.title.x=element_text(size=25, colour = "white"),
            axis.title.y=element_text(size=25, colour = "white"),
            axis.text.x=element_text(size=25, colour = "white"),
            axis.text.y=element_text(size=25, colour = "white"),
            
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  
    else {
           ggplot(NULL) +
      
   
        
        geom_text(data=subset(dat6()), aes(y=num, x=2040, label= format(round(num, 1), nsmall =1, decimal.mark=",")), size=10,  color=paas, hjust=1) +
        geom_text(data=subset(dat6()), aes(y=-num, x=2040, label= format(round(-num, 1), nsmall =1, decimal.mark=",")), size=10,  color=lul, hjust=1) +
        
        geom_text(data=subset(dat6()), aes(y=num2, x=2040, label= format(round(num2, 1), nsmall =1, decimal.mark=",")), size=10,  color=paas, hjust=1) +
        geom_text(data=subset(dat6()), aes(y=-num3, x=2040, label= format(round(-num3, 1), nsmall =1, decimal.mark=",")), size=10,  color=lul, hjust=1) +
        
         
      geom_col(data=dat1(),aes(y=maara, x=year), fill=paas, alpha=0.9, width=.9, color="white") +
      geom_col(data=dat2(), aes(y=maara, x=year), fill=lul, alpha=0.9, width=.9, color="white") +
      geom_col(data=dat4(), aes(y=maara, x=year+.1), fill=tav, alpha=0.9, width=.7, color="black") +
      
      
      geom_col(data=subset(dat3(),((sector %in% "ghg"))), aes(y=maara, x=year), fill=paas, alpha=0.9, width=.9, color="white") +
      geom_col(data=subset(dat3(),((sector %in% "net"))), aes(y=maara, x=year+.1), fill=tav, alpha=0.9, width=.7, color="black") +
      geom_col(data=subset(dat3(),((sector %in% "lulucf"))), aes(y=maara, x=year), fill=lul, alpha=0.9, width=.9, color="white") +
      
      geom_text(data=dat1(), aes(y=maara+1, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=9, angle=90, color=paas, hjust=0) +
      geom_text(data=dat2(), aes(y=maara-1.3, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=9, angle=90, color=lul, hjust=1) +
      geom_text(data=subset(dat4()), aes(y=maara+1, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=8, angle=90, color=tav, hjust=0) +
      
  
      
        geom_text(data=subset(dat3(),((sector %in% "net"))), aes(y=maara+5, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=7, angle=90, color=tav) +
        geom_text(data=subset(dat3(),((sector %in% "lulucf"))), aes(y=maara-5, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=8, angle=90, color=lul) +
        geom_text(data=subset(dat3(),((sector %in% "ghg"))), aes(y=maara+5, x=year, label=format(round(maara, 1), nsmall =1, decimal.mark=",")), size=8, angle=90, color=paas) +
        
        geom_text(data=subset(dat4(),((year %in% maxv))), aes(y=70, x=2020, 
                label=paste0("Suomen lämmitysvaikutus \nvuosilta ", minv+1,"-",maxv,": ", format(round(sumnet, 1), nsmall =1, decimal.mark=","), " Mt CO2e")), size=9, color=tav, hjust=0) +
        
        
    geom_label(data=subset(dat3(),((sector %in% "net"))), aes(y=65, x=2001, label="Kokonaispäästöt "), color=paas, fill=bla, size=11, hjust=0) +
      geom_label(data=subset(dat3(),((sector %in% "net"))), aes(y=15, x=2001, label="Nettopäästöt "), color=tav, fill=bla, size=11, hjust=0) +
      geom_label(data=subset(dat3(),((sector %in% "net"))), aes(y=-10, x=2001, label="LULUCF-nielu "), color=lul, fill=bla, size=11, hjust=0) +
      
      geom_text(data=subset(dat3(),((sector %in% "net"))), aes(y=83, x=2017.5, label="Menneet\npäästöt"), color="white", size=13, hjust=1, lineheight=.8) +
      
      geom_text(data=subset(dat3(),((sector %in% "net"))), aes(y=83, x=2019.5, label="Tulevat\npäästöt(päästöbudjetit)"), color="white", size=13, hjust=0, lineheight=.8) +
      
      geom_text(data=dat3(), aes(y=96, x=year, label=c(year)), size=10, angle=90, color="white") +
      geom_text(data=dat5(), aes(y=96, x=year, label=c(year)), size=10, angle=90, color="white") +
      
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv, y=num, xend=2038, yend=num), size=.5, color="white", linetype="dashed") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv, y=-num, xend=2038, yend=-num), size=.5, color="white", linetype="dashed") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv, y=num2, xend=2038, yend=num2), size=.5, color="white", linetype="dashed") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv, y=-num3, xend=2038, yend=-num3), size=.5, color="white", linetype="dashed") + 
        
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv-.5, y=num, xend=maxv+.5, yend=num), size=2, color="white", linetype="solid") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=maxv-.5, y=-num, xend=maxv+.5, yend=-num), size=2, color="white", linetype="solid") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv-.5, y=num2, xend=minv+.5, yend=num2), size=2, color="white", linetype="solid") + 
        geom_segment(data=subset(dat6()),mapping=aes(x=minv-.5, y=-num3, xend=minv+.5, yend=-num3), size=2, color="white", linetype="solid") +
   
        labs(x = "vuosi", y="miljoonaa CO2-ekvivalenttitonnia") +
        
      geom_segment(data=subset(dat3()),mapping=aes(x=2018.5, y=-45, xend=2018.5, yend=99), size=2, color="white", linetype="dashed") + 
             geom_point(data=subset(dat3(),((sector %in% "ghgfit"))), aes(y=maara, x=year),shape = 21, colour = bla, fill = paas, size = 8, stroke = 2, alpha=0.9) + 
       geom_point(data=subset(dat3(),((sector %in% "lulucffit"))), aes(y=maara, x=year),shape = 21, colour = bla, fill = lul, size = 8, stroke = 2, alpha=0.9) +
       geom_point(data=subset(dat3(),((sector %in% "tavfit"))), aes(y=maara, x=year),shape = 21, colour = bla, fill = tav, size = 8, stroke = 2, alpha=0.9) +
       
      

      scale_y_continuous(breaks = c(-40, -30, -20, -10, 0, 10,20,30,40,50,60,70,80,90)) +      
      scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2025, 2030, 2035)) +      
      
      coord_cartesian(xlim=c(2000, 2039), ylim = c(-41, 99)) +
      
      theme(strip.text.x = element_text(size=25, vjust=0),
            plot.background = element_rect(fill = hsv(200/360,.1,.3)),
            panel.background = element_rect(fill = hsv(200/360,.1,.3), colour = hsv(200/360,.4,.5), size = 1, linetype = "solid"),
            strip.text.y = element_blank(),
            strip.background =element_rect(fill=hsv(250/360,.2,.9)),
            strip.text = element_text(size=25,colour = 'white', hjust=0.1),
            axis.title.x=element_text(size=25, colour = "white"),
            axis.title.y=element_text(size=25, colour = "white"),
            axis.text.x=element_text(size=25, colour = "white"),
            axis.text.y=element_text(size=25, colour = "white"),
            
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        
    

    }


  }  ,height =900,width = 1200)

}

# Run the application 
shinyServer(server)

# Define UI for application that draws a histogram
