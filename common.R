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
