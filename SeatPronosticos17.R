# Librerías ---------------------------------------------------------------
library(magrittr)
c("dplyr","tidyr","lubridate","tseries", "astsa","forecast","ggplot2", 
  "lattice", "dygraphs","XLConnect", "palettetown", "xts", "Rfacebook") %>%  
  sapply(require, character.only=T)

####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

# Conexión a facebook -----------------------------------------------------
fb_oauth <- fbOAuth(app_id="1611650985792093", 
                    app_secret="35ff99b85e4bf364b35faeb0a850dbd4", 
                    extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("~/fb_oauth")
idFB_seat = 113144262054871


# Facebook ----------------------------------------------------------------

## Crecimiento
fb = read.csv("~/local/Sandra_TimeSeries/2016.csv") %>% 
  mutate(Date = as.Date(Date))
nombresFB<- names(fb)

l1 = loadWorkbook("~/local/TimeSeries17/octubre2016-ene2017.xlsx")  
l1 = l1 %>%  readWorksheet(sheet=1, header=T) %>%  
  mutate(Fecha = as.Date(Fecha), mes =month(Fecha), anio= year(Fecha))


names(l1)<- nombresFB
fb = rbind(fb, l1) %>% distinct(Date, .keep_all=T) 
TS = xts(fb$Daily.Daily.count.of.fans.online, as.Date(fb$Date)) %>%
  .["2016-02-01/"] %>%  na.omit()


modelo1 = TS %>%  na.omit %>% 
  coredata %>%  
  ts(frequency = 7) %>% 
  Arima(order=c(1,0,1), seasonal = c(1,1,1), include.drift = T) %>% 
  forecast(h = 125) 

modelo2 = TS %>%  na.omit %>% 
  coredata %>%  
  ts(frequency = 7) %>% 
  Arima(order=c(1,0,1), seasonal = c(1,1,1)) %>% 
  forecast(h = 125) 


ultimo = index(xts::last(TS)) %>% as.POSIXct()
pronostico1 = xts(modelo1$mean, seq.POSIXt(ultimo +1, length.out = 125, 
                                           by= "day"))
pronostico2 = xts(modelo2$mean, seq.POSIXt(ultimo +1, length.out = 125, 
                                           by= "day"))

grafo1 = cbind(TS %>%  na.omit(), pronostico1)
names(grafo1) <- c("Crecimiento observado de Facebook", "Estimación")

dygraph(grafo1) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut= T) %>% 
  dyAxis("y", label="Fans online") %>% 
  dyOptions(drawGrid = F) %>% 
  dyEvent(ultimo, label= "Inician Pronósticos")


grafo2 = cbind(TS %>%  na.omit(), pronostico2)
names(grafo2) <- c("Crecimiento observado de Facebook", "Estimación")

dygraph(grafo2) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut= T) %>% 
  dyAxis("y", label="Fans online") %>% 
  dyOptions(drawGrid = F) %>% 
  dyEvent(ultimo, label= "Inician Pronósticos")

crecimiento_m1 = (1303588 - 1247625)/ 1247625*100
crecimiento_m2 = (1277393 - 1247625)/ 1247625*100
paste("Sandy, tu crecimiento es de ", crecimiento_m1, "%")
paste("Sandy, tu crecimiento es de ", crecimiento_m2, "%")


## Engagement
TotalF = loadWorkbook(
  "~/local/TimeSeries17/FB - Fans - SEAT México - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=2, startRow=2) %>%  select(Date, Fans)

INT1 = getPage(idFB_seat, since="2013-01-01", until="2013-06-30",
        token=fb_oauth, reactions = T,n= 1000)

INT2 = getPage(idFB_seat, since="2013-07-01", until="2013-12-31",
        token=fb_oauth, reactions = T,n= 1500)

INT3 = getPage(idFB_seat, since="2014-01-01", until="2014-06-30",
        token=fb_oauth, reactions = T,n= 1500)

INT4 = getPage(idFB_seat, since="2014-07-01", until="2014-12-31",
        token=fb_oauth, reactions = T,n= 1500)

INT5 = getPage(idFB_seat, since="2015-01-01", until="2015-06-30",
        token=fb_oauth, reactions = T,n= 1500)

INT6 = getPage(idFB_seat, since="2015-07-01", until="2015-12-31",
        token=fb_oauth, reactions = T,n= 1500)

INT7 = getPage(idFB_seat, since="2016-01-01", until="2016-06-30",
        token=fb_oauth, reactions = T,n= 1500)

INT8 = getPage(idFB_seat, since="2016-07-01", until="2016-12-31",
        token=fb_oauth, reactions = T,n= 1500)

INT9 = getPage(idFB_seat, since="2017-01-01", until="2017-01-27",
        token=fb_oauth, reactions = T,n= 1500)

InteracionSeat = rbind(INT1,INT2, INT3, INT4, INT5, INT6,INT7,INT8, INT9)

fechas = seq(ymd('2013-01-01'),ymd('2016-12-01'), by = 'months')

summas = c(443306,410142, 364830,359057,491580,466709,453217,369618,
           308420,263196,184779,94236,74403,74767,129481,144873,160474,
           177282,228588,176236,177472,255553,94847,77876,
           121251,162493,92327,81270,66835,75607,81879,77246,
           50976,67341,64934,63627,69444,70018, 63454,57732, 57065, 64103,
           66721, 55491, 53966, 54594,52520,49986)

fanstotales = c(356210, 444359, 524536, 614642, 
                694754, 766517, 809811, 834032, 878055, 960524, 993974,
                1067261, 1070950, 1124330, 1170256, 1225499, 1266445, 
                1303574,1362823, 1429278, 1477808,1559615, 1572561,
                1576928, 1580582, 1590939, 1591753,1551294, 1556634, 
                1559181, 1558787,1560633,1562967,1564392,1566641,
                1572749,1574437,1574426,1576277, 1551294, 1556634, 1559181,
                1558787,1560633, 1562967,1599785,1604375,1611016)


suma = ((summas)/fanstotales)*100
engage = xts(suma,fechas)
Tiempo = seq_along(engage)

expFct <- function(x, beta1, beta2, beta3) {
  exp(-beta1 * x)/(beta2 + beta3 * x)
  }


nolineal <- nls(coredata(engage)~expFct(x = Tiempo ,beta1,beta2,beta3),
                start = list(beta1 = 0.1, 
                             beta2 = 0.02,
                             beta3 = 0.03))

TIME_PRED = 1:(length(Tiempo) + 6)

pred.en = xts(predict(nolineal, newdata = list(Tiempo = TIME_PRED)),
              seq(ymd(20130101), by = "month", 
                  length.out = length(engage) + 6))

engagement = cbind(engage, pred.en)
names(engagement)<-c("Observado","No lineal")
tiempoe = rep(time(engagement), 2)
grafoEn = cbind(engage %>%  na.omit(), pred.en )
names(grafoEn) <- c("Engagement", "Estimación (No lineal)")


# Twitter -----------------------------------------------------------------
##Crecimiento
TW = loadWorkbook(
  "~/local/TimeSeries17/TW - Followers - SEAT México - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=2,startRow = 2) %>%  select(Date, Followers) %>% 
  mutate(Date = as.Date(Date))
TSTW = xts(TW[,-1], TW[,1])
fechas_tw = seq(ymd(20120824), by="day", length.out = length(TSTW[,1]))
seguidores_t = cbind(seguidores= TSTW,
                     diferenciado= diff(TSTW),
                     cambio_seguidores = diff.xts(log(TSTW)))
ultimo_tw = index(xts::last(TSTW))
TIME_tw = seq_along(TSTW[,1])
seguidores_t = data.frame(fechas_tw, seguidores = 
                            seguidores_t$seguidores %>%  as.numeric())
numeros = seguidores_t$seguidores

logistico_tw = 
  nls(numeros ~K*P0*exp(R*TIME_tw)/(K+P0 *(exp(R*TIME_tw)-1)),
      start = list(P0 = min(seguidores_t$seguidores, na.rm = T),
                   K  = max(seguidores_t$seguidores, na.rm = T),
                   R = 0.01
      )
  ) 

TIME_PTW = 1:(length(TIME_tw) + 125)
DATE_Ptw = seq(ymd(20120824), by="day", length.out = max(TIME_PTW))
pred.tw = data.frame(fechas_tw = DATE_Ptw,
                     pronostico = predict(logistico_tw, 
                                          newdata = list(TIME_tw = TIME_PTW))) 

twitter_logis = pred.tw %>%  left_join(seguidores_t, by="fechas_tw")
twitter_logis = xts(twitter_logis[,-1], twitter_logis[,1])
names(twitter_logis) <- c("Pronóstico de  seguidores", "Seguidores")



##Menciones
Mentions = loadWorkbook(
  "~/local/TimeSeries17/TW - Engagement - SEAT México - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=4,startRow = 2) %>%  select(Date, Mentions) %>% 
  mutate(Date = as.Date(Date))

TSMentions = xts(Mentions[,-1], Mentions[,1])

modeloM = TSMentions %>%  na.omit %>%  coredata %>%  ts %>% 
  Arima(order = c(3,1,4)) %>%  
  forecast(125)

# TSMentions %>%  na.omit %>%  coredata %>%  auto.arima() 
# 
# TSMentions %>%  na.omit %>%  coredata %>%  ts %>% 
#   Arima(order = c(1,1,1)) %>%  
#   forecast(125) %>% plot


indiceM = index(xts::last(TSMentions)) %>% as.POSIXct()
pronosticoM = xts(modeloM$mean, seq.POSIXt(indiceM +1, length.out = 125, 
                                           by= "day"))
grafM = cbind(TSMentions %>%  na.omit(), pronosticoM)
names(grafM) <- c('Observado "Menciones"', "Estimación")
grafM %>%  dygraph()


##RetTweets
RTW = loadWorkbook(
  "~/local/TimeSeries17/TW - Engagement - SEAT México - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=3,startRow = 2) %>%  select(Date, Retweets) %>% 
  mutate(Date = as.Date(Date))
TSRTW = xts(RTW[,-1], RTW[,1]) %>%  na.omit()
TSRTW %>% coredata %>%  ts %>% auto.arima() 
TSRTW %>% coredata %>%  ts %>% Arima(order=c(1,0,3)) %>% forecast(125)
(21.20142-27)/27*100


# Instagram ---------------------------------------------------------------
#Crecimiento
IN = loadWorkbook(
  "~/local/TimeSeries17/IG - Followers - SEAT México - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=2,startRow = 2) %>% select(Date, Followers) %>% 
  mutate(Date= as.Date(Date)) 
TSIN = xts(IN[,-1], IN[,1])

dic_2015 = TSIN["2015-12-31/"] 
tmp = dic_2015[,1] %>%  na.omit()  %>% as.numeric
tiempo = seq_along(tmp)

nlm = nls(
  tmp ~K *P0 *exp(R*tiempo) / (K + P0 *(exp(R*tiempo)-1)),
  start = list(
    P0 = min(tmp, na.rm=T),
    K = max(tmp, na.rm = T),
    R =0.1
  ),
  trace = T
)

time_log = 1:(length(tiempo) + 125)
date_log = seq(ymd(20151231), by="day", length.out = max(time_log))
pred.Ins = data.frame(fechas_ins = date_log,
                      pronostico = predict(nlm, newdata = list(tiempo = time_log)))

insta_logis = pred.Ins %>%  left_join(XIN, by = "fechas_ins")

insta_logis = xts(insta_logis[, -1], insta_logis[, 1])
insta_logis = insta_logis[,-2]

names(insta_logis) = c("Pronóstico de seguidores","Seguidores")

insta_logisgg = cbind(fechas = time(insta_logis), insta_logis %>%
                        data.frame())
names(insta_logisgg)<-c("fechas", "pronostico", "seguidores")
insta_logisgg = insta_logisgg  %>%  gather(tipo, valor, -fechas)



#Interacción
INIG = loadWorkbook(
  "~/local/TimeSeries17/IG - Engagement - SEAT México - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=3,startRow = 2) %>%  select(Date, Total.Interactions) %>% 
  mutate(Date = as.Date(Date))
TSINIG = xts(INIG[,-1], INIG[,1]) %>%  na.omit

# YouTube -----------------------------------------------------------------
#Crecimiento
YT = loadWorkbook(
  "~/local/TimeSeries17/YT - Subscribers - SEATdeMexico - 2017-01-27.xlsx") %>% 
  readWorksheet(sheet=2,startRow = 2) %>%  select(Date, Subscribers) %>% 
  mutate(Date = as.Date(Date))
TSYT = xts(YT[,-1],YT[,1])
TSYT %>%  plot

# SeatWeb -----------------------------------------------------------------
fechaSeat = seq.POSIXt(from= as.POSIXct("2013-12-30"), 
                       to=as.POSIXct("2017-01-27" ), by="days")

seatWeb = read.csv("~/local/TimeSeries17/seatWebPage.csv") %>%  
  mutate(TimeRate = gsub(",", "", TimeRate), 
         TimeRate = as.numeric(TimeRate)) %>%  
  mutate(PageViews = gsub(",", "", PageViews),
         PageViews = as.numeric(PageViews)) %>%  
  mutate(DealerSearch = gsub(",", "", DealerSearch),
         DealerSearch = as.numeric(DealerSearch)) %>%  
  mutate(Visitas = gsub(",", "", Visitas),
         Visitas = as.numeric(Visitas)) %>%  
  mutate(CarConfiguration = gsub(",", "", CarConfiguration),
         CarConfiguration = as.numeric(CarConfiguration)) %>%  
  mutate(Return.Rate = gsub(",", "", Return.Rate),
         Return.Rate = as.numeric(Return.Rate)) %>% cbind(fechaSeat)  %>% 
  select(-Date)
  
xts(seatWeb[,-7], as.Date(seatWeb[,7]) ) %>%.["2015-07-01/"]  %>%
  dygraph()

# Visitas -----------------------------------------------------------------
Visitas = seatWeb %>%  select(fechaSeat, Visitas )
TSVisita = xts(Visitas[,-1], as.Date(Visitas[,1])) %>% .["2015-07-01/"]


# Car Configuration -------------------------------------------------------
CarConfiguration = seatWeb %>%  select(fechaSeat, CarConfiguration )
TSCar = xts(CarConfiguration[,-1], as.Date(CarConfiguration[,1])) %>%
  .["2015-07-01/"]


# Return Rate -------------------------------------------------------------
ReturnRate = seatWeb %>%  select(fechaSeat, Return.Rate)
TSReturn = xts(ReturnRate[,-1], as.Date(ReturnRate[,1])) %>%
  .["2015-07-01/"]



# TimeRate ----------------------------------------------------------------
TimeRate = seatWeb %>%  select(fechaSeat, TimeRate)
TSTime = xts(TimeRate[,-1], as.Date(TimeRate[,1])) %>%
  .["2015-07-01/"]


# Dealer Search -----------------------------------------------------------
Dealer = seatWeb %>%  select(fechaSeat, DealerSearch)
TSDealer = xts(Dealer[,-1], as.Date(Dealer[,1])) %>%
  .["2015-10-01/"]


# Page Views --------------------------------------------------------------
PageViews = seatWeb %>%  select(fechaSeat, PageViews)
TSPage = xts(PageViews[,-1], as.Date(PageViews[,1])) %>%
  .["2015-07-01/"]


