## read Json file 
setwd("C:/Users/Gianni/Dropbox/R_JSON")
load("./R_json.RData")
save.image("./R_json.RData") 
# rm(list=ls())  
# Using the logical operator &, it's possible to load all three packages at once and store the result in a single variable.
lcheck <- require(rjson) & require(dplyr) & require(jsonlite) & require(ggplot2)

# Give the input file name to the function.
result_regioni <- fromJSON("./Data/dpc-covid19-ita-regioni.json", flatten=TRUE)
result_nazione <- fromJSON("./Data/dpc-covid19-ita-andamento-nazionale.json", flatten=TRUE)
# Print the result.
# head(result)
# head(results)
# str(result)


# Convert JSON file to a data frame.
covid19_it <- result_regioni
covid19_it_sum <- result_nazione
#str(covid19_it)
#dim(covid19_it)
#str(covid19_it_sum)

# data frame building
# convert -data- & -denominazione_regione- columns
# delete -lat- & -long- columns
covid19_it     <- as_tibble(covid19_it)
covid19_it_sum <- as_tibble(covid19_it_sum)
class(covid19_it)
covid19_it$denominazione_regione<-as.factor(covid19_it$denominazione_regione)
covid19_it <- covid19_it %>% select(everything(), -lat, -long)

names(covid19_it); 
names(covid19_it_sum);
# new Growth rate columns test
covid19_it_sum$Perc_var_tot_positivi <- (covid19_it_sum$variazione_totale_positivi / 
                                                     lag(covid19_it_sum$variazione_totale_positivi, k=1)-1)

# Growth rate function  ( (x/y-1)*100) 
Perc_covid19 <- function(x, n) {
  y <- 100*(x / lag(x, k=n)-1)
}

# Growth rate function  ( (x/y)*100) 
Perc_covid19bis <- function(x, n) {
  y <- 100*((x / lag(x, k=n))-1)
}

ts_n = 1  ### Lag
## covid19_it_sum$Perc_var_tot_positivi = Perc_covid19(covid19_it_sum$variazione_totale_positivi, ts_n)
covid19_it_sum$Perc_totale_ospedalizzati = Perc_covid19(covid19_it_sum$totale_ospedalizzati, ts_n)
covid19_it_sum$Perc_nuovi_positivi = Perc_covid19bis(covid19_it_sum$nuovi_positivi, ts_n)
covid19_it_sum$Perc_terapia_intensiva = Perc_covid19(covid19_it_sum$terapia_intensiva, ts_n)
covid19_it_sum$Perc_deceduti = Perc_covid19(covid19_it_sum$deceduti, ts_n)
covid19_it_sum$Perc_tot_casi = Perc_covid19(covid19_it_sum$totale_casi, ts_n)
covid19_it_sum$Perc_tamponi = Perc_covid19(covid19_it_sum$tamponi, ts_n)

covid19_it$data     <- as.Date(covid19_it$data) 
covid19_it_sum$data <- as.Date(covid19_it_sum$data)

write.csv2(covid19_it_sum, "./covid19_it_sum.csv")


##
covid19_it_col <- c(names(covid19_it_sum[-(1:2)]) )  # elimino le prime due colonne
covid19_it_col[5]
class(covid19_it_col)

###############################################################################################################
require(RSQLite)
# con  connection
## con <- dbConnect(RSQLite::SQLite(), ":memory:")
con <- dbConnect(RSQLite::SQLite(), 'covid19it.sqlite')
dbListTables(con)
dbWriteTable(conn = con, name = "tbl_covid19it", value = covid19_it)

con_dplyr <- src_sqlite('covid19it.sqlite')

tbl_covid19it <- tbl(con_dplyr, "tbl_coid19it")

tbl_covid19it %>%
  group_by(data, codice_regione, denominazione_regione) %>%
  summarise(totale_casi) %>%
  show_query()

Tot.covid19it <-
  tbl_covid19it %>%
  group_by(data) %>%
  summarise(ricoverati_con_sintomi, terapia_intensiva, isolamento_domiciliare, totale_attualmente_positivi, dimessi_guariti,
            deceduti, totale_casi) %>%
  collect()

# con disconnessione
dbDisconnect(conn=con)
###############################################################################################################

# covid19_it$data     <- as.Date(covid19_it$data) 
# covid19_it_sum$data <- as.Date(covid19_it_sum$data)

str(covid19_it); str(covid19_it_sum)
names(covid19_it_sum)

# regions <- c("Abruzzo", "Lombardia", "Toscana")
covid19_it_regions <- covid19_it %>%
  filter(codice_regione == 3 | codice_regione == 9 | codice_regione == 13 | codice_regione == 19 ) %>%
  select(everything())  
write.csv2(covid19_it_regions, "./covid19_it_regions.csv")
covid19_Lomb<-covid19_it_regions[covid19_it_regions$codice_regione == 3,  ]
##covid19_Tosc<-covid19_it_regions[covid19_it_regions$codice_regione == 9 & covid19_it_regions$data >= "2020-02-28",  ]
covid19_Tosc<-covid19_it_regions[covid19_it_regions$codice_regione == 9,  ]
covid19_Abbr<-covid19_it_regions[covid19_it_regions$codice_regione ==13,  ]
covid19_Sicily<-covid19_it_regions[covid19_it_regions$codice_regione ==19,  ]


ts_n = 1  ### Lag
covid19_Lomb$Perc_var_tot_positivi = Perc_covid19bis(covid19_Lomb$variazione_totale_positivi, ts_n)
covid19_Lomb$Perc_totale_ospedalizzati = Perc_covid19(covid19_Lomb$totale_ospedalizzati, ts_n)
covid19_Lomb$Perc_nuovi_positivi = Perc_covid19bis(covid19_Lomb$nuovi_positivi, ts_n)
covid19_Lomb$Perc_ricov_con_sintomi = Perc_covid19(covid19_Lomb$ricoverati_con_sintomi, ts_n)
covid19_Lomb$Perc_terapia_intensiva = Perc_covid19(covid19_Lomb$terapia_intensiva, ts_n)
covid19_Lomb$Perc_deceduti = Perc_covid19(covid19_Lomb$deceduti, ts_n)
covid19_Lomb$Perc_tot_casi = Perc_covid19(covid19_Lomb$totale_casi, ts_n)
covid19_Lomb$Perc_tamponi = Perc_covid19(covid19_Lomb$tamponi, ts_n)

covid19_Tosc$Perc_var_tot_positivi = Perc_covid19bis(covid19_Tosc$variazione_totale_positivi, ts_n)
covid19_Tosc$Perc_totale_ospedalizzati = Perc_covid19(covid19_Tosc$totale_ospedalizzati, ts_n)
covid19_Tosc$Perc_nuovi_positivi = Perc_covid19bis(covid19_Tosc$nuovi_positivi, ts_n)
covid19_Tosc$Perc_ricov_con_sintomi = Perc_covid19(covid19_Tosc$ricoverati_con_sintomi, ts_n)
covid19_Tosc$Perc_terapia_intensiva = Perc_covid19(covid19_Tosc$terapia_intensiva, ts_n)
covid19_Tosc$Perc_deceduti = Perc_covid19(covid19_Tosc$deceduti, ts_n)
covid19_Tosc$Perc_tot_casi = Perc_covid19(covid19_Tosc$totale_casi, ts_n)
covid19_Tosc$Perc_tamponi = Perc_covid19(covid19_Tosc$tamponi, ts_n)

covid19_Abbr$Perc_var_tot_positivi = Perc_covid19bis(covid19_Abbr$variazione_totale_positivi, ts_n)
covid19_Abbr$Perc_totale_ospedalizzati = Perc_covid19(covid19_Abbr$totale_ospedalizzati, ts_n)
covid19_Abbr$Perc_nuovi_positivi = Perc_covid19bis(covid19_Abbr$nuovi_positivi, ts_n)
covid19_Abbr$Perc_ricov_con_sintomi = Perc_covid19(covid19_Abbr$ricoverati_con_sintomi, ts_n)
covid19_Abbr$Perc_terapia_intensiva = Perc_covid19(covid19_Abbr$terapia_intensiva, ts_n)
covid19_Abbr$Perc_deceduti = Perc_covid19(covid19_Abbr$deceduti, ts_n)
covid19_Abbr$Perc_tot_casi = Perc_covid19(covid19_Abbr$totale_casi, ts_n)
covid19_Abbr$Perc_tamponi = Perc_covid19(covid19_Abbr$tamponi, ts_n)

#require(ggplot2)
png("./images/Covid19_it_4reg.png", 500,500)
covid19_it_regions %>% bind_rows(covid19_it_regions %>% 
                           group_by(data, denominazione_regione) %>% 
                           summarise(totale_casi)) %>% 
  ggplot(aes(x = data, y = totale_casi, colour = denominazione_regione)) + 
#  geom_line(size= 1.3) + geom_point(size= 2) + facet_grid(denominazione_regione ~ ., scale = "free_y") + 
  geom_bar(stat= "identity", fill="white") + geom_point(size= 1.5) + facet_grid(denominazione_regione ~ ., scale = "free_y") + 
  scale_y_continuous(labels=fmt) +
  labs(y = "Total Covid-19 cases", title = "Comparison of Italian Regions", 
       subtitle = "(Note: not all regions shown here)", caption = "Data Source: https://github.com/pcm-dpc/COVID-19") + 
  theme(legend.position = "top", legend.title = element_blank())
dev.off()
## Logaritmic scale
covid19_it_sum %>%
  tidyr::gather(key,value,variazione_totale_positivi, nuovi_positivi, totale_casi, deceduti, tamponi) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(trans='log')+
  labs(y = "Cumulative Daily total Covid-19 cases logaritm scale ", title = "Covid19 in Italy", 
       subtitle = "(Note: total units in log scale)", caption = "gdr >>> Data Sources: https://github.com/pcm-dpc/COVID-19") 

## Standard scale
# Number of patients in intensive care units
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

fmt<-function(x){format(x,nsmall = 0,scientific = FALSE)}

# save plot images
png("./images/Covid19_it_cum.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value,variazione_totale_positivi, nuovi_positivi, totale_casi, deceduti) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  labs(y = "Total Covid-19 cases", title = "Covid19 - Italy", 
       subtitle = "(Note: total units)", caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

# tot_casiColor <- "#69b3a2"
# otherColor <- rgb(0.2, 0.6, 0.9, 1)
# scaleFactor <- max(covid19_it_sum$totale_casi) / max(covid19_it_sum$nuovi_positivi) ###*1/2
## scaleFactor <- max(covid19_it_sum$nuovi_positivi) / max(covid19_it_sum$totale_casi) 

# png("./images/Covid19_it_cumDualY.png", 500,500)
# ggplot(covid19_it_sum, aes(x=data)) +
#   geom_line( aes(y=totale_casi), size=1.5, color=tot_casiColor) +
#   geom_line( aes(y=nuovi_positivi/scaleFactor), size=1.5, color="red") +  scale_y_continuous(
#     # Features of the first axis
#     name = "Total Covid-19 cases",
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~.*scaleFactor, name="nuovi positivi")
#   ) +
# 
#   theme(
#     axis.title.y = element_text(color = tot_casiColor, size=12),
#     axis.title.y.right = element_text(color = "red", size=12),
#     axis.ticks.y.right = element_line(color = "red")
#     
#   ) +
#   ggtitle("Covid19 - Italy")
# 
# dev.off()


png("./images/Covid19_it_newcases.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value,variazione_totale_positivi, nuovi_positivi ) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  labs(y = "New incident confirmed Covid-19 cases", title = "Covid19 - Italy", 
       subtitle = "(Note: total units)", caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_PercTerapiaInt.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value,Perc_terapia_intensiva) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  labs(y = "Growth rate (%) d/d", title = "Covid19 - Italy", 
       subtitle = "(red- %intensive care)", caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_newcasesSmoothed.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value, nuovi_positivi ) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  geom_smooth(method="loess", se=TRUE, col="steelblue") +
  labs(y = "Daily new incident confirmed Covid-19 cases", title = "Covid19 - Italy", 
       subtitle = "(Note: total units) >> smoothed method > Local Polynomial Regression Fitting", 
       caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_varnewcasesSmoothed.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value, variazione_totale_positivi ) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  geom_smooth(method="auto", se=TRUE, col="steelblue") +
  labs(y = "Daily change total positives Covid-19", title = "Covid19 - Italy", 
       subtitle = "(Note: total units) >> smoothed method > Local Polynomial Regression Fitting", 
       caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_tamponi.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value,tamponi ) %>%
  ggplot(aes(x=data, y=value/1000, colour=key)) +
  geom_line(size= 1.3) + 
  labs(y = "Total swabs (thousands)", title = "Covid19 - Italy", 
       subtitle = "(Note: total swabs)", caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_varTamponi.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value, Perc_tamponi, Perc_totale_ospedalizzati ) %>%
  filter(data >= "2020-02-28") %>% 
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  labs(y = "Growth rate (%) d/d", title = "Covid19 - Italy", 
       subtitle = "(red-%new swabs; blue-%tot hospitalized)", 
       caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_Perc.png", 500,500)
covid19_it_sum %>%
  tidyr::gather(key,value, Perc_nuovi_positivi, Perc_tot_casi) %>%
  filter(data >= "2020-02-28") %>% 
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) +
  scale_y_continuous(labels=fmt) +
  labs(y = "Growth rate (%) d/d", x = "date", title = "Covid19 - Italy", 
       subtitle = "(red-%new positives; blue-%change tot positives)", 
       caption = "giader >>>  Data Source: https://github.com/pcm-dpc/COVID-19")
dev.off()

png("./images/Covid19_it_Lombardia.png", 500,500)
covid19_Lomb %>%
  tidyr::gather(key,value,Perc_nuovi_positivi, Perc_tot_casi) %>%
  filter(data >= "2020-02-29") %>% 
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) +
  scale_y_continuous(labels=fmt) +
  labs(y = "Growth rate (%) d/d", x = "date", title = "Covid19 - Lombardia, Italy", 
       subtitle = "(red-%new positives; blue-%change tot positives)", 
       caption = "giader >>>  Source: https://github.com/pcm-dpc/COVID-19")
dev.off()

png("./images/Covid19_it_Toscana.png", 500,500)
covid19_Tosc %>%
  tidyr::gather(key,value,Perc_nuovi_positivi, Perc_tot_casi) %>%
  filter(data >= "2020-03-01") %>% 
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) +
  scale_y_continuous(labels=fmt) +
  labs(y = "Growth rate (%) d/d", x = "date", title = "Covid19 - Tuscany, Italy", 
       subtitle = "(red-%new positives; %tot cases)", 
       caption = "giader >>>  Data Source: https://github.com/pcm-dpc/COVID-19")
dev.off()

png("./images/Covid19_it_Abbruzzo_newcasesSmoothed.png", 500,500)
covid19_Abbr %>%
  tidyr::gather(key,value, nuovi_positivi ) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) + 
  scale_y_continuous(labels=fmt) +
  geom_smooth(method="auto", se=TRUE, col="steelblue") +
  labs(y = "Daily new incident confirmed Covid-19 cases", title = "Covid19 - Italy >> Abbruzzo", 
       subtitle = "(Note: total units) >> smoothed method > Local Polynomial Regression Fitting", 
       caption = "giader >>> Data Source: https://github.com/pcm-dpc/COVID-19") 
dev.off()

png("./images/Covid19_it_Abbruzzo_newPositive.png", 500,500)
covid19_Abbr %>%
  tidyr::gather(key,value,Perc_nuovi_positivi, Perc_tot_casi) %>%
  filter(data >= "2020-03-01") %>% 
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 1.3) +
  scale_y_continuous(labels=fmt) +
  labs(y = "Growth rate (%) d/d", x = "date", title = "Covid19 - Abbruzzo, Italy", 
       subtitle = "(red-%new positives; %tot cases)", 
       caption = "giader >>>  Data Source: https://github.com/pcm-dpc/COVID-19")
dev.off()

##########################################################################################
# R0 calculation 
lcheck2 <- require(EpiEstim) & require(dplyr) & require(jsonlite) & require(ggplot2)

library(incidence)
library(echarts4r)
library(earlyR)
library(epitrix)
library(projections)
##library(EpiEstim)




# custom results plotting function to avoid the ugly
# TableGrob messages returned by the plotting function in the
# EpiEstim package
plot_Ri <- function(estimate_R_obj) {
  p_I <- plot(estimate_R_obj, "incid", add_imported_cases = TRUE)  # plots the incidence
#  p_SI <- plot(estimate_R_obj, "SI")  # plots the serial interval distribution
  p_Ri <- plot(estimate_R_obj, "R")
#  return(gridExtra::grid.arrange(p_I, p_SI, p_Ri, ncol = 1))
  return(gridExtra::grid.arrange(p_I, p_Ri, ncol = 1))
}


# IT_res_parametric_si <- EstimateR(covid19_it_sum$totale_casi, method = "ParametricSI", 
#                                   T.Start = c(5:51), T.End = c(5:51), 
# #                                  T.Start = covid19_it_sum$data[5:51], T.End = Sys.Date(), 
#                                   Mean.SI = 7.5, Std.SI = 3.4)
# #                                   config = make_config(list(mean_si = 7.5, std_si = 3.4)))

IT_res_parametric_si <- estimate_R(covid19_it_sum$totale_casi, method = "parametric_si", 
                                   config=make_config(list(
                                     mean_si = 5.6, std_mean_si = 6,
                                     min_mean_si = 1, max_mean_si = 7,
                                     std_si = 3.5, std_std_si = 3.5,
                                     min_std_si = 1.5, max_std_si = 4
                                   )))
                                  #   ,
                                  #   n1 = 1000, n2 = 1000)))

                                  #, t_start = c(5:51), t_end = c(5:51)
                                  #, mean_si = 7.5, std_si = 3.4)

png("./images/Covid19_it_EstimatedRt.png", 500,500)
plot_Ri(IT_res_parametric_si)
dev.off()


###########################################################################################
#### Bestfit comparison 
bestfit <- geom_smooth(
  method = "lm", 
  se = FALSE, 
  colour = alpha("steelblue", 0.5), 
  size = 2
)

covid19_it_sum %>% bind_rows(covid19_it_sum) %>% 
  ggplot(aes(x = data, y = totale_casi)) + 
  geom_line() + geom_point() + facet_grid(stato ~ ., scale = "free_y") + 
  labs(y = "Cumulative Daily total Covid-19 cases", title = "Covid19 in Italy", 
       subtitle = "(Note: total units)", caption = "Source: https://github.com/pcm-dpc/COVID-19") + 
  theme(legend.position = "top", legend.title = element_blank()) +
  bestfit
covid19_it_sum %>%
  tidyr::gather(key,value,Perc_nuovi_attualmente_positivi, Perc_tot_attualmente_positivi, Perc_tot_casi, ) %>%
  ggplot(aes(x=data, y=value, colour=key)) +
  geom_line(size= 2) +
  labs(y = "Growth rate (%)", x = "date", title = "Covid19 in Italy", 
       caption = "giader >>>  Source: https://github.com/pcm-dpc/COVID-19") +
  bestfit

par(mfrow=c(1,1))
###   TS analysis test
#FastFourierTrasform Periodogram 
require(TSA)
periodogram(covid19_it_sum$totale_casi)   
p<-periodogram(covid19_it_sum$totale_casi)

library(forecast)

#Esempio funzione ts package {stats}
tsCovid19_it = ts(rev(covid19_it_sum$totale_casi),start=c(2020, 2),end=c(2020,3),frequency=mean(top2[,1]))
Acf(tsCovid19_it)
Pacf(tsCovid19_it)
(tsCovid19_it2 <- round(acf(tsCovid19_it, 6, plot=F)$acf[-1], 3))


tseries<-covid19_it_sum$totale_casi %>% msts( seasonal.periods = c(1, 1*7))
#Plot 
tseries  %>% head(  24 *7 *4 ) %>% mstl() %>% autoplot() 



