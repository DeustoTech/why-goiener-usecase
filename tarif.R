library(data.table)
library(PMCMRplus)
library(stargazer)
library(extrafont)
font_install('fontcm')
loadfonts()

#options(latexcmd='pdflatex'); options(dviExtension='pdf');options(xdvicmd='okular')

FILE        <- "output.txt"; cat("",file=FILE)
THRESHOLD_E <- 5       ## THRESHOLD in % to decide if a client is losing, neutral or winning money with the change of tariff
THRESHOLD_P <- 25      ## THRESHOLD in % to decide if a client has the correct power contracted
TOL         <- 1e-4    ## TOLerance to test if we are splitting consumption on the different periods correctly

p20         <- 42.043
p21         <- 44.445
p20td1      <- 32.877
p20td2      <- 3.4370
            
e20a        <- 0.133628
e20dha1     <- 0.149215
e20dha2     <- 0.081109
e20dhs1     <- 0.152897
e20dhs2     <- 0.090644
e20dhs3     <- 0.075332
            
e21a        <- 0.148642
e21dha1     <- 0.168154
e21dha2     <- 0.095355
e21dhs1     <- 0.167112
e21dhs2     <- 0.107245
e21dhs3     <- 0.082661
            
e20td1      <- 0.228596
e20td2      <- 0.130633	
e20td3      <- 0.085102

## TARIFA_2.0A   p1*p20    + e20a*e1
## TARIFA_2.0DHA p1*p20    + e20dha1*e1 + e20dha2*e1
## TARIFA_2.0TD  p1*p20td1 + p2*p20td1 + e20td1*e1  + e20td2*e2 + e20td3*e3

variables <- c("tariff",
               "maximum",
               "kwh_2y_total",
               "kwh_2y_peak_old",
               "kwh_2y_flat_old",
               "kwh_2y_supervalley_old",
               "kwh_2y_peak_new",
               "kwh_2y_flat_new",
               "kwh_2y_valley_new",
               "p1_kw", 
               "p2_kw", 
               "p3_kw")

D <- fread("feats_v1.08.csv",stringsAsFactors=T)
R <- D[D$data_set=="goi",..variables]
R <- R[R$tariff!="",]
R <- R[R$tariff!="3.0A",]
rm(D)

cat("Numero de entradas ",length(R$kwh_2y_total),"\n",file=FILE,append=T)
cat("Entradas correctas sobre periodos viejos ",sum(abs(R$kwh_2y_flat_old + R$kwh_2y_peak_old + R$kwh_2y_supervalley_old - R$kwh_2y_total) <= TOL,na.rm=T),"\n",file=FILE,append=T)
cat("Entradas correctas sobre periodos nuevos ",sum(abs(R$kwh_2y_flat_new + R$kwh_2y_peak_new + R$kwh_2y_valley_new      - R$kwh_2y_total) <= TOL,na.rm=T),"\n",file=FILE,append=T)

R$PPOWER  <- 100*(R$p1_kw-R$maximum)/R$p1_kw
R$OPOWER  <- ifelse(0 <= R$PPOWER & R$PPOWER <= THRESHOLD_P, R$p1_kw, ifelse(R$maximum > 15, 15, R$maximum))
R$GROUP_P <- ifelse(0 <= R$PPOWER & R$PPOWER <= THRESHOLD_P, "NO CHANGE", "CHANGE")
R$GROUP_P <- as.factor(R$GROUP_P)

R <- R[complete.cases(R),]

###### Tariff calculation with real data ######

T20A  <- R$p1_kw                   * p20    + 
        (R$kwh_2y_peak_old                  +
         R$kwh_2y_flat_old                  +
         R$kwh_2y_supervalley_old) * e20a/2

T20DHA <- R$p1_kw                                       * p20       + 
          R$kwh_2y_peak_old                             * e20dha1/2 +
         (R$kwh_2y_flat_old + R$kwh_2y_supervalley_old) * e20dha2/2

T20DHS <- R$p1_kw                  * p20       + 
          R$kwh_2y_peak_old        * e20dhs1/2 +
          R$kwh_2y_flat_old        * e20dhs2/2 +
          R$kwh_2y_supervalley_old * e20dhs3/2

T21A   <- R$p1_kw                  * p21     + 
         (R$kwh_2y_peak_old                  +
          R$kwh_2y_flat_old                  +
          R$kwh_2y_supervalley_old)* e21a/2

T21DHA <- R$p1_kw                                       * p21     + 
          R$kwh_2y_peak_old                             * e21dha1/2 +
         (R$kwh_2y_flat_old + R$kwh_2y_supervalley_old) * e21dha2/2

T21DHS <- R$p1_kw                  * p21       + 
          R$kwh_2y_peak_old        * e21dhs1/2 +
          R$kwh_2y_flat_old        * e21dhs2/2 +
          R$kwh_2y_supervalley_old * e21dhs3/2

T20TD  <- R$p1_kw             * p20td1   + 
          R$p2_kw             * p20td2   + 
          R$kwh_2y_peak_new   * e20td1/2 +
          R$kwh_2y_flat_new   * e20td2/2 +
          R$kwh_2y_valley_new * e20td3/2

###### Tariff calculation with optimal power ######
          
TO20A  <- R$OPOWER                  * p20    + 
         (R$kwh_2y_peak_old                  +
          R$kwh_2y_flat_old                  +
          R$kwh_2y_supervalley_old) * e20a/2

TO20DHA<- R$OPOWER                                      * p20       + 
          R$kwh_2y_peak_old                             * e20dha1/2 +
         (R$kwh_2y_flat_old + R$kwh_2y_supervalley_old) * e20dha2/2

TO20DHS<- R$OPOWER                 * p20       + 
          R$kwh_2y_peak_old        * e20dhs1/2 +
          R$kwh_2y_flat_old        * e20dhs2/2 +
          R$kwh_2y_supervalley_old * e20dhs3/2

TO21A  <- R$OPOWER                 * p21     + 
         (R$kwh_2y_peak_old                  +
          R$kwh_2y_flat_old                  +
          R$kwh_2y_supervalley_old)* e21a/2

TO21DHA<- R$OPOWER                                      * p21     + 
          R$kwh_2y_peak_old                             * e21dha1/2 +
         (R$kwh_2y_flat_old + R$kwh_2y_supervalley_old) * e21dha2/2

TO21DHS<- R$OPOWER                 * p21       + 
          R$kwh_2y_peak_old        * e21dhs1/2 +
          R$kwh_2y_flat_old        * e21dhs2/2 +
          R$kwh_2y_supervalley_old * e21dhs3/2

TO20TD <- R$OPOWER            * p20td1   + 
          R$OPOWER            * p20td2   + #### FIXME revisar cuando tenga los maximos periodos
          R$kwh_2y_peak_new   * e20td1/2 +
          R$kwh_2y_flat_new   * e20td2/2 +
          R$kwh_2y_valley_new * e20td3/2

R <- cbind(R,T20A, T20DHA, T20DHS, T21A, T21DHA, T21DHS, T20TD,
             TO20A,TO20DHA,TO20DHS,TO21A,TO21DHA,TO21DHS,TO20TD)         

pdf(width=10,family="CM Roman",encoding="ISOLatin9.enc")
  TARIFAS <- c("T20TD","T20A","T20DHA","T20DHS","T21A","T21DHA")#,"T21DHS")
  AUX  <- c(R[                  ,"T20TD" ],
            R[R$tariff=="2.0A"  ,"T20A"  ],
            R[R$tariff=="2.0DHA","T20DHA"],
            R[R$tariff=="2.0DHS","T20DHS"],
            R[R$tariff=="2.1A"  ,"T21A"  ],
            R[R$tariff=="2.1DHA","T21DHA"])
#            R[R$tariff=="2.1DHS","T21DHS"],

  plot(kwAllPairsConoverTest(AUX),names=TARIFAS, range=1.5, outline=F, notch=T,
            ylab="€",main="Distribution of invoices costs per tariff")
  
  TARIFAS <- c("TO20TD","TO20A","TO20DHA","TO20DHS","TO21A","TO21DHA")#,"TO21DHS")
  AUX  <- c(R[                  ,"TO20TD" ],
            R[R$tariff=="2.0A"  ,"TO20A"  ],
            R[R$tariff=="2.0DHA","TO20DHA"],
            R[R$tariff=="2.0DHS","TO20DHS"],
            R[R$tariff=="2.1A"  ,"TO21A"  ],
            R[R$tariff=="2.1DHA","TO21DHA"])
#            R[R$tariff=="2.1DHS","T21DHS"],

  plot(kwAllPairsConoverTest(AUX),names=TARIFAS, range=1.5, outline=F, notch=T,
            ylab="€",main="Distribution of invoices costs per (optimal) tariff")
            
  tz <- t.test(R[R$tariff=="2.0A"  ,"T20A"], R[R$tariff=="2.0A"  ,"T20TD" ])
  tzo<- t.test(R[R$tariff=="2.0A"  ,"TO20A"],R[R$tariff=="2.0A"  ,"TO20TD" ])
  tzz<- t.test(R[R$tariff=="2.0A"  ,"T20A"], R[R$tariff=="2.0A"  ,"TO20TD" ])
  boxplot(   c(R[R$tariff=="2.0A"  ,"T20A"], R[R$tariff=="2.0A"  ,"T20TD" ],
               R[R$tariff=="2.0A"  ,"TO20A"],R[R$tariff=="2.0A"  ,"TO20TD"]),
               outline = FALSE, notch=T,
               ylab="€",
               main=paste("Comparison between invoices costs of new and old tariffs\n", 
                          "p-value (real) = "   ,tz$p.value,"\n",
                          "p-value (optimal) = ",tzo$p.value,"\n",
                          "p-value (cross) = "  ,tzz$p.value)
                           )

  tz <- t.test(R[R$tariff=="2.0DHA"  ,"T20DHA"], R[R$tariff=="2.0DHA"  ,"T20TD" ])
  tzo<- t.test(R[R$tariff=="2.0DHA"  ,"TO20DHA"],R[R$tariff=="2.0DHA"  ,"TO20TD" ])
  tzz<- t.test(R[R$tariff=="2.0DHA"  ,"T20DHA"], R[R$tariff=="2.0DHA"  ,"TO20TD" ])
  boxplot(   c(R[R$tariff=="2.0DHA"  ,"T20DHA"], R[R$tariff=="2.0DHA"  ,"T20TD" ],
               R[R$tariff=="2.0DHA"  ,"TO20DHA"],R[R$tariff=="2.0DHA"  ,"TO20TD"]),
               outline = FALSE, notch=T,
               ylab="€",
               main=paste("Comparison between invoices costs of new and old tariffs\n", 
                          "p-value (real) = "   ,tz$p.value,"\n",
                          "p-value (optimal) = ",tzo$p.value,"\n",
                          "p-value (cross) = "  ,tzz$p.value)
                           )
  
  boxplot(R$OPOWER, R$p1_kw, names = c("OPOWER","p1_kw"), outline = FALSE, notch=T,
          ylab="kW", main="Comparison between contracted power and maximum power")
  boxplot(R$p1_kw-R$maximum, outline = FALSE, notch=T,
          ylab="kW", xlab=expression(Delta), 
          main=paste("Differences between contracted power and maximum power\n",100*sum(R$p1_kw>R$maximum)/length(R$p1_kw),"% of negative values"))
                           
  D20A    <- R[R$tariff=="2.0A"  ,"T20A"  ] - R[R$tariff=="2.0A"  ,"T20TD"  ]
  D20DHA  <- R[R$tariff=="2.0DHA","T20DHA"] - R[R$tariff=="2.0DHA","T20TD"  ]
  D20DHS  <- R[R$tariff=="2.0DHS","T20DHS"] - R[R$tariff=="2.0DHS","T20TD"  ]
  D21A    <- R[R$tariff=="2.1A"  ,"T21A"  ] - R[R$tariff=="2.1A"  ,"T20TD"  ]
  D21DHA  <- R[R$tariff=="2.1DHA","T21DHA"] - R[R$tariff=="2.1DHA","T20TD"  ]
  D21DHS  <- R[R$tariff=="2.1DHS","T21DHS"] - R[R$tariff=="2.1DHS","T20TD"  ]

  DO20A   <- R[R$tariff=="2.0A"  ,"T20A"  ] - R[R$tariff=="2.0A"  ,"TO20TD" ]
  DO20DHA <- R[R$tariff=="2.0DHA","T20DHA"] - R[R$tariff=="2.0DHA","TO20TD" ]
  DO20DHS <- R[R$tariff=="2.0DHS","T20DHS"] - R[R$tariff=="2.0DHS","TO20TD" ]
  DO21A   <- R[R$tariff=="2.1A"  ,"T21A"  ] - R[R$tariff=="2.1A"  ,"TO20TD" ]
  DO21DHA <- R[R$tariff=="2.1DHA","T21DHA"] - R[R$tariff=="2.1DHA","TO20TD" ]
  DO21DHS <- R[R$tariff=="2.1DHS","T21DHS"] - R[R$tariff=="2.1DHS","TO20TD" ]
  
  P20A    <- 100 * D20A   /  R[R$tariff=="2.0A"  ,"T20A"  ]
  P20DHA  <- 100 * D20DHA /  R[R$tariff=="2.0DHA","T20DHA"]
  P20DHS  <- 100 * D20DHS /  R[R$tariff=="2.0DHS","T20DHS"]
  P21A    <- 100 * D21A   /  R[R$tariff=="2.1A"  ,"T21A"  ]
  P21DHA  <- 100 * D21DHA /  R[R$tariff=="2.1DHA","T21DHA"]
  P21DHS  <- 100 * D21DHS /  R[R$tariff=="2.1DHS","T21DHS"]

  TARIFAS <- c("T20A","T20DHA","T20DHS","T21A","T21DHA","T21DHS")
  boxplot(D20A$T20A, D20DHA$T20DHA, D20DHS$T20DHS, D21A$T21A, D21DHA$T21DHA, D21DHS$T21DHS, 
          outline = FALSE, names=TARIFAS, 
          ylab="€",main="Differences in € between old and new tariff\n Positive is good")
  boxplot(P20A$T20A, P20DHA$T20DHA, P20DHS$T20DHS, P21A$T21A, P21DHA$T21DHA, P21DHS$T21DHS, 
          outline = FALSE,names=TARIFAS,
          ylab="%",main="Differences in % between old and new tariff\n Positive is good")
  boxplot(DO20A$T20A, DO20DHA$T20DHA, DO20DHS$T20DHS, DO21A$T21A, DO21DHA$T21DHA, DO21DHS$T21DHS, 
          outline = FALSE, names=TARIFAS, 
          ylab="€",main="Differences in € between old and new (optimized) tariff\n Positive is good")

  R[R$tariff=="2.0A"  ,"P"  ] <- P20A
  R[R$tariff=="2.0DHA","P"  ] <- P20DHA
  R[R$tariff=="2.0DHS","P"  ] <- P20DHS
  R[R$tariff=="2.1A"  ,"P"  ] <- P21A
  R[R$tariff=="2.1DHA","P"  ] <- P21DHA
  R[R$tariff=="2.1DHS","P"  ] <- P21DHS
  
  R[R$tariff=="2.0A"  ,"D"  ] <- D20A
  R[R$tariff=="2.0DHA","D"  ] <- D20DHA
  R[R$tariff=="2.0DHS","D"  ] <- D20DHS
  R[R$tariff=="2.1A"  ,"D"  ] <- D21A
  R[R$tariff=="2.1DHA","D"  ] <- D21DHA
  R[R$tariff=="2.1DHS","D"  ] <- D21DHS
  
  R[R$tariff=="2.0A"  ,"DO" ] <- DO20A
  R[R$tariff=="2.0DHA","DO" ] <- DO20DHA
  R[R$tariff=="2.0DHS","DO" ] <- DO20DHS
  R[R$tariff=="2.1A"  ,"DO" ] <- DO21A
  R[R$tariff=="2.1DHA","DO" ] <- DO21DHA
  R[R$tariff=="2.1DHS","DO" ] <- DO21DHS

  R$GROUP_E  <- ifelse(abs(R$P) < THRESHOLD_E, "NEUTRAL", ifelse(R$P<= 0 , "LOSER" , "WINNER")) 
  R$GROUP_E  <- as.factor(R$GROUP_E)

  plot(R$GROUP_E,main="Number of clients in each energy intervention group")
  plot(R$GROUP_P,main="Number of clients to review contracted power")
dev.off()

embed_fonts("Rplots.pdf", outfile="Rplots.pdf")

stargazer(R,out="summary.html",median=TRUE)

sink(file=FILE,append=TRUE)
  100*summary(R$GROUP_E)/length(R$GROUP_E)
  100*summary(R$GROUP_P)/length(R$GROUP_P)
sink()
