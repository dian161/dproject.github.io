library(tseries)
library(forecast)

# memanggil data
kp<-read.csv("C:/Users/DIANWL/Documents/# KP/# NYUSUNNNNN/Data/cabai merah.csv")
head(kp, 5)

# ubah ke ts
kp.ts<-ts(kp$Harga, start = c(2016,1), frequency = 12)
plot(kp.ts, main="TS:Harga Cabai Merah", col="blue")

# cek stationer data berdasarkan hipotesis
# Uji ADF
# Ho: data tidak stasioner
# H1: data stasioner
adf.test(kp.ts)
# hasil 0.01 tolak Ho, data sudah stasioner

# cek ke stasioneran data dpt dilihat dgn grafik
# par >> melihat 2 grafik menjadi 1 tampilan
par(mfrow=c(1,2))
# jika Acf menurun scr lambat berarti, data tdk stasioner
# jika data tdk stasioner, maka dilakukan Diferensi (agar data stasioner)

# acf >> melambangkan model MA
# pacf >> untuk model AR
Acf(kp.ts, lag.max = 48)
Pacf(kp.ts, lag.max = 248)

# output acf >> batang[Y] acf (- lag ke 4), p=2
# output pacf >> batang[Y] pacf ( - lag ke 4), q=2

# mengetahu model yang ada layak
# model ARIMA (p,d,q)
library(lmtest)
model1=Arima(kp.ts, order = c(2,0,2), include.mean = F)
model1

model2=Arima(kp.ts, order = c(1,0,2), include.mean = F)

model3=Arima(kp.ts, order = c(0,0,2),include.mean = F)
model3

model4=Arima(kp.ts, order = c(2,0,1),include.mean = F)
model4 # AIC terkecil kedua

model5=Arima(kp.ts, order = c(2,0,0), include.mean = F)
model5 # AIC terkecil pertama (MODEL TERBAIK)

# melihat signifikansi dari model
printsarima=function(x, digits=4, se=T,...){
  if(length(x$coef)>0){
    cat("\nCoefficients:\n")
    coef=round(x$coef, digits = digits)
    if(se&&nrow(x$var.coef)){
      ses=rep(0, length(coef))
      ses[x$mask]<-round(sqrt(diag(x$var.coef)), digits = digits)
      coef=matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef=rbind(coef,s.e.=ses)
      statt=coef[1,]/ses
      pval=2*pt(abs(statt), df=length(x$residuals)-1, lower.tail = F)
      coef=rbind(coef, t=round(statt, digits = digits), 
                 sign.=round(pval, digits = digits))
      coef=t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}

# hipotesis 
# Ho: koef-koef tdk sig trhdp model
# H1: koef-koef sig trhdp model
printsarima(model1)
printsarima(model2)
printsarima(model3)# << gagal tolak Ho, yg tdk sig dapat dihapus dan di uji kembali
printsarima(model4)
printsarima(model5)

# uji diagnostik
# untuk melihat ada autokorelasi atau tidak
par(mfrow=c(1,2))
tsdiag(model4)
tsdiag(model5)

# peramalan
forecast1=forecast(model4, h=12)
forecast1

forecast2=forecast(model5, h=12)
forecast2 # hasil yg paling mendekati


