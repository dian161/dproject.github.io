uas=read.csv("C:/Users/DIANWL/Documents/# Semester 5/8 EKONOMETRIKA 4 BISNSS/Project to UAS/capeeuy.csv")
View(uas)

library(plm)
attach(uas)

plot(uas$penduduk.miskin)

# uji chow
gc=plm(penduduk.miskin~ahh+tpak+tpt+pdrb, data = uas, model = "pooling")
gf=plm(penduduk.miskin~ahh+tpak+tpt+pdrb, data = uas, model = "within")
pooltest(gc, gf)
# model signifikan tolak Ho maka, model yg tepat untuk regresi data panel
# adalah model fixed effect model

# uji hausman
gf=plm(penduduk.miskin~ahh+tpak+tpt+pdrb, data = uas, model = "within")
gr=plm(penduduk.miskin~ahh+tpak+tpt+pdrb, data = uas, model = "random")
phtest(gf,gr)
# gagal tolak Ho maka, model yg tepat untuk regresi data panel adalah
# model fixed effect model

# uji breusch pagan
g=plm(penduduk.miskin~ahh+tpak+tpt+pdrb, data = uas, model = "pooling")
plmtest(g,effect="twoways", type="bp")
plmtest(g,effect="individual", type="bp")
plmtest(g, effect="time", type="bp")

# estimasi model
# model 1: model efek random dgn efek individu
g=NULL
g=plm(penduduk.miskin~ahh+tpak+tpt+pdrb, data = uas, model = "within", effect = "individu")
summary(g)
# 1a tpak tidak signifikan
g=plm(penduduk.miskin~ahh+tpt+pdrb, data = uas, model = "within", effect = "individu")
summary(g)
# 1b amh tidak signifikan
g=plm(penduduk.miskin~ahh+tpt, data = uas, model = "within", effect = "individu")
summary(g)

fixef(g, type = "level")

#######################################
# uji korelasi serial (uji individual)
glc=plm(penduduk.miskin~ahh+tpt, data=uas, model="within", effect = "individual")
pbgtest(glc, order=2)


#######################################
# heterokedasitas robust covariance estimator
library(lmtest)
bptest(glc)
