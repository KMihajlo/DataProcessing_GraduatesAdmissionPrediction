Osnovna = read.table("C:/Users/computer/Desktop/Mnozhestvo Podatoci/Admission_Predict.csv", sep=",", fill=TRUE, header = TRUE)
# PRV DEL 
#________________________________________________________________________________

# Shansi (Verojatnosti) za vlez vo magistarski studii
chance = Osnovna [, 9]

# Dodiplomski uspeh na site aplikanti
GPA = Osnovna [, 7]

# Tabela so raspredelba na chestoti на UGPA
len = length(GPA)
brIntervali = seq(6.5, 10, by=0.5)
d.int = cut(GPA, brIntervali, right=FALSE)
freq = table(d.int)
Rfreq = freq/len
CumFreq = cumsum(freq)
R_CumFreq = cumsum(freq)/len
R_CumFreq2 = cumsum(Rfreq)
Pfreq = Rfreq*100
P_Cumfreq = R_CumFreq*100
d.table = cbind(freq, Rfreq, CumFreq, Pfreq, P_Cumfreq)
d.table

# Sredni tochki na intervalite na prvata raspredelba na chestoti
middle = c()
for(i in 1:length(brIntervali)-1)
{
  middle = c(middle, (brIntervali[i]+brIntervali[i+1])/2)
}
middle

# Histogram 1
histogram1 = hist(GPA, ylim = c(0, 150), xlim = c(6.5, 10), breaks = 10, col = "gray", xlab = "Постдипломски успех", ylab = "Честоти", main = "Хистограм 1 - UGPA")

# Poligon 1
P_Cumfreq0 = c(0, P_Cumfreq)
plot(brIntervali, P_Cumfreq0, axes = F, main = "Полигон на кумулативни честоти во % (Ogive)", xlab = "Интервали", ylab = "Кумулативни честоти %")
axis(side = 1, at = brIntervali)
axis(side = 2)
lines(brIntervali, P_Cumfreq0)

# Tabela so raspredelba na chestoti za shansi za priem
len = length(chance)
brIntervali = seq(0, 1, by=0.2)
d.int = cut(chance, brIntervali, right=FALSE)
freq = table(d.int)
Rfreq = freq/len
CumFreq = cumsum(freq)
R_CumFreq = cumsum(freq)/len
R_CumFreq2 = cumsum(Rfreq)
Pfreq = Rfreq*100
P_Cumfreq = R_CumFreq*100
d.table = cbind(freq, Rfreq, CumFreq, Pfreq, P_Cumfreq)
d.table

# Sredni tochki na intervalite na vtorata raspredelba na chestoti
middle = c()
for(i in 1:length(brIntervali)-1)
{
  middle = c(middle, (brIntervali[i]+brIntervali[i+1])/2)
}
middle

# Histogram 2
histogram2 = hist(chance, ylim = c(0, 150), xlim = c(0, 1), breaks = 5, col = "gray", xlab = "Веројатност за прием во магистарски студии", ylab = "Честоти", main = "Хистограм 2 - Веројатност")

# Poligon 2
P_Cumfreq0 = c(0, P_Cumfreq)
plot(brIntervali, P_Cumfreq0, axes = F, main = "Полигон на кумулативни честоти во % (Ogive)", xlab = "Интервали", ylab = "Кумулативни честоти %")
axis(side = 1, at = brIntervali)
axis(side = 2)
lines(brIntervali, P_Cumfreq0)

# Steblo List na GPA
cost = sort(GPA)
library(data.table)
myStem = function(x, leftDigits, rounding = 1)
{
  data = data.table("x" = x)
  data[, left := floor(x/10^leftDigits)]
  data[, right := (round(x - left*10^leftDigits, rounding))*10^rounding]
  data = data[, paste(sort(right), collapse = " "), by = left]
  data[, out := paste(left, " | ", V1), by = left]
  cat(data$out, sep = "\n")
}
myStem(cost, 0, 2)

# Steblo List na Chance of admit
cost = sort(chance)
library(data.table)
myStem = function(x, leftDigits, rounding = 1)
{
  data = data.table("x" = x)
  data[, left := floor(x/10^leftDigits)]
  data[, right := (round(x - left*10^leftDigits, rounding))*10^rounding]
  data = data[, paste(sort(right), collapse = " "), by = left]
  data[, out := paste(left, " | ", V1), by = left]
  cat(data$out, sep = "\n")
}
myStem(cost, 0, 2)

# Grafik na rasejuvanje
plot(GPA, chance, main = "График на расејување", xlab = "Постдипломски успех", ylab = "Веројатност за прием во магистарски студии")

# Moda
Mode <- function(x)
{
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(GPA)
Mode(chance)

# Medijana
median(GPA)
median(chance)

# Prosek
prosek1 = mean(GPA)
prosek1
prosek2 = mean(chance)
prosek2

# Kvartali
quantile(GPA)
quantile(chance)

# Opseg
opseg1 = max(GPA) - min(GPA)
opseg1
opseg2 = max(chance) - min(chance)
opseg2

# Interkvartalen Raspon
interkvartalenRaspon1 = IQR(GPA)
interkvartalenRaspon1
interkvartalenRaspon2 = IQR(chance)
interkvartalenRaspon2

# Disperzija
dis1 = var(GPA)
dis1
dis2 = var(chance)
dis2

# Standardna devijacija
std1 = sd(GPA)
std1
std2 = sd(chance)
std2

# Koeficient na korelacija
cor(GPA, chance)


# VTOR DEL 
#_____________________________________________________________________________________________

# Interval na doverba
n = 400                            # Golemina na primerok
error = qnorm(0.975)*std1/sqrt(n)  # std1 - Prethodno presmetanata standardna devijacija na GPA
                                   # prosek1 - Prethodno presmetaniot prosek na GPA
prosek1 - error                    # Dolna granica na intervalot vo koj se naogja prosekot
prosek1 + error                    # Gorna granica na intervalot vo koj se naogja prosekot

# Postavuvanje i testiranje na hipotezi
mi = 9.0                                             # Pretpostavuvam deka prosekot na GPA e 9

z.test = function(GPA, mi, dis1){                    # dis1 - Prethodno presmetanata disperzija na GPA
  zeta = (prosek1 - mi) / (sqrt(dis1 / length(GPA)))
  return(zeta)
} 
Z = z.test(GPA, mi, dis1)
Z
if(Z < -1.96 || Z > 1.96)
{
  print('H0 se otfrla')
}else
{
  print('H0 se prifakja')
}

# Test za raspredelba
shapiro.test(GPA)      # Shapiro-Wilk normality test

ks.test(GPA, "pnorm", prosek1, std1)


library(e1071)         #install.packages("e1071", dep = TRUE, type = "source")

kurtosis(GPA)          # Excess kurtosis
skewness(GPA)          # Naklonosta na histogramot

# Testiranje na hipotezi za nezavisnost
library(MASS)
tbl = table(Osnovna$CGPA, Osnovna$Chance.of.Admit)
tbl                    # Tabela na kontingencija
chisq.test(tbl)

library("ggpubr")      #install.packages("ggpubr")
cor.test(GPA, chance, method=c("pearson", "kendall", "spearman"))

# Regresiona analiza
model = lm(GPA~chance)
round(coefficients(model), 3)
plot(GPA~chance, main = "Линеарна регресија меѓу две обележја")
abline(reg = model, col="red")
Z = coefficients(model)
summary(model)
model
Z
plot(model)
termplot(model)
