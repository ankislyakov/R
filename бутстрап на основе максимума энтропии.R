# подключение необходимых пакетов
library(meboot)
library(lmtest)
library(dynlm)
library(boot)
library(hdrcde)

# загрузка исходного набора данных в виде таблицы
ie <- read.csv("D:/ie.csv", header = TRUE, sep =";")

# выбор из исходного набора данных результирующего показателя и факторных признаков 
ie2 <- ie[ , -c(1)]
ds1 <- ie$date
# преобразование даты в необходимый формат
ds<-strptime(ds1, format = "%d.%m.%Y")
ie3<- data.frame(ds,ie2)
# Масштабирование числовых значений показателей (логарифмирование) и записть в датафрэйм
ie2 <- log(ie2)
ie4<- data.frame(ds,ie2)
# визуализация формы зависимостей выбранных показателей в виде точечных диаграмм
plot(ie2)
# Формирование и визуализация выбранных показателей в виде временных рядов 
ie.ts <- ts(ie2)
plot(ie.ts)
# Конструирование динамической линейной модели временного ряда 
lmcf <- dynlm(import ~ L(import, 1) + L(RUBUSD,1) + L(oil, 1), data = ie.ts)
# Вывод коэффициентов модели
lmcf
coeftest(lmcf)
# инициализация параметров генератора случайнфх чисел
set.seed(135)
# Выполнение тесте Дарбина-Уотсона для автокорреляции
durbinWatsonTest(model = lmcf, max.lag = 4)

# задание количества переменных в модели
theta <- function(y, x1, x2) {
  reg <- lm(y ~ x1 + x2)
  thet <- coef(reg)[2] + coef(reg)[3]
  return(thet)
}

# Создание функции, описывающей параметры расширения выборки
bstar.consu <- function(y, x1, x2, theta = theta,
                        level = 0.95, bigJ = 499, seed1 = 135) {
  set.seed(seed1)
  semy <- meboot(x = y, reps = bigJ)$ensemble
  semx1 <- meboot(x = x1, reps = bigJ)$ensemble
  semx2 <- meboot(x = x2, reps = bigJ)$ensemble
  n <- NROW(y)
  m <- length(theta(y, x1, x2))
  if(m!=1) stop("много параметров")
  bb <- matrix(NA, bigJ)
  for(j in 1:bigJ) {
    yy <- semy[,j]
    xx1 <- semx1[,j]
    xx2 <- semx2[,j]
    bb[j] <- theta(yy, xx1, xx2)
  }
  return(bb)
}

# Выбор факторных признаков и результирующих переменных из датафрэйма временных рядов
y <- ie.ts[,2]
x1 <- ie.ts[,3]
x2 <- ie.ts[,4]

# Настройка параметров сэмплирования - виртуального расширения выборки
reg <- dynlm(y ~ L(y, 1) + L(x1, 1) + L(x2,1))
su <- summary(reg)
se <- su$coefficients[3,2]
t0 <- theta(y, x1, x2)
b3s <- bstar.consu(y, x1, x2, theta)
simple.percentile <- quantile(b3s, c(0.025, 0.975), type = 8)
asymmetric.around.0 <- null.ci(b3s)
out <- list(t = b3s, t0 = t0, var.t0 = se^2, R = 499)
class(out) <- "boot"

# Выполнение виртуального расширения выборки по указанным параметрам
boot.percentile <- boot.ci(out, type = "perc")$percent[4:5]
boot.norm <- boot.ci(out, type = "norm")$normal[2:3]
boot.basic <- boot.ci(out, type = "basic")$basic[4:5]
rbind(simple.percentile, asymmetric.around.0, boot.percentile,
      boot.norm, boot.basic)
# Вывод коэффициентов модели с учетом доверительных интервалов
plot(out)
# визуализация функции плотности вероятностей
hdr.den(b3s, main = expression(Функция ~ плотности ~ вероятностей))
# вывод параметров распределения
out