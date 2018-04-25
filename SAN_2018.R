##install.packages("Cairo")
##install.packages("ggplot2)

###ТОЛЬКО ДЛЯ ПАРЫ ОБЪЕКТОВ!!!!


###################################
##########исходные данные##########
## сравниваются объекты Y1 и Y2
## сраваниваются по характеристикам X1, X2,...,Xk
## характеристики имеют коэфициенты важности M1, M2,...,Mk
## все характеристики с характером изменения свойства ТОЛЬКО "больше-лучше"
## количество итераций Монте-Карло n
######################################
## необходимые пакеты
library(ggplot2)
###################################
##########функции##########

##1 функция классифческого САН
SAN_classic <- function(y1, y2, m){
  q1 <- 0 
  for(i in 1:length(y1)-1){
    i<- i+1
    q1<-sum(q1, y1[i]/max(y1[i], y2[i]) * m[i])
  }
  return(q1)
}
##2 функция САН с применением метода Монте-Карло
SAN_algoritm <- function(y1, y2, m, n, name_object){
  alpha_min <-c()
  qi_integral <- c()
  for(i in 1:length(y1)-1){
    i<- i+1
    alpha_min<-c(alpha_min, pi/2 - atan((1/(max(y1[i], y2[i]) - min(y1[i], y2[i])))))
  }
  alpha <- runif(n, alpha_min[1], pi/2)
  alpha <- cbind(alpha, runif(n, alpha_min[2], pi/2))
  alpha <- cbind(alpha, runif(n, alpha_min[3], pi/2))
  alpha <- cbind(alpha, runif(n, alpha_min[4], pi/2))
  ##далее в цикле по монте-карло 
  for(k in 0:n){
    q1 <- 0  
    for(i in 1:length(y1)-1){
      i<- i+1
      q1<-sum(q1,(tan(pi/2-alpha[k,i])*(y1[i]-max(y1[i], y2[i])) +1)* m[i])
    }
    qi_integral <- c(qi_integral,q1)
  }  
  ## конец цикла по монте-карло 
  res_dat<-data.frame(VAL = qi_integral, OBJECT = name_object)
  return((res_dat))
}
###############################
## ввод исходных данных
Y1 <-c(3, 25, 15, 1)
Y2 <-c(5, 30, 9, 0.8)
#Y3 <-c(1, 17, 13, 0.5)
#Y4 <-c(4, 2, 13, 0.5)
M  <-c(0.1, 0.2, 0.5, 0.2)
n  <-1000
###################################
#### выполнение программы

### классический подход
print("интегральный показатель объекта Y1 по классическому методу: ")
print(SAN_classic(Y1, Y2, M))
print("интегральный показатель объекта Y2 по классическому методу: ")
print(SAN_classic(Y2, Y1, M))

### новый подход
print("интегральный показатель объекта Y1 по новому методу: ")
print(mean(SAN_algoritm(Y1, Y2, M, n, 'Y1')$VAL ))
print("интегральный показатель объекта Y2 по новому методу: ")
print(mean(SAN_algoritm(Y2, Y1, M, n, 'Y2')$VAL ))



###отображение на графике boxplot
result<-rbind(SAN_algoritm(Y1, Y2, M, n, 'Y1'), SAN_algoritm(Y2, Y1, M, n, 'Y2'))
gr<-ggplot(data=result, aes(x=  OBJECT, y = VAL)) + 
           geom_boxplot(aes(fill = OBJECT) )+ 
           labs(x = "Сравниваемые объекты",
                y = "Интегральный показатель превосходства")  + theme_minimal() 
print(gr)#отобразить


###сохранение графика boxplot в файл
png(filename = "/home/.../file.png", type = "cairo", units = "px", 
    width = 1600, height = 1200, res = 300, pointsize = 10)
gr # записать  график в файл
dev.off() # Отключить запись в файл и вернуть отображение графика в стандартном окне




