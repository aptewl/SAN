
###################################
##########исходные данные##########
## сравниваются объекты Y1 и Y2
## сраваниваются по 7 характеристикам X1, X2,...,X7
## характеристики имеют коэфициенты важности M1, M2,...,M7
## все характеристики с характером "больше-лучше"
## количество итераций Монте-Карло n
###################################
##########функции##########
SAN_classic <- function(y1, y2, m){
  q1 <- 0 
  for(i in 1:length(y1)-1){
    i<- i+1
    q1<-sum(q1, y1[i]/max(y1[i], y2[i]) * m[i])
  }
  return(q1)
}

SAN_algoritm <- function(y1, y2, m, n){
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
  ## в цикле по монте-карло 
  for(k in 0:n){
    
    q1 <- 0  
    for(i in 1:length(y1)-1){
      i<- i+1
      q1<-sum(q1,(tan(pi/2-alpha[k,i])*(y1[i]-max(y1[i], y2[i])) +1)* m[i])
    }
    qi_integral <- c(qi_integral,q1)
  }  
  ## конец цикла по монте-карло 
 # plot(density(qi_integral))
  return(mean(qi_integral))
}
## ввод исходных данных
Y1 <-c(3, 25, 15, 1)
Y2 <-c(5, 30, 9, 0.8)
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
print(SAN_algoritm(Y1, Y2, M, n))
print("интегральный показатель объекта Y2 по новому методу: ")
print(SAN_algoritm(Y2, Y1, M, n))

