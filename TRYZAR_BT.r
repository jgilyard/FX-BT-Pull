#'
#'
#'MIT License

#'Copyright (c) 2016 Julian Gilyard
#'Permission is hereby granted, free of charge, to any person obtaining a copy
#'of this software and associated documentation files (the "Software"), to deal
#'in the Software without restriction, including without limitation the rights
#'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#'copies of the Software, and to permit persons to whom the Software is
#'furnished to do so, subject to the following conditions:
#'The above copyright notice and this permission notice shall be included in all
#'copies or substantial portions of the Software.
#'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#'SOFTWARE.


library(ggplot2); library(caret)
setwd("C:/Users/Julian Gilyard/Documents/London Presentations")
C_Data <- read.csv("TRYZAR_1Y.csv")
N_Val <- -100000000 #amount of Brazlian Real's Invested
L_Time <- 45 # length of window
L_Time_Min <- 30
L_Time_Max <- 60
E_Vector <- C_Data[2]

P_ret <- vector(length = nrow(E_Vector) - L_Time)
iter <- (nrow(E_Vector) - L_Time)

P_ret_Min <- vector(length = nrow(E_Vector) - L_Time_Min)
iter_Min <- (nrow(E_Vector) - L_Time_Min)

P_ret_Max <- vector(length = nrow(E_Vector) - L_Time_Max)
iter_Max <- (nrow(E_Vector) - L_Time_Max)


for (i in 1:iter -1)
{
  t_len <-nrow(E_Vector)
  P_ret[i] <- (E_Vector[t_len - i -L_Time ,] - E_Vector[t_len -i,])
  P_ret[i] <- P_ret[i]/E_Vector[t_len -i -L_Time,] * N_Val
}


for (i in 1:iter_Min -1)
{
  t_len <-nrow(E_Vector)
  P_ret_Min[i] <- (E_Vector[t_len - i -L_Time_Min ,] - E_Vector[t_len -i,])
  P_ret_Min[i] <- P_ret_Min[i]/E_Vector[t_len -i -L_Time_Min,] * N_Val
}

for (i in 1:iter_Max -1)
{
  t_len <-nrow(E_Vector)
  P_ret_Max[i] <- (E_Vector[t_len - i -L_Time_Max ,] - E_Vector[t_len -i,])
  P_ret_Max[i] <- P_ret_Max[i]/E_Vector[t_len -i -L_Time_Max,] * N_Val
}


summary(P_ret)
min_loss_1 <-match(min(P_ret),P_ret)
min_loss_1 <- nrow(E_Vector) - min_loss_1 
max_profit_1 <-match(max(P_ret),P_ret)
max_profit_1 <- nrow(E_Vector) - max_profit_1
min(P_ret)
min_loss_1
max(P_ret)
max_profit_1

summary(P_ret_Max)
min_loss_2 <-match(min(P_ret_Max),P_ret_Max)
min_loss_2 <- nrow(E_Vector) - min_loss_2 
max_profit_2 <-match(max(P_ret_Max),P_ret_Max)
max_profit_2 <- nrow(E_Vector) - max_profit_2
min(P_ret_Max)
min_loss_2
max(P_ret_Max)
max_profit_2


summary(P_ret_Min)
min_loss_3 <-match(min(P_ret_Min),P_ret_Min)
min_loss_3 <- nrow(E_Vector) - min_loss_3 
max_profit_3 <-match(max(P_ret_Min),P_ret_Min)
max_profit_3 <- nrow(E_Vector) - max_profit_3
min(P_ret_Min)
min_loss_3
max(P_ret_Min)
max_profit_3



p <- ggplot(as.data.frame(P_ret), aes(x=P_ret)) +geom_density(fill="cadetblue1") + scale_y_continuous(labels = scales::percent)+ scale_x_continuous(labels = scales::dollar, limits=c(min(P_ret)*3, max(P_ret)*1.5))+ labs(x=" Short TRYZAR Return on Portfolio for 45 Day Long Moving Window", y="Observation Frequency")
p_Min <- ggplot(as.data.frame(P_ret_Min), aes(x=P_ret_Min)) +geom_density(fill="mistyrose1") + scale_y_continuous(labels = scales::percent)+ scale_x_continuous(labels = scales::dollar, limits=c(min(P_ret_Min)*3, max(P_ret_Min)*1.5))+ labs(x=" Short TRYZAR Return on Portfolio for 30 Day Long Moving Window", y="Observation Frequency")
p_Max <- ggplot(as.data.frame(P_ret_Max), aes(x=P_ret_Max)) +geom_density(fill="slategray1") + scale_y_continuous(labels = scales::percent)+ scale_x_continuous(labels = scales::dollar, limits=c(min(P_ret_Max)*3, max(P_ret_Max)*1.5))+ labs(x="Short TRYZAR Return on Portfolio for 60 Day Long Moving Window", y="Observation Frequency")



#ggplot(as.data.frame(P_ret), aes(x=P_ret)) +geom_density() + xlim(min(P_ret)*3, max(P_ret)*1.5)\
p + geom_vline(aes(xintercept=mean(P_ret)),
               color="blue", linetype="dashed", size=1)
p_Min + geom_vline(aes(xintercept=mean(P_ret_Min)),
                   color="red", linetype="dashed", size=1)
p_Max + geom_vline(aes(xintercept=mean(P_ret_Max)),
                   color="black", linetype="dashed", size=1)