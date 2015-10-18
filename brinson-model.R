##================导入数据===================
rm(list=ls())
setwd("C:/Users/ptang/Desktop")
return <- read.csv("return.csv",header=F,stringsAsFactors=F)
weight <- read.csv("weight.csv",header=F,stringsAsFactors=F)
industry <- read.csv("industry.csv",header=F)
indexweight <- read.csv("indexWeight.csv",header=F,stringsAsFactors=F)

## 把股票代码整理成6位数的字符串。不足用0补在前面
sixcode <- function(x){
  x[1,] <- as.character(x[1,])
  x[1,] <- paste("00000",x[1,],sep="")
  x[1,] <- substring(x[1,],nchar(x[1,])-5,nchar(x[1,]))
  return(x)
}
return <- sixcode(return)
weight <- sixcode(weight)
indexweight <- sixcode(indexweight)

industry <- industry[-1,]
#industry <- t(industry)
#industry_level <- industry[,3]
i_u <- unique(industry[,3])#_level)
i_loc <- numeric()
i_length <- numeric()
for(i in 1:length(i_u)){
  i_loc <- c(i_loc,which(industry_level==i_u[i]))
  i_length <- c(i_length,length(which(industry_level==i_u[i])))
}

location <- return[1,]%in%indexweight[1,]
indexreturn <- return[location]

remains <- data.frame(matrix(nrow=1611,ncol=2807))
remains[1,] <- return[1,]
remains[,1] <- return[,1]
remain1 <- remains
remains[location] <- indexreturn
remain1[location] <- indexweight
indexreturn <- remains
indexweight <- remain1

return[is.na(return)]<-0
weight[is.na(weight)]<-0
indexreturn[is.na(indexreturn)]<-0
indexweight[is.na(indexweight)]<-0

##===============brinson模型处理=============
#先把股票代码那一行删除，便于字符串数字的处理
code <- return[1,]
date <- return[,1]
date <- date[-1]
returns <- return[-1,]
returnss <- returns[,-1]
weights <- weight[-1,]
weightss <- weights[,-1]
index_return <- indexreturn[-1,]
index_return <- index_return[,-1]
index_weight <- indexweight[-1,]
index_weight <- index_weight[,-1]

toMatrix <- function(x){
  x1 <- as.matrix(x,ncol=ncol(x))
  x2 <- matrix(as.numeric(x1), nrow=nrow(x1))
  return(x2)
}
index_returnss <- toMatrix(index_return)
index_weightss <- toMatrix(index_weight)
returnsss <- toMatrix(returnss)
weightsss <- toMatrix(weightss)
index <- index_returnss*index_weightss
real_Return <- returnsss*weightsss

#计算28个行业的收益率Rp,权重Wp,以及基准收益率Rb,基准权重Wb
count_RW <- function(x,i_length,i_loc){
  row_sum <- NULL
  loc <- i_loc[1:i_length[1]]
  row_sum <- c(row_sum,rowSums(x[,loc]))
  for(j in 2:length(i_length)){
    loc <- i_loc[(i_length[j-1]+1):(i_length[j-1]+i_length[j])]
    row_sum <- matrix(c(row_sum,rowSums(x[,loc])),nrow=1610)
  }
  return(row_sum)
}
Wb <- count_RW(index_weightss,i_length,i_loc)
Wp <- count_RW(weightsss,i_length,i_loc)
Rb <- count_RW(index,i_length,i_loc)/Wb
Rb[is.na(Rb)] <- 0
Rp <- count_RW(real_Return,i_length,i_loc)/Wp
Rp[is.na(Rp)] <- 0

#算基准收益率rb
rb <- matrix(rowSums(index),ncol=28,nrow=1610)
rp <- matrix(rowSums(real_Return),ncol=28,nrow=1610)

AR <- rowSums((Wp-Wb)*((1+Rb)/(1+rb)-1))
SR <- rowSums(Wb*(Rp-Rb)/(1+rb))
IR <- (1+rp[,1])/(1+rb[,1])/(1+AR)/(1+SR)
ASI <- cbind(AR,SR,IR)

AR_Total <- 1
SR_Total <- 1
RP <- 1
RB <- 1
for(i in 1:(nrow(ASI)-1)){
  AR_Total <- AR_Total*(ASI[i,1]+1)
  SR_Total <- SR_Total*(ASI[i,2]+1)
  RP <- RP*(rp[i,1]+1)
  RB <- RB*(rb[i,1]+1)
}
IR_Total <- RP/RB/AR_Total/SR_Total




