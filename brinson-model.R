#---------------groundworks-------------------
rm(list=ls())
setwd("C:/Users/ptang/Desktop/cjr/Brinson模型/data")
#---------------导入数据-------------------
loadfile <- function(filename){
  ##本函数用于导入csv文件
  #输入：
  #filename:csv文件名字
  
  #
  #输出：
  #x:导入数据，以数据框形式存储
  #returns:从2009-01-05至2015-08-18共1610天的沪深300投资组合每天收益率，共有股票2806只
  #weight:投资组合每天权重
  #industry:投资组合的股票所在行业
  #indexweight:基准组合每天权重，共有546只股票
  x <- read.csv(filename,header=F,stringsAsFactors=F)
  return(x)
}
#---------------股票代码变成6位字符串-------------------

sixcode <- function(x){
  # 本函数用于把股票代码整理成6位数的字符串。不足用0补在前面
  #输入：
  #x:上面导入的数据框
  
  #
  #输出：
  #x:原数据框，数据框第一行的股票代码变成了6位的字符串
  x[1,] <- as.character(x[1,])
  x[1,] <- paste("00000",x[1,],sep="")
  x[1,] <- substring(x[1,],nchar(x[1,])-5,nchar(x[1,]))
  return(x)
}
#---------------处理股票所在行业的数据-------------------
unique_industry <- function(industry){
  #本函数用于将行业数据中每一个不同的行业所有所处的列数找出来
  #输入：
  #industry:行业数据
  
  #
  #输出：
  #i_loc:每个行业所在的列位置，是一个数组
  industry <- industry[-1,] #把数据的第一行无关信息删除
  i_u <- unique(industry[,3])#找出所有不同的行业
  i_loc <- numeric()
  for(i in 1:length(i_u)){
    i_loc <- c(i_loc,which(industry[,3]==i_u[i]))
  }
  return(i_loc)
}

unique_length <- function(industry){
  #本函数用于将行业数据中每一个不同的行业所含股票只数计算出来
  #输入：
  #industry:行业数据
  
  #
  #输出：
  #i_length:每个行业里的股票支数，是一个数组
  industry <- industry[-1,] #把数据的第一行无关信息删除
  i_u <- unique(industry[,3])#找出所有不同的行业
  i_length <- numeric()
  for(i in 1:length(i_u)){
    i_length <- c(i_length,length(which(industry[,3]==i_u[i])))
  }
  return(i_length)
}
#---------------生成基准组合每日收益率--------------------
genReturn <- function(returns,indexweight){
  #本函数用来生成基准组合每日收益率，为数据框
  #输入：
  #returns:投资组合收益率数据框
  #indexweight:基准组合权重数据框
  
  #输出：
  #indexreturn:基准组合每日收益率
  location <- returns[1,]%in%indexweight[1,]#找出投资组合所有股票里包含基准组合股票的逻辑值
  indexreturn <- returns[location]#生成列数与indexweight等长的基准组合收益率，是一个数据框
  return(indexreturn)
}
#---------------补充基准组合的股票只数--------------------
addCol <- function(returns,x){
  #本函数用于补充基准组合的权重和收益率的列使得股票数与投资组合相同
  #输入：
  #returns:投资组合每日收益率数据框
  #indexweight:基准组合每日权重
  #x:取值为indexreturn或者indexweight
  
  #输出：
  #y:基准组合数据框
  location <- returns[1,]%in%indexweight[1,]
  remains <- data.frame(matrix(nrow=nrow(returns),ncol=ncol(returns)))#生成行列与returns相等的空数据框
  remains[1,] <- returns[1,]#提取returns第一行remains数据框，为股票代码到
  remains[,1] <- returns[,1]#提取returns第一列remains数据框，为时间区间
  remains[location] <- x
  y <- remains#把indexreturn补充成股票代码与投资组合一致，补充部分用0表示收益率
  return(y)
}
#---------------Brinson模型处理--------------------
delete <- function(x){
  #本函数用于把股票代码和时间区间，即第一行和第一列删除，便于进行字符串的处理
  #输入：
  #x:取值为returns,weight,index_return,index_weight
  
  #
  #输出：
  #y:处理完毕的数据框
  x <- x[-1,]
  y <- x[,-1]
  return(y)
}
#---------------数据框处理为矩阵--------------------
toMatrix <- function(x){
  #本函数用于将数据框处理成矩阵
  #输入：
  #x:取值为returns,weight,index_return,index_weight
  
  #
  #输出：
  #x2:处理后的数据框
  x1 <- as.matrix(x,ncol=ncol(x))
  x2 <- matrix(as.numeric(x1), nrow=nrow(x1))
  return(x2)
}
#---------------每个行业的收益率和权重--------------------
count_RW <- function(x,i_length,i_loc){
  #此函数用来计算28个行业的收益率Rp,权重Wp,以及基准收益率Rb,基准权重Wb
  #输入：
  #x:取值为index_weight,weight,index,real_return
  #i_length:是一个数组
  #i_loc:是一个数组
  
  #
  #输出：
  #row_sum:28个行业的每天收益率，权重，基准收益率，基准权重，是一个矩阵
  row_sum <- NULL
  loc <- i_loc[1:i_length[1]]
  row_sum <- c(row_sum,rowSums(x[,loc]))
  for(j in 2:length(i_length)){
    loc <- i_loc[(sum(i_length[1:(j-1)])+1):(sum(i_length[1:j]))]
    row_sum <- matrix(c(row_sum,rowSums(x[,loc])),nrow=1610)
  }
  return(row_sum)
}
#---------------主函数----------------
gen_Matrix <- function(j,returns,weight,industry,indexweight){
  #本函数用于生成每天的配置收益，选股收益和交叉收益
  #输入：
  #j:取值为0到28,当为0时，输出的是总体的Brinson模型，当取值为1-28时，分别对应一个行业的细分Brinson模型框架
  #returns：投资组合每日收益率，是一个数据框
  #weight：投资组合每日权重，是一个数据框
  #industry：投资组合内每只股票对应行业，是一个数据框
  #indexweight：基准组合每日权重，是一个数据框
  
  #
  #输出：
  #ASI_Total：每日配置收益，选股收益和交叉收益以及时间段内的总配置收益，选股收益和交叉收益，是一个矩阵
  returns <- sixcode(returns)
  weight <- sixcode(weight)
  indexweight <- sixcode(indexweight)
  
  i_loc <- unique_industry(industry)
  i_length <- unique_length(industry)
  
  indexreturn <- genReturn(returns,indexweight)
  
  index_return <- addCol(returns,indexreturn)
  index_weight <- addCol(returns,indexweight)
  returns[is.na(returns)]<-0
  weight[is.na(weight)]<-0
  index_return[is.na(index_return)]<-0
  index_weight[is.na(index_weight)]<-0
  
  code <- returns[1,]#股票代码哪一行存储在code里
  date <- returns[,1]
  date <- date[-1]#股票时间那一列存储在date里
  returns <- delete(returns)
  weight <- delete(weight)
  index_return <- delete(index_return)
  index_weight <- delete(index_weight)
  industry <- industry[-1,]
  
  returns <- toMatrix(returns)
  weight <- toMatrix(weight)
  index_return <- toMatrix(index_return)
  index_weight <- toMatrix(index_weight)
  
  index <- index_return*index_weight#投资组合每一只股票每一天Wi*Ri的矩阵
  index[is.na(index)] <- 0
  real_Return <- returns*weight#基准组合每一只股票每一天Wi*Ri的矩阵
  real_Return[is.na(real_Return)] <- 0
  
  Wb <- count_RW(index_weight,i_length,i_loc)#Wb为基准组合每个行业的权重
  Wb[is.na(Wb)] <- 0
  Wp <- count_RW(weight,i_length,i_loc)#Wp为投资组合每个行业的权重
  Wp[is.na(Wp)] <- 0
  Rb <- count_RW(index,i_length,i_loc)/Wb#Rb为基准组合每个行业的收益率
  Rb[is.na(Rb)] <- 0
  Rp <- count_RW(real_Return,i_length,i_loc)/Wp#Rp为投资组合每个行业的收益率
  Rp[is.na(Rp)] <- 0
  
  
  if(j==0){
    #算每天的总基准收益率rb，总投资收益率rp，为一个矩阵，列均复制为相同，便于下面的矩阵加减运算
    rb <- matrix(rowSums(index),ncol=28,nrow=1610)
    rp <- matrix(rowSums(real_Return),ncol=28,nrow=1610)
    
    AR <- rowSums((Wp-Wb)*((1+Rb)/(1+rb)-1))#每天的配置收益
    SR <- rowSums(Wb*(Rp-Rb)/(1+rb))#每天的个股选择收益
    IR <- (1+rp[,1])/(1+rb[,1])/(1+AR)/(1+SR)#每天的交叉收益
    ASI <- cbind(AR,SR,IR)#将每天的配置收益，个股选择收益和交叉收益列合并成一个矩阵
    
    #多期Brinson模型总超额收益
    AR_Total_plus1 <- 1
    SR_Total_plus1 <- 1
    RP_plus1 <- 1
    RB_plus1 <- 1
    for(i in 1:nrow(ASI)){
      AR_Total_plus1 <- AR_Total_plus1*(ASI[i,1]+1)#计算多期总配置收益再加1的值
      SR_Total_plus1 <- SR_Total_plus1*(ASI[i,2]+1)#计算多期总选股收益再加1的值
      RP_plus1 <- RP_plus1*(rp[i,1]+1)#计算多期总投资组合收益再加1的值
      RB_plus1 <- RB_plus1*(rb[i,1]+1)#计算多期总基准组合收益再加1的值
    }
    IR_Total_plus1 <- RP_plus1/RB_plus1/AR_Total_plus1/SR_Total_plus1#计算多期总交叉收益再加1的值
    ASI_Total <- rbind(ASI,c(AR_Total_plus1-1,SR_Total_plus1-1,IR_Total_plus1-1))
    date <- c(date,"total")
    ASI_Total <- data.frame(cbind(date,ASI_Total))
  }
  else if(j ==1){
    loc <- i_loc[1:i_length[j]]
    W_bi <- index_weight[,loc]/Wb[,1]
    W_pi <- weight[,loc]/Wb[,1]
    r_bi <- index_return[,loc]
    r_pi <- returns[,loc]
    AR_i <- rowSums((W_pi-W_bi)*((1+r_bi)/(1+Rb[,j])-1))#每天的配置收益
    SR_i <- rowSums(W_bi*(r_pi-r_bi)/(1+Rb[,j]))#每天的个股选择收益
    IR_i <- (1+Rp[,j])/(1+Rb[,j])/(1+AR_i)/(1+SR_i)#每天的交叉收益
    ASI_Total <- data.frame(cbind(rep(industry[loc[1],3],1610),date,AR_i, SR_i, IR_i))
  }
  else{
    loc <- i_loc[(sum(i_length[1:(j-1)])+1):(sum(i_length[1:j]))]
    W_bi <- index_weight[,loc]/Wb[,j]
    W_pi <- weight[,loc]/Wb[,j]
    r_bi <- index_return[,loc]
    r_pi <- returns[,loc]
    W_bi[is.na(W_bi)] <- 0  
    W_pi[is.na(W_pi)] <- 0
    AR_i <- rowSums((W_pi-W_bi)*((1+r_bi)/(1+Rb[,j])-1))#每天的配置收益
    SR_i <- rowSums(W_bi*(r_pi-r_bi)/(1+Rb[,j]))#每天的个股选择收益
    IR_i <- (1+Rp[,j])/(1+Rb[,j])/(1+AR_i)/(1+SR_i)#每天的交叉收益
    ASI_Total <- data.frame(cbind(rep(industry[loc[1],3],1610),date,AR_i, SR_i, IR_i))
  }
  return(ASI_Total)
}
#---------------执行部分----------------
returns <- loadfile("return.csv")
weight <- loadfile("weight.csv")
industry <- loadfile("industry.csv")
indexweight <- loadfile("indexWeight.csv")

#下面计算简单多期Brinson模型的超额收益归因
ASI_Total <- gen_Matrix(0,returns,weight,industry,indexweight)
write.table(ASI_Total,file="Brinson Result",append = FALSE, quote = FALSE, sep = " ")

#下面计算Brinson模型细分到行业的超额收益归因，共28个行业
for(i in 1:28){
  ASI_i <- gen_Matrix(i,returns,weight,industry,indexweight)
  write.table(ASI_i,file=as.character(ASI_i[1,1]),append = FALSE, quote = FALSE, sep = " ")
}
