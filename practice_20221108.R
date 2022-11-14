# practice 1 While Taylor

func_p1 <- function(x,error){
  
  tgt <- 1 / x
  tmp <- 1 
  cnt <- 1
  flag <- abs(tgt-tmp)
  
  while(flag > error)
  {
    tmp <- tmp + (-1)^(cnt%%2) * (x-1)^cnt 
    flag <- abs(tgt-tmp)
    cnt <- cnt+1
  }
  result <- cnt
  result
}

# practice 2 BMI

func_p2 <- function(){
  
  height <- c(170,176,154,144,160,140,150,161,166,155,141,143,150,172,150)
  weight <- c(79,69,43,53,68,56,57,76,80,65,45,51,83,77,55)
  BMI <-  weight / (height / 100) ^ 2
  BMIGroup <- rep(0, length(height))
  
  for (i in 1:length(height)){
    
    if (BMI[i]<=22) BMIGroup[i]= 1
    else if ((BMI[i]>22)&(BMI[i]<=27)) BMIGroup[i]= 2
    else if ((BMI[i]>27)&(BMI[i]<=35)) BMIGroup[i]= 3
    else BMIGroup[i]= 4
    
  }
  
  result <- data.frame(id=1:15, height,weight,BMI,BMIGroup)
  result
}


# practice 3 vector

func_p3 <- function(c1,c2){
  
  resultlen <- ifelse(length(c1)>length(c2),length(c1),length(c2))
  result <- rep(0,resultlen)
  
  for(i in 1:resultlen){
    if(is.na(c1[i])){
      result[i] <- c2[i]
    }else if(is.na(c2[i])){
      result[i] <- c1[i]  
    }else{
      result[i] <- c1[i]+c2[i]
    }
  }
  
  result
}

# practice 4

func_p4 <- function(){
  rowNames <- seq(0,3,0.1)
  colNames <- seq(0,0.09,0.01)
  result <- matrix(0, length(rowNames),length(colNames))
  
  for(i in 1:length(rowNames))
  {
    for(j in 1:length(colNames))
    {
      q1=rowNames[i]
      q2=colNames[j]
      q=q1+q2
      result[i,j]=round(pnorm(q),4)
      
    }
  }
  colnames(result)=paste("0.0",0:9, sep="")
  rownames(result)=format(seq(0.0,3.0,0.1))#format讓1顯示成1.0
  result
}

# main
main <- function(){
  repeat{
    
    option <- readline(prompt="1)While Taylor 2)BMI 3)vector sum 4) normal (key in -1 to quit) : \n")
    
    if(option==-1) {
      
      print('quit.')
      break
      
    }else if(option==1){
      
      #利用while迴圈做1/x在x=1的泰勒展式：
      #若x=0.3，試求泰勒展式要加總到第幾項，才會跟實際值 1/0.3＝3.3333 差距在0.00001以內。
      #使用(-1)^(i%%2)來控制每項的正負交錯；%%是在求餘數。
      x <- readline(prompt = "X : ")
      error <- readline((prompt="error :　"))
      print(paste('加總到第',toString(func_p1(as.numeric(x),as.numeric(error))),'項會跟實際值 1/',toString(x),'＝',toString(1/as.numeric(x)),'差距在0.00001以內'))
      
    }else if(option==2){
      
      r_p2 <- func_p2()
      
    }else if(option==3){
      
      c1 <- c(1:10,5)
      c2 <- c(5:30,12)
      c3 <- c(TRUE,TRUE,TRUE,FALSE)
      c4 <- c(TRUE,FALSE,TRUE,FALSE)
      c5 <- c(1,2,0,5)
      
      r1 <- func_p3(c1,c2)
      r1
      
      r2 <- func_p3(c3,c4)
      r2
      
    }else if(option==4){
      
      r_p4 <- func_p4()
      
    }else
    {
      print('Invalid input,please try again.')
    }
  }
  print('program stopped.')
}

main()