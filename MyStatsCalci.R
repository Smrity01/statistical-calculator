calculate_mean <- function(data){
  #data_len   :length of the data entered
  #data       : the data entered by user(as vector)
  #total      : summation of Xi's
  
  data_len <- length(data)
  total <- 0
  for (i in 1:data_len){
    total <- total + as.numeric(data[i])
  }
  answer <- total/data_len
  mean1 <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(mean1))
}

mode <- function(data){
  #unique_data  : unique values in the data 
  #count        : contains the count of each unique element
  #index        : index of element with Maximum count
  #mode_value   : return the mode of the data
  unique_data <- unique(data)
  count <<- array()
  
  for ( i in 1:(length(unique_data)))
  {
    len <- length(grep(unique_data[i],data))
    count[i] <<- len
  }  
  index <- which(count == max(count))
  
  if( length(index) == 1 )
  {
    mode_value  <- unique_data[index]  
    ;return(mode_value)
  }
  else 
  {
    ;return('ERROR: Please check your data')
  }  
  
}

cal_median<-function(data){
  #data           :sorted data
  #data_len       :length of the user entered data
  #median_element :first median if data is even no. of length
  #median_element2:second median  if data is even no. of length
  #median_avg     :median average of two medians
  data <- sort(data)
  data_len <- length(data)
  
  if ((data_len %% 2) == 0)
  {
    index <- data_len/2
    index2 <- index+1
    median_element <- data[index]
    median_element2 <- data[index2]
    answer <- (as.numeric(median_element) + as.numeric(median_element2))/2
    median_avg <- formatC(answer,digits = 6,format = "f")
    return(median_avg)
  }
  else {
    index <- (data_len+1)/2
    answer <- data[index]
    median_element <- formatC(answer,digits = 6,format = "f")
    return(median_element)
  }
  
  
}

cal_quartiles<-function(data){
  #data_len   :length of the user entered data
  #sorted     :sorted data
  #quartile   :vector that contain all three quartiles
  
  data_len <-length(data)
  sorted <- sort(data)
  #cat('\n sorted data:',sorted)
  q1_index <- as.numeric(data_len/4)+1
  q2 <- cal_median(data)
  q3_index <- as.numeric((3*data_len)/4)+1
  
  answer <- c(sorted[q1_index],q2,sorted[q3_index])
  quartile <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(quartile))
  
  
}

cal_IQR<-function(data){
  #quartile  : three quartiles of the data
  #iqr_data  : find the iqr using three quartiles by calling quartile function
  
  quartile <- cal_quartiles(data)
  answer <- quartile[3]-quartile[1]
  iqr_data <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(iqr_data))
}

cal_skewness<-function(data){
  #quartile      :three quartiles of the data
  #skewness_data :Skewness of the data
  quartile <- cal_quartiles(data)
  answer <- ((quartile[3]+quartile[1])-(2*quartile[2])) / (IQR(data))
  skewness_data <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(skewness_data))
}
minimum<-function(data){
  # sorted  : sorted data
  # return first element of sorted data
  sorted <- sort(data)
  return(sorted[1])
}
maximum<-function(data){
  # sorted  : sorted data
  # return last element of sorted data
  sorted <- sort(data)
  data_len <- length(data)
  return(sorted[data_len])
}

cal_range<-function(data){
  # sorted    : sorted data
  # range_data: return range of the data
  sorted_data <- sort(data)
  data_len <- length(data)
  answer <- as.numeric(sorted_data[data_len]) - as.numeric(data[1])
  range_data <- formatC(answer,digits = 6,format = "f")
  return(range_data)
}

mean_absolute_dev<-function(data){
  # sorted   : sorted data
  # data_len : length of the data
  # madev    : mean absolute value of the data
  data_len <- length(data)
  mean_data <- calculate_mean(data)
  summition <- 0
  for (i in 1:data_len){
    
    summation <-  summation + abs(data[i]-mean_data)
  }
  
  answer <- summation/data_len
  madev <- formatC(answer,digits = 6,format = "f")
  return(madev)
}

variance<-function(data)
{ #data_len:  length of the data
  #total   : sum of square of Xi's
  #answer  : calculation of variance
  data_len <- length(data)
  total <- 0
  for (i in 1:data_len){
    product <- 1
    product <- as.numeric(data[i]) * as.numeric(data[i])
    total <- total + product
  }
  mean_data <- calculate_mean(data)
  answer <- (total-(data_len * mean_data * mean_data)) / (data_len-1)
  var <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(var))
  
  
}


standard_dev<-function(data){
  #square root of variance
  answer <- sqrt(variance(data))
  sd <- formatC(answer,digits = 6,format = "f")
  return(sd)
}

temp_dev<-function(data,pow,arbitrary){
  summation <- 0
  data_len <- length(data)
  mean_data <- as.numeric(arbitrary)
  for (i in 1:data_len){
    summation <-  summation + ((as.numeric(data[i])-mean_data)**pow)
  }
  answer<- summation / data_len
  return(answer)
  
}
moments<-function(data,arbitrary){
  m<-temp_dev(data,1,arbitrary)
  m1<-formatC(m,digits = 6,format = "f")
  m2<-formatC(temp_dev(data,2,arbitrary),digits = 6,format = "f")
  m3<-formatC(temp_dev(data,3,arbitrary),digits = 6,format = "f")
  m4<-formatC(temp_dev(data,4,arbitrary),digits = 6,format = "f")
  moment<-c(m1,m2,m3,m4)
  return(moment)
  
}

cal_kurtosis<-function(data){
  moment<-moments(data)
  var<-variance(data)
  m4<-moment[4]
  m4f <- as.numeric(m4)
  k<-(m4f/(var**2))-3
  return(k)
}


descriptive_analysis<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------DESCRIPTIVE ANALYSIS---------')
    #cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Mean \n 2.Median \n 3.Mode \n 4.Variance \n 5.Standard Deviation \n 6.Mean Absolute Deviation \n 7.Range \n 8.Quartiles \n 9.IQR \n 10.Minimum \n 11.Maximum \n 12.Skewness \n 13.Kurtosis \n 14.Moments')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('*****Mean of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********MEAN OF THE DATA**********
      m_data <- calculate_mean(data)
      cat('\n \n Mean of the data is: ',m_data)
      
    }
    else if (choice1==2){
      print('\n *****Median of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #******MEDIAN OF THE DATA***********
      med_data<-cal_median(data)
      cat('\n \n Median of the data: ',med_data)
      
    }
    else if (choice1==3){
      print('\n *****Mode of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #******MODE OF THE DATA***********
      mode_data <- mode(data)
      cat('\n \n Mode of the data: ',mode_data)
      
    }
    else if (choice1==4){
      print('\n *****Variance of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********VARIANCE OF THE DATA************
      var_data<-variance(data)
      cat('\n \n Variance of the data is :',var_data)
      
    }
    else if (choice1==5){
      print('\n *****Standard deviation of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #**********STANDARD DEVIATION OF THE DATA********
      sd_data<-standard_dev(data)
      cat('\n \n Standard Deviation of the data is :',sd_data) 
      
    }
    else if (choice1==6){
      print('\n *****Mean absolute deviation of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #***********MEAN ABSOLUTE OF THE DATA **********
      mean_a_dev<-mean_absolute_dev(data)
      cat('\n \n Mean Absolute Deviation of the data is : ',mean_a_dev)
      
    }
    
    else if (choice1==7){
      print('\n *****Range of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********RANGE OF THE DATA***************
      range_of_data<-cal_range(data)
      cat('\n \n Range of the data is: ',range_of_data)
      
    }
    else if (choice1==8){
      print('\n *****Quartiles of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*******QUARTILE OF THE DATA**************
      quartile_data<-cal_quartiles(data)
      cat('\n \n quartiles of the data is : Q1=',quartile_data[1],' Q2=',quartile_data[2],' Q3=',quartile_data[3])
      
    }
    else if (choice1==9){
      print('\n *****IQR of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********IQR OF THE DATA**************
      iqr_of_data<-cal_IQR(data)
      cat('\n \n IQR of the data is: ',iqr_of_data)
      
    }
    else if (choice1==10){
      print('\n *****Minimum of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********MINIMUM OF THE DATA*****
      min<-minimum(data)
      cat('\n \n Minimum of the data: ',min)
      
    }
    
    else if (choice1==11){
      print('\n *****Maximum of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********MAXIMUMOF THE DATA******
      max<-maximum(data)
      cat('\n \n Maximum of the data: ',max)
      
    }
    else if (choice1==12){
      print('\n *****Skweness of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #********SKEWNESS OF THE DATA*********
      skewness_of_data<-cal_skewness(data)
      cat('\n \n Skewness of the data: ',skewness_of_data)
      
    }
    else if (choice1==13){
      print('\n *****Kurtosis of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      kur <- cal_kurtosis(data)
      cat('\n \n Kurtosis of the data: ',kur)
      if (kur == 0){
        cat('\n Mesokurtic data')
      }
      else if(kur < 0 ){
        cat('\n Platykurtic data')
      }
      else {
        cat('\n Leptokurtic data')
      }
    }
    else if (choice1==14){
      print('\n *****Moments of the data*****')
      data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #*********MOMENTS OF THE DATA*******
      choice2 <- readline(prompt = '1.Central moment 2. Arbitrary moment: ')
      if(choice2 == 1){
        mean_value <- calculate_mean(data)
        moments_data<-moments(data,mean_value)
         cat('\n \n -----Moments--- \n m1=',moments_data[1],'m2=',moments_data[2],'m3=',moments_data[3],'m4=',moments_data[4])
      }
      else{
        arbitrary <- readline(prompt = 'Enter the arbitrary value: ')
        moments_data<-moments(data,arbitrary)
        cat('\n \n ^^Moments^^ \n m1=',moments_data[1],'m2=',moments_data[2],'m3=',moments_data[3],'m4=',moments_data[4])
      }
      
    }
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = 'Do you want to continue with descriptive analysis (yes/no): ')
    
  }
}

correlation <- function(X,Y){
  
  n <- length(X)  #n is no. of data points & is same for X and Y
  X <- as.numeric(X)
  Y <- as.numeric(Y)
  Sxx = as.numeric(sum(X**2)) - (as.numeric(sum(X))**2)/n
  Syy = as.numeric(sum(Y**2)) - (as.numeric(sum(Y))**2)/n
  Sxy = as.numeric(sum(X*Y)) - (as.numeric(sum(X))*as.numeric(sum(Y)))/n
  
  corCoeff = Sxy/(Sxx*Syy)^(1/2)
  return(corCoeff)
  
}

#cor(X,Y)

sum_vec <- function(vec)
{
  n <- length(vec)  
  s <- 0
  
  for(i in 1:n)
  {
    s <- s + as.numeric(vec[i])
  }
  return(s)
}


multiple_regression <- function(x1,x2,y)
{
  n=length(x1)
  x1 <- as.numeric(x1)
  x2 <- as.numeric(x2)
  y <- as.numeric(y)
  sumx1 <- sum_vec(x1)
  sumx2 <- sum_vec(x2)
  x1x2_prod <- x1*x2
  sum_x1x2_prod <- sum_vec(x1x2_prod)
  x1Sq <- x1^2
  x2Sq <- x2^2
  sum_x1Sq <- sum_vec(x1Sq)
  sum_x2Sq <- sum_vec(x2Sq)
  
  
  mat1 <- matrix(c(n,sumx1,sumx2,
                   sumx1,sum_x1Sq,sum_x1x2_prod,
                   sumx2,sum_x1x2_prod,sum_x2Sq), nrow=3)
  
  sumy <- sum_vec(y)
  x1Prody <- x1*y
  sum_x1Prody <- sum_vec(x1Prody)
  x2Prody <- x2*y
  sum_x2Prody <- sum_vec(x2Prody)
  
  mat2 <- matrix(c(sumy,
                   sum_x1Prody,
                   sum_x2Prody) , nrow=3, ncol=1)
  
  library(matlib)
  inv_mat1 <- inv(mat1)
  coeff_mat <- inv_mat1 %*% mat2
  
  return(coeff_mat)
  
}



predictive_analysis<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------PREDICTIVE ANALYSIS---------')
    cat('\n 1.CORRELATION \n 2.MULTIPLE REGRESSION') 
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Correlation*****')
      X <- as.vector(strsplit(readline(prompt= "Enter data for X (comma-separated list) \n"), ",")[[1]])
      Y <- as.vector(strsplit(readline(prompt= "Enter data for Y (comma-separated list) \n"), ",")[[1]])
      cc <- correlation(X,Y)
      cat('\n \n Correlation coefficient : ',cc)
    }
    
    else if(choice1==2){
      print('\n ***** Multiple Regression *****')
      x1 <- as.vector(strsplit(readline(prompt= "Enter data for x1 (comma-separated list) \n"), ",")[[1]])
      x2 <- as.vector(strsplit(readline(prompt= "Enter data for x2 (comma-separated list) \n"), ",")[[1]])
      Y <- as.vector(strsplit(readline(prompt= "Enter data for Y (comma-separated list) \n"), ",")[[1]])
      
      cm <- multiple_regression(x1,x2,Y)    #cm : coefficient matrix
      cat("\n \n Regression model : \n")
      cat("Y = ",cm[1]," + ",cm[2],"X1 + ",cm[3],"X2")
    }
    
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with predictive analysis (yes/no): ')
    
  }
}


fact<-function(number){
  prod <- 1
  for( i in 1:number){
    prod <- prod*i
  }
  return(prod)
}
permutation<-function(n,r){
  num <- fact(n)
  diff <- fact(n-r)
  permut <- (num/diff)
  return(permut)
  
}

combination<-function(n,r){
  fact_n <- fact(n)
  diff <- fact(n-r)
  #print(diff)
  fact_r <- fact(r)
  #print(fact_r*diff)
  answer <- (fact_n/(fact_r*diff))
  comb <- formatC(answer,digits = 6,format = "f")
  return(as.numeric(comb))
  
}

basic_probability <-function(favorable,total){
  answer<- (favorable/total)
  prob <- formatC(answer,digits = 6,format = "f")
  return(prob)
}


intersection <- function(set1,set2){
  count<-0
  len_set1<-length(set1)
  len_set2<-length(set2)
  
  for (i in 1:len_set1){
    for (j in 1:len_set2){
      if (set1[i]==set2[j]){
        count<-count+1
      }
    }
  }
  
  return(count)
  
}

conditional_prob <- function(setA,setB){
  num_intersection <- intersection(setA,setB)
  num_A <- length(setA)
  num_B <- length(setB)
  prob1 <- num_intersection / num_B
  prob2 <- num_intersection / num_A
  answer<-c(prob1,prob2)
  return(answer)
}

bayes <- function(prob_Ai,B_condAi,n){
  prob_Ai_len <- length(prob_Ai)
  sum <- 0
  numerator <- as.numeric(prob_Ai[n]) * as.numeric(B_condAi[n])
  
  #prod <- as.numeric(prob_Ai) * as.numeric(B_condA)
  #sum <- sum(prod)
  for (i in 1:prob_Ai_len){
    prod <- as.numeric(prob_Ai[i]) * as.numeric(B_condAi[i])
    sum <- sum + prod
  }
  answer<-(numerator/sum)
  p<-formatC(answer,digits = 6,format = "f")
  return(p)
  
}

probability_analysis <- function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------PROBABILITY ANALYSIS---------')
    #cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Permutation \n 2.combination \n 3.Basic Probability \n 4.Conditional Probability \n 5.Bayes Theorem \n ')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Permutation*****')
      n <- as.numeric(readline(prompt= "Enter the n: \n"))
      r <- as.numeric(readline(prompt= "Enter the r: \n"))
      #********Permutation**********
      p <- permutation(n,r)
      cat('\n Permuation: ',p)
      
      
    }
    else if (choice1==2){
      print('\n *****combination*****')
      n <- as.numeric(readline(prompt = 'Enter n: '))
      r <- as.numeric(readline(prompt = 'Enter r: '))
      #******Combination***********
      solution <- combination(n,r)
      cat('\n \n combination ',solution)
    }
    else if (choice1==3){
      print('\n *****Basic Probability *****')
      favourable <- as.numeric(readline(prompt = 'Enter favourable outcome: '))
      total <- as.numeric(readline(prompt = 'Enter total outcome: '))
      #******Basic Probability ***********
      bp <- basic_probability(favourable,total)
      cat('\n \n Basic Probability : ',bp)
      
    }
    else if (choice1==4){
      print('\n *****Conditional Probability*****')
      setA <- as.vector(strsplit(readline(prompt= "Enter the Set A (comma-separated list):"), ",")[[1]])
      setB <- as.vector(strsplit(readline(prompt= "Enter the setB (comma-separated list) \n"), ",")[[1]])
      
      #********Conditional Probability************
      prob<-conditional_prob(setA,setB)
      cat('\n \n Conditional Probability of P(A|B):',prob[1])
      cat('\n Conditional Probability of P(B|A):',prob[2])
    }
    else if (choice1==5){
      print('\n *****Bayes Theorem*****')
      #**********Bayes Theorem********
      prob_Ai<-as.vector(strsplit(readline(prompt= "Enter the Ai (comma-separated list):"), ",")[[1]])
      B_condAi<-as.vector(strsplit(readline(prompt= "Enter the B|Ai (comma-separated list):"), ",")[[1]])
      n<-as.integer(readline(prompt = "Enter which probability you want(value of 'i' in Ai|B) [enter as integer]:"))
      prob<-bayes(prob_Ai,B_condAi,n)
      cat('\n \n "Bayes Theorem" Probability (A',n,'|B):',prob) 
      
    }
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with probability analysis (yes/no): ')
    
  }
}


discrete_Uniform_Dist <- function(N){
  #N is no. of elements
  pdf = 1/N
  return(pdf)
}


discrete_bernoulli_dist <- function(x,p){
  
  if(x==1){
    return(p)
  }
  else if(x==0){
    return(1-p)
  }
  else
    return(0)
}


discrete_binomial_dist <- function(x,n,p){
  
  #Applying combination function here
  #nCx = factorial(n)/(factorial(x)*factorial(n-x))
  nCx = combination(n,x)
  res = nCx * p^x * (1-p)^(n-x)
  return(res)
}

#stats function : dbinom


discrete_Geometric_Dist <- function(x,p){
  q=1-p
  return((q^(x-1))*p)
}


discrete_HyperGeometric_Dist <- function(N,M,n,x){
  
  #will use combination function here
  
  # M C x
  #num1 = factorial(M)/(factorial(x)*factorial(M-x))
  num1 = combination(M,x)
  
  # (N-M) C (n-x)
  # num2 = factorial(N-M)/(factorial(n-x)*factorial((N-M)-(n-x)))
  num2 = combination(N-M,n-x)
  
  #denominator N C n
  den = fact(N)/(fact(n)*fact(N-n))
  
  return((num1*num2)/den)
}


discrete_NegativeBinomial_Dist <- function(x,r,p){
  
  #will use combination function here
  # x-1 C r-1
  # p1 = factorial(x-1)/(factorial(r-1)*factorial((x-1)-(r-1)))
  p1 = combination(x-1,r-1)
  
  return(p1*(p^r)*((1-p)^(x-r)))
}


discrete_Poisson_Dist <- function(x,lambda,t){
  
  return((((lambda*t)^x)*(2.718^(-lambda*t)))/ factorial(x))
}


discrete_Multinomial_Dist <- function(n,p){
  
  n <- as.numeric(n)
  p <- as.numeric(p)
  #where n & p are vectors having length k
  k = length(n)
  nSum = sum(n)
  
  #initializing
  nFact_Prod = 1
  pProd = 1
  
  for(i in 1:k){
    nFact_Prod = (fact(n[i])) * nFact_Prod
  }
  
  for(i in 1:k){
    pProd = ((p[i])^(n[i])) * pProd
  }
  
  return(((fact(nSum))/nFact_Prod)*pProd)
}


discrete_Multivariate_Hypergeometric_Dist <- function(M,x){
  
  # M and x are vectors of length k
  M <- as.numeric(M)
  x <- as.numeric(x)
  
  k = length(M)
  N = sum(M)
  n = sum(x)
  num = 1 #intialzing numerator num
  
  for(i in 1:k){
    
    #use combination function here
    # Mi C xi
    # num = (factorial(M[i])/(factorial(x[i])*factorial(M[i]-x[i]))) * num
    num = combination(M[i],x[i]) * num
  }
  
  #use combination function here
  # N C n
  # den = factorial(N)/(factorial(n)*factorial(N-n))
  den = combination(N,n)
  return(num/den)
}



discete_distribution <- function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------DISCRETE DISTRIBUTION---------')
    cat('\n 1.Uniform \n 2.Bernoulli \n 3.Binomial \n 4.Geometric \n 5.Hyper Geometric \n 6. Negative Binomial \n 7.Poisson \n 8. Multinomial \n 9. Multivariate Hypergeometric \n')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Uniform*****')
      N <- as.numeric(readline(prompt= "Enter no. of elements N: \n"))
      dud <- discrete_Uniform_Dist(N)
      cat('\n Discrete Uniform Distribution pdf : ',formatC(dud,digits = 6,format = "f"))
    }
    
    else if (choice1==2){
      print('\n *****bernoulli*****')
      x <- as.numeric(readline(prompt = 'Enter x: '))
      p <- as.numeric(readline(prompt = 'Enter p: '))
      dbd <- discrete_bernoulli_dist(x,p)
      cat('\n \n Discrete Bernoulli Distribution : ',formatC(dbd,digits = 6,format = "f"))
    }
    
    else if (choice1==3){
      print('\n *****Binomial*****')
      x <- as.numeric(readline(prompt = 'Enter x: '))
      n <- as.numeric(readline(prompt = 'Enter n: '))
      p <- as.numeric(readline(prompt = 'Enter p: '))
      dbd <- discrete_binomial_dist(x,n,p)
      cat('\n \n Discrete Binomial Distribution : ',formatC(dbd,digits = 6,format = "f"))
    }
    
    else if (choice1==4){
      print('\n *****Geometric*****')
      x <- as.numeric(readline(prompt = 'Enter x: '))
      p <- as.numeric(readline(prompt = 'Enter p: '))
      dgd <- discrete_Geometric_Dist(x,p)
      cat('\n \n Discrete Geometric Distribution : ',formatC(dgd,digits = 6,format = "f")) 
    }
    
    else if (choice1==5){
      
      print('\n *****Hyper-Geometric*****')
      N <- as.numeric(readline(prompt = 'Enter N: '))
      M <- as.numeric(readline(prompt = 'Enter M: '))
      n <- as.numeric(readline(prompt = 'Enter n: '))
      x <- as.numeric(readline(prompt = 'Enter x: '))
      dhg <- discrete_HyperGeometric_Dist(N,M,n,x)
      cat('\n \n Discrete HyperGeometric Distribution : ',formatC(dhg,digits = 6,format = "f"))
    }
    
    else if (choice1==6){
      
      print('\n *****Negative Binomial*****')
      x <- as.numeric(readline(prompt = 'Enter x: '))
      r <- as.numeric(readline(prompt = 'Enter r: '))
      p <- as.numeric(readline(prompt = 'Enter p: '))
      dnb <- discrete_NegativeBinomial_Dist(x,r,p)  
      cat('\n \n Discrete Negative Binomial Distribution : ',formatC(dnb,digits = 6,format = "f"))
    }
    
    else if (choice1==7){
      print('\n ***** Poisson *****')
      x <- as.numeric(readline(prompt = 'Enter x: '))
      lambda <- as.numeric(readline(prompt = 'Enter lambda: '))
      t <- as.numeric(readline(prompt = 'Enter t: '))
      dpd <- discrete_Poisson_Dist(x,lambda,t)
      cat('\n \n Discrete Poisson Distribution : ',formatC(dpd,digits = 6,format = "f"))
    }
    
    else if (choice1==8){
      print('\n *****Multinomial*****')
      n <- as.vector(strsplit(readline(prompt= "Enter data for n (comma-separated list) \n"), ",")[[1]])
      p <- as.vector(strsplit(readline(prompt= "Enter data for p (comma-separated list) \n"), ",")[[1]])
      dmd <- discrete_Multinomial_Dist(n,p)
      cat('\n \n Discrete Multinomial Distribution : ',formatC(dmd,digits = 6,format = "f"))
    }
    
    else if (choice1==9){
      print('\n *****Multivariate Hyper-Geometric*****')
      M <- as.vector(strsplit(readline(prompt= "Enter data for M (comma-separated list) \n"), ",")[[1]])
      x <- as.vector(strsplit(readline(prompt= "Enter data for x (comma-separated list) \n"), ",")[[1]])
      dmh <- discrete_Multivariate_Hypergeometric_Dist(M,x) 
      cat('\n \n Discrete Multivariate Hypergeometric Distribution : ',formatC(dmh,digits = 6,format = "f"))
    }  
    
    
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with Discrete Distribution Functions (yes/no): ')
    
  }
}


uniform<-function(a,b,lower,upper){
  
  answer <- (1/(b-a))*(upper-lower)
  udf <- formatC(answer,digits = 6,format = "f")
  return(udf)
}
normal <- function(pop_mean,pop_var){
  f1 <- function(x){
    numerator <- exp(-((x - pop_mean)**2 / (2*pop_var)))
  }
  return(f1)
  
  
}

normal_integration <- function(pop_mean,pop_var,a,b){
  numerator <- integrate(normal(pop_mean,pop_var),lower=a,upper=b)$value
  denominator <- (sqrt(pop_var * 2 * pi))
  answer <- numerator / denominator
  ndf <- formatC(answer,digits = 6,format = "f")
  return(ndf)
}

EXPO<-function(Mean){
  f1 <- function(x)
  {  
    EXP <- Mean * exp(-(Mean * x))
  }
  return(f1)
}

prob_expo <- function(Mean,lower_limit,upper_limit){
  integrate(EXPO(Mean),lower=lower_limit,upper=upper_limit)$value
  #$value <- absolute value
}

GAMMA <- function(x){
  equation <- function(t)
  {  
    EXP <- (t**(x-1)) * (exp(-t))
  }
  return(equation)
}

gammaFunction_integrate <- function(x){
  gamma_val <- integrate(GAMMA(x),lower=0,upper=Inf)$value
  #$value <- absolute value
  return(gamma_val)  
}

gamma_distribution <- function(g){
  equation <- function(x){
    ans <- (x**(g-1)) * exp(-x)
  }
  return(equation)
}
gamma_dist_integrate <- function(g,a,b){
  numerator <- integrate(gamma_distribution(g),lower=a,upper=b)$value
  denominator <- gammaFunction_integrate(g)
  answer <- numerator / denominator
  gdf <- formatC(answer,digits = 6,format = "f")
  return(gdf)
}


cont_distribution<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------CONTINUOUS DISTRIBUTION FUNCTION---------')
    #cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Uniform Distribution function\n 2.Normal Distribution function \n 3.Bivariate Normal Distribution function \n 4.Gamma Distribution function \n 5.Exponential Distribution function \n ')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Uniform Distribution function*****')
      a <- as.numeric(readline(prompt= " Enter the a: "))
      b <- as.numeric(readline(prompt= " Enter the b: "))
      c <- as.numeric(readline(prompt= " Enter the lowerlimit: "))
      d <- as.numeric(readline(prompt= " Enter the upperlimit: "))  
      #********Uniform Distribution function**********
      if (d<=b && c>=a){
        x <- uniform(a,b,c,d)
        cat('\n F(x)=',x,' when a<=x<=b \n F(x)= 0 when x<a and x>b')
      }
      else {
        print('OOPs......Your entered wrong input....TRY AGAIN')
      }
      
    }
    else if (choice1==2){
      print('\n *****Normal Distribution function*****')
      Pop_mean <- as.numeric(readline(prompt = 'Enter population mean: '))
      Pop_var <- as.numeric(readline(prompt = 'Enter population Variance: '))
      lower <- as.numeric(readline(prompt = 'Enter The lower limit: '))
      upper <- as.numeric(readline(prompt = 'Enter The upper limit: '))
      #******Normal Distribution function***********
      solution <- normal_integration(Pop_mean,Pop_var,lower,upper)
      cat('\n \n Normal Distribution function ',solution)
    }
    
    else if (choice1==3){
      print('\n *****Bivariate Normal Distribution function *****')
      #data <- as.vector(strsplit(readline(prompt= "Enter the data (comma-separated list) \n"), ",")[[1]])
      #******Bivariate Normal Distribution function ***********
      
      #cat('\n \n Bivariate Normal Distribution function : ',)
      
    }
    else if (choice1==4){
      print('\n *****Gamma Distribution function*****')
      g <- as.numeric(readline(prompt= "Enter the gamma(mean or variance)(>=0): "))
      lower <- as.numeric(readline(prompt= "Enter the lower limit(>=0): "))
      upper <- as.numeric(readline(prompt= "Enter the upper limit: "))
      #********Gamma Distribution function************
      if(g>=0 && lower>=0){
        prob <- gamma_dist_integrate(g,lower,upper)
        cat('\n \n Gamma Distribution function :',prob)
      }
      else{
        print('OOPS...you entered wrong input....')
      }
    }
    else if (choice1==5){
      print('\n *****Exponential Distribution function*****')
      Mean <- as.numeric(readline(prompt= "\n Enter the mean: "))
      RV <- as.numeric(readline(prompt= "\n Enter the value of Random variable: "))
      user_choice <- as.numeric(readline(prompt= "\n 1.Atleast  2.Atmost Enter which Type of Random variable you want: "))
      #**********Exponential Distribution function********
      answer <- prob_expo(Mean,0,RV)
      EXP <- formatC(answer,digits = 6,format = "f")
      
      if (user_choice==1){
        cat('\n \n Exponential Distribution function :',EXP)
      }
      else{
        cat('\n \n Exponential Distribution function :',1-EXP) 
        
      }
    }
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with continuous Distribution Function (yes/no): ')
    
  }
}
estimation_of_mean <- function(n,sample_mean,variance,alpha){
  half_alpha <- alpha/2
  z_half_alpha<-qnorm(1-half_alpha)
  right <- sample_mean + (z_half_alpha * sqrt(variance/n))
  left <- sample_mean - (z_half_alpha * sqrt(variance/n))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
}

estimation_of_mean_T<-function(n,sample_mean,variance,alpha){
  half_alpha <- alpha/2
  t_half_alpha <- qt(1-half_alpha,df=n-1)
  right <- sample_mean + (t_half_alpha * sqrt(variance/n))
  left <- sample_mean - (t_half_alpha * sqrt(variance/n))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}
estimation_of_mean_difference <- function(n1,n2,X1,X2,variance1,variance2,alpha){
  mean_diff <- (X1-X2)
  half_alpha <- alpha/2
  z_half_alpha <- qnorm(1-half_alpha)
  right <- mean_diff + z_half_alpha * sqrt((variance1 / n1) + (variance2 / n2))
  left <- mean_diff - z_half_alpha * sqrt((variance1 / n1) + (variance2 / n2))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}

estimation_of_mean_difference_T <- function(n1,n2,X1,X2,variance1,variance2,alpha){
  half_alpha <- alpha/2
  degree_freedom <- n1 + n2-2
  t_half_alpha<-qt(1-half_alpha,df=degree_freedom)
  first <- (n1 - 1) * variance1
  second <- (n2 - 1) * variance2
  SP <- ( first + second )/ degree_freedom
  #print(SP)
  right <- (X1 -X2) - (t_half_alpha * sqrt(SP) * sqrt((1/n1)+(1/n2)))
  left <- (X1 - X2) + (t_half_alpha * sqrt(SP) * sqrt((1/n1)+(1/n2)))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}
estimation_of_proportion<-function(n,theta,alpha){
  half_alpha <- alpha/2
  z_half_alpha <- qnorm(1-half_alpha)
  right <- theta + z_half_alpha * sqrt((theta * (1 - theta)) / n)
  left <- theta - z_half_alpha * sqrt((theta * (1 - theta)) / n)
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}

estimation_of_proportion_diff<-function(n1,theta1,n2,theta2,alpha){
  half_alpha <- alpha/2
  z_half_alpha <- qnorm(1 - half_alpha)
  right <- (theta1 - theta2) + z_half_alpha * sqrt(((theta1 * (1 - theta1)) / n1) + ((theta2 * (1 - theta2)) / n2))
  left <- (theta1 - theta2) - z_half_alpha * sqrt(((theta1 * (1 - theta1)) / n1) + ((theta2 * (1 - theta2)) / n2))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}
estimation_of_variance<-function(n,variance,alpha){
  half_alpha <- alpha/2
  
  right <- ((n-1)*(variance**2)) / qchisq(half_alpha,n-1)
  left <- ((n-1)*(variance**2)) / qchisq(1-half_alpha,n-1)
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}

estimation_of_Ratiovariance<-function(n1,variance1,n2,variance2,alpha){
  half_alpha <- alpha/2
  
  right <- qf(1-half_alpha,n1-1,n2-1) * ((variance1**2) / (variance2**2))
  left <- (1 / qf(1-half_alpha,n1-1,n2-1)) * ((variance1**2) / (variance2**2))
  answer <- c(formatC(left,digits = 6,format = "f"),formatC(right,digits = 6,format = "f"))
  return(answer)
  
}


Interval_estimation<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------INTERVAL ESTIMATION---------')
    #cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Estimation of Mean \n 2.Estimation of difference of mean  \n 3.Estimation of proportion \n 4.Estimation of difference of proportion  \n 5.Estimation of variance 6.Estimation of ratio  of variance  \n ')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Estimation of Mean*****')
      
      n <- as.numeric(readline(prompt = "Enter the sample size: "))
      s_mean <- as.numeric(readline(prompt = "Enter the sample mean: "))
      alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
      print("1.Population variance known 2.Population variance unknown")
      ans<-as.numeric(readline(prompt = "enter choice: "))
      
      if (ans==1){
        variance <- as.numeric(readline(prompt = "Enter the population variance: "))
        eom <- estimation_of_mean(n,s_mean,variance,alpha)
        cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
      }
      else {
        if(n>30){
          variance <- as.numeric(readline(prompt = "Enter the sample variance: "))
          eom <- estimation_of_mean(n,s_mean,variance,alpha)
          cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
          
        }
        else{
          variance <- as.numeric(readline(prompt = "Enter the sample variance: "))
          eom <- estimation_of_mean_T(n,s_mean,variance,alpha)
          cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
          
        }
        
      }
      
    }
    else if (choice1==2){
      print('\n *****Estimation of difference of mean *****')
      
      n1 <- as.numeric(readline(prompt = "Enter the first sample size: "))
      X1 <- as.numeric(readline(prompt = "Enter the first sample mean: "))
      n2 <- as.numeric(readline(prompt = "Enter the second sample size: "))
      X2 <- as.numeric(readline(prompt = "Enter the second sample mean: "))
      
      alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
      print("1.Population variance known 2.Population variance unknown")
      ans<-as.numeric(readline(prompt = "enter choice: "))
      
      if (ans==1){
        variance1 <- as.numeric(readline(prompt = "Enter the first population variance: "))
        variance2 <- as.numeric(readline(prompt = "Enter the first population variance: "))
        eom <- estimation_of_mean_difference(n1,n2,X1,X2,variance1,variance2,alpha)
        cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
      }
      else {
        if(n1>30 && n2>30){
          variance1 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
          variance2 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
          eom <- estimation_of_mean_difference(n1,n2,X1,X2,variance1,variance2,alpha)
          cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
          
        }
        else{
          variance1 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
          variance2 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
          eom <- estimation_of_mean_difference_T(n1,n2,X1,X2,variance1,variance2,alpha)
          cat("confidence interval for population mean is:",eom[1],'< population_mean <',eom[2])
          
        }
        
      }
    }
    
    else if (choice1==3){
      print('\n *****Estimation of proportion *****')
      n <- as.numeric(readline(prompt = "Enter the sample size: "))
      theta <- as.numeric(readline(prompt = "Enter the proportion: "))
      alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
      eop <- estimation_of_proportion(n,theta,alpha)
      cat("confidence interval for theta is:",eop[1],'< theta <',eop[2])
      
    }
    else if (choice1==4){
      print('\n *****Estimation of proportion difference*****')
      n1 <- as.numeric(readline(prompt = "Enter the first sample size: "))
      theta1 <- as.numeric(readline(prompt = "Enter the proportion: "))
      n2 <- as.numeric(readline(prompt = "Enter the second sample size: "))
      theta2 <- as.numeric(readline(prompt = "Enter the second proportion: "))
      alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
      eop <- estimation_of_proportion_diff(n1,theta1,n2,theta2,alpha)
      cat("confidence interval for theta1-theta2 is:",eop[1],'< theta1-theta2 <',eop[2])
      
    }
    else if (choice1==5){
      print('\n *****Estimation of variance*****')
      n <- as.numeric(readline(prompt = "Enter the sample size: "))
      variance <- as.numeric(readline(prompt = "Enter the sample variance: "))
      alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
      eov <- estimation_of_variance(n,variance,alpha)
      cat("confidence interval for population variance is:",eov[1],'< population_variance <',eov[2])
      
    }
    else if (choice1==6){
      print('\n *****Estimation of ratio of variance*****')
      n1 <- as.numeric(readline(prompt = "Enter the first sample size: "))
      variance1 <- as.numeric(readline(prompt = "Enter the first sample variance: "))
      n2 <- as.numeric(readline(prompt = "Enter the second sample size: "))
      variance2 <- as.numeric(readline(prompt = "Enter the second sample variance: "))
      alpha <- as.numeric(readline(prompt = "Enter the alpha: "))
      eov <- estimation_of_Ratiovariance(n1,variance1,n2,variance2,alpha)
      cat("confidence interval for ratio of variance is:",eov[1],'< ratio ofvariance<',eov[2])
      
      
    }
    
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with Interval estimation (yes/no): ')
    
  }
}

signTest <- function(mew,set,alpha=0.05)
{
  set <- as.numeric(set) 
  n = length(set)
  
  
  pos_sign = 0
  neg_sign = 0
  
  for(i in 1:n)
  {
    val = set[i]-mew
    if(val > 0){
      pos_sign = pos_sign + 1
    }
    else if(val < 0){
      neg_sign = neg_sign + 1
    }
  }
  
  tot = pos_sign + neg_sign
  
  if(n<30)
  {
    p_value = discrete_binomial_dist(min(pos_sign,neg_sign),tot,0.5)
    # print(p_value)
  }
  else{  #n>30
    p_value = (pos_sign-n*0.5)/((n*0.5)(1-0.5))^(1/2)
  }
  
  if(p_value < alpha)
  {
    return(0)  #Null hypothesis rejected
  }
  else{
    return(1)  #Null hypothesis accepted
  }
  
}


wilcoxon_srt <- function(mew,set,reln,alpha=0.05)
{
  set <- as.numeric(set)
  diff <- c()
  n <- length(set)
  
  diff <- set - mew
  
  diff <- diff[diff!=0]
  
  rank_diff <- rank(abs(diff))
  ldiff <- length(diff)
  for(i in 1:ldiff)
  {
    if(diff[i]<0)
    {
      rank_diff[i] = -rank_diff[i] 
    }
  }
  
  
  T_neg = 0
  T_pos = 0
  
  for(i in 1:ldiff){
    
    if(rank_diff[i] < 0)
    {
      T_neg = T_neg + rank_diff[i]
    }
    else{
      T_pos = T_pos + rank_diff[i]
    }
    
  }
  
  T_neg = -T_neg
  T_cal = min(T_neg,T_pos)
  
  T_alpha = (qsignrank(alpha/2,ldiff) )- 1
  
  T_twice_alpha = (qsignrank(alpha,ldiff) ) - 1
  
  if(reln==1){ #0 : mew NOT equal mew_not
    
    if(T_cal <= T_alpha){
      return(0)  #Null hypothesis rejected
    }
    else{
      return(1)  #Null hypothesis accepted
    }
  }
  
  else if(reln==2){ #1 : mew > mew_not
    if(T_neg <= T_twice_alpha){
      return(0) #Null hypothesis rejected
    }
    else{
      return(1)  #Null hypothesis accepted
    }
  }
  
  else if(reln==3){  #2 : mew < mew_not
    if(T_pos <= T_twice_alpha){
      return(0) #Null hypothesis rejected
    }
    else{
      return(1)  #Null hypothesis accepted
    }
  }  
}


Mann_whitney_test <- function(x,y,reln,alpha)
{
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  n1 <- length(x)
  n2 <- length(y)
  
  rank_xy = rank(c(x,y))
  x_rank = rank_xy[1:n1]
  y_rank = rank_xy[(n1+1) : (n1+n2)]
  
  W1 = sum(x_rank)
  W2 = sum(y_rank)
  
  U1 = W1 - ((n1(n1+2))/2)
  U2 = W2 - ((n2(n2+2))/2)
  
  U_alpha = (qwilcox(alpha/2,n1,n2) )- 1
  
  U_twice_alpha = (qwilcox(alpha,n1,n2) ) - 1
  
  if(reln==1){ #0 : mew NOT equal mew_not
    U_cal = min(U1,U2)
    if(U_cal <= U_alpha){
      return(0)  #Null hypothesis rejected
    }
    else{
      return(1)  #Null hypothesis accepted
    }
  }
  
  else if(reln==2){ #1 : mew > mew_not
    if(U2 <= U_twice_alpha){
      return(0) #Null hypothesis rejected
    }
    else{
      return(1)  #Null hypothesis accepted
    }
  }
  
  else if(reln==3){  #2 : mew < mew_not
    if(U1 <= U_twice_alpha){
      return(0) #Null hypothesis rejected
    }
    else{
      return(1)  #Null hypothesis accepted
    }
  }  
  
}


Kruskal_Wallis_test <- function(x,y,z,alpha)
{
  x <- as.numeric(x)
  y <- as.numeric(y)
  z <- as.numeric(z)
  
  n1 <- length(x)
  n2 <- length(y)
  n3 <- length(z)
  
  rank_xyz = rank(c(x,y,z))
  x_rank = rank_xyz[1:n1]
  y_rank = rank_xyz[(n1+1) : (n1+n2)]
  z_rank = rank_xyz[(n1+n2+1) : (n1+n2+n3)]
  
  R1 = sum_vec(x_rank)
  R2 = sum_vec(y_rank)
  R3 = sum_vec(z_rank)
  
  n <- n1+n2+n3
  temp = ((R1^2)/n1) + ((R2^2)/n2) + ((R3^2)/n3)
  temp_res = (temp*12)/(n*(n+1))
  H <- temp_res - (3*(n+1))
  
  CS_val = qchisq(1-alpha,2)  #Chi-square value
  
  if(H >= CS_val)
  {
    return(0)  #Reject null hypothesis
  }
  else
    return(1)  #Accept null hypothesis
}


non_parametric <- function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------NON PARAMETRIC ANALYSIS---------')
    cat('\n 1.SIGN TEST \n 2.WILCOXON SIGNED RANK TEST  \n 3.MANN-WHITNEY TEST  \n 4.KRUSKAL WALLIS TEST  \n') 
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****SIGN TEST*****')
      set <- as.vector(strsplit(readline(prompt= "Enter data (comma-separated list) \n"), ",")[[1]])
      mew <- as.numeric(readline(prompt = 'Enter mew : '))
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      res <- signTest(mew,set,alpha)
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
      
    }
    
    else if(choice1==2){
      print('\n *****WILCOXON SIGNED RANK TEST*****')
      set <- as.vector(strsplit(readline(prompt= "Enter data (comma-separated list) \n"), ",")[[1]])
      mew <- as.numeric(readline(prompt = 'Enter mew : '))
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      
      cat('\n 1. mew not eqaul mew0 \n 2. mew > mew0  \n 3.mew < mew0 \n') 
      reln <- readline(prompt = 'ENTER YOUR CHOICE: ')
      res <- wilcoxon_srt(meW,set,reln,alpha)
      
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
    }
    
    else if(choice1==3){
      print('\n *****MANN WHITNEY TEST*****')
      X <- as.vector(strsplit(readline(prompt= "Enter data for X (comma-separated list) \n"), ",")[[1]])
      Y <- as.vector(strsplit(readline(prompt= "Enter data for Y (comma-separated list) \n"), ",")[[1]])
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      
      cat('\n 1. mew not eqaul mew0 \n 2. mew > mew0  \n 3.mew < mew0 \n') 
      reln <- readline(prompt = 'ENTER YOUR CHOICE: ')
      res <- Mann_whitney_test(x,y,reln,alpha)
      
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
    }
    
    else if(choice1==4){
      print('\n *****KRUSKAL WALLIS TEST*****')
      X <- as.vector(strsplit(readline(prompt= "Enter data for X (comma-separated list) \n"), ",")[[1]])
      Y <- as.vector(strsplit(readline(prompt= "Enter data for Y (comma-separated list) \n"), ",")[[1]])
      Z <- as.vector(strsplit(readline(prompt= "Enter data for Z (comma-separated list) \n"), ",")[[1]])
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      
      res <- Kruskal_Wallis_test(x,y,z,alpha)
      
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
    }
    
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with Non Parametric Analysis (yes/no): ')
    
  }
}



draw_histogram <- function(data){
  
  hist(as.numeric(data),xlab = 'User entered data',main="Histogram ",col=rainbow(length(data)))
}

line_graph<-function(data){
  
  plot(data,type = "o",col = rainbow(length(data)), xlab = "x-axis", ylab = "y-axis", 
       main = "line graph")
}

pie_chart<-function(data,data_name){
  pie(as.numeric(data), as.character(data_name), main = "Pie chart", col = rainbow(length(data)))
  
}

scatter_plot<-function(data_x,data_y){
  plot(x = as.numeric(data_x),y = as.numeric(data_y),xlab = "x-axis",ylab = "y-axis",main = "Scatter plot ",col=rainbow(length(data)))
}

box_plot<-function(data_x,data_y){
  boxplot(as.numeric(data_x),as.numeric(data_y),xlab = "x-axis",ylab = "y-axis",main = "Box plot",col = c("green","yellow","purple"))
}

bar_graph<-function(data,data_name){
  barplot(as.numeric(data),names.arg = as.character(data_name),xlab = "x-axis",ylab = "y-axis",col = rainbow(length(data)),main = "Bar chart",border = "red")
}


library(e1071)
stem_leaf_plot<-function(data){
  ##a stem and leaf plot of a quantative variable is a textual
  ##graph that clasifies data items according to thier most
  ##significant numeric digits
  ##scale: controls the plot
  ##atom:tolerance
  ##width desired width of plot
  
  stem(as.numeric(data),scale=1,width=80,atom=1e-08)
}

library(qcc)
paretochart<-function(data){
  
  pareto.chart(data,ylab = 'frequency',ylab2 = "cumulative percentage",cumperc = seq(0,100, by=25),main = 'pareto chart',col = rainbow(length(data)),plot=TRUE)
}


Q_Qplot<-function(data_X,data_y){
  qqplot(data_X[data_y==0],data_X[data_y==1],it=TRUE,main='Quartile Quartile plot')
  
}


Visualizations<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------Visualizations---------')
    cat('\n*****ENTER YOUR CHOICE*****')
    cat('\n 1.Histogram \n 2.Line graph \n 3.Bar graph \n 4.Pie chart \n 5.Scatter Plot \n 6.Box Plot \n 7.Quartile-Quartile Plot \n 8.Steam Leaf Plot \n 9.Pareto Chart \n')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('*****HISTOGRAM*****')
      data <- as.vector(strsplit(readline(prompt = 'enter the data (comma seperated): '),',')[[1]])
      draw_histogram(data)
      
    }
    else if (choice1==2){
      print('\n *****LINE GRAPH*****')
      data <- as.vector(strsplit(readline(prompt = 'enter the data (comma seperated): '),',')[[1]])
      line_graph(data)
      
    }
    else if (choice1==3){
      print('\n *****BAR GRAPH*****')
      data <- as.vector(strsplit(readline(prompt = 'Enter the data (comma seperated): '),',')[[1]])
      data_name <- as.vector(strsplit(readline(prompt = 'Enter the labels for each bar (comma seperated): '),',')[[1]])
      bar_graph(data,data_name)
      
    }
    else if (choice1==4){
      print('\n *****PIE CHART*****')
      data <- as.vector(strsplit(readline(prompt = 'Enter the data (comma seperated): '),',')[[1]])
      data_name <- as.vector(strsplit(readline(prompt = 'Enter the labels for each bar (comma seperated): '),',')[[1]])
      pie_chart(data,data_name)
      
      
      
    }
    else if (choice1==5){
      print('\n *****SCATTER PLOT*****')
      data_x <- as.vector(strsplit(readline(prompt = 'Enter the data x-axis(comma seperated): '),',')[[1]])
      data_y <- as.vector(strsplit(readline(prompt = 'Enter the data y -axis (comma seperated): '),',')[[1]])
      scatter_plot(data_x,data_y)
      
      
    }
    else if (choice1==6){
      
      print('\n *****BOX PLOT*****')
      data_x <- as.vector(strsplit(readline(prompt = 'Enter the data x-axis(comma seperated): '),',')[[1]])
      data_y <- as.vector(strsplit(readline(prompt = 'Enter the data y -axis (comma seperated): '),',')[[1]])
      box_plot(data_x,data_y)
      
    }
    
    else if (choice1==7){
      print('\n *****QUARTILE-QUARILE PLOT*****')
      data_x <- as.vector(strsplit(readline(prompt = 'Enter the data x-axis(comma seperated): '),',')[[1]])
      data_y <- as.vector(strsplit(readline(prompt = 'Enter the data y-axis (0 or 1) (comma seperated): '),',')[[1]])
      Q_Qplot(data_x,data_y)
      
    }
    else if (choice1==8){
      print('\n *****STEM LEAF PLOT*****')
      data <- as.vector(strsplit(readline(prompt = 'enter the data (comma seperated): '),',')[[1]])
      stem_leaf_plot(data)
      
    }
    else if (choice1==9){
      print('\n *****PARETO CHART*****')
      data_x <- as.vector(strsplit(readline(prompt = 'Enter the data x-axis(comma seperated): '),',')[[1]])
      paretochart(as.numeric(data_x))
      
    }
    
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = 'Do you want to continue with vISUALIZATION (yes/no): ')
    
  }
}
chi_square <- function(sam,sd_pop,alpha){
  
  sam <- as.numeric(sam)
  #n is sample size
  #sam is vector sample
  #sd_pop is standard deviation of population
  #alpha is level of significance
  
  n=length(sam)
  var_sam=variance(sam)
  
  cs_cal =  ((n-1)*(var_sam))/((sd_pop)^2)
  # print(cs_cal)
  
  cs_obs <- qchisq(1-alpha,n-1)
  #  print(cs_obs)
  
  if(cs_obs < cs_cal)
    return(0)  #Null hypothesis rejected
  else
    return(1)  #Null hypothesis is accepted
  
}



student_tTest <- function(sam,pop_mean,alpha){
  
  #sam : vector sample
  #pop_mean : mean of population
  #sam_var : variance of sample
  #alpha : level of significance
  #n : sample size
  sam <- as.numeric(sam)
  
  n=length(sam)
  sam_mean = calculate_mean(sam)
  sam_var = variance(sam)
  
  t_cal = (sam_mean - pop_mean)/(sam_var^(1/2)/(n^(1/2)))
  print(t_cal)
  
  t_obs = qt(alpha,n-1)
  print(t_obs)
  
  
  if(abs(t_obs) < abs(t_cal))
    return(0)  #Null hypothesis rejected
  else
    return(1)  #Null hypothesis is accepted
  
}


F_test <- function(sam1,sam2,alpha){
  
  #sam1: sample 1
  #sam2 : sample 2
  
  sam1 <- as.numeric(sam1)
  sam2 <- as.numeric(sam2)
  
  #var_sam1 : variance of sample1
  #var_sam2 : variance of sample2
  
  
  n1 <- length(sam1)
  n2 <- length(sam2)
  
  var_sam1 <- variance(sam1)
  var_sam2 <- variance(sam2)
  
  f_cal = ((n2-1)*var_sam1)/((n1-1)*var_sam2)
  
  f_obs = qf(1-alpha,n1-1,n2-1)
  
  if(f_cal < f_obs){
    print("Variances are equal")
  }
  else{
    print("Variances are not equal")
  }
  
}


z_test <- function(data,mew,sigma,alpha){
  
  data <- as.numeric(data)
  mean_val <- calculate_mean(data)
  n <- length(data)
  z_test <- (mean_val-mew)/(sigma/(n^0.5))
  z_val = pnorm(z_test)
  if(z_test > 0){
    z_val <- 1-z_val
  }
  if(z_val < alpha)
    return(0)  #Null hypothesis rejected
  else
    return(1)  #Null hypothesis accepted
}

test_statistic<-function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------SAMPLE DISTRIBUTION TEST STATISTIC---------')
    cat('\n 1.CHI-SQUARE \n 2.STUDENT T TEST  \n 3.F-TEST  \n 4.Z-TEST  \n') 
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      print('\n *****Chi-Square*****')
      sam <- as.vector(strsplit(readline(prompt= "Enter sample data (comma-separated list) \n"), ",")[[1]])
      sd_pop <- as.numeric(readline(prompt = 'Enter standard deviation of population : '))
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      res <- chi_square(sam,sd_pop,alpha)
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
      
    }
    
    else if(choice1==2){
      print('\n *****STUDENT T TEST*****')
      sam <- as.vector(strsplit(readline(prompt= "Enter sample data (comma-separated list) \n"), ",")[[1]])
      pop_mean <- as.numeric(readline(prompt = 'Enter mean of population : '))
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      res <- student_tTest(sam,pop_mean,alpha)
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
      
    }
    
    else if(choice1==3){
      print('\n *****F-TEST*****')
      sam1 <- as.vector(strsplit(readline(prompt= "Enter sample 1 data (comma-separated list) \n"), ",")[[1]])
      sam2 <- as.vector(strsplit(readline(prompt= "Enter sample 2 data (comma-separated list) \n"), ",")[[1]])
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : '))
      F_test(sam1,sam2,alpha)
    }
    
    else if(choice1==4){
      print('\n *****Z-TEST*****')
      data <- as.vector(strsplit(readline(prompt= "Enter data elements (comma-separated list) \n"), ",")[[1]])
      mew <- as.numeric(readline(prompt = 'Enter mew : '))
      sigma <- as.numeric(readline(prompt = 'Enter sigma : '))
      alpha <- as.numeric(readline(prompt = 'Enter level of significance : ')) 
      
      res = z_test(data,mew,sigma,alpha)
      if(res==0)
        print("Null hypothesis rejected")
      else
        print("Null hypothesis accepted")
    }
    
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = '\n Do you want to continue with Sample Distribution Test Statistic (yes/no): ')
    
  }
}



calculator<- function(){
  answer<-'yes'
  
  while(answer=='yes'){
    cat('\f')
    cat('\n ----------^^^^^^^^-------STATISTICAL CALCULATOR----------^^^^^^^^---------')
    cat('\n*****PLEASE ENTER YOUR CHOICE*****')
    cat('\n 1.Descriptive Analysis \n 2.Predictive Analysis \n 3.Probability Analysis \n 4.Discrete Distribution Functions \n 5.Continuous Distribution Functions \n 6.Sample Distribution Test Statistic \n 7.Interval Estimation \n 8.Non-Parametric Analysis \n 9.Visualizations\n ')
    choice1 <- readline(prompt = 'ENTER YOUR CHOICE: ')
    
    if (choice1==1){
      descriptive_analysis()
    }
    else if (choice1==2){
      predictive_analysis()
    }
    else if (choice1==3){
      probability_analysis()
      
    }
    else if (choice1==4){
      discete_distribution()
    }
    else if (choice1==5){
      cont_distribution()
    }
    else if (choice1==6){
      test_statistic()
    }
    else if (choice1==7){
      Interval_estimation()
    }
    else if (choice1==8){
      non_parametric()
      
    }
    else if (choice1==9){
      Visualizations()
    }
    else{ print('OOPS.....You Have entered a wrong choice.......')}
    answer<-readline(prompt = ' Do you want to continue with STATISTICAL CALCULATOR (yes/no): ')
    
  }
}
calculator()