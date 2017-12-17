# STATISTICAL CALCULATOR

## Functionalities

* Module 1: Descriptive Analysis
* Module 2: Predictive Analysis
* Module 3: Probability Analysis
* Module 4: Discrete Distribution functions
* Module 5: Continuous Distribution Function
* Module 6: Sample Distribution Test Statistics
* Module 7: Interval Estimation
* Module 8: Non Parametric Analysis
* Module 9: Visualization

## Prerequisites

Need to install this library: 
> qcc  : QUALITY CONTROL CHARTS, library is used fot pareto.chart()
> matlib : this library is used for integration function

## Assumptions

 * Different Analysis are performed only for ungrouped data.
 * We have defined only central and absolute moment.
 * For mode, we are considering only unimodal data.
 
 ## User Defined Functions and their Local variables
 
 ### descriptive_analysis()          : function to perform these functions
				

* calculate_mean(data)         : calculates mean of data(vector taken from user)
> data_len   :length of the data entered
> data        : the data entered by user(as vector)
> 				 total        : summation of Xi's

* cal_median(data)	 : calculates median of data(vector taken from user)
> data                  :sorted data
>	data_len             :length of the user entered data
>	median_element  :first median if data is even no. of length
>	median_element2 :second median  if data is even no. of length
>	median_avg         :median average of two medians

* mode(data)        : calculates mode of data(vector taken from user)
>	unique_data   : unique values in the data 
>	count             : contains the count of each unique element
>	index             : index of element with Maximum count
>	mode_value   : return the mode of the data
  
* variance(data)  : calculates variance of data(vector taken from user)
> data_len :  length of the data
> total       : sum of square of Xi's
> answer   : calculation of variance

* standard_dev(data)        : calculates standard deviation of data(vector taken from user)
> return square root of variance

* mean_absolute_dev(data)    : calculates mean absolute deviation of data(vector taken from user)
> sorted    : sorted data
> data_len : length of the data
> madev    : mean absolute value of the data

* cal_range(data)         : calculates range of data(vector taken from user)
> sorted    : sorted data
> range_data: return range of the data

* cal_quartiles(data) : calculates quartiles of data(vector taken from user)
> data_len   :length of the user entered data
> sorted      :sorted data
> quartile    :vector that contain all three quartiles

* cal_IQR(data) : calculates inter quartile range of data(vector taken from user)

* minimum(data) : calculates minimum of data(vector taken from user)
> sorted  : sorted data
> return first element of sorted data

* maximum(data) : calculates maximum of data(vector taken from user)
> sorted  : sorted data
> return last element of sorted data

* cal_skewness(data) 	: calculates skewness of data(vector taken from user)
> quartile      :three quartiles of the data
> skewness_data :Skewness of the data

* cal_kurtosis(data) 	: calculates kurtosis of data(vector taken from user)
> using moments kurtosis is calculated AND ALSO TELL YOU ABOUT THE TYPE OF  DATA YOU ENTERED
> ************Mesokurtic data**************
> **************Platykurtic data************** 
> **************Leptokurtic data**************

* moments(data)		 : calculates moments of data(vector taken from user)
> 	CENTRAL MOMENT AND ARBITRARY MOMENTS BOTH

### predictive_analysis() 	: function to perform these 2 functions:
> 1.Correlation  2.Multiple Regression


* correlation(X,Y) 		:  function returns correlation coefficient between 2 data X and Y
  where X and Y are taken as user input vetors


* multiple_regression(x1,x2,Y)	 : function returns coefficient matrix
> And regression model is given in function predictive_analysis()
> x1, x2 and Y are user input vectors
> CALCULATING R-SQUARED VALUE FOR MULTIPLE REGRESSION

### probability_analysis() 		: function to perform these functions-
> 1.Permutation   2.combination   3.Basic Probability   4.Conditional Probability   5.Bayes Theorem   

* permutation(n,r) 			: calculates and returns permutation from user input n and r

* combination(n,r) 			: calculates and returns combination from user input n and r

* basic_probability(favourable,total)	: calculates and returns basic probability from user input favourable and total

* conditional_prob(setA,setB) 		: calculates and returns conditional probability from user input vector setA and setB

* bayes(prob_Ai,B_condAi,n)		 : calculates and returns value from bayes's theorem
> Input parameters:
> prob_Ai : input vector having probability of Ai
>  B_condAi : input vector B|Ai probaility 
> n : for which probability is desired
 
### discrete_distribution()			 : function to perform these functions:
> 1.Uniform   2.Bernoulli   3.Binomial   4.Geometric   5.Hyper Geometric   6. Negative Binomial   7.Poisson   8. Multinomial   9. Multivariate Hypergeometric

* discrete_uniform_Dist(N) 			: function returns probability distribution function of discrete uniform distribution having N no. of elements as input

* discrete_bernoulli_dist(x,p)			 : function returns 
> probability of success p for x=1
> probability of faliure 1-p for x=0
> and 0 for invalid x
> where x & p are user inputs

				
* discrete_binomial_dist(x,n,p)			 : function returns binomial distribution value for x,n and p user inputs
                                
* discrete_Geometric_Dist(x,p) 	 	: function returns geometric distribution for x and p user inputs

* discrete_HyperGeometric_Dist(N,M,n,x) 	 : function returns hyper geometric distribution for N,M,n and x user inputs

* discrete_NegativeBinomial_Dist(x,r,p)   	 : function returns negatve binomial distribution for x, r and p user inputs

* discrete_Poisson_Dist(x,lambda,t) 		: function returns poisson distribution for x, lambda and t user inputs

* discrete_Multinomial_Dist(n,p) 		: function returns multinomial distribution for n and p vector user inputs

* discrete_Multivariate_Hypergeometric_Dist(M,x) : function returns Multivariate Hypergeometric distribution for M and x vector user inputs
 
 ### cont_distribution 			: this function will call the following functions and take the input---

* Uniform Distribution function 		: Calculate the probability of random variable following uniform distribution
>   uniform(a,b,c,d)
>   c and d are the lower and upper limits of integration respectively

* Normal Distribution function 			: Calculate the probability of random variable following normal distribution
	>  normal_integration(Pop_mean,Pop_var,lower,upper)
	>	 pop_mean: population mean
	>        Pop_var   : population variance
	>	 lower and upper are the lower and upper limits of integration respectively

* Gamma Distribution function 		: Calculate the probability of random variable following gamma distribution
	>	gamma_dist_integrate(g,lower,upper)
	>	g : this variable represent shape parameter
	>	lower and upper are the lower and upper limits of integration respectively

* Exponential Distribution function 		: Calculate the probability of random variable following exponential distribution
>  	prob_expo(Mean,0,RV)
> 	mean : mean of the data
>	RV : random variable 
>     	0 :is the lower limit


### test_statistic() : function to perform these functions-
> 1. CHI-SQUARE  2. STUDENT T TEST   3.F-TEST  4.Z-TEST        


* chi_square(sam,sd_pop,alpha)	 : function calculates chi square value from parameters given by user
It compares calculated chi sqaure value from value from table And returns 0 (to signify null hypothesis is rejected) or returns 1 (to signify null hypothesis is accepted

> :Input parameters:
>       sam : sample vector from user
>       sd_pop : standard deviation of population given by user
>       alpha : level of significance
                              

* student_tTest(sam,pop_mean,alpha) 	: function calculates t value from parameters given by user
  			           		 It compares calculated t value from value from table And returns 0 (to signify null hypothesis is rejected) or returns 1 (to signify null hypothesis is accepted
                               
>    :Input parameters:
>      sam : sample vector from user
>      pop_mean : mean of population given by user
>      alpha : level of significance


* F_test(sam1,sam2,alpha) 		: function calculates f value from parameters given by user
  		          			It compares calculated f value from value from ta ble 
		         			 And prints whether variances are equal or are not equal
                                
> :Input parameters:
>   sam1 : sample vector from user
>  sam2 : sample vector from user
>  alpha : level of significance


 * z_test(data,mew,sigma,alpha) 	: function calculates z value from parameters given by user
    		              			 It compares calculated z value from value from ta ble 
		              			 And returns 0 (to signify null hypothesis is rejected) or returns 1 (to signify null hypothesis is accepted
                                
> Input parameters:
>  data : data vector from user
>  mew : population mean value from user
>  sigma : standard deviation value from user
>  alpha : level of significance


### Interval_estimation			: this function will call the following functions and take the input---

* Estimation of Mean 		: Calculate the interval for population mean for confidence interval (1-alpha)*100
					estimation_of_mean(n,s_mean,variance,alpha)
>	:n          : number of  elements in the data(n>30)
>       :s_mean : sample mean
>	:variance : sample variance
>	:alpha:level of significance

* estimation_of_mean_T(n,s_mean,variance,alpha)
> :n          : number of  elements in the data(n<30)
> :s_mean : mean of the data
> :variance : variance of population
> :alpha     :level of significance

* Estimation of mean difference	: Calculate the interval for difference of population mean for confidence interval (1-alpha)*100
	estimation_of_mean_difference(n1,n2,X1,X2,variance1,variance2,alpha)
> :n1           : number of  elements in the first data(n>30)
> :X1          : first sample mean
> :variance1 : first sample variance
> :n2           : number of  elements in the first data(n>30)
> :X2           : second sample mean
> :variance2 : second sample variance
> :alpha:level of significance

*estimation_of_mean_difference_T(n1,n2,X1,X2,variance1,variance2,alpha)
> :n1           : number of  elements in the first data(n<30)
> :X1          : first  mean
> :variance1 : first population variance
> :n2           : number of  elements in the first data(n<30)
> :X2           : second mean
> :variance2 : second population variance
> :alpha:level of significance

* Estimation of proportion		: Calculate the interval for proportion for confidence interval (1-alpha)*100
> estimation_of_proportion(n,theta,alpha)	
> :n:  number of  elements in the data
> :theta: proportion 
> :alpha:level of significance

* Estimation of proportion difference	: Calculate the interval for difference of proportion for confidence interval (1-alpha)*100
> estimation_of_proportion_diff(n1,theta1,n2,theta2,alpha)	
 > :n1:  number of  elements in the first data
> :theta1: first proportion 
> :alpha:level of significance
> :n2:  number of  elements in the second data
> :theta2:second proportion 
						
* Estimation of variance		: Calculate the interval for population variance for confidence interval (1-alpha)*100
> estimation_of_variance(n,variance,alpha)
> :n:  number of  elements in the data
> :variance: variance of data 
> :alpha:level of significance

* Estimation of variance difference	: Calculate the interval for population mean for confidence interval (1-alpha)*100
> estimation_of_Ratiovariance(n1,variance1,n2,variance2,alpha)
> :n1:  number of  elements in the first data
> : variance1:  variance of first data  
> :alpha:level of significance
> :n2:  number of  elements in the second data
> :variance2: variance of second data 
### non_parametric() 			: function to perform these functions-
 1.SIGN TEST  2.WILCOXON SIGNED RANK TEST  3.MANN-WHITNEY TEST  4.KRUSKAL WALLIS TEST  


* signTest(med,set,alpha) 		: function returns 0 (to signify null hypothesis is rejected) or returns 1 (Null hypotheses is accepted)
                          
  >              : Input parameters:
  >                 mew : taken from user
  >                 set : vector taken from user
  >                 alpha : level of significance


* wilcoxon_srt(mew,set,reln,alpha) 	: function returns 0 (to signify null hypothesis is rejected) or returns 1 (Null hypotheses is accepted)

>               :Input parameters:
>                mew : taken from user
>                set : vector taken from user
>                reln : takes values 1,2,3 from user and signifies:
>                1 : mew not equal mew_not
>                2 : mew > mew_not
>                3 : mew < mew_not
>                alpha : level of significance


* Mann_whitney_test(x,y,reln,alpha) 	: function returns 0 (to signify null hypothesis is rejected) or returns 1 (Null hypotheses is accepted)

  >            :Input parameters:
  >            	 x : set x taken from user
  >            	 y : vector taken from user
  >            	 reln : takes values 1,2,3 from user and signifies:
  >              1 : mew not equal mew_not
  >              2 : mew > mew_not
  >              3 : mew < mew_not
  >              alpha : level of significance


* Kruskal_wallis_test(x,y,z,alpha)	: function returns 0 (to signify null hypothesis is rejected) or returns 1 (Null hypotheses is accepted)
>
>         :Input parameters:
>             	 x : set x vector taken from user
>              	y : set y vector taken from user
>              	z : set z vector taken from user
> 	       	alpha : level of significance

### > hist()    			: We can create histograms with the function hist(x) where x is a numeric vector of values to be plotted. 

* plot()    		: It is used to make graphs according to the type of the object passed.The plot() function in R is used to create the line graph.

* lines()   		: Draws a line in the plot section.

* barplot() 		: Bar graph can be created in R using the barplot() function. 

* pie()  		: In R the pie chart is created using the pie() function which takes positive numbers as a vector input.

* boxplot() 		: A Boxplot is an exploratory graphic that allows you to summarize the features of quantitative variables for one or several levels of a discrete variable. In R, it is done by using boxplot().

* qqplot() 		: A Q-Q plot is a scatterplot created by plotting two sets of quantiles against one another. If both sets of quantiles came from the same distribution, we should see the points forming a line that’s roughly straight.It is done by qqplot()

* qqnorm() 		: QQ plot normality is defined by qqnorm()

* qqline() 		: QQ line plot a line using least square distance from scatter points. 

* stem()   		: Stem and Leaf plot, is a special table where each numeric value is split into a stem (First digit(s) ) and a leaf (last Digit). 

* pareto.chart()  		: A Pareto Chart is a sorted bar chart that displays the frequency (or count) of occurrences that fall in different categories, from greatest frequency on the left to least frequency on the right, with an overlaid line chart that plots the cumulative percentage of occurrences.This operation is achieved by pareto.chart() in r.




