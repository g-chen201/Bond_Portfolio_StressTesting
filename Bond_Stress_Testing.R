###################### Credit Metrics ##############################
library(expm)
library(Matrix)
library(gdata)

# The Transition Values function calculates migration thersholds 
# given percentage values from a credit rating transition matrix.

TransitionValues <- function(TransitionMatrix)
{
	matrix_size=dim(TransitionMatrix)
	
	
	
	Thresholds = matrix(0, matrix_size[1], matrix_size[2])
	
	for (i in 1:matrix_size[1])
	{
		sum = 0
		for (j in matrix_size[2]:2)
		{
			sum = sum + TransitionMatrix[i,j]
			Thresholds[i,j] = min(10, qnorm(sum))
		}	
	}
	
	# Due to computer numerical precesion, there are round off issues.
	# We set the first column of threshold matrix equal to 10 
	
	Thresholds[,1] = 10
	return(Thresholds)
}


# This Time Dependent Matrix calcualtes a time depent matrix from an 
# initial transition matrix. The function first calculates a generateor 
# matrix from an initial input transtion matrix and ensures that off 
# diagonal elements are greater than or equal to zero. Methodology 
# is based on : 
# Finding generators for Markov Chains via emperical transition matrices, 
# with applications to credit ratings, by Israel, Rosenthal, and Wei Mathematical Finance. 


TimeDependentMatrix <- function(TransitionMatrix, Time)
{
	n = dim(TransitionMatrix)[1]
	ApproximateGenerator = logm(TransitionMatrix)
	
	Generator = matrix(0, n, n)
	
	for (i in 1:n)
	{
		for (j in 1:n)
		{
			if (i == j)
			{
				Generator[i,j]=ApproximateGenerator[i,j]+sum(pmin(ApproximateGenerator[i,],0))-min(ApproximateGenerator[i,j],0)
			}
			else 
			{
				Generator[i,j]=max(ApproximateGenerator[i,j], 0)
			}
		}
	}
	
	TimeDependentMatrix=expm(Time*Generator)
	
	# Due to computer numerical precesion, there are round off issues.
	# We can normalize each row sum to 1
	
	# for (i in 1:dim(TimeDependentMatrix)[1])
	# {
	#	TimeDependentMatrix[i,]=TimeDependentMatrix[i,]/sum(TimeDependentMatrix[i,])
	# }
	return(TimeDependentMatrix)
}


# This Generate Scenarios function generates an array of correlated 
# standard noraml random variables which are then compared versus 
# thresholds to determine if an obligor has migrated to a different 
# credit grade or not. 



GenerateScenarios <- function(nObligors, nSimulations, Corr_Matrix, seed)
{
	set.seed(seed)
	#Calcuate the cholesky decompsed matrix from the correlation matrix
	Corr_Matrix_chol = chol(Corr_Matrix)
	X=matrix(rnorm(ceiling(nSimulations/2)*nObligors, mean=0, sd=1), ceiling(nSimulations/2),nObligors)
	
	# Use antithetic sampling to introduce negative correclation between 
	# random draws, to reduce sampling variance
	Corr_Sample = rbind(X,-X)%*%Corr_Matrix_chol
	
	return(Corr_Sample)
		
}


# This function find the index i, where year_arrary[i-1] < year <= year_arrary[i]
# this helps the linear interplation of interest rate. 

lower_upper_year <- function(year, year_arrary)
{
	n = length(year_arrary)
	
	for (i in 1:n)
	{
		if (year <= year_arrary[i])
		{
			break
		}
	}
	return(i)
}



############ Read data
 
#set working directory
setwd("C:\\Users\\Jason\\Desktop")

# Change xlsx file name for different scenarios
tranMat =  data.matrix(read.xls("Data_scenario_3.xlsx", sheet="TransitionMatrix", header=FALSE))  
spreadShocks =  data.matrix(read.xls("Data_scenario_3.xlsx", sheet="spreadShocks", header=FALSE))
rateShockCurve = data.matrix(read.xls("Data_scenario_3.xlsx", sheet="rateShockCurve", header=FALSE))

initPort=data.matrix(read.xls("Data_scenario_3.xlsx", sheet="initPort", header=FALSE))
keyrates=data.matrix(read.xls("Data_scenario_3.xlsx", sheet="keyrates", header=FALSE))
spread_dur=data.matrix(read.xls("Data_scenario_3.xlsx", sheet="spread_dur", header=FALSE))
creditchangerate=data.matrix(read.xls("Data_scenario_3.xlsx", sheet="creditchangerate", header=FALSE))

trails = 10000
time = 0.5  # forecast time horizon
rho = 0.3  # correlation between different bonds in the portfolio
recovRate = 0.4

seed = 123

nPort = dim(initPort)[1]  # size of portfolio

rateShockCurve_keyr = cbind(t(rateShockCurve[4:7,]), t(rateShockCurve[9:11,]))
rateShockCurve_keyr = t(rateShockCurve_keyr)

# create output matrix
CMResults = matrix(0, trails, 1)

# Calculate time dependent transition matrix
TDMatrix = TimeDependentMatrix(tranMat, time)

# Calculate the trasition threshold value for the TDMatrix
ThresholdMatrix = TransitionValues(TDMatrix)

# Find the correlation matrix give nObligors and rho
Corr_Matrix = diag(nPort) + rho - rho*diag(nPort)

# Sample the correlated random variables
OutputMatrix = GenerateScenarios(nPort, trails, Corr_Matrix, seed)

# create matrix to save final portfolio, assign intiPort to it, update first column (credit grade next)
finalPort = initPort 

for (i in 1:trails) 
{
	for (j in 1:nPort) 
	{
		# compare with threshold, and find out if credit migration occured or not
		if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],2])
		{
			OutputRating = 1 
		}
		else if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],3])
		{
			OutputRating = 2
		}
		else if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],4])
		{
			OutputRating = 3
		}
		else if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],5])
		{
			OutputRating = 4
		}
		else if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],6])
		{
			OutputRating = 5
		}
		else if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],7])
		{
			OutputRating = 6
		}
		else if (OutputMatrix[i,j] > ThresholdMatrix[initPort[j,1],8])
		{
			OutputRating = 7
		}
		else 
		{
			OutputRating = 8
		}
		
		if (initPort[j,2] == 5) # treasury
		{
			OutputRating = min(4, OutputRating)
		}
			
		finalPort[j,1] = OutputRating  # update the final credit grade
		
		# calculate shock
		if (finalPort[j,1]<8) 
		{
			if (keyrates[j,1]> -50)  
			{
				rateShock = sum(keyrates[j,]*(rateShockCurve_keyr[,2]))
			}
			else 
			{
				if (finalPort[j,4] <= rateShockCurve[1,1])
				{
					rate=rateShockCurve[1,2]  
				}
				else if (finalPort[j,4] >= rateShockCurve[7,1])
				{
					rate=rateShockCurve[7,2]
				}
				else 
				{
					# linear interpolate the rate shock
					ui = lower_upper_year(finalPort[j,4], rateShockCurve[,1])
					li = ui-1
					t1 = rateShockCurve[li,1]
					t2 = rateShockCurve[ui,1]
					r1 = rateShockCurve[li,2]
					r2 = rateShockCurve[ui,2]
					
					rate = (r2-r1)/(t2-t1)*(finalPort[j,4]-t1)+r1
				}
				rateShock=finalPort[j, 4]*rate # the rate spread change to econ 
			}
			
			sp_dur = spread_dur[j]
			spreadShock = sp_dur*spreadShocks[finalPort[j,2], finalPort[j,1]] # the rate spread shock due to sector specific shock
			
			creditrateShock = sp_dur*creditchangerate[initPort[j,1], finalPort[j,1]] #the rate spread change due to creidt migration
			
			#sum shocks
			CMResults[i] = CMResults[i]+(rateShock+spreadShock+creditrateShock)*finalPort[j,3]
		}
		else 
		{
			# for defaults
			CMResults[i] = CMResults[i]+finalPort[j,3]*(1-recovRate) 
		}
		
			
	}
}


CMResultsP=CMResults/100
CM_mean=mean(CMResultsP)
CM_min=min(CMResultsP)
CM_max=max(CMResultsP)
CM_95 = quantile(CMResultsP, 0.95)
CM_99 = quantile(CMResultsP, 0.99)
CM_CVar95 = mean(CMResultsP[CMResultsP > CM_95])
CM_CVar99 = mean(CMResultsP[CMResultsP > CM_99])

CM_mean
CM_min
CM_max
CM_95
CM_99
CM_CVar95
CM_CVar99


hist(-CMResults, breaks=50, xlab="Portfolio Change (%)", ylab="Occurences", main="Portfolio Loss Distribution")


