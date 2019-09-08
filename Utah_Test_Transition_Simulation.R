######################################################################
###
### Code to simulate analyses the Leslie/Scott did
###
######################################################################

require(data.table)

Utah_Test_Transition_Simulation_Function <- function(number.years) {
	tmp.list <- list()
	for (i in 1:50000) {
		tmp.random <- rnorm(number.years)
		tmp.list[[i]] <- (tail(tmp.random, 1) - mean(head(tmp.random, -1)))/sd(head(tmp.random, -1))
	}
	return(unlist(tmp.list))
}

Utah_Percent_in_Categories <- function(tmp.values) {
	tmp.dt <- data.table(VALUES=tmp.values)
	tmp.dt[tmp.values < -0.8,CATEGORY:="1. NEGATIVE LARGE"]
	tmp.dt[tmp.values >= -0.8 & tmp.values < -0.5,CATEGORY:="2. NEGATIVE MEDIUM"]
	tmp.dt[tmp.values >= -0.5 & tmp.values < -0.2,CATEGORY:="3. NEGATIVE SMALL"]
	tmp.dt[tmp.values >= -0.2 & tmp.values <= 0.2,CATEGORY:="4. NEGLIGIBLE"]
	tmp.dt[tmp.values > 0.2 & tmp.values <= 0.5,CATEGORY:="5. POSITIVE SMALL"]
	tmp.dt[tmp.values > 0.5 & tmp.values <= 0.8,CATEGORY:="6. POSITIVE MEDIUM"]
	tmp.dt[tmp.values > 0.8,CATEGORY:="7. POSITIVE LARGE"]
	return(tmp.dt[,list(PERCENTAGE_IN_CATEGORY=100*(.N/dim(tmp.dt)[1])), keyby="CATEGORY"])
}
