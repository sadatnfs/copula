

#### Purpose: Correlate education draws across time
rm(list=ls())
require(MASS)
require(foreign)
require(ggplot2)
require(grid)
require(reshape2)
require(dplyr)
require(data.table)
require(MASS)

if(Sys.info()["sysname"]=="Windows") {
  require(readstata13)
  require(feather)
  require(gridExtra)
  require(VineCopula)
  require(copula)
}
os <- ifelse(Sys.info()["sysname"]=="Linux","/home/j/", ifelse(Sys.info()["sysname"]=="Darwin", "~/j","J:/"))
home <- ifelse(Sys.info()["sysname"]=="Linux","/homes/sadatnfs/", ifelse(Sys.info()["sysname"]=="Darwin", "~/h","H:/"))


data <- data.frame(v1 = rnorm(100,0,1), v2 = runif(100), v3 = rpois(100, 4))

rank_time_series <- function(X, corr, ts=F, print=FALSE){
  draws <- nrow(X)
  years <- ncol(X)
  # X <- matrix(runif(draws*years), nrow=years, ncol=draws)
  
  if(print){
    print(sapply(2:years, function(x) cor(X[x,], X[x-1,])))
  }
  
  if(ts == F) {
    corr_mat <- matrix(data=corr, nrow= years, ncol= years )
    diag(corr_mat) <- 1
  }
  
  if(ts == T) {
    corr_mat <- corr**abs(outer(0:(years-1), 0:(years-1), "-"))
  }
  
  
  print(corr_mat)
  mvdat <- mvrnorm(n= draws, mu=0 * 1:ncol(X), Sigma=corr_mat, empirical=T)
  ranks <- apply(mvdat, 2, rank, ties.method="first")
  sorted_X <- apply(X, 2, sort)
  sapply(1:ncol(X), function(x) sorted_X[,x][ranks[,x]])
}


sorted_data_noTS <- rank_time_series(data, 0.95, ts=F)
cor(sorted_data_noTS, method="spearman")

sorted_data_TS <- rank_time_series(data, 0.95, ts=T)
cor(sorted_data_TS, method="spearman")


 





set.seed(123)
draws <- read.csv(paste0(os,"/WORK/01_covariates/02_inputs/education/update_2017/data/output_data/20161121_GBD2016prelim_95+_raked_2/gpr_draws_country_year_collapsed.csv"))
colnames(draws) <- c("X", "location_id", "year_id", paste0("draw_", c(1:1000)))
setwd(paste0(os, "/temp/central_comp/libraries/current/r"))
source("get_location_metadata.R")
loc <- get_location_metadata("22")
loc <- loc[, .(location_id, ihme_loc_id)]


# Merge ihme_loc_id to draws
draws_loc <- merge(draws, loc, by="location_id", all.x=T)
draws_loc$X <- NULL



# Number of draws
N_draws <- 1000

# Number of offseting variables before the draws (location_id, year_id)
offset_vars <- 3

# Subset a country of choice
isoiso <- "CHN"

# Create a DF of just the country above
country_sub <- subset(draws_loc, ihme_loc_id==paste0(isoiso) )[,1:(N_draws + offset_vars)]

# cbind(country_sub[, c("location_id", "year_id")], 
#                      rowMeans(country_sub[, grep(glob2rx("draw_*"), names(country_sub))], na.rm = TRUE, dims = 1), 
#                      t(apply(country_sub[, grep(glob2rx("draw_*"), names(country_sub))],1,quantile,probs=c(0.025, 0.975),na.rm=TRUE)))


# Transpose draws to long and year to wide
melt1 <- melt(country_sub, id.vars = c("location_id", "ihme_loc_id", "year_id"))
dcast1 <- dcast(melt1, ihme_loc_id + location_id  + variable ~   year_id, value.var="value")




# Baseline RHO: Compute correlation for each pairs of year from 1950 to 2015? (country specific)
baseline_rho <- function(data, yr1, yr2) {
  
  rho <- vector()
  j = 0
  for(i in yr1:yr2) {
    j = j + 1
    rho[j] = cor(x = data[,paste0(i)], y = data[,paste0(i+1)], method = "spearman")
  }
  
  return(rho)
}
mean_rho <- baseline_rho(dcast1, 1950,2015)




plot(c(1950:2015),mean_rho)
y1_vect <- c(2016:2040)


post_copula <- dcast1 
post_copula <- arrange(post_copula, `2015`)




# Testing Neal's function
neal_cop <- post_copula[,c("location_id", "ihme_loc_id")]
neal_cop$variable <- paste0("draw_",c(1:1000))

new_post<-data.frame(rank_time_series(post_copula[, 4:94], mean(mean_rho)))
neal_cop <- cbind(neal_cop, new_post)
colnames(neal_cop) <- c("location_id", "ihme_loc_id", "variable", paste0(c(1950:2040)))






# Reshape back properly
t1 <- melt(neal_cop, id.vars = c("location_id", "ihme_loc_id", "variable"), value.name = "edu")
colnames(t1) <- c("location_id", "ihme_loc_id", "draw", "year_id", "edu")
neal_cop_proper <- dcast(t1, value.var = "edu", location_id + ihme_loc_id + year_id ~ draw)

t1 <- melt(post_copula, id.vars = c("location_id", "ihme_loc_id", "variable"), value.name = "edu")
colnames(t1) <- c("location_id", "ihme_loc_id", "draw", "year_id", "edu")
pre_cop_proper <- dcast(t1, value.var = "edu", location_id + ihme_loc_id + year_id ~ draw)





######### PLOTS

## Vector of random draws
samp_colnames <- c("ihme_loc_id", "year_id", paste0("draw_",sample(N_draws, size = 15)))

## Subset out each of the 
melt_pre <- pre_cop_proper[,samp_colnames]

## Post copula data
melt_post <- neal_cop_proper[,samp_colnames]

## Melt the data post-sampling
melt_pre <- melt(melt_pre, id.vars = c("ihme_loc_id", "year_id"), value.name = "edu")
melt_pre$year <- as.numeric(melt_pre$year) + 2015
melt_post <- melt(melt_post, id.vars = c("ihme_loc_id", "year_id"), value.name = "edu")
## Plot objects
pre_copula<-ggplot() +
  geom_line(data = melt_pre, aes(x= year_id, y = edu, group= variable,color = variable)) + 
  theme(legend.position = "none") + ggtitle("Pre-copula")

post_copula<-ggplot() +
  geom_line(data = melt_post, aes(x= year_id, y = edu, group= variable,color = variable)) + 
  theme(legend.position = "none") + ggtitle("Post-copula")

grid.arrange(pre_copula, post_copula, ncol=2, top = paste0(isoiso))
