
rm(list=ls())
pacman::p_load(data.table, MASS, ggplot2, stringr, matrixStats, doParallel, abind, ncdf4)

parallel::detectCores()
## Resizing notebook plot space
options(repr.plot.width=16, repr.plot.height=9)

## POP DATA from UN
# pop <- data.table(fread("/home/j/Project/IRH/Forecasting/data/WPP_forecasted_population.csv"))
pop <- fread('/ihme/forecasting/data/pop/20150101_wpp/data.csv')
pop <- pop[, age_group_id:=15+5*(age_group_id -8)] 
pop<- pop[age_group_id == 15 | age_group_id == 20 | age_group_id == 25 | age_group_id == 30 | age_group_id == 35 | age_group_id == 40 | 
          age_group_id == 45 | age_group_id == 50 | age_group_id == 55 | age_group_id == 60 | age_group_id == 65 | age_group_id == 70 | 
          age_group_id == 75 | age_group_id == 80,]
head(pop)

### Let's concatenatate the age and sex columns to be one identifier:
data_cleanup_1 <- function(dt) {
    dt <- dt[, age_sex := paste0(age_group_id, "_", sex_id)]
    dt <- dt[, age_group_id:= NULL]
    dt <- dt[, sex_id:= NULL]
#     dt <- setcolorder(dt, c("location_id", "year_id", "age_sex", paste0("draw_",c(0:999)) ))
    head(dt)
    
    return(dt)
}

## Merge pop age-sex var
pop_joined <- data_cleanup_1(pop)

## Bring in edu from Pat!

system.time(edu_ref <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/20170725_cohort_maternal_scenarios0.csv"))
system.time(edu_pes <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/20170725_cohort_maternal_scenarios-1.csv"))
system.time(edu_opt <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/20170725_cohort_maternal_scenarios1.csv"))

## Convert age_group_id to age_start
  ## Leave out the younglings (no maternal education)    
    edu_ref <- edu_ref[age_group_id >7]
    edu_ref <- edu_ref[, age_group_id:=15+5*(age_group_id -8)] 
    
    edu_pes <- edu_pes[age_group_id >7]
    edu_pes <- edu_pes[, age_group_id:=15+5*(age_group_id -8)] 
    
    edu_opt <- edu_opt[age_group_id >7]
    edu_opt <- edu_opt[, age_group_id:=15+5*(age_group_id -8)] 

## Use the function
Sys.time()
edu_ref <- data_cleanup_1(edu_ref)
edu_pes <- data_cleanup_1(edu_pes)
edu_opt <- data_cleanup_1(edu_opt)
Sys.time()

# Make draws long 
system.time(edu_ref_long <- melt(edu_ref, id.vars = c("location_id", "age_sex", "year_id"), value.name = "edu", varnames = "draw"))
system.time(edu_pes_long <- melt(edu_pes, id.vars = c("location_id", "age_sex", "year_id"), value.name = "edu", varnames = "draw"))
system.time(edu_opt_long <- melt(edu_opt, id.vars = c("location_id", "age_sex", "year_id"), value.name = "edu", varnames = "draw"))

## Number of countries we have to array over
countries <- unique(edu_ref[year_id ==1980 & age_sex=="15_1", location_id])
head(countries); length(countries);

## Ease up on the memory
rm(edu_ref); rm(edu_pes); rm(edu_opt)

## Get our baseline correlation matrix
load("/home/j/WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/archive_nafis/insample_corr_mat.Rdata")

## For now, trim the corr matrix to have 14 age groups
corr_mat <- corr_mat[1:28,1:28]

head(edu_pes_long)

## Convert them all to multidim array in the dims: [loc, year, age_sex, draw_num, data]

## Reference
system.time(edu_ref_array<- mclapply(countries, 
                                function(x) {reshape2::acast(edu_ref_long[location_id == x,], 
                                                      location_id ~  year_id ~ age_sex ~ variable, 
                                                      value.var = "edu") }, mc.cores = 15, mc.preschedule = F))
edu_ref_array <- abind(edu_ref_array, along=1)
str(edu_ref_array)

## Pessimistic
system.time(edu_pes_array<- mclapply(countries, 
                                function(x) {reshape2::acast(edu_pes_long[location_id == x,], 
                                                      location_id ~  year_id ~ age_sex ~ variable, 
                                                      value.var = "edu") }, 
                                                        mc.cores = 15, mc.preschedule = F))
edu_pes_array <- abind(edu_pes_array, along=1)
str(edu_pes_array)

## Optimistic
system.time(edu_opt_array<- mclapply(countries, 
                                function(x) {reshape2::acast(edu_opt_long[location_id == x,], 
                                                      location_id ~  year_id ~ age_sex ~ variable, 
                                                      value.var = "edu") }, 
                                                        mc.cores = 15, mc.preschedule = F))
edu_opt_array <- abind(edu_opt_array, along=1)
str(edu_opt_array)

draw2Dcopula <- function(X, cor_mat, df_return = F){
  L <- dim(X)[2]
  D <- dim(X)[3]
  Xsum <- apply(X, c(2, 3), sum)
  mvdat <- mvrnorm(n=D, mu=0 * 1:L, Sigma=cor_mat, empirical=TRUE)
  ranks <- apply(mvdat, 2, rank, ties.method="first")
  sortedXsim <- apply(Xsum, 1, function(x) sort(x, index.return=TRUE)$ix)
  sortedX <- X
  for(i in 1:L){
    sortedX[,i,] <- X[,i,sortedXsim[,i]]
  }
  Xcorr <- sortedX
  for(i in 1:L){
    Xcorr[,i,] <- sortedX[,i,ranks[,i]]
  }
  if (df_return==T) {
    return(data.table(melt(Xcorr)))
    }
      else {
          Xcorr
      }
}

system.time(edu_ref_corr_array <- mclapply(countries, 
                                    function(x) {cbind(x, draw2Dcopula(edu_ref_array[paste0(x),,,],
                                                                      corr_mat , df_return = T))}, mc.cores = 15, mc.preschedule = F ))
length(edu_ref_corr_array)                                        
edu_ref_corr_df <- do.call(rbind, edu_ref_corr_array)
                                        
colnames(edu_ref_corr_df) <- c("location_id", "year_id", "age_sex", "draw_num", "edu")
head(edu_ref_corr_df)    


system.time(edu_pes_corr_array <- mclapply(countries, 
                                    function(x) {cbind(x, draw2Dcopula(edu_pes_array[paste0(x),,,],
                                                                      corr_mat , df_return = T))}, mc.cores = 15, mc.preschedule = F))
                                        
edu_pes_corr_df <- do.call(rbind, edu_pes_corr_array)
                                        
colnames(edu_pes_corr_df) <- c("location_id", "year_id", "age_sex", "draw_num", "edu")
head(edu_pes_corr_df)    



system.time(edu_opt_corr_array <- mclapply(countries, 
                                    function(x) {cbind(x, draw2Dcopula(edu_opt_array[paste0(x),,,],
                                                                      corr_mat , df_return = T))}, mc.cores = 15, mc.preschedule = F))
                                        
edu_opt_corr_df <- do.call(rbind, edu_opt_corr_array)
                                        
colnames(edu_opt_corr_df) <- c("location_id", "year_id", "age_sex", "draw_num", "edu")
head(edu_opt_corr_df)

dim(edu_ref_long); dim(edu_opt_corr_df); dim(edu_pes_corr_df); dim(edu_ref_corr_df)

country_year_collapser <- function(edu_data, pop_data = pop, split_AS = T, dcast_needed=T) {
    
    if(dcast_needed) {
        ## Reshape wide
        Xcorr_df_with_pop <- dcast(edu_data, location_id + year_id + age_sex ~ draw_num, value.var = c("edu"))
    } else {
        Xcorr_df_with_pop <- edu_data
    }

    # Split back into age and sex
    Xcorr_df_with_pop2 <- Xcorr_df_with_pop
    
    if(split_AS) {
        ## Split the string "_"
        system.time({ Xcorr_df_with_pop2[, c("age_group_id","sex_id") := data.table(str_split_fixed(age_sex,"_",2))] })
        Xcorr_df_with_pop2[, age_sex:=NULL]    

        ## Convert the new columns to numerics
        Xcorr_df_with_pop2[, age_group_id := as.numeric(age_group_id)]
        Xcorr_df_with_pop2[, sex_id := as.integer(sex_id)]
    }
   
    #### MAKE THE POP DT INTO A SINGLE AGE_SEX AND DO THE MERGE! FASTER
    
    ## Merge in population
    Xcorr_df_with_pop2 <- merge(x=Xcorr_df_with_pop2, 
                                           y=pop_data, by = c("location_id", "year_id", "age_sex") )

#     Xcorr_df_with_pop2 <- Xcorr_df_with_pop2[!is.na(pop)]
    
    head(Xcorr_df_with_pop2)
    
    # Collapse correlated data and compute the mean and PCs
#     Xcorr_df_CY<- Xcorr_df_with_pop2[, lapply(.SD, function(x) sum(x*pop)/sum(pop)), 
#                             by=c("location_id", "year_id"), .SDcols=cbind(paste0("draw_",c(0:999))) ] 
       Xcorr_df_CY<- Xcorr_df_with_pop2[, list(data = sum(edu*pop, na.rm=T)/sum(pop, na.rm=T)), 
                             by=c("location_id", "year_id", "draw_num")] 

    
    ## Melt super long to get stats
#     Xcorr_df_CY_super_long <- melt(Xcorr_df_CY, id.vars = c("location_id", "year_id"), 
#                                    value.name = "data", variable.name = "draws")
    
        
    ## Get stats
    Xcorr_df_CY_stats <- Xcorr_df_CY[, list(mean = mean(data, na.rm=T), 
                                                            upper = quantile(data, 0.975, na.rm=T), 
                                                            lower = quantile(data, 0.025, na.rm=T)), 
                                                     by = c("location_id", "year_id")]

    ## Return stuff
    returner <- list()
    returner[["draws"]] <- Xcorr_df_CY
    returner[["stats"]] <- Xcorr_df_CY_stats
        
    return(returner)
}


## Collapse the three scenarios
system.time(edu_ref_corr_CY <- country_year_collapser(edu_data = edu_ref_corr_df, pop_joined, split_AS = F,dcast_needed = F))
system.time(edu_opt_corr_CY <- country_year_collapser(edu_data = edu_opt_corr_df, pop_joined, split_AS = F,dcast_needed = F))
system.time(edu_pes_corr_CY <- country_year_collapser(edu_data = edu_pes_corr_df, pop_joined, split_AS = F,dcast_needed = F))

## Collapse the un-correlated data
try(edu_ref_long[, draw_num:= variable])
try(edu_ref_long[, variable:= NULL])
system.time(edu_ref_uncorr_CY <-country_year_collapser(edu_data = edu_ref_long, pop_joined, split_AS=F, dcast_needed=F))

try(edu_pes_long[, draw_num:= variable])
try(edu_pes_long[, variable:= NULL])
system.time(edu_pes_uncorr_CY <-country_year_collapser(edu_data = edu_pes_long, pop_joined, split_AS=F, dcast_needed=F))

edu_opt_long[, draw_num:= variable]
edu_opt_long[, variable:= NULL]
system.time(edu_opt_uncorr_CY <-country_year_collapser(edu_data = edu_opt_long, pop_joined, split_AS=F, dcast_needed=F))

## Plot by country
plot_country_collapsed <- function(loc_id, data_ref, data_pes, data_opt) {
    ggplot() +  xlab("Year") + ylab("Education per capita") + ggtitle(paste0(loc_id)) +
    
    ## Reference
    geom_line(data = data_ref[year_id<=2015 & location_id == loc_id],
                aes(x = year_id, y = mean), color = "blue3", alpha = .3) +
    geom_ribbon(data = data_ref[location_id == loc_id],
                aes(x = year_id, ymin= lower, ymax = upper), fill = "steelblue", alpha = .3) +
    geom_line(data = data_ref[year_id>=2015 & location_id == loc_id],
                aes(x = year_id, y = mean), color = "blue3", linetype = "dashed", alpha = .3) +
    
    ## Optimistic
    geom_ribbon(data = data_opt[year_id >=2015 & location_id == loc_id],
                aes(x = year_id, ymin= lower, ymax = upper), fill = "green2", alpha = .3) +
    geom_line(data = data_opt[year_id>=2015& location_id == loc_id],
                aes(x = year_id, y = mean), color = "green4", linetype = "dashed", alpha = .3) +
    
    ## Pessimistic
    geom_ribbon(data = data_pes[year_id>=2015 & location_id == loc_id],
                aes(x = year_id, ymin= lower, ymax = upper), fill = "red2", alpha = .3) +
    geom_line(data = data_pes[year_id>=2015 & location_id == loc_id],
                aes(x = year_id, y = mean), color = "red4", linetype = "dashed", alpha = .3) 
    
}

ggplot(edu_opt_corr_CY[["stats"]][location_id==522]) +
 geom_ribbon(aes(x = year_id, ymin = lower, ymax = upper)) +
geom_line(aes(x = year_id, y=mean))

## Correlated
plot_country_collapsed(6, data_ref = edu_ref_corr_CY[["stats"]],
                      data_pes = edu_pes_corr_CY[["stats"]], data_opt=edu_opt_corr_CY[["stats"]])

## Uncorrelated
plot_country_collapsed(6, data_ref = edu_ref_uncorr_CY[["stats"]],
                      data_pes = edu_pes_uncorr_CY[["stats"]], data_opt=edu_opt_uncorr_CY[["stats"]])

## Prep for saving out the data (merge in the stats AND add a column of scenarios)
reference_save <- merge(edu_ref_corr_CY[["draws"]], edu_ref_corr_CY[["stats"]], by = c("location_id", "year_id"))
reference_save[, scenario:= 0]

pessimistic_save <- merge(edu_pes_corr_CY[["draws"]], edu_pes_corr_CY[["stats"]], by = c("location_id", "year_id"))
pessimistic_save[, scenario:= -1]

optimistic_save <- merge(edu_opt_corr_CY[["draws"]], edu_opt_corr_CY[["stats"]], by = c("location_id", "year_id"))
optimistic_save[, scenario:= 1]

## Save out data


## Reference
fwrite(reference_save, paste0(
       "/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/correlated_CY_edu/",
          "20170725_cohort_maternal_scenarios0.csv"))

## Pessimistic
fwrite(pessimistic_save, paste0(
       "/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/correlated_CY_edu/",
          "20170608_GBD2016Final_gpr_draws_cohort_scenarios-1_CY.csv"))

## Optimistic
fwrite(optimistic_save, paste0(
       "/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/correlated_CY_edu/",
          "20170608_GBD2016Final_gpr_draws_cohort_scenarios1_CY.csv"))
