
  # rm(list=ls())
  require(data.table)
  require(MASS)
  require(ggplot2)
  require(parallel)
  require(doParallel)
  require(foreach)
  require(gridExtra)
  require(stringr)
  require(matrixStats)
  require(abind)

  parallel::detectCores()
  ## Resizing notebook plot space
  options(repr.plot.width=16, repr.plot.height=9)

  ## POP DATA from UN
  # pop <- data.table(fread("/home/j/Project/IRH/Forecasting/data/WPP_forecasted_population.csv"))
  pop <- fread('/ihme/forecasting/data/pop/20150101_wpp/data.csv')
  pop <- pop[, age_group_id:=15+5*(age_group_id -8)] 
  pop<- pop[age_group_id == 15 | age_group_id == 20 | age_group_id == 25 | age_group_id == 30 | age_group_id == 35 | age_group_id == 40 | age_group_id == 45 | age_group_id == 50 | age_group_id == 55 | age_group_id == 60 | age_group_id == 65 | age_group_id == 70 | age_group_id == 75 | age_group_id == 85,]
  head(pop)



  ## Education forecasts
  # edu_orig<-data.table(fread(paste0("/home/j/WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/archive_nafis/20161121_GBD2016prelim_95+_raked_2/gpr_draws.csv"), header = T))
  # edu_orig <- edu_orig[, V1:=NULL]
  # head(edu_orig)

  edu_ref <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/20170608_GBD2016Final_gpr_draws_scenario0.csv")
  edu_pes <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/20170608_GBD2016Final_gpr_draws_scenario-1.csv")
  edu_opt <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/20170608_GBD2016Final_gpr_draws_scenario1.csv")

  ## Convert age_group_id to sensible age groups
    ## Leave out the younglings (no maternal education)    
      edu_ref <- edu_ref[age_group_id >7]
      edu_ref <- edu_ref[, age_group_id:=15+5*(age_group_id -8)] 
      
      edu_pes <- edu_pes[age_group_id >7]
      edu_pes <- edu_pes[, age_group_id:=15+5*(age_group_id -8)] 
      
      edu_opt <- edu_opt[age_group_id >7]
      edu_opt <- edu_opt[, age_group_id:=15+5*(age_group_id -8)] 



  
  ## Number of countries we have to array over
  countries <- unique(edu_ref[year_id ==1980 & age_group_id == 15 & sex_id == 1, location_id])
  countries
  length(countries)

  ## How you lookin?
   head(edu_ref); head(edu_opt); head(edu_pes);

  ### Let's concatenatate the age and sex columns to be one identifier:
  data_cleanup_1 <- function(dt) {
      dt <- dt[, age_sex := paste0(age_group_id, "_", sex_id)]
      dt <- dt[, age_group_id:= NULL]
      dt <- dt[, sex_id:= NULL]
      dt <- setcolorder(dt, c("location_id", "year_id", "age_sex", paste0("draw_",c(0:999)) ))
      head(dt)
      
      return(dt)
  }

  edu_ref <- data_cleanup_1(edu_ref)
  edu_pes <- data_cleanup_1(edu_pes)
  edu_opt <- data_cleanup_1(edu_opt)

  # Make draws long first
  system.time(edu_ref_long <- melt(edu_ref, id.vars = c("location_id", "age_sex", "year_id"), value.name = "edu", varnames = "draw"))
  system.time(edu_pes_long <- melt(edu_pes, id.vars = c("location_id", "age_sex", "year_id"), value.name = "edu", varnames = "draw"))
  system.time(edu_opt_long <- melt(edu_opt, id.vars = c("location_id", "age_sex", "year_id"), value.name = "edu", varnames = "draw"))


  ## Ease up on the memory
  rm(edu_ref); rm(edu_pes); rm(edu_opt)

  ## Get our baseline correlation matrix
  load("/home/j/WORK/01_covariates/02_inputs/education/update_2017/data/tabulated_data/archive_nafis/insample_corr_mat.Rdata")

  ## For now, trim the corr matrix to have 14 age groups
  corr_mat <- corr_mat[1:28,1:28]

  ## Convert them all to multidim array in the dims: [loc, year, age_sex, draw_num, data]

  ## Reference
  system.time(edu_ref_array<- lapply(countries, 
                                  function(x) { reshape2::acast(edu_ref_long[location_id == x,], 
                                                        location_id ~  year_id ~ age_sex ~ variable, 
                                                        value.var = "edu") }
                                                        )
  )

  edu_ref_array <- abind(edu_ref_array, along=1)
  str(edu_ref_array)
 

  ## Pessimistic
  system.time(edu_pes_array<- mclapply(countries, 
                                  function(x) {reshape2::acast(edu_pes_long[location_id == x,], 
                                                        location_id ~  year_id ~ age_sex ~ variable, 
                                                        value.var = "edu") }, 
                                                          mc.cores = 10))
  edu_pes_array <- abind(edu_pes_array, along=1)
  str(edu_pes_array)

  ## Optimistic
  system.time(edu_opt_array<- mclapply(countries, 
                                  function(x) {reshape2::acast(edu_opt_long[location_id == x,], 
                                                        location_id ~  year_id ~ age_sex ~ variable, 
                                                        value.var = "edu") }, 
                                                          mc.cores = 10))
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
                                                                        corr_mat , df_return = T))}, mc.cores = 10 ))
                                          
  edu_ref_corr_df <- do.call(rbind, edu_ref_corr_array)
                                          
  colnames(edu_ref_corr_df) <- c("location_id", "year_id", "age_sex", "draw_num", "edu")
  head(edu_ref_corr_df)    


  system.time(edu_pes_corr_array <- mclapply(countries, 
                                      function(x) {cbind(x, draw2Dcopula(edu_pes_array[paste0(x),,,],
                                                                        corr_mat , df_return = T))}, mc.cores = 10 ))
                                          
  edu_pes_corr_df <- do.call(rbind, edu_pes_corr_array)
                                          
  colnames(edu_pes_corr_df) <- c("location_id", "year_id", "age_sex", "draw_num", "edu")
  head(edu_pes_corr_df)    



  system.time(edu_opt_corr_array <- mclapply(countries, 
                                      function(x) {cbind(x, draw2Dcopula(edu_opt_array[paste0(x),,,],
                                                                        corr_mat , df_return = T))}, mc.cores = 10 ))
                                          
  edu_opt_corr_df <- do.call(rbind, edu_opt_corr_array)
                                          
  colnames(edu_opt_corr_df) <- c("location_id", "year_id", "age_sex", "draw_num", "edu")
  head(edu_opt_corr_df)

  # Reshape wide
  Xcorr_df_with_pop <- dcast(Xcorr_df, location_id + year_id + age_sex ~ draw_num, value.var = c("edu"))
  head(Xcorr_df_with_pop)

  # Split back into age and sex
  Xcorr_df_with_pop2 <- Xcorr_df_with_pop
  system.time({ Xcorr_df_with_pop2[, c("age_group_id","sex_id") := data.table(str_split_fixed(age_sex,"_",2))] })
  Xcorr_df_with_pop2[, age_sex:=NULL]
  head(Xcorr_df_with_pop2)
  str(Xcorr_df_with_pop2)

  # Merge in population
  Xcorr_df_with_pop2 <- merge(x=Xcorr_df_with_pop2, y=pop, by = c("location_id", "year_id", "age_group_id", "sex_id"), all.x=T)
  head(Xcorr_df_with_pop2)

  # Collapse correlated data and compute the mean and PCs
  Xcorr_df_CY<- Xcorr_df_with_pop2[, lapply(.SD, function(x) sum(x*pop)/sum(pop)), 
      by=c("location_id", "year_id"), .SDcols=cbind(paste0("draw_",c(0:999))) ] 
  Xcorr_df_CY <- Xcorr_df_CY[, `:=` (mean = rowMeans(.SD, na.rm=T),
                                    lower = rowQuantiles(.SD, na.rm=T, probs=c(0.025)),
                                    upper = rowQuantiles(.SD, na.rm=T, probs=c(0.975))), 
                             .SDcols = c(paste0("draw_",c(0:999))) ]    
  head(Xcorr_df_CY)

  # Collapse un-correlated data
  uncorr_df_CY<- edu_orig_2[,lapply(.SD, function(x) sum(x*pop)/sum(pop)), 
      by=c("location_id", "year_id"), .SDcols=cbind(paste0("draw_",c(0:999))) ] 
  uncorr_df_CY <- uncorr_df_CY[, `:=` (mean = rowMeans(.SD, na.rm=T),
                                lower = rowQuantiles(.SD, na.rm=T, probs=c(0.025)),
                                upper = rowQuantiles(.SD, na.rm=T, probs=c(0.975))), 
                         .SDcols = c(paste0("draw_a",c(0:999))) ] 
  head(uncorr_df_CY)

  # Convert to arrays for super faster subsetting
  system.time(myarray3<- lapply(countries, 
                                  function(x) reshape2::acast(Xcorr_df[location_id == x,], 
                                                        location_id ~  year_id ~ age_sex ~ draw_num, 
                                                        value.var = "edu")))
  myarray3 <- abind(myarray3, along=1)
  str(myarray3)

  x1<-data.frame(myarray2["6","2040","15_1",]); colnames(x1) <- "data"
  x2<-data.frame(myarray3["6","2040","15_1",]); colnames(x2) <- "data"

  ggplot(data = x1, aes(x=data)) + 
      geom_histogram(aes(y=  ..density..), alpha=0.3, fill="steelblue") +
      geom_density(color ="blue4") +
  geom_histogram(data = x1, aes(x=data, y=  ..density..), alpha=0.15, fill="red1") +
      geom_density(color ="red4")   + ggtitle("China: age group 15-20; male")

  x1<-data.frame(myarray2["6","2040","75_2",]); colnames(x1) <- "data"
  x2<-data.frame(myarray3["6","2040","75_2",]); colnames(x2) <- "data"

  ggplot(data = x1, aes(x=data)) + 
      geom_histogram(aes(y=  ..density..), alpha=0.3, fill="steelblue") +
      geom_density(color ="blue4") +
  geom_histogram(data = x1, aes(x=data, y=  ..density..), alpha=0.15, fill="red1") +
      geom_density(color ="red4") + ggtitle("China: age group 75-80; female")

  ## Turn the collapsed ones into arrays as well
  array_Xcorr_df_CY <- melt(Xcorr_df_CY, id.vars = c("location_id", "year_id"), value.name = "edu", variable.name = "variable")
  array_Xcorr_df_CY <- reshape2::acast(array_Xcorr_df_CY, location_id ~ year_id ~ variable, value.var = "edu")
  str(array_Xcorr_df_CY) ### last 3 values are mean, upper, lower

  array_uncorr_df_CY <- melt(uncorr_df_CY, id.vars = c("location_id", "year_id"), value.name = "edu", variable.name = "variable")
  array_uncorr_df_CY <- reshape2::acast(array_uncorr_df_CY, location_id ~ year_id ~ variable, value.var = "edu")
  str(array_uncorr_df_CY) ### last 3 values are mean, upper, lower

  plot_country_collapsed<-function(loc_id) {
  ggplot(uncorr_df_CY[location_id == loc_id,], aes(x = year_id, y = mean, ymin = lower, ymax = upper)) +
      geom_ribbon(alpha = 0.3, fill="steelblue") +
      geom_line(color = "blue4") +
      geom_ribbon(data=Xcorr_df_CY[location_id == loc_id,], aes(x = year_id, ymin = lower, ymax = upper),
                  alpha = 0.15, fill="red1") +
      geom_line(aes(x=year_id, y=mean),color = "red4") + xlab("Year") + ylab("Education") +
      ggtitle(paste0("location_id: ", loc_id, "; collapsed to C-Y : blue before copula, red after copula"))
  }

  plot_country_collapsed(6)

  ## Save out data

  # Collapsed C-Y
  fwrite(Xcorr_df_CY, "/home/j/WORK/01_covariates/02_inputs/education/update_2017/data/output_data/20161121_GBD2016prelim_95+_raked_2/country_year_A_S_T_collapsed.csv")

  filename <- paste0("/homes/sadatnfs/test_edu.pdf")
  pdf(filename, width=10, height = 6)
      for(c in countries){
          print(plot_country_collapsed(c))
      }
  dev.off()


