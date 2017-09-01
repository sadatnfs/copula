
    ##########################################
    ## Correlating EDU, TFR and LDI; by scenarios!
    ##########################################

    rm(list=ls())
    require(readstata13)    
    require(data.table)
    require(MASS)
    require(ggplot2)    
    require(gridExtra)
    require(stringr)
    require(matrixStats)
    require(doParallel)
    require(abind)


    parallel::detectCores()


    ### Bring in location metadata
    locs <- data.table(read.dta13("/home/j/Project/IRH/DAH/RESEARCH/INTEGRATED DATABASES/DATA/FGH_2016/region_data/location_metadata.dta"))
    countries <- unique(locs[, location_id])

    ### Prep data

      ### LDI
        ldi <- fread("/home/j/Project/IRH/Forecasting/gdp/national_LDIpc_scenarios_prepped_20170721.csv")
        
        ## Scenarios
        ldi_ref <- ldi[scenario==0]
        ldi_pes <- ldi[scenario==-1]
        ldi_opt <- ldi[scenario==1]


      ### TFR         

        ## Scenarios
        tfr_ref <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/binded_tfr_draws_scenario0.csv")
        tfr_pes <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/binded_tfr_draws_scenario-1.csv")
        tfr_opt <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/binded_tfr_draws_scenario1.csv")


      ### EDU

        ## Scenarios
        edu_ref <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/correlated_CY_edu/20170731_cohort_maternal_scenarios0_CY.csv")
        edu_pes <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/correlated_CY_edu/20170731_cohort_maternal_scenarios-1_CY.csv")
        edu_opt <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/covariate/education/20170608_GBD2016Final/correlated_CY_edu/20170731_cohort_maternal_scenarios1_CY.csv")

        ## Fix edu data (draw_0 -> draw_1000)
        edu_ref <- edu_ref[draw_num == "draw_0", draw_num := "draw_1000"]
        edu_opt <- edu_opt[draw_num == "draw_0", draw_num := "draw_1000"]
        edu_pes <- edu_pes[draw_num == "draw_0", draw_num := "draw_1000"]
  
    ### Get the baseline correlation matrix
        ldi_insample <- ldi_ref[year_id <= 2015, .(location_id, year_id, mean.ldi = rt_mean)]
        tfr_insample <- tfr_ref[year_id <= 2015, .(location_id, year_id, mean.tfr = draw_1)]
        edu_insample <- edu_ref[year_id <= 2015 & draw_num== "draw_1", .(location_id, year_id, mean.edu = mean)]

        ## Merge together
        insample_data_merged <- merge(x=ldi_insample, y=edu_insample, by=c("location_id", "year_id" ))
        insample_data_merged <- merge(x=insample_data_merged, y=tfr_insample, by=c("location_id", "year_id" ))
        head(insample_data_merged)

        print(corr_mat<-cor(insample_data_merged[, .( mean.edu, mean.ldi,mean.tfr)], method = "spearman"))


    ### My boi's copula funk
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
        return(data.frame(reshape2::melt(Xcorr)))
        }
          else {
              Xcorr
          }
    }


    ### Row bind all the data and convert to a multidimz array

      ## LDI

      ldi_prep <- function(data) {
        ## Subset cols
        data2 <- data[year_id >= 1990, .SD, .SDcols = c("location_id", "year_id", paste0("LDIpc_", c(1:1000)))]

        ## Ren cols
        colnames(data2) <- c("location_id", "year_id", paste0("draw_", c(1:1000)))

        ## Melt 
        data3 <- melt(data2, id.vars = c("location_id", "year_id"), variable.name = "draw_num", value.name = "data")

        ## Add varname
        data3 <- data3[, variable := "LDIpc"]

        return(data3)
      }

      ldi_ref_fc <- ldi_prep(ldi_ref)
      ldi_pes_fc <- ldi_prep(ldi_pes)
      ldi_opt_fc <- ldi_prep(ldi_opt)

      ## EDU
        edu_ref_fc <- edu_ref[year_id>=1990, .(location_id, year_id, draw_num, data, variable = "EDU")]
        edu_pes_fc <- edu_pes[year_id>=1990, .(location_id, year_id, draw_num, data, variable = "EDU")]
        edu_opt_fc <- edu_opt[year_id>=1990, .(location_id, year_id, draw_num, data, variable = "EDU")]

      ## TFR
      tfr_prep <- function(data) {
        ## Subset cols
        data2 <- data[, .SD, .SDcols = c("location_id", "year_id", paste0("draw_", c(0:999)))]

        ## Ren cols
        colnames(data2) <- c("location_id", "year_id", paste0("draw_", c(1:1000)))

        ## Melt 
        data3 <- melt(data2, id.vars = c("location_id", "year_id"), variable.name = "draw_num", value.name = "data")

        ## Add varname
        data3 <- data3[, variable := "TFR"]

        return(data3)
      }

      tfr_ref_fc <- tfr_prep(tfr_ref)
      tfr_opt_fc <- tfr_prep(tfr_opt)
      tfr_pes_fc <- tfr_prep(tfr_pes)


      ### Convert to array
      make_array_1 <- function(d1, d2, d3) {
        
        ## Row bind
        bindz <- rbind(d1, d2, d3)

        ## Make array by country
        system.time(myarray2<- mclapply(countries, 
                                function(x) reshape2::acast(bindz[location_id == x,], 
                                                      location_id ~  year_id ~ variable ~ draw_num, 
                                                      value.var = "data"), mc.cores = 20))
        ## Array bind
        myarray2 <- abind(myarray2, along=1)
        print(str(myarray2))

        return(myarray2)

      }

      system.time(ref_array <- make_array_1(ldi_ref_fc, edu_ref_fc, tfr_ref_fc))
      system.time(pes_array <- make_array_1(ldi_pes_fc, edu_pes_fc, tfr_pes_fc))
      system.time(opt_array <- make_array_1(ldi_opt_fc, edu_opt_fc, tfr_opt_fc))


      ### Extra helper: 
       ### Return a list where each item is a covariate, and cast wide on draws
         wide_caster <- function(data, var) {
           tmp <- data[variable == paste0(var), list(location_id, year_id, draw_num, data)]
           tmp <- dcast(tmp, location_id + year_id ~ draw_num, value.var = "data")
           colnames(tmp) <- c("location_id", "year_id", paste0("draw", c(0:999)))
           return(tmp)
         }


    #### Run the copula function!
      copula_array <- function(array_input, scen) {
         
        ### Copulate by countries
         system.time(corrd_list <- mclapply(countries, 
                                    function(x) cbind(x, draw2Dcopula(array_input[paste0(x),,,], corr_mat , df_return = T)),
                                    mc.cores = 25 ))         

        ### Stack the list
         stacked_list <- rbindlist(corrd_list)
         colnames(stacked_list) <- c("location_id", "year_id", "variable", "draw_num", "data")

         # print(head(stacked_list))
                
         ## Make the list
         cov_list <- mclapply(c("LDIpc", "EDU", "TFR"), function(v)  { wide_caster(stacked_list, paste0(v))[, scenario:= scen] }, mc.cores = 16 )
         names(cov_list) <- c("LDIpc", "EDU", "TFR")

         return(cov_list)
  
      }


      ## Run it!
      system.time(corr_ref_data <- copula_array(ref_array, 0))
      system.time(corr_pes_data <- copula_array(pes_array, -1))
      system.time(corr_opt_data <- copula_array(opt_array, 1))



    ### Stack each variable
    LDI_sorted <- rbindlist(list(corr_ref_data[["LDIpc"]], corr_pes_data[["LDIpc"]], corr_opt_data[["LDIpc"]]))
    EDU_sorted <- rbindlist(list(corr_ref_data[["EDU"]], corr_pes_data[["EDU"]], corr_opt_data[["EDU"]]))
    TFR_sorted <- rbindlist(list(corr_ref_data[["TFR"]], corr_pes_data[["TFR"]], corr_opt_data[["TFR"]]))

    ## Save out
    fwrite(LDI_sorted, "/ihme/forecasting/data/covariates/ldi_per_capita/national_LDIpc_corrd_with_EDU_20170831.csv")
    fwrite(EDU_sorted, "/ihme/forecasting/data/covariates/education/national_EDU_corrd_with_LDIpc_20170831.csv")
    fwrite(TFR_sorted, "/ihme/forecasting/data/covariates/TFR/national_TFR_corrd_with_LDIpc_EDU_20170831.csv")
