
# rm(list=ls())
require(foreign)
require(readstata13)
require(data.table)
require(MASS)
require(ggplot2)
# require(foreach)
require(gridExtra)
require(stringr)
require(matrixStats)
require(parallel)
require(abind)
require(rlist)
require(assertthat)
require(doParallel)

# registerDoParallel(cores = 6)

## Resizing notebook plot space
options(repr.plot.width=16, repr.plot.height=9)

slots <-strtoi(Sys.getenv("NSLOTS"))
if (grepl("Intel",system("cat /proc/cpuinfo | grep 'name'| uniq",intern=TRUE))) {
    cores <- slots * 0.86
} else {
  cores <- slots * 0.64
}
print(cores)

ghes <- data.table(read.dta13("/home/j/Project/IRH/Forecasting/data_new/stats_archive_new_set/ghes_per_cap_draws_MADcap.dta"))
ghes <- ghes[ , .SD, .SDcols = c("iso3", "year", paste0("draw_",c(1:10000)))]
# head(ghes)

countries <- unique(ghes[, iso3])

gdp <- data.table(read.dta13('/home/j/Project/IRH/Forecasting/gdp/final/Copula_GDPpc_Draws_REV_linConv_RW_using_capped_MAD_data_chaos_25_10000.dta'))
gdp <- gdp[iso3 %in% countries, .SD, .SDcols = c("iso3", "year", "gdppc_mean", paste0("draw_",c(1:10000)))]
# head(gdp)

gge <- data.table(read.dta13('/home/j/Project/IRH/GGE/final/Copula_GGE_per_GDP_Draws_REV_linConv_V5_logit_gge_gdp_noConv_MADcaps_FINAL_data_chaos_25_10000.dta'))
gge <- gge[iso3 %in% countries , .SD, .SDcols = c("iso3", "year", "gge_gdp_mean", paste0("draw_",c(1:10000)))]
# head(gge)

THE <- data.table(read.dta13("/home/j/Project/IRH/Forecasting/data_new/stats_archive_new_set/the_per_cap_draws_MADcap.dta"))
THE <- THE[ , .SD, .SDcols = c("iso3", "year", paste0("draw_",c(1:10000)))]
# head(THE)

gdp_insample <- gdp[year<=2014 & year>=1995 , .SD, .SDcols = c("iso3", "year", "gdppc_mean")]
colnames(gdp_insample) <- c("iso3", "year", "mean.gdp")
gge_insample <- gge[year<=2014 & year>=1995 , .SD, .SDcols = c("iso3", "year", "gge_gdp_mean")]
colnames(gge_insample) <- c("iso3", "year", "mean.gge")
ghes_insample <- ghes[year<=2014 & year>=1995 , .SD, .SDcols = c("iso3", "year", "draw_1")]
colnames(ghes_insample) <- c("iso3", "year", "mean.ghes")
THE_insample <- THE[year<=2014 & year>=1995 , .SD, .SDcols = c("iso3", "year", "draw_1")]
colnames(THE_insample) <- c("iso3", "year", "mean.THE")

insample_data_merged <- merge(x=gdp_insample, y=gge_insample, by=c("iso3", "year"), all.y=T)
insample_data_merged <- merge(x=insample_data_merged, y=ghes_insample, by=c("iso3", "year"), all.y=T)
insample_data_merged <- merge(x=insample_data_merged, y=THE_insample, by=c("iso3", "year"), all.y=T)
head(insample_data_merged)

print(corr_mat<-cor(insample_data_merged[, .( mean.gdp, mean.gge, mean.ghes, mean.THE)], method = "spearman"))

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

gdp_fc <- gdp[, .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:10000)))]
gdp_fc <- gdp_fc[, variable := "GDPpc"]
gdp_fc <- setcolorder(gdp_fc, c("iso3","year", "variable", paste0("draw_", c(1:10000))) )
gdp_fc <- melt(gdp_fc, id.vars=c("iso3","year", "variable"), variable.name = "draw_num", value.name = "data")
head(gdp_fc)

gge_fc <- gge[, .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:10000)))]
gge_fc <- gge_fc[, variable := "GGE_GDP"]
gge_fc <- setcolorder(gge_fc, c("iso3","year", "variable", paste0("draw_", c(1:10000))) )
gge_fc<- melt(gge_fc, id.vars=c("iso3","year", "variable"), variable.name = "draw_num", value.name = "data")
head(gge_fc)

ghes_fc <- ghes[, .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:10000)))]
ghes_fc <- ghes_fc[, variable := "GHES_pc"]
ghes_fc <- setcolorder(ghes_fc, c("iso3","year", "variable", paste0("draw_", c(1:10000))) )
ghes_fc<- melt(ghes_fc, id.vars=c("iso3","year", "variable"), variable.name = "draw_num", value.name = "data")
head(ghes_fc)

THE_fc <- THE[, .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:10000)))]
THE_fc <- THE_fc[, variable := "THE_pc"]
THE_fc <- setcolorder(THE_fc, c("iso3","year", "variable", paste0("draw_", c(1:10000))) )
THE_fc<- melt(THE_fc, id.vars=c("iso3","year", "variable"), variable.name = "draw_num", value.name = "data")
head(THE_fc)

system.time(all_data_binded <- rbind(gdp_fc, gge_fc, ghes_fc, THE_fc))
all_data_binded <- all_data_binded[year >=1995,]
tail(all_data_binded)

system.time(myarray2<- mclapply(countries, 
                                function(x) reshape2::acast(all_data_binded[iso3 == x ,], 
                                                            iso3 ~  year ~ variable ~ draw_num, 
                                                      value.var = "data"), mc.cores=16, mc.preschedule = F))



# system.time(
#     myarray2 <- foreach(i = countries) %dopar% {
#        temp <- reshape2::acast(all_data_binded[iso3 == paste0(i) ,], 
#               iso3 ~  year ~ variable ~ draw_num,  value.var = "data")
#     return(temp)
#         }
# )

myarray2 <- abind(myarray2, along=1)
str(myarray2)



system.time(Xcorr_array <- mclapply(countries, 
                                     function(x) cbind(x, draw2Dcopula(myarray2[paste0(x),,,], 
                                     corr_mat , df_return = T)), mc.cores=16, mc.preschedule = F ))



system.time(test2 <- list.stack(Xcorr_array, data.table=T))
colnames(test2) <- c("iso3", "year", "variable", "draw_num", "data")
str(test2)
head(test2)
tail(test2)                  

### Stack into a DT
test3 <- dcast(test2, iso3 + year + variable ~ draw_num, value.var = c("data"))
head(test3)

### Validation
test4 <- melt(test3, id.vars=c("iso3", "year", "variable"), value.name = "data", variable.name = "draw_num")
test4 <- dcast(test4, iso3 + year + draw_num ~ variable)
test4 <- test4[,  GGE_pc := GGE_GDP * GDPpc]
# head(test4)

test4 <- test4[, GDP_minus_GHES := GDPpc - GHES_pc]
test4 <- test4[, GDP_minus_GGE := GDPpc - GGE_pc]
test4 <- test4[, GGE_minus_GHES := GGE_pc - GHES_pc]
test4 <- test4[, GDP_minus_THE := GDPpc - THE_pc]
# head(test4)

## Subset out where inequalities break
test5 <- test4[GDP_minus_GHES < 0 | GDP_minus_GGE < 0 | GGE_minus_GHES < 0 | GDP_minus_THE < 0,]
bads <- unique(test5[,draw_num])

length(bads)

## Drop the bads
test4 <- test4[!draw_num %in% bads,]

## Test!!
summary(test4[, GDP_minus_GHES])
summary(test4[, GDP_minus_GGE])
summary(test4[, GGE_minus_GHES])
summary(test4[, GDP_minus_THE])

## Let's melt and cast out and rename and sample 1000
test6 <- melt(test4[, .(iso3, year, draw_num, GDPpc, GGE_GDP, GHES_pc, GGE_pc, THE_pc)], id.vars = c("iso3", "year", "draw_num"), value.name = "data")
test6 <- dcast(test6, iso3 + year + variable ~ draw_num, value.var = "data")
dim(test6)
head(test6)

## Rename
len_draw <- length(grep("draw_", names(test6), value = TRUE))
colnames(test6) <- c("iso3", "year", "variable", paste0("draw_",c(1:len_draw)))
# head(test6)
test7 <- test6[, .SD , .SDcols = c("iso3", "year", "variable", paste0("draw_", sample(len_draw, 1000,replace = F)))]
colnames(test7) <- c("iso3", "year", "variable", paste0("draw_",c(1:1000)))
head(test7)

GDP_sorted <- test7[variable=="GDPpc", .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:1000)))]
colnames(GDP_sorted) <- c("iso3","year", paste0("draw_", c(1:1000)))
GGE_sorted <- test7[variable=="GGE_GDP", .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:1000)))]
colnames(GGE_sorted) <- c("iso3","year", paste0("draw_", c(1:1000)))
GHES_sorted <- test7[variable=="GHES_pc", .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:1000)))]
colnames(GHES_sorted) <- c("iso3","year", paste0("draw_", c(1:1000)))
THE_sorted <- test7[variable=="THE_pc", .SD, .SDcols = c("iso3","year", paste0("draw_", c(1:1000)))]
colnames(THE_sorted) <- c("iso3","year", paste0("draw_", c(1:1000)))

head(GDP_sorted)
head(GGE_sorted)
head(GHES_sorted)
head(THE_sorted)

## Save out
fwrite(GDP_sorted, "/home/j/Project/IRH/Forecasting/data_new/GDPpc_sorted.csv" )
fwrite(GGE_sorted, "/home/j/Project/IRH/Forecasting/data_new/GGE_GDP_sorted.csv" )
fwrite(GHES_sorted, "/home/j/Project/IRH/Forecasting/data_new/GHES_pc_sorted.csv" )
fwrite(THE_sorted, "/home/j/Project/IRH/Forecasting/data_new/THE_pc_sorted.csv" )

fwrite(GDP_sorted[year==2014 | year == 2040, ], "/home/j/Project/IRH/Forecasting/data_new/GDPpc_sorted_2014_2040.csv" )
fwrite(GGE_sorted[year==2014 | year == 2040, ], "/home/j/Project/IRH/Forecasting/data_new/GGE_GDP_sorted_2014_2040.csv" )
fwrite(GHES_sorted[year==2014 | year == 2040, ], "/home/j/Project/IRH/Forecasting/data_new/GHES_pc_sorted_2014_2040.csv" )
fwrite(THE_sorted[year==2014 | year == 2040, ], "/home/j/Project/IRH/Forecasting/data_new/THE_pc_sorted_2014_2040.csv" )

