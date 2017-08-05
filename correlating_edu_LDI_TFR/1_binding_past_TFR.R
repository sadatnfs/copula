
  ### Bind TFR past and future

  rm(list=ls())
  .libPaths("/homes/sadatnfs/R/x86_64-unknown-linux-gnu-library/3.4/")
  library("data.table")


  ## Past TFR 
  past_tfr <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/past_tfr.csv")
  past_tfr <- past_tfr[, .(location_id, year_id, tfr)]

  ## Duplicate draws
  past_tfr <- past_tfr[, (paste0("draw_", c(0:999))) := lapply(c(0:999), function(x) get("tfr") )]
  past_tfr[, tfr:= NULL]

  
  ## Get the three future TFRs
  ref_tfr <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/tfr_draws_scenario0.csv")
  pes_tfr <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/tfr_draws_scenario-1.csv")
  opt_tfr <- fread("/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/tfr_draws_scenario1.csv")

  ## Bind with past
  ref_tfr <- rbind(past_tfr, ref_tfr)[order(location_id, year_id)]
  pes_tfr <- rbind(past_tfr, pes_tfr)[order(location_id, year_id)]
  opt_tfr <- rbind(past_tfr, opt_tfr)[order(location_id, year_id)]

  ## Save out
  fwrite(ref_tfr, "/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/binded_tfr_draws_scenario0.csv")
  fwrite(pes_tfr, "/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/binded_tfr_draws_scenario-1.csv")
  fwrite(opt_tfr, "/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/binded_tfr_draws_scenario1.csv")