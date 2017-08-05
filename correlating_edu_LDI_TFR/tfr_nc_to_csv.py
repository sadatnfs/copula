

# Small code to convert netcdf to data frame in Python

import xarray as xr
import pandas as pd

for scen in [-1, 0, 1]:
    scenarios = xr.open_dataarray('/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_draws.nc')
    scenarios = scenarios.loc[{'scenario':scen}]
    scenarios = scenarios.to_dataframe()
    scenarios = scenarios.drop('scenario', axis=1)
    scenarios = scenarios.unstack()
    scenarios = scenarios['tfr'].reset_index()
    scenarios =  scenarios.rename(columns={i:'draw_{}'.format(i) for i in range(1000)})
    scenarios.to_csv('/ihme/forecasting/data/fbd_scenarios_data/forecast/asfr/best/tfr_csv/tfr_draws_scenario{}.csv'.format(scen), index=False)
    print(scen)



