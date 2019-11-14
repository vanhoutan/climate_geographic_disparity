### compare clim anomarlies from MPI-ESM and CMIP5 ENSEM ###

rm(list = ls())

#CMIP5 ENSMN based anomalies
ens_2006_2055 = stack("/Users/ktanaka/clim_geo_disp/data/CMIP5 ENSMN RCP4.5 anomaly (2006-2055)-(1956-2005).nc", varname = "anomaly") #RCP8.5 anomaly (2050-2099)-(1956-2005)
ens_2050_2099 = stack("/Users/ktanaka/clim_geo_disp/data/CMIP5 ENSMN RCP8.5 anomaly (2050-2099)-(1956-2005).nc", varname = "anomaly") #RCP8.5 anomaly (2006-2055)-(1956-2005)

#CMIP5 ENSMN based anomalies
mpi_2006_2055 = stack("/Users/ktanaka/clim_geo_disp/data/MPI-ESM-MR RCP8.5 anomaly (2006-2055)-(1956-2005).nc", varname = "anomaly") #RCP8.5 anomaly (2050-2099)-(1956-2005)
mpi_2050_2099 = stack("/Users/ktanaka/clim_geo_disp/data/MPI-ESM-MR RCP8.5 anomaly (2050-2099)-(1956-2005).nc", varname = "anomaly") #RCP8.5 anomaly (2006-2055)-(1956-2005)

par(mfrow = c(2,3))
anom = stack(ens_2006_2055, mpi_2006_2055, ens_2050_2099, mpi_2050_2099)
names(anom) = c("CMIP5_Ensemble_Mean_2006:2055", "MPI_ESM_MR_2006:2055", "CMIP5_Ensemble_Mean_2050:2099", "MPI_ESM_MR_2050:2099")
plot(anom, col = matlab.like(100))

diff_1 = anom$CMIP5_Ensemble_Mean_2006.2055 - anom$MPI_ESM_MR_2006.2055
diff_2 = anom$CMIP5_Ensemble_Mean_2050.2099 - anom$MPI_ESM_MR_2050.2099
diff = stack(diff_1, diff_2)
names(diff) = c("Difference_2006:2055", "Difference_2050:2099")
plot(diff, col = matlab.like(100))

anom = stack(anom, diff)
plot(anom, col = matlab.like(100))

pdf("/Users/ktanaka/Dropbox/PAPER climate geographic disparities/figures/supplemental/CMIP5_MPI_anomalies_comparison.pdf", 
    height = 6, width = 8)
spplot(anom, col.regions = matlab.like(100))
dev.off()
 