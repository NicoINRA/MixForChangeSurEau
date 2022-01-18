# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 
# Launcher Puechabon_LFMC avec Van-Genuchten formulation 
# Authors : Nicolas Martin (nicolas.martin@inrae.fr)
#           Julien Ruffault (julien.ruff@gmail.com)
# Date : 17/01/2022           
# ### ### ### ### ### ### #s## ### ### ### ### ### ### ### ### ### ### ### ### 

# Initialization ---------------------------------------------------------------
rm(list = ls()) # Clear environment
gc()            # Clear memory

# Set paths  -----------------------------------------------------------------
mainDir <-   dirname(dirname(rstudioapi::getActiveDocumentContext()$path))  
climateData_path          <- paste0(mainDir,'/Input_parameters/ERA_land_sureau_PtsEx_daily/ERA_land_ORPHEE_1990-2020.csv')

soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Generic_MixForChange.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/vegetation_GenericMixForChange.csv')
output_path               <-  paste0(mainDir,'/scripts_base_simulations/OutPutMixForChange.csv')

# load SurEau-Ecos ------------------------------------------------------------
source(paste0(mainDir,'/functions/load.SurEau_Ecos.R'))

modeling_options  <- create.modeling.options(compOptionsForEvapo = "Fast",
                                             transpirationModel = 'Jarvis',
                                             defoliation = F,
                                             stomatalRegFormulation = "Sigmoid",
                                             PedoTransferFormulation="VG") 

simulation_parameters <- create.simulation.parameters(startYearSimulation = 2007,                       
                                                      endYearSimulation = 2020,
                                                      mainDir = mainDir,
                                                      outputType = 'simple_subdaily',
                                                      overWrite = T,
                                                      outputPath = output_path)

stand_parameters      <- create.stand.parameters(LAImax = 3, lat = 44.7, lon = -0.8)

climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
soil_parameters       <- create.soil.parameters(filePath = soilParameters_path, modeling_options = modeling_options, offSetPsoil = .3)

vegetation_parameters <- create.vegetation.parameters(filePath = vegetationParameters_path, 
                                                      stand_parameters = stand_parameters, 
                                                      soil_parameter = soil_parameters,
                                                      modeling_options = modeling_options)

PlotTheStandAndPlant(vegetation_parameters, soil_parameters, modeling_options, openWindow=T)

run.SurEau_Ecos(modeling_options = modeling_options ,
                simulation_parameters = simulation_parameters, 
                climate_data = climate_data,
                stand_parameters = stand_parameters, 
                soil_parameters = soil_parameters,
                vegetation_parameters = vegetation_parameters)

# Output loading an plotting  ------------------------------------------
filename  = paste0(mainDir,"/scripts_base_simulations/OutPutMixForChange.csv")
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
Year= strftime(DATA$Time, format='%Y')

SWS = DATA$SWS1+DATA$SWS2+DATA$SWS3
REW = (SWS-soil_parameters$V_soil_storage_capacity_res)/(sum(soil_parameters$V_saturation_capacity)-soil_parameters$V_soil_storage_capacity_wilt)

MinREW = tapply(REW, Year, min,na.rm=T)
Pmin = tapply(DATA$Psi_LSym, Year, min,na.rm=T)
PLCYear = tapply(DATA$PLC_Leaf, Year, max,na.rm=T)

quartz()
plot(Pmin~unique(na.omit(Year)), type='b', ylim=c(-5, -2))
plot(PLCYear~unique(na.omit(Year)), type='b')

plot(DATA$Psi_LSym[1:8000])

##### 
quartz()
par(mfrow=c(3,1))
plot(DATA$transpiration_mm, type='l', ylab="Transpi (mm/h)")
plot(DATA$Psi_LSym, type='l', col=2, ylab="Water potential Leaf (MPa)")
plot(DATA$PLC_Leaf, type='l', col=2, ylim=c(0,100), ylab="PLC")

# Loading and computing  plant water potential data --------
data_potential = read.csv(paste0(mainDir,'/validation_data/Water_Potential_MIND_Control-1.csv'),dec=',',sep=';')  
data_potential = data_potential[data_potential$Treatment=='Control',]
data_PDpot     = aggregate(data_potential$Pd_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# conversion en une données jour par le PD_pot et le Md_pot
colnames(data_PDpot) <- c('Date','PDpot_measured')
data_PDpot$Date      <- as.Date(data_PDpot$Date,format='%d/%m/%Y')
data_MDpot = aggregate(data_potential$Md_pot,by=list(data_potential$Date),FUN=function(x) mean(x,na.rm=T))# conversion en une données jour par le PD_pot et le Md_pot
colnames(data_MDpot) <- c('Date','MDpot_measured')
data_MDpot$Date <- as.Date(data_MDpot$Date,format='%d/%m/%Y')


# Loading and computing spflow dataset (2016-2018)---------------------------------------------------
library(readxl)
library(lubridate)
data_sapflow = as.data.frame(read_excel(paste0(mainDir,'/validation_data/Sapflow_day_SMR_2016-2018.xlsx')))
data_sapflow$Date = as.Date(data_sapflow$Date,format='%Y-%m-%d')
colnames(data_sapflow)

# converting hourly simu data to daily 
DATA_day= aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),min)

colnames(DATA_day) <- c('DOY','YEAR','Psi_min')
DATA_day$Psi_base = aggregate(DATA$Psi_LSym,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$LAI = aggregate(DATA$LAI,by=list(yday(DATA$Time),year(DATA$Time)),max)$x
DATA_day$transpiration_mm = aggregate(DATA$transpiration_mm,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
#DATA_day$evapotranspiration_mm = aggregate(DATA$transpiration_mm+DATA$SoilEvaporation_mm,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x

DATA_day$PPT= aggregate(DATA$PPT,by=list(yday(DATA$Time),year(DATA$Time)),sum)$x
DATA_day$Date = as.Date(paste(DATA_day$DOY,DATA_day$YEAR,sep='/'),format='%j/%Y')
DATA_day$gs_max= aggregate(DATA$gs_lim,by=list(yday(DATA$Time),year(DATA$Time)),max)$x

simu_DD = merge(DATA_day,data_PDpot,by='Date',all.x=T)
simu_DD = merge(simu_DD,data_MDpot,by='Date',all.x=T)
simu_DD = merge(simu_DD,data_sapflow,by='Date',all.x=T)


DatPsi = simu_DD[!is.na(simu_DD$PDpot_measured),]
quartz()
plot(DatPsi$Psi_base~DatPsi$PDpot_measured, ylim=c(-6,0),xlim=c(-6,0), bg=4,cex=1.3, pch=21)
points(DatPsi$Psi_min~DatPsi$MDpot_measured, ylim=c(-6,0),xlim=c(-6,0), bg=2,cex=1.3, pch=21)
summary(lm(DatPsi$Psi_min~DatPsi$MDpot_measured))
summary(lm(DatPsi$Psi_base~DatPsi$PDpot_measured))
abline(0,1)
quartz()
plot(simu_DD$transpiration_mm~simu_DD$E_Av.gap)
summary(lm(simu_DD$transpiration_mm~simu_DD$E_Av.gap))

#--------------
#Test Plots
year=2017

io =year(simu_DD$Date)==year
quartz()
plot(DATA_day$Date[io],DATA_day$Psi_min[io],type='l',col=2 , main=year,)
lines(DATA_day$Date[io],(DATA_day$Psi_base[io]),type='l',col=1)
points(data_MDpot$Date,data_MDpot$MDpot_measured,col='red',pch=16)
points(data_PDpot$Date,data_PDpot$PDpot_measured,col='black',pch=16)
par(new=T)
barplot(DATA_day$PPT[io],col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,100))

axis(4,col='blue',col.ticks='blue')
grid()
quartz()
plot(simu_DD$Date[io],simu_DD$transpiration_mm[io],type='l',ylim=c(0,2.6))
points(simu_DD$Date[io],simu_DD$E_Av.gap[io],col='forestgreen')
grid()
par(new=T)
plot(simu_DD$Date[io],simu_DD$LAI[io],type='l',ylim=c(0,2.6))
quartz()
plot(simu_DD$E_Av.gap[io],simu_DD$transpiration_mm[io],main=year,pch=16,col=rgb(0.2,0.2,0.2,alpha=0.5),ylim=c(0,2.6),xlim=c(0,2.6))
abline(0,1,col=2)
lm_a =lm(simu_DD$E_Av.gap[io]  ~simu_DD$transpiration_mm[io])
summary(lm_a)
mtext(side=3, adj=1,paste0('R2 = ', round(summary(lm_a)$r.squared,digit=2)))
grid()

quartz()
plot(simu_DD$Date[io],simu_DD$E_Av.gap[io],col='forestgreen')
par(new=T)
plot(DATA_day$Date[io],DATA_day$Psi_min[io],type='l',col=2 , main=year,)
par(new=T)
plot(DATA_day$Date[io], DATA_day$gs_max[io],type='l',col=1 , main=year,)
quartz()
plot(DATA$PLC_Leaf,type='l',col=1 , main=year,)

#--------------
#Nice Plots
year=2016
for (year in 2016:2018){
  
  io =year(simu_DD$Date)==year
  quartz(width=8,height=10)
  par(mfrow=c(4,1),mar=c(2,2,2,2),oma=c(2,2,2,2))
  
  io =year(DATA_day$Date)==year
  
  plot(DATA_day$Date[io],DATA_day$Psi_min[io],type='l',col=2,ylim=c(-6,0),main=year,)
  lines(DATA_day$Date[io],DATA_day$Psi_base[io],type='l',col=1)
  points(data_MDpot$Date,data_MDpot$MDpot_measured,col='red',pch=16)
  points(data_PDpot$Date,data_PDpot$PDpot_measured,col='black',pch=16)
  par(new=T)
  barplot(DATA_day$PPT[io],col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,100))
  axis(4,col='blue',col.ticks='blue')
  grid()
  
  
  plot(simu_DD$E_Av.gap[io],simu_DD$transpiration_mm[io],main=year,pch=16,col=rgb(0.2,0.2,0.2,alpha=0.5),ylim=c(0,2.6),xlim=c(0,2.6))
  abline(0,1,col=2)
  lm_a =lm(simu_DD$E_Av.gap[io]  ~simu_DD$transpiration_mm[io])
  summary(lm_a)
  mtext(side=3, adj=1,paste0('R2 = ', round(summary(lm_a)$r.squared,digit=2)))
  grid()
  
  plot(simu_DD$Date[io],simu_DD$transpiration_mm[io],type='l',ylim=c(0,2.6))
  points(simu_DD$Date[io],simu_DD$E_Av.gap[io],col='forestgreen')
  grid()
  
  
  plot(simu_DD$Date[io],cumsum(simu_DD$transpiration_mm[io]),type='l',ylim=c(0,350))
  points(simu_DD$Date[io],cumsum(simu_DD$E_Av.gap[io]),col='forestgreen')
  grid()
  
  
  
}

io =year(DATA$Time)==2017
#quartz()

quartz(width=8,height=10)
par(mfrow=c(4,1),mar=c(2,2,2,2),oma=c(2,2,2,2))

plot(DATA_day$Date[year(DATA_day$Date)==2017],DATA_day$Psi_min[year(DATA_day$Date)==2017],type='l',col=2,ylim=c(-6,0),main=2017,)
lines(DATA_day$Date[year(DATA_day$Date)==2017],DATA_day$Psi_base[year(DATA_day$Date)==2017],type='l',col=1)
points(data_MDpot$Date,data_MDpot$MDpot_measured,col='red',pch=16)
points(data_PDpot$Date,data_PDpot$PDpot_measured,col='black',pch=16)
par(new=T)
barplot(DATA_day$PPT[year(DATA_day$Date)==2017],col='blue',border='blue',axes=F,ylab='',xlab='',ylim=c(0,100))
axis(4,col='blue',col.ticks='blue')
grid()


plot(DATA$fluxSoilToCollar1_mm[io],type='l',lwd=0.2)
lines(DATA$fluxSoilToCollar2_mm[io],type='l',col=2,lwd=0.2)
lines(DATA$fluxSoilToCollar3_mm[io],type='l',col=3,lwd=0.3)

plot(DATA$PsiSoil1[io],type='l',lwd=0.5)
lines(DATA$PsiSoil2[io],type='l',col=2,lwd=0.5)
lines(DATA$PsiSoil3[io],type='l',col=3,lwd=0.5)
#lines(DATA$Psi_AllSoil)
legend('bottomleft', col=c(1,2,3),c('layer1', 'layer2', 'layer3'),lty=1,cex=1.5)


# plot(DATA$PsiSoil1[io],type='l',lwd=0.5)
plot(DATA$REW1[io],type='l',lwd=0.5)
lines(DATA$REW2[io],type='l',col=2,lwd=0.5)
lines(DATA$REW3[io],type='l',col=3,lwd=0.5)
#lines(DATA$Psi_AllSoil)
legend('bottomleft', col=c(1,2,3),c('layer1', 'layer2', 'layer3'),lty=1,cex=1.5)



