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

Site= "ORPHEE"
Site= "B_tree"
Site= "FORBIO_Gedinne"
Site= "FORBIO_Hechtel"
Site= "FORBIO_Zedelgem"
Site= "IDENT_Freiburg"
Site= "IDENT_Macomer"
Site= "Satakunta"




climateData_path          <- paste0(mainDir,'/Input_parameters/ERA_land_sureau_PtsEx_daily/ERA_land_',Site  , '_1990-2020.csv')

soilParameters_path       <- paste0(mainDir,'/Input_parameters/Soil_Generic_MixForChange.csv')
vegetationParameters_path <- paste0(mainDir,'/Input_parameters/vegetation_GenericMixForChange.csv')

output_path               <-  paste0(mainDir,'/scripts_base_simulations/OutPutMixForChange_',Site, '.csv')

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
Site= "ORPHEE"
Site= "B_tree"
Site= "FORBIO_Gedinne"
Site= "FORBIO_Hechtel"
Site= "FORBIO_Zedelgem"
Site= "IDENT_Freiburg"
Site= "IDENT_Macomer"
Site= "Satakunta"

climateData_path          <- paste0(mainDir,'/Input_parameters/ERA_land_sureau_PtsEx_daily/ERA_land_',Site  , '_1990-2020.csv')
climate_data          <- create.climate.data(filePath = climateData_path, 
                                             modeling_options = modeling_options,
                                             simulation_parameters = simulation_parameters) #
filename  = paste0(mainDir,'/scripts_base_simulations/OutPutMixForChange_',Site, '.csv')
DATA      = read.csv(filename,header=T, dec='.', sep="")
DATA$Time = as.POSIXct(DATA$Time,format='%Y-%m-%d/%H:%M:%S')
Year = strftime(DATA$Time, format='%Y')
DOY = strftime(DATA$Time, format='%j')

SWS = DATA$SWS1+DATA$SWS2+DATA$SWS3
REW = (SWS-soil_parameters$V_soil_storage_capacity_res)/(sum(soil_parameters$V_saturation_capacity)-soil_parameters$V_soil_storage_capacity_wilt)

SWS_mean = tapply(SWS, DOY, min,na.rm=T)

Pmin = tapply(DATA$Psi_LSym, Year, min,na.rm=T)
PLCYear = tapply(DATA$PLC_Leaf, Year, max,na.rm=T)

# quartz()
# plot(Pmin~unique(na.omit(Year)), type='b', ylim=c(-5, -2), main = Site)
# abline(h=vegetation_parameters$P50_VC_Leaf)
# plot(SWS, type='l')
# plot(DATA$Psi_LSym, type='l')
# plot(Pmin, type='l')

#-------------------
MeanT = tapply(climate_data$Tair_mean,climate_data$DOY, mean)
MaxT = tapply(climate_data$Tair_max,climate_data$DOY, mean)
MinT = tapply(climate_data$Tair_min,climate_data$DOY, mean)
MeanRG = tapply(climate_data$RG_sum,climate_data$DOY, mean)

RWC = SWS_mean/max(SWS_mean)
quartz()
par(mfrow=c(2,2), pty="s", las=1)
plot(MeanT, type='l', ylim=c(-3, 30), ylab="temperature", main=Site)
lines(MaxT, type='l', col=2)
lines(MinT, type='l', col=4)
plot(MeanRG, type='l', ylab="Radiation (MJ)", col="orange", lwd=2)
par(new=T)
plot(RWC, type='l', yaxt='n',ylab="", ylim=c(0.3,1), lwd=2)
axis(4)
mtext("RWC",4,2.5)
plot(SWS, type='l')
plot(Pmin, type='l')


