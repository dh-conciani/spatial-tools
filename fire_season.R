## DH - Análise temporal do regime de fogo

library (rgdal)
library (raster)

#1. read .shp data
#dns is the folder, layer is the shapefile without extension 
focus <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='data')
focus.unidades <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='unidades')
focus.eecl <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='ecologica')
focus.eex <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='experimental')
focus.anexo <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='anexo')
focus.buffer <- readOGR(dsn= "H:/results/LANDSAT mapas/data", layer= 'buffer')

#2. check spatial projection
print(proj4string(focus))
print(proj4string(focus.unidades))
print(proj4string(focus.eecl))
print(proj4string(focus.eex))
print(proj4string(focus.anexo))
print(proj4string(focus.buffer))

# 3. plot a simple map to check
plot(focus, axes=TRUE, border="black", col='red')
plot(focus.unidades, axes=TRUE, border="black", col='red')
plot(focus.eecl, axes=TRUE, border="black", col='red')
plot(focus.eex, axes=TRUE, border="black", col='red')
plot(focus.anexo, axes=TRUE, border="black", col='red')
plot(focus.buffer, axes=TRUE, border="black", col='red')

# 4. create a raster mask with same data lenght/ projection and 30 meters resolution
mask.focus <- raster(crs = projection(focus), ext = extent(focus))
            res(mask.focus)=30.0
mask.unidades <- raster(crs = projection(focus.unidades), ext = extent(focus.unidades))
                     res(mask.unidades)=30.0
mask.eecl <- raster(crs = projection(focus.eecl), ext = extent(focus.eecl))
                        res(mask.eecl)=30.0
mask.eex <- raster(crs = projection(focus.eex), ext = extent(focus.eex))
                        res(mask.eex)=30.0
mask.anexo <- raster(crs = projection(focus.anexo),ext = extent(focus.anexo))
                        res(mask.anexo)=30.0
mask.buffer <- raster(crs = projection(focus.buffer),ext = extent(focus.buffer))
                        res(mask.buffer)=30.0

                        
# 5. Count pixel frequency (polygon to raster) 
raster.focus <- rasterize(focus, mask.focus, field = 30, fun = "count", 
                     update = TRUE, updateValue = "NA")
raster.unidades <- rasterize(focus.unidades, mask.unidades, field = 30, fun = "count", 
                          update = TRUE, updateValue = "NA")
raster.eecl <- rasterize(focus.eecl, mask.eecl, field = 30, fun = "count", 
                          update = TRUE, updateValue = "NA")
raster.eex <- rasterize(focus.eex, mask.eex, field = 30, fun = "count", 
                          update = TRUE, updateValue = "NA")
raster.anexo <- rasterize(focus.anexo, mask.anexo, field = 30, fun = "count", 
                          update = TRUE, updateValue = "NA")
raster.buffer <- rasterize(focus.buffer, mask.buffer, field = 30, fun = "count", 
                          update = TRUE, updateValue = "NA")

# 6. Plot frequency maps
plot(raster.focus, axes= TRUE)
plot(raster.unidades, axes= TRUE)
plot(raster.eecl, axes= TRUE)
plot(raster.eex, axes= TRUE)
plot(raster.anexo, axes= TRUE)
plot(raster.buffer, axes= TRUE)

#export geotiff raster
writeRaster (raster.focus, filename='count_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.unidades, filename='Unidades_count_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.eecl, filename='EEcl_count_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.eex, filename='EEx_count_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.anexo, filename='Anexo_count_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.buffer, filename='Buffer_count_1985_2016.tif',format='GTiff', overwrite=TRUE)

#######################################################################################################

#Last fire occurrence 

# 7. Define year value as numeric
year.focus <- as.numeric(paste(focus$year))
year.unidades <- as.numeric(paste(focus.unidades$year))
year.eecl <- as.numeric(paste(focus.eecl$year))
year.eex <- as.numeric(paste(focus.eex$year))
year.anexo <- as.numeric(paste(focus.anexo$year))
year.buffer <- as.numeric(paste(focus.buffer$year))

# 8. Last fire occurence (max value $year pixel) 
raster.focus.last <- rasterize(focus, field= year.focus , fun = "max", mask.focus,
                              update = TRUE, updateValue = "NA")
raster.unidades.last <- rasterize(focus.unidades, field= year.unidades , fun = "max", mask.unidades,
                              update = TRUE, updateValue = "NA")
raster.eecl.last <- rasterize(focus.eecl, field= year.eecl , fun = "max", mask.eecl,
                     update = TRUE, updateValue = "NA")
raster.eex.last <- rasterize(focus.eex, field= year.eex , fun = "max", mask.eex,
                              update = TRUE, updateValue = "NA")
raster.anexo.last <- rasterize(focus.anexo, field= year.anexo , fun = "max", mask.anexo,
                              update = TRUE, updateValue = "NA")
raster.buffer.last <- rasterize(focus.buffer, field= year.buffer , fun = "max", mask.buffer,
                              update = TRUE, updateValue = "NA")

#9. Plot last fire diagnostics
plot(raster.focus.last, axes= TRUE)
plot(raster.unidades.last, axes= TRUE)
plot(raster.eecl.last, axes= TRUE)
plot(raster.eex.last, axes= TRUE)
plot(raster.anexo.last, axes= TRUE)
plot(raster.buffer.last, axes= TRUE)

# write raster data
writeRaster (raster.focus.last, filename='last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.unidades.last, filename='Unidades_last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.eecl.last, filename='EEcl_last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.eex.last, filename='EEx_last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.anexo.last, filename='Anexo_last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)
writeRaster (raster.buffer.last, filename='Buffer_last_fire_1985_2016.tif',format='GTiff', overwrite=TRUE)

#####################################################################################################################

library (plyr)
library(tidyr)

#Pt.2 - Organize data

#Define functions
IC<-function(x)
{
  qt(0.975,(length(x)-1))*sqrt(var(x,na.rm=T))/sqrt(length(x)-1)
}
sem<-function(x)
{
  sqrt(var(x,na.rm=T))/sqrt(length(x))
}

#Fuse data of EEcl + 'Anexo' (same ecological context)
str(focus.unidades@data)
junc.eecl <- subset (focus.unidades, class!='Experimental', drop=T)
list (junc.eecl$month)
plot(junc.eecl)

#Mensal Burned area for 32 years - Estação Ecológica
facteur=factor(paste(junc.eecl$month,junc.eecl$month,sep="_"))
moy=tapply(junc.eecl$area,facteur,sum)
Month <- c("10", "12", "2", "3", "4", "6", "7", "8", "9")  #FACTOR "11" IN JUNC.EECL WITH "ZERO" DATA
resultats=data.frame(Month,Sum_Burned_Area=paste(round(moy,2)))
eecl.month.area <- resultats #somatória

#Mensal Burned area for 32 years - Estação Experimental
facteur=factor(paste(focus.eex$month,focus.eex$month,sep="_"))
moy=tapply(focus.eex$area,facteur,sum)
Month=levels(focus.eex$month)
resultats=data.frame(Month,Sum_Burned_Area=paste(round(moy,2)))
eex.month.area <- resultats
eex.month.area

#Mensal Burned Area - Buffer
facteur=factor(paste(focus.buffer$month,focus.buffer$month,sep="_"))
moy=tapply(focus.buffer$area,facteur,sum)
Month=levels(focus$month)
resultats=data.frame(Month,Sum_Burned_Area=paste(round(moy,2)))
buffer.month.area <- resultats
buffer.month.area
rm(resultats)

#Read fire frequency per month
month.eecl.count <- table (junc.eecl$month)
month.eex.count <- table (focus.eex$month)
month.buffer.count <- table (focus.buffer$month)


#write xlsx
library(xlsx)
library (ggplot2)
#write.xlsx(buffer.month.area, file="buffer_area.xlsx", sheetName="sheet1")
#write.xlsx(eecl.month.area, file="eecl_area.xlsx", sheetName="sheet1")
#write.xlsx(eex.month.area, file="eex_area.xlsx", sheetName="sheet1")
#write.xlsx(month.buffer.count, file="buffer_freq.xlsx", sheetName="sheet1")
#write.xlsx(month.eecl.count, file="eecl_freq.xlsx", sheetName="sheet1")
#write.xlsx(month.eex.count, file="eex_freq.xlsx", sheetName="sheet1")
#write.xlsx(month.eex.count, file="eex_freq.xlsx", sheetName="sheet1")

#Here the archives are processed in Excel2007 to make continous data and calc percentage 
#of burned area 

#Read Table
month <- read.table ("month.txt", header=TRUE)
x <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D") #Levels to plot
buffer= subset (month, Class=="Buffer", drop=T)
eecl= subset (month, Class=='Ecologica', drop=T)
eex= subset (month, Class=='Experimental', drop=T)

#Make graph C
#Estação Ecológica
x11()
par(mfrow=c(3,3))

barplot (eecl$Sum_Percent, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Estação Ecológica', ylim=c(0,40))
axis(side=4, ylab= "")
par(new=TRUE)
plot (eecl$Freq, type='o', pch=16, col='brown', axes=F, xlab= "", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 10, legend= c('Área média de queima', 'Frequencia'),
        #col=c("gray70", "brown"), lty=1:1, cex=0.9,
        #bg= 'white')

#Estação Experimental
barplot (eex$Sum_Percent, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Estação Experimental', ylim=c(0,1))
axis(side=4, ylab= "Área queimada (%)")
par(new=TRUE)
plot (eex$Freq, type='o', pch=16, col='brown', axes=F, xlab= "", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 7, legend= c('Area média de queima', 'Frequencia'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
#bg= 'white')

#Buffer
barplot (buffer$Sum_Percent, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Buffer', ylim=c(0,30))
axis(side=4, ylab= "")
par(new=TRUE)
plot (buffer$Freq, type='o', pch=16, col='brown', axes=F, xlab= "Meses", ylab= "Nº de cicatriz(es)")
axis(side=2, labels= TRUE)
box()
#legend (1, 275, legend= c('Área queimada (%)', 'Nº de cicatriz(es)'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
#bg= 'white')



##############################################################################################
#Graph B

#Make graph
#Estação Ecológica
barplot (eecl$Annual_Percent_Mean, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Estação Ecológica', ylim=c(0,1.5))
axis(side=4, ylab= "")
par(new=TRUE)
plot (eecl$Freq, type='o', pch=16, col='brown', axes=F, xlab= "", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 10, legend= c('Área média de queima', 'Frequencia'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
#bg= 'white')

#Estação Experimental
barplot (eex$Annual_Percent_Mean, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Estação Experimental', ylim=c(0,0.03))
axis(side=4, ylab= "Área queimada (%)")
par(new=TRUE)
plot (eex$Freq, type='o', pch=16, col='brown', axes=F, xlab= "", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 7, legend= c('Area média de queima', 'Frequencia'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
#bg= 'white')

#Buffer
barplot (buffer$Annual_Percent_Mean, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Buffer', ylim=c(0,1))
axis(side=4, ylab= "")
par(new=TRUE)
plot (buffer$Freq, type='o', pch=16, col='brown', axes=F, xlab= "Meses", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 275, legend= c('%Área queimada/ 32anos', 'Nº de cicatriz(es)'),
        #col=c("gray70", "brown"), lty=1:1, cex=0.9,
       # bg= 'white')

###########################################################################################################
#Graph A
barplot (eecl$Mean_percent_scar, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Estação Ecológica', ylim=c(0,5))
axis(side=4, ylab= "")
par(new=TRUE)
plot (eecl$Freq, type='o', pch=16, col='brown', axes=F, xlab= "", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 10, legend= c('Área média de queima', 'Frequencia'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
#bg= 'white')

#Estação Experimental
barplot (eex$Mean_percent_scar, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Estação Experimental', ylim=c(0,0.5))
axis(side=4, ylab= "Área queimada (%)")
par(new=TRUE)
plot (eex$Freq, type='o', pch=16, col='brown', axes=F, xlab= "", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 7, legend= c('Area média de queima', 'Frequencia'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
#bg= 'white')

#Buffer
barplot (buffer$Mean_percent_scar, col='ivory3', names.arg=x, axes=F, ylab= NULL,
         main='Buffer', ylim=c(0,0.1))
axis(side=4, ylab= "")
par(new=TRUE)
plot (buffer$Freq, type='o', pch=16, col='brown', axes=F, xlab= "Meses", ylab= "")
axis(side=2, labels= TRUE)
box()
#legend (1, 275, legend= c('%Área queimada/ 32anos', 'Nº de cicatriz(es)'),
#col=c("gray70", "brown"), lty=1:1, cex=0.9,
# bg= 'white')

#########################################################################################################

#Triple bar plot to compare % of annual burned area on treatments 
#Sum of burned area in EEcl along the 32 years
facteur=factor(paste(junc.eecl$year,junc.eecl$year,sep="_"))
moy=tapply(junc.eecl$area,facteur,sum)
Year=levels(facteur)
resultats=data.frame(Year,Burned_Area=paste(round(moy,2)))
eecl.year.area <- resultats
eecl.year.area

#Sum of burned area in EEx along the time
facteur=factor(paste(focus.eex$year,focus.eex$year,sep="_"))
moy=tapply(focus.eex$area,facteur,sum)
Year=levels(focus.eex$year)
resultats=data.frame(Year,Burned_Area=paste(round(moy,2)))
eex.year.area <- resultats
eex.year.area

#Sum of burned area in Buffer along the time
facteur=factor(paste(focus.buffer$year,focus.buffer$year,sep="_"))
moy=tapply(focus.buffer$area,facteur,sum)
Year=levels(focus$year)
resultats=data.frame(Year,Burned_Area=paste(round(moy,2)))
buffer.year.area <- resultats
buffer.year.area
rm(resultats)

#Read fire frequency per year
year.eecl.count <- table (junc.eecl$year)
year.eex.count <- table (focus.eex$year)
year.buffer.count <- table (focus.buffer$year)

#Write XLSX to calc percent area in Excel2007
library (xlsx)
#write.xlsx(year.buffer.count, file="buffer_year_freq.xlsx", sheetName="sheet1")
#write.xlsx(year.eecl.count, file="eecl_year_freq.xlsx", sheetName="sheet1")
#write.xlsx(year.eex.count, file="eex_year_freq.xlsx", sheetName="sheet1")


# Create the input vectors.
colors <- c("chartreuse4","chocolate","dimgray")
colors2 <- c("chartreuse4", "chocolate","dimgray","dodgerblue4")
color3 <- c ('lightgreen', 'tomato2', 'steelblue3')
management <- c("EEcl", "Buffer","EEI", "Precip. acum.")

#Load the count values
data <- read.table ('year.txt', header=TRUE)
df= subset (data, class=="Ecologica", drop=T) #to exclude multiple precip/year data
precip= df$accum_precip
years= df$year
percents <- data$percent_burned
data

#Place data in a matrix that will have 3 columns since the number levels() for Journal is 3.
#Also, based on the ordering feature of the data, load the matrix such that we fill the matrix by row. 
percents=matrix (percents,ncol=3,byrow=F)
percents

# Create the matrix of the values.
Val <- matrix(percents ,nrow = 3,ncol = 32,byrow = TRUE) #3 bars(treat) vs 32 groups (years)
Val

# Create the bar chart.
x11()
barplot(Val, names.arg = years, xlab = "Anos",ylab = "Área queimada (%)",
        col = colors, ylim = c(0,20), beside=TRUE, cex.axis=1, cex.lab=1, cex=0.7)
box()
legend("topleft", management, cex = 1, fill = colors2)
par(new=TRUE)
plot (precip~years, type='o', col=c('dodgerblue4'), axes=F, ylab= '', xlab='', pch=16, lty=1,
      lwd=1)
axis(side=4, labels=T)


#################################################################################################
#Plot 3 - Mean percent burned area in function of months per treatment - Multiple bars

data= read.table ('month.txt', header=TRUE)
months= x
data
percents <- data$Sum_Percent
management <- c("EEcl", "Buffer","EEI")

#Place data in a matrix that will have 3 columns since the number levels() for Journal is 3.
#Also, based on the ordering feature of the data, load the matrix such that we fill the matrix by row. 
percents=matrix (percents,ncol=3,byrow=F)
percents

# Create the matrix of the values.
Val <- matrix(percents ,nrow = 3,ncol = 12,byrow = TRUE) #3 bars(treat) vs 12 groups (months)
Val

#Plot
x11()
barplot(Val, names.arg = months, xlab = "Meses",ylab = "Área queimada (%)",
        col = color3, ylim = c(0,45), beside=TRUE, horiz=FALSE, cex.axis=0.7, cex.lab=0.8, cex=0.7, xlim=c(0,50))
legend("topleft", management, cex = 1, fill = color3)
box()


#########################################################################################

library (ggplot2)
year <- read.table ('year.txt', header=TRUE)
eecl= subset (year, class=='Ecologica', drop=T)
buffer= subset (year, class=='Buffer', drop=T)
eex= subset (year, class=='Experimental', drop=T)
eecl

#Year Analisys
#EEcl
sp1 <- ggplot(eecl, aes(x=Freq,y=mean_burned,label=year)) +
  geom_point(aes(size=percent_burned), col='red', alpha=0.5) +  
  theme_bw(base_size=12) + 
  scale_size(name=" Área queimada (%)",range=c(1,12)) + 
  ylab("Área média da cicatriz (ha)") + xlab("Cicatriz de queima") + 
  geom_text(aes(label=year, hjust=0, vjust=0)) +
  ggtitle('EEcl')
sp1 

#EEx
sp2 <- ggplot(eex, aes(x=Freq,y=mean_burned,label=year)) +
  geom_point(aes(size=percent_burned), col='red', alpha =0.5) +
  theme_bw(base_size=12) +
  scale_size(name=" Área queimada (%)",range=c(1,12)) +
  ylab("Área média da cicatriz (ha)") + xlab("Cicatriz de queima") + 
  geom_text(aes(label=year),hjust=0, vjust=0) +
  ggtitle('EEI')
sp2

#Buffer
sp3 <- ggplot(buffer, aes(x=Freq,y=mean_burned,label=year)) +
  geom_point(aes(size=percent_burned), col='red', alpha= 0.5) + 
  theme_bw(base_size=12) +
  ylab("Área média da cicatriz (ha)") + 
  xlab("Cicatriz de queima") + 
  scale_size(name= "Área queimada (%)", range=c(1,12)) +
  geom_text(aes(label=year), hjust=0, vjust=0) +
  ggtitle('Buffer')
sp3

data= read.table ('year.txt', header = TRUE)
str(data)
sp4 <- ggplot(data=data, aes(x=year, y=accum_percent, group = class, colour=class)) +
  geom_line() +
  xlab('Anos') + ylab('Área queimada (%)') + theme_bw() +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2016)) +
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100, 120)) 
sp4

library(gridExtra)
x11()
grid.arrange(sp1, sp2, sp3, sp4, nrow=2, ncol=2)


##################################################################################################
#Models 
#Compare burned area (%) in UC's along 32 years
data <- read.table ('year.txt', header= TRUE)

## Parameter test
library(nortest)
hist(data$percent_burned)
lillie.test(data$percent_burned)
shapiro.test(aov(data$percent_burned ~ data$class)$residuals)
shapiro.test(aov(sqrt(data$percent_burned) ~ data$class)$residuals)
shapiro.test(aov(log(data$percent_burned) ~ data$class)$residuals)
kruskal.test(data$percent_burned ~ data$class)

#mod <- lm(percent_burned ~ class, data= data)
#summary(mod)
#hsd<- aov(percent_burned ~ class, data= data)
#TukeyHSD(hsd)
#summary(hsd)
#boxplot (percent_burned ~ class, data= data)

#Compare fire density in UC's along 32 years
#mod <- lm(density ~ class, data= data)
#summary(mod)
#hsd<- aov(density ~ class, data= data)
#TukeyHSD(hsd)
#summary(hsd)
#boxplot (density ~ class, data= data)

##########################################################################
# STATISTICAL MODELS - cronological time

eecl= subset (year, class=='Ecologica', drop=T)
eex= subset (year, class=='Experimental', drop=T)
buffer= subset (year, class=='Buffer', drop=T)

#12 months accumuled precipitation 
x11()
par(mfrow=c(4,3))

#EEcl
mod12.eecl <- lm (burned_area ~ accum_precip, data = eecl)
mod12.eecl
summary(mod12.eecl) #R² = 0.1212  p 0.050
plot (burned_area ~ accum_precip, data= eecl, pch=20, main= 'EEcl',
      xlab='Precipitação acumulada em 12 meses (mm)', ylab= 'Área queimada')
#abline (a=-188.7627, b= 0.1917, col='tomato1')
ba= as.matrix (eecl$burned_area)
ap= as.matrix (eecl$accum_precip)
cor (ba, ap)


#EEx
mod12.eex <- lm (burned_area ~ accum_precip, data = eex)
mod12.eex
summary(mod12.eex) #R² = 0.001485 p 0.8342
plot (burned_area ~ accum_precip, data= eex, pch=20, main= 'EEx',
      xlab='Precipitação acumulada em 12 meses (mm)', ylab= 'Área queimada')
#abline (a=13.662869, b= -0.003497, col='tomato1')
ba= as.matrix (eex$burned_area)
ap= as.matrix (eex$accum_precip)
cor (ba, ap)

#Buffer
mod12.buffer <- lm (burned_area ~ accum_precip, data = buffer)
mod12.buffer
summary(mod12.buffer) #R² = 0.006853 p 0.652
plot (burned_area ~ accum_precip, data= buffer, pch=20, main= 'Buffer',
      xlab='Precipitação acumulada em 12 meses (mm)', ylab= 'Área queimada')
#abline (a=1816.1653, b= -0.3499, col='tomato1')
ba= as.matrix (buffer$burned_area)
ap= as.matrix (buffer$accum_precip)
cor (ba, ap)

# Dias de chuva (12 meses)
#EEcl
mod12.eecl <- lm (burned_area ~ dias_chuva, data = eecl)
mod12.eecl
summary(mod12.eecl) #R² = 0.04868, p 0.225
plot (burned_area ~ dias_chuva, data= eecl, pch=20, main= 'EEcl',
      xlab='Dias de chuva', ylab= 'Área queimada')
#abline (a=-79.225, b= 1.565, col='tomato1')
ba= as.matrix (eecl$burned_area)
dc= as.matrix (eecl$dias_chuva)
cor (ba, dc)


#EEx
mod12.eex <- lm (burned_area ~ dias_chuva, data = eex)
mod12.eex
summary(mod12.eex) #R² = 0.01575, p 0.4937
plot (burned_area ~ dias_chuva, data= eex, pch=20, main= 'EEx',
      xlab='Dias de chuva', ylab= 'Área queimada')
#abline (a=24.170, b= -0.143, col='tomato1')
ba= as.matrix (eex$burned_area)
dc= as.matrix (eex$dias_chuva)
cor (ba, dc)

#Buffer
mod12.buffer <- lm (burned_area ~ dias_chuva, data = buffer)
mod12.buffer
summary(mod12.buffer) #R² = 0.03983, p 0.2734
plot (burned_area ~ dias_chuva, data= buffer, pch=20, main= 'Buffer',
      xlab='Dias de chuva', ylab= 'Área queimada')
#abline (a=2482.72, b= -10.87, col='tomato1')
ba= as.matrix (buffer$burned_area)
dc= as.matrix (buffer$dias_chuva)
cor (ba, dc)

# 24 month accum precip model
#EEcl
mod24.eecl <- lm (burned_area ~ accum_precip_24, data = eecl)
mod24.eecl
summary(mod24.eecl) #R² = 0.03511, p 0.3044
plot (burned_area ~ accum_precip_24, data= eecl, pch=20, main= 'EEcl',
      xlab='Precipitação acumulada em 24 meses (mm)', ylab= 'Área queimada')
#abline (a=-114.79140, b= 0.07083, col='tomato1')
ba= as.matrix (eecl$burned_area)
ap= as.matrix (eecl$accum_precip_24)
cor (ba, ap)

#EEx
mod24.eex <- lm (burned_area ~ accum_precip_24, data = eex)
mod24.eex
summary(mod24.eex) #R² = 0.0005968, p 0.8944
plot (burned_area ~ accum_precip_24, data= eex, pch=20, main= 'EEx',
      xlab='Precipitação acumulada em 24 meses (mm)', ylab= 'Área queimada')
#abline (a=12.996087, b= -0,001483, col='tomato1')
ba= as.matrix (eex$burned_area)
ap= as.matrix (eex$accum_precip_24)
cor (ba, ap)

#Buffer
mod24.buffer <- lm (burned_area ~ accum_precip_24, data = buffer)
mod24.buffer
summary(mod24.buffer) #R² = 0.0273, p 0.3661
plot (burned_area ~ accum_precip, data= buffer, pch=20, main= 'Buffer',
      xlab='Precipitação acumulada em 24 meses (mm)', ylab= 'Área queimada')
#abline (a=2693.8080, b= -0.4794, col='tomato1')
ba= as.matrix (buffer$burned_area)
ap= as.matrix (buffer$accum_precip_24)
cor (ba, ap)


# Dias de chuva (24 meses)
#EEcl
mod24.eecl <- lm (burned_area ~ dias_chuva_24, data = eecl)
mod24.eecl
summary(mod24.eecl) #R² = 0.01275, p 0.5383
plot (burned_area ~ dias_chuva_24, data= eecl, pch=20, main= 'EEcl',
      xlab='Dias de chuva (24 meses)', ylab= 'Área queimada')
#abline (a= -23.7782, b= 0.5258, col='tomato1')
ba= as.matrix (eecl$burned_area)
dc= as.matrix (eecl$dias_chuva_24)
cor (ba, dc)

#EEx
mod24.eex <- lm (burned_area ~ dias_chuva_24, data = eex)
mod24.eex
summary(mod24.eex) #R² = 0.01104, p 0.5671
plot (burned_area ~ dias_chuva_24, data= eex, pch=20, main= 'EEx',
      xlab='Dias de chuva (24 meses)', ylab= 'Área queimada')
#abline (a= -8.30326, b= 0.07857, col='tomato1')
ba= as.matrix (eex$burned_area)
dc= as.matrix (eex$dias_chuva_24)
cor (ba, dc)

#Buffer
mod24.buffer <- lm (burned_area ~ dias_chuva_24, data = buffer)
mod24.buffer
summary(mod24.buffer) #R² = 0.001783,  p 0.8185
plot (burned_area ~ dias_chuva_24, data= buffer, pch=20, main= 'Buffer',
      xlab='Dias de chuva (24 meses)', ylab= 'Área queimada')
#abline (a=1633.977, b= -1.509, col='tomato1')
ba= as.matrix (buffer$burned_area)
dc= as.matrix (buffer$dias_chuva_24)
cor (ba, dc)


############################################################################################
## Ignition and growing cicatrizes geral
data <- readOGR(dsn= "H:/results/LANDSAT mapas/data",layer='class')
summary(data$season)

## Separar grupos
eecl= subset (data, class=='Ecologica', drop=T)
eex = subset (data, class=='Experimental', drop=T)
buffer = subset (data, class=='Buffer', drop=T)

#Ignition e growing dos grupos
summary(eecl$season)
summary(eex$season)
summary(buffer$season)

#export
#library(xlsx)
#write.xlsx(eecl, file="season_eecl.xlsx", sheetName="sheet1")
#write.xlsx(eex, file="season_eex.xlsx", sheetName="sheet1")
#write.xlsx(buffer, file="season_buffer.xlsx", sheetName="sheet1")

## tabela com fire year calculado pelo excel
scars.season <- read.table ('scars_season.txt', header=TRUE)
scars.season

##separar classes para calcular área queimada por ano fogo
eecl= subset (scars.season, class=='Ecologica')
eex = subset (scars.season, class=='Experimental')
buffer = subset (scars.season, class=='Buffer')

#area queimada por ano fogo
## EEcl
facteur=factor(paste(eecl$fire_year,eecl$fire_year,sep="_"))
sum=tapply(eecl$area,facteur,sum)
eecl$fire_year= as.factor (eecl$fire_year)
fire_year=levels(eecl$fire_year)
resultats=data.frame(fire_year,Sum_Burned_Area=paste(round(sum,2)))
eecl.fyear <- resultats

## EEx
facteur=factor(paste(eex$fire_year,eex$fire_year,sep="_"))
sum=tapply(eex$area,facteur,sum)
eex$fire_year= as.factor (eex$fire_year)
fire_year=levels(eex$fire_year)
resultats=data.frame(fire_year,Sum_Burned_Area=paste(round(sum,2)))
eex.fyear <- resultats

## Buffer
facteur=factor(paste(buffer$fire_year,buffer$fire_year,sep="_"))
sum=tapply(buffer$area,facteur,sum)
buffer$fire_year= as.factor (buffer$fire_year)
fire_year=levels(buffer$fire_year)
resultats=data.frame(fire_year,Sum_Burned_Area=paste(round(sum,2)))
buffer.fyear <- resultats
buffer.fyear

#Frequencia por ano fogo
eecl.fyear.freq <- table (eecl$fire_year)
eex.fyear.freq <- table (eex$fire_year)
buffer.fyear.freq <- table (buffer$fire_year)

#Exporar dados 
library(xlsx)
#write.xlsx(eecl.fyear, file="eecl_fyear.xlsx", sheetName="sheet1")
#write.xlsx(eex.fyear, file="eex_fyear.xlsx", sheetName="sheet1")
#write.xlsx(buffer.fyear, file="buffer_fyear.xlsx", sheetName="sheet1")
#write.xlsx(eecl.fyear.freq, file="eecl_fyear_freq.xlsx", sheetName="sheet1")
#write.xlsx(eex.fyear.freq, file="eex_fyear_freq.xlsx", sheetName="sheet1")
#write.xlsx(buffer.fyear.freq, file="buffer_fyear_freq.xlsx", sheetName="sheet1")

#############################################################################################3

#Modelos com ano fogo 
fire_year <- read.table ('fire_year.txt', header=TRUE)
eecl= subset (fire_year, class=='Ecologica', drop=T)
eex= subset (fire_year, class=='Experimental', drop=T)
buffer= subset (fire_year, class=='Buffer', drop=T)

#12 months accumuled precipitation 
#EEcl
mod12f.eecl <- lm (burned_area ~ accum_precip, data = eecl)
mod12f.eecl
summary(mod12f.eecl) #R² = 0.02414, p 0.3958
plot (burned_area ~ accum_precip, data= eecl, pch=20, main= 'EEcl',
      xlab='Precipitação acumulada em 12 meses (mm)', ylab= 'Área queimada')
abline (a= -28.67096, b= 0.08165, col='tomato1')
var1= (eecl$burned_area)
var2= (eecl$accum_precip)
cor(var1, var2)

#EEx
mod12f.eex <- lm (burned_area ~ accum_precip, data = eex)
mod12f.eex
summary(mod12f.eex) #R² = 0.03644, p 0.2953
plot (burned_area ~ accum_precip, data= eex, pch=20, main= 'EEx',
      xlab='Precipitação acumulada em 12 meses (mm)', ylab= 'Área queimada')
abline (a= 30.2805, b= -0.0154, col='tomato1')
var1= (eex$burned_area)
var2= (eex$accum_precip)
cor(var1, var2)

#Buffer
mod12f.buffer <- lm (burned_area ~ accum_precip, data = buffer)
mod12f.buffer
summary(mod12f.buffer) #R² = 0.007039, p 0.648
plot (burned_area ~ accum_precip, data= buffer, pch=20, main= 'Buffer',
      xlab='Precipitação acumulada em 12 meses (mm)', ylab= 'Área queimada')
abline (a= 1787.1471, b= -0.3323, col='tomato1')
var1= (buffer$burned_area)
var2= (buffer$accum_precip)
cor(var1, var2)

#D############################################################################
# Dias de chuva (12 meses)
#EEcl
mod12f.eecl <- lm (burned_area ~ dias_chuva, data = eecl)
mod12f.eecl
summary(mod12f.eecl) #R² = 0.02015, p 0.4384
plot (burned_area ~ dias_chuva, data= eecl, pch=20, main= 'EEcl',
      xlab='Dias de chuva', ylab= 'Área queimada')
abline (a=-33.85, b= 1.14, col='tomato1')
var1= (eecl$burned_area)
var2= (eecl$dias_chuva)
cor(var1, var2)

#EEx
mod12f.eex <- lm (burned_area ~ dias_chuva, data = eex)
mod12f.eex
summary(mod12f.eex) #R² = 0.001518, p 0.8323
plot (burned_area ~ dias_chuva, data= eex, pch=20, main= 'EEx',
      xlab='Dias de chuva)', ylab= 'Área queimada')
abline (a=13.11365, b= -0.04802, col='tomato1')
var1= (eex$burned_area)
var2= (eex$dias_chuva)
cor(var1, var2)

#Buffer
mod12f.buffer <- lm (burned_area ~ dias_chuva, data = buffer)
mod12f.buffer
summary(mod12f.buffer) #R² = 0.01743, p 0.4713
plot (burned_area ~ dias_chuva, data= buffer, pch=20, main= 'Buffer',
      xlab='Dias de chuva', ylab= 'Área queimada')
abline (a=2172.732, b= -7.992, col='tomato1')
var1= (buffer$burned_area)
var2= (buffer$dias_chuva)
cor(var1, var2)

#####################################################################################

## only IGNITION precip
#EEcl
mod24f.eecl <- lm (burned_area ~ ignition, data = eecl)
mod24f.eecl
summary(mod24f.eecl) #R² = 0.003396, p 0.7514
plot (burned_area ~ ignition, data= eecl, pch=20, main= 'EEcl',
      xlab='Precipitação do período de ignição', ylab= 'Área queimada')
abline (a=67.68703, b= 0.07387, col='tomato1')
var1= (eecl$burned_area)
var2= (eecl$ignition)
cor(var1, var2)

#EEx
mod24f.eex <- lm (burned_area ~ ignition, data = eex)
mod24f.eex
summary(mod24f.eex) #R² = 0.02986, p 0.3443
plot (burned_area ~ ignition, data= eex, pch=20, main= 'EEx',
      xlab='Precipitação do período de ignição', ylab= 'Área queimada')
abline (a=18.07609, b= -0.03362, col='tomato1')
var1= (eex$burned_area)
var2= (eex$ignition)
cor(var1, var2)

#Buffer
mod24f.buffer <- lm (burned_area ~ ignition, data = buffer)
mod24f.buffer
summary(mod24f.buffer) #R² = 0.00451, p 0.715
plot (burned_area ~ accum_precip, data= buffer, pch=20, main= 'Buffer',
      xlab='Precipitação do período de ignição', ylab= 'Área queimada')
abline (a= 1498.2562, b= -0.6417, col='tomato1')
var1= (buffer$burned_area)
var2= (buffer$ignition)
cor(var1, var2)

# only GROWING precip
#EEcl
mod24f.eecl <- lm (burned_area ~ growing, data = eecl)
mod24f.eecl
summary(mod24f.eecl) #R² = 0.02159, p 0.4223
plot (burned_area ~ growing, data= eecl, pch=20, main= 'EEcl',
      xlab='Precipitação do período de crescimento', ylab= 'Área queimada')
abline (a= -9.45748, b= 0.08645, col='tomato1')
var1= (eecl$burned_area)
var2= (eecl$growing)
cor(var1, var2)

#EEx
mod24f.eex <- lm (burned_area ~ growing, data = eex)
mod24f.eex
summary(mod24f.eex) #R² = 0.01783, p 0.4663
plot (burned_area ~ growing, data= eex, pch=20, main= 'EEx',
      xlab='Precipitação do período de crescimento', ylab= 'Área queimada')
abline (a=21.77450, b= -0.01206, col='tomato1')
var1= (eex$burned_area)
var2= (eex$growing)
cor(var1, var2)


#Buffer
mod24f.buffer <- lm (burned_area ~ growing, data = buffer)
mod24f.buffer
summary(mod24f.buffer) #R² = 0.00394
plot (burned_area ~ growing, data= buffer, pch=20, main= 'Buffer',
      xlab='Precipitação do período de crescimento', ylab= 'Área queimada')
abline (a= 1624.3609, b= -0.2784, col='tomato1')
var1= (buffer$burned_area)
var2= (buffer$growing)
cor(var1, var2)


#######################################################################################
# multiple regression - dias de chuva em ignition + growing

#EEcl
mod24f.eecl <- lm (burned_area ~ growing_days + ignition_days, data = eecl)
mod24f.eecl
summary(mod24f.eecl) #R² = 0.02542, p 0.6885

#EEx
mod24f.eex <- lm (burned_area ~ growing_days + ignition_days, data = eex)
mod24f.eex
summary(mod24f.eex) #R² = 0.03482, p 0.5981

#Buffer
mod24f.buffer <- lm (burned_area ~ growing_days + ignition_days, data = buffer)
mod24f.buffer
summary(mod24f.buffer) #R² = 0.01781, p 0.7706

###########################################################################################
#Check years with fire in march on EEcl
#Check if March fires are a standard of 'anexo'
list= subset (focus.anexo, month=='3', drop=T)
list #only 1 scar

#Check only administrative EEcl
list= subset (focus.eecl, month=='3', drop=T)
list #4 scars

#Junc with 'Anexo' + EEcl
list= subset (junc.eecl, month=='3', drop=T)
list #5 scars
plot(list, col='red') #blood fires muhaahahah

#read years
y.eecl.mar= list$year
y.eecl.mar

#make a spatial polygon dataframe with this years
eecl.1998 <- subset (junc.eecl, year=='1998', drop=T)
eecl.1999 <- subset (junc.eecl, year=='1999', drop=T)
eecl.2005 <- subset (junc.eecl, year=='2005', drop=T)
eecl.2011 <- subset (junc.eecl, year=='2011', drop=T)
eecl.2013 <- subset (junc.eecl, year=='2013', drop=T)

#look this
plot (eecl.1998) # OMG        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
plot (eecl.1999) # OH MY GOD  !!!!El niño forte 1997-1998      !!!!
plot (eecl.2005) # LOL        !!!!El niño moderado 2004-2005   !!!!
plot (eecl.2011) # Oo         !!!!El ninõ fraco 2010           !!!!
eecl.2013 # !!!        !!!!Sem el niño- foco no anexo   !!!!
#            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   FONTE: CPTEC/INPE

##############################################################################################
#Check EEI december fires
#read EEI
list= subset (focus.eex, month=='12', drop=T)
list #2 scars
plot(list, col='red') #Antropico =)

#read years
y.eex.dec= list$year
y.eex.dec

#make a spatial polygon dataframe with this years
eex.1988 <- subset (focus.eex, year=='1988', drop=T)
eex.1999 <- subset (focus.eex, year=='1999', drop=T)

#look this
plot (eex.1988)
plot (eex.1999)  #fogos antropicos de 'altas proporções'

#############################################################################################
library (ggplot2)

data= read.table ('general.txt', header=TRUE)
str(data)

x11()
eecl= subset (data, class=='Ecologica', drop=T)
eecl$month <- as.factor (eecl$month)
boxplot (percent_burned ~ month, data=eecl)
ggplot(eecl,aes(month,percent_burned)) + 
  geom_jitter(aes(size= Hectares), alpha=0.5) + 
  theme_bw() +
  xlab('Meses') +
  ylab('Área queimada (%)') +
  ggtitle('EEcl')

x11()
eex= subset (data, class=='Experimental', drop=T)
eex$month <- as.factor (eex$month)
boxplot (percent_burned ~ month, data=eex)
ggplot(eex,aes(month,percent_burned)) + 
  geom_jitter(aes(size= Hectares), alpha= 0.5) + 
  theme_bw() +
  xlab('Meses') +
  ylab ('Área queimada (%)') +
  ggtitle('EEI')

x11()
buffer= subset (data, class=='Buffer', drop=T)
buffer$month <- as.factor (buffer$month)
boxplot (percent_burned ~ month, data= buffer)
ggplot(buffer,aes(month,percent_burned)) + 
  geom_jitter(aes(size= Hectares), alpha= 0.5) + 
  theme_bw() +
  xlab ('Meses') +
  ylab ('Área queimada (%)') +
  ggtitle ('Buffer')


##########################################################################

data= read.table ('month.txt', header=TRUE)
cores= read.table ('colors.txt', header=FALSE)
cores= as.matrix (cores)

#area
x11()
ggplot(data=data, aes(x=Month, y=Sum_Percent, group = Class, colour = Class)) +
  geom_line() +
  geom_point( size=1.3, fill="black") +
  scale_x_continuous (breaks=1:12, labels= c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  theme_classic() +
  xlab('Meses') + ylab('Área queimada (%)')

#density
x11()
ggplot(data=data, aes(x=Month, y=Density, group = Class, colour=Class)) +
  geom_line() +
  geom_point( size=1.3) +
  scale_x_continuous (breaks=1:12, labels= c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  theme_classic() +
  xlab('Meses') + ylab('Densidade (ignição/hectare)')



#compare
data= read.table ('general.txt', header= TRUE)
head(data)
str(data)



data= read.table ('year.txt', header = TRUE)
str(data)
x11()
ggplot(data=data, aes(x=year, y=accum_percent, group = class, colour=class)) +
  geom_line() +
  xlab('Anos') + ylab('Área queimada (%)') + theme_bw() +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2016)) +
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100, 120)) 



data
v1

v1= subset (data, class=='Experimental', drop=T)
ggplot(data=v1, aes(x=year, y=accum_percent)) +
  geom_line(col='red') +
  xlab('Anos') + ylab('Área queimada (%)') + theme_bw() +
  scale_x_continuous(breaks=c(1985, 1990, 1995, 2000, 2005, 2010, 2016))


v1= subset (data, Class=='Experimental', drop=T)

ggplot(data=v1, aes(x=Month, y=Sum_Percent)) +
  geom_line(col='red') +
  geom_point( size=1.3, fill="black") +
  scale_x_continuous (breaks=1:12, labels= c('J','F','M','A','M','J','J','A','S','O','N','D')) +
  theme_classic() +
  xlab('Meses') + ylab('Área queimada (%)')
