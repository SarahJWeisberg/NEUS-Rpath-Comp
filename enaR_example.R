##example code for enaR uses with Rpath
##by: Sarah J. Weisberg

# Tue Oct 29 11:48:24 2024 ------------------------------


# Load packages -----------------------------------------------------------

#data organizing and wrangling
library(here)
library(dplyr)
library(tidyr)

#Rpath, enaR
library(remotes)
remotes::install_github('NOAA-EDAB/Rpath',force = T)
library(Rpath)
remotes::install_github("andybeet/enaR")
library(enaR)

#plotting
library(igraph)
library(ggplot2)
library(ggnetwork)
library(viridis)


# Load balanced Rpath model -----------------------------------------------
#GOM
load(url("https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/outputs/GOM_Rpath.RData?raw=true"))
load(url("https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/outputs/GOM_params_Rpath.RData?raw=true"))

# Convert Rpath model to network -----------------------------------------

#These steps should be converted to function so that Rpath can be smoothly integrated with enaR
#Inputs needed: balanced model & parameter files AND estimate of gross primary production for each producer group

#Set up model with group names and types
groups<-as.vector(GOM$Group)

#Count number of each group type
nliving <- nrow(GOM.params$model[Type <  2, ])
ndead   <- nrow(GOM.params$model[Type == 2, ])

#Find index of pp groups
pp<- which(groups == "Phytoplankton")

#Pull diet matrix
diet<-GOM$DC
#Get consumption values by DC*QB*Biomass
QQ<-matrix(nrow = (nliving + ndead + 1),ncol=nliving)
for (j in 1:nliving){
  QQ[,j]<-diet[,j]*GOM$QB[j]*GOM$Biomass[j]
}
#Ignore Imports
QQ<-QQ[1:(nliving+ndead),]
colnames(QQ)<-groups[1:nliving]
rownames(QQ)<-groups[1:(nliving+ndead)]
#Calculate flow to detritus
M0<-GOM$PB*(1-GOM$EE)
Detritus<-(M0*GOM$Biomass+GOM$QB*GOM$Biomass*GOM$Unassim)*GOM$DetFate[,1]
#Detritus<-GOM$QB*GOM$Biomass*GOM$Unassim
Detritus<-Detritus[1:(nliving+ndead)]
#Note that I am ignoring discards here
#Flow to detritus from detritus = 0
Detritus[(nliving+1)]<-0
#Bind diet matrix (QQ) with flow to detritus, discards
QQ<-cbind(QQ,Detritus)
#Calculate exports
#First sum catch
Catch<-rowSums(GOM$Landings)
#Add positive biomass accumulation terms
Export<-Catch+(ifelse(GOM$BA>0,GOM$BA,0))
Export<-Export[1:(nliving+ndead)]
for (i in 1:ndead){
  Export[nliving+i]<-GOM$PB[(nliving+i)]*GOM$Biomass[(nliving+i)]
}
#Calculate respiration
#Assume detritus, discards have 0 respiration
Resp<-((1-GOM$Unassim)*GOM$QB-GOM$PB)*GOM$Biomass
Resp<-ifelse(Resp>0,Resp,0)
Resp<-Resp[1:(nliving+ndead)]
Resp[(nliving+1):(nliving+ndead)]<-0
#Deal with Primary Production
#First, estimate GROSS production = Imports
#P/B in Ecopath model gives NET production
#Ratio of gross:net is going to be fixed based on EMAX
gross_net<-4101.9/3281.5
gross<-gross_net*GOM$PB[pp]*GOM$Biomass[pp]
Resp[1]<-gross-(GOM$PB[pp]*GOM$Biomass[pp])
#Calculate imports
#Negative biomass accumulation terms
#Gross primary production
Import<-abs(ifelse(GOM$BA<0,GOM$BA,0))
Import[pp]<-gross
Import<-Import[1:(nliving+ndead)]
#Trim biomass
Biomass<-GOM$Biomass[1:(nliving+ndead)]
#Pack the model directly and store
network_GOM<-enaR::pack(flow = QQ,
                             input = Import,
                             export = Export,
                             living = c(rep(TRUE,nliving),rep(FALSE,ndead)),
                             respiration = Resp,
                             storage = Biomass)


# ENA calculations --------------------------------------------------------

#calculate ascendency metrics from network model
info_GOM <- enaR::enaAscendency(network_GOM)

#calculate flow metrics from network model
flow_GOM <- enaR::enaFlow(network_GOM,balance.override=T)


# Network plot -----------------------------------------------------------

A<-enaR::enaStructure(network_GOM)$A
g<-igraph::graph_from_adjacency_matrix(A)

#get x and y coordinates
#derived from the plotfw function (https://rfrelat.github.io/BalticFoodWeb.html)
nylevel<-7 #determines number of levels along y-axis
n <- igraph::vcount(g) #number of vertices
tl<-GOM$TL[1:(nliving+ndead)] #pull trophic level calculations from rpath
bks <- c(0.9, seq(1.9, max(tl), length.out = nylevel))
ynod <- cut(tl, breaks = bks, include.lowest = TRUE, 
            labels = 1:(length(bks)-1)) #assign group to a y-level
maxx <- max(table(ynod)) #looks for max # of groups at any y-level
xnod <- rep(0,n)
for (i in 1:nylevel){
  l <- sum(ynod==i)
  
  ltr <- (l/maxx)**(1/2)*maxx
  if (l>1) {
    xnod[ynod==i] <- seq(-ltr,ltr,length.out = l)
  } else {
    xnod[ynod==i] <- 0
  }
}

coo <- cbind(xnod,tl) #these inform x and y coordinates

#move groups manually as needed
coo[2,1]<-(-10)
coo[31,1]<-7
coo[48,1]<-16
coo[3,1]<-0
coo[6,1]<-4
coo["Microzooplankton",1]<-6
coo["Megabenthos",1]<-12
coo["SmCopepods",1]<-10


#use ggnetwork to create a network geometry
#storage and flows from original model
n_GOM<-ggnetwork::ggnetwork(network_GOM,layout=coo,weights="flow")
#bind with TL info
TL<-as.data.frame(cbind(GOM$TL,groups)) %>% rename(vertex.names=groups,TL=V1)
TL<-TL %>% mutate(TL = as.numeric(TL)) %>% mutate(TL = round(TL,2))
n_GOM<-left_join(n_GOM,TL,by="vertex.names")

#adjust Detritus storage so it doesn't swamp everything
det_GOM<-max(GOM$Biomass)
n_GOM <- n_GOM %>% mutate(storage_adjust = ifelse(storage < 0.05,0.05,storage))
n_GOM <- n_GOM %>% mutate(storage_adjust = ifelse(storage == det_GOM,100,storage))

GOM_web<-ggplot(n_GOM,aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(7, "pt"), type = "open"),
             curvature = 0.15,position="jitter",
             aes(color=TL,linewidth=flow)) +
  scale_color_gradientn(colors = turbo(6))+
  scale_linewidth(range = c(0.15,9))+
  geom_nodelabel(aes(label=vertex.names,size=((storage_adjust))),show.legend = F) +
  scale_size(range=c(3,10))+
  guides(linewidth="none")+
  annotate("text",x=0.025,y=0.975,label="GOM",size=9)+
  theme_blank(legend.position="none")+
  theme_blank(legend.position=c(0.8,0.2))+
  theme(panel.background = element_rect(fill="#EEEEEEFF"),
        plot.background = element_rect(fill="#EEEEEEFF"),
        legend.background=element_rect(fill="#EEEEEEFF"))

GOM_web
