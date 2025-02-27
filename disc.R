#preparing discards data to be included in model
#have some issues with species codes in discards / observer databases 

# Thu Jan  2 10:58:46 2025 ------------------------------

#load packages
library(here)
library(dplyr)
library(ggplot2)
library(tidyverse)

#load data
comdisc <- readRDS(here("comdiscRpath.rds"))
discards <- comdisc$comdisc 
#79944 of 142646 are NAs -- true NAs? Or 0s?

#load in species codes
load(here("Species_codes.RData")) #this is from Sean, has Rpath name assignments
obspp <- readRDS(here("observerSpecies.rds")) #additional species in observer database
otherspp <- readRDS(here("otherMissingSpecies.rds"))

#filter spp for columns needed, remove NAs
spp_codes <- spp %>%
  dplyr::select(NESPP3,RPATH) %>%
  dplyr::filter(is.na(NESPP3) == F) %>%
  dplyr::distinct()
  
#some NESPP3 codes are associated with multiple Rpath codes -- check those
dupes <- spp_codes %>% 
  dplyr::group_by(NESPP3) %>%
  dplyr::filter(n() > 1)

#all OtherDemersals and something else -- either SouthernDemersals or SmPelagics
#Going to keep all with OtherDemersals except 363 == Silversides
spp_codes <- spp_codes %>%
  dplyr::filter(!(NESPP3 == 363 & RPATH == "OtherDemersals")) %>%
  dplyr::filter(!(NESPP3 %in% dupes$NESPP3 & RPATH == "SouthernDemersals"))

#merge spp with discards
discards <- left_join(discards,spp_codes, by = "NESPP3")

#this works BUT the biggest values are for groups with NESPP3 codes that don't match existing Rpath codes
#35440 NAs in Rpath columns, 80 codes
nas <- discards %>%
  dplyr::filter(is.na(RPATH)) %>%
  distinct(NESPP3) 

#match 4 digit codes with observer database
obspp <- obspp %>%
  dplyr::rename(NESPP3 = NESPP4) %>%
  dplyr::select(NESPP3, COMNAME, SCINAME) %>%
  dplyr::mutate(NESPP3 = as.numeric(NESPP3))

nas1 <- left_join(nas,obspp, by = "NESPP3")

#that took care of 43 out of 80, now have 37 left
nas <- nas1 %>%
  dplyr::filter(is.na(COMNAME)) %>%
  dplyr::select(NESPP3)

nas1 <- nas1 %>%
  dplyr::filter(!is.na(COMNAME)) 

obspp_short <- obspp %>%
  dplyr::rename(NESPP4 = NESPP3) %>%
  dplyr::mutate(NESPP3 = NESPP4/10) 

nas2 <- left_join(nas,obspp_short,by="NESPP3")

nas2 <- nas2 %>%
  dplyr::select(-NESPP4)

nas <- rbind(nas1,nas2)


#and then the rest with other categories
otherspp <- otherspp %>%
  dplyr::mutate(NESPP4 = as.numeric(NESPP4)) %>%
  dplyr::mutate(NESPP3 = NESPP4/10) %>%
  dplyr::rename(COMNAME_other = COMNAME)

nas <- left_join(nas,otherspp, by = c("NESPP3"))

### Come back to NA groups but Max's script basically works
nas <- NAs_discards %>%
  dplyr::rename(RPATH = Group) %>%
  dplyr::select(NESPP3,RPATH)

#merge group names
discards <- left_join(discards,nas,by = "NESPP3")
discards <- discards %>% 
  dplyr::mutate(RPATH = coalesce(RPATH.x, RPATH.y)) %>% 
  dplyr::select(-RPATH.x, -RPATH.y)

#still have two groups unassigned, want to know how big the values are
#most of the rows have NAs for DISMT = 447597 / 708400 = 63%
discards_pos <- discards %>%
  dplyr::filter(!is.na(DISMT))

discards_pos <- discards_pos %>%
  group_by(EPU,RPATH,Fleet,YEAR) %>%
  summarise(sumDISMT = sum(DISMT)) %>%
  filter(!EPU %in% c("SS","Other"))

ggplot(data=discards_pos)+
  geom_smooth(aes(x=YEAR,y=sumDISMT,colour = EPU))+
  facet_wrap(~RPATH,scales="free")
  
#save output
write.csv(discards_pos,"discards_fleet_year.csv")

#test for GB
#filter 
comland <- readRDS(here("comlandRpath.rds"))
GB_land <- comland$comland %>% 
  dplyr::filter(EPU == "GB")
#79944 of 142646 are NAs -- true NAs? Or 0s?

#load in species codes
load(here("Species_codes.RData"))

#filter for columns needed, remove NAs
spp_codes <- spp %>%
  dplyr::select(NESPP3,RPATH) %>%
  dplyr::filter(is.na(NESPP3) == F) %>%
  dplyr::distinct()

#some NESPP3 codes are associated with multiple Rpath codes -- check those
dupes <- spp_codes %>% 
  dplyr::group_by(NESPP3) %>%
  dplyr::filter(n() > 1)

#all OtherDemersals and something else -- either SouthernDemersals or SmPelagics
#Going to keep all with OtherDemersals except 363 == Silversides
spp_codes <- spp_codes %>%
  dplyr::filter(!(NESPP3 == 363 & RPATH == "OtherDemersals")) %>%
  dplyr::filter(!(NESPP3 %in% dupes$NESPP3 & RPATH == "SouthernDemersals"))

#merge spp with discards
GB_land <- dplyr::left_join(GB_land,spp_codes, by = "NESPP3")

#843 are NAs -- much less than landings
nas_land <- GB_land %>%
  dplyr::filter(is.na(RPATH)) %>%
  distinct(NESPP3)
