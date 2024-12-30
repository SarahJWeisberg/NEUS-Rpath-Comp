#Title: NAs_discards_assigned

# Purpose: This script assigns remaining NA groups to Rpath groups

# DataFiles: 'NAs_discards.csv', '
# Raw data (if needed): observerSpecies.rds', 
# 'otherMissingSpecies.rds'

# Author: M.T. Grezlik
# Date: 12-30-2024

# load packages ------------------------------------------------
library(tidyverse)

# load in the data --------------------------------------------
NAs_discards <- read_csv("NAs_discards.csv")
NAs_discards <- select(NAs_discards, NESPP3, COMNAME, SCINAME)

# Get list of Rpath Groups -------------------------------------
MAB <- read_csv("MAB_balanced.csv")
MAB_rpath <- select(MAB, Group)
GOM <- read_csv("GOM_balanced.csv")
GOM_rpath <- select(GOM, Group)
GB <- read_csv("GB_balanced.csv")
GB_rpath <- select(GB, Group)

Rpath_groups <- full_join(MAB_rpath, GOM_rpath, GB_rpath, by = "Group")

# Assign NAs to Rpath groups --------------------------------------------------
# Starting from the top
# leaving any that are unclear for the next section

## CATFISH (FRESHWATER) being added to OtherDemersals -------------------------
catfish <- NAs_discards |> filter (NESPP3 == 660) |>
                mutate(Group = "OtherDemersals")

NAs_discards <- left_join(NAs_discards, catfish, 
                          by = c("NESPP3",'COMNAME', 'SCINAME'))

## Pilot Whales, Dwarf Sperm Whales, and Sowerbys Beaked Whale ----------------
# added to Odontocetes
whales <- NAs_discards |> filter(NESPP3 >= 6900 & NESPP3 <= 6910) |>
                mutate(Group = "Odontocetes")

# Join whales to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          whales, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group, 
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Fin, Finback, Humpback Whales added to Baleen Whales -----------------------

bwhales <- NAs_discards |> filter(NESPP3 %in% c(6929,6931,6933)) |>
                mutate(Group = "BaleenWhales")

# Join bwhales to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          bwhales, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Dolphins added to odontocetes ----------------------------------------------
dolphins <- NAs_discards |> filter(NESPP3 >= 6936 & NESPP3 <= 6944)|>
                mutate(Group = "Odontocetes")
# Join dolphins to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          dolphins, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Minke, baleen and Right Whale added to baleen whales ------------------------------
mwhales <- NAs_discards |> filter(NESPP3 %in% c(6945,6946, 6993)) |>
                mutate(Group = "BaleenWhales")
# Join mwhales to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          mwhales, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Sperm, Killer, Beaked, Toothed Whales, Dolphins, Porpoises added to -------
# Odontocetes

odonts <- NAs_discards |> filter(NESPP3 >= 6948 & NESPP3 <= 6980) |> 
                mutate(Group = "Odontocetes")
# Join odonts to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          odonts, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Seals added to Pinnipeds -------------------------------------------------
seals <- NAs_discards |> filter(NESPP3 %in% c(6981, 6982, 6994:6996)) |>
                mutate(Group = "Pinnipeds")
# Join seals to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          seals, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Pilot whale and dolphin, NK added to odontocetes --------------------------
pilot <- NAs_discards |> filter(NESPP3 %in% c(6992, 6997)) |>
                mutate(Group = "Odontocetes")
# Join pilot to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          pilot, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Gadiform NK added to otherdemersals ----------------------------------------
gadiform <- NAs_discards |> filter(NESPP3 == 524) |>
                mutate(Group = "OtherDemersals")
# Join gadiform to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          gadiform, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Birds added to SeaBirds ---------------------------------------------------
birds <- NAs_discards |> filter(NESPP3 %in% c(610,615,620,621,
                                              633,640,643,652))|>
                mutate(Group = "SeaBirds")
# Join birds to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          birds, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Northern Sennet, FISH,PELAGIC,NK added to SmPelagics ---------------------------------------
sennet <- NAs_discards |> filter(NESPP3 %in% c(665,525)) |>
                mutate(Group = "SmPelagics")
# Join sennet to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          sennet, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Redeye Gaper and Louvar added to SouthernDemersals -------------------------
gaper <- NAs_discards |> filter(NESPP3 %in% c(666,676)) |>
                mutate(Group = "SouthernDemersals")
# Join gaper to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          gaper, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Roughtail stingray, cownose added to OtherSkates ---------------------------
stingray <- NAs_discards |> filter(NESPP3 %in% c(671, 674)) |>
                mutate(Group = "OtherSkates")
# Join stingray to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          stingray, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Jack, NK added to OtherPelagics --------------------------------------------
jack <- NAs_discards |> filter(NESPP3 == 678) |>
                mutate(Group = "OtherPelagics")
# Join jack to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          jack, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Remora and Wrymouth added to OtherDemersals -------------------------------
remora <- NAs_discards |> filter(NESPP3 %in% c(675, 679)) |>
                mutate(Group = "OtherDemersals")
# Join remora to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          remora, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Moon Snail, Flat Worm, Whelks added to Macrobenthos -----------------------
snail <- NAs_discards |> filter(NESPP3 %in% c(687, 689, 772,773)) |>
                mutate(Group = "Macrobenthos")
# Join snail to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          snail, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)


# Less obvious groups --------------------------------------------------------
# May warrant further discussion

## MAMMAL,MARINE, NK added to Pinnipeds -------------------------------------
# Justification: If it was a whale they would put it in that catch all
pins <- NAs_discards |> filter(NESPP3 == 6991) |>
                mutate(Group = "Pinnipeds")
# Join pins to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          pins, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Whale, NK added to Odontocetes ------------------------------------------
# Justification: There are more species of Odontocetes than Baleen Whales,
# so more likely that there was difficulty determining between species
more_whale <- NAs_discards |> filter(NESPP3 == 6999) |>
                mutate(Group = "Odontocetes")
# Join more_whale to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          more_whale, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Turtles added to HMS ---------------------------------------------------
# Justification: Process of elimination. They don't fit anywhere else

turtles <- NAs_discards |> filter(NESPP3 >= 8090 & NESPP3 <= 8161) |>
                mutate(Group = "HMS")
# Join turtles to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          turtles, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## VERTEBRATE,NK added to SmPelagics --------------------------------------
# Justification: Seemed like the group that would be most commonly 
# unable to identify
vertebrate <- NAs_discards |> filter(NESPP3 == 527) |>
                mutate(Group = "SmPelagics")
# Join vertebrate to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          vertebrate, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)

## Stomach Contents, NK added to Macrobenthos -----------------------------
# Justification: Seemed most likely in fisheries data
stomach <- NAs_discards |> filter(NESPP3 == 685) |>
                mutate(Group = "Macrobenthos")
# Join stomach to NAs_discards
NAs_discards <- left_join(NAs_discards, 
                          stomach, by = c("NESPP3",'COMNAME', 'SCINAME'))
# Combine values from columns Group.x and Group.y into Group,
# remove Group.x and Group.y
NAs_discards <- NAs_discards |> mutate(Group = coalesce(Group.x, Group.y)) |> 
  select(-Group.x, -Group.y)


# Create csv file -----------------------------------------------------------
write_csv(NAs_discards, "NAs_discards_assigned.csv")
