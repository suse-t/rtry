#------------------
# required packages
# -----------------
library(openxlsx)
library(tidyverse)
library(reshape2)


#------------
# data import
#------------
import_TRY <- read.table("C:/Users/gabi9/Desktop/Paper_Dateien/6440/6440.txt", header =T, 
                         sep = "\t", dec = ".", fill = T, quote = "", na.strings = NA)
import_TRY <- read.table("C:/Users/Nutzer/Desktop/6440/6440.txt", header =T, 
                         sep = "\t", dec = ".", fill = T, quote = "", na.strings = NA)


str(import_TRY)
save(import_TRY, file = "C:/Users/Nutzer/Desktop/6440/TRY_6440_import.RData")


# 1) TOPIC data
add_data_1_TOPIC <- read.csv("C:/Users/Nutzer/Desktop/6440/additional data/1/Tautenhahn_TRY6440a_TOPIC_EM120_EM121_2019_06_30.csv")

# 2) additional data 2
data_2 <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/2/dataset_eUSA_TRY_2019_0701.xlsx")

# 3) additional data 3 (tundra trait team)
data_3 <- read.csv("C:/Users/Nutzer/Desktop/6440/additional data/3/TundraTraitTeam.csv")

# 4) additional data 4
data_4a <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/4/Eight Wet Grassland Experimental Communities.xlsx")
data_4b <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/4/Korean Forest Trees.xlsx", sheet = 1)
data_4c <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/4/Korean Forest Trees.xlsx", sheet = 2)

# 5) additional data 5
data_5 <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/5/Cameroon Tropical Trees.xlsx")

# 7) additional data 7
data_7a <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/7/Pibiri data for TRY - 190625.xlsx", sheet = 1)
data_7a_header <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/7/Pibiri data for TRY - 190625.xlsx", sheet = 2)
data_7b <- read.xlsx("C:/Users/Nutzer/Desktop/6440/additional data/7/Slot_Respiration.xlsx")


#-------------------------------
# standardization of file format
#-------------------------------
# desired columns + types 
#------------------------
# SpeciesName: Factor
# AccSpeciesName: Factor (initially fill with NA)
# ObservationID: int / char
# TraitName: Character
# DataName: 
# OriglName
# OrigValueStr
# OrigUnitStr
# ValueKindName
# StdValue
# UnitName
# Reference
# Comment

trait_fomat <- import_TRY %>%
  select(TraitName, UnitName) %>%
  distinct(TraitName, UnitName) %>%
  arrange(TraitName)

DataNames <- import_TRY %>%
  distinct(DataName) %>%
  arrange(DataName)

import_TRY %>%
  filter(DataName == "Canopy position: sun vers. Shade leaf qualifier, light exposure") %>%
  distinct(DataName, OrigValueStr) %>%
  arrange(DataName) %>%
  View()

import_TRY %>%
  filter(DataID == 442|
           DataID == 2646) %>%
  distinct(DataID, DataName, OrigValueStr) %>%
  View("Leaf ID")

import_TRY %>%
  filter(DataID == 441) %>%
  distinct(DataID, DataName, OrigValueStr) %>%
  View("Plant ID")


#--------------
# 1) TOPIC data
#--------------
str(add_data_1_TOPIC)

# create identifier for ObservationID
add_data_1_TOPIC_std <- add_data_1_TOPIC %>%
  mutate(ObservationID = paste(Topic_Dataset_Number, TOPICDataset_ID, sep = "_")) %>%
  select(ObservationID, Sample_number:Leaf_Number, StudyReference)

# create subset with trait data
TOPIC_trait_data <- add_data_1_TOPIC_std %>%
  select(ObservationID, Sp_FullName:DataUnit, StudyReference) %>%
  rename(OriglName = "DataName",
         OrigValueStr = "DataValue", 
         OrigUnitStr = "DataUnit")

TOPIC_trait_data$DataName <- NA
TOPIC_trait_data$TraitName <- NA
TOPIC_trait_data$ValueKindName <- NA
TOPIC_trait_data$StdValue <- NA
TOPIC_trait_data$UnitName <- NA

TOPIC_trait_data %>%
  distinct(OriglName, OrigUnitStr)
      
      #              OriglName OrigUnitStr
      # 1       Leaf N content     percent    -> multiply value by 10 (1 % = 1g/100g = 1000mg/100g = 10mg)
      # 2       Leaf C content     percent
      # 3       Leaf P content     percent
      # 4 Leaf_Area_Individual         mm2    -> this is correct
      # 5      Total_Leaf_area         mm2    -> unit correct, but is this a sum?
      # 6    Total_Leaf_weight           g    -> leaf dry mass OR leaf fresh mass?
      # 7   Specific leaf area       mm2/g    -> divide value by 1000 (1 mm2/g = 1 mm2/ 1000mg)

# standardize trait data
LNC <- TOPIC_trait_data %>%
  filter(OriglName == "Leaf N content") %>%
  mutate(TraitName = "Leaf nitrogen (N) content per leaf dry mass",
         DataName = "Leaf nitrogen (N) content per leaf dry mass",
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

LCC <- TOPIC_trait_data %>%
  filter(OriglName == "Leaf C content") %>%
  mutate(TraitName = "Leaf carbon (C) content per leaf dry mass",
         DataName = "Leaf carbon (C) content per leaf dry mass",
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")  

LPC <- TOPIC_trait_data %>%
  filter(OriglName == "Leaf P content") %>%
  mutate(TraitName = "Leaf phosphorus (P) content per leaf dry mass",
         DataName = "Leaf phosphorus (P) content per leaf dry mass",
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g") 

LA <- TOPIC_trait_data %>%
  filter(OriglName == "Leaf_Area_Individual") %>%
  mutate(TraitName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         DataName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "mm2") 

L_mass <- TOPIC_trait_data %>%
  filter(OriglName == "Total_Leaf_weight") # no changes here since it is unclear if dry or fresh mass

LA_total <- TOPIC_trait_data %>%
  filter(OriglName == "Total_Leaf_area") %>%
  mutate(TraitName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         DataName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         ValueKindName = "Sum",
         StdValue = OrigValueStr,
         UnitName = "mm2") 

SLA <- TOPIC_trait_data %>%
  filter(OriglName == "Specific leaf area") %>%
  mutate(TraitName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         ValueKindName = "Single",
         StdValue = OrigValueStr / 1000,
         UnitName = "mm2 mg-1") 

TOPIC_trait_data_std <- bind_rows(LNC, LCC, LPC, LA, LA_total, L_mass, SLA)
TOPIC_trait_data_std$OrigValueStr <- as.character(TOPIC_trait_data_std$OrigValueStr)
rm(LNC, LCC, LPC, LA, LA_total, L_mass, SLA, TOPIC_trait_data)




# create subset with georeferences
TOPIC_georef <- add_data_1_TOPIC_std %>%
  select(ObservationID, Sp_FullName, StudyReference, Latitude, Longitude)
TOPIC_georef_long <- melt(TOPIC_georef, id.vars = c("ObservationID", "StudyReference", "Sp_FullName"))
TOPIC_georef_long <- TOPIC_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value")


TOPIC_georef_Lat <- TOPIC_georef_long %>%
  filter(OriglName == "Latitude") %>%
  mutate(hour = as.integer(str_sub(word(OrigValueStr, 1), end = -2)),
         minute = as.integer(str_sub(word(OrigValueStr, 3), end = -2))) %>% # blank space is counted as a word (for whatever reason) 
  mutate(DataName = OriglName, 
         StdValue = hour + minute/60) %>%
  select(ObservationID:OrigValueStr, DataName, StdValue)

TOPIC_georef_Lon <- TOPIC_georef_long %>%
    filter(OriglName == "Longitude") %>%
mutate(hour = as.integer(str_sub(word(OrigValueStr, 1), end = -2)),
       minute = as.integer(str_sub(word(OrigValueStr, 2), end = -2))) %>% # blank space is NOT counted as a word (for whatever reason) 
  mutate(DataName = OriglName, 
         StdValue = hour + minute/60) %>%
  select(ObservationID:OrigValueStr, DataName, StdValue)

# combine subsets
TOPIC_georef_std <- bind_rows(TOPIC_georef_Lat, TOPIC_georef_Lon)
TOPIC_georef_std$OrigValueStr <- as.character(TOPIC_georef_std$OrigValueStr)
rm(TOPIC_georef, TOPIC_georef_long, TOPIC_georef_Lat,TOPIC_georef_Lon)


# create subset with remaining data
add_data_1_TOPIC_std <- add_data_1_TOPIC_std %>%
  select(ObservationID:Sampling_Date, Sp_FullName, Site:StudyReference) 

# transform wide format to long format
add_data_1_TOPIC_std <- melt(add_data_1_TOPIC_std, id.vars = c("ObservationID", "Sp_FullName", "StudyReference"))
add_data_1_TOPIC_std <- add_data_1_TOPIC_std %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  arrange(ObservationID) %>%
  mutate_all(as.character)



# combine subsets
TOPIC_std <- full_join(add_data_1_TOPIC_std, TOPIC_trait_data_std,
                       by = c("ObservationID", "Sp_FullName", "StudyReference", "OriglName", "OrigValueStr"))
TOPIC_std <- full_join(TOPIC_std, TOPIC_georef_std,
                       by = c("ObservationID", "Sp_FullName", "StudyReference", "OriglName", "OrigValueStr", "DataName", "StdValue"))

TOPIC_std <- TOPIC_std %>%
  mutate(LastName = "Aubin",
         FirstName = "Isabelle",
         Dataset = "TOPIC_additional") %>%
  select(ObservationID, LastName:Dataset, Sp_FullName, OriglName:UnitName, StudyReference) %>%
  rename(SpeciesName = "Sp_FullName") %>%
  arrange(ObservationID)

# add DataName for maturity, IndividualID, LeafID and treatment
TOPIC_std$DataName[TOPIC_std$OriglName == "Leaf_Number"] <- "Leaf ID"
TOPIC_std$DataName[TOPIC_std$OriglName == "Sample_number"] <- "Plant ID / Individual ID"

rm(TOPIC_georef_std, TOPIC_trait_data_std, add_data_1_TOPIC, add_data_1_TOPIC_std)

save(TOPIC_std, file = "C:/Users/Nutzer/Desktop/6440/TOPIC_std.RData")

#---------------------
# 2) additional data 2
#---------------------

str(data_2)

# create unique identifier for each row
data_2$ObservationID <- paste(2, row.names(data_2), sep = "_")
data_2$SpeciesName <- paste(data_2$GENUS, data_2$SPECIES, sep = " ")

# select trait data
data_2_traits <- data_2 %>%
  select(ObservationID, DBH1:BASAL_AREA, LMA:d13Cpermil)

data_2_traits_long <- melt(data_2_traits, id.vars = "ObservationID") 
data_2_traits_long <- data_2_traits_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA)

DBH1 <- data_2_traits_long %>%
  filter(OriglName == "DBH1") %>%
  mutate(OrigUnitStr = "cm",
         DataName = "Stem diameter",
         TraitName = "Stem diameter",
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

DBH2 <- data_2_traits_long %>%
  filter(OriglName == "DBH2") %>%
  mutate(OrigUnitStr = "cm",
         DataName = "Stem diameter",
         TraitName = "Stem diameter",
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

DBH3 <- data_2_traits_long %>%
  filter(OriglName == "DBH3") %>%
  mutate(OrigUnitStr = "cm",
         DataName = "Stem diameter",
         TraitName = "Stem diameter",
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

BASAL_AREA <- data_2_traits_long %>%      # no standardization since trait doesn't exist in our TRY-dataset
  filter(OriglName == "BASAL_AREA") %>%
  mutate(OrigUnitStr = "cm2")

LMA <- data_2_traits_long %>%
  filter(OriglName == "LMA") %>%
  mutate(OrigUnitStr = "g/m2",     # 1 / (g/m2) to get inverse (LMA -> SLA), then multiply by 1000 (m2/g -> mm2/mg)
         DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         TraitName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         ValueKindName = "Single",
         StdValue = 1 / OrigValueStr * 1000,
         UnitName = "mm2 mg-1")

Ppercent <- data_2_traits_long %>%
  filter(OriglName == "Ppercent") %>%
  mutate(OrigUnitStr = "percent",     
         DataName = "Leaf phosphorus (P) content per leaf dry mass",
         TraitName = "Leaf phosphorus (P) content per leaf dry mass",
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

Parea <- data_2_traits_long %>%
  filter(OriglName == "Parea") %>%
  mutate(OrigUnitStr = "g/m2",     
         DataName = "Leaf phosphorus (P) content per leaf area",
         TraitName = "Leaf phosphorus (P) content per leaf area",
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g m-2")

Npercent <- data_2_traits_long %>%
  filter(OriglName == "Npercent") %>%
  mutate(OrigUnitStr = "percent",     
         DataName = "Leaf nitrogen (N) content per leaf dry mass",
         TraitName = "Leaf nitrogen (N) content per leaf dry mass",
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

Narea <- data_2_traits_long %>%
  filter(OriglName == "Narea") %>%
  mutate(OrigUnitStr = "g/m2",     
         DataName = "Leaf nitrogen (N) content per leaf area",
         TraitName = "Leaf nitrogen (N) content per leaf area",
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g m-2")

Cpercent <- data_2_traits_long %>%
  filter(OriglName == "Cpercent") %>%
  mutate(OrigUnitStr = "percent",     
         DataName = "Leaf carbon (C) content per leaf dry mass",
         TraitName = "Leaf carbon (C) content per leaf dry mass",
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

Carea <- data_2_traits_long %>%
  filter(OriglName == "Carea") %>%
  mutate(OrigUnitStr = "g/m2",     
         DataName = "Leaf carbon (C) content per leaf area",
         TraitName = "Leaf carbon (C) content per leaf area",
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g m-2")

d15Npermil <- data_2_traits_long %>%
  filter(OriglName == "d15Npermil") %>%
  mutate(OrigUnitStr = "per mill",     
         DataName = "Leaf nitrogen (N) isotope signature (delta 15N)",
         TraitName = "Leaf nitrogen (N) isotope signature (delta 15N)",
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "per mill")

d13Cpermil <- data_2_traits_long %>%
  filter(OriglName == "d13Cpermil") %>%
  mutate(OrigUnitStr = "per mill",     
         DataName = "Leaf carbon (C) isotope signature (delta 13C)",
         TraitName = "LeLeaf carbon (C) isotope signature (delta 13C)",
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "per mill")

data_2_traits_std <- bind_rows(DBH1, DBH2, DBH3, BASAL_AREA, LMA, Ppercent, Parea, Npercent, Narea, Cpercent, Carea, d15Npermil, d13Cpermil)
data_2_traits_std$OrigValueStr <- as.character(data_2_traits_std$OrigValueStr)

rm(data_2_traits, data_2_traits_long, DBH1, DBH2, DBH3, BASAL_AREA, LMA, Ppercent, Parea, Npercent, Narea, Cpercent, Carea, d15Npermil, d13Cpermil)


# select georeferences
data_2_georef <- data_2 %>%
  select(ObservationID, LAT, LON)

data_2_georef_long <- melt(data_2_georef, id.vars = "ObservationID") 
data_2_georef_long <- data_2_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA)

LAT <- data_2_georef_long %>%
  filter(OriglName == "LAT") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- data_2_georef_long %>%
  filter(OriglName == "LON") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

data_2_georef_std <- bind_rows(LAT, LON)
data_2_georef_std$OrigValueStr <- as.character(data_2_georef_std$OrigValueStr)

rm(data_2_georef, data_2_georef_long, LAT, LON)


# create subset of ObservationID - SpeciesName combinations for assigning the correct species names to observations
data_2_obs_spec <- data_2 %>%
  distinct(ObservationID, SpeciesName) %>%
  mutate(LastName = "Lichstein", 
         FirstName = "Jeremy",
         Dataset = "eUSA_additional")


# select rest of data (additional information)
data_2_rest <- data_2 %>%
  select(ObservationID, COLL_LAST:SITE, LATLON_UNCERTAINTY_m:DATE_DBH, DATE_SAMPLE:LFSTATE)

data_2_rest_long <- melt(data_2_rest, id.vars = "ObservationID")
data_2_rest_long <- data_2_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA)

data_2_rest_long$DataName <- NA
data_2_rest_long$DataName[data_2_rest_long$OriglName == "LFSTATE"] <- "Health status of plants (vitality)"
data_2_rest_long$DataName[data_2_rest_long$OriglName == "LFLIGHT"] <- "Canopy position: sun vers. Shade leaf qualifier, light exposure"
data_2_rest_std <- data_2_rest_long

rm(data_2_rest, data_2_rest_long)

# combine all dataframes
names(data_2_traits_std)
names(data_2_georef_std)
names(data_2_rest_std)

data_2_std <- bind_rows(data_2_traits_std, data_2_georef_std, data_2_rest_std)
data_2_std <- left_join(data_2_std, data_2_obs_spec, by = "ObservationID")
data_2_std <- data_2_std %>%
  select(ObservationID, LastName:Dataset, SpeciesName, OriglName:UnitName) %>%
  arrange(ObservationID)

rm(data_2_georef_std, data_2_obs_spec, data_2_rest_std, data_2_traits_std)

save(data_2_std, file = "C:/Users/Nutzer/Desktop/6440/data_2_std.RData")


#---------------------
# 3) additional data 3
#---------------------
data_3 <- data_3[, -1]

# create unique identifier for each observation
data_3 <- data_3 %>%
  mutate(ObservationID = paste(3, IndividualID, sep = "_"))

# create subset with ObservationID & Species Names
data_3_obs_spec <- data_3 %>%
  select(ObservationID, OriginalName, AccSpeciesName) %>%
  distinct(ObservationID, .keep_all = T) %>%
  mutate(LastName = "Bjorkman",
         FirstName = "Anne",
         Dataset = "TundraTraitTeam")

# standardize traits
data_3_traits_long <- data_3 %>%
  select(ObservationID, ValueKindName, Trait, Value, Units, Comments) %>%
  rename(OriglName = "Trait",
         OrigValueStr = "Value",
         OrigUnitStr = "Units", 
         Comment = "Comments") %>%
  mutate(DataName = NA,
         TraitName = NA, 
         StdValue = NA, 
         UnitName = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment)

data_3_traits_long %>%
  distinct(OriglName, OrigUnitStr) %>%
  View()

LA <- data_3_traits_long %>%
  filter(OriglName == "Leaf area") %>%
  mutate(DataName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mm2")

SLA <- data_3_traits_long %>%
  filter(OriglName == "Leaf area per leaf dry mass (specific leaf area, SLA)") %>%
  mutate(DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mm2 mg-1")

LCC <- data_3_traits_long %>%
  filter(OriglName == "Leaf carbon (C) content per leaf dry mass") %>%
  mutate(DataName = "Leaf carbon (C) content per leaf dry mass",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mg/g")

d13C <- data_3_traits_long %>%
  filter(OriglName == "Leaf carbon (C) isotope discrimination (delta 13C)") %>%
  mutate(DataName = "Leaf carbon (C) isotope signature (delta 13C)",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "per mill")

CN <- data_3_traits_long %>%
  filter(OriglName == "Leaf carbon/nitrogen (C/N) ratio") %>%
  mutate(DataName = "Leaf carbon/nitrogen (C/N) ratio",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "g/cm3")              # ratio -> g/cm3 -> how's that possible?

LDM <- data_3_traits_long %>%
  filter(OriglName == "Leaf dry mass") %>%
  mutate(DataName = "Leaf dry mass (single leaf)",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mg")

LDMC <- data_3_traits_long %>%
  filter(OriglName == "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)") %>%
  mutate(DataName = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "g g-1")

LFM <- data_3_traits_long %>%
  filter(OriglName == "Leaf fresh mass") %>%
  mutate(DataName = "Leaf fresh mass",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "g")

LNC <- data_3_traits_long %>%
  filter(OriglName == "Leaf nitrogen (N) content per leaf dry mass") %>%
  mutate(DataName = "Leaf nitrogen (N) content per leaf dry mass",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mg/g")

d15N <- data_3_traits_long %>%
  filter(OriglName == "Leaf nitrogen (N) isotope signature (delta 15N)") %>%
  mutate(DataName = "Leaf nitrogen (N) isotope signature (delta 15N)",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "per mill")

NP <- data_3_traits_long %>%
  filter(OriglName == "Leaf nitrogen/phosphorus (N/P) ratio") %>%
  mutate(DataName = "Leaf nitrogen/phosphorus (N/P) ratio",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "g/cm3")

LPC <- data_3_traits_long %>%
  filter(OriglName == "Leaf phosphorus (P) content per leaf dry mass") %>%
  mutate(DataName = "Leaf phosphorus (P) content per leaf dry mass",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mg/g")

H_rep <- data_3_traits_long %>%
  filter(OriglName == "Plant height, reproductive") %>%
  mutate(DataName = "Plant height generative",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "m")

H_veg <- data_3_traits_long %>%
  filter(OriglName == "Plant height, vegetative") %>%
  mutate(DataName = "Plant height vegetative",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "m")

root_depth <- data_3_traits_long %>%
  filter(OriglName == "Rooting depth") %>%
  mutate(DataName = NA,
         TraitName = NA, 
         StdValue = NA,
         UnitName = NA)

SDM <- data_3_traits_long %>%
  filter(OriglName == "Seed dry mass") %>%
  mutate(DataName = "Seed dry mass",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "mg")

SD <- data_3_traits_long %>%
  filter(OriglName == "Stem diameter") %>%
  mutate(DataName = "Stem diameter",
         TraitName = DataName, 
         StdValue = OrigValueStr / 100,
         UnitName = "m")

SSD <- data_3_traits_long %>%
  filter(OriglName == "Stem dry mass per stem fresh volume (stem specific density, SSD)") %>%
  mutate(DataName = "Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)",
         TraitName = DataName, 
         StdValue = OrigValueStr,
         UnitName = "g/cm3")        # mg/mm3 -> g/cm3 (equivalent)

data_3_traits_std <- bind_rows(LA, SLA, LCC, d13C, CN, LDM, LDMC, LFM, LNC, d15N, NP, LPC, H_rep, H_veg, root_depth, SDM, SD, SSD)
data_3_traits_std$OrigValueStr <- as.character(data_3_traits_std$OrigValueStr)

rm(data_3_traits_long, LA, SLA, LCC, d13C, CN, LDM, LDMC, LFM, LNC, d15N, NP, LPC, H_rep, H_veg, root_depth, SDM, SD, SSD)


# standardize georeferences
data_3_georef <- data_3 %>%
  select(ObservationID, Latitude, Longitude, Elevation)

data_3_georef_long <- melt(data_3_georef, id.vars = "ObservationID") 
data_3_georef_long <- data_3_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

LAT <- data_3_georef_long %>%
  filter(OriglName == "Latitude") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- data_3_georef_long %>%
  filter(OriglName == "Longitude") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

Altitude <- data_3_georef_long %>%
  filter(OriglName == "Elevation") %>%
  mutate(DataName = "Altitude",
         StdValue = OrigValueStr)


data_3_georef_std <- bind_rows(LAT, LON, Altitude)
data_3_georef_std$OrigValueStr <- as.character(data_3_georef_std$OrigValueStr)

rm(data_3_georef, data_3_georef_long, LAT, LON, Altitude)


# standardize rest of data
data_3_rest <- data_3 %>%
  select(ObservationID, SiteName:DataContributor)    # ErrorRisk is left out - calculated separately in complete dataset

data_3_rest_long <- melt(data_3_rest, id.vars = "ObservationID")
data_3_rest_std <- data_3_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)

rm(data_3_rest, data_3_rest_long)


# combine all dataframes
names(data_3_traits_std)
names(data_3_georef_std)
names(data_3_rest_std)

data_3_std <- bind_rows(data_3_traits_std, data_3_georef_std, data_3_rest_std)
data_3_std <- left_join(data_3_std, data_3_obs_spec, by = "ObservationID")
data_3_std <- data_3_std %>%
  select(ObservationID, LastName:Dataset, OriginalName, AccSpeciesName, OriglName:Comment) %>%
  rename(SpeciesName = "OriginalName") %>%
  arrange(ObservationID)

rm(data_3_georef_std, data_3_obs_spec, data_3_rest_std, data_3_traits_std)

save(data_3_std, file = "C:/Users/Nutzer/Desktop/6440/data_3_std.RData")


#-----------------------
# 4a) additional data 4a
#-----------------------
summary(data_4a)

# create unique identifier for each observation
data_4a <- data_4a %>%
  mutate(ObservationID = paste("4a", rownames(data_4a), sep = "_"))

# create subset with ObservationID & Species Names
data_4a_obs_spec <- data_4a %>%
  select(ObservationID, Species) %>%
  distinct(ObservationID, .keep_all = T) %>%
  mutate(LastName = "Dolezal",
         FirstName = "Jiri",
         Dataset = "Eight Wet Grassland Experimental Communities") 

# standardize traits
data_4a_traits <- data_4a %>%
  select(ObservationID, Height:`leaf.N%`)

data_4a_traits_long <- melt(data_4a_traits, id.vars = "ObservationID")

data_4a_traits_long <- data_4a_traits_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA,
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr))

unique(data_4a_traits_long$OriglName)

Height <- data_4a_traits_long %>%
  filter(OriglName == "Height") %>%
  mutate(OrigUnitStr = "cm",
         DataName = "Plant height vegetative",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

LDMC <- data_4a_traits_long %>%
  filter(OriglName == "LDMC") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g g-1")

# StDMC  # what kind of trait is that? "stomata ..."?
SLA <- data_4a_traits_long %>%
  filter(OriglName == "SLA") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "mm2 mg-1")

d13C <- data_4a_traits_long %>%
  filter(OriglName == "d13C") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf carbon (C) isotope signature (delta 13C)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "per mill")

leaf.C <- data_4a_traits_long %>%
  filter(OriglName == "leaf.C%") %>%
  mutate(OrigUnitStr = "percent",
         DataName = "Leaf carbon (C) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

d15N <- data_4a_traits_long %>%
  filter(OriglName == "d15N") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf nitrogen (N) isotope signature (delta 15N)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "per mill")

leaf.N <-  data_4a_traits_long %>%
  filter(OriglName == "leaf.N%") %>%
  mutate(OrigUnitStr = "percent",
         DataName = "Leaf nitrogen (N) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

data_4a_traits_std <- bind_rows(Height, LDMC, SLA, d13C, leaf.C, d15N, leaf.N)
data_4a_traits_std$OrigValueStr <- as.character(data_4a_traits_std$OrigValueStr)

rm(data_4a_traits, data_4a_traits_long, Height, LDMC, SLA, d13C, leaf.C, d15N, leaf.N)


# standardize georeferences
data_4a_georef <- data_4a %>%
  select(ObservationID, Latitude, Longitude, Altitude)

data_4a_georef_long <- melt(data_4a_georef, id.vars = "ObservationID") 
data_4a_georef_long <- data_4a_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

LAT <- data_4a_georef_long %>%
  filter(OriglName == "Latitude") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- data_4a_georef_long %>%
  filter(OriglName == "Longitude") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

Altitude <- data_4a_georef_long %>%
  filter(OriglName == "Altitude") %>%
  mutate(DataName = "Altitude",
         StdValue = OrigValueStr)


data_4a_georef_std <- bind_rows(LAT, LON, Altitude)
data_4a_georef_std$OrigValueStr <- as.character(data_4a_georef_std$OrigValueStr)

rm(data_4a_georef, data_4a_georef_long, LAT, LON, Altitude)

# standardize rest of data
data_4a_rest <- data_4a %>%
  select(ObservationID, Sampling.date:Experimental.treatments)
data_4a_rest_long <- melt(data_4a_rest, id.vars = "ObservationID")

data_4a_rest_long <- data_4a_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)

data_4a_rest_long$DataName[data_4a_rest_long$OriglName == "Maturity"] <- "Plant developmental status / plant age / maturity / plant life stage"
data_4a_rest_long$DataName[data_4a_rest_long$OriglName == "Plot.ID"] <- "Plot ID"
data_4a_rest_long$DataName[data_4a_rest_long$OriglName == "Experimental.treatments"] <- "Experimental treatment"
data_4a_rest_long$Comment[data_4a_rest_long$OriglName == "Experimental.treatments"] <- "M = mown, F = fertilized, R = dominant species removal"

data_4a_rest_std <- data_4a_rest_long

rm(data_4a_rest, data_4a_rest_long)


# combine all dataframes
names(data_4a_traits_std)
names(data_4a_georef_std)
names(data_4a_rest_std)

data_4a_std <- bind_rows(data_4a_traits_std, data_4a_georef_std, data_4a_rest_std)
data_4a_std <- left_join(data_4a_std, data_4a_obs_spec, by = "ObservationID")
data_4a_std <- data_4a_std %>%
  select(ObservationID, LastName:Dataset, Species, OriglName:Comment) %>%
  rename(SpeciesName = "Species") %>%
  arrange(ObservationID)

rm(data_4a_traits_std, data_4a_georef_std, data_4a_rest_std, data_4a_obs_spec)

save(data_4a_std, file = "C:/Users/Nutzer/Desktop/6440/data_4a_std.RData")



#-----------------------
# 4b) additional data 4b
#-----------------------
summary(data_4b)

# create unique identifier for each observation
data_4b <- data_4b %>%
  mutate(ObservationID = paste("4b", rownames(data_4b), sep = "_"))

# create subset with ObservationID & Species Names
data_4b_obs_spec <- data_4b %>%
  select(ObservationID, Species) %>%
  distinct(ObservationID, .keep_all = T) %>%
  mutate(LastName = "Dolezal",
         FirstName = "Jiri",
         Dataset = "Korean Forest Trees")

# standardize traits
data_4b_traits <- data_4b %>%
  select(ObservationID, 'SLA.m2/kg', LDMC)

data_4b_traits_long <- melt(data_4b_traits, id.vars = "ObservationID")

data_4b_traits_long <- data_4b_traits_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA,
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr))

unique(data_4b_traits_long$OriglName)

SLA <- data_4b_traits_long %>%
  filter(OriglName == "SLA.m2/kg") %>%
  mutate(OrigUnitStr = "m2/kg",
         DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "mm2 mg-1")

LDMC <- data_4b_traits_long %>%
  filter(OriglName == "LDMC") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g g-1")

data_4b_traits_std <- bind_rows(SLA, LDMC)
data_4b_traits_std$OrigValueStr <- as.character(data_4b_traits_std$OrigValueStr)

rm(data_4b_traits, data_4b_traits_long, LDMC, SLA)


# standardize georeferences
data_4b_georef <- data_4b %>%
  select(ObservationID, Latitude, Longitude, Altitude)

data_4b_georef_long <- melt(data_4b_georef, id.vars = "ObservationID") 
data_4b_georef_long <- data_4b_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

LAT <- data_4b_georef_long %>%
  filter(OriglName == "Latitude") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- data_4b_georef_long %>%
  filter(OriglName == "Longitude") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

Altitude <- data_4b_georef_long %>%
  filter(OriglName == "Altitude") %>%
  mutate(DataName = "Altitude",
         StdValue = OrigValueStr)

data_4b_georef_std <- bind_rows(LAT, LON, Altitude)
data_4b_georef_std$OrigValueStr <- as.character(data_4b_georef_std$OrigValueStr)

rm(data_4b_georef, data_4b_georef_long, LAT, LON, Altitude)

# standardize rest of data
data_4b_rest <- data_4b %>%
  select(ObservationID, Mountain.site:Plot, Sampling.date:Plant.growth.form)
data_4b_rest_long <- melt(data_4b_rest, id.vars = "ObservationID")

data_4b_rest_long <- data_4b_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)

data_4b_rest_long$DataName[data_4b_rest_long$OriglName == "Maturity"] <- "Plant developmental status / plant age / maturity / plant life stage"
data_4b_rest_long$DataName[data_4b_rest_long$OriglName == "Plot"] <- "Plot ID"

data_4b_rest_std <- data_4b_rest_long

rm(data_4b_rest, data_4b_rest_long)

# combine all dataframes
names(data_4b_traits_std)
names(data_4b_georef_std)
names(data_4b_rest_std)

data_4b_std <- bind_rows(data_4b_traits_std, data_4b_georef_std, data_4b_rest_std)
data_4b_std <- left_join(data_4b_std, data_4b_obs_spec, by = "ObservationID")
data_4b_std <- data_4b_std %>%
  select(ObservationID, LastName:Dataset, Species, OriglName:Comment) %>%
  rename(SpeciesName = "Species") %>%
  arrange(ObservationID)

rm(data_4b_traits_std, data_4b_georef_std, data_4b_rest_std, data_4b_obs_spec)

save(data_4b_std, file = "C:/Users/Nutzer/Desktop/6440/data_4b_std.RData")


#-----------------------
# 4c) additional data 4c
#-----------------------

summary(data_4c)

# create unique identifier for each observation
data_4c <- data_4c %>%
  mutate(ObservationID = paste("4c", rownames(data_4c), sep = "_"))

# create subset with ObservationID & Species Names
data_4c_obs_spec <- data_4c %>%
  select(ObservationID, Species) %>%
  distinct(ObservationID, .keep_all = T) %>%
  mutate(LastName = "Dolezal",
         FirstName = "Jiri",
         Dataset = "Korean Forest Trees")

# standardize traits
data_4c_traits <- data_4c %>%
  select(ObservationID, d13C:'P.(%)')

data_4c_traits_long <- melt(data_4c_traits, id.vars = "ObservationID")

data_4c_traits_long <- data_4c_traits_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA,
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr))

unique(data_4c_traits_long$OriglName)

d13C <- data_4c_traits_long %>%
  filter(OriglName == "d13C") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf carbon (C) isotope signature (delta 13C)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "per mill")

LCC <- data_4c_traits_long %>%
  filter(OriglName == "C.(%)") %>%
  mutate(OrigUnitStr = "percent",
         DataName = "Leaf carbon (C) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

LNC <-  data_4c_traits_long %>%
  filter(OriglName == "N.(%)") %>%
  mutate(OrigUnitStr = "percent",
         DataName = "Leaf nitrogen (N) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

d15N <- data_4c_traits_long %>%
  filter(OriglName == "d15N") %>%
  mutate(OrigUnitStr = "",
         DataName = "Leaf nitrogen (N) isotope signature (delta 15N)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "per mill")

LPC <-  data_4c_traits_long %>%
  filter(OriglName == "P.(%)") %>%
  mutate(OrigUnitStr = "percent",
         DataName = "Leaf phosphorus (P) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")


data_4c_traits_std <- bind_rows(d13C, LCC, LNC, d15N, LPC)
data_4c_traits_std$OrigValueStr <- as.character(data_4c_traits_std$OrigValueStr)

rm(data_4c_traits, data_4c_traits_long, d13C, LCC, LNC, d15N, LPC)

# standardize georeferences
data_4c_georef <- data_4c %>%
  select(ObservationID, Latitude, Longitude, Altitude)

data_4c_georef_long <- melt(data_4c_georef, id.vars = "ObservationID") 
data_4c_georef_long <- data_4c_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

LAT <- data_4c_georef_long %>%
  filter(OriglName == "Latitude") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- data_4c_georef_long %>%
  filter(OriglName == "Longitude") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

Altitude <- data_4c_georef_long %>%
  filter(OriglName == "Altitude") %>%
  mutate(DataName = "Altitude",
         StdValue = OrigValueStr)


data_4c_georef_std <- bind_rows(LAT, LON, Altitude)
data_4c_georef_std$OrigValueStr <- as.character(data_4c_georef_std$OrigValueStr)

rm(data_4c_georef, data_4c_georef_long, LAT, LON, Altitude)


# standardize rest of data
data_4c_rest <- data_4c %>%
  select(ObservationID, Mountain.site:Code, Sampling.date:Plant.growth.form)
data_4c_rest_long <- melt(data_4c_rest, id.vars = "ObservationID")

data_4c_rest_long <- data_4c_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)

data_4c_rest_long$DataName[data_4c_rest_long$OriglName == "Maturity"] <- "Plant developmental status / plant age / maturity / plant life stage"

data_4c_rest_std <- data_4c_rest_long

rm(data_4c_rest, data_4c_rest_long)

# combine all dataframes
names(data_4c_traits_std)
names(data_4c_georef_std)
names(data_4c_rest_std)

data_4c_std <- bind_rows(data_4c_traits_std, data_4c_georef_std, data_4c_rest_std)
data_4c_std <- left_join(data_4c_std, data_4c_obs_spec, by = "ObservationID")
data_4c_std <- data_4c_std %>%
  select(ObservationID, LastName:Dataset, Species, OriglName:Comment) %>%
  rename(SpeciesName = "Species") %>%
  arrange(ObservationID)

rm(data_4c_traits_std, data_4c_georef_std, data_4c_rest_std, data_4c_obs_spec)

save(data_4c_std, file = "C:/Users/Nutzer/Desktop/6440/data_4c_std.RData")


#---------------------
# 5) additional data 5
#---------------------
summary(data_5)

# create unique identifier for each observation
data_5 <- data_5 %>%
  mutate(ObservationID = paste("5", rownames(data_5), sep = "_"))

# create subset with ObservationID & Species Names
data_5_obs_spec <- data_5 %>%
  select(ObservationID, Species.name) %>%
  distinct(ObservationID, .keep_all = T) %>%
  mutate(LastName = "Dolezal",
         FirstName = "Jiri",
         Dataset = "Cameroon Tropical Trees")

# standardize traits
data_5_traits <- data_5 %>%
  select(ObservationID, DBH, Tree.height)  
    # other traits are not included (not in TRY-dataset included or not possible to standardize - e.g. missing unit of wood density)

data_5_traits_long <- melt(data_5_traits, id.vars = "ObservationID")

data_5_traits_long <- data_5_traits_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA,
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr))

unique(data_5_traits_long$OriglName)

DBH <- data_5_traits_long %>%
  filter(OriglName == "DBH") %>%
  mutate(OrigUnitStr = "cm",
         DataName = "Stem diameter",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

Height <- data_5_traits_long %>%
  filter(OriglName == "Tree.height") %>%
  mutate(OrigUnitStr = "m",
         DataName = "Plant height vegetative",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "m")

data_5_traits_std <- bind_rows(DBH, Height)
data_5_traits_std$OrigValueStr <- as.character(data_5_traits_std$OrigValueStr)

rm(data_5_traits, data_5_traits_long, Height, DBH)


# standardize georeferences
data_5_georef <- data_5 %>%
  select(ObservationID, Latitude, Longitude, Elevation)

data_5_georef_long <- melt(data_5_georef, id.vars = "ObservationID") 
data_5_georef_long <- data_5_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

LAT <- data_5_georef_long %>%
  filter(OriglName == "Latitude") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- data_5_georef_long %>%
  filter(OriglName == "Longitude") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

Altitude <- data_5_georef_long %>%
  filter(OriglName == "Elevation") %>%
  mutate(DataName = "Altitude",
         StdValue = OrigValueStr)

data_5_georef_std <- bind_rows(LAT, LON, Altitude)
data_5_georef_std$OrigValueStr <- as.character(data_5_georef_std$OrigValueStr)

rm(data_5_georef, data_5_georef_long, LAT, LON, Altitude)


# standardize rest of data
data_5_rest <- data_5 %>%
  select(ObservationID, Country:ForestType, Tree.no.:Family)

data_5_rest_long <- melt(data_5_rest, id.vars = "ObservationID")

data_5_rest_long <- data_5_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)

data_5_rest_long$DataName[data_5_rest_long$OriglName == "Plot"] <- "Plot ID"
data_5_rest_long$DataName[data_5_rest_long$OriglName == "Tree.no."] <- "Plant ID / Individual ID"

data_5_rest_std <- data_5_rest_long

rm(data_5_rest, data_5_rest_long)

# combine all dataframes
names(data_5_traits_std)
names(data_5_georef_std)
names(data_5_rest_std)

data_5_std <- bind_rows(data_5_traits_std, data_5_georef_std, data_5_rest_std)
data_5_std <- left_join(data_5_std, data_5_obs_spec, by = "ObservationID")
data_5_std <- data_5_std %>%
  select(ObservationID, LastName:Dataset, Species.name, OriglName:Comment) %>%
  rename(SpeciesName = "Species.name") %>%
  arrange(ObservationID)

rm(data_5_traits_std, data_5_georef_std, data_5_rest_std, data_5_obs_spec)

save(data_5_std, file = "C:/Users/Nutzer/Desktop/6440/data_5_std.RData")



#---------------------
# 6) additional data 6         # for now not imported - problems in recalculating georeferences
#---------------------

#-----------------------
# 7a) additional data 7a
#-----------------------
summary(data_7a)

# create unique identifier for each observation
data_7a <- data_7a %>%
  mutate(ObservationID = paste("7a", rownames(data_7a), sep = "_"))

# create subset with ObservationID & Species Names
data_7a_obs_spec <- data_7a %>%
  select(ObservationID, Species, Citation) %>%
  rename(Reference = "Citation") %>%
  distinct(ObservationID, .keep_all = T)  %>%
  mutate(LastName = "van der Sande",
         FirstName = "Masha",
         Dataset = "Pibiri data for TRY - 190625")

# standardize traits
data_7a_traits <- data_7a %>%
  select(ObservationID, H, DBH, LA, LDM, LFM, SLA:Parea, Chl, Cmass)

data_7a_traits_long <- melt(data_7a_traits, id.vars = "ObservationID")
data_7a_traits_long <- left_join(data_7a_traits_long, data_7a_header, by = c("variable" = "OriglName"))

data_7a_traits_long <- data_7a_traits_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  mutate(DataName = NA,
         TraitName = NA,
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr),
         OrigValueStr != "?" &
           OrigValueStr != "Not measured") %>%
  mutate(OrigValueStr = as.numeric(OrigValueStr))


H  <- data_7a_traits_long %>%
  filter(OriglName == "H") %>%
  mutate(DataName = "Plant height vegetative",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "m")

DBH <- data_7a_traits_long %>%
  filter(OriglName == "DBH") %>%
  mutate(DataName = "Stem diameter",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

LA <- data_7a_traits_long %>%
  filter(OriglName == "LA") %>%
  mutate(DataName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 100,  # cm2 -> mm2
         UnitName = "mm2")

LDM <- data_7a_traits_long %>%
  filter(OriglName == "LDM") %>%
  mutate(DataName = "Leaf dry mass (single leaf)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 1000,
         UnitName = "mg")

LFM <- data_7a_traits_long %>%
  filter(OriglName == "LFM") %>%
  mutate(DataName = "Leaf fresh mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g")

SLA <- data_7a_traits_long %>%
  filter(OriglName == "SLA") %>%
  mutate(DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr / 10, # cm2/g -> mm2/mg (divide by 10) 
         UnitName = "mm2 mg-1")

LT <- data_7a_traits_long %>%
  filter(OriglName == "LT") %>%
  mutate(DataName = "Leaf thickness",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "mm")

LD <- data_7a_traits_long %>%
  filter(OriglName == "LD") %>%
  mutate(DataName = "Leaf density (leaf tissue density, leaf dry mass per leaf volume)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g/cm3")

LDMC <- data_7a_traits_long %>%
  filter(OriglName == "LDMC") %>%
  mutate(DataName = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g g-1")

Nmass <- data_7a_traits_long %>%
  filter(OriglName == "Nmass") %>%
  mutate(DataName = "Leaf nitrogen (N) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "")

Narea <- data_7a_traits_long %>%
  filter(OriglName == "Narea") %>%
  mutate(DataName = "Leaf nitrogen (N) content per leaf area",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g m-2")

Pmass <- data_7a_traits_long %>%
  filter(OriglName == "Pmass") %>%
  mutate(DataName = "Leaf phosphorus (P) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

Parea <- data_7a_traits_long %>%
  filter(OriglName == "Parea") %>%
  mutate(DataName = "Leaf phosphorus (P) content per leaf area",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr,
         UnitName = "g m-2")

Chl <- data_7a_traits_long %>%
  filter(OriglName == "Chl") %>%
  mutate(DataName = "Leaf chlorophyll content per leaf area",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr / 100,   # 1g/m2 = 1,000,000µg / 10,000cm2 = 100µg/cm2 = 1g/m2
         UnitName = "g m-2")

Cmass <- data_7a_traits_long %>%
  filter(OriglName == "Cmass") %>%
  mutate(DataName = "Leaf carbon (C) content per leaf dry mass",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = OrigValueStr * 10,
         UnitName = "mg/g")

data_7a_traits_std <- bind_rows(H, DBH, LA, LDM, LFM, SLA, LT, LD, LDMC, Nmass, Narea, Pmass, Parea, Chl, Cmass)
data_7a_traits_std$OrigValueStr <- as.character(data_7a_traits_std$OrigValueStr)

rm(data_7a_traits, data_7a_traits_long, H, DBH, LA, LDM, LFM, SLA, LT, LD, LDMC, Nmass, Narea, Pmass, Parea, Chl, Cmass)

# standardize georeferences
data_7a_georef <- data_7a %>%
  select(ObservationID, Latitude, Longitude, Altitude)

data_7a_georef_long <- melt(data_7a_georef, id.vars = "ObservationID") 
data_7a_georef_long <- data_7a_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

unique(data_7a_georef_long$OrigValueStr)
    # "5°13'N"  "58°38'W" "120"

data_7a_georef_long$StdValue[data_7a_georef_long$OriglName == "Latitude"] <- 5.2166667
data_7a_georef_long$DataName[data_7a_georef_long$OriglName == "Latitude"] <- "Latitude"
data_7a_georef_long$StdValue[data_7a_georef_long$OriglName == "Longitude"] <- -58.63333333333333
data_7a_georef_long$DataName[data_7a_georef_long$OriglName == "Longitude"] <- "Longitude"
data_7a_georef_long$StdValue[data_7a_georef_long$OrigValueStr == "120"] <- 120
data_7a_georef_long$DataName[data_7a_georef_long$OriglName == "Altitude"] <- "Altitude"

data_7a_georef_std <- data_7a_georef_long

rm(data_7a_georef, data_7a_georef_long)


# standardize rest of data
data_7a_rest <- data_7a %>%
  select(ObservationID, Data.owner:Country, Family, Tree.nr:Plant.growth.form)

data_7a_rest_long <- melt(data_7a_rest, id.vars = "ObservationID")

data_7a_rest_long <- data_7a_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)

unique(data_7a_rest_long$OriglName)

data_7a_rest_long$DataName[data_7a_rest_long$OriglName == "Maturity"] <- "Plant developmental status / plant age / maturity / plant life stage"
data_7a_rest_long$DataName[data_7a_rest_long$OriglName == "Tree.nr"] <- "Plant ID / Individual ID"
data_7a_rest_long$DataName[data_7a_rest_long$OriglName == "Leaf.nr"] <- "Leaf ID"

data_7a_rest_std <- data_7a_rest_long

rm(data_7a_rest, data_7a_rest_long)

# combine all dataframes
names(data_7a_traits_std)
names(data_7a_georef_std)
names(data_7a_rest_std)

data_7a_std <- bind_rows(data_7a_traits_std, data_7a_georef_std, data_7a_rest_std)
data_7a_std <- left_join(data_7a_std, data_7a_obs_spec, by = "ObservationID")
data_7a_std <- data_7a_std %>%
  select(ObservationID, LastName:Dataset, Species, OriglName:Comment, Reference) %>%
  rename(SpeciesName = "Species") %>%
  arrange(ObservationID)

rm(data_7a_traits_std, data_7a_georef_std, data_7a_rest_std, data_7a_obs_spec, data_7a_header)

save(data_7a_std, file = "C:/Users/Nutzer/Desktop/6440/data_7a_std.RData")


#-----------------------
# 7b) additional data 7b
#-----------------------
# create unique identifier for each observation
data_7b <- data_7b %>%
  mutate(ObservationID = paste("7b", rownames(data_7b), sep = "_"))

# create subset with ObservationID & Species Names
data_7b_obs_spec <- data_7b %>%
  select(ObservationID, Species) %>%       
  distinct(ObservationID, .keep_all = T) %>%
  mutate(LastName = "Slot",
         FirstName = "Martijn",
         Dataset = "Slot_Respiration")

# standardize traits
data_7b_traits <- data_7b %>%
  select(ObservationID, LMA)  
# other traits are not included (not in TRY-dataset included or not possible to standardize)

data_7b_traits_std <- data_7b_traits %>%
  rename(OrigValueStr = "LMA") %>%
  mutate(OriglName = "LMA",
         OrigUnitStr = "g/m2",
         DataName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         TraitName = DataName,
         ValueKindName = "Single",
         StdValue = 1/ OrigValueStr * 1000, 
         UnitName = "mm2 mg-1", 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr))
data_7b_traits_std$OrigValueStr <- as.character(data_7b_traits_std$OrigValueStr)
rm(data_7b_traits)


# standardize geo-references
# PNM: Parque Natural Metropolitano (PNM, 8°59' 41.55'' N, 79°32' 35.22'' W, 30 m a.s.l.)
# PNSL: Parque Nacional San Lorenzo (PNSL, 9°16'51.71'' N, 79° 58' 28.27''W, 130 m a.s.l.)
 
data_7b$Latitude[data_7b$Site == "PNM"] <- 8.9947222
data_7b$Longitude[data_7b$Site == "PNM"] <- -79.54305555555555
data_7b$Altitude[data_7b$Site == "PNM"] <- 30

data_7b$Latitude[data_7b$Site == "PNSL"] <- 9.2808333
data_7b$Longitude[data_7b$Site == "PNSL"] <- -79.97444444444444
data_7b$Altitude[data_7b$Site == "PNSL"] <- 130

data_7b_georef <- data_7b %>%
  select(ObservationID, Latitude, Longitude, Altitude)

data_7b_georef_long <- melt(data_7b_georef, id.vars = "ObservationID") 
data_7b_georef_std <- data_7b_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = OriglName,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = as.numeric(OrigValueStr), 
         UnitName = NA, 
         Comment = NA)
data_7b_georef_std$OrigValueStr <- as.character(data_7b_georef_std$OrigValueStr)

rm(data_7b_georef, data_7b_georef_long)


# standardize rest of data
data_7b_rest <- data_7b %>%
  select(ObservationID, Site, Family:Leaf)

data_7b_rest_long <- melt(data_7b_rest, id.vars = "ObservationID")

data_7b_rest_std <- data_7b_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)
data_7b_rest_std$OrigValueStr <- as.character(data_7b_rest_std$OrigValueStr)
rm(data_7b_rest, data_7b_rest_long)

data_7b_rest_std$DataName[data_7b_rest_std$OriglName == "Tree"] <- "Plant ID / Individual ID"
data_7b_rest_std$DataName[data_7b_rest_std$OriglName == "Leaf"] <- "Leaf ID"


# combine all dataframes
names(data_7b_traits_std)
names(data_7b_georef_std)
names(data_7b_rest_std)

data_7b_std <- bind_rows(data_7b_traits_std, data_7b_georef_std, data_7b_rest_std)
data_7b_std <- left_join(data_7b_std, data_7b_obs_spec, by = "ObservationID")
data_7b_std <- data_7b_std %>%
  select(ObservationID, LastName:Dataset, Species, OriglName:Comment) %>%
  rename(SpeciesName = "Species") %>%
  arrange(ObservationID)

rm(data_7b_traits_std, data_7b_georef_std, data_7b_rest_std, data_7b_obs_spec)

save(data_7b_std, file = "C:/Users/Nutzer/Desktop/6440/data_7b_std.RData")


#----------
# BIEN data
#----------

install.packages("BIEN")
library(BIEN)

# create list of traits in BIEN
BIEN_traits <- BIEN_trait_list()



# create list of traits (that are included in our TRY dataset)
desired_traits <- c("diameter at breast height (1.3 m)", 
                    "leaf area", 
                    "leaf area per leaf dry mass", 
                    "leaf carbon content per leaf dry mass",
                    "leaf carbon content per leaf nitrogen content", 
                    "leaf dry mass",
                    "leaf dry mass per leaf fresh mass",
                    "leaf fresh mass",
                    "leaf nitrogen content per leaf area",
                    "leaf nitrogen content per leaf dry mass",
                    "leaf phosphorus content per leaf area",
                    "leaf phosphorus content per leaf dry mass",
                    "leaf photosynthetic rate per leaf area", 
                    "leaf photosynthetic rate per leaf dry mass",
                    "leaf thickness",
                    "seed mass",
                    "stem wood density",
                    "whole plant height")


BIEN_trait_data <- BIEN_trait_trait(trait = desired_traits)

BIEN_height <- BIEN_trait_trait(trait = "whole plant height")

# check structure of dataset
str(BIEN_trait_data)

# create unique identifier for each observation
BIEN_trait_data <- BIEN_trait_data %>%
  mutate(ObservationID = paste("BIEN", rownames(BIEN_trait_data), sep = "_"))

# create subset with ObservationID & Species Names
BIEN_obs_spec <- BIEN_trait_data %>%
  select(ObservationID, scrubbed_species_binomial, project_pi) %>%
  distinct(ObservationID, .keep_all = T) %>%
  rename(LastName = "project_pi") %>%         # not very sophisticated but manual separation wouldn't be worth the effort
  mutate(FirstName = NA,
         Dataset = "BIEN_4.1")


# standardize traits
BIEN_traits <- BIEN_trait_data %>%
  select(ObservationID, trait_name, trait_value, unit) %>%
  rename(OriglName = "trait_name",
         OrigValueStr = "trait_value",
         OrigUnitStr = "unit") %>%
  mutate(OrigValueStr = as.numeric(OrigValueStr),
         DataName = NA,
         TraitName = NA,
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA) %>%
  select(ObservationID, OriglName, OrigValueStr, OrigUnitStr, DataName, TraitName, ValueKindName, StdValue, UnitName, Comment) %>%
  filter(complete.cases(OrigValueStr))


SD <- BIEN_traits %>%
  filter(OriglName == "diameter at breast height (1.3 m)") %>%
  mutate(TraitName = "Stem diameter",
         StdValue = OrigValueStr / 100,
         UnitName = "m")

LA <- BIEN_traits %>%
  filter(OriglName == "leaf area") %>%
  mutate(TraitName = "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)",
         StdValue = OrigValueStr,
         UnitName = "mm2")

SLA <- BIEN_traits %>%
  filter(OriglName == "leaf area per leaf dry mass") %>%
  mutate(TraitName = "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded",
         StdValue = OrigValueStr,
         UnitName = "mm2 mg-1")

LCCmass <- BIEN_traits %>%
  filter(OriglName == "leaf carbon content per leaf dry mass") %>%
  mutate(TraitName = "Leaf carbon (C) content per leaf dry mass",
         StdValue = OrigValueStr,
         UnitName = "mg/g")

CN <- BIEN_traits %>%
  filter(OriglName == "leaf carbon content per leaf nitrogen content") %>%
  mutate(TraitName = "Leaf carbon/nitrogen (C/N) ratio",
         StdValue = OrigValueStr,
         UnitName = "g/cm3")

LDM <- BIEN_traits %>%
  filter(OriglName == "leaf dry mass") %>%
  mutate(TraitName = "Leaf dry mass (single leaf)",
         StdValue = OrigValueStr * 1000,  # g -> mg
         UnitName = "mg")

LDMC <- BIEN_traits %>%
  filter(OriglName == "leaf dry mass per leaf fresh mass") %>%
  mutate(TraitName = "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)",
         StdValue = OrigValueStr / 1000,  # mg/ -> g/g
         UnitName = "g g-1")

LFM <- BIEN_traits %>%
  filter(OriglName == "leaf fresh mass") %>%
  mutate(TraitName = "Leaf fresh mass",
         StdValue = OrigValueStr,  
         UnitName = "g")

LNCarea <- BIEN_traits %>%
  filter(OriglName == "leaf nitrogen content per leaf area") %>%
  mutate(TraitName = "Leaf nitrogen (N) content per leaf area",
         StdValue = OrigValueStr * 1000,  # kg/m2 -> g/m2
         UnitName = "g m-2")

LNCmass <- BIEN_traits %>%
  filter(OriglName == "leaf nitrogen content per leaf dry mass") %>%
  mutate(TraitName = "Leaf nitrogen (N) content per leaf dry mass",
         StdValue = OrigValueStr, 
         UnitName = "mg/g")

LPCarea <- BIEN_traits %>%
  filter(OriglName == "leaf phosphorus content per leaf area") %>%
  mutate(TraitName = "Leaf phosphorus (P) content per leaf area",
         StdValue = OrigValueStr, 
         UnitName = "g m-2")

LPCmass <- BIEN_traits %>%
  filter(OriglName == "leaf phosphorus content per leaf dry mass") %>%
  mutate(TraitName = "Leaf phosphorus (P) content per leaf dry mass",
         StdValue = OrigValueStr, 
         UnitName = "mg/g")

Photo_area <- BIEN_traits %>%
  filter(OriglName == "leaf photosynthetic rate per leaf area") %>%
  mutate(TraitName = "Leaf photosynthesis rate per leaf area",
         StdValue = OrigValueStr, 
         UnitName = "micro mol m-2 s-1")

Photo_mass <- BIEN_traits %>%
  filter(OriglName == "leaf photosynthetic rate per leaf dry mass") %>%
  mutate(TraitName = "Leaf photosynthesis rate per leaf dry mass",
         StdValue = OrigValueStr, 
         UnitName = "micro mol g-1 s-1")

LT <- BIEN_traits %>%
  filter(OriglName == "leaf thickness") %>%
  mutate(TraitName = "Leaf thickness",
         StdValue = OrigValueStr, 
         UnitName = "mm")

SM <- BIEN_traits %>%
  filter(OriglName == "seed mass") %>%
  mutate(TraitName = "Seed dry mass",
         StdValue = OrigValueStr, 
         UnitName = "mg")

SSD <- BIEN_traits %>%
  filter(OriglName == "stem wood density") %>%
  mutate(TraitName = "Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)",
         StdValue = OrigValueStr, 
         UnitName = "g/cm3")

height <- BIEN_traits %>%
  filter(OriglName == "whole plant height") %>%
  mutate(TraitName = "Plant height vegetative",
         StdValue = OrigValueStr, 
         UnitName = "m")

BIEN_traits_std <- bind_rows(SD, LA, SLA, LCCmass, CN, LDM, LDMC, LFM, LNCarea, LNCmass, LPCarea, LPCmass, 
                             Photo_area, Photo_mass, LT, SM, SSD, height)
BIEN_traits_std$OrigValueStr <- as.character(BIEN_traits_std$OrigValueStr)

rm(BIEN_traits, SD, LA, SLA, LCCmass, CN, LDM, LDMC, LFM, LNCarea, LNCmass, LPCarea, LPCmass, Photo_area, Photo_mass, LT, SM, SSD, height)


BIEN_traits_std <- BIEN_traits_std %>%
  mutate(DataName = TraitName)

BIEN_traits_std <- BIEN_traits_std %>%
  mutate(ValueKindName = "Single") %>%
  filter(StdValue != 0)


# standardize georeferences
BIEN_georef <- BIEN_trait_data %>%
  select(ObservationID, latitude, longitude, elevation_m)

BIEN_georef_long <- melt(BIEN_georef, id.vars = "ObservationID") 
BIEN_georef_long <- BIEN_georef_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA, 
         Comment = NA)

LAT <- BIEN_georef_long %>%
  filter(OriglName == "latitude") %>%
  mutate(DataName = "Latitude",
         StdValue = OrigValueStr)

LON <- BIEN_georef_long %>%
  filter(OriglName == "longitude") %>%
  mutate(DataName = "Longitude",
         StdValue = OrigValueStr)

Altitude <- BIEN_georef_long %>%
  filter(OriglName == "elevation_m") %>%
  mutate(DataName = "Altitude",
         StdValue = OrigValueStr)

BIEN_georef_std <- bind_rows(LAT, LON, Altitude)
BIEN_georef_std$OrigValueStr <- as.character(BIEN_georef_std$OrigValueStr)

rm(BIEN_georef, BIEN_georef_long, LAT, LON, Altitude)


# standardize rest of data
BIEN_rest <- BIEN_trait_data %>%
  select(ObservationID, method, url_source, project_pi_contact, access, id)

BIEN_rest_long <- melt(BIEN_rest, id.vars = "ObservationID")

BIEN_rest_std <- BIEN_rest_long %>%
  rename(OriglName = "variable",
         OrigValueStr = "value") %>%
  filter(complete.cases(OrigValueStr)) %>%
  mutate(OrigUnitStr = NA,
         DataName = NA,
         TraitName = NA, 
         ValueKindName = NA,
         StdValue = NA, 
         UnitName = NA,
         Comment = NA) %>%
  distinct(ObservationID, OriglName, OrigValueStr, .keep_all = T)


rm(BIEN_rest, BIEN_rest_long)


# combine all dataframes
names(BIEN_traits_std)
names(BIEN_georef_std)
names(BIEN_rest_std)

BIEN_traits_std$OrigValueStr <- as.character(BIEN_traits_std$OrigValueStr)

BIEN_std <- bind_rows(BIEN_traits_std, BIEN_georef_std, BIEN_rest_std)
rm(BIEN_traits_std, BIEN_georef_std, BIEN_rest_std)
gc()

BIEN_std <- left_join(BIEN_std, BIEN_obs_spec, by = "ObservationID")
rm(BIEN_obs_spec)
gc()

BIEN_std <- BIEN_std %>%
  select(ObservationID, LastName:Dataset, scrubbed_species_binomial, OriglName:Comment) %>%
  rename(SpeciesName = "scrubbed_species_binomial") %>%
  arrange(ObservationID)


save(BIEN_std, file = "C:/Users/Nutzer/Desktop/6440/BIEN_std.RData")


#---------------------------------------
# combination of all dataframes into one
#---------------------------------------
# load all datasets

# change type of all columns in all data frames to character & set correct amount of columns
TOPIC_std <- TOPIC_std %>%
  rename(Reference = "StudyReference") %>%
  mutate_all(as.character)
data_2_std <- data_2_std %>%
  mutate(Comment = NA) %>%
  mutate_all(as.character)
data_3_std <- data_3_std %>%
  mutate_all(as.character)
data_4a_std <- data_4a_std %>%
  mutate_all(as.character)
data_4b_std <- data_4b_std %>%
  mutate_all(as.character)
data_4c_std <- data_4c_std %>%
  mutate_all(as.character)
data_5_std <- data_5_std %>%
  mutate_all(as.character)
data_7a_std <- data_7a_std %>%
  mutate_all(as.character)
data_7b_std <- data_7b_std %>%
  mutate_all(as.character)
BIEN_std <- BIEN_std %>%
  mutate_all(as.character)

str(TOPIC_std)

# combine all dataframes
additional_data <- full_join(TOPIC_std, data_2_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName"))
additional_data <- full_join(additional_data, data_3_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
additional_data <- full_join(additional_data, data_4a_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
additional_data <- full_join(additional_data, data_4b_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
additional_data <- full_join(additional_data, data_4c_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
additional_data <- full_join(additional_data, data_5_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
additional_data <- full_join(additional_data, data_7a_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment", "Reference"))
additional_data <- full_join(additional_data, data_7b_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
additional_data <- full_join(additional_data, data_7b_std, 
                             by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
                                    "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
                                    "StdValue", "UnitName", "Comment"))
# additional_data <- full_join(additional_data, BIEN_std, 
#                              by = c("ObservationID","LastName", "FirstName", "Dataset", "SpeciesName", "OriglName", "OrigValueStr", 
#                                     "OrigUnitStr", "DataName", "TraitName", "ValueKindName", 
#                                     "StdValue", "UnitName", "Comment"))


additional_data <- additional_data %>%
  select(ObservationID:Comment)

rm(data_2_std, data_3_std, data_4a_std, data_4b_std, data_4c_std, data_5_std, data_7a_std, data_7b_std,
   TOPIC_std)

#------------------------------------------
# standardization of species names via TNRS
#------------------------------------------
# 1) for "additional_data"
#-------------------------

# get unque species names of all additional dataframes
spec_names <- additional_data %>%
  distinct(SpeciesName) %>%
  arrange(SpeciesName)

# save names as .txt (input for TNRS)
write.table(spec_names, file = "spec_names.txt", col.names = F, row.names = F)
rm(spec_names)

# load names from .txt (output of TNRS)
spec_names_TNRS <- read.delim("spec_names_TNRS.txt", encoding = "UTF-8")

spec_names_TNRS %>%
  filter(Accepted_name == "")

# match accepted names to original names
spec_names_std <- spec_names_TNRS %>%
  select(Name_submitted, Accepted_name) %>%
  rename(SpeciesName = "Name_submitted",
         AccSpeciesName = "Accepted_name") %>%
  mutate_all(as.character)

# copy SpeciesName to AccSpeciesName if AccSpeciesName == ""

spec_names_std$AccSpeciesName[spec_names_std$AccSpeciesName == ""] <- spec_names_std$SpeciesName

additional_data <- left_join(additional_data, spec_names_std, by = "SpeciesName")
additional_data <- additional_data %>%
  select(ObservationID:Dataset, SpeciesName, AccSpeciesName, OriglName:Comment)

rm(spec_names_std, spec_names_TNRS)


# 2) for BIEN data
#-----------------
# get unque species names of all additional dataframes
spec_names <- BIEN_std %>%
  distinct(SpeciesName) %>%
  arrange(SpeciesName)

# split up in parts of 5000 species (maximum of TNRS)
spec_names_1 <- spec_names[1:5000, ]
spec_names_2 <- spec_names[5001:10000, ]
spec_names_3 <- spec_names[10001:15000, ]
spec_names_4 <- spec_names[15001:20000, ]
spec_names_5 <- spec_names[20001:25000, ]
spec_names_6 <- spec_names[25001:nrow(spec_names), ]

# save names as .txt (input for TNRS)
write.table(spec_names_1, file = "spec_names_BIEN_1.txt", col.names = F, row.names = F)
write.table(spec_names_2, file = "spec_names_BIEN_2.txt", col.names = F, row.names = F)
write.table(spec_names_3, file = "spec_names_BIEN_3.txt", col.names = F, row.names = F)
write.table(spec_names_4, file = "spec_names_BIEN_4.txt", col.names = F, row.names = F)
write.table(spec_names_5, file = "spec_names_BIEN_5.txt", col.names = F, row.names = F)
write.table(spec_names_6, file = "spec_names_BIEN_6.txt", col.names = F, row.names = F)

rm(spec_names, spec_names_1, spec_names_2, spec_names_3, spec_names_4, spec_names_5, spec_names_6)

# load names from .txt (output of TNRS)
spec_names_TNRS_1 <- read.delim("spec_names_BIEN_1_TNRS.txt", encoding = "UTF-8")
spec_names_TNRS_2 <- read.delim("spec_names_BIEN_2_TNRS.txt", encoding = "UTF-8")
spec_names_TNRS_3 <- read.delim("spec_names_BIEN_3_TNRS.txt", encoding = "UTF-8")
spec_names_TNRS_4 <- read.delim("spec_names_BIEN_4_TNRS.txt", encoding = "UTF-8")
spec_names_TNRS_5 <- read.delim("spec_names_BIEN_5_TNRS.txt", encoding = "UTF-8")
spec_names_TNRS_6 <- read.delim("spec_names_BIEN_6_TNRS.txt", encoding = "UTF-8")

spec_names_TNRS <- bind_rows(spec_names_TNRS_1, spec_names_TNRS_2, spec_names_TNRS_3, spec_names_TNRS_4,spec_names_TNRS_5, spec_names_TNRS_6)
rm(spec_names_TNRS_1, spec_names_TNRS_2, spec_names_TNRS_3, spec_names_TNRS_4, spec_names_TNRS_5, spec_names_TNRS_6)

# select matched species names according to taxonomic state (accepted > synonym > unresolved | no opinion)

spec_names_TNRS <- spec_names_TNRS %>%
  rename("Name_submitted" = X.U.FEFF.Name_submitted) 
species <- unique(spec_names_TNRS$Name_submitted)

TNRS_std <- data.frame("Name_submitted" = NA,
                       "Name_matched" = NA,
                       "Author_matched" = NA,
                       "Overall_score" = NA,
                       "Taxonomic_status" = NA,
                       "Accepted_name" = NA,
                       "Accepted_author" = NA,
                       "Accepted_family" = NA,
                       "Source" = NA,
                       "Warnings" = NA,
                       "Accepted_name_lsid" = NA)

for (i in 1:length(species)) {
  TNRS_tmp <- filter(spec_names_TNRS, Name_submitted == species[i])
  if ("Accepted" %in% TNRS_tmp$Taxonomic_status) {
    TNRS_acc <- TNRS_tmp[TNRS_tmp$Taxonomic_status == "Accepted", ]
    TNRS_std <- bind_rows(TNRS_std, TNRS_acc[1, ])
    next()
  } else {
    if ("Synonym" %in% TNRS_tmp$Taxonomic_status) {
      TNRS_syn <- TNRS_tmp[TNRS_tmp$Taxonomic_status == "Synonym", ]
      TNRS_std <- bind_rows(TNRS_std, TNRS_syn[1, ])
      next()
    } else {
      TNRS_std <- bind_rows(TNRS_std, TNRS_tmp[1, ])
      next()
    }
  }
}

rm(TNRS_acc, TNRS_syn, TNRS_tmp, i, species)

# match accepted names to original names
spec_names_std <- TNRS_std %>%
  select(Name_submitted, Accepted_name) %>%
  rename(SpeciesName = "Name_submitted",
         AccSpeciesName = "Accepted_name") %>%
  mutate_all(as.character) %>%
  filter(complete.cases(SpeciesName))

# copy SpeciesName to AccSpeciesName if AccSpeciesName == ""
spec_names_std$AccSpeciesName[spec_names_std$AccSpeciesName == ""] <- spec_names_std$SpeciesName

# combine BIEN and spec_names_std
BIEN_std <- left_join(BIEN_std, spec_names_std, by = "SpeciesName")
BIEN_std <- BIEN_std %>%
  select(ObservationID:Dataset, SpeciesName, AccSpeciesName, OriglName:Comment)

rm(spec_names_std, spec_names_TNRS, TNRS_std)


#----------------------------------------------------------------------------------------
# assign correct DataIDs to Latitude, Longitude, TraitName & correct TraitID to TraitName
#----------------------------------------------------------------------------------------

# load "TRY_6440_import.RData"

# 1) for "additional_data"
DataIDs <- import_TRY %>%
  distinct(DataID, DataName) %>%
  arrange(DataName) 

TraitIDs <- import_TRY %>%
  distinct(TraitID, TraitName) %>%
  arrange(TraitName) 

additional_data <- left_join(additional_data, DataIDs, by = "DataName")
additional_data <- left_join(additional_data, TraitIDs, by = "TraitName")

save(additional_data, file = "additional_data_std.RData")

# 2) for "BIEN_std"
# remove observations for stem diameter
Observarions_SD <- BIEN_std %>%
  filter(TraitName == "Stem diameter")
Observarions_SD <- unique(Observarions_SD$ObservationID)

BIEN_std <- BIEN_std %>%
  filter(!ObservationID %in% Observarions_SD)
rm(Observarions_SD)

BIEN_std <- left_join(BIEN_std, DataIDs, by = "DataName")
BIEN_std <- left_join(BIEN_std, TraitIDs, by = "TraitName")

BIEN_std <- BIEN_std %>%
  distinct(ObservationID, LastName, FirstName, Dataset, SpeciesName, AccSpeciesName, OriglName, OrigValueStr, OrigUnitStr,
           DataName, TraitName, ValueKindName, StdValue, UnitName, Comment, TraitID, .keep_all = T)

save(BIEN_std, file = "C:/Users/Nutzer/Desktop/6440/BIEN_std_WO_stemdia.RData")

rm(DataIDs, TraitIDs)


#-----------------------------------------------------------
# creating dataframe containing additional data and TRY data
#-----------------------------------------------------------
# load("TRY_6440_import.RData")
# change column type in "import_TRY" to character for easy merging
import_TRY <- import_TRY %>%
  mutate_all(as.character)

names(import_TRY)

# create missing columns to bind rows from both datasets
additional_data <- additional_data %>%
  mutate(LastName = NA,
         FirstName = NA,
         DatasetID = NA,
         Dataset = NA, 
         AccSpeciesID = NA,
         ObsDataID = NA,
         OrigUncertaintyStr = NA,
         UncertaintyName = NA, 
         Replicates = NA, 
         RelUncertaintyPercent = NA,
         OrigObsDataID = NA, 
         ErrorRisk = NA, 
         X = NA)

additional_data <- additional_data %>%
  select(names(import_TRY))

additional_data <- additional_data %>%
  mutate_all(as.character)

names(additional_data)
names(import_TRY)

data_full <- bind_rows(additional_data, import_TRY)
data_full <- data_full %>%
  mutate(LastName = as.factor(LastName),
         FirstName = as.factor(FirstName),
         DatasetID = as.factor(DatasetID),
         Dataset = as.factor(Dataset),
         SpeciesName = as.factor(SpeciesName),
         AccSpeciesID = as.factor(AccSpeciesID),
         AccSpeciesName = as.factor(AccSpeciesName),
         ObservationID = as.factor(ObservationID),
         ObsDataID = as.integer(ObsDataID),
         TraitID = as.factor(TraitID),
         TraitName = as.factor(TraitName),
         DataID = as.factor(DataID),
         DataName = as.factor(DataName),
         OriglName = as.character(OriglName),
         OrigValueStr = as.character(OrigValueStr),
         OrigUnitStr = as.character(OrigUnitStr),
         ValueKindName = as.factor(ValueKindName),
         OrigUncertaintyStr = as.character(OrigUncertaintyStr),
         UncertaintyName = as.character(UncertaintyName),
         Replicates = as.numeric(Replicates),
         StdValue = as.numeric(StdValue),
         UnitName = as.factor(UnitName),
         RelUncertaintyPercent = as.numeric(RelUncertaintyPercent),
         OrigObsDataID = as.integer(OrigObsDataID),
         ErrorRisk = as.numeric(ErrorRisk),
         Reference = as.factor(Reference),
         Comment = as.factor(Comment),
         X = as.character(X))

str(data_full)

rm(additional_data, import_TRY)

save(data_full, file = "data_TRY_and_additional1-7.RData")


#------------------------------------
# checking data frames for duplicates
#------------------------------------
# create comparable dataframes for BIEN and TRY

# load and format TRY data
load("C:/Users/Nutzer/Dropbox/Gabriel/R-Skripte/data_TRY_and_additional1-7.RData")

traits <- data_full %>%
  select(ObservationID, FirstName, LastName, AccSpeciesName, TraitID, TraitName, StdValue, UnitName, OrigObsDataID, ObsDataID) %>%
  filter(complete.cases(TraitID) &      # exclude all entries with "" in TraitName
           complete.cases(StdValue))    # exclude potential categorical traits that don't have a StdValue

Latitude <- data_full %>%
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Latitude") %>%
  rename(Latitude = StdValue) %>%
  select(ObservationID, Latitude) %>%
  distinct(ObservationID, .keep_all = T)

Longitude <- data_full %>%
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Longitude") %>%
  rename(Longitude = StdValue) %>%
  select(ObservationID, Longitude) %>%
  distinct(ObservationID, .keep_all = T)

TRY_traits_x_georef <- left_join(traits, Latitude, by = "ObservationID")
TRY_traits_x_georef <- left_join(TRY_traits_x_georef, Longitude, by = "ObservationID")

TRY_traits_x_georef <- TRY_traits_x_georef %>%
  filter(complete.cases(AccSpeciesName)) %>%
  arrange(AccSpeciesName, TraitName)

rm(traits, Latitude, Longitude)

# load and format BIEN data
load("C:/Users/Nutzer/Desktop/6440/BIEN_std_WO_stemdia.RData")

traits <- BIEN_std %>%
  select(ObservationID, FirstName, LastName, AccSpeciesName, TraitID, TraitName, StdValue, UnitName) %>%
  filter(complete.cases(TraitID) &      # exclude all entries with "" in TraitName
           complete.cases(StdValue))    # exclude potential categorical traits that don't have a StdValue

traits <- traits %>%
  arrange(AccSpeciesName, TraitName)

Latitude <- BIEN_std %>%
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Latitude") %>%
  rename(Latitude = StdValue) %>%
  select(ObservationID, Latitude) %>%
  distinct(ObservationID, .keep_all = T)

Longitude <- BIEN_std %>%
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Longitude") %>%
  rename(Longitude = StdValue) %>%
  select(ObservationID, Longitude) %>%
  distinct(ObservationID, .keep_all = T)

BIEN_traits_x_georef <- left_join(traits, Latitude, by = "ObservationID")
BIEN_traits_x_georef <- left_join(BIEN_traits_x_georef, Longitude, by = "ObservationID")

BIEN_traits_x_georef <- BIEN_traits_x_georef %>%
  filter(complete.cases(AccSpeciesName)) %>%
  arrange(AccSpeciesName, TraitName)

rm(traits, Latitude, Longitude)


# check number of geo-referenced trait observations in both datasets
TRY_traits_x_georef %>%
  filter(complete.cases(Latitude) & complete.cases(Longitude)) %>%
  summarize(n = n())
    # 2206056 / 2678551 = 82.36%

BIEN_traits_x_georef %>%
  filter(complete.cases(Latitude) & complete.cases(Longitude)) %>%
  summarize(n = n())
    # 9056154 / 10205828 = 88.74%

# separate traits in 1) geo-referenced and 2) not geo-referenced observations (in TRY and BIEN)
BIEN_traits_geo <- BIEN_traits_x_georef %>%
  filter(complete.cases(Latitude) & complete.cases(Longitude))
BIEN_traits_non_geo <- setdiff(BIEN_traits_x_georef, BIEN_traits_geo)

#-------------------------------------
# loop for identification of duplicats
#-------------------------------------
# create counter vector for different dataframes
# this way duplicats within datasets are possible but not between datasets
author_ID <- unique(BIEN_traits_geo$LastName)
# create initial dataframe that only contains observations from 1 single dataset
BIEN_geo_WO_duplicats <- BIEN_traits_geo %>%
  filter(LastName == author_ID[1])

# create process bar
pb <- txtProgressBar(min = 0, max = length(author_ID), initial = 0)

for (h in 2:length(author_ID)) {  # start with 2 because 1 = dataset without duplicats
  # create dataframe (step-by-step selection of different datasets in dataframe)
  # observations from this dataframe are compared with observations from dataframe without duplicats to identify duplicats  
  BIEN_tmp <- BIEN_traits_geo %>%
    filter(LastName == author_ID[h])
  BIEN_storage <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), names(BIEN_geo_WO_duplicats))
  
  for (i in 1:nrow(BIEN_tmp)) {
    # select each row in temporary DF separately
    # create vector that stores logical values (indicating if line i of temporary DF occurs somewhere in DF without duplicats)
    x <- logical(length = 0)
    for (j in 1:nrow(BIEN_geo_WO_duplicats)) {
      # compare selected line i from temporary dataframe with each row in dataframe without duplicats
      # "test_tmp[i, 2:3] %in% test_WO_duplicats[j, 2:3]" returns a vector of n (n = number of columns to compare) logical values
      # if one value of a column in temporary dataframe is identical to a value in DF without duplicats it returns a "TRUE"
      # "FALSE %in% (test_tmp[i, 2:3] %in% test_WO_duplicats[j, 2:3])" returns "TRUE" if at least one element in the output vector is "FALSE"
      # meaning that the observation is not a duplicat
      dupl <- FALSE %in% (BIEN_tmp[i, c(4, 6, 7, 9, 10)] %in% BIEN_geo_WO_duplicats[j, c(4, 6, 7, 9, 10)])
      # output for the comparison of line i with all lines j is stored in "x"
      x <- c(x, dupl)
    }
    if (FALSE %in% x) {
      # if there is one "FALSE" in vector "x" it means observation of line i is a duplicat
      # thus, the next i is checked
      next()
    } else {
      # if it is not a duplicat the value is stored in another dataframe
      # this is necessary to allow for duplicats within datasets but not between them
      # (adding the lines directly to the DF without duplicats would not enable this)
      BIEN_storage <- bind_rows(BIEN_storage, BIEN_tmp[i, ])
    }
  }
  # all observations of temporary DF are added to DF without duplicats
  # i.e. DF without duplicats contains initial observations + all observations from temporary DF that are not part of initial DF
  # (but with duplicats within temporary DF)
  BIEN_geo_WO_duplicats <- bind_rows(BIEN_geo_WO_duplicats, BIEN_storage)
  setTxtProgressBar(pb, h)
}

# only add observations from BIEN to TRY that do not occur in TRY yet (AccSpeciesName, TraitName, StdValue, Latitude, Longitude)

#---------------------------------------
# alternative way to identify duplicates 
#---------------------------------------

# create easy example
# (last Observation of B and C should be identified as duplicate of 1st and 2nd Observation in A)
#------------------------------------------------------------------------------------------------
dfA <- data.frame(LastName = c("A", "A", "A"), AccSpeciesName = c("Aa", "Aa", "Bb"), StdValue = c(1, 1, 1))
dfB <- data.frame(LastName = c("B", "B", "B"), AccSpeciesName = c("Aa", "Aa", "Aa"), StdValue = c(2, 2, 1))
dfC <- data.frame(LastName = c("C", "C", "C"), AccSpeciesName = c("Aa", "Aa", "Aa"), StdValue = c(3, 3, 1))

df <- rbind(dfA, dfB, dfC)
df$duplicate <- NA

df_names <- unique(df$LastName)

df_WO_duplicate <- df %>%
  filter(LastName == df_names[1])

for (i in 2:length(df_names)) {
  df_check_duplicate <- df %>%
    filter(LastName == df_names[i])
  
  df_temp <- rbind(df_WO_duplicate, df_check_duplicate)
  df_temp$duplicate <- dupsBetweenGroups(df_temp, "LastName")
  
  df_to_add <- df_temp %>%
    filter(LastName == df_names[i] &
             duplicate == F)
  
  df_WO_duplicate <- rbind(df_WO_duplicate, df_to_add)
  
}


# apply easy example to BIEN & TRY data
# 1) check for duplicats BETWEEN datasets WITHIN BIEN
#----------------------------------------------------
start.time <- proc.time()

df_names <- unique(BIEN_traits_geo$LastName)

df_WO_duplicate <- BIEN_traits_geo %>%
  filter(LastName == df_names[1])

for (i in 2:length(df_names)) {
  df_check_duplicate <- BIEN_traits_geo %>%
    filter(LastName == df_names[i])
  
  df_temp <- rbind(df_WO_duplicate, df_check_duplicate)
  df_temp_NO_ObsID <- subset(df_temp, select = -ObservationID)
  duplicate_check <- dupsBetweenGroups(df_temp_NO_ObsID, "LastName")
  # df_temp$duplicate <- df_temp_NO_ObsID$duplicate
  df_temp <- cbind(df_temp, duplicate = duplicate_check)
  
  df_to_add <- df_temp %>%
    filter(LastName == df_names[i] &
             duplicate == F) %>%
    select(-duplicate)
  
  df_WO_duplicate <- rbind(df_WO_duplicate, df_to_add)
  
}

end.time <- proc.time()
end.time - start.time

BIEN_WO_duplicates <- df_WO_duplicate

rm(df_check_duplicate, df_temp, df_temp_NO_ObsID, df_to_add, df_WO_duplicate, duplicate_check, df_names, i, start.time, end.time)

# 2) check TRY for internal duplicates
TRY_OrigObsdataIDs <- TRY_traits_x_georef %>%
  distinct(OrigObsDataID, in.df = NA) %>%
  filter(complete.cases(OrigObsDataID))

pb <- txtProgressBar(min = 0, max = nrow(TRY_OrigObsdataIDs), initial = 0)

for (i in 1:nrow(TRY_OrigObsdataIDs)) {
  if (TRY_OrigObsdataIDs$OrigObsDataID[i] %in% TRY_traits_x_georef$ObsDataID) {
    TRY_OrigObsdataIDs$in.df[i] <- T
  } else {
    TRY_OrigObsdataIDs$in.df[i] <- F
  }
  setTxtProgressBar(pb, i)
}

rm(pb, i)

TRY_traits_x_georef <- left_join(TRY_traits_x_georef, TRY_OrigObsdataIDs, by = "OrigObsDataID")
TRY_traits_x_georef <- TRY_traits_x_georef %>%
  filter(in.df == F | is.na(in.df))  # include only Observations that have an entry in ORigObsDataID that matches no other entry in ObsDataID somewhere in DF and all NAs (entries that don't have an OrigObsdataID)

  # some OrigObsDataIDs are in DF more than once -> unique?


# 3) check for duplicats between BIEN and TRY (keep TRY Observations)
names(TRY_traits_x_georef)
BIEN_WO_duplicates$OrigObsDataID <- NA
BIEN_WO_duplicates$ObsDataID <- NA

TRY_traits_x_georef <- TRY_traits_x_georef[, -13]

BIEN_WO_duplicates <- BIEN_WO_duplicates %>%
  select(names(TRY_traits_x_georef))


TRY_traits_x_georef$DF <- "TRY"
BIEN_WO_duplicates$DF <- "BIEN"


df_temp <- rbind(TRY_traits_x_georef, BIEN_WO_duplicates)
df_temp_NO_ObsID <- subset(df_temp, select = -c(ObservationID, FirstName, LastName, TraitID, OrigObsDataID, ObsDataID))
duplicate_check <- dupsBetweenGroups(df_temp_NO_ObsID, "DF")
# df_temp$duplicate <- df_temp_NO_ObsID$duplicate
df_temp <- cbind(df_temp, duplicate = duplicate_check)

table(df_temp$duplicate)
  #    FALSE     TRUE 
  # 10834518    18842


# get ObservationsIDs from BIEN data that are dupicated in TRY
#-------------------------------------------------------------
# These ObservationIDs should be deleted in the standardized BIEN-DF. 
# Afterwards the 3 DFs (import_TRY, BIEN_std, additional_data) can be combined in 1 overall DF
# ATTENTION: Duplicate selection for TRY data has to be done again afterwards

BIEN_duplicates <- df_temp %>%
  filter(duplicate == T & DF == "BIEN")
exclude <- unique(BIEN_duplicates$ObservationID)

BIEN_std$exclude <- BIEN_std$ObservationID %in% exclude
BIEN_std_WO_dupl <- BIEN_std[BIEN_std$exclude == F, -ncol(BIEN_std)]

save.image(file = "C:/Users/Nutzer/Desktop/duplicate_exclusion_current_selection.RData")

rm(BIEN_std, BIEN_traits_geo, BIEN_traits_x_georef, BIEN_WO_duplicates, BIEN_duplicates, df_temp, df_temp_NO_ObsID, TRY_OrigObsdataIDs, 
   TRY_traits_x_georef, duplicate_check, exclude)

save(BIEN_std_WO_dupl, file = "C:/Users/Nutzer/Desktop/6440/BIEN_WO_dupl.RData")
save(data_full, file = "C:/Users/Nutzer/Desktop/6440/TRY_additional.RData")


#-----------------------
# combine all dataframes
#-----------------------

# standardize DF headers for "rbind"
col_names <- names(data_full)

missing_names <- setdiff(col_names, names(BIEN_std_WO_dupl))
BIEN_std_WO_dupl[missing_names] <- NA
BIEN_std_WO_dupl <- BIEN_std_WO_dupl[col_names]

rm(col_names, missing_names)

# transform all columns in both DFs into character for combining
BIEN_std_WO_dupl <- BIEN_std_WO_dupl %>%
  mutate(LastName = as.character(LastName),
         FirstName = as.character(FirstName),
         DatasetID = as.character(DatasetID),
         Dataset = as.character(Dataset),
         SpeciesName = as.character(SpeciesName),
         AccSpeciesID = as.character(AccSpeciesID),
         AccSpeciesName = as.character(AccSpeciesName),
         ObservationID = as.character(ObservationID),
         ObsDataID = as.character(ObsDataID),
         TraitID = as.character(TraitID),
         TraitName = as.character(TraitName),
         DataID = as.character(DataID),
         DataName = as.character(DataName),
         OriglName = as.character(OriglName),
         OrigValueStr = as.character(OrigValueStr),
         OrigUnitStr = as.character(OrigUnitStr),
         ValueKindName = as.character(ValueKindName),
         OrigUncertaintyStr = as.character(OrigUncertaintyStr),
         UncertaintyName = as.character(UncertaintyName),
         Replicates = as.character(Replicates),
         StdValue = as.character(StdValue),
         UnitName = as.character(UnitName),
         RelUncertaintyPercent = as.character(RelUncertaintyPercent),
         OrigObsDataID = as.character(OrigObsDataID),
         ErrorRisk = as.character(ErrorRisk),
         Reference = as.character(Reference),
         Comment = as.character(Comment),
         X = as.character(X))

data_full <- data_full %>%
  mutate(LastName = as.character(LastName),
         FirstName = as.character(FirstName),
         DatasetID = as.character(DatasetID),
         Dataset = as.character(Dataset),
         SpeciesName = as.character(SpeciesName),
         AccSpeciesID = as.character(AccSpeciesID),
         AccSpeciesName = as.character(AccSpeciesName),
         ObservationID = as.character(ObservationID),
         ObsDataID = as.character(ObsDataID),
         TraitID = as.character(TraitID),
         TraitName = as.character(TraitName),
         DataID = as.character(DataID),
         DataName = as.character(DataName),
         OriglName = as.character(OriglName),
         OrigValueStr = as.character(OrigValueStr),
         OrigUnitStr = as.character(OrigUnitStr),
         ValueKindName = as.character(ValueKindName),
         OrigUncertaintyStr = as.character(OrigUncertaintyStr),
         UncertaintyName = as.character(UncertaintyName),
         Replicates = as.character(Replicates),
         StdValue = as.character(StdValue),
         UnitName = as.character(UnitName),
         RelUncertaintyPercent = as.character(RelUncertaintyPercent),
         OrigObsDataID = as.character(OrigObsDataID),
         ErrorRisk = as.character(ErrorRisk),
         Reference = as.character(Reference),
         Comment = as.character(Comment),
         X = as.character(X))

# combine DFs
data_all_DF <- bind_rows(data_full, BIEN_std_WO_dupl)
rm(data_full, BIEN_std_WO_dupl)

# transform all columns in final DF to desired type
data_all_DF <- data_all_DF %>%
  mutate(LastName = as.factor(LastName),
         FirstName = as.factor(FirstName),
         DatasetID = as.factor(DatasetID),
         Dataset = as.factor(Dataset),
         SpeciesName = as.factor(SpeciesName),
         AccSpeciesID = as.factor(AccSpeciesID),
         AccSpeciesName = as.factor(AccSpeciesName),
         ObservationID = as.factor(ObservationID),
         ObsDataID = as.integer(ObsDataID),
         TraitID = as.factor(TraitID),
         TraitName = as.factor(TraitName),
         DataID = as.factor(DataID),
         DataName = as.factor(DataName),
         OriglName = as.character(OriglName),
         OrigValueStr = as.character(OrigValueStr),
         OrigUnitStr = as.character(OrigUnitStr),
         ValueKindName = as.factor(ValueKindName),
         OrigUncertaintyStr = as.character(OrigUncertaintyStr),
         UncertaintyName = as.character(UncertaintyName),
         Replicates = as.numeric(Replicates),
         StdValue = as.numeric(StdValue),
         UnitName = as.factor(UnitName),
         RelUncertaintyPercent = as.numeric(RelUncertaintyPercent),
         OrigObsDataID = as.integer(OrigObsDataID),
         ErrorRisk = as.numeric(ErrorRisk),
         Reference = as.factor(Reference),
         Comment = as.factor(Comment),
         X = as.character(X))

save(data_all_DF, file = "C:/Users/Nutzer/Desktop/6440/data_all_DF.RData")

#-----------------------------------------------------------------------------------------------
# pre-selection / exclusion of trait measurements that have same trait twice for 1 ObservationID
# e.g. SLA (shade leaf) & SLA (sun leaf)
#-----------------------------------------------------------------------------------------------
# identify TraitNames that have several "undesired" DataNames
#------------------------------------------------------------
trait_x_data_names <- data_all_DF %>%
  filter(TraitName != "") %>%
  distinct(TraitName, DataName) %>%
  arrange(TraitName)

write.csv(trait_x_data_names, file = "trait_x_data_names.csv")


#-------------------------------------------------------
# identify ObservationIDs with same trait more than once
#-------------------------------------------------------


# identify traits that occur more than once in an ObservationID
#--------------------------------------------------------------
ObservationID_traits <- data_all_DF %>%
  filter(TraitName_short != "" & complete.cases(TraitName_short)) %>%
  group_by(ObservationID, TraitName_short) %>%
  summarize(n_traits = n()) %>%
  arrange(TraitName_short, n_traits)

max_values_per_trait <- ObservationID_traits %>%
  group_by(TraitName_short) %>%
  summarize(max(n_traits))

Trait_x_DataName <- data_all_DF %>%
  distinct(TraitName_short, TraitName, DataName) %>%
  filter(TraitName_short != "" & complete.cases(TraitName_short)) %>%
  arrange(TraitName_short)

rm(ObservationID_traits, max_values_per_trait, Trait_x_DataName)


# create 2 DFs: 1 - contains ONLY trait data; 2 - contains ONLY additional data (exact counterpart to 1)
#-------------------------------------------------------------------------------------------------------
data_all_DF_supplemental_or_categorical_traits <- data_all_DF %>%
  filter(is.na(TraitName) | TraitName == "" | is.na(StdValue))

data_all_DF_numerical_traits <- data_all_DF %>%
  filter(complete.cases(TraitName) & 
           TraitName != "" & 
           complete.cases(StdValue))

numerical_traits_mean <- data_all_DF_numerical_traits %>%
  group_by(LastName, FirstName, DatasetID, Dataset, SpeciesName, AccSpeciesID,
           AccSpeciesName, ObservationID, StdValue, UnitName, TraitName_short) %>%
  summarize(StdValue = mean(StdValue))


rm(data_all_DF_numerical_traits, data_all_DF_supplemental_or_categorical_traits)


# ATTENTION!!!
# from here on only the column "TraitName_short" can be used for infos on numerical traits
# trait-related information that are lost in the step of combining "doubled" traits:
# 
# ObsDataID -> problematic for duplicate exclusion
# TraitID, TraitName, DataID, DataName, OriglName, OrigValueStr, OrigUnitStr, ValueKindName, OrigUncertaintyStr,
# UncertaintyName, Replicates, RelUncertaintyPercent, OrigObsDataID, ErrorRisk, Reference, Comment, X

# since many information are needed, it would be meaningful to carry out this step after all data filtering is done  

#----------------------------------------------------------------------------------------------------------
# 2) calculate mean values for individuals if we have information on leaf number or organ (LeafID, OrganID)
#----------------------------------------------------------------------------------------------------------


#--------
    # #----------------------------------------------------------------------------------------------------
    # # 1) calculate leaf_N_area for ObservationIDs where leaf_N_mass and SLA are available (also LPC, LCC)
    # #----------------------------------------------------------------------------------------------------
    # 
    # # How to combine the two traits mathematically?
    # # ---------------------------------------------
    # # given: SLA = mm2/mg; LNC (mass) = mg/g
    # # goal: LNC (area) = g/m2
    # # 1) SLA * LNC (mass) = mm2/mg * mg/g = mm2/g         | eliminates "mg"
    # # 2) mm2/g / 1,000,000 = m2/g                         | transforms mm2 into m2
    # # 3) 1 / m2/g = g/m2                                  | build reciprocal
    # 
    # data_full %>%
    #   distinct(TraitID, TraitName, UnitName) %>%
    #   arrange(TraitName) %>%
    #   View()
    # 
    # # select Observations with TraitName containing "Leaf area per leaf dry mass"
    # # AND "Leaf carbon (C) content per leaf dry mass" 
    # # OR "Leaf nitrogen (N) content per leaf dry mass"
    # # OR "Leaf phosphorus (P) content per leaf dry mass"
    # 
    # #-------
    # # A) LCC
    # #-------
    # LCC_derived <- data_full %>%
    #   filter(str_detect(TraitName, "Leaf area per leaf dry mass") |
    #            TraitName == "Leaf carbon (C) content per leaf dry mass")
    # 
    # # only select ObservationIDs for which an entry for LCC is available
    # ObservationIDs_LCC <- LCC_derived$ObservationID[LCC_derived$TraitName == "Leaf carbon (C) content per leaf dry mass"]
    # ObservationIDs_SLA <- LCC_derived$ObservationID[str_detect(LCC_derived$TraitName, "Leaf area per leaf dry mass")]
    # ObservationIDs_intersect <- ObservationIDs_LCC[ObservationIDs_LCC %in% ObservationIDs_SLA]
    # 
    # LCC_derived <- LCC_derived %>%
    #   filter(ObservationID %in% ObservationIDs_intersect)
    # 
    # rm(ObservationIDs_intersect, ObservationIDs_LCC, ObservationIDs_SLA)
    # 
    # # check how many trait entries tehre are per ObservationID 
    # # (in case there are some with more than 2 this is a problem, since this means there are diff. values for SLA)
    # ObservationIDs_focal <- LCC_derived %>%
    #   group_by(ObservationID) %>%
    #   summarize(n_traits = n()) %>%
    #   filter(n_traits == 2)               # there are some with 4 entries
    # 
    # ObservationIDs_focal <- unique(ObservationIDs_focal$ObservationID)
    # 
    # LCC_derived <- LCC_derived %>%
    #   filter(ObservationID %in% ObservationIDs_focal) %>%
    #   select(ObservationID, TraitName, StdValue)
    # 
    # rm(ObservationIDs_focal)
    # 
    # # define function which can be used via "summarize"???
    # # calc_nutrients_area <- function()
    # 
    # # spread long dataframe 
    # LCC_derived_wide <- spread(LCC_derived, key = "TraitName", value = "StdValue")
    # 
    # # bring all SLA values in 1 column
    # LCC_derived_wide$SLA <- NA
    # for (i in 1:nrow(LCC_derived_wide)) {
    #   if (complete.cases(LCC_derived_wide[i, 2])) {
    #     LCC_derived_wide$SLA[i] <- LCC_derived_wide[i, 2]
    #   } else {
    #     if(complete.cases(LCC_derived_wide[i, 3])) {
    #       LCC_derived_wide$SLA[i] <- LCC_derived_wide[i, 3]
    #     } else {
    #       LCC_derived_wide$SLA[i] <- LCC_derived_wide[i, 4]
    #     }
    #   }
    # }
    # 
    # rm(i)
    # 
    # # calculate LCC_area
    # LCC_derived_wide <- LCC_derived_wide %>%
    #   mutate(LCC_area = 1/((SLA * `Leaf carbon (C) content per leaf dry mass`)/1000000))
    # 
    # 
    # # bringing derived data in format that is similar to original data
    # LCC_derived_final <- LCC_derived_wide %>%
    #   select(ObservationID, LCC_area) %>%
    #   mutate(TraitID = 570,
    #          TraitName = "Leaf carbon (C) content per leaf area",
    #          ValueKindName = "Single",
    #          ObsDataID = NA,
    #          DataName = TraitName,
    #          DataID = NA,
    #          OriglName = NA,
    #          OrigValueStr = NA,
    #          OrigUnitStr = NA,
    #          OrigUncertaintyStr = NA,
    #          UncertaintyName = NA,
    #          Replicates = NA,
    #          UnitName = "g m-2",
    #          RelUncertaintyPercent = NA,
    #          OrigObsDataID = NA,
    #          ErrorRisk = NA,
    #          Reference = NA,
    #          Comment = "calculated from SLA and LCC(mass)",
    #          X = NA) %>%
    #   rename("StdValue" = LCC_area)
    # 
    # completion_data <- data_full %>%
    #   distinct(ObservationID, LastName, FirstName, DatasetID, Dataset, SpeciesName, 
    #            AccSpeciesID, AccSpeciesName) # DF that contains unique information for each ObservationID
    # 
    # # control if each ObservatioID is available only once
    #   # completion_data %>%
    #   #   group_by(ObservationID) %>%
    #   #   summarize(n_ObservationID = n()) %>%
    #   #   filter(n_ObservationID > 1)
    # 
    # # complement LCC_derived_final by columns from completion data
    # LCC_derived_final <- left_join(LCC_derived_final, completion_data, by = "ObservationID")
    # 
    # # order columns in LCC_derived_final according to data_full
    # LCC_derived_final <- LCC_derived_final %>%
    #   select(names(data_full))
    # 
    # rm(LCC_derived, LCC_derived_wide)
    # 
    # #-------
    # # B) LNC
    # #-------
    # LNC_derived <- data_full %>%
    #   filter(str_detect(TraitName, "Leaf area per leaf dry mass") |
    #            TraitName == "Leaf nitrogen (N) content per leaf dry mass")
    # 
    # # only select ObservationIDs for which an entry for LCC is available
    # ObservationIDs_LNC <- LNC_derived$ObservationID[LNC_derived$TraitName == "Leaf nitrogen (N) content per leaf dry mass"]
    # ObservationIDs_SLA <- LNC_derived$ObservationID[str_detect(LNC_derived$TraitName, "Leaf area per leaf dry mass")]
    # ObservationIDs_intersect <- ObservationIDs_LNC[ObservationIDs_LNC %in% ObservationIDs_SLA]
    # 
    # LNC_derived <- LNC_derived %>%
    #   filter(ObservationID %in% ObservationIDs_intersect)
    # 
    # rm(ObservationIDs_intersect, ObservationIDs_LNC, ObservationIDs_SLA)
    # 
    # # check how many trait entries tehre are per ObservationID 
    # # (in case there are some with more than 2 this is a problem, since this means there are diff. values for SLA)
    # ObservationIDs_focal <- LNC_derived %>%
    #   group_by(ObservationID) %>%
    #   summarize(n_traits = n()) %>%
    #   filter(n_traits == 2)               # there are some with way more than 2 entries
    #     # unfortunately there are ObservationIDs with 2 measurements of LNC(mass)
    #     # e.g. 1 for sun-leaves, 1 for shaded leaves
    # 
    # ObservationIDs_focal <- unique(ObservationIDs_focal$ObservationID)
    # 
    # LNC_derived <- LNC_derived %>%
    #   filter(ObservationID %in% ObservationIDs_focal) %>%
    #   select(ObservationID, TraitName, StdValue)
    # 
    # rm(ObservationIDs_focal)
    # 
    # # define function which can be used via "summarize"???
    # # calc_nutrients_area <- function()
    # 
    # # spread long dataframe 
    # LNC_derived_wide <- spread(LNC_derived, key = "TraitName", value = "StdValue")
    # 
    # # bring all SLA values in 1 column
    # LNC_derived_wide$SLA <- NA
    # for (i in 1:nrow(LNC_derived_wide)) {
    #   if (complete.cases(LNC_derived_wide[i, 2])) {
    #     LNC_derived_wide$SLA[i] <- LNC_derived_wide[i, 2]
    #   } else {
    #     if(complete.cases(LNC_derived_wide[i, 3])) {
    #       LNC_derived_wide$SLA[i] <- LNC_derived_wide[i, 3]
    #     } else {
    #       LNC_derived_wide$SLA[i] <- LNC_derived_wide[i, 4]
    #     }
    #   }
    # }
    # 
    # rm(i)
    # 
    # # calculate LCC_area
    # LNC_derived_wide <- LNC_derived_wide %>%
    #   mutate(LNC_area = 1/((SLA * `Leaf nitrogen (N) content per leaf dry mass`)/1000000))
    # 
    # 
    # # bringing derived data in format that is similar to original data
    # LNC_derived_final <- LNC_derived_wide %>%
    #   select(ObservationID, LNC_area) %>%
    #   mutate(TraitID = 50,
    #          TraitName = "Leaf nitrogen (N) content per leaf area",
    #          ValueKindName = "Single",
    #          ObsDataID = NA,
    #          DataName = TraitName,
    #          DataID = NA,
    #          OriglName = NA,
    #          OrigValueStr = NA,
    #          OrigUnitStr = NA,
    #          OrigUncertaintyStr = NA,
    #          UncertaintyName = NA,
    #          Replicates = NA,
    #          UnitName = "g m-2",
    #          RelUncertaintyPercent = NA,
    #          OrigObsDataID = NA,
    #          ErrorRisk = NA,
    #          Reference = NA,
    #          Comment = "calculated from SLA and LNC(mass)",
    #          X = NA) %>%
    #   rename("StdValue" = LNC_area)
    # 
    # completion_data <- data_full %>%
    #   distinct(ObservationID, LastName, FirstName, DatasetID, Dataset, SpeciesName, 
    #            AccSpeciesID, AccSpeciesName) # DF that contains unique information for each ObservationID
    # 
    # # control if each ObservatioID is available only once
    # # completion_data %>%
    # #   group_by(ObservationID) %>%
    # #   summarize(n_ObservationID = n()) %>%
    # #   filter(n_ObservationID > 1)
    # 
    # # complement LCC_derived_final by columns from completion data
    # LNC_derived_final <- left_join(LNC_derived_final, completion_data, by = "ObservationID")
    # 
    # # order columns in LCC_derived_final according to data_full
    # LNC_derived_final <- LNC_derived_final %>%
    #   select(names(data_full))
    # 
    # rm(LNC_derived, LNC_derived_wide)
    # 
    # 
    # #-------
    # # C) LPC
    # #-------

