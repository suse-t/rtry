#-------------------------------
#        data filtering
#-------------------------------

# required packages
#------------------
install.packages("tidyverse")
library(tidyverse)

# load data (data_TRY_and_additional1-7.RData)
load("C:/Users/Nutzer/Desktop/6440/data_all_DF.RData")

# here the dataframe with all trait measurements is loaded as "data_all_DF"
# this DF includes: original TRY dataset, additional data 1-7, BIEN trait data
# the format of this dataframe is identical with the TRY data format after importing

# view structure of dataframe
str(data_all_DF)

# select important columns for further working process
work_data <- data_all_DF %>%
  select(DatasetID, AccSpeciesID:ValueKindName, StdValue, UnitName, OrigObsDataID, Comment)

gc()


#--------------------------------------------------------------------
# create new column "TraitName_short" with abbreviation of trait name
# for later data filtering
#--------------------------------------------------------------------
TraitNames <- work_data %>%
  distinct(TraitName) %>%
  mutate(TraitName = as.character(TraitName)) %>%
  arrange(TraitName)
TraitNames <- unique(TraitNames$TraitName)

TraitName_short <- c("", "Bark thickness", "Leaf area", "Leaf area", "Leaf area", "Leaf area", "Leaf area", "Leaf area", "Leaf area",
                     "SLA", "SLA", "SLA", "SLA", "Leaf C (area)", "Leaf C (mass)", "delta 13C", "Leaf C/N", 
                     "Leaf chlorophyll content (area)", "Leaf chlorophyll content (mass)", "Leaf density", "Leaf dry mass",
                     "LDMC", "Leaf fresh mass", "Leaf N (area)", "Leaf N (mass)", "delta 15N", "Leaf N/P",
                     "Leaf petiole dry mass", "Leaf petiole fresh mass", "Leaf P (area)", "Leaf P (mass)", 
                     "Leaf photosynthesis rate (area)", "Leaf photosynthesis rate (mass)", "Leaf photosynthesis: intercellular CO2 concentration", 
                     "Leaf relative water content", "Leaf respiration rate in the dark (area)", "Leaf respiration rate in the dark (mass)",
                     "Leaf thickness", "Leaf transpiration rate (area)", "Leaf transpiration rate (mass)",
                     "Leaf water content (mass; at saturation)", "Leaf water content (mass; not saturated)", 
                     "Leaf water content (mass; missing to leaf water saturation)", "Leaf water content (total)",
                     "Leaf water saturation deficit", "delta 13C", 
                     "Plant biomass and allometry: Leaf mass (dry or fresh) per plant",
                     "Plant biomass and allometry: Shoot dry mass (plant aboveground dry mass) per plant", 
                     "Plant biomass and allometry: Stem mass (including branches) per plant",
                     "Plant biomass and allometry: Whole plant aboveground mass", 
                     "height (generative)", "Plant height of lowest branch", "height (vegetative)",
                     "longevity", "Plant N fixation capacity", 
                     "Seed mass", "Stem diameter", "Stem respiration rate (surface area)",
                     "Stem specific density (SSD)", "Stomata conductance (area)", "Wood N (mass)", NA)

rename_traits <- data.frame("TraitName" = TraitNames,
                            "TraitName_short" = TraitName_short)

work_data <- left_join(work_data, rename_traits, by = "TraitName")

rm(TraitNames, TraitName_short, rename_traits)


#----------------------------------------------
# standardize species to format "genus species"
#----------------------------------------------
# check for string legths (number of words in AccSpeciesName)
species_names <- work_data %>%
  distinct(AccSpeciesName) %>%
  arrange(AccSpeciesName) %>%
  mutate(word_length = str_count(AccSpeciesName, "\\w+"))

# check for different cases in word_length > 2
# species_names %>%
#   filter(word_length == 5) %>%
#   View()

# "var.", "subsp." - these can be reduced to "genus species"
# species with authors (3 words) - these can be reduced to "genus species"
# " x " - these are hybrids - pattern has to be preserved
# "Abies borisii-regis" - species with compund species name - these have to be preserved (use blank space as word separator to keep these)

species_names <- species_names %>%
  filter(complete.cases(AccSpeciesName)) %>%
  select(AccSpeciesName) 

# find hybrids and keep them, reduce the rest of the species names to "genus species"
species_names$AccSpeciesName_short <- ""

pb <- txtProgressBar(min = 0, max = nrow(species_names), style = 3)
for (i in 1:nrow(species_names)) {
  if (str_detect(species_names$AccSpeciesName[i], " x ")) {
    species_names$AccSpeciesName_short[i] <- word(species_names$AccSpeciesName[i], 1, 3, sep = " ")
  } else {
    species_names$AccSpeciesName_short[i] <- word(species_names$AccSpeciesName[i], 1, 2, sep = " ")
  }
  
  setTxtProgressBar(pb, i)
}

rm(i, pb)

# combine species_names with work_data
work_data <- left_join(work_data, species_names, by = "AccSpeciesName")
names(work_data)
work_data <- work_data[, c(1:3, 19, 4:7, 18, 8:17)]
rm(species_names)



#------------
# filter data
#------------

#-------------------------------------------
# 1 - exclude duplicates via "OrigObsDataID"
#-------------------------------------------
data_filtered <- work_data %>%
  filter(is.na(OrigObsDataID))


#-----------------------------------------------
# 2 - exclude observations that seem to be wrong
#-----------------------------------------------
# observations should be excluded that are more than 5 times of the SD away from mean 
# (focus: each species-trait combination)


# calculate mean, SD for each species-trait combination
SD_mean_spec_trait <- data_filtered %>%
  group_by(AccSpeciesName_short, TraitName) %>%
  summarize(n = n(),                                  # column: number of individuals per spec-trait-combi
            mean_spec_x_trait = mean(StdValue),
            SD_spec_x_trait = sd(StdValue)) %>%
  mutate(lower_limit = mean_spec_x_trait - 5*SD_spec_x_trait, 
         upper_limit = mean_spec_x_trait + 5*SD_spec_x_trait) %>%
  select(AccSpeciesName_short, TraitName, lower_limit, upper_limit)

    # all spec x trait combinations that are not continuous or only consist of 1 observation
    # get NA or NaN as SD

# combine work_data with SD_mean_spec_trait
data_filtered <- left_join(data_filtered, SD_mean_spec_trait, by = c("AccSpeciesName_short", "TraitName"))
rm(SD_mean_spec_trait)

# exclude observations where this is true: 
# StdValue < lower_limit | StdValue > upper_limit
exclude <- data_filtered %>%
  filter(StdValue < lower_limit |
           StdValue > upper_limit)

data_filtered <- setdiff(data_filtered, exclude)
names(data_filtered)
data_filtered <- data_filtered[, 1:19]

rm(exclude)



#---------------------
# 3 - filter juveniles
#---------------------
# check which DataID contains information about juveniles

DataIDs <- data_filtered %>%
  group_by(DataID, DataName) %>%
  summarize(n = n()) %>%
  arrange(DataName)

data_filtered %>% 
  filter(DataID == 413) %>%
  distinct(DataName, OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment) %>%
  View()

    # 308 - Experimental treatment
    # 6813 - Status: dead or alive
    # 413 - Plant developmental status / plant age / maturity / plant life stage

# check different states of OriglValueStr
data_filtered %>% 
  filter(DataID == 413) %>%
  distinct(OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment) %>%
  arrange(OrigValueStr) %>%
  View()

# these should be excluded:
#--------------------------
# OrigValueStr == "immature", "junvenil", "juvenil (3 years)", "juvenile", "Juvenile", "juvenile, 11-14 weeks", "juvenile, 6 weeks", "juveniles",
#                 "sapling", "Sapling (1 - 10 y)", "saplings", "seedling", "Seedling", "Seedling (0 - 1 y)", "seedlings", "seedlings, < 1/2 year"
# OriglName == "Developmental stage" & OrigValueStr == "S"
# OriglName == "Seedlings (True/False)" & OrigValueStr == "T"
# OriglName == "Juvenile" & OrigValueStr == "Y"

exclude <- data_filtered %>%
  filter(OrigValueStr == "immature"|
           OrigValueStr == "junvenil"|
           OrigValueStr == "juvenil (3 years)"|
           OrigValueStr == "juvenile"|
           OrigValueStr == "Juvenile"|
           OrigValueStr == "juvenile, 11-14 weeks"|
           OrigValueStr == "juvenile, 6 weeks"|
           OrigValueStr == "juveniles"|
           OrigValueStr == "sapling"|
           OrigValueStr == "Sapling (1 - 10 y)"|
           OrigValueStr == "saplings"|
           OrigValueStr == "seedling"|
           OrigValueStr == "Seedling"|
           OrigValueStr == "Seedling (0 - 1 y)"|
           OrigValueStr == "seedlings"|
           OrigValueStr == "seedlings, < 1/2 year"|
           OriglName == "Developmental stage" & OrigValueStr == "S"|
           OriglName == "Seedlings (True/False)" & OrigValueStr == "T"|
           OriglName == "Juvenile" & OrigValueStr == "Y")

exclude <- unique(exclude$ObservationID)
data_filtered$exclude <- data_filtered$ObservationID %in% exclude
data_filtered <- data_filtered[data_filtered$exclude == F, -20]

rm(exclude)



#-----------------------------
# 4 - filter experimental data
#-----------------------------
# check different states of OriglValueStr
data_filtered %>% 
  filter(DataID == 327) %>%
  distinct(OriglName, OrigValueStr, OrigUnitStr, StdValue, Comment) %>%
  arrange(OrigValueStr) %>%
  View()

# these should be excluded:
#--------------------------
# OrigValueStr == "Climate Chamber", "Climate chamber, non-limiting conditions, (cf. dataset reference)", "climate chambers", 
#                 "controlled environment room", "GH", "Glass house", "Glasshouse", "Glasshouse experiment", "Greehouse", "Green house", 
#                 "greenhouse", "Greenhouse", "Greenhouse plants", "Greenhouse, grrowth container", "Greenhouse, Indiana University",
#                 "Greenhouse: highlight_highpH_competition", "Greenhouse: highlight_highpH_nocompetition", 
#                 "Greenhouse: highlight_lowpH_competition", "Greenhouse: highlight_lowpH_nocompetition", 
#                 "Greenhouse: lowleight_lowpH_competition", "Greenhouse: lowlight_highpH_competition", 
#                 "Greenhouse: lowlight_highpH_nocompetition", "Greenhouse: lowlight_lowpH_nocompetition",
#                 "groth chamber", "growth chamber", "Growth chamber", "Growth Chamber", "Growth chamber, -N", "Growth chamber, +N",
#                 "growth chambers", "growth_chamber"
# OriglName == "Artificial growth conditions (G=greenhouse, C=growth chamber)" & OrigValueStr == "G"
# OriglName == "Natural / Greenhouse" & OrigValueStr == "G"

exclude <- data_filtered %>%
  filter(OrigValueStr == "Climate Chamber"|
           OrigValueStr == "Climate chamber, non-limiting conditions, (cf. dataset reference)"|
           OrigValueStr == "climate chambers"|
           OrigValueStr == "controlled environment room"|
           OrigValueStr == "GH"|
           OrigValueStr == "Glass house"|
           OrigValueStr == "Glasshouse"|
           OrigValueStr == "Glasshouse experiment"|
           OrigValueStr == "Greehouse"|
           OrigValueStr == "Green house"|
           OrigValueStr == "greenhouse"|
           OrigValueStr == "Greenhouse"|
           OrigValueStr == "Greenhouse plants"|
           OrigValueStr == "Greenhouse, grrowth container"|
           OrigValueStr == "Greenhouse, Indiana University"|
           OrigValueStr == "Greenhouse: highlight_highpH_competition"|
           OrigValueStr == "Greenhouse: highlight_highpH_nocompetition"|
           OrigValueStr == "Greenhouse: highlight_lowpH_competition"|
           OrigValueStr == "Greenhouse: highlight_lowpH_nocompetition"|
           OrigValueStr == "Greenhouse: lowleight_lowpH_competition"|
           OrigValueStr == "Greenhouse: lowlight_highpH_competition"|
           OrigValueStr == "Greenhouse: lowlight_highpH_nocompetition"|
           OrigValueStr == "Greenhouse: lowlight_lowpH_nocompetition"|
           OrigValueStr == "groth chamber"|
           OrigValueStr == "growth chamber"|
           OrigValueStr == "Growth chamber"|
           OrigValueStr == "Growth Chamber"|
           OrigValueStr == "Growth chamber, -N"|
           OrigValueStr == "Growth chamber, +N"|
           OrigValueStr == "growth chambers"|
           OrigValueStr == "growth_chamber"|
           OriglName == "Artificial growth conditions (G=greenhouse, C=growth chamber)" & OrigValueStr == "G"|
           OriglName == "Natural / Greenhouse" & OrigValueStr == "G")


exclude <- unique(exclude$ObservationID)
data_filtered$exclude <- data_filtered$ObservationID %in% exclude
data_filtered <- data_filtered[data_filtered$exclude == F, -20]

rm(exclude)



#-------------------------------------------------------
# 5 - exclude traits that are not in line with protocols
#-------------------------------------------------------
data_filtered <- data_filtered %>%
  filter(DataName != "Leaf dry area" &
           DataName != "Leaf lamina dry area" &
           DataName != "SLA disc: mid-vein, petiole and rhachis excluded; shade" &
           DataName != "SLA lamina: petiole and rhachis excluded; shade" &
           DataName != "SLA leaf; shade" &
           DataName != "Leaf senescent carbon content per dry mass" &
           DataName != "Leaf senescent dry mass" &
           DataName != "Leaf nitrogen content per dry mass (shaded leaves)" &
           DataName != "Leaf senescent nitrogen (N) content per dry mass" &
           DataName != "Leaf senescent phosphorus (P) content per dry mass" &
           DataName != "Hemisurface leaf area" &
           DataName != "Leaf nitrogen content per total area" &
           DataName != "Diameter at base" &
           DataName != "Stem diameter at base of crown (seedlings)" &
           DataName != "Stem diameter at base (basal diameter)" &
           DataName != "Stem diameter 10 cm above soil surface" |
           is.na(DataName)) # keep NAs in DataName


save(data_filtered, file = "C:/Users/Nutzer/Desktop/DF_filtered_before6.RData")


#-----------------------------------------------------
# 6 - filter measurement type (column "ValueKindName")
#-----------------------------------------------------

# check different states in ValueKindName
data_filtered %>%
  group_by(ValueKindName) %>%
  summarize(n = n())
    
    #   ValueKindName               n
    #   <fct>                   <int>
    # 1 ""                   18243037
    # 2 Best estimate           29850
    # 3 Class mean               7036
    # 4 High                     3370
    # 5 Individual Mean          7309
    # 6 Low                      5291
    # 7 Maximum                 30839
    # 8 Maximum in plot         13618
    # 9 Mean                    52693
    # 10 Minimum                27914
    # 11 Plot mean               3043
    # 12 Single              12410577
    # 13 Site specific mean     39795
    # 14 Species mean            1879
    # 15 Sum                       42
    # 16 Upper 95 percentile       66
    # 17 NA                  41722198

# I would choose these states for continuous traits: "Mean", "Median", "Single", ("Species mean")

data_filtered <- data_filtered %>%
  filter(ValueKindName != "Best estimate" &
           ValueKindName != "Class mean" &
           ValueKindName != "High" &
           ValueKindName != "Low" &
           ValueKindName != "Maximum" &
           ValueKindName != "Maximum in plot" &
           ValueKindName != "Minimum" &
           ValueKindName != "Site specific mean" &
           ValueKindName != "Species mean" &
           ValueKindName != "Sum" &
           ValueKindName != "Upper 95 percentile" &
           ValueKindName != "Plot mean"|
           is.na(ValueKindName))


save(data_filtered, file = "C:/Users/Nutzer/Desktop/DF_filtered_before7.RData")


#---------------------------------------------------------------------------------------------------
# 7 - calculate mean for StdValue where the same Trait occurs within an ObservationID more than once
#---------------------------------------------------------------------------------------------------

# create 2 DFs: 
# 1 - contains ONLY trait data 
# 2 - contains ONLY additional data (exact counterpart to 1)
#-----------------------------------------------------------
data_all_DF_supplemental_or_categorical_traits <- data_filtered %>%
  filter(is.na(TraitName) | TraitName == "" | is.na(StdValue))

data_all_DF_numerical_traits <- data_filtered %>%
  filter(complete.cases(TraitName) & 
           TraitName != "" & 
           complete.cases(StdValue))

numerical_traits_mean <- data_all_DF_numerical_traits %>%
  group_by(DatasetID, AccSpeciesID, AccSpeciesName, AccSpeciesName_short, ObservationID, UnitName, TraitName_short) %>%
  summarize(StdValue_mean = mean(StdValue),
            n_trait_observations = n())  %>% # via number it is possible to investigate how many traits were merged
  ungroup()


# change names of columns
numerical_traits_mean <- numerical_traits_mean %>%
  select(DatasetID, AccSpeciesID, AccSpeciesName, AccSpeciesName_short, ObservationID, StdValue_mean, UnitName, TraitName_short) %>%
  rename(StdValue = "StdValue_mean")

# create columns, so DF can be merged again
col_names <- names(data_all_DF_numerical_traits)

missing_names <- setdiff(col_names, names(numerical_traits_mean))
numerical_traits_mean[missing_names] <- NA
numerical_traits_mean <- numerical_traits_mean[col_names]

rm(col_names, missing_names)


# comining both dataframes again
data_filtered_traits_merged <- bind_rows(numerical_traits_mean, data_all_DF_supplemental_or_categorical_traits)
data_filtered_traits_merged <- data_filtered_traits_merged %>%
  arrange(ObservationID)

rm(data_all_DF_numerical_traits, data_all_DF_supplemental_or_categorical_traits, numerical_traits_mean)

save(data_filtered_traits_merged, file = "C:/Users/Nutzer/Desktop/DF_filtered_before8.RData")


#-----------------------------------------------------------------------
# 8 - combine traits where info on Individual ID / Leaf ID are available
#-----------------------------------------------------------------------

data_filtered_traits_merged %>%
  distinct(DataName) %>% 
  arrange(DataName) %>%
  View("DataNames")


data_filtered_traits_merged %>%
  filter(DataName == "Branch ID") %>%
  View()

data_filtered_traits_merged %>%
  filter(ObservationID == 1035889 | ObservationID == 1036038) %>%
  View("Observation")
data_filtered_traits_merged %>%
  filter(ObservationID == 2749291) %>%
  View("Observation")

import_TRY %>%
  filter(ObservationID == 2752454) %>%
  View("Observation")

#----------------------------------------------------------------------------------------------------
# split DF into 2 sub-dataframes (DF with info on individual ID and DF without info on individual ID)
#----------------------------------------------------------------------------------------------------
# filter ObservationIDs that fit DataName == "Plant ID / Individual ID" | "Tree ID"
ObservationIDs_individual_ref <- data_filtered_traits_merged %>%
  filter(DataName == "Plant ID / Individual ID" | DataName == "Tree ID") %>%
  mutate(duplicate = ifelse(duplicated(ObservationID), T, F)) %>%
  filter(ObservationID != 2749291)
Observations_individual_ref <- data_filtered_traits_merged %>%
  filter(ObservationID %in% ObservationIDs_individual_ref$ObservationID)

rm(ObservationIDs_individual_ref)

# filter all other ObservationIDs
Observations_WO_individual_ref <- data_filtered_traits_merged %>%
  filter(!ObservationID %in% ObservationIDs_individual_ref$ObservationID)

#----------------------------------------------------------------------------------------------
# for ObservationIDs with Individual IDs: 
# create matrix with all DataNames that are neccessary for unique identification of individuals
#----------------------------------------------------------------------------------------------
individual_identification_matrix <- Observations_individual_ref %>%
  filter(DataName == "Location / Site Name" |
           DataName == "Location Site ID" |
           DataName == "Block ID" |
           DataName == "Plot ID" |
           DataName == "Subplot ID" |
           DataName == "Quadrat ID" |
           DataName == "Plant ID / Individual ID") %>%
  select(ObservationID, DatasetID, DataName, OrigValueStr) %>%
  distinct(ObservationID, DatasetID, DataName, OrigValueStr)

individual_identification_matrix <- spread(individual_identification_matrix, key = "DataName", value = "OrigValueStr")

# create list of unique individuals and assign unique ID
individual_names <- individual_identification_matrix %>%
  select(names(individual_identification_matrix)[-1]) %>% 
  distinct(.keep_all = T)
individual_names$IndividualID <- 1:nrow(individual_names)

# merge ObservationIDs and IndividualIDs
individual_identification_matrix <- individual_identification_matrix %>%
  left_join(individual_names, by = names(individual_identification_matrix)[-1]) %>%
  select(ObservationID, IndividualID)

data_filtered_traits_merged <- data_filtered_traits_merged %>%
  left_join(individual_identification_matrix, by = "ObservationID") %>%
  select(DatasetID:ObservationID, IndividualID, ObsDataID:Comment)

rm(individual_identification_matrix, individual_names, Observations_individual_ref, Observations_WO_individual_ref)




#-----------------------------------------------------------------------------------------------------
# transform data frame into format: 
# ObservationID, IndividualID, AccSpeciesName, TraitID, TraitName, StdValue, Unit, Latitude, Longitude
#-----------------------------------------------------------------------------------------------------
traits <- data_filtered_traits_merged %>%
  select(ObservationID, IndividualID, AccSpeciesName_short, TraitName_short, StdValue, UnitName) %>%
  filter(complete.cases(TraitName_short) &
           TraitName_short != "" & # exclude all entries with "" in TraitName
           complete.cases(StdValue))    # exclude potential categorical traits that don't have a StdValue

Latitude <- data_filtered_traits_merged %>%
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Latitude") %>%
  rename(Latitude = StdValue) %>%
  select(ObservationID, Latitude) %>%
  distinct(ObservationID, .keep_all = T)

Longitude <- data_filtered_traits_merged %>%
  select(ObservationID, DataName, StdValue) %>%
  filter(DataName == "Longitude") %>%
  rename(Longitude = StdValue) %>%
  select(ObservationID, Longitude) %>%
  distinct(ObservationID, .keep_all = T)

traits_x_georef <- traits %>%
  left_join(Latitude, by = "ObservationID") %>%
  left_join(Longitude, by = "ObservationID") %>%
  filter(complete.cases(AccSpeciesName_short))

rm(traits, Latitude, Longitude)


#--------------------------------------------
# calculate mean trait values for individuals
#--------------------------------------------
# create 2 subsets for mean calculation
# subset A: all observations with IndividualID
individuals_traits <- traits_x_georef %>%
  filter(complete.cases(IndividualID))

# subset B: all observations without IndividualID
Observations_traits <- traits_x_georef %>%
  filter(is.na(IndividualID))

# for individuals: group observations by all columns except ObservationID 
# calculate mean for individuals that have more than 1 measurement for a trait
individuals_traits_means <- individuals_traits %>%
  group_by(IndividualID, AccSpeciesName_short, TraitName_short, UnitName, Latitude, Longitude) %>%
  summarize(StdValue = mean(StdValue)) %>%
  ungroup() %>%
  mutate(ObservationID = NA) %>%
  select(names(individuals_traits))

# bind dataframes of trait values for Observations and trait values calculated for individuals
traits_x_georef_final <- Observations_traits %>%
  bind_rows(individuals_traits_means)



rm(individuals_traits, Observations_traits, individuals_traits_means)

write.csv(traits_x_georef_final, file = "traits_x_georef_final.csv")

#--------------------------------------------------------------------------------------
# filter species-trait combinations with at least 25 observations per species and trait
#--------------------------------------------------------------------------------------

#--------------------------------------------------
# A) geo-referenced and NON-geo-referenced together
#--------------------------------------------------
# create table with number of observations per species x trait combination
obs_species_x_traits <- data_filtered_traits_merged %>%
  group_by(AccSpeciesName_short, TraitName_short) %>%
  summarize(n_obs = n()) %>%
  filter(TraitName_short != "")

# create table with number of species per trait (species with at least 25 observations) 
traits_25_obs_per_spec <- obs_species_x_traits %>%
  filter(n_obs >= 25) %>%
  group_by(TraitName_short) %>%
  summarize(n_spec = n())

rm(obs_species_x_traits)

#---------------------------------------------
# B) select only geo-referenced ObservationIDs
#---------------------------------------------
# DataID 59 - Latitude, DataID 60 - Longitude

georef <- data_filtered %>%
  filter(DataID == 59|
           DataID == 60)
georef <- unique(georef$ObservationID)

data_filtered$georef <- data_filtered$ObservationID %in% georef
data_filtered_georef <- data_filtered[data_filtered$georef == T, -18]
data_filtered <- data_filtered[, -18]

# create table with number of observations per species x trait combination
obs_species_x_traits <- data_filtered_georef %>%
  group_by(AccSpeciesName_short, TraitName) %>%
  summarize(n_obs = n()) %>%
  filter(TraitName != "")

# create table with number of species per trait (species with at least 25 observations) 
traits_25_georef_obs_per_spec <- obs_species_x_traits %>%
  filter(n_obs >= 25) %>%
  group_by(TraitName) %>%
  summarize(n_spec_georef = n())

rm(georef, obs_species_x_traits)

# combine tables with 25 observations per trait and species (all observations, georef observations)
traits_25_obs_per_spec <- left_join(traits_25_obs_per_spec, traits_25_georef_obs_per_spec, by = "TraitName") 
rm(traits_25_georef_obs_per_spec)

write.csv(traits_25_obs_per_spec, file = "n_species_per_trait.csv")
write.table(traits_25_obs_per_spec, file = "n_species_per_trait.txt")


#---------------------------------------------------------------------------------------------------------------------
# Build dataset for sensitivity Analysis (based on species trait combinations with at least 500 georeferenced entries)
#---------------------------------------------------------------------------------------------------------------------

georef <- data_filtered %>%
  filter(DataID == 59|
           DataID == 60)
georef <- unique(georef$ObservationID)

data_filtered$georef <- data_filtered$ObservationID %in% georef
data_filtered_georef <- data_filtered[data_filtered$georef == T, -18]
data_filtered <- data_filtered[, -18]

# create table with number of observations per species x trait combination
obs_species_x_traits <- data_filtered_georef %>%
  group_by(AccSpeciesName_short, TraitName) %>%
  summarize(n_obs = n()) %>%
  filter(TraitName != "")

obs_species_x_traits_500 <- obs_species_x_traits %>% 
  filter(n_obs>500)

# reduce data_filtered_georef to species trait combinations with more than 500 observations
include <- inner_join(data_filtered_georef, obs_species_x_traits_500[,1:2], by = c("AccSpeciesName_short", "TraitName"))
include < -unique(include$ObsDataID)

data_filtered_georef_n500 < -data_filtered_georef
Longitude <- data_filtered_georef_n500 %>% 
  filter(DataName=="Longitude") %>% 
  select("ObservationID", "StdValue") %>% 
  distinct(ObservationID, StdValue)
names(Longitude) <- c("ObservationID","Longitude")

Latitude <- data_filtered_georef_n500 %>%
  filter(DataName=="Latitude") %>%
  select("ObservationID", "StdValue") %>%
  distinct(ObservationID, StdValue)
names(Latitude) <- c("ObservationID","Latitude")

data_filtered_georef_n500 <- left_join(data_filtered_georef_n500, Longitude, by = "ObservationID")
data_filtered_georef_n500 <- left_join(data_filtered_georef_n500, Latitude, by = "ObservationID")

data_filtered_georef_n500$include <- data_filtered_georef_n500$ObsDataID %in% include
data_filtered_georef_n500 <- data_filtered_georef_n500[data_filtered_georef_n500$include == T, -20]

rm(georef, obs_species_x_traits, Latitude, Longitude, include, obs_species_x_traits_500)

data_filtered_georef_n500

save(data_filtered_georef_n500, file = "data_filtered_georef_n500_vorübergehend.RData") 
