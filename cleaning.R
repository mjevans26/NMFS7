library(digest)
library(dplyr)
library(tidyr)
library(stringr)

setwd("C:/Users/mevans/repos/NMFS7/data")
file1 <- read.csv("raw/FY2001_FY2017_filea.csv", header = TRUE, sep = ",")
file2 <- read.csv("raw/FY2001_FY2017_fileb.csv", header = TRUE, sep = ",")
file3 <- read.csv("raw/FY73_FY2000.csv", header = TRUE, sep = ",")

cr_hab <- read.csv("data/raw/CH_ECOS.csv", header = TRUE, sep = ",")

new <- rbind(file1, file2, file3)

#corals have populations named, but these correspond to entire ranges
new$Population.Name[grepl("coral", new$Common.Name, ignore.case = TRUE) & new$Population.Name != ""] <- "Range-wide"
#two species of right whale were occassionally distinguished in the population.name field
new[new$Common.Name == "Whale, right" & new$Population.Name == "North Atlantic",c(17,18)]<- c("Whale, North Atlantic right", "")
new[new$Common.Name == "Whale, right" & new$Population.Name == "North Pacific",c(17,18)]<- c("Whale, North Pacific right", "")

new$Population <- new$Population.Name
new$Population[is.na(new$Population)] <- "Range-wide"
new$Population[new$Population == ""] <- "Range-wide"

#Add columns for separated, corrected CH and Spp determinations
new$CH <- NA
new$Sp <- NA
new$CH_cons <- NA
new$Spp_cons <- NA

##RECODING JAM SCORING
thing <- sapply(1:nrow(new), function(i){
  old_spp <- new$NMFS.Final.Determination..Species.[i]
  old_ch <- new$NMFS.Final.Determination..Critical.Habitat.[i]
  if(old_spp == "No Jeopardy, No Adverse Mod" & old_ch == "Adverse Modification"){
    ch <- "No Adverse Modification"
    sp <- "No Jeopardy"
    #1
  }else if(old_spp == "Adverse Mod, No Jeopardy" & old_ch == "Adverse Modification"){
    ch <- "Adverse Modification"
    sp <- "No Jeopardy"
    #2
  }else if (old_spp == "Jeopardy & Adverse Modificaiton" & old_ch == "Adverse Modification"){
    ch <- "Adverse Modification"
    sp <- "Jeopardy"
    #3
  }else if (old_spp == "Jeopardy & Adverse Modification" & old_ch == "Jeopardy & Adverse Modification"){
    ch <- "Adverse Modification"
    sp <- "Jeopardy"
    #4
  }else if (old_spp == "Jeopardy, No Adverse Mod" & old_ch == "Jeopardy, No Adverse Mod"){
    ch <- "No Adverse Modificaiton"
    sp <- "Jeopardy"
    #5
  }else if (old_spp == "Jeopardy, No Adverse Mod" & old_ch == "May Affect - Not Likely to Adversely Affect"){
    ch <- "No Adverse Modification"
    sp <- "Jeopardy"
    #6
  }else if (old_spp == "Jeopardy, No Adverse Mod" & old_ch == "No Adverse Modification"){
    ch <- "No Adverse Modificaiton"
    sp <- "Jeopardy"
    #7
  }else if (old_spp == "No Jeopardy, No Adverse Mod" & old_ch == "May Affect - Not Likely to Adversely Affect"){
    ch <- "No Adverse Modification"
    sp <- "No Jeopardy"
    #8
  }else if (old_spp == "No Jeopardy, No Adverse Mod" & old_ch == "No Adverse Modification"){
    ch <- "No Adverse Modificaiton"
    sp <- "No Jeopardy"
    #9
  }else if (old_spp == "Not Likely to Adversely Affect" & old_ch == "Adverse Modification"){
    ch <- "Adverse Modification"
    sp <- "NLAA"
    #10
  }else if (old_spp == "Not Likely to Adversely Affect" & old_ch == "May Affect - Not Likely to Adversely Affect"){
    ch <- "NLAA"
    sp <- "NLAA"
    #11
  }else if (old_spp == "Not Likely to Adversely Affect" & old_ch == "No Adverse Modification"){
    ch <- "No Adverse Modification"
    sp <- "NLAA"
    #12
  }else if (old_spp == "Not Likely to Adversely Affect" & old_ch == "No Effect"){
    ch <- "No Effect"
    sp <- "NLAA"
    #16
  }else if (old_spp == "Withdrawn" & old_ch == "Adverse Modification"){
    ch <- "Adverse Modification"
    sp <- NA
    #17
  }else if (old_spp == "Withdrawn" & old_ch == "May Affect - Not Likely to Adversely Affect"){
    ch <- "NLAA"
    sp <- NA
    #18
  }else if (old_spp == "Withdrawn" & old_ch == "No Adverse Modification"){
    ch <- "No Adverse Modification"
    sp <- NA
    #19
  }else if (old_spp == "Withdrawn" & old_ch == "No Effect"){
    ch <- "No Effect"
    sp <- NA
    #20
  }else if (old_spp == "No Jeopardy, No Adverse Mod" & old_ch == "No Effect"){
    ch <- NA
    sp <- "No Jeopardy"
  }else {
    ch <- NA
    sp <- NA
  }
  return(c(sp, ch))
})

###CORRECT DATES
#dates is csv file pulled from all records where repsonse date is before initiation date
dates <- read.csv(file = 'data/helper/dates.csv', header = TRUE)
dates$ConsultationInitiation <- vapply(1:nrow(dates), function(i){
  text <- str_split_fixed(dates$start.1[i], "/", n = 3)
  if(nchar(text[1,1]) == 1){text[1,1] <- paste("0", text[1,1], sep = "")}
  if(nchar(text[1,2]) == 1){text[1,2] <- paste("0", text[1,2], sep = "")}
  return(paste(text[1,c(3,1,2)], collapse = "-"))
  }, USE.NAMES = FALSE, FUN.VALUE = character(1))


for(i in 1:nrow(dates)){
  tn <- dates$tracking[i]
  new[new$NMFS.Tracking.Number == tn, 52:53] <- dates[i, 8:9]
}

new$Start <- as.character(new$Consultation.Initiation.Date)
new$End <- as.character(new$NMFS.Response.Date)

#create unique combos of Common.Name & Population
unique(select(new, Common.Name, Population))

#there are many records where Common.Name == ""...
blanklist <- unique(new$NMFS.Tracking.Number[new$Common.Name == ""])
goodlist <- unique(new$NMFS.Tracking.Number[grepl(".", new$Common.Name)])

blank_key <- read.csv(file = "data/BlankKey.csv", header = TRUE, na.strings = "N/A", stringsAsFactors = FALSE)

blank_fix <- lapply (unique(blank_key$NMFS.Tracking.Number), function(i){

  left <- filter(blank_key, NMFS.Tracking.Number == i)[,1:3]
  right <- filter(new, NMFS.Tracking.Number == i)
  df <- left_join(left, right, by = c("NMFS.Tracking.Number", "NMFS.Final.Determination..Species.", "NMFS.Final.Determination..Critical.Habitat."))
  df$Common.Name <- blank_key$Common.Name[blank_key$NMFS.Tracking.Number == i]
  df$Population.Name <- blank_key$Population.Name[blank_key$NMFS.Tracking.Number == i]
  df$Population <- blank_key$Population.Name[blank_key$NMFS.Tracking.Number == i]

  return(df)
})%>%bind_rows()

test <- bind_rows(blank_fix, filter(new, !NMFS.Tracking.Number %in% blank_key$NMFS.Tracking.Number))

doublelist <- unique(test$NMFS.Tracking.Number[duplicated(select(test, NMFS.Tracking.Number, Common.Name))])
blanklist <- unique(test$NMFS.Tracking.Number[test$Common.Name == ""])
goodlist <- unique(test$NMFS.Tracking.Number[grepl(".", test$Common.Name)])

filter(test, NMFS.Tracking.Number %in% blanklist)%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(count = n())%>%
  arrange(desc(count))%>%write.csv(file = "data/blankcounts2.csv")

#note, goodlist != inv(blanklist)
#so now keep records with a TN that never has a spp populated..
test2 <- bind_rows(
filter(test, !NMFS.Tracking.Number %in% goodlist),
#...and those with a TN for which spp is never blank...
filter(test, !NMFS.Tracking.Number %in% blanklist),
#...and those with a TN that has a mixture, but only those with spp populated
filter(test, NMFS.Tracking.Number %in% blanklist, grepl(".", Common.Name))
)

blanklist <- unique(test2$NMFS.Tracking.Number[test2$Common.Name == ""])
goodlist <- unique(test2$NMFS.Tracking.Number[grepl(".", test2$Common.Name)])

filter(test2, NMFS.Tracking.Number %in% blanklist)%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(count = n())%>%
  #filter(count>1)
  arrange(desc(count))%>%write.csv(file = "data/blankcounts2.csv")

#seemed to work, now need to eliminate double blanks
doublelist <- unique(test2$NMFS.Tracking.Number[duplicated(select(test2, NMFS.Tracking.Number, Common.Name, Population.Name))])
blanklist <- unique(test2$NMFS.Tracking.Number[test2$Common.Name == ""])
goodlist <- unique(test2$NMFS.Tracking.Number[grepl(".", test2$Common.Name)])

test3 <- bind_rows(
  filter(test2, !NMFS.Tracking.Number %in% doublelist),
  filter(test2, NMFS.Tracking.Number %in% doublelist, grepl(".", NMFS.Final.Determination..Species.))
)
#checks out, nrow(test2) - nrow(test3) = previous number of records with double blanks
#and the following should not produce any counts > 2
blanklist <- unique(test3$NMFS.Tracking.Number[test3$Common.Name == ""])
goodlist <- unique(test3$NMFS.Tracking.Number[grepl(".", test3$Common.Name)])

filter(test3, NMFS.Tracking.Number %in% blanklist)%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize(count = n())%>%
  #filter(count>1)
  arrange(desc(count))%>%write.csv(file = "data/blankcounts2.csv")


#Resolve confusing/contradictory determination combos
#Mass key presents crosswalk for all records meeting certain conditions
mass_key <- read.csv(file = "data/helper/MassKey.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = 'N/A')

good_data <- bind_rows(lapply(1:nrow(mass_key), function(i){
  spp <- mass_key$Jeopardy[i]
  ch <- mass_key$CriticalHabitat[i]
  reg <- mass_key$Region[i]
  if (grepl(".", reg)){
    rows <- filter(test3, NMFS.Final.Determination..Species. == spp,
                   NMFS.Final.Determination..Critical.Habitat. == ch,
                   grepl(reg, NMFS.Lead.Region)
    )
  }else{
    rows <- filter(test3, NMFS.Final.Determination..Species. == spp,
                   NMFS.Final.Determination..Critical.Habitat. == ch
    )
  }

  if(nrow(rows) > 0){
    rows$CH <- mass_key$CH[i]
    rows$Sp <- mass_key$Spp[i]
    rows$CH_cons <- mass_key$CH_cons[i]
    rows$Spp_cons <- mass_key$Spp_cons[i]
  }

  return(rows)
})
)

#Update conclusions with findings from spot checking
spot_key <- read.csv(file = "data/SpotKey.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = 'NA')

for(i in 1:nrow(spot_key)){
  print(i)
  tn <- spot_key$TrackingNumber[i]
  spp <- spot_key$Common.Name[i]
  pop <- spot_key$Population.Name[i]

  if(is.na(pop)){
    good_data[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == spp & is.na(good_data$Population.Name), 42:45] <- spot_key[i ,6:9]
  }else{
    good_data[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == spp & good_data$Population.Name == pop, 42:45] <- spot_key[i ,6:9]
  }
}

good_data$Action.Agency.Proposed.Effect.Determination..Species.[good_data$Action.Agency.Proposed.Effect.Determination..Species. == "May Adversely Affect"] <- "Likely to Adversely Affect"
table(good_data$Consultation.Type, good_data$Sp)

##ADDING ACTION AGENCY DATA
agencies <- bind_rows(
  read.csv(file = "file1 clean.csv", header = TRUE)[,1:2],
  read.csv(file = "file2 clean.csv", header = TRUE)[,1:2],
  read.csv(file = "file3 clean.csv", header = TRUE)[,1:2],
  read.csv(file = "file4 clean.csv", header = TRUE)[,1:2],
  read.csv(file = "file5 clean.csv", header = TRUE)[,1:2],
  read.csv(file = "file6_clean.csv", header = TRUE)[,1:2]
)

agencies <- group_by(agencies, NMFS.Tracking.Number)%>%summarize(Agency = first(Lead.Federal.Action.Agency))
test <- left_join(good_data, agencies, by = "NMFS.Tracking.Number")

#Standardize agency names with FWS data and add parent Department
agency_key <- read.csv(file = "data/helper/agencykey.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

for (i in 1:nrow(agency_key)){
  print(i)
  #test$Agency[test$Agency == agency_key$Original[i]] <- agency_key$New[i]
  good_data$Parent[good_data$Agency == agency_key$New[i]] <- agency_key$Parent[i]
}

##ADDING WORK SUB-CATEGORY DATA PROVIDED BY NMFS
worktype <- read.csv(file= "data/raw/WorkCategories.csv", header = TRUE)[,1:3]
worktype <- bind_cols(lapply(1:ncol(worktype), function(i){as.character(worktype[,i])}))
worktype$Subcateogry<- as.character(worktype$Subcateogry)

test2 <- left_join(test, types, by = "NMFS.Tracking.Number")

worktype <- filter(worktype, grepl(".", Category.of.Activity))
for (i in 1:nrow(worktype)){
  good_data$Category[good_data$NMFS.Tracking.Number == worktype$NMFS.Tracking.Number[i]] <- worktype$Category.of.Activity[i]
  good_data$SubCategory[good_data$NMFS.Tracking.Number == worktype$NMFS.Tracking.Number[i]] <- worktype$Subcateogry[i]
  }

good_data$Category[good_data$Category == "Forest Management (Retired)"] <- "Forestry"
good_data$Category[good_data$Category == "Construction - Other (Retired)"] <- "Construction"

##ADDING CRITICAL HABITAT DATA
combos <- read.csv(file = "data/output/combos.csv", na.strings = "NA", stringsAsFactors = FALSE)
combos$CriticalHabitat <- as.Date(combos$CriticalHabitat, format = "%m/%d/%Y")
for (i in 1:nrow(combos)){
  sp <- combos$Common.Name[i]
  pop <- combos$Population[i]
  ch <- combos$CriticalHabitat[i]
  if (is.na(ch)){
    #good_data$CH[good_data$Common.Name == sp & good_data$Population == pop] <- "No CH"
    good_data$CH_cons[good_data$Common.Name == sp & good_data$Population == pop] <- "No CH"
  } else {
    #good_data$CH[good_data$Common.Name == sp & good_data$Population == pop & as.Date(good_data$Date.Request.for.Consultation.Received, format = "%b %d, %Y") < ch] <- "No CH"
    good_data$CH_cons[good_data$Common.Name == sp & good_data$Population == pop & as.Date(good_data$NMFS.Response.Date, format = "%b %d, %Y") < ch] <- "No CH"
  }
}

##Enter 'No CH' for species that have never had CH, nor have DPS with CH
chspecies <- unique(combos$Common.Name[!is.na(combos$CriticalHabitat)])
chspecies <- c(chspecies, "")
good_data$CH_cons[!good_data$Common.Name %in% chspecies] <- "No CH"
good_data$CH[!good_data$Common.Name %in% chspecies] <- "No CH"


#EPA pesticide biops don't spell out DPS for salmonids.  Need to confirm admod manually
good_data$CH_cons[good_data$NMFS.Tracking.Number == "FPR-2003-2430"]<- "Adverse Modification"
good_data$CH_cons[good_data$NMFS.Tracking.Number == "FPR-2002-2724"]<- "Adverse Modification"
good_data$CH_cons[good_data$NMFS.Tracking.Number == "FPR-2002-1905"]<- "Adverse Modification"

#Enter 'No CH' for DPS/ESUs that have never had ch
for (i in 1:nrow(combos)){
  sp <- combos$Common.Name[i]
  pop <- combos$Population[i]
  ch <- combos$CriticalHabitat[i]
  if (is.na(ch) & pop != "Range-wide"){
    good_data$CH[good_data$Common.Name == sp & good_data$Population == pop] <- "No CH"
    good_data$CH_cons[good_data$Common.Name == sp & good_data$Population == pop] <- "No CH"
  }
}

table(good_data$CH_cons, good_data$CH)

saveRDS(good_data, file = "data/good_data.rds")

##Updated Jeop/AdMod Calls Based on Hand-Corrected Data
jeopardies <- read.csv(file = "data/helper/Jeopardies.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
ammendmendts <- filter(jeopardies, BiOp == "Y", NMFSJeopardy != ""| NMFSCriticalHabitat != "")
for(i in 1:nrow(ammendmendts)){
  sp <- ammendmendts$Common.Name[i]
  pop <- ammendmendts$Population.Name[i]
  tn <- ammendmendts$TrackingNumber[i]
  newpop <- ammendmendts$Population[i]
  if(nrow(good_data[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop, 42:45]) == 2){
    print(good_data$Population.Name[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop])
  }
  #print(ammendmendts[i,6:9])
  #good_data$CH[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- ammendmendts$AdMod[i]
  #good_data$Sp[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- ammendmendts$Jeopardy[i]
  }

###UPDATE ACTION AGENCY DETERMINATIONS FOR JEOPARDY BIOPS

for(i in 1:nrow(jeopardies)){

  if (grepl(".", jeopardies$ProposedSP[i])){
    sp <- jeopardies$Common.Name[i]
    pop <- jeopardies$Population.Name[i]
    tn <- jeopardies$TrackingNumber[i]
    #newpop <- jeopardies$Population[i]
    good_data$Action.Agency.Proposed.Effect.Determination..Species.[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- jeopardies$ProposedSP[i]
    good_data$Action.Agency.Proposed.Effect.Determination..Critical.Habitat.[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- jeopardies$ProposedCH[i]
    good_data$CH[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- jeopardies$AdMod[i]
    good_data$Sp[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- jeopardies$Jeopardy[i]
    good_data$CH_cons[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- jeopardies$CH_cons[i]
    good_data$Spp_cons[good_data$NMFS.Tracking.Number == tn & good_data$Common.Name == sp & good_data$Population.Name == pop] <- jeopardies$Spp_cons[i]
    }
}

newrecords <- filter(jeopardies, BiOp == "Y", NMFSJeopardy == "", NMFSCriticalHabitat == "")%>%
  select(TrackingNumber, Common.Name, Population.Name, NMFSJeopardy, NMFSCriticalHabitat, AdMod, Jeopardy, CH_cons, Spp_cons, RPA, WorkType)

newrecords$Population <- newrecords$Population.Name

newrecords <- select(good_data, -Common.Name, -Population.Name, -Population, -NMFS.Final.Determination..Species., -NMFS.Final.Determination..Critical.Habitat., -CH, -Sp, -CH_cons, -Spp_cons, -Action.Agency.Proposed.Effect.Determination..Species., -Action.Agency.Proposed.Effect.Determination..Critical.Habitat.)%>%
  group_by(NMFS.Tracking.Number)%>%
  summarize_all(first)%>%
  right_join(newrecords, by = c("NMFS.Tracking.Number" = "TrackingNumber"))


newrecords <- left_join(newrecords, types, by = "NMFS.Tracking.Number")
newrecords <- left_join(newrecords, agencies, by = "NMFS.Tracking.Number")

for (i in 1:nrow(agency_key)){
  print(i)
  newrecords$Agency[newrecords$Agency == agency_key$Original[i]] <- agency_key$New[i]
  newrecords$Parent[newrecords$Agency == agency_key$Original[i]] <- agency_key$Parent[i]
}

#newrecords almost ready to rbind with good_data...need to fill in proposed determinations
good_data <- bind_rows(good_data, newrecords)

#State is blank for all records
good_data$State <- trimws(str_split_fixed(good_data$NMFS.Office, ", | - ", n = 3)[,2])


filter(good_data, Sp == "Jeopardy", is.na(Action.Agency.Proposed.Effect.Determination..Species.))%>%
  select(NMFS.Tracking.Number, Common.Name, Population.Name)

filter(good_data, Sp == "Jeopardy", Action.Agency.Proposed.Effect.Determination..Species. == "")%>%
  select(NMFS.Tracking.Number, Common.Name, Population.Name)

#CROSSWALK NMFS CONCLUSION CODING TO DISCREPANCY SCORES
scorekey <- read.csv(file = "data/helper/scorekey.csv", header = TRUE)
good_data$Discrepancy <- NA
for(i in 1:nrow(scorekey)){
  score <- scorekey$Score[i]
  sp <- scorekey$Sp[i]
  psp <- scorekey$Action.Agency.Proposed.Effect.Determination..Species.[i]
  good_data$Discrepancy[good_data$Sp == sp & good_data$Action.Agency.Proposed.Effect.Determination..Species. == psp & !is.na(good_data$Sp) & !is.na(good_data$Action.Agency.Proposed.Effect.Determination..Species.)] <- score
}

good_data$Discrepancy <- as.factor(good_data$Discrepancy)

##PREPARE RANGE OVERLAP DATA FOR PACIFIC SALMONIDS
dist <- read.csv(file = "data/helper/Salmonidoverlap.csv", header = TRUE, stringsAsFactors = FALSE)
dist$Species <- unlist(str_extract_all(dist$DPS1, "((Coho|Chinook|Chum|Sockeye) Salmon)|Steelhead"))
dist$Species2 <- unlist(str_extract_all(dist$DPS2, "((Coho|Chinook|Chum|Sockeye) Salmon)|Steelhead"))
dist$Pop1 <- unlist(str_remove_all(dist$DPS1, dist$Species))
dist$Pop2 <- unlist(str_remove_all(dist$DPS2, dist$Species2))
dist <- dist[dist$Area > 0,]
##remove duplicates
cols <- c(1, 2)
newdist <- dist[,cols]
for (i in 1:nrow(dist)){
  newdist[i,] <- sort(dist[i,cols])}

for (i in 1:nrow(dist)){
  if (!duplicated(newdist)[i]){
    dist$Area[i] <- max(dist$Area[dist$FID1 == dist$FID1[i] & dist$FID2 == dist$FID2[i]] & duplicated(newdist))
  }
}
dist <- dist[!duplicated(newdist),]
rm(newdist, cols)

dist <- as.data.frame(
  group_by(dist, DPS1, DPS2)%>%
    summarize(AREA = sum(Area), AREA1 = sum(unique(AREA1)), AREA2 = sum(unique(AREA2)),
              Species = first(Species), Species2 = first(Species2), Pop1 = first(Pop1), Pop2 = first(Pop2))%>%
    mutate(Overlap = AREA/(AREA1+AREA2-AREA))
)

dist <- mutate(dist, sp2_name = paste(str_split(dist$Species2, " ", n = 2, simplify = TRUE)[,2], ", ", str_split(dist$Species2, " ", n = 2, simplify = TRUE)[,1], " (", dist$Pop2, ")", sep = ""))
dist <- mutate(dist, sp1_name = paste(str_split(dist$Species, " ", n = 2, simplify = TRUE)[,2], ", ", str_split(dist$Species, " ", n = 2, simplify = TRUE)[,1], " (", dist$Pop1, ")", sep = ""))
dist$sp1_name <- unlist(str_replace_all(dist$sp1_name, ", Steelhead", "Steelhead"))
dist$sp2_name <- unlist(str_replace_all(dist$sp2_name, ", Steelhead", "Steelhead"))
dist$sp1_name <- unlist(str_replace_all(dist$sp1_name, " \\)", ")"))
dist$sp2_name <- unlist(str_replace_all(dist$sp2_name, " \\)", ")"))

for(i in 1:nrow(dist)){dist[i,11:12] <- sort(dist[i,11:12])}
