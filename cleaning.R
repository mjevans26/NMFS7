library(digest)
library(dplyr)
library(tidyr)
library(stringr)
setwd("C:/Users/mevans/repos/NMFS7")
file1 <- read.csv("FY2001_FY2017_filea.csv", header = TRUE, sep = ",")
file2 <- read.csv("FY2001_FY2017_fileb.csv", header = TRUE, sep = ",")
file3 <- read.csv("FY73_FY2000.csv", header = TRUE, sep = ",")

cr_hab <- read.csv("data/CH_ECOS.csv", header = TRUE, sep = ",")

temp <- rbind(file1, file2, file3)

newSpecies <- as.character(unique(new$Species.Involved...Evaluated))
fullSpecies <- unique(unlist(full$spp_ev_ls))
inFull <- newSpecies %in% full$Species

fullData <- data.frame(species = fullSpecies)
fullData <- mutate(fullData, scientific = vapply(species, function(x) {str_extract_all(x, "\\([^()]+\\)")[[1]][1]}, FUN.VALUE = "chr", USE.NAMES = FALSE))

fullData$scientific[grepl("pocket gopher",fullData$scientific)] <- c("(Thomomys mazama pugetensis)", "(Thomomys mazama yelmensis)", "(Thomomys mazama tumuli)", "(Thomomys mazama)", "(Thomomys mazama glacialis)")

newData <- data.frame(species = newSpecies)
newData <- mutate(newData, scientific = vapply(species, function(x) {str_extract_all(x, "\\([^()]+\\)")[[1]][1]}, FUN.VALUE = "chr", USE.NAMES = FALSE))
comb <- left_join(newData, fullData, by = "scientific")
comb$species.x <- as.character(comb$species.x)
comb$species.y <- as.character(comb$species.y)
comb$species.y[comb$species.x == "Western burrowing owl (Athene cunicularia ssp. hypugaea)"] <- "Owl, western burrowing (Athene cunicularia hypugaea)"
comb <- comb[comb$species.x!="", ]
comb$species.y[comb$species.x == "haha (Cyanea crispa)"] <- "haha (Cyanea crispa)"

comb$species.y <- sapply(1:nrow(comb), function(x, y, z) {
  if(is.na(y[x, 3])){
    tryCatch({y[x, 3] <- agrep(y[x, 1], z, max.distance = 0.25, ignore.case = TRUE, value = TRUE)},
    warning = function(w){return(NA)},
    error = function(e){return(NA)})
  }else{
    y[x, 3] <- y[x, 3]
  }
}, y = comb, z = fullSpecies, USE.NAMES = FALSE)

info <- function(x){
i <- grep(x, fullSpecies, ignore.case = TRUE, value = TRUE)
j <- grep(x, comb$species.x, ignore.case = TRUE, value = TRUE)
k <- grep(x, comb$species.x, ignore.case = TRUE)
return(c("Old",i,"New",j,k))
}

repl <- function(i, j = NULL){
  if(!is.null(j)){
    comb$species.y[i] <- j
  }else{
    comb$species.y[i] <- comb$species.x[i]}
}

#find species for which join failed
filter(comb, is.na(species.y))
#find species that have '(=name)' in scientific name, that may have screwed up joining
filter(comb, grepl("\\(=[A-Za-z]+\\)", scientific))
#final check, length of unique should equal total nrow
length(unique(comb$species.x))
#
comb[duplicated(comb$species.x),]


comb$species.y[1448] <- "Mouse, Santa Rosa beach (Peromyscus polionotus leucocephalus)"
comb$species.y[1440] <- "Tiger beetle, cobblestone (Cicindela marginipennis)"
comb$species.y[1380] <- "Bear, Florida black (Ursus Americanus floridanus)"
comb$species.y[1379] <- "Butterfly, Florida leafwing (Anaea troglodyta floridalis)"
comb$species.y[1431] <- comb$species.x[1431]
comb$species.y[1432] <- "Loosestrife, curtiss (Lythrum curtissii)"
comb$species.y[1339] <- comb$species.x[1339]
comb$species.y[867] <- "Pocket gopher, Roy prairie (Thomomys mazama glacialis)"
comb$species.y[1434] <- "Flax, wests (linum westii)"

new$Biological.Opinion.Species <- as.character(new$Biological.Opinion.Species)
new$Species.Involved...Evaluated <- as.character(new$Species.Involved...Evaluated)
new$Biological.Conclusion.Determination <- as.character(new$Biological.Conclusion.Determination)
new$Critical.Habitat.Biological.Conclusion.Determination <- as.character(new$Critical.Habitat.Biological.Conclusion.Determination)
new$Action.Work.Type <- as.character(new$Action.Work.Type)

for (i in 1:nrow(comb)){
  new$Species.Involved...Evaluated[new$Species.Involved...Evaluated == comb$species.x[i]] <- comb$species.y[i]
  new$Biological.Opinion.Species[new$Biological.Opinion.Species == comb$species.x[i]] <- comb$species.y[i]
}

#convert species names to lower case for spp_BO_ls, remove commas from findings
new$Biological.Conclusion.Determination <- str_replace(new$Biological.Conclusion.Determination, ",", "")
new$Critical.Habitat.Biological.Conclusion.Determination <- str_replace(new$Critical.Habitat.Biological.Conclusion.Determination, ",", "")
new$Biological.Opinion.Species <- str_to_lower(new$Biological.Opinion.Species)
new$Action.Work.Type <- str_to_lower(new$Action.Work.Type)


new$Fiscal.Year <- str_extract(new$Start, "[0-9]{4}")
new$FY.Start <- str_extract(new$Start, "[0-9]{4}")
new$FY.End <- str_extract(new$End, "[0-9]{4}")

new$Type <- vapply(1:nrow(new), function(x){
  text <- new$Consultation.Type[x]
  if (grepl("Formal Emergency", text)){
    type <- "Formal Emergency Consultation"
  } else if (grepl("Informal Emergency", text)){
    type <- "Informal Emergency Consultation"
  } else if (grepl("Combined", text)){
    type <- "Combined Consultation"
  } else if (grepl("Formal$|Formal [^Em]", text)){
    type <- 'Formal Consultation'
  } else if (grepl("Informal$|Informal .[^m]", text)){
    type <- "Informal Consultation"
  } else if (grepl("Formal$|Formal .[^m]", text)){
    type <- "Formal Consultation"
  }
  return(type)

}, FUN.VALUE = character(1), USE.NAMES = FALSE)

new$Complex <- vapply(1:nrow(new), function(x){
  text <- new$Consultation.Type[x]
  if (grepl("Programmatic", text)){
    comp <- "Programmatic"
  } else if (grepl("Conference", text)){
    comp <- "Conference"
  } else if (grepl("Early", text)){
    comp <- "Early"
  } else if (grepl("Combined|formal$|formal Em", text, ignore.case = TRUE)){
    comp <- "Standard"
  }
  return(comp)

}, FUN.VALUE = character(1), USE.NAMES = FALSE)


new$Timely <- vapply(1:nrow(new), function(x){
  if(as.Date(new$Estimated.Response.Date[1], format = "%Y-%m-%d") >
       as.Date(new$NMFS.Response.Date[1], format = "%Y-%m-%d"))
    {"Yes"}
  else if (as.Date(new$Estimated.Response.Date[1], format = "%Y-%m-%d") <
             as.Date(new$NMFS.Response.Date[1], format = "%Y-%m-%d"))
  {"No"}
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

new$Elapsed.Days <- vapply(1:nrow(new), function(x){
  days <- as.numeric(as.Date(new$End[x], format = "%Y-%m-%d") - as.Date(new$Start[x], format = "%Y-%m-%d"))
  return(days)
}, FUN.VALUE = numeric(1), USE.NAMES = FALSE)
#select(comb, species.x, species.y)%>%
  #right_join(new, by = c("Species.Involved...Evaluated" = "species.x"))

test <- group_by(new, NMFS.Tracking.Number)%>%
  summarise(activity_code = first(NMFS.Tracking.Number),
            region = first(NMFS.Lead.Region),
            state = first(State),
            ESOffice = first(NMFS.Office),
            title = first(TITLE),
            lead_agency = first(Lead.Federal.Action.Agency),
            consult_type = first(Consultation.Type),
            consult_complex = first(Complex),
            FY = first(Fiscal.End),
            FY_start = first(FY.Start),
            FY_concl = first(FY.End),
            start_date = first(Date.Request.for.Consultation.Received),
            date_formal_consult = first(Consultation.Initiation.Date),
            due_date = first(Estimated.Repsonse.Date),
            FWS_concl_date = first(NMFS.Response.Date),

            elapsed = first(Elapsed.Days),
            date_active_concl = first(NMFS.Response.Date),
            #date_active_concl = str_extract(first(Active.Concluded), "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
            timely_concl = first(Timely),
            #hours_logged = first(Staff.Hours.Logged.Current.FY),
            #events_logged = first(Events.Logged.Current.FY),

            #work_type = paste(unique(Action.Work.Type), collapse = " - "),
            #ARRA = first(ARRA.Fund),
            datum = first(Datum),
            lat = first(Latitude),
            long = first(Longitude),
            spp_ev_ls = list(unique(paste(Common.Name, Population, sep = ", "))),
            spp_BO_ls = paste0(unique(paste(Biological.Opinion.Species[Biological.Opinion.Species!=""], ": BO = ", Biological.Conclusion.Determination[Biological.Opinion.Species!=""], "; CH = ", sep = "")), collapse = ", "),
            n_spp_eval = length(unique(Species.Involved...Evaluated)),
            n_spp_BO = length(unique(Biological.Opinion.Species)),
            n_nofx = length(grep("No Effect", NMFS.Final.Determination..Species.)) + length(grep("No Effect", NMFS.Final.Determination..Critical.Habitat.)),
            n_NLAA = length(grep("Not Likely to Adversely Affect", NMFS.Final.Determination..Species.)) + length(grep("Not Likely to Adversely Affect", NMFS.Final.Determination..Critical.Habitat.)),
            n_conc = length(grep("[^No] Concurrence", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("[^No] Concurrence", Biological.Conclusion.Determination)),
            n_jeop = length(grep("Jeopardy", NMFS.Final.Determination..Species.)),
            n_admo = length(grep("[^No] Adverse Modification", NMFS.Final.Determination..Critical.Habitat.)),
            n_rpa = length(grep("with RPA", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("with RPA", Biological.Conclusion.Determination)),
            n_tech = length(grep("Technical Assistance", NMFS.Final.Determination..Species.)) + length(grep("Technical Assistance", NMFS.Final.Determination..Critical.Habitat.)),
            staff_lead_hash = digest(first(NMFS.Project.Lead), algo = "md5"))
            #staff_support_hash = digest(first(Supporting.Staff), algo = "md5"))

#create boolean formal_consult
test$formal_consult <- NA
test$formal_consult[test$consult_type == "Informal Consultation"] <- "No"
test$formal_consult[test$consult_type == "Formal Consultation"] <- "Yes"

#create state abbreviations for offices
test$state <- NA
office_states <- data.frame(office = unique(test$ESOffice),
                              abb = c("ID", "OR", "HI", "WA", "AZ", "OK", "TX", "TX", "MO", "OH", "MI", "MN", "AL", "FL", "FL", "FL", "GA", "KY", "LA", "NC", "SC", "TN", "ME", "NH", "NJ", "VA", "WV", "MT", "WY", "SD", "UT", "CO", "CO", "AK", "CA", "OR", "NV", "NV", "CA", "CA", "CA", "WA", "TX",  "TX", "IL", NA, "PR", "NC", "AR", "NY", "PA", "KS", "NE", "AK", "CA", "CA", NA, "IL", "MS", "MD", "VA", "ND", NA, NA))

office_states$abb <- as.character(office_states$abb)

for (i in 1:nrow(office_states)){
  test$state[as.character(test$ESOffice) == as.character(office_states$office[i])] <- office_states$abb[i]
}

#negative time elapsed values occur when date_formal_consult is after FWS_concl_date (why?)
test$elapsed[test$elapsed < 0] <- NA

#work_category is first listed work_type
test$work_category <- NA
test$work_category <- str_split(test$work_type, " - ", simplify = TRUE)[,1]

#align data structure with old data
test$FWS_concl_date <- as.character(test$FWS_concl_date)
test$timely_concl <- as.character(test$timely_concl)
test$events_logged <- as.character(test$events_logged)
test$hours_logged <- as.character(test$hours_logged)
test$date_formal_consult <- as.character(test$date_formal_consult)
test$start_date <- as.character(test$start_date)
test$title <- as.character(test$title)
test$ESOffice <- as.character(test$ESOffice)
test$datum <- as.character(test$datum)

#replacements for old data (full) that created duplicate names
"Yellow-legged","yellow-legged"
"Lupine, Kincaid's", "Lupine, Kincaids"
"Lily, western", "Lily, Western"
"Horned lark, streaked", "Lark, streaked horned"
"Cottontail, new england", "Rabbit, new england cottontail"
"Rose-mallow, Neches", "Rose-mallow, neches"
"Spurge, darlington's", "Spurge, darlingtons"
"Loosestrife, fraser's", "Lossestrife, frasers"
"Saxifrage, gray's", "Saxifrage, grays"
"Liverwort [unnamed] (Plagiochila sharpii)", "Liverwort, Sharp's leafy (Plagiochila sharpii)"
"Trefoil, heller's", "Trefoil hellers"
"Trillium, ozark", "Trillium, least, ozark"
"Lily, gray's", "Lily, grays"
"Snaketail, edmund's", "Snaketail, edmunds"
"Plymouth Red-Bellied Turtle", "Turtle, Plymouth red-bellied"
"Glade cress, Kentucky", "Cress, kentucky glade"
"Rockcress, Georgia", "Rockcress, georgia"
"Salamander, Georgetown", "Salamander, georgetown"
"No Common Name (Lindera subcoriacea)", "Spicebush, bog (Lindera subcoriacea)"
"Sedge, [unnamed] (Carex impressinervia)", "Sedge, ravine (Carex impressinervia)"

for(i in 1:length(full$spp_ev_ls)){
  full$spp_ev_ls[[i]]<-gsub("Rice rat (Oryzomys palustris natator)", "Rat, rice (Oryzomys palustris natator)", full$spp_ev_ls[[i]])
}

grep("^[^,]+$", comb$species.y, value = TRUE)


update <- rbind(full, test)

for(i in 1:nrow(BO)){
  update$spp_BO_ls[update$activity_code == BO$Activity.Code[i]] <- BO$spp_BO_ls[i]
}

save(update, file = "C:/Users/mevans/repos/section7_explorer/data/FWS_S7_clean_03Feb2017.RData")

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

spot_key <- read.csv(file = "data/SpotKey.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = 'NA')
mass_key <- read.csv(file = "data/MassKey.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = 'N/A')

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

for(i in 816:nrow(spot_key)){
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
agency_key <- read.csv(file = "data/agencykey.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

for (i in 1:nrow(agency_key)){
  print(i)
  #test$Agency[test$Agency == agency_key$Original[i]] <- agency_key$New[i]
  good_data$Parent[good_data$Agency == agency_key$New[i]] <- agency_key$Parent[i]
}

##ADDING WORK CATEGORY DATA
worktype <- read.csv(file= "data/WorkCategories.csv", header = TRUE)[,1:3]
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
combos <- read.csv(file = "data/combos.csv", na.strings = "NA", stringsAsFactors = FALSE)
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

##Updated Jeop/AdMod Calls
jeopardies <- read.csv(file = "Jeopardies.csv", header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)
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

###NEED TO UPDATE ACTION AGENCY DETERMINATIONS FOR JEOPARDY BIOPS

for(i in 1066:nrow(jeopardies)){

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

scorekey <- read.csv(file = "scorekey.csv", header = TRUE)
good_data$Discrepancy <- NA
for(i in 1:nrow(scorekey)){
  score <- scorekey$Score[i]
  sp <- scorekey$Sp[i]
  psp <- scorekey$Action.Agency.Proposed.Effect.Determination..Species.[i]
  good_data$Discrepancy[good_data$Sp == sp & good_data$Action.Agency.Proposed.Effect.Determination..Species. == psp & !is.na(good_data$Sp) & !is.na(good_data$Action.Agency.Proposed.Effect.Determination..Species.)] <- score
}

good_data$Discrepancy <- as.factor(good_data$Discrepancy)

##pacific salmonid overlap data
dist <- read.csv(file = "Salmonidoverlap.csv", header = TRUE, stringsAsFactors = FALSE)
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
