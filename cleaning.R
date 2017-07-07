library(digest)
library(dplyr)
library(tidyr)
library(stringr)
setwd("C:/Users/mevans/repos/NMFS7")
file1 <- read.csv("FY2001_FY2017_filea.csv", header = TRUE, sep = ",")
file2 <- read.csv("FY2001_FY2017_fileb.csv", header = TRUE, sep = ",")
file3 <- read.csv("FY73_FY2000.csv", header = TRUE, sep = ",")

temp <- rbind(file1, file2, file3)

AugOctF <- read.csv("TAILS_Report_Formal_Aug16Dec16.csv", header = TRUE, sep = ",")
MarJulF <- read.csv("TAILS_Report_Formal_Mar16Jul16.csv", header = TRUE, sep = ",")
JulAugI <- read.csv("TAILS_Report_Informal_Jul16Aug16.csv", header = TRUE, sep = ",")
MarAprI <- read.csv("TAILS_Report_Informal_Mar16Apr16.csv", header = TRUE, sep = ",")
MayJunI <- read.csv("TAILS_Report_Informal_May16Jun16.csv", header = TRUE, sep = ",")
NovDecI <- read.csv("TAILS_Report_Informal_Nov16Dec16.csv", header = TRUE, sep = ",")
SepOctI <- read.csv("TAILS_Report_Informal_Sep16Oct16.csv", header = TRUE, sep = ",")
OctDecF <- read.csv("TAILS_Report_Formal_Oct16Dec16.csv", header = TRUE, sep = ",")
new <- rbind(AugOctF, OctDecF, MarJulF, JulAugI, MarAprI, MayJunI, NovDecI, SepOctI)

head(unique(SepOctI))

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

#select(comb, species.x, species.y)%>%
  #right_join(new, by = c("Species.Involved...Evaluated" = "species.x"))

test <- group_by(new, Activity.Code)%>%
  summarise(activity_code = first(Activity.Code),
            region = first(Lead.Region),
            ESOffice = first(Lead.Office),
            title = first(Activity.Title),
            lead_agency = first(Lead.Agency...Tribe),
            FY = first(Fiscal.Year),
            FY_start = first(Start.Date.Fiscal.Year),
            FY_concl = first(Conclusion.Date.FY),
            start_date = first(Start.Received.Date),
            date_formal_consult = first(Formal.Consultation.Initiated.Date),
            due_date = first(Due.Date),
            FWS_concl_date = first(FWS.Response...Conclusion.Date),
            elapsed = first(Elapsed.Days),
            date_active_concl = str_extract(first(Active.Concluded), "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
            timely_concl = first(Concluded.in.Timely.Manner),
            hours_logged = first(Staff.Hours.Logged.Current.FY),
            events_logged = first(Events.Logged.Current.FY),
            consult_type = first(Consultation.Type),
            consult_complex = first(Consultation.Complexity),
            work_type = paste(unique(Action.Work.Type), collapse = " - "),
            ARRA = first(ARRA.Fund),
            datum = first(Datum),
            lat = first(Latitude),
            long = first(Longitude),
            spp_ev_ls = list(unique(Species.Involved...Evaluated)),
            #spp_BO_ls = paste(Biological.Opinion.Species[Biological.Opinion.Species!=""],": BO = ",Biological.Conclusion.Determination[Biological.Opinion.Species!=""],"; CH = ",Critical.Habitat.Biological.Conclusion.Determination[Biological.Opinion.Species!=""], sep = "", collapse = ", "),
            spp_BO_ls = paste0(unique(paste(Biological.Opinion.Species[Biological.Opinion.Species!=""], ": BO = ", Biological.Conclusion.Determination[Biological.Opinion.Species!=""], "; CH = ", sep = "")), collapse = ", "),
            n_spp_eval = length(unique(Species.Involved...Evaluated)),
            n_spp_BO = length(unique(Biological.Opinion.Species)),
            n_nofx = length(grep("No Effect", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("No Effect", Biological.Conclusion.Determination)),
            n_NLAA = length(grep("NLAA", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("NLAA", Biological.Conclusion.Determination)),
            n_conc = length(grep("[^No] Concurrence", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("[^No] Concurrence", Biological.Conclusion.Determination)),
            n_jeop = length(grep("Jeopardy", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("Jeopardy", Biological.Conclusion.Determination)),
            n_admo = length(grep("[^No] Adverse Modification", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("[^No] Adverse Modification", Biological.Conclusion.Determination)),
            n_rpa = length(grep("with RPA", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("with RPA", Biological.Conclusion.Determination)),
            n_tech = length(grep("Technical Assistance", Critical.Habitat.Biological.Conclusion.Determination)) + length(grep("Technical Assistance", Biological.Conclusion.Determination)),
            staff_lead_hash = digest(first(Staff.Lead), algo = "md5"),
            staff_support_hash = digest(first(Supporting.Staff), algo = "md5"))

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
