f1 <- read.csv("file1 clean.csv", header = TRUE)
f1[,10]<- mdy(f1[,10])
f2 <- read.csv("file2 clean.csv", header = TRUE)
f2[,10]<- mdy(f2[,10])
f3 <- read.csv("file3 clean.csv", header = TRUE)
f3[,10]<- ymd(f3[,10])
f4 <- read.csv("file4 clean.csv", header = TRUE)
f4[,10]<- ymd(f4[,10])
f5 <- read.csv("file5 clean.csv", header = TRUE)
f5[,10]<- ymd(f5[,10])
f6 <- read.csv("file6_clean.csv", header = TRUE)
f6[,10]<- ymd(f6[,10])
nosp <- read.csv("NMFS_s7_nospecies.csv", header = TRUE)

full <- rbind(f1,f2,f3,f4,f5,f6)
full2 <- full[full$NMFS.Tracking.Number!="",]

colnames(full)[1] <- "activity_code"
colnames(full)[2] <- "title"
colnames(full)[3] <- "consult_type"
colnames(full)[8] <- "start_date"
colnames(full)[14] <- "lead_agency"
colnames(full)[15] <- "agency_division"
colnames(full)[31] <- "state"

test <- group_by(full, NMFS.Tracking.Number)%>%
  summarise(NMFS.Tracking.Number = first(NMFS.Tracking.Number),
            region = first(NMFS.Lead.Region),
            ESOffice = NA,
            title = first(TITLE),
            lead_agency = first(Lead.Federal.Action.Agency),
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

