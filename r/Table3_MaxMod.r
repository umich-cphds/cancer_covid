## MODIFIED VERSION OF LARS Table3.r
## Original file: /net/junglebook/michiganmedicine/larsf/Projects/COVID_Cancer/Table3.r
## Original output: /net/junglebook/michiganmedicine/larsf/Projects/COVID_Cancer/Outcome_vs_Cancer_20220701.csv

library(data.table)
library(tableone)

# setwd("/net/junglebook/michiganmedicine/larsf/Projects/COVID_Cancer")
setwd("objects")

#version <- "20220202"
version <- "20220701"

## Load PCR data

load(file=paste0("/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/",version,"/MichiganMedicine_PCRcohort_plus_VaccinationData_",version,".Rsav"))
PCRcohort[,Gender:=factor(Sex,levels=c("M","F"),labels=c("M","F"))]

PCRcohort[,DaysSinceBirth_KeyDate:=ifelse(COV19_positiveYN == 1,
	DaysSinceBirth_First_PositiveTestDiagnosis,
	DaysSinceBirth_First_Test_Diagnosis)]

PCRcohort <- PCRcohort[is.finite(DaysSinceBirth_KeyDate),]
PCRcohort[,Age_KeyDate:=DaysSinceBirth_KeyDate/365.25] 
PCRcohort <- PCRcohort[Age_KeyDate > 18,]

PCRcohort[,table(COV19_positiveYN)]

ehrdir <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883"

# input files
ehrfiles <- list.files(paste0(ehrdir,"/",version),".txt",full.name=T)

## Add Elixhauser score
load(file=paste0("/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/",version,"/MichiganMedicine_ElixhauserScoreCombined_",version,".Rsav"))
PCRcohort <- merge(PCRcohort,ElixhauserScoreCombined[,.(Encrypted_PatientID,wscore_ahrq,windex_ahrq)],by="Encrypted_PatientID",sort=F,all.x=T)
setnames(PCRcohort,c("wscore_ahrq","windex_ahrq"),c("ElixhauserScore_AHRQ","ElixhauserScore_AHRQ_Binned"))

## Add Key date for outcome definitions and phenome filtering


## Use procedures to define outcomes
file.proc <- ehrfiles[grepl("/Procedures_[0-9]",ehrfiles)]
file.demo <- ehrfiles[grepl("/Demographics",ehrfiles)]


## ---------------------------

# Filter procedure data

procs <- fread(file.proc)

## Map hospital

pmap <- unique(procs[,.(ProcedureCode,ProcedureName,Lexicon)])
pmap <- pmap[order(ProcedureCode),]


# pmap[grepl("Emergency",ProcedureName,ignore.case=T),]
# Emergency visit
ecodes <- 99281:99285

# pmap[grepl("Crit",ProcedureName,ignore.case=T),]
# Critical care
cccodes <- c(99289:99298,99466:99476)

# pmap[grepl("hosp",ProcedureName,ignore.case=T) & grepl("high|mod|extensive",ProcedureName,ignore.case=T),]
# Hospital visit
hcodes <- c(99221:99239,99460:99462,99477)

# Filter procedure data
procs2 <- procs[ProcedureCode %in% sort(c(ecodes,hcodes,cccodes)),]
procs2 <- unique(procs2[,.(Encrypted_PatientID,ProcedureDate_DaysSinceBirth,ProcedureCode)])

procs2 <- merge(procs2,PCRcohort[,.(Encrypted_PatientID,DaysSinceBirth_KeyDate)],by="Encrypted_PatientID")

## only keep procedures between -7 and + 30 days of key data
procs2 <- procs2[ProcedureDate_DaysSinceBirth >= DaysSinceBirth_KeyDate - 14 & 
	ProcedureDate_DaysSinceBirth <= DaysSinceBirth_KeyDate +30,]

# Hospitalizations
part1 <- procs2[ProcedureCode %in% c(hcodes,cccodes),]
part1 <- part1[order(Encrypted_PatientID,ProcedureDate_DaysSinceBirth),]
part1 <- part1[!duplicated(Encrypted_PatientID),]
part1 <- part1[,.(Encrypted_PatientID,DaysSinceBirth_First_HospitalCare_Procedure=ProcedureDate_DaysSinceBirth)]

# ICU
part2 <- procs2[ProcedureCode %in% cccodes,]
part2 <- part2[order(Encrypted_PatientID,ProcedureDate_DaysSinceBirth),]
part2 <- part2[!duplicated(Encrypted_PatientID),]
part2 <- part2[,.(Encrypted_PatientID,DaysSinceBirth_First_CriticalCare_Procedure=ProcedureDate_DaysSinceBirth)]

PCRcohort <- merge(PCRcohort,part1,by="Encrypted_PatientID",all.x=T)
PCRcohort <- merge(PCRcohort,part2,by="Encrypted_PatientID",all.x=T)

## Add cancer coding
cancer_phecodes <- c("145", "145.1", "145.2", "145.3", "145.4", "145.5", "149", 
	"149.1", "149.2", "149.3", "149.4", "149.5", "149.9", "150", 
	"151", "153", "153.2", "153.3", "155", "155.1", "157", "158", 
	"159", "159.2", "159.3", "159.4", "164", "165", "165.1", "170", 
	"170.1", "170.2", "172.1", "172.11", "174", "174.1", "174.11", 
	"174.2", "174.3", "175", "180", "180.1", "180.3", "182", "184", 
	"184.1", "184.11", "184.2", "185", "187", "187.1", "187.2", "187.8", 
	"189", "189.1", "189.11", "189.12", "189.2", "189.21", "189.4", 
	"190", "191", "191.1", "191.11", "193", "194", "195", "195.1", 
	"195.3", "196", "197", "198", "198.1", "198.2", "198.3", "198.4", 
	"198.5", "198.6", "198.7", "199", "199.4", "200", "200.1", "201", 
	"202", "202.2", "202.21", "202.22", "202.23", "202.24", "204", 
	"204.1", "204.11", "204.12", "204.2", "204.21", "204.22", "204.3", 
	"204.4", "209", "860")
	
cancer_codes_path <- "/net/wonderland/home/mmsalva/projects/covid/new_cancer/lists/cancer_codes.rds"
cancer_phecodes <- readRDS(cancer_codes_path)$cancer_phecodes

## Extract individuals who had cancer at least 14 days before the index date
load(file=paste0("/net/junglebook/michiganmedicine/data/PCR_Tested_Cohort/",version,"/phenomes/UNFILTERED_",version,"/UNFILTERED_",version,"_Phecodes_Birthyears.Rsav"))
diagnoses_Phecodes <- merge(PCRcohort[,.(Encrypted_PatientID,DaysSinceBirth_KeyDate)],
	diagnoses_Phecodes,by.x="Encrypted_PatientID",by.y="IID")
diagnoses_Phecodes0 <- diagnoses_Phecodes[DaysSinceBirth < DaysSinceBirth_KeyDate - 14,]

hasPhenome <- diagnoses_Phecodes0[,unique(Encrypted_PatientID)]
PCRcohort[Encrypted_PatientID %in% hasPhenome,table(COV19_positiveYN)]

diagnoses_Phecodes0 <- diagnoses_Phecodes0[phecode %in% cancer_phecodes,]

hadcancer <- unique(diagnoses_Phecodes0$Encrypted_PatientID)

PCRcohort[,Cancer:=ifelse(Encrypted_PatientID %in% hadcancer,1,0)]

groups <- c("Cancer_COVID_Positive","Cancer_COVID_Negative","Cancer_COVID_Negative_Matched")
vars <- c("Deceased","CriticalCare","HospitalCare","NotHospitalized")

## compare with data from processed cohort
processed.data <- readRDS("whole_data_20220701.rds")
subPCRcohort <- PCRcohort[Encrypted_PatientID %in% processed.data[, id]]

## Matching
library(MatchIt)

mahvars <- "Age_KeyDate"
exactvars <- "Gender"

mdata <- data.frame(na.omit(PCRcohort[,c("Encrypted_PatientID","COV19_positiveYN",mahvars,exactvars),with=F]),check.names=F)
submdata <- data.frame(na.omit(PCRcohort[,c("Encrypted_PatientID","COV19_positiveYN",mahvars,exactvars),with=F]),check.names=F)
rownames(mdata) <- mdata$Encrypted_PatientID
rownames(submdata) <- submdata$Encrypted_PatientID

MATCHED1 <- matchit(COV19_positiveYN ~ Age_KeyDate,
	exact = exactvars, data = mdata, method = "nearest", ratio = 5, discard = "both", mahvars = mahvars)
subMATCHED1 <- matchit(COV19_positiveYN ~ Age_KeyDate,
                    exact = exactvars, data = submdata, method = "nearest", ratio = 5, discard = "both", mahvars = mahvars)

MATCHEDOUT1 <- match.data(MATCHED1)
matchMatrix <- MATCHED1$match.matrix
MATCHEDOUT1 <- data.table(MATCHEDOUT1)

subMATCHEDOUT1 <- match.data(subMATCHED1)
submatchMatrix <- subMATCHED1$match.matrix
subMATCHEDOUT1 <- data.table(subMATCHEDOUT1)

PCRcohort_matched <- merge(PCRcohort, MATCHEDOUT1[,.(Encrypted_PatientID, COVID_Strata = subclass)], by = "Encrypted_PatientID", all.x = T)

subPCRcohort_matched <- merge(subPCRcohort, subMATCHEDOUT1[,.(Encrypted_PatientID, COVID_Strata = subclass)], by = "Encrypted_PatientID", all.x = T)

# check1 <- data.table(PCRcohort_matched[,table(COV19_positiveYN,COVID_Strata)])
# check1[COV19_positiveYN == 0,table(N)]

# PCRcohort[,Cancer:=ifelse(Encrypted_PatientID %in% hadcancer,1,0)]

PCRcohort_matched[,Cancer_COVID_Positive:=
	ifelse(Cancer == 1 & COV19_positiveYN == 1,1,ifelse(Cancer == 0 & COV19_positiveYN == 1,0,NA))]
PCRcohort_matched[,Cancer_COVID_Negative:=
	ifelse(Cancer == 1 & COV19_positiveYN == 0,1,ifelse(Cancer == 0 & COV19_positiveYN == 0,0,NA))]
PCRcohort_matched[,Cancer_COVID_Negative_Matched:=
	ifelse(Cancer == 1 & COV19_positiveYN == 0 & !is.na(COVID_Strata),1,
		ifelse(Cancer == 0 & COV19_positiveYN == 0 & !is.na(COVID_Strata),0,NA))]

subPCRcohort_matched[,Cancer_COVID_Positive:=
                    ifelse(Cancer == 1 & COV19_positiveYN == 1,1,ifelse(Cancer == 0 & COV19_positiveYN == 1,0,NA))]
subPCRcohort_matched[,Cancer_COVID_Negative:=
                    ifelse(Cancer == 1 & COV19_positiveYN == 0,1,ifelse(Cancer == 0 & COV19_positiveYN == 0,0,NA))]
subPCRcohort_matched[,Cancer_COVID_Negative_Matched:=
                    ifelse(Cancer == 1 & COV19_positiveYN == 0 & !is.na(COVID_Strata),1,
                           ifelse(Cancer == 0 & COV19_positiveYN == 0 & !is.na(COVID_Strata),0,NA))]


# 
# PCRcohort_matched[!is.na(Cancer_COVID_Positive),.N]
# PCRcohort_matched[!is.na(Cancer_COVID_Negative),.N]
# PCRcohort_matched[!is.na(Cancer_COVID_Negative_Matched),.N]
# 
# PCRcohort_matched[!is.na(Cancer_COVID_Positive),table(Outcome)]
# PCRcohort_matched[!is.na(Cancer_COVID_Negative),table(Outcome)]
# PCRcohort_matched[!is.na(Cancer_COVID_Negative_Matched),table(Outcome)]
# 
# PCRcohort_matched[!is.na(Cancer_COVID_Positive),table(Cancer)]
# PCRcohort_matched[!is.na(Cancer_COVID_Negative),table(Cancer)]
# PCRcohort_matched[!is.na(Cancer_COVID_Negative_Matched),table(Cancer)]


save(PCRcohort_matched, file = paste0("Matched_Cohort_", version, ".Rsav"))
save(subPCRcohort_matched, file = paste0("Matched_Cohort_Sub_", version, ".Rsav"))
# save(PCRcohort_matched,file=paste0("Matched_Cohort_",version,".Rsav"))

## Fill in outcomes
load(file=paste0("Matched_Cohort_",version,".Rsav"))
load(file=paste0("Matched_Cohort_Sub_",version,".Rsav"))


PCRcohort_matched[,Deceased:=ifelse(DeceasedDaysSinceBirth >= DaysSinceBirth_KeyDate - 14 & DeceasedDaysSinceBirth <= DaysSinceBirth_KeyDate + 60,1,NA)]

PCRcohort_matched[,CriticalCare:=ifelse(
	DaysSinceBirth_First_CriticalCare_Procedure >= DaysSinceBirth_KeyDate - 14 & 
	DaysSinceBirth_First_CriticalCare_Procedure <= DaysSinceBirth_KeyDate + 30,1,NA)]

PCRcohort_matched[,HospitalCare:=ifelse(
	(DaysSinceBirth_First_HospitalCare_Procedure >= DaysSinceBirth_KeyDate -14 & 
	DaysSinceBirth_First_HospitalCare_Procedure <= DaysSinceBirth_KeyDate + 30) |
	(DaysSinceBirth_First_CriticalCare_Procedure >= DaysSinceBirth_KeyDate - 14 & 
	DaysSinceBirth_First_CriticalCare_Procedure <= DaysSinceBirth_KeyDate + 30),1,NA)]

PCRcohort_matched[,NotHospitalized:=ifelse(is.na(HospitalCare),1,NA)]
	
vars <- c("Cancer_COVID_Positive","Cancer_COVID_Negative","Cancer_COVID_Negative_Matched")
tab1 <- CreateTableOne(data=PCRcohort_matched,vars=vars,factorVars=vars,test=F,addOverall=T)
tab1Mat <- print(tab1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels=T)

for(outcome in c("NotHospitalized","HospitalCare","CriticalCare","Deceased")){
	tab2 <- CreateTableOne(data=PCRcohort_matched[get(outcome) == 1,],vars=vars,factorVars=vars,test=F,addOverall=T)
	tab2Mat <- print(tab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels=T)
	
	tab2Mat <- data.table(tab2Mat)
	colnames(tab2Mat)[2] <- outcome
	tab1Mat <- cbind(tab1Mat,tab2Mat[,-1])
}

tab1Mat <- data.table(c(1,4,3,7,6,10,9),c("n",rep(c("n No Cancer","n Cancer"),3)),tab1Mat)

tab2Mat <- data.table(V1=c(2,5,8),V2="n total",level="",rbind(
	suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",tab1Mat[2,4:8])))+
	suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",tab1Mat[3,4:8]))),
	suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",tab1Mat[4,4:8])))+
	suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",tab1Mat[5,4:8]))),
	suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",tab1Mat[6,4:8])))+
	suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",tab1Mat[7,4:8])))
	))

colnames(tab2Mat) <- colnames(tab1Mat)

tableOut <- data.table(Subgroup=c("Overall","COVID-19 Positive","","","COVID-19 Negative","","","COVID-19 Negative (Matched)","",""), rbind(tab1Mat,tab2Mat)[order(V1),])
tableOut[,`:=`(V1=NULL,level=NULL)]
colnames(tableOut)[2] <- ""

groups <- c("Overall","NotHospitalized","HospitalCare","CriticalCare","Deceased")

for(group in groups) tableOut[[group]] <- gsub(" \\([0-9\\.]+\\)","",tableOut[[group]])

groups <- c("NotHospitalized","HospitalCare","CriticalCare","Deceased")

for(i in 1:nrow(tableOut)){
	overall <- as.numeric(tableOut[["Overall"]][i])
	for(group in groups) {
			counts <- as.numeric(tableOut[[group]][i])
			perc <- counts / overall * 100
			perc <- ifelse(perc < 1,sprintf(perc, fmt = ' (%#.1f)'),sprintf(perc, fmt = ' (%#.1f)'))
			tableOut[[group]][i] <- paste0(counts,perc)
	}
}

fwrite(tableOut, file =paste0("Outcome_vs_Cancer_",version,".csv"))


##

subPCRcohort_matched[,Deceased:=ifelse(DeceasedDaysSinceBirth >= DaysSinceBirth_KeyDate - 14 & DeceasedDaysSinceBirth <= DaysSinceBirth_KeyDate + 60,1,NA)]

subPCRcohort_matched[,CriticalCare:=ifelse(
  DaysSinceBirth_First_CriticalCare_Procedure >= DaysSinceBirth_KeyDate - 14 & 
    DaysSinceBirth_First_CriticalCare_Procedure <= DaysSinceBirth_KeyDate + 30,1,NA)]

subPCRcohort_matched[,HospitalCare:=ifelse(
  (DaysSinceBirth_First_HospitalCare_Procedure >= DaysSinceBirth_KeyDate -14 & 
     DaysSinceBirth_First_HospitalCare_Procedure <= DaysSinceBirth_KeyDate + 30) |
    (DaysSinceBirth_First_CriticalCare_Procedure >= DaysSinceBirth_KeyDate - 14 & 
       DaysSinceBirth_First_CriticalCare_Procedure <= DaysSinceBirth_KeyDate + 30),1,NA)]

subPCRcohort_matched[,NotHospitalized:=ifelse(is.na(HospitalCare),1,NA)]

vars <- c("Cancer_COVID_Positive","Cancer_COVID_Negative","Cancer_COVID_Negative_Matched")
subtab1 <- CreateTableOne(data=subPCRcohort_matched,vars=vars,factorVars=vars,test=F,addOverall=T)
subtab1Mat <- print(subtab1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels=T)

for(outcome in c("NotHospitalized","HospitalCare","CriticalCare","Deceased")){
  subtab2 <- CreateTableOne(data=subPCRcohort_matched[get(outcome) == 1,],vars=vars,factorVars=vars,test=F,addOverall=T)
  subtab2Mat <- print(subtab2, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, showAllLevels=T)
  
  subtab2Mat <- data.table(subtab2Mat)
  colnames(subtab2Mat)[2] <- outcome
  subtab1Mat <- cbind(subtab1Mat,subtab2Mat[,-1])
}

subtab1Mat <- data.table(c(1,4,3,7,6,10,9),c("n",rep(c("n No Cancer","n Cancer"),3)),subtab1Mat)

subtab2Mat <- data.table(V1=c(2,5,8),V2="n total",level="",rbind(
  suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",subtab1Mat[2,4:8])))+
    suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",subtab1Mat[3,4:8]))),
  suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",subtab1Mat[4,4:8])))+
    suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",subtab1Mat[5,4:8]))),
  suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",subtab1Mat[6,4:8])))+
    suppressWarnings(as.numeric(gsub(" \\([0-9\\.]+\\)","",subtab1Mat[7,4:8])))
))

colnames(subtab2Mat) <- colnames(subtab1Mat)

subtableOut <- data.table(Subgroup=c("Overall","COVID-19 Positive","","","COVID-19 Negative","","","COVID-19 Negative (Matched)","",""), rbind(subtab1Mat,subtab2Mat)[order(V1),])
subtableOut[,`:=`(V1=NULL,level=NULL)]
colnames(subtableOut)[2] <- ""

groups <- c("Overall","NotHospitalized","HospitalCare","CriticalCare","Deceased")

for(group in groups) subtableOut[[group]] <- gsub(" \\([0-9\\.]+\\)","",subtableOut[[group]])

groups <- c("NotHospitalized","HospitalCare","CriticalCare","Deceased")

for(i in 1:nrow(subtableOut)){
  overall <- as.numeric(subtableOut[["Overall"]][i])
  for(group in groups) {
    counts <- as.numeric(subtableOut[[group]][i])
    perc <- counts / overall * 100
    perc <- ifelse(perc < 1,sprintf(perc, fmt = ' (%#.1f)'),sprintf(perc, fmt = ' (%#.1f)'))
    subtableOut[[group]][i] <- paste0(counts,perc)
  }
}

fwrite(subtableOut, file =paste0("Outcome_vs_Cancer_Sub_",version,".csv"))
