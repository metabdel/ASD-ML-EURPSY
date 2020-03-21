setwd("~/schlesslab/Autism/")
library(data.table)
aut.demo <- read.csv("research//Table1.csv", header = F, na.strings= ".U", sep = ",",
                        stringsAsFactors =T, encoding = "Latin1")
colnames(aut.demo) <- c("As","ID", "SIBTYPE", "SIBPAIRID", "SEX", 
                           "BIRTH_DATY", "BIRTH_DATM", "BIRTH_DATD", 
                           "DEATH_DATY", "DEATH_DATM", "DEATH_DATD", 
                           "SES", "CITY")
head(aut.demo)

aut.demo$DOB <- as.Date(with(aut.demo, 
                                paste(BIRTH_DATY, BIRTH_DATM, BIRTH_DATD,sep="-")), 
                           "%Y-%m-%d") 
aut.demo$DOD <- as.Date(with(aut.demo, 
                                paste(DEATH_DATY, DEATH_DATM, DEATH_DATD,sep="-")), 
                           "%Y-%m-%d") 


aut.demo$mDOB <- aut.demo$DOB
aut.demo$mDOB[aut.demo$SIBTYPE != "MID"] <- NA

aut.demo$pDOB <- aut.demo$DOB
aut.demo$pDOB[aut.demo$SIBTYPE != "FID"] <- NA

aut.demo$cDOB <- aut.demo$DOB
aut.demo$cDOB[aut.demo$SIBTYPE != "CID"] <- NA

aut.demo$sDOB <- aut.demo$DOB
aut.demo$sDOB[aut.demo$SIBTYPE != "SID"] <- NA
aut.demo$As = NULL
head(aut.demo)
#############################################################################

# Create family data : cases and siblings data w/ birth order; not linked by family; n=5022

aut.fam <- read.csv("research/Table2.csv", 
                     header = F, na.strings= ".U", sep = ",", 
                     stringsAsFactors =T)
colnames(aut.fam) <-  c("As", "ID", "SIBTYPE", "BIRTH_DATE", 
                         "SIBPAIRD", "SITE", "B_ORDER")
aut.fam$As = NULL
aut.fam$DOB <- as.Date( as.character(aut.fam$BIRTH_DATE), "%Y-%m-%d")

#############################################################################

# Family relations data : separate row for each sib-case combination (n=3855); family relations clear

aut.ids <- read.csv("research/Table3.csv", header = F, 
                     na.strings= c(".U", "NULL"),  stringsAsFactors =T)
colnames(aut.ids) <- c("As","MID", "FID", "SID", "CID", "SIDPAIR", "CITYL")
str (aut.ids); dim(aut.ids); head(aut.ids)
aut.ids$As = NULL

#############################################################################
# Table 4 - CID, Table 5 - SID, Table 6 - MID, Table 7 - FID
# the below function creates medication data tables: separate row for each mother/father/sibling/case ID; SIBTYPE colmn indicates ID
library(tidyverse)
TidyTables4_to_7 <- function (DataTableToTidy, ID) {
  DataTableNamed <- read.csv(DataTableToTidy, header = F, 
                             na.strings=  c(".U", "NULL"), sep = ",", stringsAsFactors = FALSE)
  colnames(DataTableNamed) <- c("As", ID , "DATEMP", "DATERP", 
                                "DRUG_NAME", "PILLS", "BOXES", "ATC5_Code", "DDDcode", "PillsPaid")
  DataTableNamed$DATE_MP <- as.Date(as.character(DataTableNamed$DATEMP), "%Y%m%d")
  DataTableNamed$DATE_RP <- as.Date(as.character(DataTableNamed$DATERP), "%Y%m%d")         
  return (DataTableNamed)
}

aut.rx.cid <- TidyTables4_to_7("./research/Table4.csv.2", "CID")
aut.rx.sid <- TidyTables4_to_7("./research/Table5.csv", "SID")
aut.rx.mid <- TidyTables4_to_7("./research/Table6.csv.2", "MID")
aut.rx.mid <- TidyTables4_to_7("./research/Table7.csv.2", "FID")

#############################################################################
#############################################################################
# this function reads Tables 9 to 13
# Table 9 - CID, Table 11 - SID, Table 12 - MID, Table 13 - FID
# diagnoses info: separate row for each mother/father/sibling/case ID; SIBTYPE colmn indicates ID

TidyTables9_to_13 <- function (DataTableToTidy, ID){
  DataTableNamed <- read.csv(DataTableToTidy, header = F, na.strings= ".U", sep = ",")
  colnames(DataTableNamed) <- c("As", ID, "DX_DATY", "DX_DATM", "DX_DATD", "DX_CODE", "DX_DESC")
  DataTableNamed$DX_DATE <- as.Date(with(DataTableNamed, paste(DX_DATY, DX_DATM, DX_DATD,sep="-")), 
                                    "%Y-%m-%d") 
  return (DataTableNamed)
}

aut.dx.cid <- TidyTables9_to_13("./research/Table9.csv", "CID")
aut.dx.sid <- TidyTables9_to_13("./research/Table11.csv", "SID")
aut.dx.mid <- TidyTables9_to_13("./research/Table12.csv", "MID")
aut.dx.fid <- TidyTables9_to_13("./research/Table13.csv", "FID")
aut.dx.cid.code = read.table("./research/Table10.csv.2", sep = ",", 
                             header = F, stringsAsFactors = F, 
                             na.strings = ".U")

names(aut.dx.cid.code) = c("As", "CID", "ASDSTY", "ASDSTM", "ASDSTD", "DIAG", "DIAGCODE", "DIAGPHYS", "SERIOUS")


#############################################################################