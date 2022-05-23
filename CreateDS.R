install.packages('openxlsx')
install.packages('shinyjs')
library(openxlsx)
MeXds <- read.xlsx("Datas/Humidity_20210703-0930.xlsx")
MeXds$time<-as.POSIXct(MeXds$DT,origin="1970-01-01",tz="UTC")
MeXds$Name <- NULL
MeXds$Unit <- NULL
MeXds$DT <- NULL

MeXds <- transform(MeXds, temperature=read.xlsx("Datas/Temperature_20210703-0930.xlsx")$Temperature)
MeXds <- transform(MeXds, humidity=MeXds$Humidity)
MeXds$Humidity <- NULL 
MeXds <- transform(MeXds, pressure=read.xlsx("Datas/Pressure_20210703-0930.xlsx")$Pressure)
MeXds <- transform(MeXds, luminance=read.xlsx("Datas/Luminance_20210703-0930.xlsx")$Luminance)
MeXds <- transform(MeXds, noise=read.xlsx("Datas/Noise_20210703-0930.xlsx")$Noise)
MeXds <- transform(MeXds, motion=read.xlsx("Datas/Motion_20210703-0930.xlsx")$Motion)
MeXds <- transform(MeXds, presence=read.xlsx("Datas/Presence_20210703-0930.xlsx")$Presence)

#Predefine Summarize Data
doPredefine <- function() {
    intervals <- c(60*60,60*60*24,60*60*24*30)
    functions <- c("mean", "max", "min" ,"sum")
    for (i in intervals){
        for (f in functions){
            ds <- mexSum.org(interval=i, f=eval(parse(text=f)))
            assign(paste("MeXds.",i,f,sep=""), ds, envir = .GlobalEnv)
            write_rds(ds, file=paste("MeXds.",i,f,".rds",sep=""))
        }
    }
}
MeXds.60 <- mexSum()
write(MeXds.60, file="MeXds.60.rds")



Node <- read.xlsx("Datas/Node.xlsx")
DataListJP <- read.xlsx("Datas/DataList_JP.xlsx")
DataListEN <- read.xlsx("Datas/DataList_EN.xlsx")
Findings<-list()
fdParams <- list()
Learnings<-list()
Learn_Find<-NULL
ImprovementIdea<-list()

library(readr) 
write_rds(MeXds, file = "MeXds.rds")
write_rds(Node, file = "Node.rds")
write_rds(Findings, file = "Findings.rds")
write_rds(fdParams, file = "fdParams.rds")
write_rds(Learnings, file = "Learnings.rds")
write_rds(Learnings, file = "Learnings.rds")
write_rds(Learn_Find, file = "Learn_Find.rds")
write_rds(DataListEN, file= "DataListEN.rds")
write_rds(DataListJP, file= "DataListJP.rds")

