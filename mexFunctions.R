# packageの読み込み
#install.packages("tidyverse")
#install.packages("DT")
library(tidyverse)

#### MeXdsを時間（秒数指定）、ノード毎に集計する
mexSum.org <- function(interval=60, sDS=MeXds, fdt="2021-07-03 08:00:00", tdt="2021-09-30 18:00:00", f=mean){
    ##warningの非表示
    ##suppressWarnings()
    ##メッセージの非表示
    ##suppressMessages()
    ##cat()やprint()の非表示
    ##capture.output()
    ##options(warn=-1)

    #データの時間範囲を文字列からPOSIXct型へ
    fdtP<-as.POSIXct(fdt,tz="UTC")
    tdtP<-as.POSIXct(tdt,tz="UTC")
    stDS<-data.frame(sDS[fdtP <= sDS$time & sDS$time <= tdtP,])
    ##元のデータセットと基本的には列を変えない
    #集約する時間単位（秒数）に分類するキーを設定
    ##print(f)
    tDS<-transform(stDS, sumtime=as.POSIXct((as.integer(stDS$time)%/%interval)*interval, origin="1970-01-01",tz="UTC"))
    ##tDS<-transform(sDS, sumtime=as.POSIXct((as.integer(sDS$time)%/%interval)*interval, origin="1970-01-01",tz="UTC"))
    f<-rep(c(f),length=7)
    ##agent毎、時間単位毎に分類可能にして、aent毎＆時間単位毎に集計する
    DSgb<-group_by(tDS, NodeNo, sumtime)
    sumDS<-suppressMessages(summarise(DSgb, temperature=f[[1]](temperature), humidity=f[[2]](humidity), pressure=f[[3]](pressure), luminance=f[[4]](luminance), presence=f[[5]](presence), motion=f[[6]](motion), noise=f[[7]](noise)))
    names(sumDS)[[2]] <- "time"
    sumDS
}

#デモ用に、その場でデータの集約をするのではなく、UIに合わせて、60min, 24hourについて、Mean, Mαx ,Min ,Sumを事前に計算した結果を用いる。
mexSum <- function(interval=60, sDS=MeXds, fdt="2021-07-03 08:00:00", tdt="2021-09-30 18:00:00", f="mean"){
    #データの時間範囲を文字列からPOSIXct型へ
    fdtP<-as.POSIXct(fdt,tz="UTC")
    tdtP<-as.POSIXct(tdt,tz="UTC")
    if (interval == 60){
        sDS <- MeXds60
    } else {
        ##sDS <- eval(parse(text=paste("MeXds.",interval,as.character(eval(substitute(alist(f)))),sep="")))
        sDS <- eval(parse(text=paste("MeXds.",interval,f,sep="")))
    }
    tDS<-sDS[fdtP <= sDS$time & sDS$time <= tdtP,]
    tDS
}


##任意の2つのデータ列をプロットする。
mexPlot.tx <- function(x="time", y="presence", fdt="2021-07-03 08:00:00", tdt="2021-09-30 18:00:00", interval=60, sDS=MeXds, f="mean",  node=1:11){
    ##sDS<-suppressMessages(mexSum(interval=interval, sDS=sDS, f=f))
    #データの時間範囲を文字列からPOSIXct型へ
    fdtP<-as.POSIXct(fdt,tz="UTC")
    tdtP<-as.POSIXct(tdt,tz="UTC")
    tDS<-mexSum(interval=interval, sDS=sDS, f=f, fdt=fdt, tdt=tdt)
    idx <- is.element(tDS$NodeNo, node)
    if (any(idx)){
        plot(xval, yval, type="n", xlab=x, ylab=y)
        nn <- tDS$NodeNo[idx]
        text(xval[idx], yval[idx], labels=as.character(nn), col=nn)
        title(paste(fdtP, tdtP, sep=" - "))
    }
}
mexPlot <- function(x="time", y="presence", type="o", fdt="2021-07-03 08:00:00", tdt="2021-09-30 18:00:00", interval=60, sDS=MeXds, f=mean,  node=1:11){
   
    tDS<-mexSum(interval=interval, sDS=sDS, f=f, fdt=fdt, tdt=tdt)
    #取り出す列を変数で指定しているので、おまじない
    xval<-eval(parse(text=paste("tDS",x,sep="$")))
    yval<-eval(parse(text=paste("tDS",y,sep="$")))
    #データの時間範囲を文字列からPOSIXct型へ
    fdtP<-as.POSIXct(fdt,tz="UTC")
    tdtP<-as.POSIXct(tdt,tz="UTC")
    if (length(xval) >0){
        oldpar <- par
        par(mar=c(3,4,3,6))
        plot(xval, yval, type="n", xlab=x, ylab=y)
        for (i in seq(node)){
            idx <- (tDS$NodeNo==node[i])
            if (any(idx)){
                points(xval[idx], yval[idx], type=type, col=i, pch=i, lty=i)
            }
		}
	    title(paste(fdt, tdt, sep=" - "))
        par(xpd=T)
        legend(par()$usr[2], par()$usr[4], legend=paste("Node-",node,sep=""), pch=seq(node), col=seq(node))
        par(oldpar)
    }
}


library(readr)
MeXds <- read_rds("MeXds.rds")
MeXds60 <- read_rds("MeXds.60.rds")
MeXds.3600max <- read_rds("MeXds.3600max.rds")
MeXds.3600mean <- read_rds("MeXds.3600mean.rds")
MeXds.3600min <- read_rds("MeXds.3600min.rds")
MeXds.3600sum <- read_rds("MeXds.3600sum.rds")
MeXds.86400max <- read_rds("MeXds.86400max.rds")
MeXds.86400mean <- read_rds("MeXds.86400mean.rds")
MeXds.86400min <- read_rds("MeXds.86400min.rds")
MeXds.86400sum <- read_rds("MeXds.86400sum.rds")
MeXds.2592000max <- read_rds("MeXds.2592000max.rds")
MeXds.2592000mean <- read_rds("MeXds.2592000mean.rds")
MeXds.2592000min <- read_rds("MeXds.2592000min.rds")
MeXds.2592000sum <- read_rds("MeXds.2592000sum.rds")
Node <- read_rds("Node.rds")
Findings <- read_rds("Findings.rds")
fdParams <- read_rds("fdParams.rds")
Learnings <- read_rds("Learnings.rds")
Learn_Find <- read_rds("Learn_Find.rds")
ImprovementIdea <- read_rds("ImprovementIdea.rds")

library(jpeg)
sensorNodeMap <- readJPEG("SensorNodeMap.jpg") 
DataListJP <- read_rds("DataListJP.rds")
DataListEN <- read_rds("DataListEN.rds")

Mission <- c("To improve the exhibition service, let's think of ideas for keeping the viewing environment appropriate. ")
MissonJA <- "展示サービスの改善のために、展示ホールの室温を確認と鑑賞環境を適切に保つためのアイデアを考えてみましょう。"
