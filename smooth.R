
S[1:10]
T[1:10]
F[1:10]
cbind(T[1:N],F[1:N],S[1:N])
cbind(T[1:N]/1e6,F[1:N]/1000,S[1:N])[S[1:N]==1,]
TFS=cbind(T[1:N]/1e6,F[1:N]/1000,S[1:N])[S[1:N]>=2,]
plot(TFS[,1],TFS[,2],pch=".")
plot(T,F,pch=".",col="red")
points(TFS[,1],TFS[,2],pch=".",col="black")
plot(T[F>1],log(F[F>1]),pch=".")
plot((1:N)[F[1:N]>1 ],F[1:N][F[1:N]>1],pch=".")

TF=cbind(T*3e-8,F)
TF[1:100,]
TF[F>1,]

dirname<-"/users/juliesims/bats/AnabatFilesFromChris"
alldirs<-list.dirs(dirname)
allbats<-list.dirs(dirname,full.names=FALSE)
for (dirno in 2:length(alldirs)) {
  battype<-alldirs[dirno]
  batname<-allbats[dirno]
  allfiles<-list.files(battype,recursive=FALSE)
  if (length(allfiles)>0) {
    for (batno in 1:length(allfiles)) {
      FILENAME <- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(battype,"/",FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER = readBin(binfile, raw(), chars)
      FILELEN  = length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER = as.integer(BUFFER)
      LOADFILE()
      write.csv(cbind(T[1:N],F[1:N],S[1:N]),paste("/users/juliesims/bats/ChrisLoadedFiles/",batname,"_",FILENAME,".csv",sep=""))
      close(binfile)
      cat(battype,"-",FILENAME,"\n")
    }
  }
}

dirno=31
  battype<-alldirs[dirno]
  batname<-allbats[dirno]
  allfiles<-list.files(battype,recursive=FALSE)
  par(mfrow=c(3,4))
for (batno in 1:12) {
      FILENAME <- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(battype,"/",FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER = readBin(binfile, raw(), chars)
      FILELEN  = length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER = as.integer(BUFFER)
      LOADFILE()
      close(binfile)
batdat=F[F>1]
hist(batdat)
}

dirno=31
  battype<-alldirs[dirno]
  batname<-allbats[dirno]
  allfiles<-list.files(battype,recursive=FALSE)
  par(mfrow=c(1,1))
for (batno in 5) {
      FILENAME <- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(battype,"/",FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER = readBin(binfile, raw(), chars)
      FILELEN  = length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER = as.integer(BUFFER)
      LOADFILE()
      close(binfile)
plot((1:N)[F[1:N]>1 ],F[1:N][F[1:N]>1],pch=".")
}

dirno=31
  battype<-alldirs[dirno]
  batname<-allbats[dirno]
  allfiles<-list.files(battype,recursive=FALSE)
for (batno in 1:length(allfiles)) {
      FILENAME <- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(battype,"/",FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER = readBin(binfile, raw(), chars)
      FILELEN  = length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER = as.integer(BUFFER)
      LOADFILE()
      close(binfile)
	  cat(rawToChar(oldBUFFER[7:281]),"\n")
}

dirname<-"/users/juliesims/bats/SMZC/Backyard"
  allfiles<-list.files(dirname,recursive=FALSE)
  if (length(allfiles)>0) {
    for (batno in 1:(length(allfiles)-1)) {
      FILENAME <- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(dirname,"/",FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER = readBin(binfile, raw(), chars)
      FILELEN  = length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER = as.integer(BUFFER)
      LOADFILE()
      write.csv(cbind(T[1:N],F[1:N],S[1:N]),paste(dirname,"/",FILENAME,".csv",sep=""))
      close(binfile)
      cat(dirname,"-",FILENAME,"\n")
    }
  }

batno=1
plotbat<-function(batno) {
      FILENAME <<- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(dirname,"/", FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER <<- readBin(binfile, raw(), chars)
      FILELEN  <<- length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER <<- as.integer(BUFFER)
      LOADFILE()
      close(binfile)
      plot(T[F>0]/1e6,F[F>0]/1e3,pch=".",ylim=c(0,70),main=FILENAME)
}  
par(mfrow=c(3,4))
for (i in 1:12) plotbat(i)
   
plotbat(1)   
   
F[1:10]
ls() 
      plot(T,F,pch=".")
      hist(F[F>0])
      
histbat<-function(batno) {
      FILENAME <<- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(dirname,"/", FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER <<- readBin(binfile, raw(), chars)
      FILELEN  <<- length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER <<- as.integer(BUFFER)
      LOADFILE()
      close(binfile)
      hist(F[F>0]/1e3,xlim=c(0,70),main=FILENAME)
}  
par(mfrow=c(3,4))
for (i in 1:12) histbat(i)

smoothbat<-function(batno) {
      FILENAME <<- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(dirname,"/", FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER <<- readBin(binfile, raw(), chars)
      FILELEN  <<- length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER <<- as.integer(BUFFER)
      LOADFILE()
      close(binfile)
      plot(density(F[F>0]/1e3),xlim=c(0,70),main=FILENAME)
}  
par(mfrow=c(3,4))
for (i in 13:24) smoothbat(i)

par(mfrow=c(1,1))
smoothbat(1)

loessbat<-function(batno,span=0.75,xmin=-100,xmax=100) {
      FILENAME <<- allfiles[batno]
# Read the file into BUFFER
      binfile = file(paste(dirname,"/", FILENAME,sep=""), "rb")
      chars = 40000
      BUFFER <<- readBin(binfile, raw(), chars)
      FILELEN  <<- length(BUFFER)
      oldBUFFER = BUFFER
      BUFFER <<- as.integer(BUFFER)
      LOADFILE()
      close(binfile)
      freq=F[F>0]/1e3
      time=T[F>0]/1e6
      xsmooth=loess(freq~time,span=span)
      if (xmax>15) xmax = max(time)
      if (xmin<0) xmin=min(time)
      plot(xsmooth$x,xsmooth$fitted,ylim=c(0,70),xlim=c(xmin,xmax),main=FILENAME,type="p",pch=".",col="red")
}  
par(mfrow=c(3,4))
for (i in 1:12) loessbat(i,span=0.04)

par(mfrow=c(1,1))
loessbat(12,span=0.04,xmin=0,xmax=3)
points(T[F>0]/1e6,F[F>0]/1e3)

x=loess(freq~time)
plot(loess.smooth(time,freq))