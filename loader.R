# IMPORTANT *******

# Chris Corben's C++ code http://users.lmi.net/corben/loader.txt 
# modified by me to be in R
# Chris:
# I have not checked the code in this file to ensure it works!
# It is a cut down version of the code used in ANALOOK.
# Use it at your own risk!

# *****************

alldirs<-list.dirs("/Volumes/Julie/Bats/AnabatFilesFromChris")
dirno<-2
battype<-alldirs[dirno]
allfiles<-list.files(battype,recursive=FALSE)
batno<-2
FILENAME <- allfiles[batno]
# Read the file into BUFFER

FILENAME = "6B152023.31#"
battype = "CHDW"
FILENAME = "F2052247.59#"
battype = "RHME"
FILENAME = "T2072238.22#"
battype = "KEPA"
binfile = file(paste("/users/juliesims/bats/AnabatFilesFromChris/",battype,"/",FILENAME,sep=""), "rb")
binfile = file(paste(battype,"/",FILENAME,sep=""), "rb")

#binfile = file("/users/juliesims/bats/batloader/F6B152023.31", "rb")
#binfile = file("F6B152023.31", "rb")

chars = 40000
#chars = 10000L
#BUFFER = readChar(binfile, chars, useBytes = FALSE)
BUFFER = readBin(binfile, raw(), chars)
#nchar(BUFFER)
BUFFER[1:10]
FILELEN  = length(BUFFER)
oldBUFFER = BUFFER
BUFFER = as.integer(BUFFER)
BUFFER[1:10]

#IDvals = readBin(binfile, integer(), size=4, endian = "little")

# LOADFILE() Loads an ANABAT sequence file stored in BUFFER
# into global arrays T, F and S
# Before using it, the file must have already been loaded 
# into BUFFER, the global variable FILELEN set to
# the file length in bytes and the string FILENAME
# set to the filename

# Loadfile() loads the arrays with the data from the file.
# If the file is corrupt, LOADFILE() returns 0,
# otherwise it returns the number of data points,
# which value is also stored in the global N.
# For making use of the data in the arrays, 
# use an index j such that 0  < = j  <  N

# WORD is 2 byte unsigned
# DWORD is 4 byte unsigned
# BYTE is 1 byte unsigned

# global variables

#int FILELEN;#  already set to length of file
#BYTE BUFFER[32768];# 32768 is max file size
                   # BUFFER already filled with file contents

#DWORD F[16384];
#DWORD T[16384];
#BYTE S[16384];
F = numeric(16384)
T = numeric(16384)
S = numeric(16384)

# F will hold frequencies in Herz
# T will hold times in microseconds
# S will hold status byte.
#   if S=0 dot is out of range
#   if S=1 dot switched off
#   if S=2 dot normal
#   if S=3 dot is maindot (has been selected as part of call body)

#int DIVRAT;#  holds division ratio
DIVRAT = 0
#DWORD RES1;
RES1 = 0
#int N;#  holds number of data points
N=0

#int YEAR;
#int MON;
#int DAY;
#int HOURS;
#int MINS;
#int SECS;
#int HUNDS;
#int MICROS;
YEAR=0
MON=0
DAY=0
HOURS=0
MINS=0
SECS=0
HUNDS=0
MICROS=0

#int FTYPE;#  holds file TYPE (must be 129 to 132)
FTYPE=BUFFER[4]
getfiledatetime(0x011b)
#char FILENAME[13];# Already contains zero-terminated string of filename


# getfiledatetime(int) fetches the date and time into
# global variables. These values are obtained from the 
# data information table buried in the file structure
# for those file types which support this.
# Otherwise, the date and time are extracted from
# the filename, provided the filename is based
# on date and time.

getfiledatetime = function(p) # p is increased by 1 in the call
{
  tem = character(2)
  #int t;
  if(FTYPE >=132)
  {
    YEAR<<-BUFFER[p+6]+256*BUFFER[p+7]
    MON<<-BUFFER[p+8]
    DAY<<-BUFFER[p+9]
    HOURS<<-BUFFER[p+10]
    MINS<<-BUFFER[p+11]
    SECS<<-BUFFER[p+12]
    HUNDS<<-BUFFER[p+13]
    MICROS<<-BUFFER[p+14]+256*BUFFER[p+15]
    return(list(YEAR=YEAR,MON=MON,DAY=DAY,HOURS=HOURS,MINS=MINS,SECS=SECS,HUNDS=HUNDS,MICROS=MICROS,FTYPE=FTYPE))
  }

#for filetypes before 132, get date and time from filename
 
  t=substring(FILENAME,1,1)
  if(isDigit(t))
  {
    YEAR<<-as.numeric(t)+1990
    # goto month;
  } else {  
    t=toupper(t)
    if((t>='A')&&(t<='Z'))
    {
      YEAR<<-strtoi(charToRaw(t))+2000-strtoi(charToRaw('A'))
      # goto month;
    } else baddate()
  }  
  # goto baddate;
#month:
  t=substring(FILENAME,2,2)
  if(isDigit(t))
  {
    MON<<-as.numeric(t)
    #goto day;
  } else {  
    t=toupper(t)
    if((t>='A')&&(t<='C'))
    {
      MON<<-strtoi(charToRaw(t))+10-strtoi(charToRaw('A'))
    #goto day;
    } else baddate()
  }
#  goto baddate;
#day:
  tem[1]=substring(FILENAME,3,3)
  if(!isDigit(tem[1])) baddate()
  tem[2]=substring(FILENAME,4,4)
  if(!isDigit(tem[2])) baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  DAY<<-t
  tem[1]=substring(FILENAME,5,5)
  if(!isDigit(tem[1])) baddate()
  tem[2]=substring(FILENAME,6,6)
  if(!isDigit(tem[2])) baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  HOURS<<-t
  tem[1]=substring(FILENAME,7,7)
  if(!isDigit(tem[1])) baddate()
  tem[2]=substring(FILENAME,8,8)
  if(!isDigit(tem[2])) baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  MINS<<-t
  tem[1]=substring(FILENAME,10,10)
  if(!isDigit(tem[1])) baddate()
  tem[2]=substring(FILENAME,11,11)
  if(!isDigit(tem[2])) baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  SECS<<-t
  HUNDS<<-0
  MICROS<<-0
    return(list(YEAR=YEAR,MON=MON,DAY=DAY,HOURS=HOURS,MINS=MINS,SECS=SECS,HUNDS=HUNDS,MICROS=MICROS,FTYPE=FTYPE))
}

baddate<-function() 
{
  YEAR<<-0;
  MON<<-0;
  DAY<<-0;
  HOURS<<-0;
  MINS<<-0;
  SECS<<-0;
  HUNDS<<-0;
  MICROS<<-0;
}
isDigit<-function(c) { ((c >= "0") && (c <= "9"))}

# calcfreq() calculates the frequency data from the time data
# filling F[] with data derived from DIVRAT and T[]

calcfreq<-function()
{
  t=3 # JAS Increased by 1 for array indices
  #long td;
  #double A;

  F[1]<<-0 # JAS Increased by 1
  F[2]<<-0 # JAS Increased by 1

  #long n;

  Tmin<<-ceiling(DIVRAT*4.0) # corresponds to 250 KHz
#  Tmax<<-floor(DIVRAT*250.0) # corresponds to 4 KHz
#  Tmax<<-floor(DIVRAT*280.0) # corresponds to 3.571 KHz - might be what AnalookW is using by default
  Tmax<<-floor(DIVRAT*1000.0) # corresponds to 1 KHz 
  #if(Tmin < 48)Tmin<<-48
  #if(Tmax > 12859)Tmax<<-12859
  while (t <= N)
  {
    td=T[t]-T[t-2]
    if ((td >= Tmin) && (td <= Tmax))
    {
      A=(DIVRAT)*1.0e6/td
      F[t]<<-A
    } else {
      F[t]<<-0
      S[t]<<-0
    }
    t=t+1
  }
}



LOADFILE<-function()
{
  s=0
  time=0
  lastdif=0
  dif=0
  #fdif,temdif;
  #double timefactor;

  # clear all arrays
  for(j in 1:16384)T[j]<<-0
  for(j in 1:16384)F[j]<<-0
  for(j in 1:16384)S[j]<<-2 # dots on

  if(BUFFER[4]==0x81)
  {#type 129 File Structure
    FTYPE<<-BUFFER[4]
    p=BUFFER[1]+BUFFER[2]*256
    if(p!=0x11a)return(0)
    RES1<<-BUFFER[p+3]+256*BUFFER[p+4]
    if((RES1>60000)||(RES1<10000))return(0)
    if(RES1!=25000)timefactor=25.0e3/RES1
    if((BUFFER[p+5]>64)||(BUFFER[p+5]<1))return(0)
    getfiledatetime(p+1)
    DIVRAT<<-BUFFER[p+5]
    p=BUFFER[p+1]+BUFFER[p+2]*256
    if(p!=0x120)return(0)
    T[1]<<-0
    S[1]<<-0
    S[2]<<-0
    t=1
    while(p<FILELEN)
    {
      if(BUFFER[p+1]<128)
      {
        dif=BUFFER[p+1]
        if(dif>63)dif=dif-128 
        lastdif=lastdif+dif
        if(RES1!=25000)
        {
          dif=floor(timefactor*lastdif+0.5)
        } else dif=lastdif
        time=time+dif
        T[t+1]<<-time
        t=t+1
        p=p+1;
      } else {
        if(BUFFER[p+1]>0xf8)
        {
          s=BUFFER[p+1]-0xf8;
          while(s>0)
          {
            S[t+s]<<-1;
            s=s-1;
          }
          p=p+1;
        } else {
          nshift=floor(BUFFER[p+1]/8) %% 16
          #nshift=nshift>>3;
          #nshift=nshift&0x0f;
          dif=(BUFFER[p+1] %% 8)*256+BUFFER[p+2]
          if(nshift>0)dif=dif*(2^nshift)
          lastdif=dif
          if(RES1!=25000)
          {
            dif=floor(timefactor*lastdif+0.5)
          }
          time=time+dif
          T[t+1]<<-time
          t=t+1
          p=p+2
        }
      }
    }
    return(alldone(t))
  } else if((BUFFER[4]== 130)||(BUFFER[4]== 131)||(BUFFER[4]== 132)) # Index increased by 1
  {# type 130/131/132 File Structure
    FTYPE<<-BUFFER[4]
    p=BUFFER[1]+BUFFER[2]*256
    if(p!=0x11a)return(0)
    RES1<<-BUFFER[p+3]+256*BUFFER[p+4]
    if((RES1 > 60000)||(RES1 < 10000))return(0)
    if(RES1!=25000)timefactor=25.0e3/RES1
    if((BUFFER[p+5] > 64)||(BUFFER[p+5] < 1))return(0)
    DIVRAT<<-BUFFER[p+5]
    getfiledatetime(p+1)
    p=BUFFER[p+1]+BUFFER[p+2]*256
    if((p!=0x120) && (FTYPE < 0x84))return(0) # file corrupt
    if((p!=0x150) && (FTYPE==0x84))return(0) # file corrupt
    T[1]<<-0
    S[1]<<-0
    S[2]<<-0
    t=1
    while((p < FILELEN) && (t < 16384))
    {
      if(BUFFER[p+1] < 128)
      {
        dif=BUFFER[p+1] 
        if(dif > 63)dif=dif-128  
          lastdif=lastdif+dif
        if(RES1!=25000)
        {
          dif=floor(timefactor*lastdif+0.5)
        } else dif=lastdif
        time=time+dif
        T[t+1]<<-time
        t=t+1
        p=p+1
      } else {
        if(BUFFER[p+1] >= 0xe0) #224
        {
          if((FTYPE==131)||(FTYPE==132))
          {
            #unsigned char c;
            if((p+1) >= FILELEN) return(alldone(t))
            c=BUFFER[p+1] %% 4 # Last 2 bits
            s=BUFFER[p+2]
            if((t+s-1) > 16383)s=16384-t # limits index to arrays
            while(s > 0)
            {
              S[t+s]<<-c
              s=s-1
            }
            p=p+2
          } else { # if FTYPE==131 or 132
            s=BUFFER[p+1]-0xe0
            if((t+s-1) > 16383)s=16384-t # limits index to arrays
            while(s > 0)
            {
              S[t+s]<<-1
              s=s-1
            }
            p=p+1
          }# else FTYPE==130
        } else { # BUFFER[p+1] > 0xe0 status change
          switchvar = ((floor(BUFFER[p+1]/32)-4)) # Bits 5 and 6 
          {
            if (switchvar == 0)
            {
              if((p+1) >= FILELEN) return(alldone(t))
              dif=((BUFFER[p+1] %% 32) * 2^8)+(BUFFER[p+2]) # Changed +\ to +  oldBUFFER[p+1]
              lastdif=dif
              if(RES1!=25000)
              {
                dif=floor(timefactor*lastdif+0.5)
              }
              time=time+dif
              T[t+1]<<-time
              t=t+1
              p=p+2
              #break;
            } else if (switchvar == 1) {
              if((p+2) >= FILELEN) return(alldone(t))
              dif=((BUFFER[p+1] %% 32) * 2^16)+((BUFFER[p+2]) * 2^8)+(BUFFER[p+3])
              lastdif=dif
              if(RES1!=25000)
              {
                dif=floor(timefactor*lastdif+0.5)
              }
              time=time+dif
              T[t+1]<<-time
              t=t+1
              p=p+3
              #break;
            } else if (switchvar == 2) {
              if((p+3) >= FILELEN) return(alldone(t))
 
              dif=((BUFFER[p+1] %% 32) * 2^24)+((BUFFER[p+2]) * 2^16)+((BUFFER[p+3]) * 2^8)+(BUFFER[p+4])
              lastdif=dif
              if(RES1!=25000)
              {
                dif=floor(timefactor*lastdif+0.5)
              }
              time=time+dif
              T[t+1]<<-time
              t=t+1
              p=p+4
              #break;
            }
          }
        }
      }
    }
    return(alldone(t))
  } else {#Invalid File Structure
    return(0)
  }
}
  
  
alldone<-function(t)
{
  N<<-t
  S[t+1]<<- -2
  RES1<<-25000 # all data transormed to microseconds
  calcfreq()
  return(t) # returns number of data points
}
LOADFILE()

