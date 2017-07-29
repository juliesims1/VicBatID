# IMPORTANT *******

# Chris Corben's C++ code http://users.lmi.net/corben/loader.txt 
# modified by me to be in R
# Chris:
# I have not checked the code in this file to ensure it works!
# It is a cut down version of the code used in ANALOOK.
# Use it at your own risk!

# *****************

LOADFILE<-function(filelist) {
  # Set the buffer length
  chars <- 40000
  # Number of files in the list to loop over
  nfile<-length(filelist)
  allout<- vector("list", nfile)
  # Loop over files in the list
  for (ifile in 1:nfile) {
    fileout<-NULL
    fullname<-filelist[ifile]
    cat("Starting file ",ifile," ",fullname,"\n")
    # Set up the binary file contents in BUFFER
    binfile = file(fullname, "rb")
    BUFFER = readBin(binfile, raw(), chars)
    close(binfile)
    FILELEN  = length(BUFFER)
    oldBUFFER = BUFFER
    BUFFER = as.integer(BUFFER)
    
    # Extract the filename from the fully qualified name
    FILENAME<-basename(fullname)
    
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
# The main output is in fileout
    fileout$F = numeric(16384)
    fileout$T = numeric(16384)
    fileout$S = numeric(16384)

# F will hold frequencies in Herz
# T will hold times in microseconds
# S will hold status byte.
#   if S=0 dot is out of range
#   if S=1 dot switched off
#   if S=2 dot normal
#   if S=3 dot is maindot (has been selected as part of call body)

#int DIVRAT;#  holds division ratio
    fileout$DIVRAT = 0
#DWORD RES1;
    fileout$RES1 = 0
#int N;#  holds number of data points
    fileout$N=0

#int YEAR;
#int MON;
#int DAY;
#int HOURS;
#int MINS;
#int SECS;
#int HUNDS;
#int MICROS;

#int FTYPE;#  holds file TYPE (must be 129 to 132)
    fileout$FTYPE=BUFFER[4]
    fileout$datelist<-getfiledatetime(0x011b,fileout$FTYPE,BUFFER)
    
    # Remove nulls in header
    heading <- oldBUFFER[7:280]
    heading <- heading[heading != 0x00]
    fileout$header<-trimws(rawToChar(heading))
#char FILENAME[13];# Already contains zero-terminated string of filename


    s=0
    time=0
    lastdif=0
    dif=0
  #fdif,temdif;
  #double timefactor;

  # clear all arrays
    for(j in 1:16384)fileout$T[j]<-0
    for(j in 1:16384)fileout$F[j]<-0
    for(j in 1:16384)fileout$S[j]<-2 # dots on
  
    if(BUFFER[4]==0x81)
  {#type 129 File Structure
      fileout$FTYPE<-BUFFER[4]
      p=BUFFER[1]+BUFFER[2]*256
      if(p!=0x11a) {allout[[ifile]]<-0;next}
      fileout$RES1<-BUFFER[p+3]+256*BUFFER[p+4]
      if((fileout$RES1>60000)||(fileout$RES1<10000)) {allout[[ifile]]<-0;next}
      if(fileout$RES1!=25000)timefactor=25.0e3/fileout$RES1
      if((BUFFER[p+5]>64)||(BUFFER[p+5]<1)){allout[[ifile]]<-0;next}
      fileout$datelist<-getfiledatetime(p+1,fileout$FTYPE,BUFFER)
      fileout$DIVRAT<-BUFFER[p+5]
      p=BUFFER[p+1]+BUFFER[p+2]*256
      if(p!=0x120){allout[[ifile]]<-0;next}
      fileout$T[1]<-0
      fileout$S[1]<-0
      fileout$S[2]<-0
      t=1
      while(p<FILELEN)
      {
        if(BUFFER[p+1]<128)
        {
          dif=BUFFER[p+1]
          if(dif>63)dif=dif-128 
          lastdif=lastdif+dif
          if(fileout$RES1!=25000)
          {
            dif=floor(timefactor*lastdif+0.5)
          } else dif=lastdif
          time=time+dif
          fileout$T[t+1]<-time
          t=t+1
          p=p+1;
        } else {
          if(BUFFER[p+1]>0xf8)
          {
            s=BUFFER[p+1]-0xf8;
            while(s>0)
            {
              fileout$S[t+s]<-1;
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
            if(fileout$RES1!=25000)
            {
              dif=floor(timefactor*lastdif+0.5)
            }
            time=time+dif
            fileout$T[t+1]<-time
            t=t+1
            p=p+2
          }
        }
      }
      allout[[ifile]]<-alldone(t,fileout);next
    } else if((BUFFER[4]== 130)||(BUFFER[4]== 131)||(BUFFER[4]== 132)) # Index increased by 1
    {# type 130/131/132 File Structure
      fileout$FTYPE<-BUFFER[4]
      p=BUFFER[1]+BUFFER[2]*256
      if(p!=0x11a){allout[[ifile]]<-0;next}
      fileout$RES1<-BUFFER[p+3]+256*BUFFER[p+4]
      if((fileout$RES1 > 60000)||(fileout$RES1 < 10000)){allout[[ifile]]<-0;next}
      if(fileout$RES1!=25000)timefactor=25.0e3/fileout$RES1
      if((BUFFER[p+5] > 64)||(BUFFER[p+5] < 1)){allout[[ifile]]<-0;next}
      fileout$DIVRAT<-BUFFER[p+5]
      fileout$datelist<-getfiledatetime(p+1,fileout$FTYPE,BUFFER)
      p=BUFFER[p+1]+BUFFER[p+2]*256
      if((p!=0x120) && (fileout$FTYPE < 0x84)){allout[[ifile]]<-0;next} # file corrupt
      if((p!=0x150) && (fileout$FTYPE==0x84)){allout[[ifile]]<-0;next} # file corrupt
      fileout$T[1]<-0
      fileout$S[1]<-0
      fileout$S[2]<-0
      t=1
      while((p < FILELEN) && (t < 16384))
      {
        #cat(p," ",t,"\n") # FOr debug
        if(BUFFER[p+1] < 128)
        {
          dif=BUFFER[p+1] 
          if(dif > 63)dif=dif-128  
            lastdif=lastdif+dif
          if(fileout$RES1!=25000)
          {
            dif=floor(timefactor*lastdif+0.5)
          } else dif=lastdif
          time=time+dif
          fileout$T[t+1]<-time
          t=t+1
          p=p+1
        } else {
          if(BUFFER[p+1] >= 0xe0) #224
          {
            if((fileout$FTYPE==131)||(fileout$FTYPE==132))
            {
              #unsigned char c;
              if((p+1) >= FILELEN) {allout[[ifile]]<-alldone(t,fileout);next}
              c=BUFFER[p+1] %% 4 # Last 2 bits
              s=BUFFER[p+2]
              if((t+s-1) > 16383)s=16384-t # limits index to arrays
              while(s > 0)
              {
                fileout$S[t+s]<-c
                s=s-1
              }
              p=p+2
            } else { # if FTYPE==131 or 132
              s=BUFFER[p+1]-0xe0
              if((t+s-1) > 16383)s=16384-t # limits index to arrays
              while(s > 0)
              {
                fileout$S[t+s]<-1
                s=s-1
              }
              p=p+1
            }# else FTYPE==130
          } else { # BUFFER[p+1] > 0xe0 status change
            switchvar = ((floor(BUFFER[p+1]/32)-4)) # Bits 5 and 6 
            {
              if (switchvar == 0)
              {
                if((p+1) >= FILELEN) {allout[[ifile]]<-alldone(t,fileout);break}
                dif=((BUFFER[p+1] %% 32) * 2^8)+(BUFFER[p+2]) # Changed +\ to +  oldBUFFER[p+1]
                lastdif=dif
                if(fileout$RES1!=25000)
                {
                  dif=floor(timefactor*lastdif+0.5)
                }
                time=time+dif
                fileout$T[t+1]<-time
                t=t+1
                p=p+2
                #break;
              } else if (switchvar == 1) {
                if((p+2) >= FILELEN) {allout[[ifile]]<-alldone(t,fileout);break}
                dif=((BUFFER[p+1] %% 32) * 2^16)+((BUFFER[p+2]) * 2^8)+(BUFFER[p+3])
                lastdif=dif
                if(fileout$RES1!=25000)
                {
                  dif=floor(timefactor*lastdif+0.5)
                }
                time=time+dif
                fileout$T[t+1]<-time
                t=t+1
                p=p+3
                #break;
              } else if (switchvar == 2) {
                if (p==5592 & t==3718) {
                  #cat("here we are")
                }
                if((p+3) >= FILELEN) {allout[[ifile]]<-alldone(t,fileout);break}
   
                dif=((BUFFER[p+1] %% 32) * 2^24)+((BUFFER[p+2]) * 2^16)+((BUFFER[p+3]) * 2^8)+(BUFFER[p+4])
                lastdif=dif
                if(fileout$RES1!=25000)
                {
                  dif=floor(timefactor*lastdif+0.5)
                }
                time=time+dif
                fileout$T[t+1]<-time
                t=t+1
                p=p+4
                #break;
              }
            }
          }
        }
      }
      allout[[ifile]]<-alldone(t,fileout);next
    } else {#Invalid File Structure
      allout[[ifile]]<-0;next
    }
  }
  return(allout)
}  

alldone<-function(t,fileout)
{
  fileout$N<-t
  fileout$S[t+1]<- -2
  fileout$RES1<-25000 # all data transormed to microseconds
  # calcfreq() calculates the frequency data from the time data
  # filling F[] with data derived from DIVRAT and T[]
  # Calculate frequencies
  t=3 # JAS Increased by 1 for array indices
  #long td;
  #double A;
  
  fileout$F[1]<-0 # JAS Increased by 1
  fileout$F[2]<-0 # JAS Increased by 1
  
  #long n;
  
  Tmin<-ceiling(fileout$DIVRAT*4.0) # corresponds to 250 KHz
  #  Tmax<-floor(DIVRAT*250.0) # corresponds to 4 KHz
  #  Tmax<-floor(DIVRAT*280.0) # corresponds to 3.571 KHz - might be what AnalookW is using by default
  Tmax<-floor(fileout$DIVRAT*1000.0) # corresponds to 1 KHz 
  #if(Tmin < 48)Tmin<-48
  #if(Tmax > 12859)Tmax<-12859
  while (t <= fileout$N)
  {
    td=fileout$T[t]-fileout$T[t-2]
    if ((td >= Tmin) && (td <= Tmax))
    {
      A=(fileout$DIVRAT)*1.0e6/td
      fileout$F[t]<-A
    } else {
      fileout$F[t]<-0
      fileout$S[t]<-0
    }
    t=t+1
  }
  return(fileout) # returns number of data points
}


# getfiledatetime(int) fetches the date and time into
# global variables. These values are obtained from the 
# data information table buried in the file structure
# for those file types which support this.
# Otherwise, the date and time are extracted from
# the filename, provided the filename is based
# on date and time.

getfiledatetime = function(p,FTYPE,BUFFER) # p is increased by 1 in the call
{
  tem = character(2)
  datelist<-NULL
  #int t;
  if(FTYPE >=132)
  {
    datelist$YEAR<-BUFFER[p+6]+256*BUFFER[p+7]
    datelist$MON<-BUFFER[p+8]
    datelist$DAY<-BUFFER[p+9]
    datelist$HOURS<-BUFFER[p+10]
    datelist$MINS<-BUFFER[p+11]
    datelist$SECS<-BUFFER[p+12]
    datelist$HUNDS<-BUFFER[p+13]
    datelist$MICROS<-BUFFER[p+14]+256*BUFFER[p+15]
    return(datelist)
  }
  
  #for filetypes before 132, get date and time from filename
  
  t=substring(FILENAME,1,1)
  if(isDigit(t))
  {
    datelist$YEAR<-as.numeric(t)+1990
    # goto month;
  } else {  
    t=toupper(t)
    if((t>='A')&&(t<='Z'))
    {
      datelist$YEAR<-strtoi(charToRaw(t))+2000-strtoi(charToRaw('A'))
      # goto month;
    } else datelist<-baddate()
  }  
  # goto baddate;
  #month:
  t=substring(FILENAME,2,2)
  if(isDigit(t))
  {
    datelist$MON<-as.numeric(t)
    #goto day;
  } else {  
    t=toupper(t)
    if((t>='A')&&(t<='C'))
    {
      datelist$MON<-strtoi(charToRaw(t))+10-strtoi(charToRaw('A'))
      #goto day;
    } else datelist<-baddate()
  }
  #  goto baddate;
  #day:
  tem[1]=substring(FILENAME,3,3)
  if(!isDigit(tem[1])) datelist<-baddate()
  tem[2]=substring(FILENAME,4,4)
  if(!isDigit(tem[2])) datelist<-baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  datelist$DAY<-t
  tem[1]=substring(FILENAME,5,5)
  if(!isDigit(tem[1])) datelist<-baddate()
  tem[2]=substring(FILENAME,6,6)
  if(!isDigit(tem[2])) datelist<-baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  datelist$HOURS<-t
  tem[1]=substring(FILENAME,7,7)
  if(!isDigit(tem[1])) datelist<-baddate()
  tem[2]=substring(FILENAME,8,8)
  if(!isDigit(tem[2])) datelist<-baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  datelist$MINS<-t
  tem[1]=substring(FILENAME,10,10)
  if(!isDigit(tem[1])) datelist<-baddate()
  tem[2]=substring(FILENAME,11,11)
  if(!isDigit(tem[2])) datelist<-baddate()
  #tem[2]=0;
  t=as.numeric(tem[1])*10 + as.numeric(tem[2])
  datelist$SECS<-t
  datelist$HUNDS<-0
  datelist$MICROS<-0
  return(datelist)
}


baddate<-function() 
{
  YEAR<-0;
  MON<-0;
  DAY<-0;
  HOURS<-0;
  MINS<-0;
  SECS<-0;
  HUNDS<-0;
  MICROS<-0;
  return(list(YEAR=YEAR,MON=MON,DAY=DAY,HOURS=HOURS,MINS=MINS,SECS=SECS,HUNDS=HUNDS,MICROS=MICROS))
}

isDigit<-function(c) { ((c >= "0") && (c <= "9"))}




