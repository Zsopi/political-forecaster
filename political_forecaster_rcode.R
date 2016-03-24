
        ##(Machine) Learning about political trouble
        
        ###This code implements an estimation of “political trouble” in a given year in a given country my machine learning methods. One country-year is one record.
        
        ##Variable definitions and data sources
        
        ###Target variable
        
        ###“Political trouble” is a binary target variable, defined as 1 if either of the following happening in the given year: coup, attempted coup, self-coup, rebels ousting executive, interregnum periods (as defined by code -77 in the Polity IV database, see [**here**](http://www.systemicpeace.org/inscr/p4manualv2013.pdf)), civil war or violence, ethnic war or violence (otherwise the variable is 0). The civil violence data is described [**here**](http://www.systemicpeace.org/inscr/MEPVcodebook2014.pdf) and the data sources is [**here**](http://www.systemicpeace.org/inscr/MEPV2014.xls). The estimate uses the “CIVTOT” variable, but only with an intensity code of at least 3 (on a 0 to 10 scale). This is to exclude long running, often low intensity cases like ethnic strife in China or Thailand that is still ongoing. The definition of “political trouble” includes only domestic events and excludes cases of external wars. The source of political variables is the [**Center for Systemic Peace**](http://www.systemicpeace.org/inscrdata.html).
        
        ###Features (or independent variables) are:  

#* __ln_gdp_pc__: The natural log of) GDP per capita at purchasing power in 2011 USA dollars. Source: [**IMF**](https://www.imf.org/external/pubs/ft/weo/2015/02/weodata/index.aspx) World Economic Outlook Database or the [**Maddison Project**](http://www.ggdc.net/maddison/maddison-project/home.htm).

#* __gdp_pc_grth_lgd__:Lagged GDP growth rate 5 years up to the year prior to the year examined, % annualized (this is to exclude reverse causation). Source: own calculations from above sources.

#* __youth_r__: Ratio of “young” population in the adult population. Source: own calculation based on [**UN demographic data**](http://esa.un.org/unpd/wpp/Download/Standard/ASCII/). For definition of “young” see below – this was the result of a kind of a grid search, final result is 17-24 years old.

#* __young_pop_grth__: Growth rate of young population, % per annum. Source: same as above.

#* __total_pop_grth__: Growth rate of total population, % per annum. Source: same as above.

#* __polity2__: Political system variable. Denotes how authoritarian or democratic is in the country in the given year. originally on a scale of -10 to +10, normalized to 1 to 21.  Source: Polity IV:  Regime Authority Characteristics and Transitions Datasets from the [**Center for Systemic Peace**](http://www.systemicpeace.org/inscrdata.html).

#* __resdummy__: Dummy variable for resource economies (“resdummy”). Takes values of 1 if resource rents  where resource rent minus “forest rent” is larger than 20% of GDP in 2013 or latest available, otherwise 0. Source: [**World Bank**](http://data.worldbank.org/indicator/NY.GDP.TOTL.RT.ZS).

#* __polity_youthr__: Youth ratio and polity2 interaction terms:  the product of the two variables above.

#* __gdp_youthr__: Youth ratio and GDP per capita interaction terms:  the product of the two variables above.


#In the following I collect the necessary variables and construct a unified database, unifying country codes and fixing some bugs in the in-built R functions (like the missing data for Turkey in the Maddsion package). This is a boring part and the code is not shown here (but executed).

#The following packages need to be installed (not included in the code as it could mess up execution)

'/'        

install.packages ("readr")

install.packages ("dplyr")

install.packages ("ggplot2")

install.packages ("xlsx")

install.packages("tidyr")

install.packages("countrycode")

install.packages("data.table")

install.packages("pwt8")

install.packages("WDI")

install.packages("maddison")

install.packages("openxlsx")

install.packages("readxl")

install.packages("h2o")

install.packages("pander")

library("readr")
library("dplyr")
library("ggplot2")
library("xlsx")
library("tidyr")
library("countrycode")
library("data.table")
library("pwt8")
library("WDI")
library("maddison")
library("openxlsx")
library("readxl")
library("h2o")
library("pander")

#getting IMF World Economic Outlook data
imf<-read.csv('https://www.imf.org/external/pubs/ft/weo/2015/02/weodata/WEOOct2015all.xls',
              header=TRUE, sep="\t",fill = TRUE,stringsAsFactors=FALSE )

setDT(imf)

## related column names
cols <- names(imf)[10:50]

## apply the anonymous function on all columns names specified in .SDcols and overwrite the original values
imf[, (cols) := lapply(.SD, function(x) as.numeric(gsub(',', '', x))), .SDcols = cols]

#narrowing down data to GDP per capita, fixing dates
imf_nrw<-imf[imf$WEO.Subject.Code=="PPPPC",]

imf_nrw<-imf_nrw %>% gather(date, gdp_pc_imf,-c(1:9))%>%select(WEO.Country.Code,ISO,date,gdp_pc_imf)
imf_nrw$date<-as.numeric(substring(imf_nrw$date,2,6))
imf_nrw<-imf_nrw[!is.na(imf_nrw$date),]
imf_nrw<-imf_nrw[order(imf_nrw$WEO.Country.Code),]


#getting US GDP deflator
dfltr<-imf[imf$Country=="United States"&imf$WEO.Subject.Code=="NGDP_D",c(10:50),with = FALSE]
dfltr<-t(dfltr)
dfltr<-as.numeric(dfltr)
dfltr<-data.frame(dfltr)
dfltr_lg<-as.data.frame(lapply(dfltr, rep, 189))

imf_nrw<-cbind(imf_nrw,dfltr_lg)
names(imf_nrw)<-c("code","ISO","year","gdp_pc_imf","defl")

#calculating 2011 real GDP per capita with USA GDP deflator
imf_nrw$gdp_pc_imf<-(imf_nrw$gdp_pc_imf/imf_nrw$defl)*imf_nrw$defl[imf_nrw$year==2011] #103.311

#generating iso2c and iso3c country codes
imf_nrw$iso2c <- countrycode(imf_nrw$code, "imf", "iso2c")
imf_nrw$iso3c <- countrycode(imf_nrw$code, "imf", "iso3c")

#fixing some missing/wrong country codes
imf_nrw$iso2c[imf_nrw$ISO=="UVK"]<-"XK"
imf_nrw$iso3c[imf_nrw$ISO=="UVK"]<-"KSV"

imf_nrw$iso2c[imf_nrw$ISO=="TUV"]<-"TV"
imf_nrw$iso3c[imf_nrw$ISO=="TUV"]<-"TUV"

imf_nrw<-imf_nrw[,c(3,4,7)]
imf_nrw<-droplevels(imf_nrw)

#Gettin Penn World Table GDP per capita data. 
#This will not be used in this round,just here for potential future use
#downloading Penn World Table data (2005 real USD). Original source: "http://www.rug.nl/research/ggdc/data/pwt/"

penn_data<-pwt8.1
penn_data<-mutate(penn_data,gdp_pc_pn=rgdpo/pop)
penn_data<-rename(penn_data,iso3c=isocode)
#generating iso2 code
penn_data<-mutate(penn_data,iso2c=countrycode(iso3c,"iso3c", "iso2c"))
#getting rid of Zimbabwe duplicates
penn_data<-filter(penn_data,penn_data$currency!="Zimbabwe Dollar")

#subsetting narrow penn data
penn_nrw<-penn_data[,c("iso3c","year","gdp_pc_pn")]
#converting to 2011 USD, using US GDP deflator 
penn_nrw$gdp_pc_pn<-penn_nrw$gdp_pc_pn*dfltr[32,]/dfltr[26,]

penn_nrw<-droplevels(penn_nrw)

#getting World Bank per capita GDP and resource rent variables
#WDIsearch('gdp.*capita.*constant',cache=new_cache)

#downloading World Bank data
wb_dat <- WDI(indicator=c('NY.GDP.PCAP.KD','NY.GDP.PCAP.PP.KD','NY.GDP.TOTL.RT.ZS','NY.GDP.FRST.RT.ZS'), country="all", start=1960, end=2014,extra=TRUE)

#fixing country codes
wb_dat$iso2c[wb_dat$country=="Kosovo"]<-"XK"
wb_dat$iso3c[wb_dat$country=="Kosovo"]<-"KSV"
wb_dat$iso3c[wb_dat$country=="Cabo Verde"]<-"CPV"

#renaming gdp per capita variables and creating a conversion factor
wb_dat<-mutate(wb_dat,rent=NY.GDP.TOTL.RT.ZS-NY.GDP.FRST.RT.ZS)
wb_dat<-rename(wb_dat,gdp_pc_wb=NY.GDP.PCAP.KD,gdp_pc_ppp_wb=NY.GDP.PCAP.PP.KD)
wb_dat<-mutate(wb_dat,conv_f=gdp_pc_ppp_wb/gdp_pc_wb)

wb_nrw<-wb_dat[,c("iso3c","year","gdp_pc_ppp_wb","gdp_pc_wb","rent")]
wb_nrw<-wb_nrw[!is.na(wb_nrw$iso3c),]
wb_nrw<-droplevels(wb_nrw)

#Maddison data is a historical GDP per capita (PPP) database

m<-maddison
m<-m[!is.na(m$year),]
m<-rename(m,gdp_pc_m=gdp_pc)
m$year<-as.numeric(format(m$year,'%Y'))

#fixing missing country codes
m$iso2c[m$country_original=="N. Zealand"]<-"NZ"
m$iso3c[m$country_original=="N. Zealand"]<-"NZL"

m$iso2c[m$country_original=="Kosovo"]<-"XK"
m$iso3c[m$country_original=="Kosovo"]<-"KSV"

m$iso2c[m$country_original=="Turk-menistan"]<-"TM"
m$iso3c[m$country_original=="Turk-menistan"]<-"TKM"

m$iso2c[m$country_original=="F. USSR"]<-"SU"
m$iso3c[m$country_original=="F. USSR"]<-"SUN"

m$iso2c[m$country_original=="F. USSR"]<-"SU"
m$iso3c[m$country_original=="F. USSR"]<-"SUN"

m$iso2c[m$country_original=="Czecho-slovakia"]<-"CS"
m$iso3c[m$country_original=="Czecho-slovakia"]<-"CSK"

m$iso2c[m$country_original=="F. Czecho-slovakia"]<-"CS"
m$iso3c[m$country_original=="F. Czecho-slovakia"]<-"CSK"

m$iso2c[m$country_original=="Yugoslavia"]<-"YU"
m$iso3c[m$country_original=="Yugoslavia"]<-"YUG"

m$iso2c[m$country_original=="F. Yugoslavia"]<-"YU"
m$iso3c[m$country_original=="F. Yugoslavia"]<-"YUG"

#fixing missing Turkey data

## download file to local folder
download.file('http://www.ggdc.net/maddison/maddison-project/data/mpd_2013-01.xlsx', 'mpd_2013-01.xlsx',mode="wb")
trk_new<-read.xlsx('mpd_2013-01.xlsx', startRow = 2,cols = c(1,124))

names(trk_new)<-c("year","gdp_pc_m")
trk_new<-trk_new%>%mutate(iso3c="TUR")
trk_new$gdp_pc_m<-as.numeric(trk_new$gdp_pc_m)
m<- merge(m,trk_new,id="iso3c",all=TRUE)

#getting rid of NAs
m<-m[m$year!="NA",]

#subsetting Maddison data
m_nrw<-m[m$iso3c!="YUG"&m$iso3c!="CSK"&m$iso3c!="SUN",c("iso3c","year","gdp_pc_m","country_original")]
m_nrw<-droplevels(m_nrw)

#converting to 2011 price levels, using IMF US 1990 real value from IMF data
imf_US_1990<-imf_nrw%>%filter(year==1990&iso3c=="USA")%>%select(gdp_pc_imf)
m_US_1990<-m_nrw%>%filter(year==1990&iso3c=="USA")%>%select(gdp_pc_m)
m_nrw$gdp_pc_m<-m_nrw$gdp_pc_m*as.numeric(imf_US_1990/m_US_1990)

#getting rid of NAs
m_nrw<-m_nrw[!is.na(m_nrw$iso3c),]
m_nrw<-droplevels(m_nrw)

#getting demographic data from the United Nations
#downloading zipped yearly UN population data with yearly age distribution
temp <- tempfile()
download.file("http://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/ASCII_FILES/WPP2015_INT_F3_Population_By_Age_Annual_Single_Medium.zip",temp, mode="wb")
data <- read.csv(unz(temp, "WPP2015_INT_F3_Population_By_Age_Annual_Single_Medium.csv"))
unlink(temp)

#calculating population aged 80 to 100+ where missing
data[is.na(data[,88]),88]<-rowSums(data[is.na(data[,88]),89:109])

#renaming columns
data<-rename(data,year=Time)
data<-rename(data,country=Location)


#calculating total population
data$total_pop<-rowSums(data[,8:88])

#subsetting total population from full demographic file
#(both sexes, origal dataset also have males and females)
demdata<-data[data$Sex=="Both",]

#creating iso2c, iso3c country codes
demdata$iso2c <- countrycode(demdata$LocID, "iso3n", "iso2c")
demdata$iso3c <- countrycode(demdata$LocID, "iso3n", "iso3c")
demdata<-demdata[!is.na(demdata$iso2c),]


#political data collection

#reading polity data from Center for Systemic Peace, 
#data description here: http://www.systemicpeace.org/inscrdata.html
library(foreign)
polity<-read.spss("http://www.systemicpeace.org/inscr/p4v2014.sav", to.data.frame = TRUE)

#converting country codes
polity$iso2c<-countrycode(polity$scode,"cowc","iso2c")
polity$iso3c<-countrycode(polity$scode,"cowc","iso3c")
polity$iso3c[polity$country=="Kosovo"]<-"KSV"
polity$iso3c[polity$scode=="ETI"]<-"ETH"
polity$iso3c[polity$scode=="FJI"]<-"FJI"
polity$iso3c[polity$scode=="GCL"]<-"COL"
polity$iso3c[polity$scode=="GFR"]<-"DEU"
polity$iso3c[polity$scode=="IVO"]<-"CIV"
polity$iso3c[polity$scode=="KOR"]<-"KOR"
polity$iso3c[polity$scode=="KOS"]<-"KSV"
polity$iso3c[polity$scode=="MNT"]<-"MNE"
polity$iso3c[polity$scode=="PKS"]<-"PAK"
polity$iso3c[polity$scode=="RUM"]<-"ROU"
polity$iso3c[polity$scode=="SDN"]<-"SDN"
polity$iso3c[polity$scode=="SER"]<-"SRB"
polity$iso3c[polity$scode=="UPC"]<-"CAN"
polity$iso3c[polity$scode=="VIE"]<-"VNM"
polity$iso3c[polity$scode=="YGS"]<-"SRB"
polity$iso3c[polity$scode=="ZAI"]<-"COD"
polity$iso3c[polity$scode=="SSU"]<-"SSD"

#getting rid of Ethiopia duplicate in 1993
polity<-polity%>%filter(cyear!=5301993)

polity_nrw<-polity[!is.na(polity$iso3c),]
polity_nrw<-polity_nrw[,c("iso3c","year","polity","polity2")]
polity_nrw<-droplevels(polity_nrw)

#reading political violence data
pol_viol<-read.spss("http://www.systemicpeace.org/inscr/MEPV2014.sav", to.data.frame = TRUE)

#converting country codes
pol_viol$iso2c<-countrycode(pol_viol$scode,"cowc","iso2c")
pol_viol$iso3c<-countrycode(pol_viol$scode,"cowc","iso3c")
pol_viol$iso3c[pol_viol$country=="Kosovo"]<-"KSV"
pol_viol$iso3c[pol_viol$scode=="ETI"]<-"ETH"
pol_viol$iso3c[pol_viol$scode=="FJI"]<-"FJI"
pol_viol$iso3c[pol_viol$scode=="GCL"]<-"COL"
pol_viol$iso3c[pol_viol$scode=="GFR"]<-"DEU"
pol_viol$iso3c[pol_viol$scode=="IVO"]<-"CIV"
pol_viol$iso3c[pol_viol$scode=="KOR"]<-"KOR"
pol_viol$iso3c[pol_viol$scode=="KOS"]<-"KSV"
pol_viol$iso3c[pol_viol$scode=="MNT"]<-"MNE"
pol_viol$iso3c[pol_viol$scode=="PKS"]<-"PAK"
pol_viol$iso3c[pol_viol$scode=="RUM"]<-"ROU"
pol_viol$iso3c[pol_viol$scode=="SDN"]<-"SDN"
pol_viol$iso3c[pol_viol$scode=="SER"]<-"SRB"
pol_viol$iso3c[pol_viol$scode=="UPC"]<-"CAN"
pol_viol$iso3c[pol_viol$scode=="VIE"]<-"VNM"
pol_viol$iso3c[pol_viol$scode=="YGS"]<-"SRB"
pol_viol$iso3c[pol_viol$scode=="ZAI"]<-"COD"
pol_viol$iso3c[pol_viol$scode=="SSU"]<-"SSD"

#generating political violence dummy
pol_viol$pol_viol_dummy=ifelse(pol_viol$civtot<3,0,1)
pol_viol$pol_viol_dummy1=ifelse(pol_viol$actotal==0,0,1)

pol_viol_nrw<-pol_viol[!is.na(pol_viol$iso3c),]
pol_viol_nrw<-pol_viol_nrw[,c("iso3c","year","inttot","civtot","actotal","pol_viol_dummy","pol_viol_dummy1")]
pol_viol_nrw<-droplevels(pol_viol_nrw)

#getting coup and other executive change database. All political data is from here: http://www.systemicpeace.org/inscrdata.html

## download file to local folder
download.file('http://www.systemicpeace.org/inscr/CSPCoupsAnnual2014.xls', 'CSPCoupsAnnual2014.xls',mode="wb")

coups<-read_excel('CSPCoupsAnnual2014.xls')

#creating coup and other regime change dummy (coups, attempted coups, self-coups, rebels ousting executive)
coups<-coups%>%mutate(coup_dummy=scoup1+atcoup2+agcoup+reboutex)%>%
        mutate(coup_dummy=ifelse(coup_dummy>0,1,0))

#fixing country codes
coups$iso3c<-countrycode(coups$scode,"cowc","iso3c")
coups$iso3c[coups$country=="Kosovo"]<-"KSV"
coups$iso3c[coups$scode=="ETI"]<-"ETH"
coups$iso3c[coups$scode=="FJI"]<-"FJI"
coups$iso3c[coups$scode=="GCL"]<-"COL"
coups$iso3c[coups$scode=="GFR"]<-"DEU"
coups$iso3c[coups$scode=="IVO"]<-"CIV"
coups$iso3c[coups$scode=="KOR"]<-"KOR"
coups$iso3c[coups$scode=="KOS"]<-"KSV"
coups$iso3c[coups$scode=="MNT"]<-"MNE"
coups$iso3c[coups$scode=="PKS"]<-"PAK"
coups$iso3c[coups$scode=="RUM"]<-"ROU"
coups$iso3c[coups$scode=="SDN"]<-"SDN"
coups$iso3c[coups$scode=="SER"]<-"SRB"
coups$iso3c[coups$scode=="UPC"]<-"CAN"
coups$iso3c[coups$scode=="VIE"]<-"VNM"
coups$iso3c[coups$scode=="YGS"]<-"SRB"
coups$iso3c[coups$scode=="ZAI"]<-"COD"
coups$iso3c[coups$scode=="SSU"]<-"SSD"
coups$iso3c[coups$scode=="USR"]<-"RUS"

#getting rid of Ethiopia duplicate in 1993
coups<-coups%>%filter(!(scode=="ETI"&year==1993))

#subsetting coup database
coups_nrw<-coups%>%select(year, iso3c,coup_dummy)

#merging data, first GDP

gdp_pc_merged <- merge(wb_nrw,penn_nrw,id="iso3c",all=TRUE)
gdp_pc_merged <- merge(gdp_pc_merged,m_nrw,id="iso3c",all=TRUE)
gdp_pc_merged <- merge(gdp_pc_merged,imf_nrw,id="iso3c",all=TRUE)

gdp_pc_merged <-gdp_pc_merged[!is.na(gdp_pc_merged$year),]
gdp_pc_merged$iso2c <- countrycode(gdp_pc_merged$iso3c, "iso3c", "iso2c")
gdp_pc_merged$country <- countrycode(gdp_pc_merged$iso3c, "iso3c", "country.name")

#generating imf growth rates
gdp_pc_merged<-gdp_pc_merged%>%mutate(gdp_pc_imf_grth=NA)
cnames<-levels(as.factor(gdp_pc_merged$iso3c))
yrs<-levels(as.factor(gdp_pc_merged$year))
yrs<-as.numeric(yrs)
yrs<-yrs[yrs<2021&yrs>1980]
yrs<-sort(yrs,decreasing = TRUE)
for (cn in cnames){
        for (yr in yrs){
                gdp_pc_merged$gdp_pc_imf_grth[gdp_pc_merged$year==yr&gdp_pc_merged$iso3c==cn]<-gdp_pc_merged$gdp_pc_imf[gdp_pc_merged$year==yr&gdp_pc_merged$iso3c==cn]/gdp_pc_merged$gdp_pc_imf[gdp_pc_merged$year==yr-1&gdp_pc_merged$iso3c==cn]
        }
}

#generating new gdp per capita series, based on Maddison or if not available, IMF 
gdp_pc_merged<-gdp_pc_merged%>%mutate(gdp_pc=gdp_pc_m)

for (j in 1980:2005){
        gdp_pc_merged$gdp_pc[gdp_pc_merged$year==j&is.na(gdp_pc_merged$gdp_pc)]<-gdp_pc_merged$gdp_pc_imf[gdp_pc_merged$year==j&is.na(gdp_pc_merged$gdp_pc)]
}
for (j in 2:length(gdp_pc_merged$year)){
        if (is.na(gdp_pc_merged$gdp_pc[j])) {
                gdp_pc_merged$gdp_pc[j]<-gdp_pc_merged$gdp_pc[j-1]*
                        gdp_pc_merged$gdp_pc_imf_grth[j]
        }
}

#adding South Sudan data from IMF
gdp_pc_merged$gdp_pc[gdp_pc_merged$iso3c=="SSD"]<-gdp_pc_merged$gdp_pc_imf[gdp_pc_merged$iso3c=="SSD"]

#generating growth rates for overall gdp per capita time series
gdp_pc_merged<-mutate(gdp_pc_merged,gdp_pc_grth=NA)
for (j in 2:length(gdp_pc_merged$year)){
        if (gdp_pc_merged$iso3c[j]==gdp_pc_merged$iso3c[j-1]) {
                gdp_pc_merged$gdp_pc_grth[j]<-gdp_pc_merged$gdp_pc[j]/
                        gdp_pc_merged$gdp_pc[j-1]-1
        }
}

#generating lagged growth rates (growth from 6 years prior to previous year, to avoid reverse causation)
gdp_pc_merged<-gdp_pc_merged%>%mutate(gdp_pc_grth_lgd=(shift(gdp_pc)/shift(gdp_pc,6))^.2-1)


#generating log GDP per capita variable
gdp_pc_merged<-gdp_pc_merged%>%mutate(ln_gdp_pc=log(gdp_pc))
#gdp_pc_merged<-gdp_pc_merged%>%mutate(gdp=gdp_pc*total_pop/1000000)


#creating dummies for countries where resource rent minus forest rent is larger than 20% of GDP in 2013 or latest available
gdp_pc_merged<-mutate(gdp_pc_merged,resdummy=0)

dummyc<-c("KW", "CG", "GQ", "LY", "SA", "IQ", "GA", "MR", "OM", "AZ", "BN", "AO", "QA", "TT", "MN", "TM", "KZ",
          "IR", "DZ", "VE", "SS", "PG", "SR", "AE", "BH", "TD", "CD", "UZ", "CG", "GQ", "LY", "SA", "IQ", "GA", "MR", "OM", "AZ", "BN", "AO", "QA", "TT", "MN", "TM", "KZ", "IR", "DZ", "VE", "SS", "PG", "SR", "SY", "AE", "BH", "TD", "CD", "UZ")

for (i in dummyc) {gdp_pc_merged$resdummy[gdp_pc_merged$iso2c==i]<-1
}


#adding political data
alldata_merged <- merge(gdp_pc_merged,polity_nrw,id="iso3c",all=TRUE)
alldata_merged <- merge(alldata_merged,pol_viol_nrw,id="iso3c",all=TRUE)
alldata_merged <- merge(alldata_merged,coups_nrw,id="iso3c",all=TRUE)
alldata_merged <-alldata_merged[!is.na(alldata_merged$year),]

#creating ultimate political instability (trouble) dummy, 
#1 if either there was a coup or domestic political violence
alldata_merged<-alldata_merged%>%mutate(sf_dummy=ifelse(polity==-77,1,0))

alldata_merged<-alldata_merged%>%mutate(trouble_dummy=pol_viol_dummy+coup_dummy+sf_dummy)%>%
        mutate(trouble_dummy=ifelse(trouble_dummy>0,1,0))

#end of datafile generation (apart from demographic data)


