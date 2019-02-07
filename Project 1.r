##library 
library(tidyverse)
library(ggplot2)
library(stringr)
library(plotly)
library(gganimate)
library(ggmap)
library(geosphere)
library(maps)
register_google(key="AIzaSyDsxH8fNkzjuhqDahhLTpAaH1RAgkeK8O8")
cd="/Users/rli/Desktop/DV/Project 1/gun-violence-database/"

#static 1 Mass shooting deaths and injuries
#Data Processing
mshoting<-read.csv(paste(cd,"mass_shootings_all.csv",sep=""),header=T,stringsAsFactor=F)
head(mshoting)
mshoting<-mshoting[,-c(3,4,7)]
head(mshoting)
Iyy<-c()
for (i in mshoting[1])
{
  dummy<-strptime(i,"%B %d, %Y")
  dummy<-as.character(dummy)
  yy<-substr(dummy,1,4)
  Iyy<-cbind(Iyy,yy)
}
mshoting$iyear<-Iyy
label_data=mshoting
number_of_bar=nrow(label_data)
data<-mshoting %>% distinct(State,.keep_all = TRUE)
##
p=ggplot(mshoting,aes(x=as.factor(mshoting$State),y=(mshoting$X..Injured+mshoting$X..Killed)))
p=p+geom_bar(stat="identity",fill=alpha("blue",0.3))+theme_minimal()+ylim(-100,300)
p=p+coord_polar(start = 0)
p=p+theme(axis.title = element_blank(),plot.margin = unit(rep(-1,4), "cm"))
p

#static 2 The number of deaths and injuries of mass shooting
#Data Cleanning
ms<-read.csv(paste(cd,"mass_shootings_all.csv",sep=""))
mstate<-aggregate(x=ms$X..Injured+ms$X..Killed,by=list(ms$State),FUN=sum)
names(mstate)=c("region","Killed or Injured")
mstate
us_states<-map_data("state")
head(us_states)
dim(us_states)
#Data Processing
mstate$region <- tolower(mstate$region)
us_states_vio <- left_join(us_states, mstate)
p0 <- ggplot(data = us_states_vio,mapping = aes(x = long, y = lat,group = group,fill=us_states_vio$`Killed or Injured`))
p1<-p0+geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)+coord_map(projection = "albers", lat0 = 39, lat1 = 45) 
p2 <-p1 + scale_fill_gradientn(colours=heat.colors(15)) +labs(title = "Mass Gun Shoting: Killed or Injuries From 2014 to 2016")
p2

#Static 3 the comparision of mass gun cases by district
west=c('California',' Oregon',' Washington',' Nevada',' Idaho',	' Utah',' Arizona',' Montana',' Alaska',' Hawaii')
central=c('Wyoming',	' Colorado',	' New Mexico',	' North Dakota',	' South Dakota',	' Nebraska',	' Kansas',	' Oklahoma',	' Texas',	' Minnesota',	' Iowa',	' Missouri',	' Arkansas',	' Louisiana',	' Wisconsin',	' Illinois',	' Mississippi')
east=c('Michigan',	' Indiana',	' Kentucky',	' Tennessee',	' Alabama',	' Ohio',	' Georgia',	' Florida',	' South Carolina',	' North Carolina',	' Virginia',	' West Virginia',	' Delaware',	' Maryland',	' New Jersey',	' Pennsylvania',	' New York',	' Connecticut',	' Rhode Island',	' Massachusetts',	' Vermont',	' New Hampshire',	' Maine')
mass_shootings_all<-read.csv(paste(cd,"mass_shootings_all.csv",sep=""),header=T,stringsAsFactor=F)
#Deciding district
whichstate=function(x){
	if(length(grep(x,west))>0) {
		statecode="west"
	}else{
		if(length(grep(x,central))>0) {
			statecode="central"
		}else {statecode="east"}
	}
	return(statecode)
	}


mass_shootings_all$district=unlist(lapply(mass_shootings_all$State,whichstate))

getmonth1 <- function(data){
	library(stringr)
	data$month=word(data$Incident.Date,1)
	newdata=data[,c("district","month")]
	return(newdata)
}

tot_shooting=getmonth1(mass_shootings_all)
tot_shooting=tot_shooting %>% group_by(district,month) %>% summarise(num=n())#Count
tot_shooting$month=factor(tot_shooting$month,levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

p=ggplot(tot_shooting,aes(x=month, y=num, fill=district))+geom_bar(stat='identity',position="dodge")
p=p+labs(x="month",title="Mass shooting issues by district") 
p

#animate 1 the mass shooting cases changed by year
#Data Processing
mass_shootings_2014<-read.csv(paste(cd,"mass_shootings_2014.csv",sep=""),header=T,stringsAsFactor=F)
mass_shootings_2014$year=2014

mass_shootings_2015<-read.csv(paste(cd,"mass_shootings_2015.csv",sep=""),header=T,stringsAsFactor=F)
mass_shootings_2015$year=2015


mass_shootings_2016<-read.csv(paste(cd,"mass_shootings_2016.csv",sep=""),header=T,stringsAsFactor=F)
mass_shootings_2016$year=2016

getmonth2 <- function(data){
	library(stringr)
	data$month=word(data$Incident.Date,1)
	newdata=data[,c("year","month")]
	return(newdata)
}

shootings_2014=getmonth2(mass_shootings_2014)
shootings_2015=getmonth2(mass_shootings_2015)
shootings_2016=getmonth2(mass_shootings_2016)

shoot_year=rbind(shootings_2014,shootings_2015,shootings_2016)
shoot_year=shoot_year %>% group_by(year,month) %>% summarise(num=n())
shoot_year$month=factor(shoot_year$month,levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

#Make up for 2016 Dec for "0" issues
shoot_year[nrow(shoot_year)+1,]=c(2016,"December",0)

g <- ggplot(shoot_year, aes(month, num, size=num,color = month)) +
	 geom_point(alpha = 0.7, show.legend = FALSE) +
   #scale_size(range = c(2, 12)) +
   transition_manual(year)+
	 labs(title = 'Year: {current_frame}', x = 'month') 
g


##animate 2 the comparision between deaths and injuries children 
#for the lack of months for teens injuries, we use children to compare 
children_injured<-read.csv(paste(cd,"children_injured.csv",sep=""),header=T,stringsAsFactor=F)
children_injured$group="children_injured"

children_killed<-read.csv(paste(cd,"children_killed.csv",sep=""),header=T,stringsAsFactor=F)
children_killed$group="children_killed"
getmonth3 <- function(data){
	library(stringr)
	data$month=word(data$Incident.Date,1)
	newdata=data[,c("group","month")]
	return(newdata)
}

children_inj=getmonth3(children_injured)
children_kil=getmonth3(children_killed)


shoot_young=rbind(children_inj,children_kil)
shoot_young=shoot_young %>% group_by(group,month) %>% summarise(num=n())
shoot_young$month=factor(shoot_young$month,levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

g <- ggplot(shoot_young, aes(month, num, size=num,color = month)) +
	 geom_point(alpha = 0.7, show.legend = FALSE) +
   scale_size(range = c(2, 12)) +
	 transition_manual(group)+
	 labs(title = 'type: {current_frame}', x = 'month')
g


##animate 3: the number of mass shooting cases changed by city level
mass_shootings_all<-read.csv(paste(cd,"mass_shootings_all.csv",sep=""),header=T,stringsAsFactor=F)
topcity=c("Chicago","Boston","New York (Manhattan)","Los Angeles","San Francisco",'Washington',	'Miami,')
middlecity=c('Birmingham',	'Phoenix',	'Los Angeles (Hollywood)',	'Los Angeles (county)',	'San Diego',	'Denver',	'Bridgeport',	'Wilmington',		'Miami Gardens',	'Miami (Goulds)',	'Miami-dade (county)',	'Atlanta',	'Chicago (Englewood)',	'Indianapolis',	'Kansas City',	'Des Moines',	'Louisville',	'New Orleans',	'Baltimore',	'Minneapolis (Brooklyn Center)',	'Minneapolis',	'Detroit',	'Omaha',	'Las Vegas',	'Las Vegas',	'Brooklyn',	'Albuquerque',	'Newark',	'Columbus',	'Charlotte',	'Wilmington',	'Portland',	'Pittsburgh',	'Pittsburgh (Wilkinsburg)',	'Pittsburgh (Mount Oliver)',	'Philadelphia',	'Memphis',	'Columbia',	'Houston',	'Austin',	'Dallas',	'Seattle',	'Burlington',	'Charleston',	'Milwaukee')
#City level
whichlevelcity=function(x){
	if(length(grep(x,topcity))>0) {
		statecode="topcity"
	}else{
		if(length(grep(x,middlecity))>0) {
			statecode="middlecity"
		}else {statecode="others"}
	}
	return(statecode)
	}
mass_shootings_all$citylevel=unlist(lapply(mass_shootings_all$City.Or.County,whichlevelcity))

getmonth4 <- function(data){
	library(stringr)
	data$month=word(data$Incident.Date,1)
	newdata=data[,c("citylevel","month")]
	return(newdata)
}

citylevel_shooting=getmonth4(mass_shootings_all)
citylevel_shooting=citylevel_shooting %>% group_by(citylevel,month) %>% summarise(num=n())
citylevel_shooting$month=factor(citylevel_shooting$month,levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

dx=as.data.frame(tapply(mass_shootings_all$City.Or.County,mass_shootings_all$citylevel,function(x)length(unique(x))))
dx$citylevel=row.names(dx)
row.names(dx)=1:nrow(dx)
names(dx)=c("index","citylevel")
citylevel_shooting=merge(citylevel_shooting,dx,"citylevel",all.x=T)
citylevel_shooting$newnum=round(citylevel_shooting$num/citylevel_shooting$index,4)

g <- ggplot(citylevel_shooting, aes(month, newnum, size=newnum,color = month)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  transition_manual(citylevel)+
  labs(title = 'type: {current_frame}', x = 'month')
g

#interactive 1 the number of accidental deaths or injuries,including teens,children
#Data Processing
accidental_injuries=read.csv(paste(cd,"accidental_injuries.csv",sep=""))
accidental_injuries_teens=read.csv(paste(cd,"accidental_injuries_teens.csv",sep=""))
accidental_injuries_children=read.csv(paste(cd,"accidental_injuries_children.csv",sep=""))
accidental_deaths=read.csv(paste(cd,"accidental_deaths.csv",sep=""))
accidental_deaths_teens=read.csv(paste(cd,"accidental_deaths_teens.csv",sep=""))
accidental_deaths_children=read.csv(paste(cd,"accidental_deaths_children.csv",sep=""))
accidental_d_and_i1=rbind(accidental_deaths,accidental_injuries)
accidental_d_and_i2=rbind(accidental_deaths_children,accidental_injuries_children)
accidental_d_and_i3=rbind(accidental_deaths_teens,accidental_injuries_teens)
accidental_d_and_i4=rbind(accidental_d_and_i2,accidental_d_and_i3)
accidental_d_and_i=rbind(accidental_d_and_i1,accidental_d_and_i4)
num_of_people<-aggregate(x=accidental_d_and_i$X..Injured+accidental_d_and_i$X..Killed,by=list(accidental_d_and_i$State),FUN=sum)
#convert to abbr state name
AbbrState=c()
for (i in num_of_people[,1]){
  AbbrState<-c(AbbrState,state.abb[grep(i, state.name)])
}
num_of_people$State<-AbbrState
num_of_people<-num_of_people[-1]
names(num_of_people)=c("k_o_i","region")
#
trace1 <- list(
  z = num_of_people[,1],
  autocolorscale =FALSE, 
  colorscale = list(c(0, "rgb(221, 42, 145)"),list(0.35, "rgb(177, 77, 236)"),list(0.5, "rgb(118, 117, 237)"),list(0.6, "rgb(46, 142, 191)"),list(0.7, "rgb(11, 152, 121)"),list(1, "rgb(19, 152, 99)")), 
  locationmode = "USA-states", 
  locations =num_of_people[,2],
  locationssrc = "muttreja96shaurya:8:9694e3", 
  name = "D", 
  type = "choropleth", 
  uid = "777494", 
  zauto = TRUE, 
  zmax = 84, 
  zmin = 1, 
  zsrc = "muttreja96shaurya:8:1a3a13"
)
data<-list(trace1)
layout<-list(
  autosize = TRUE, 
  geo = list(scope = "usa"), 
  hovermode = "closest", 
  title = "Accidental Killed or Injured in State"
)
p <- plot_ly()
p <- add_trace(p, z=trace1$z, autocolorscale=trace1$autocolorscale, colorscale=trace1$colorscale, locationmode=trace1$locationmode, locations=trace1$locations, locationssrc=trace1$locationssrc, name=trace1$name, type=trace1$type, uid=trace1$uid, zauto=trace1$zauto, zmax=trace1$zmax, zmin=trace1$zmin, zsrc=trace1$zsrc)
p <- layout(p, autosize=layout$autosize, geo=layout$geo, hovermode=layout$hovermode, title=layout$title)
p


#interactive 2
#the number of accidental deaths or injuries,including teens,children compared by month
#Data Processing
accidental_deaths<-read.csv(paste(cd,"accidental_deaths.csv",sep=""),header=T,stringsAsFactor=F)
accidental_deaths$group="accidental_deaths"
accidental_deaths_children<-read.csv(paste(cd,"accidental_deaths_children.csv",sep=""),header=T,stringsAsFactor=F)
accidental_deaths_children$group="accidental_deaths_children"
accidental_deaths_teens<-read.csv(paste(cd,"accidental_deaths_teens.csv",sep=""),header=T,stringsAsFactor=F)
accidental_deaths_teens$group="accidental_deaths_teens"
getmonth <- function(data){
  library(stringr)
  data$month=word(data$Incident.Date,1)
  newdata=data[,c("group","month")]
  return(newdata)
}

tot_death=getmonth(accidental_deaths)
children_death=getmonth(accidental_deaths_children)
teens_death=getmonth(accidental_deaths_teens)
dea=rbind(tot_death,teens_death,children_death)
dea=dea %>% group_by(group,month) %>% summarise(num=n())##Count
dea$month=factor(dea$month,levels=c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
plot_ly(dea, x = ~month, y=~num,color = ~group,type="bar")  %>% layout(title = 'All_kind_accidental_deaths by month')
