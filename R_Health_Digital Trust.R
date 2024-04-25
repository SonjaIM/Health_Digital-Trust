################################################################################
################################################################################
#On Social Costs of Data Sovereignty and Data-Driven Decision-Making
#The Case of Health and Digital Trust
#Sonja Müller
################################################################################
################################################################################

#set working directory
#remove environment
rm(list=ls())

#load packages
library(tidyverse)
library(dplyr)
library(stargazer)
library(tidyr)
library(ggplot2)
library(broom)
library(eurostat)
library(sf)
library(countrycode)
library(formattable)
library(ggthemes)
library(RColorBrewer)

#load and subset data
ESS=read.csv("ESS10.csv")
ESSSC=read.csv("ESS10SC.csv")

ESS=ESS %>% select("cntry","gndr","agea","health","hlthhmp","mcpriv","mcmsinf","govmonpb","eisced","famadvs")
ESSSC=ESSSC %>% select("cntry","gndr","agea","health","hlthhmp","mcpriv","mcmsinf","panmonpb","eisced","famadvs")
colnames(ESSSC)[8]="govmonpb"
dat=rbind(ESS,ESSSC)

#health
#1: very good
#7 refusal, 8 don't know, 9 no answer --> delete
dat=dat%>%filter(health<7)
hist(dat$health)

#change range to 5: Very Good
dat=dat%>%
  mutate(health=ifelse(health==1,5,
                ifelse(health==2,4,
                ifelse(health==4,2,
                ifelse(health==5,1,3)))))


#hlthhmp
#1: yes, a lot
#7 refusal, 8 don't know, 9 no answer --> delete
dat=dat%>%filter(hlthhmp<7) 
hist(dat$hlthhmp)
colnames(dat)[5]="hmp"

#--> High IV = Good Health

#mcpriv: Online/mobile communication undermines personal privacy
#0: no
#77 refusal, 88 don't know, 99 no answer --> delete
dat=dat%>%filter(mcpriv<11) 
dat$mcpriv=dat$mcpriv+1
dat=dat%>%
  mutate(priv=ifelse(mcpriv==1,11,
                    ifelse(mcpriv==2,10,
                    ifelse(mcpriv==3,9,
                    ifelse(mcpriv==4,8,
                    ifelse(mcpriv==5,7,
                    ifelse(mcpriv==7,5,
                    ifelse(mcpriv==8,4,
                    ifelse(mcpriv==9,3,
                    ifelse(mcpriv==10,2,
                    ifelse(mcpriv==11,1,6)))))))))))

#mcmisnf: Online/mobile communication exposes people to misinformation
#0: no
#77 refusal, 88 don't know, 99 no answer --> delete
dat=dat%>%filter(mcmsinf<11) 
dat$mcmsinf=dat$mcmsinf+1
dat=dat%>%
  mutate(msinf=ifelse(mcmsinf==1,11,
                     ifelse(mcmsinf==2,10,
                     ifelse(mcmsinf==3,9,
                     ifelse(mcmsinf==4,8,
                     ifelse(mcmsinf==5,7,
                     ifelse(mcmsinf==7,5,
                     ifelse(mcmsinf==8,4,
                     ifelse(mcmsinf==9,3,
                     ifelse(mcmsinf==10,2,
                     ifelse(mcmsinf==11,1,6)))))))))))

#govmonpb: More important for governments to monitor and track the public or to maintain public privacy
#0: important to monitor and track the public
#66, not applicable, 77 refusal, 88 don't know, 99 no answer --> delete
dat=dat%>%filter(govmonpb<11) 
dat$govmonpb=dat$govmonpb+1
dat=dat%>%
  mutate(gov=ifelse(govmonpb==1,11,
         ifelse(govmonpb==2,10,
         ifelse(govmonpb==3,9,
                ifelse(govmonpb==4,8,
                ifelse(govmonpb==5,7,
                ifelse(govmonpb==7,5,
                ifelse(govmonpb==8,4,
                ifelse(govmonpb==9,3,
                ifelse(govmonpb==10,2,
                ifelse(govmonpb==11,1,6)))))))))))

#combine DVs
dat$dtrust=dat$priv+dat$msinf+dat$gov

#--> Low DV = Low Digital Trust

#eisced: Highest level of education, ES - ISCED
#1: ES-ISCED I , less than lower secondary
#55 other, 77 refusal, 88 don't know, 99 no answer --> delete
#1 not possible to harmonise - dalete?
dat=dat%>%filter(eisced<50) 
dat=dat%>%filter(eisced>1)
table(dat$eisced)

#famadvs: Advanced search, how familiar
#1: Not at all familiar
#7 refusal, 8 don't know, 9 no answer --> delete
dat=dat%>%filter(famadvs<7) 
table(dat$famadvs)

#gndr: Gender
#1: Male
#3 no answer --> delete
dat=dat%>%filter(gndr<3) 
dat$male=ifelse(dat$gndr==2, 0, 1)
table(dat$male)

#agea: Age
#999 no answer --> delete
dat=dat%>%filter(agea<999) 
table(dat$agea)

#CPI
unique(dat$cntry)
CPI=read.csv("CPI.csv",sep=";")
#CPI manually written into a separate file from website

d=merge(dat,CPI)

################################################################################
#Health x Hampered
d$factor_health=factor(d$health,labels=c("Very bad","Bad","Fair","Good","Very Good"))
d$factor_hmp=factor(d$hmp,labels=c("Yes a lot","Yes to some extent","No"))

TUMcols = colorRampPalette(c("#D7E4F4","#C2D7EF","#9ABCE4","#5E94D4",
                           "#165DB1","#14519A","#114584",
                           "#0E396E","#0A2D57","#072140"))(100)

d %>% group_by(factor_health) %>%
  count(factor_hmp) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(y=as_factor(factor_health),x=percent,fill=as_factor(factor_hmp))) +
  geom_col() + scale_fill_manual(values = c("#0A2D57", "#165DB1", "#C2D7EF")) + 
  labs(x="Proportion of Respondents [%]", y= "", fill = "Hampered in\ndaily activities")+
  theme_minimal() +
  theme(text=element_text(size=5,  family="serif"))


################################################################################
#Digital Trust Map
EU1=get_eurostat_geospatial(resolution = 10, 
                        nuts_level = 0, 
                        year = 2021)

EU1 %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()

dt=select(d,cntry,dtrust)
dt_sum=dt %>%
  group_by(cntry) %>%
  summarise(dtrust = mean(dtrust), n = n())
dt_sum$cntry=ifelse(dt_sum$cntry=="GB","UK",dt_sum$cntry)
colnames(EU1)[12]="cntry"

EUmaps=dt_sum %>% 
  select(cntry, dtrust) %>% 
  full_join(EU1, by = "cntry", all.x = TRUE, all.y = TRUE) %>% 
  st_as_sf()

EUmaps %>% 
  ggplot(aes(fill = dtrust)) +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_gradientn(breaks = c(11,16),colors = TUMcols) +
  labs(fill = "Digital\nTrust") +
  theme_void() +
  theme(text=element_text(size=12,  family="serif"))


################################################################################
#Table
table(d$cntry)

d$RR=0
d$RR=ifelse(d$cntry=="AT",33.7,d$RR)
d$RR=ifelse(d$cntry=="BE",39.19,d$RR)
d$RR=ifelse(d$cntry=="BG",72.5,d$RR)
d$RR=ifelse(d$cntry=="CH",49.5,d$RR)
d$RR=ifelse(d$cntry=="CY",14.72,d$RR)
d$RR=ifelse(d$cntry=="CZ",72.8,d$RR)
d$RR=ifelse(d$cntry=="DE",37.0,d$RR)
d$RR=ifelse(d$cntry=="EE",47.2,d$RR)
d$RR=ifelse(d$cntry=="ES",35.5,d$RR)
d$RR=ifelse(d$cntry=="FI",41.1,d$RR)
d$RR=ifelse(d$cntry=="GB",20.88,d$RR)
d$RR=ifelse(d$cntry=="GR",48.0,d$RR)
d$RR=ifelse(d$cntry=="HR",43.1,d$RR)
d$RR=ifelse(d$cntry=="HU",40.4,d$RR)
d$RR=ifelse(d$cntry=="IE",36.31,d$RR)
d$RR=ifelse(d$cntry=="IL",32.78,d$RR)
d$RR=ifelse(d$cntry=="IS",33.6,d$RR)
d$RR=ifelse(d$cntry=="IT",49.8,d$RR)
d$RR=ifelse(d$cntry=="LT",35.6,d$RR)
d$RR=ifelse(d$cntry=="LV",23.32,d$RR)
d$RR=ifelse(d$cntry=="MK",60.3,d$RR)
d$RR=ifelse(d$cntry=="NL",35.7,d$RR)
d$RR=ifelse(d$cntry=="NO",37.9,d$RR)
d$RR=ifelse(d$cntry=="PL",39.2,d$RR)
d$RR=ifelse(d$cntry=="PT",41.7,d$RR)
d$RR=ifelse(d$cntry=="RS",29.6,d$RR)
d$RR=ifelse(d$cntry=="SE",37.9,d$RR)
d$RR=ifelse(d$cntry=="SI",54.7,d$RR)
d$RR=ifelse(d$cntry=="SK",44.3,d$RR)

table=d %>% 
  group_by(cntry)%>%
  summarise(RRmean = mean(RR,na.rm=T),
            female = length(which(male==0)),
            malet = length(which(male==1)),
            agemean = mean(agea,na.rm=T),
            dtrustmean = mean(dtrust,na.rm=T),
            Total = n())

table$RRmean=round(table$RRmean,digits=1)
table$female=table$female/table$Total*100
table$female=round(table$female,digits=1)
table$malet=table$malet/table$Total*100
table$malet=round(table$malet,digits=1)
table$agemean=round(table$agemean,digits=1)
table$dtrustmean=round(table$dtrustmean,digits=1)

table$cntry=countrycode(table$cntry, origin = 'iso2c', destination = 'country.name') 

colnames(table)=c("Country","Response Rate [%]","Female [%]","Male [%]","Age Mean","Digital Trust Mean","Number of Respondents")
table=table[order(table$Country),]
formattable(table)
#* for self completion countries manually added

################################################################################
#Multiple Linear Regression
md=lm(dtrust~ health+hmp+hmp:health, data=d)
mpriv=lm(priv~ health+hmp+hmp:health, data=d)
mmsinf=lm(msinf~ health+hmp+hmp:health, data=d)
mgov=lm(gov~ health+hmp+hmp:health, data=d)

stargazer(md,mpriv,mmsinf,mgov,type="html",
          dep.var.labels = c("Digital Trust Index","Privacy","Misinformation","Government Focus"),
          covariate.labels = c("Subjective Health","Not Hampered by Health","Interaction Health:Hampered"),
          out="Regression_plain.htm")


#controls
mdtc=lm(dtrust~ health+hmp+hmp:health+eisced+CPI21+famadvs, data=d)
mprivc=lm(priv~ health+hmp+hmp:health+eisced+CPI21+famadvs, data=d)
mmsinfc=lm(msinf~ health+hmp+hmp:health+eisced+CPI21+famadvs, data=d)
mgovc=lm(gov~ health+hmp+hmp:health+eisced+CPI21+famadvs, data=d)

stargazer(mdtc,mprivc,mmsinfc,mgovc,type="html",
          dep.var.labels = c("Digital Trust Index","Privacy","Misinformation","Government Focus"),
          covariate.labels = c("Subjective Health","Not Hampered by Health",
                               "Interaction Hampered:Health",
                               "Education","CPI","Familiarity with Advanced Search"),
          out="Regression_controls.htm")


#Coefficient plot
cplot=d  %>%
  group_by(cntry) %>%
  do(tidy(lm(dtrust ~ hmp, data = .)))

cplotCPI=merge(cplot, CPI)

cplotCPI$cntry=countrycode(cplotCPI$cntry, origin = 'iso2c', destination = 'country.name') 

cplotCPI%>% filter(term == "hmp") %>%
  ggplot(aes(x=as_factor(cntry),y=estimate,
               ymax = estimate + 1.96*std.error,
               ymin = estimate - 1.96*std.error)) +
  geom_pointrange() + 
  geom_point(size=4, aes(colour=CPI21)) +
  scale_color_gradientn(colors = TUMcols) +
  labs(x="",y="Coefficient Estimate") +
  ggtitle("Association between Not Hampered by Health and Digital Trust by Country") +
  geom_hline(yintercept=0, linetype=2) +
  scale_x_discrete(name = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.3, hjust=.25)) +
  theme(text=element_text(size=12,  family="serif"))+
  labs(color= "CPI")
