library(dplyr)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )


homi.r <- read.csv("data/database.csv")
suppressMessages(attach(homi.r))
options(warn=-1)
# head(homi.r)
#cityincident <- homi.r %>% 
#  ungroup() %>%
 # select(City,Incident)
#cityincident <- aggregate(cityincident$Incident, by=list(City=cityincident$City), FUN=sum)

warn.conflicts = FALSE

suppressMessages(library(tidyr))
suppressMessages(library(stringr))
suppressMessages(library(stringi))
suppressMessages(library(forcats))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(Hmisc))

str(homi.r)
dim(homi.r)
anyDuplicated(homi.r$Record.ID)
names(homi.r)
names(homi.r) <- tolower(names(homi.r))

names(homi.r)
sapply(homi.r[1,],class)
length(homi.r$crime.solved[homi.r$crime.solved == "Yes"])
homi.r$victim.age[homi.r$victim.age==998]<-98

homi.r$perpetrator.age[is.na(homi.r$perpetrator.age)]<- 1
levels(homi.r$crime.solved)


##############

homi.r.solved <- homi.r %>% filter(
  crime.solved    == "Yes" &
    victim.sex      != "Unknown" &
    perpetrator.sex != "Unknown" &
    relationship    != "Unknown"
)%>%
  droplevels()


homi.r.solved$victim.age[homi.r.solved$victim.age==998]<-98

homi.r.solved$perpetrator.age[is.na(homi.r.solved$perpetrator.age)]<- 1


girl.boy.crime<-homi.r.solved %>% filter(
  relationship    %in% c("Girlfriend","Boyfriend") &
    victim.age      >= 18 &
    perpetrator.age >= 18 &
    victim.sex      != "Unknown" &
    perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,victim.race,victim.ethnicity,perpetrator.sex,
         perpetrator.age,perpetrator.race,perpetrator.ethnicity,relationship,weapon,record.source)

# table(girl.boy.crime$victim.sex)
# Female   Male 
#   7468   3619
# table(girl.boy.crime$perpetrator.sex)
# Female   Male 
#   3460   7627

# something wrong !!!

girl.boy.crime<-girl.boy.crime[!(girl.boy.crime$victim.sex==girl.boy.crime$perpetrator.sex),]

# table(girl.boy.crime$victim.sex)
# Female   Male 
# 7434   3426 
# table(girl.boy.crime$perpetrator.sex)
# Female   Male 
# 3426   7434 

# now it is OK 

############################################

# Who killed Who (Familicide). Unfortunately !


homi.r.solved$who.killed.who <- ifelse(homi.r.solved$relationship=="Brother" &
                                         homi.r.solved$perpetrator.sex =="Male" ,
                                       "Brother Killed by Brother",
                                       ifelse(homi.r.solved$relationship=="Brother" &
                                                homi.r.solved$perpetrator.sex =="Female" ,
                                              "Brother Killed by Sister",
                                              ifelse(homi.r.solved$relationship=="Sister" &
                                                       homi.r.solved$perpetrator.sex =="Female" ,
                                                     "Sister Killed by Sister",
                                                     ifelse(homi.r.solved$relationship=="Sister" &
                                                              homi.r.solved$perpetrator.sex =="Male" ,
                                                            "Sister Killed by Brother",
                                                            ifelse(homi.r.solved$relationship=="Father" &
                                                                     homi.r.solved$perpetrator.sex =="Female" ,
                                                                   "Father Killed by Daughter",
                                                                   ifelse(homi.r.solved$relationship=="Father" &
                                                                            homi.r.solved$perpetrator.sex =="Male" ,
                                                                          "Father Killed by Son",
                                                                          ifelse(homi.r.solved$relationship=="Mother" &
                                                                                   homi.r.solved$perpetrator.sex =="Male" ,
                                                                                 "Mother Killed by Son",
                                                                                 ifelse(homi.r.solved$relationship=="Mother" &
                                                                                          homi.r.solved$perpetrator.sex =="Female" ,
                                                                                        "Mother Killed by Daughter",
                                                                                        ifelse(homi.r.solved$relationship=="Wife" &
                                                                                                 homi.r.solved$perpetrator.sex =="Male" ,
                                                                                               "Wife Killed by Husband",
                                                                                               ifelse(homi.r.solved$relationship=="Husband" &
                                                                                                        homi.r.solved$perpetrator.sex =="Female" ,
                                                                                                      "Husband Killed by Wife",
                                                                                                      ifelse(homi.r.solved$relationship=="Son" &
                                                                                                               homi.r.solved$perpetrator.sex =="Female" ,
                                                                                                             "Son Killed by Mother",
                                                                                                             ifelse(homi.r.solved$relationship=="Son" &
                                                                                                                      homi.r.solved$perpetrator.sex =="Male" ,
                                                                                                                    "Son Killed by Father","UKN"
                                                                                                             ))))))))))))
# Im sure there is a simpler way to do the same, but really i do not have Time ..!

ex.husband.ex.wife.crime<-homi.r.solved %>% filter(
  relationship     %in% c("Ex-Husband","Ex-Wife") &
    victim.age       >= 18 &
    perpetrator.age  >= 18 &
    victim.sex      != "Unknown" &
    perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)

# there are Female in both features victim .sex and perpetrator.sex ..and Male as well which is wrong..!
# so I consider that the victim sex to be my reference because it is the subject of this dataset.
ex.husband.ex.wife.crime$relationship <- ifelse(ex.husband.ex.wife.crime$victim.sex=="Male",
                                                "Ex-Husband","Ex-Wife")
# I think there was some typo here 

# OK let's move on

ex.husband.ex.wife.crime$older.or.younger <- ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Husband" &
                                                      ex.husband.ex.wife.crime$perpetrator.age < ex.husband.ex.wife.crime$victim.age,
                                                    "Ex-wife Killed an old Ex-Husband",
                                                    ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Husband" &
                                                             ex.husband.ex.wife.crime$perpetrator.age > ex.husband.ex.wife.crime$victim.age,
                                                           "Ex-wife Killed a young Ex-Husband",
                                                           ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Wife" &
                                                                    ex.husband.ex.wife.crime$perpetrator.age > ex.husband.ex.wife.crime$victim.age,
                                                                  "Ex- Husband Killed a young Ex-Wife",
                                                                  ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Wife" &
                                                                           ex.husband.ex.wife.crime$perpetrator.age < ex.husband.ex.wife.crime$victim.age,
                                                                         "Ex-Husband Killed an old Ex-Wife","Smae Age"))))
ndf.by.weapon <- homi.r.solved %>% filter(
  weapon != "Unknown" &
    victim.sex != "Unknown"&
    perpetrator.sex != "Unknown" &
    victim.age       >= 18 &
    perpetrator.age  >= 18
) %>% 
  droplevels()%>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)

homi.r.solved$who.killed.who.sex <- ifelse(homi.r.solved$perpetrator.sex=="Female"& homi.r.solved$victim.sex=="Male",
                                           "Male Killed by Female",
                                           ifelse(homi.r.solved$perpetrator.sex=="Male"& homi.r.solved$victim.sex=="Female",
                                                  "Female Killed by Male",
                                                  ifelse(homi.r.solved$perpetrator.sex =="Male" & homi.r.solved$victim.sex == "Male",
                                                         "Male Killed by Male",
                                                         ifelse(homi.r.solved$perpetrator.sex =="Female" & homi.r.solved$victim.sex == "Female",
                                                                "Female Killed by Female", "UNK"))))






















employee.employer.crime<-homi.r.solved %>% filter(
  relationship     %in% c("Employee","Employer") &
    victim.age       >= 18 &
    perpetrator.age  >= 18 &
    victim.sex      != "Unknown" &
    perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon,who.killed.who.sex)
friend.crime<-homi.r.solved %>% filter(
  relationship       == "Friend" &
    victim.sex      != "Unknown" &
    victim.age       >= 18 &
    perpetrator.age  >= 18 &
    perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)

friend.crime$who.killed.who.Friend.sex <- ifelse(friend.crime$perpetrator.sex == "Female" & friend.crime$victim.sex == "Male",
                                                 "Friends Male Killed by Female",
                                                 ifelse(friend.crime$perpetrator.sex == "Male"& friend.crime$victim.sex == "Female",
                                                        "Friends Female Killed by Male",
                                                        ifelse(friend.crime$perpetrator.sex == "Male" & friend.crime$victim.sex == "Male",
                                                               "Friends Male Killed by Male",
                                                               ifelse(friend.crime$perpetrator.sex == "Female" & friend.crime$victim.sex == "Female",
                                                                      "Friends Female Killed by Female", "UNK"))))


by.race <-homi.r.solved %>% filter(
  victim.age       >= 18 &
    perpetrator.age  >= 18 &
    victim.race      != "Unknown" &
    perpetrator.race != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.race,perpetrator.race,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)


by.weapon <- dplyr::summarise(group_by(ndf.by.weapon,weapon),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

by.weapon.sex <- dplyr::summarise(group_by(ndf.by.weapon,weapon,victim.sex),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

ndf.by.weapon$sex.weapon.used <- sprintf("%s Killed by %s Using a %s",
                                         ndf.by.weapon$victim.sex,
                                         ndf.by.weapon$perpetrator.sex,
                                         ndf.by.weapon$weapon)


by.ndf.by.weapon.used <- dplyr::summarise(group_by(ndf.by.weapon,sex.weapon.used),
                                   freq.by.weapon.used =n())%>%
  arrange(desc(freq.by.weapon.used))


ndf.by.weapon.geom.points <-ndf.by.weapon %>% filter(
  sex.weapon.used   %in% c("Male Killed by Male Using a Handgun",
                           "Male Killed by Male Using a Knife",
                           "Female Killed by Male Using a Handgun",
                           "Male Killed by Male Using a Blunt Object",
                           "Female Killed by Male Using a Knife")
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon,sex.weapon.used) 

homi.r.solved.age <- homi.r.solved %>% filter(
  victim.age       >= 18 &
    perpetrator.age  >= 18 &
    victim.race      != "Unknown" &
    perpetrator.race != "Unknown"
) %>%
  droplevels()



homi.r.solved.age$vic.age.group <- cut(homi.r.solved.age$victim.age, 
                                       breaks = c(18, 30, 40, 50,60, 70, 80, 90, 100), 
                                       labels = c("18-30 yrs", "30-40 yrs",
                                                  "40-50 yrs", "50-60 yrs","60-70 yrs","70-80 yrs",
                                                  "80-90 yrs","90-100 yrs"),
                                       right = FALSE)

homi.r.solved.age$per.age.group <- cut(homi.r.solved.age$perpetrator.age, 
                                       breaks = c(18, 30, 40, 50,60, 70, 80, 90, 100), 
                                       labels = c("18-30 yrs", "30-40 yrs",
                                                  "40-50 yrs", "50-60 yrs","60-70 yrs","70-80 yrs",
                                                  "80-90 yrs","90-100 yrs"),
                                       right = FALSE)















by.year <- dplyr::summarise(group_by(homi.r.solved,year),freq.year =n())%>%
  arrange(desc(freq.year)) 

by.month <- dplyr::summarise(group_by(homi.r.solved,month),freq.month =n())%>%
  arrange(desc(freq.month))


by.family <- dplyr::summarise(group_by(homi.r.solved[homi.r.solved$who.killed.who!="UKN", ],who.killed.who),total.number.re =n())%>%
  arrange(desc(total.number.re))

by.state <- dplyr::summarise(group_by(homi.r.solved,state),freq.by.state =n())%>%
  arrange(desc(freq.by.state))


by.weapon <- dplyr::summarise(group_by(ndf.by.weapon,weapon),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

by.weapon.sex <- dplyr::summarise(group_by(ndf.by.weapon,weapon,victim.sex),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

ndf.by.weapon$sex.weapon.used <- sprintf("%s Killed by %s Using a %s",
                                         ndf.by.weapon$victim.sex,
                                         ndf.by.weapon$perpetrator.sex,
                                         ndf.by.weapon$weapon)


by.ndf.by.weapon.used <- dplyr::summarise(group_by(ndf.by.weapon,sex.weapon.used),
                                   freq.by.weapon.used =n())%>%
  arrange(desc(freq.by.weapon.used))
empyr.empee.sex <- employee.employer.crime %>%
  group_by(victim.sex, 
           perpetrator.sex,
           who.killed.who.sex,
           relationship) %>%
  dplyr::summarise(sex.freq = n()) %>%
  arrange(victim.sex, perpetrator.sex)

by.p.race.group <- dplyr::summarise(group_by(by.race,perpetrator.race,victim.race),total.by.race = n()) %>%
  arrange(total.by.race)


by.p.race.group$who.killed.who.race <- sprintf("%s Killed by %s",
                                               by.p.race.group$victim.race,
                                               by.p.race.group$perpetrator.race)

by.p.race.group$freq <- round(by.p.race.group$total.by.race/sum(by.p.race.group$total.by.race)*100,2)


by.p.race.group$freq<- paste(by.p.race.group$freq,"%",sep="")


top4.by.race <- by.p.race.group[16:13,c(4,3,5)]


table.by.race <- tableGrob(top4.by.race[ ,c(1,3)], rows=NULL)

by.v.age.group <- dplyr::summarise(group_by(homi.r.solved.age,vic.age.group),total.by.group.v = n()) %>%
  arrange(total.by.group.v)

by.p.age.group <- dplyr::summarise(group_by(homi.r.solved.age,per.age.group),total.by.group.p = n()) %>%
  arrange(total.by.group.p)


by.v.p.age.group <- dplyr::summarise(group_by(homi.r.solved.age,vic.age.group,per.age.group,perpetrator.sex,
                                       victim.sex),
                              total.by.group.vp = n()) %>%
  arrange(total.by.group.vp)




#################################################
homi.theme<-theme(
  axis.text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size = 14),
  panel.grid.major = element_line(color = "grey"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "snow1"),
  legend.position = "right",
  legend.justification = "top", 
  legend.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, size = 1))
####################################################

tt.homi.f <- ttheme_minimal(
  core=list(bg_params = list(fill = "azure", col="darkblue"),
            fg_params=list(fontface=6)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)))




ggplot(by.v.p.age.group,aes(x = per.age.group, vic.age.group,y = total.by.group.vp))+
  geom_bar(stat = "identity",fill="steelblue",width=0.5)+
  facet_wrap(~ vic.age.group)+
  homi.theme+
  theme(axis.text.x=element_text(size= 7, 
                                 angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))+
  theme(strip.text = element_text(size=10,color ="darkblue"))+
  ggtitle("Age Group 18+ \n Victim VS perpedator")+
  labs(x="Perpedator",
       y="Number of Incidents")