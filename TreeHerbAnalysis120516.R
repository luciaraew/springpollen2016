#getwd()
#setwd("/Users/luciaweinman/R")

rm(list = ls())
setwd("~/Desktop/Fall_2014/Research/Early spring bees/data")


#######updated with dets, so only using forest bees
#visitation data
#visits<-read.csv("Official bee pinning log_LW091316.csv" ,header = T, stringsAsFactors = F)
visits<-read.csv("Official bee pinning log_CS101316.csv" ,header = T, stringsAsFactors = F)
visits$genus.species<-tolower(visits$genus.species)
visits2<-visits[,c(1, 7:15)]
head(visits)

#bee species data
dets<-read.csv("SpringPollen2016_dets.csv", stringsAsFactors = F)
dets$uniqueID<-substr(dets$beeid, 15, 18)
head(dets)

#outline
#1) Pollen
#a. subset all data to only include females from forest genera
#c. pick primary pollen: in vector called t.primary
#d. pick mismatch pollen
#e. for both these get rid of morphotypes and upload spreadsheet to give the full name


#########1) Visitation##############

#a. subset all data to only include forest-associated bees

#make a vector of forest bee genera (Andrena, Augochlora, Augochloropsis, Bombus, Colletes, Lasioglossum and Osmia,)
forest<-read.csv("forest_beesONLY.csv", header = T)
forest<-c("Andrena", "Augochlora", "Augochloropsis", "Bombus", "Colletes", "Lasioglossum", "Osmia", "GreenBeeNEEDSID")

#add a column to the 'dets' df with just the genus of the specimen
genus<-{}
for(i in dets$beeid){
    genus<-c(genus, strsplit(dets[dets$beeid==i,]$genus_species, "_")[[1]][1])
}
length(genus)
length(dets$beeid)
dets$genus<-genus

#make vector of forest bee uniqueIDs that are female ('forest.female') and all forest bees ('forest.bees)
#exclude non-native bees from the forest bee list (Osmia cornifrons, Osmia taurus, others?)
forest.df<-dets[dets$genus %in% forest & dets$genus_species != "Osmia_cornifrons" & dets$genus_species != "Osmia_taurus",]
forest.females<-substr(forest.df[forest.df$sex == "female",]$beeid, 15, 18)
forest.bees<-substr(forest.df$beeid, 15, 18)


#how many forest bees do we have did we have?
length(forest.females)
length(forest.bees)
2907 %in% forest.females
dets[dets$uniqueID ==2877,]

#b. get that visitaiton data for both males and females

#get vector of plants called 'forest.visits' which are plants visited by bees in forest.bees uniqueID list

forest.visits<-visits[visits$uniqueID %in% forest.bees,]$genus.species
forest.visits.female<-visits[visits$uniqueID %in% forest.females,]$genus.species



#########1) Visitation##############

#a. subset all data to only include forest-associated bees

#make a vector of forest bee genera (Andrena, Augochlora, Augochloropsis, Bombus, Colletes, Lasioglossum and Osmia,)
forest<-read.csv("forest_beesONLY.csv", header = T)
forest<-c("Andrena", "Augochlora", "Augochloropsis", "Bombus", "Colletes", "Lasioglossum", "Osmia", "GreenBeeNEEDSID")

#add a column to the 'dets' df with just the genus of the specimen
genus<-{}
for(i in dets$beeid){
    genus<-c(genus, strsplit(dets[dets$beeid==i,]$genus_species, "_")[[1]][1])
}
length(genus)
length(dets$beeid)
dets$genus<-genus

#make vector of forest bee uniqueIDs that are female ('forest.female') and all forest bees ('forest.bees)
#exclude non-native bees from the forest bee list (Osmia cornifrons, Osmia taurus, others?)
forest.df<-dets[dets$genus %in% forest & dets$genus_species != "Osmia_cornifrons" & dets$genus_species != "Osmia_taurus",]
forest.females<-substr(forest.df[forest.df$sex == "female",]$beeid, 15, 18)
forest.bees<-substr(forest.df$beeid, 15, 18)


#how many forest bees do we have did we have?
length(forest.females)
length(forest.bees)
2907 %in% forest.females
dets[dets$uniqueID ==2877,]

#b. get that visitaiton data for both males and females

#get vector of plants called 'forest.visits' which are plants visited by bees in forest.bees uniqueID list

forest.visits<-visits[visits$uniqueID %in% forest.bees,]$genus.species
forest.visits.female<-visits[visits$uniqueID %in% forest.females,]$genus.species

############2) Pollen################

#a. format the pollen data

pollen<-read.csv("pollen.id_modified_CS111816justforest.csv", stringsAsFactors = F)

five<-unique(pollen$beeID)
df <- data.frame(beeID = character(), slide = character(), scan = character(), pollen = character(), grains = character(), notes = character(), X = character(), stringsAsFactors = F) 

#if grains >5 we give it frequency of 1; if its sum > 5 but no grains in 1 scan >5 
#it just gets a 1 for its total frequency

for(i in five){
    test<-pollen[pollen$beeID==i,]
    num<-tapply(test$grains, test$pollen, sum)
    data1<-num[num>5]
    
    #for i in test if grains >5 keep row otherwise discard, then check if anything in rownames(data1) is not in pollen
    test2<-test[test$grains>5,]
    df<-rbind(df, test2)
    
    a<-test[test$grains<=5,]
    num2<-tapply(a$grains, a$pollen, sum)
    data2<-num2[num2>5]
    if(length(data2)!= 0){
        c<-a[a$pollen %in% names(data2),]
        for(j in unique(c$pollen)){
            b<-c[c$pollen == j,][1,]
            df<-rbind(df, b)
        }
        
    }
}
head(df)
df<-(df[is.na(df$beeID) == F,])
df[df$pollen == 0,]

#add column with plant the bee visited, within each bee id pick pollen with max grains see what percentage of times it matches visitation

vis<-{}

for(i in unique(df$beeID)){
    
    p<-tolower(visits[visits$uniqueID == i,]$genus.species)
    pg<-rep(p, length(df[df$beeID == i,]$pollen))
    vis<-c(vis, pg)
    
}
vis
df2<-df
df2$plant_visit<-vis
df2$occur<-rep(1, length(df2$grains))


#make a df called "agg" with each bee having one row per pollen species and a column for its relative frequency

beeID<-{}
pollen<-{}
proportion<-{}
het<-{}


for(i in unique(df2$beeID)){
    dat<-df2[df2$beeID == i,]
    #threequarter<-{}
    total = sum(dat$occur)
    for(j in unique(dat$pollen)){
        sp<-dat[dat$pollen == j,]
        prop<-sum(sp$occur)/total
        
        proportion<-c(proportion, prop)
        pollen<-c(pollen, j)
        beeID<-c(beeID, i)
        
    }
    
}

ag<-cbind(beeID, pollen, proportion, het)
agg<-as.data.frame(ag, stringsAsFactors = F)

#add plant_visit data to agg
plant_visit<-{}
for(i in agg$beeID){
    v<-tolower(visits[visits$uniqueID == i,]$genus.species)
    plant_visit<-c(plant_visit, v)
}

agg$plant_visit<-plant_visit

#b. subset all data to only include forest associated bees

#'forest.bees' is a vector of all the bees that are in the forest bees list. 
#we didn't take pollen from all of these because some are female, or we haven't got to them yet

pollen<-read.csv("pollen.id_modified_CS111816justforest.csv", stringsAsFactors = F)

#number of bees we took pollen from
length(unique(pollen$beeID))

forest.pollen<-agg[agg$beeID %in% forest.females,]

#number of bees we took pollen from that are forest bees
length(unique(forest.pollen$beeID))


#c. pick primary pollen: in table called t.primary

#loop through each unique forest bee and pick the pollen with the max proportion in the forest.pollen df

primary.pollen<-{}
beez<-{}
for(i in unique(forest.pollen$beeID)){
    p.max<-max(forest.pollen[forest.pollen$beeID == i, ]$proportion)
    prim.pollen<-forest.pollen[forest.pollen$proportion == p.max & forest.pollen$beeID == i,]$pollen
    primary.pollen<-c(primary.pollen, prim.pollen)
    beez<-c(beez, rep(i, length(primary.pollen)))
}

t.primary<-sort(table(primary.pollen))

#c. pick mismatch pollen

#get rid of pollen morphotypes to simplify things: df without morphotypes is forest.pollen.sp
morph<-grepl("mon", forest.pollen$pollen) | grepl("por", forest.pollen$pollen) | 
    grepl("col", forest.pollen$pollen) | grepl("ret", forest.pollen$pollen) | 
    grepl("psi", forest.pollen$pollen) | grepl("rug", forest.pollen$pollen) | 
    grepl("ver", forest.pollen$pollen) | grepl("dyad", forest.pollen$pollen) | 
    grepl("tetrad", forest.pollen$pollen) | grepl("sulcate", forest.pollen$pollen)
forest.pollen$morph<-morph
forest.pollen.sp<-forest.pollen[forest.pollen$morph == F,]


all.pollen.sp<-sort(table(forest.pollen.sp$pollen))
#pollen grains without species codes (string length < 6) 
#are ones that need special manipulation to tell if they are match or nonmatch pollen; 
#some of these (ones grouped with unrelated species) need to be in the ms as a supplementary table
#make tabe supptable then convert to df "sup"
supptable<-all.pollen.sp[nchar(names(all.pollen.sp)) != 6]
all.pollen.sp[nchar(names(all.pollen.sp)) == 6]

sup<-cbind(names(supptable), supptable)
sup<-as.data.frame(sup)
nam<-read.csv("species_codes_to_names_growthform.csv", stringsAsFactors = F)

#add species name data
plant.full<-{}
for(i in sup$V1){
    plant.full<-c(plant.full, nam[nam$code==i,]$genus_species)
    #print(i)
    #print(nam[nam$code==i,]$genus_species)
}
length(plant.full)
length(sup$V1)
sup$plant.species.genus<-plant.full
write.csv(sup, "pollen.species.groups.csv")


#add TRUE/FALSE vector to forest.pollen df; might be easiest to do some of this manually

ids<-{}
pollen<-{}
visit<-{}
tfvector<-{}


for(i in unique(forest.pollen$beeID)){
    #t gives you the plant visit data
    t<-forest.pollen[forest.pollen$beeID == i,]
    t3<-forest.pollen[forest.pollen$beeID ==i,]$pollen
    
    for(j in t3[1:length(t3)]){
        if(j != "ace.ros" & j != "brass" & grepl("ran",j) == F & j != "ace.ros.vin" 
           & j != "rosaceae" & j != "allpet.eupesu" & j != "ane.aqu" & j != "carex" 
           & j != "cor" & j != "mag" & j != "vibpru.euoala" & j != "betulaceae" & j != "cerastium" & j != "ace.trolax"
           & j != "rho" & grepl("ace", j) == F & j != "fravir.ducind" & j != "aster" & j != "ros"){
            
            tfvector<-c(tfvector, j == tolower(t$plant_visit[1]) ) 
        }
        
        else{
            if(j == "brass"){
                tfvector<-c(tfvector, t$plant_visit[1] == "barvul")
            }
            else{
                if(grepl("ran",j)){
                    tfvector<-c(tfvector, grepl("ran",t$plant_visit[1]))
                }
                else{
                    if(j == "ace.ros"){
                        tfvector<-c(tfvector, substr(t$plant_visit[1], 1, 3) == "ace" | substr((t$plant_visit[1]), 1, 3) == "pyr" | substr(t$plant_visit[1], 1, 3) == "mal" | substr(t$plant_visit[1], 1, 3) == "pru" | substr(t$plant_visit[1], 1, 4) == "rosa")
                    }
                    else{
                        if(j == "cerastium"){
                            tfvector <- c(tfvector, grepl("cer", t$plant_visit[1]))
                        }
                        else{
                            if(j == "ace"){
                                tfvector<-c(tfvector, substr(t$plant_visit[1], 1, 3) == "ace")
                            }
                            else{
                                if(j == "ros"){
                                    tfvector <- c(tfvector, substr((t$plant_visit[1]), 1, 3) == "pyr" | substr(t$plant_visit[1], 1, 3) == "mal" | substr(t$plant_visit[1], 1, 3) == "pru" | substr(t$plant_visit[1], 1, 4) == "rosa")
                                }
                                else{
                                    tfvector<-c(tfvector, "needs assessment")
                                }
                                
                            }
                        }
                        
                    }
                }
            }
        }
    }
    #}
    
    r<-rep(i, length(t3))
    ids<-c(ids, r)
    pollen<-c(pollen, t3)
    visit<-c(visit, rep(tolower(t$plant_visit[1]), length(t3)))
    
    
}


length(pollen)
length(visit)
length(tfvector)
length(ids)
length(morph)

matchez<-as.data.frame(cbind(tfvector, ids, pollen, visit, morph), stringsAsFactors = F)

#download matchez df to assess the pollen that needs assessment for whether it is a match or a mismatch
write.csv(matchez, "assess_match_pollen_springpollenms2.csv")

#match.a<-read.csv("assessed_match_pollen111816.csv", stringsAsFactors = F)
match.a<-read.csv("assess_match_pollen_springpollenms112916.csv", stringsAsFactors = F)


mismatch<-match.a[match.a$tfvector == F & match.a$morph ==F,]

##########add the growth form info##########
growth<-read.csv("species_codes_to_names_growthform.csv", stringsAsFactors = F)
#are there any additional pollen species in need of growth form info?
lookup.growth<- mismatch[!mismatch$pollen %in% growth$code,]
length(lookup.growth$pollen)
#there weren't, so I used "species_codes to names_growthform.csv" as is

growthform<-{}
#for each pollen classify its growth form
for(i in mismatch$pollen){
    if(i %in% growth$code){
        growthform<-c(growthform, growth[growth$code == i,]$growth)
    }
    else{
        #print pollen species/ morphs not on the data form and then add them to the data form
        print(i)
    }
}
length(mismatch$pollen)
length(growthform)
mismatch.pollen2<-cbind(mismatch, growthform)

#add a column for broader categories of tree/shrub and herb
growth.form<-{}
for(i in mismatch.pollen2$growthform){
    if(i == "forb/ herb" | i == "forb/ herb, subshrub"){
        growth.form<-c(growth.form, "herbaceous")
    }
    else{
        if(i == "shrub, tree" | i == "shrub, vine" | i == "shrub" | i == "tree"){
            growth.form<-c(growth.form, "tree/ shrub")
        }
        else{
            growth.form<-c(growth.form, "other/ unknown")
        }
        
    }
    
}
mismatch.pollen2$growth<-growth.form
table(mismatch.pollen2$growth)

head(visits)
forest.vis<-visits[visits$uniqueID %in% forest.females,]
gform<-{}
for(i in forest.vis$uniqueID){
    p<-tolower(visits[visits$uniqueID == i,]$genus.species)
    if(p %in% growth$code){
        gform<-c(gform, growth[growth$code == p,]$growth)
    }
    else{
        print(p)
    }
    
}
forest.vis$growth.forms<-gform
table(forest.vis$growth.forms)

#add broader categories
growth.f <- {}
for(i in forest.vis$growth.form){
    if(i == "forb/ herb" | i == "forb/ herb, subshrub" | i == "forb/ herb, vine"){
        growth.f<-c(growth.f, "herbaceous")
    }
    else{
        if(i == "shrub, tree" | i == "shrub, vine" | i == "shrub" | i == "tree" | i == "subshrub"){
            growth.f<-c(growth.f, "tree/ shrub")
        }
        else{
            growth.f<-c(growth.f, "other/ unknown")
        }
        
    }
    
}
forest.vis$growth<-growth.f

table(forest.vis$growth)
pollen.bees<-forest.vis[forest.vis$uniqueID %in% mismatch.pollen2$ids,]
#pollen.bees<-forest.vis[forest.vis$uniqueID %in% forest.pollen$beeID,]

from.pollen<-mismatch.pollen2[,c(3,4,8)]
from.visit<-pollen.bees[,c(1,3,17)]

from.pollen$data<-rep("pollen", length(from.pollen$growth))
from.visit$data<-rep("visit", length(from.visit$growth))
names(from.visit)<-(c("id", "plant", "growth", "data"))
names(from.pollen)<-(c("id", "plant", "growth", "data"))

newdf<-rbind(from.pollen, from.visit)
newdf$growth2<-as.factor(newdf$growth)
levels(newdf$growth2)
newdf$growth2 <- factor(newdf$growth2, levels = c("herbaceous", "tree/ shrub", "other/ unknown"))
newdf$data2<-as.factor(newdf$data)
str(newdf)
growth.usage<-table(newdf$data, newdf$growth)
growth.usage2<-table(newdf$growth2, newdf$data2)

    #make barplots
par(mfrow = c(1,2))
growth.usage2
pollen<-growth.usage2[,1]/colSums(growth.usage2)[1]
visit<-growth.usage2[,2]/colSums(growth.usage2)[2]
growth.usage3<-cbind(visit,pollen)
barplot(growth.usage3, col = c("green" , "brown", "gray", "green", "brown", "gray"), xlab = "Type of Data", ylab = "Frequency",  names.arg = c("Visitation", "Mismatch Pollen"))


    #also compare with all pollen bees not just ones that had mismatch pollen on them

pollen.bees<-forest.vis[forest.vis$uniqueID %in% forest.pollen$beeID,]

from.pollen<-mismatch.pollen2[,c(3,4,8)]
from.visit<-pollen.bees[,c(1,3,17)]

from.pollen$data<-rep("pollen", length(from.pollen$growth))
from.visit$data<-rep("visit", length(from.visit$growth))
names(from.visit)<-(c("id", "plant", "growth", "data"))
names(from.pollen)<-(c("id", "plant", "growth", "data"))

newdf<-rbind(from.pollen, from.visit)
newdf$growth2<-as.factor(newdf$growth)
levels(newdf$growth2)
newdf$growth2 <- factor(newdf$growth2, levels = c("herbaceous", "tree/ shrub", "other/ unknown"))
newdf$data2<-as.factor(newdf$data)
str(newdf)
growth.usage<-table(newdf$data, newdf$growth)
growth.usage2<-table(newdf$growth2, newdf$data2)

#make barplots
growth.usage2
pollen<-growth.usage2[,1]/colSums(growth.usage2)[1]
visit<-growth.usage2[,2]/colSums(growth.usage2)[2]
growth.usage3<-cbind(visit,pollen)
barplot(growth.usage3, col = c("green" , "brown", "gray", "green", "brown", "gray"), xlab = "Type of Data", ylab = "Frequency",  names.arg = c("Visitation", "Mismatch Pollen"))


#4) modify bar graph to show what proportion of mismatch pollen is new tree species
#[should also probably do the same thing for herbaceous plants -- but later]
value.added.treespecies<-c("vibpru.euoala","betulaceae", "mag" ,"lirtul", "vibpru", "sasalb")
value.added.treepollen <- mismatch.pollen2[mismatch.pollen2$pollen %in% value.added.species,]

#Make a new table with columns for herbaceous value added, tree value added, tree and herbaceous and other
growth3 <-{}
for(i in newdf$plant){
    if(! i %in% value.added.treespecies){
        growth3 <- c(growth3, newdf[newdf$plant == i,]$growth[1])
    }
    else{
        growth3<-c(growth3, "new.tree")
    }
    
}
newdf$growth3 <- growth3


#growth.usage<-table(newdf$data, newdf$growth)
#growth.usage2<-table(newdf$growth2, newdf$data2)
growth.usage3<-table(newdf$growth3, newdf$data2)

pollen<-growth.usage3[,1]/colSums(growth.usage3)[1]
visit<-growth.usage3[,2]/colSums(growth.usage3)[2]
growth.usage4<-cbind(pollen,visit)
par(mfrow = c(1,2))
barplot(growth.usage4, col = c("green" , "burlywood3", "gray","chocolate4", "green", "burlywood3", "gray", "chocolate4"), xlab = "Type of Data", ylab = "Frequency",  names.arg = c("Mismatch pollen", "Visitation"))


x<-newdf[newdf$growth3 == "new.tree" ,]
y<-table(x$plant)
pie(y)



#########make pie charts





