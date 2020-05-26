``` r
library(pacman)

pacman::p_load(dplyr, tidyverse, brms, patchwork, RColorBrewer)

data10 <- read.csv("data10.csv")
data20 <- read.csv("data20.csv")
data30 <- read.csv("data30.csv")
data40 <- read.csv("data40.csv")
data50 <- read.csv("data50.csv")
data60 <- read.csv("data60.csv")
data70 <- read.csv("data70.csv")
data80 <- read.csv("data80.csv")
```

DATA CLEANING
-------------

``` r
# extracting, averaging and renaming the columns for the different environment values

# 10
d10 <- data10 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)
d10$Environment <- c(10)

# 20
d20 <- data20 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)

d20$Environment <- c(20)

# 30
d30 <- data30 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...))

d30$Environment <- c(30)

# 40
d40 <- data40 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)

d40$Environment <- c(40)

# 50
d50 <- data50 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)

d50$Environment <- c(50)

# 60
d60 <- data60 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)

d60$Environment <- c(60)

# 70
d70 <- data70 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)

d70$Environment <- c(70)

# 80
d80 <- data80 %>% group_by(X.step.) %>% summarise(No_Dep = mean(count.turtles.with...severity....No.Depression...), Mild_Dep = mean(count.turtles.with...severity....Mild.Depression...), Moderate_Dep = mean(count.turtles.with...severity....Moderate.Depression...), Severe_Dep = mean(count.turtles.with...severity....Severe.Depression...), All_Dep = mean(count.turtles.with...severity.....No.Depression...), Mean_DScore = mean(mean...dep.score...of.turtles), Treatment = mean(count.turtles.with...treatment...), Talk = mean(count.turtles.with...talks...), Prob_of_Talking = mean(mean...prob.of.talking...of.turtles), Prob_of_Treatment = mean(mean...prob.of.treatment...of.turtles), Mean_DScore_Male = mean(mean...dep.score...of.turtles.with...gender.....male...), Mean_DScore_Female = mean(mean...dep.score...of.turtles.with...gender.....female...), NoDep_Male = mean(count.turtles.with...severity....No.Depression..and.gender.....male...), Mild_Male = mean(count.turtles.with...severity....Mild.Depression..and.gender.....male...), Moderate_Male = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....male...), Severe_Male = mean(count.turtles.with...severity....Severe.Depression..and.gender.....male...), AllDep_Male = mean(count.turtles.with...severity.....No.Depression..and.gender.....male...), NoDep_Female = mean(count.turtles.with...severity....No.Depression..and.gender.....female...), Mild_Female = mean(count.turtles.with...severity....Mild.Depression..and.gender.....female...), Moderate_Female = mean(count.turtles.with...severity....Moderate.Depression..and.gender.....female...), Severe_Female = mean(count.turtles.with...severity....Severe.Depression..and.gender.....female...), AllDep_Female = mean(count.turtles.with...severity.....No.Depression..and.gender.....female...), Count_Male = mean(count.turtles.with...gender.....male...), Count_Female = mean(count.turtles.with...gender.....female...), Male_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....male...), Male_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....male...), Female_Treatment = mean(count.turtles.with...treatment....TRUE.and.gender.....female...), Female_Talks = mean(count.turtles.with...talks....TRUE.and.gender.....female...)
)

d80$Environment <- c(80)

# merge the datasets
merged1 <- rbind(d10, d20)
merged2 <- rbind(merged1, d30)
merged3 <- rbind(merged2, d40)
merged4 <- rbind(merged3, d50)
merged5 <- rbind(merged4, d60)
merged6 <- rbind(merged5, d70)
merged_all <- rbind(merged6, d80)

colnames(merged_all)[1] <- "Week"

# save file
write.csv(merged_all, file = "super_data.csv")

data <- read.table("super_data.csv", sep = ",", header = T)
```

DESCRIPTIVE STATS
-----------------

``` r
data <- read.table("super_data.csv", sep = ",", header = T)

# Creating fancy table of descriptive stats

library(table1)
```

    ## Warning: package 'table1' was built under R version 3.6.3

    ## 
    ## Attaching package: 'table1'

    ## The following objects are masked from 'package:base':
    ## 
    ##     units, units<-

``` r
table1::label(data$Talk) <- "Talk"
table1::label(data$Male_Talks) <- "Talk Male"
table1::label(data$Female_Talks) <- "Talk Female"
table1::label(data$Treatment) <- "Treatment"
table1::label(data$Male_Treatment) <- "Treatment Male"
table1::label(data$Female_Treatment) <- "Treatment Female"
table1::label(data$No_Dep) <- "No Depression" 
table1::label(data$Mild_Dep) <- "Mild Depression"
table1::label(data$Moderate_Dep) <- "Moderate Depression"
table1::label(data$Severe_Dep) <- "Severe Depression"
table1::label(data$All_Dep) <- "All Depressed"
table1::label(data$NoDep_Male) <- "No Depression Male" 
table1::label(data$AllDep_Male) <- "All Depressed Male"
table1::label(data$NoDep_Female) <- "No Depression Female" 
table1::label(data$AllDep_Female) <- "All Depressed Female"

table1::table1(~ No_Dep + Mild_Dep + Moderate_Dep + Severe_Dep + All_Dep + NoDep_Male + NoDep_Female + AllDep_Male + AllDep_Female + Treatment + Male_Treatment + Female_Treatment + Talk + Male_Talks + Female_Talks | Environment, data = data)
```

    ## Warning in table1.formula(~No_Dep + Mild_Dep + Moderate_Dep + Severe_Dep + :
    ## Terms to the right of '|' in formula 'x' define table columns and are expected
    ## to be factors with meaningful labels.

    ## [1] "<table class=\"Rtable1\">\n<thead>\n<tr>\n<th class='rowlabel firstrow lastrow'></th>\n<th class='firstrow lastrow'><span class='stratlabel'>10<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>20<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>30<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>40<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>50<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>60<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>70<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>80<br><span class='stratn'>(N=105)</span></span></th>\n<th class='firstrow lastrow'><span class='stratlabel'>Overall<br><span class='stratn'>(N=840)</span></span></th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>No Depression</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>953 (4.09)</td>\n<td>952 (4.28)</td>\n<td>952 (4.11)</td>\n<td>952 (3.76)</td>\n<td>955 (4.59)</td>\n<td>955 (5.01)</td>\n<td>955 (4.80)</td>\n<td>957 (5.58)</td>\n<td>954 (4.88)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>954 [943, 957]</td>\n<td class='lastrow'>954 [941, 957]</td>\n<td class='lastrow'>953 [941, 957]</td>\n<td class='lastrow'>953 [940, 956]</td>\n<td class='lastrow'>956 [941, 960]</td>\n<td class='lastrow'>957 [940, 961]</td>\n<td class='lastrow'>956 [940, 960]</td>\n<td class='lastrow'>959 [941, 963]</td>\n<td class='lastrow'>955 [940, 963]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Mild Depression</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>27.9 (3.10)</td>\n<td>27.9 (2.87)</td>\n<td>27.6 (2.50)</td>\n<td>27.8 (1.61)</td>\n<td>25.8 (1.90)</td>\n<td>25.6 (1.56)</td>\n<td>25.7 (1.72)</td>\n<td>23.8 (2.04)</td>\n<td>26.5 (2.63)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>27.1 [24.3, 34.9]</td>\n<td class='lastrow'>27.0 [24.4, 34.6]</td>\n<td class='lastrow'>27.2 [24.5, 32.7]</td>\n<td class='lastrow'>27.6 [25.6, 31.6]</td>\n<td class='lastrow'>25.3 [23.3, 29.7]</td>\n<td class='lastrow'>25.4 [23.7, 30.5]</td>\n<td class='lastrow'>24.8 [23.9, 30.2]</td>\n<td class='lastrow'>23.0 [21.4, 28.8]</td>\n<td class='lastrow'>26.0 [21.4, 34.9]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Moderate Depression</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>10.6 (1.23)</td>\n<td>10.8 (1.30)</td>\n<td>10.9 (1.52)</td>\n<td>10.5 (1.38)</td>\n<td>10.2 (1.64)</td>\n<td>9.70 (1.54)</td>\n<td>10.1 (1.73)</td>\n<td>9.71 (1.48)</td>\n<td>10.3 (1.54)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>10.2 [9.16, 13.9]</td>\n<td class='lastrow'>10.6 [9.16, 14.2]</td>\n<td class='lastrow'>10.4 [9.34, 15.1]</td>\n<td class='lastrow'>10.1 [8.86, 14.7]</td>\n<td class='lastrow'>9.90 [8.36, 14.6]</td>\n<td class='lastrow'>9.46 [7.82, 14.3]</td>\n<td class='lastrow'>9.64 [7.90, 15.4]</td>\n<td class='lastrow'>9.22 [8.00, 14.4]</td>\n<td class='lastrow'>9.94 [7.82, 15.4]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Severe Depression</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>9.04 (1.74)</td>\n<td>8.94 (2.63)</td>\n<td>9.41 (1.95)</td>\n<td>9.66 (2.53)</td>\n<td>9.07 (2.21)</td>\n<td>9.18 (2.56)</td>\n<td>9.30 (2.37)</td>\n<td>9.47 (2.71)</td>\n<td>9.26 (2.36)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>8.08 [7.42, 13.6]</td>\n<td class='lastrow'>7.52 [6.48, 15.1]</td>\n<td class='lastrow'>8.54 [7.48, 14.3]</td>\n<td class='lastrow'>8.38 [7.22, 15.8]</td>\n<td class='lastrow'>8.08 [6.92, 14.7]</td>\n<td class='lastrow'>7.96 [6.74, 14.9]</td>\n<td class='lastrow'>8.24 [7.24, 14.9]</td>\n<td class='lastrow'>8.34 [6.88, 16.0]</td>\n<td class='lastrow'>8.22 [6.48, 16.0]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>All Depressed</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>47.5 (4.09)</td>\n<td>47.7 (4.28)</td>\n<td>47.9 (4.11)</td>\n<td>48.0 (3.76)</td>\n<td>45.1 (4.59)</td>\n<td>44.5 (5.01)</td>\n<td>45.0 (4.80)</td>\n<td>43.0 (5.58)</td>\n<td>46.1 (4.88)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>46.1 [42.5, 57.1]</td>\n<td class='lastrow'>46.5 [42.6, 58.7]</td>\n<td class='lastrow'>46.7 [43.0, 58.8]</td>\n<td class='lastrow'>47.0 [44.0, 60.3]</td>\n<td class='lastrow'>43.9 [40.1, 58.7]</td>\n<td class='lastrow'>43.0 [39.4, 59.7]</td>\n<td class='lastrow'>43.6 [39.9, 59.6]</td>\n<td class='lastrow'>41.5 [37.0, 59.2]</td>\n<td class='lastrow'>45.0 [37.0, 60.3]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>No Depression Male</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>478 (4.57)</td>\n<td>472 (5.37)</td>\n<td>472 (6.57)</td>\n<td>475 (6.58)</td>\n<td>474 (6.97)</td>\n<td>472 (6.83)</td>\n<td>473 (8.24)</td>\n<td>474 (7.48)</td>\n<td>474 (6.86)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>479 [467, 483]</td>\n<td class='lastrow'>474 [460, 478]</td>\n<td class='lastrow'>474 [458, 480]</td>\n<td class='lastrow'>476 [460, 482]</td>\n<td class='lastrow'>477 [459, 481]</td>\n<td class='lastrow'>474 [456, 479]</td>\n<td class='lastrow'>476 [455, 482]</td>\n<td class='lastrow'>476 [457, 482]</td>\n<td class='lastrow'>476 [455, 483]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>No Depression Female</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>475 (4.51)</td>\n<td>480 (5.74)</td>\n<td>480 (6.63)</td>\n<td>478 (7.62)</td>\n<td>481 (8.29)</td>\n<td>484 (9.64)</td>\n<td>482 (9.89)</td>\n<td>483 (10.2)</td>\n<td>480 (8.46)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>477 [461, 479]</td>\n<td class='lastrow'>483 [464, 485]</td>\n<td class='lastrow'>482 [462, 487]</td>\n<td class='lastrow'>480 [459, 487]</td>\n<td class='lastrow'>483 [461, 491]</td>\n<td class='lastrow'>486 [463, 496]</td>\n<td class='lastrow'>484 [459, 494]</td>\n<td class='lastrow'>485 [459, 495]</td>\n<td class='lastrow'>481 [459, 496]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>All Depressed Male</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>24.9 (4.57)</td>\n<td>24.5 (5.37)</td>\n<td>25.5 (6.57)</td>\n<td>27.3 (6.58)</td>\n<td>26.8 (6.97)</td>\n<td>26.6 (6.83)</td>\n<td>28.2 (8.24)</td>\n<td>27.6 (7.48)</td>\n<td>26.4 (6.75)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>23.6 [20.0, 35.1]</td>\n<td class='lastrow'>22.6 [19.0, 36.6]</td>\n<td class='lastrow'>23.5 [18.4, 40.0]</td>\n<td class='lastrow'>25.6 [20.1, 41.8]</td>\n<td class='lastrow'>24.5 [19.6, 42.1]</td>\n<td class='lastrow'>24.2 [19.6, 42.0]</td>\n<td class='lastrow'>25.8 [19.3, 46.2]</td>\n<td class='lastrow'>25.5 [19.6, 45.1]</td>\n<td class='lastrow'>24.3 [18.4, 46.2]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>All Depressed Female</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>22.6 (4.51)</td>\n<td>23.2 (5.74)</td>\n<td>22.4 (6.63)</td>\n<td>20.6 (7.62)</td>\n<td>18.3 (8.29)</td>\n<td>17.9 (9.64)</td>\n<td>16.9 (9.89)</td>\n<td>15.4 (10.2)</td>\n<td>19.7 (8.49)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>20.2 [18.9, 36.3]</td>\n<td class='lastrow'>20.5 [18.1, 39.0]</td>\n<td class='lastrow'>20.4 [15.5, 39.8]</td>\n<td class='lastrow'>18.4 [11.2, 39.4]</td>\n<td class='lastrow'>16.3 [7.90, 38.3]</td>\n<td class='lastrow'>15.8 [5.52, 39.2]</td>\n<td class='lastrow'>14.1 [4.96, 39.6]</td>\n<td class='lastrow'>12.6 [3.10, 38.6]</td>\n<td class='lastrow'>19.3 [3.10, 39.8]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Treatment</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>102 (5.29)</td>\n<td>112 (12.2)</td>\n<td>127 (26.4)</td>\n<td>151 (50.1)</td>\n<td>181 (76.8)</td>\n<td>211 (98.1)</td>\n<td>231 (110)</td>\n<td>251 (119)</td>\n<td>171 (91.6)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>102 [92.2, 114]</td>\n<td class='lastrow'>111 [92.9, 137]</td>\n<td class='lastrow'>121 [95.0, 190]</td>\n<td class='lastrow'>133 [93.4, 268]</td>\n<td class='lastrow'>155 [94.7, 345]</td>\n<td class='lastrow'>185 [95.6, 396]</td>\n<td class='lastrow'>211 [95.0, 423]</td>\n<td class='lastrow'>241 [95.3, 442]</td>\n<td class='lastrow'>127 [92.2, 442]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Treatment Male</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>27.9 (1.94)</td>\n<td>25.1 (3.23)</td>\n<td>22.9 (4.28)</td>\n<td>21.2 (5.29)</td>\n<td>19.2 (6.09)</td>\n<td>18.0 (6.63)</td>\n<td>16.6 (6.80)</td>\n<td>15.8 (6.99)</td>\n<td>20.8 (6.73)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>28.0 [23.6, 31.6]</td>\n<td class='lastrow'>25.1 [19.5, 31.9]</td>\n<td class='lastrow'>22.7 [15.7, 31.7]</td>\n<td class='lastrow'>20.3 [12.8, 31.1]</td>\n<td class='lastrow'>18.5 [10.1, 32.0]</td>\n<td class='lastrow'>16.9 [8.70, 31.2]</td>\n<td class='lastrow'>15.3 [7.46, 31.4]</td>\n<td class='lastrow'>14.0 [7.48, 31.0]</td>\n<td class='lastrow'>21.7 [7.46, 32.0]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Treatment Female</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>74.2 (7.05)</td>\n<td>86.5 (15.2)</td>\n<td>104 (30.4)</td>\n<td>130 (55.1)</td>\n<td>162 (82.4)</td>\n<td>193 (104)</td>\n<td>215 (116)</td>\n<td>235 (126)</td>\n<td>150 (97.8)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>74.0 [61.6, 87.8]</td>\n<td class='lastrow'>84.6 [61.9, 117]</td>\n<td class='lastrow'>99.0 [64.4, 173]</td>\n<td class='lastrow'>113 [63.6, 255]</td>\n<td class='lastrow'>137 [64.1, 334]</td>\n<td class='lastrow'>168 [65.4, 387]</td>\n<td class='lastrow'>195 [64.4, 416]</td>\n<td class='lastrow'>227 [64.5, 434]</td>\n<td class='lastrow'>105 [61.6, 434]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Talk</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>33.6 (2.56)</td>\n<td>62.9 (6.06)</td>\n<td>101 (19.9)</td>\n<td>156 (49.5)</td>\n<td>219 (81.0)</td>\n<td>279 (100)</td>\n<td>324 (105)</td>\n<td>362 (103)</td>\n<td>192 (135)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>33.1 [28.9, 39.2]</td>\n<td class='lastrow'>61.7 [54.5, 76.2]</td>\n<td class='lastrow'>94.6 [79.4, 151]</td>\n<td class='lastrow'>136 [102, 275]</td>\n<td class='lastrow'>192 [127, 383]</td>\n<td class='lastrow'>261 [154, 451]</td>\n<td class='lastrow'>323 [178, 478]</td>\n<td class='lastrow'>376 [205, 493]</td>\n<td class='lastrow'>157 [28.9, 493]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Talk Male</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>13.4 (0.512)</td>\n<td>20.4 (2.25)</td>\n<td>26.1 (4.87)</td>\n<td>30.9 (8.20)</td>\n<td>34.1 (12.3)</td>\n<td>36.5 (15.9)</td>\n<td>38.9 (19.3)</td>\n<td>40.7 (22.9)</td>\n<td>30.1 (15.9)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>13.4 [12.1, 14.9]</td>\n<td class='lastrow'>20.4 [16.4, 24.3]</td>\n<td class='lastrow'>25.8 [17.9, 36.1]</td>\n<td class='lastrow'>29.6 [18.3, 46.9]</td>\n<td class='lastrow'>32.2 [15.1, 57.2]</td>\n<td class='lastrow'>34.2 [14.5, 67.9]</td>\n<td class='lastrow'>34.4 [14.0, 80.3]</td>\n<td class='lastrow'>34.9 [12.8, 91.4]</td>\n<td class='lastrow'>24.9 [12.1, 91.4]</td>\n</tr>\n<tr>\n<td class='rowlabel firstrow'><span class='varlabel'>Talk Female</span></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n<td class='firstrow'></td>\n</tr>\n<tr>\n<td class='rowlabel'>Mean (SD)</td>\n<td>20.2 (2.46)</td>\n<td>42.5 (8.11)</td>\n<td>75.1 (24.4)</td>\n<td>125 (57.0)</td>\n<td>185 (92.6)</td>\n<td>242 (116)</td>\n<td>285 (124)</td>\n<td>321 (125)</td>\n<td>162 (136)</td>\n</tr>\n<tr>\n<td class='rowlabel lastrow'>Median [Min, Max]</td>\n<td class='lastrow'>19.9 [15.8, 25.3]</td>\n<td class='lastrow'>41.8 [30.9, 59.4]</td>\n<td class='lastrow'>68.9 [44.7, 132]</td>\n<td class='lastrow'>107 [56.6, 257]</td>\n<td class='lastrow'>159 [73.0, 367]</td>\n<td class='lastrow'>228 [86.6, 437]</td>\n<td class='lastrow'>288 [101, 464]</td>\n<td class='lastrow'>341 [115, 480]</td>\n<td class='lastrow'>114 [15.8, 480]</td>\n</tr>\n</tbody>\n</table>\n"

``` r
# table1::label(data$Prob_of_Talking) <- "Prob of Talking"
# table1::label(data$Prob_of_Treatment) <- "Prob of Treatment"
```

PRETTY PLOTS
------------

``` r
w1 <- filter(data, Week == 1 | Week == 104)

### talking
talking <- ggplot(w1, aes(Week, Talk, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Agents with treatment", title = "Number of agents seeking treatment on W1 and W104")
talking + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
### treatment seeking
treatment <- ggplot(w1, aes(Week, Treatment, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Agents with treatment", title = "Number of agents seeking treatment on W1 and W104")
treatment + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
### all depressed
dep <-ggplot(w1, aes(Week, All_Dep)) + geom_point (aes(group = Environment, color= Environment))+ geom_line(aes(group = Environment, color=Environment)) + labs(x = "Week" , y = "Agents with depression", title = "All agents")
dep + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-3.png)

``` r
### depression categories
a <- ggplot(w1, aes(Week, No_Dep, color = Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Agents with no depression", title = "No depression")

b <- ggplot(w1, aes(Week, Mild_Dep, color = Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Agents with mild depression", title = "Mild depression")

c <- ggplot(w1, aes(Week, Moderate_Dep, color = Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Agents with moderate depression", title = "Moderate depression")

d <- ggplot(w1, aes(Week, Severe_Dep, color = Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Agents with severe depression", title = "Severe depression")

a + scale_color_gradientn(colours = rainbow(5)) + b + scale_color_gradientn(colours = rainbow(5)) + c + scale_color_gradientn(colours = rainbow(5)) + d + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-4.png)

``` r
### gender all dep
male_all <- ggplot(w1, aes(Week, AllDep_Male, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Male All Dep", title = "Males")
male_all + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-5.png)

``` r
female_all <- ggplot(w1, aes(Week, AllDep_Female, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Female All Dep", title = "Females")
female_all + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-6.png)

``` r
male_all + scale_color_gradientn(colours = rainbow(5)) + female_all + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-7.png)

``` r
### gender no dep
male_no <- ggplot(w1, aes(Week, NoDep_Male, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Male No Dep", title = "Males")
male_no + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-8.png)

``` r
female_no <- ggplot(w1, aes(Week, NoDep_Female, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Female No Dep", title = "Females")
female_no + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-9.png)

``` r
male_no + scale_color_gradientn(colours = rainbow(5)) + female_no + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-10.png)

``` r
### gender mild
male_mild <- ggplot(w1, aes(Week, Mild_Male, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Male Mild", title = "Males")
male_mild + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-11.png)

``` r
female_mild <- ggplot(w1, aes(Week, Mild_Female, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Female Mild", title = "Females")
female_mild + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-12.png)

``` r
### gender moderate
male_mod <- ggplot(w1, aes(Week, Moderate_Male, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Male Moderate", title = "Males")
male_mod + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-13.png)

``` r
female_mod <- ggplot(w1, aes(Week, Moderate_Female, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Female Moderate", title = "Females")
female_mod + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-14.png)

``` r
### gender severe
male_sev <- ggplot(w1, aes(Week, Severe_Male, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Male Severe", title = "Males")
male_sev + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-15.png)

``` r
female_sev <- ggplot(w1, aes(Week, Severe_Female, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Female Severe", title = "Females")
female_sev + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-16.png)

``` r
male_no + scale_color_gradientn(colours = rainbow(5))+ male_mild + scale_color_gradientn(colours = rainbow(5))+ male_mod + scale_color_gradientn(colours = rainbow(5))+ male_sev + scale_color_gradientn(colours = rainbow(5))+ male_all+ scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-17.png)

``` r
female_no + scale_color_gradientn(colours = rainbow(5))+ female_mild + scale_color_gradientn(colours = rainbow(5))+ female_mod + scale_color_gradientn(colours = rainbow(5))+ female_sev + scale_color_gradientn(colours = rainbow(5))+ female_all+ scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-18.png)

``` r
### gender treatment
male_treat <- ggplot(w1, aes(Week, Male_Treatment, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Males seeking treatment", title = "Males")
male_treat + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-19.png)

``` r
female_treat <- ggplot(w1, aes(Week, Female_Treatment, color=Environment)) + geom_point (aes(group = Environment)) + geom_line(aes(group = Environment)) + labs(x = "Week" , y = "Females seeking treatment", title = "Females")
female_treat + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-20.png)

``` r
male_treat + scale_color_gradientn(colours = rainbow(5)) + female_treat + scale_color_gradientn(colours = rainbow(5))
```

![](exam_analysis_files/figure-markdown_github/unnamed-chunk-4-21.png)
