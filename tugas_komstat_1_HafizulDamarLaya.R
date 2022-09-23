datas_1 <- read.csv("F:/Mata Kuliah/Semester 5/Komputasi Statistik/3. Tugas Pertemuan 3.csv")
library(tidyverse)
library(ggplot2)
library(dplyr)

#nomor 1
#exploration data
colnames(datas_1)
str(datas_1)
summarytools::descr(datas_1$math)
#mengganti kolom nama race.ethnicity
race_col <- paste("race",sep="")
colnames(datas_1)[2] <- race_col
datas_1
#mencari nilai unik dari kolom
i <- 1
while (i < 6){
  print(table(datas_1[i]))
  i <- i+1
}
#factor
datas_1$gender <- factor(datas_1$gender)
datas_1$race <- factor(datas_1$race.ethnicity)
datas_1$parent_education_level <- factor(datas_1$parent_education_level, order = TRUE, 
                                         levels =c("some high school", "high school", "some college", 
                                                   "associate\'s degree", "bachelor\'s degree",
                                                   "master\'s degree"))
datas_1$lunch <- factor(datas_1$lunch)
datas_1$test_prep_course <- factor(datas_1$test_prep_course)

datas_1$gender
#sorting data math untuk divisualisasikan
datas_1.math <- arrange(datas_1, math)
head(datas_1.math)



#visualize data
#visualisasi barplot gender dan math
ggplot(datas_1.math, aes(x=gender, y=math)) + 
  geom_bar(stat='identity', aes(fill=math)) +
  coord_flip() +
  theme_light() +
  scale_fill_gradient(name = "Math score") +
  labs(x="Gender", y ="Math Score Cumulative")

#visualisasi barplot race dan math
ggplot(datas_1.math, aes(x=race, y=math)) + 
  geom_bar(stat='identity', aes(fill=math)) +
  coord_flip() +
  theme_light() +
  scale_fill_gradient(name = "Math score") +
  labs(x="Race", y ="Math Score Cumulative")

#visualisasi barplot parent education level dan math
ggplot(datas_1.math, aes(x=parent_education_level, y=math)) + 
  geom_bar(stat='identity', aes(fill=math)) +
  coord_flip() +
  theme_light() +
  scale_fill_gradient(name = "Math score") +
  labs(x="Parent Education Level", y = "Math Score Cumulative")

#visualisasi barplot lunch dan math
ggplot(datas_1.math, aes(x=lunch, y=math)) + 
  geom_bar(stat='identity', aes(fill=math)) +
  coord_flip() +
  theme_light() +
  scale_fill_gradient(name = "Math score") +
  labs(x="Lunch", y = "Math Score Cumulative")

#visualisasi barplot test_prep_course dan math
ggplot(datas_1.math, aes(x=test_prep_course, y=math)) + 
  geom_bar(stat='identity', aes(fill=math)) +
  coord_flip() +
  theme_light() +
  scale_fill_gradient(name = "Math score") + 
  labs(x="Test Prep Course", y = "Math Score Cumulative")

#boxplot
#visualisasi boxplot gender dan math
ggplot(datas_1, aes(x=gender, y=math, color=gender)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") +
  labs(x="Gender", y="Math Score")

#visualisasi boxplot gender dan math
ggplot(datas_1, aes(x=race, y=math, color=race)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") +
  labs(x="Race", y="Math Score")

#visualisasi boxplot parent education level dan math
ggplot(datas_1, aes(x=parent_education_level, y=math, color=parent_education_level)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") +
  labs(x="Parent Education", y="Math Score")

#visualisasi boxplot lunch dan math
ggplot(datas_1, aes(x=lunch, y=math, color=lunch)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") +
  labs(x="Lunch", y="Math Score")

#visualisasi boxplot test prep course dan math
ggplot(datas_1, aes(x=test_prep_course, y=math, color=test_prep_course)) + 
  geom_boxplot() +
  scale_color_brewer(palette="Dark2") +
  labs(x="Test Prep Score", y="Math Score")

#pengecekan outliers menggunakan fungsi
boxplot.stats(datas_1$math)$out

#nomor 2
#recode gender, parent education level, and lunch
datas_1$parent_education_level = recode(datas_1$parent_education_level,  "high school"='0',
                                                                "some high school"='1',
                                                                "some college"='2',
                                                                "associate's degree"='3',
                                                                "bachelor's degree"='4',
                                                                "master's degree"='5')
#recode gender
datas_1$gender = recode(datas_1$gender,'female'='0', 'male'='1')
#recode lunch
datas_1$lunch=recode(datas_1$lunch, "free/reduced"='0',"standard"='1')
head(datas_1)

#nomor 3
#urut data berdasarkan kolom race
datas_1 = datas_1[order(datas_1$race),]
head(datas_1)

#Nomor 4
#penambahan indeks huruf
datas_1$alphabet_grade = factor(cut(datas_1$math,
                                    breaks = c(-Inf, 54, 59, 64, 69, 74, 79, Inf),
                                    labels = c("E","D","C","BC","B","AB","A")))
#penambahan status kelulusan
datas_1$status = factor(cut(datas_1$math,
                            breaks = c(-Inf, 54, 59, 64, 69, 74, 79, Inf),
                            labels = c("TIDAK LULUS", "TIDAK LULUS", "LULUS", 
                                       "LULUS", "LULUS", "LULUS", "LULUS")))
head(datas_1)

#nomor 5
idSubjek = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
weathers = c("Rain", "Sunny", "Windy",
             "Rain", "Sunny", "Windy",
             "Rain", "Sunny", "Windy",
             "Rain", "Sunny", "Windy",
             "Rain", "Sunny", "Windy")
kind_stem = c("spike", "spike", "spike", 
              "none", "none", "none",
              "none", "none", "none", 
              "spike", "spike", "spike",
              "none", "none", "none")
color_petals = c("white", "white", "white", 
                 "red", "red", "red",
                 "red", "red", "red",
                 "white", "white", "white",
                 "violet", "violet", "violet")
height_cm = c(5, 6, 7,
              8, 10, 9,
              12, 13, 8,
              10, 7, 9,
              6, 7, 8)
datas_new= data.frame(idSubjek,kind_stem, color_petals, weathers, height_cm)
datas_new
#Long to wide reshape
wides = reshape(datas_new, idvar = c("idSubjek", "kind_stem","color_petals"),
        timevar = "weathers", direction = "wide")
wides
#wide to long reshape
longs = reshape(wides, list(c("height_cm.Rain", "height_cm.Sunny", "height_cm.Windy")), direction = "long")
longs

#nomor 6
#menyimupulkan melalui data terkait preparation course
#tidak mengikuti
no_course = subset(datas_1, datas_1$test_prep_course=='none')
table(no_course$status)
ggplot(no_course, aes(x="", y=test_prep_course, fill=status)) +
  geom_bar(stat='identity', width=1) +
  coord_polar("y", start=0) +
  labs(x="", y="None Prep course")

#mengikuti
course = subset(datas_1, datas_1$test_prep_course=='completed')
table(course$status)
ggplot(course, aes(x="", y=test_prep_course, fill=status)) +
  geom_bar(stat='identity', width=1) +
  coord_polar("y", start=0) +
  labs(x="", y="Prep course")
