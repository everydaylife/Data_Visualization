# Comparing Demographics of Facebook Users to Non-Users
# Jump Start Code Segment via Thomas Miller

# load required libraries 
library(ggplot2)
library(vcd) 
library(plyr)

# Load R binary file
load("facebook.RData")

# set text data fields as character
original.facebook.data.frame$pial4vb <- as.character(original.facebook.data.frame$pial4vb)
original.facebook.data.frame$pial7vb <- as.character(original.facebook.data.frame$pial7vb)

# define sex as factor variable
original.facebook.data.frame$sex <- factor(original.facebook.data.frame$sex,
  levels = c(1,2), labels = c("Male","Female"))

# define factor variable for facebook user
original.facebook.data.frame$facebook_user <- ifelse((original.facebook.data.frame$pial2 == 1),2,1)
original.facebook.data.frame$facebook_user <- factor(original.facebook.data.frame$facebook_user,
  levels = c(1,2), labels = c("Non-User","User"))

# define new data fame for work
working.data.frame <- original.facebook.data.frame

# define Internet user data frame
net <- subset(working.data.frame, subset = (intuse == 1))

# list the variables and show structure in the data frame
print(names(net))
print(str(net))

# 1. Basic understanding of data: histograms and bar charts!
# all variables looked at, but one stood out right away
qplot(net$age)

# lots of 99 year olds in this survey? unlikley
# checked further and found the following variables using 8, 9 or 99 for missing values
# intmob(8), employ, par, educ2, hisp, race (9), age, inc (99)
# will exclude during chart creation process

# Begin deeper dive: understand who is and who is not using Facebook
# trying both mosaic and bar charts
# Not color blind friendly, but I like the ggplot color palette

# find hex codes to match mosaic charts to ggplot charts
require(ggplot2) 
n <- 2 
hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65) 
# output: "#F8766D" "#00BFC4"; will use these in mosaic plots for consistency b/w charts

# Understanding Gender
mosaic( ~ sex + facebook_user, data = net,
  labeling_args = list(set_varnames = c(sex = "", facebook_user = "Facebook Usage")),
  highlighting = "facebook_user",
  highlighting_fill = c("#F8766D","#00BFC4"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

p <- ggplot(net, aes(x=sex, fill=facebook_user)) +
  geom_bar(position="dodge")

p2 <- p + ggtitle("Gender of Facebook Users") + 
  labs(fill="Facebook User") +
  xlab("Title") + # for when I want to change x axis labels
  ylab("Title") + # for when I want to change y axis labels
  theme(axis.title.x=element_blank()) + # for when I want to exclude x axis labels
  theme(axis.title.y=element_blank()) # for when I want to exclude y axis labels

pdf(file = "plot_gender.pdf", width = 11, height = 8.5)
print(p2)
dev.off()

# Understanding Ethnicity
# exclude NAs
newRace <- net[ which(net$race!=9), ]

p <- ggplot(newRace, aes(x=race, fill=facebook_user)) +
  geom_bar(position="dodge")

p + ggtitle("Race of Facebook Users") + 
  labs(fill="Facebook User") +
  xlab("Title") + # for when I want to change x axis labels
  ylab("Title") + # for when I want to change y axis labels
  theme(axis.title.x=element_blank()) + # for when I want to exclude x axis labels
  theme(axis.title.y=element_blank()) + # for when I want to exclude y axis labels
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6),
  labels=c("White", "African\nAmerican", "Asian",
  "Mixed", "Native\nAmerican", "Other"))

# this needs to be a proportional (100% stacked) bar chart to understand the differences

# Understanding income
# exclude NAs for income
newInc <- net[ which(net$inc!=99), ]

mosaic( ~ inc + facebook_user, data = newInc,
        labeling_args = list(set_varnames = c(inc = "", facebook_user = "Facebook Usage")),
        highlighting = "facebook_user",
        highlighting_fill = c("#F8766D","#00BFC4"),
        rot_labels = c(left = 0, top = 0),
        pos_labels = c("center","center"),
        offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

p <- ggplot(newInc, aes(x=inc, fill=facebook_user)) +
  geom_bar(position="dodge")

p + ggtitle("Facebook Users and Non-Users by Income") + 
  labs(fill="Facebook User") +
  xlab("Title") + # for when I want to change x axis labels
  ylab("Title") + # for when I want to change y axis labels
  theme(axis.title.x=element_blank()) + # for when I want to exclude x axis labels
  theme(axis.title.y=element_blank()) + # for when I want to exclude y axis labels
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  labels=c("<10K", "<20K", "<30K", "<40K", "<50K", "<75K", "<100K", "<150K", ">150K"))

# school
# exclude NA
newEduc2 <- net[ which(net$educ2!=9), ]

mosaic( ~ educ2 + facebook_user, data = newEduc2,
  labeling_args = list(set_varnames = c(neweduc2 = "Education Level", facebook_user = "Facebook Usage")),
  highlighting = "facebook_user",
  highlighting_fill = c("#F8766D","#00BFC4"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

p <- ggplot(net, aes(x=educ2, fill=facebook_user)) +
  geom_bar(position="dodge")

p + ggtitle("Facebook Users and Non-Users by Education") + 
  labs(fill="Facebook User") +
  xlab("Title") + # for when I want to change x axis labels
  ylab("Title") + # for when I want to change y axis labels
  theme(axis.title.x=element_blank()) + # for when I want to exclude x axis labels
  theme(axis.title.y=element_blank()) + # for when I want to exclude y axis labels
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8),
  labels=c("No High School", "Some High School", "High School Grad", "Some College", "2 Year Degree", "4 Year Degree", "Some PostGrad", "PostGrad Degree"))

# Understanding Hispanic
# exclude NAs for Hispanic
newHisp <- net[ which(net$hisp!=9), ]

mosaic( ~ hisp + facebook_user, data = newHisp,
  labeling_args = list(set_varnames = c(hisp = "", facebook_user = "Facebook Usage")),
  highlighting = "facebook_user",
  highlighting_fill = c("#F8766D","#00BFC4"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

ggplot(net, aes(x=hisp, fill=facebook_user)) +
  geom_bar(position="dodge")

ggplot(newHisp, aes(x=factor(hisp), fill=facebook_user)) +
  geom_bar(position="dodge")

# this needs to be a proportional (100% stacked) bar chart to understand the differences

# Understanding Parents
# Exclude NAs
newPar <- net[ which(net$par!=9), ]

mosaic( ~ par + facebook_user, data = newPar,
  labeling_args = list(set_varnames = c(par = "", facebook_user = "Facebook Usage")),
  highlighting = "facebook_user",
  highlighting_fill = c("#F8766D","#00BFC4"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

ggplot(newPar, aes(x=factor(par), fill=facebook_user)) +
  geom_bar(position="dodge")

# Understanding Employed
newEmploy <- net[ which(net$employ!=9), ]
newEmploy

mosaic( ~ employ + facebook_user, data = net,
  labeling_args = list(set_varnames = c(employ = "", facebook_user = "Facebook Usage")),
  highlighting = "facebook_user",
  highlighting_fill = c("#F8766D","#00BFC4"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

ggplot(net, aes(x=employ, fill=facebook_user)) +
  geom_bar(position="dodge")

ggplot(newEmploy, aes(x=factor(employ), fill=facebook_user)) +
  geom_bar(position="dodge")

# should also be changed to % for better viz

# Understanding mobile
# Exclude NAs
newIntmob <- net[ which(net$intmob!=8), ]

mosaic( ~ intmob + facebook_user, data = newIntmob,
  labeling_args = list(set_varnames = c(intmob = "", facebook_user = "Facebook Usage")),
  highlighting = "facebook_user",
  highlighting_fill = c("#F8766D","#00BFC4"),
  rot_labels = c(left = 0, top = 0),
  pos_labels = c("center","center"),
  offset_labels = c(0.0,0.6))
dev.off()  # close the pdf graphics file

ggplot(net, aes(x=intmob, fill=facebook_user)) +
  geom_bar(position="dodge")

ggplot(newIntmob, aes(x=factor(intmob), fill=facebook_user)) +
  geom_bar(position="dodge")
