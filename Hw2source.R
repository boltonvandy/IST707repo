#Reading in the data, replacing any possible blank entries as 'NA'.

storyteller<- read_csv("C:/Users/user/Downloads/data-storyteller.csv", na = c(""))

#checking data types to see what may need changing
str(storyteller)

#The School column is of the character type and it should be a factor

storyteller$School<-factor(storyteller$School)

#The section column is of the Numeric type and should be be a factor instead

storyteller$Section<-factor(storyteller$Section)

#Each of the remaining columns is a discrete count of the students in each category.
#As it is not continuous the columns 'Very Ahead', 'Middling', 'Behind', 'More behind', 'Very behind'
#and 'Completed' should all be integers.

storyteller$`Very Ahead +5`<-as.integer(storyteller$`Very Ahead +5`)
storyteller$`Middling +0`<-as.integer(storyteller$`Middling +0`)
storyteller$`Behind -1-5`<-as.integer(storyteller$`Behind -1-5`)
storyteller$`More Behind -6-10`<-as.integer(storyteller$`More Behind -6-10`)
storyteller$`Very Behind -11`<-as.integer(storyteller$`Very Behind -11`)
storyteller$Completed<-as.integer(storyteller$Completed)

#Reordering columns to get a cleaner picture. I.E. 'Completed' being shifted to the other side.
#And section being a unique identifier is moved to the leftmost column.

storytellerdummy<-storyteller[,c(2,1,8,3,4,5,6,7)]

storyteller<-storytellerdummy

#displaying top 5 rows
head(storyteller)

#Checking for any NA values
sum(is.na(storyteller))

#There are no NA values in this dataset.

#The dataset is cleaned
head(storyteller)

#Surface level observation: To be considered 'ahead' in any way, there are two categories:
#'Very Ahead' and 'Completed'. There are 3 cateories in place to describe 'behind'
#which may lead the responses to be lopsided in favor of generally 'behind' over 'generally ahead'

# Creating a bar chart to show the number of sections from each school
SchoolValues<-c(length(which(storyteller$School=='A')), length(which(storyteller$School=='B')), length(which(storyteller$School=='C')), length(which(storyteller$School=='D')), length(which(storyteller$School=='E')))

barplot(SchoolValues, names.arg = c('A', 'B', 'C', 'D', 'E'), main='Number of sections Per School')

#plotting section and Completed and summarizing the data

barplot(storyteller$Completed, main='number of completed students per section', names.arg = c(1:30))

summary(storyteller$Completed)
#Plotting section and Very Ahead and summarizing

barplot(storyteller$`Very Ahead +5`, main='number of very ahead students per section', names.arg = c(1:30))

summary(storyteller$`Very Ahead +5`)
#plotting section and Middling and summarizing

barplot(storyteller$`Middling +0`, main='number of Middling students per section', names.arg = c(1:30))

summary(storyteller$`Middling +0`)
#plotting section and Behind and summarizing

barplot(storyteller$`Behind -1-5`, main='number of Behind students per section', names.arg = c(1:30))

summary(storyteller$`Behind -1-5`)
#plotting section and More Behind

barplot(storyteller$`More Behind -6-10`, main='number of More Behind students per section', names.arg = c(1:30))


summary(storyteller$`More Behind -6-10`)
#Plotting section and Very Behind

barplot(storyteller$`Very Behind -11`, main='number of Very Behind students per section', names.arg = c(1:30))

summary(storyteller$`Very Behind -11`)
#determining the number of students in each category
StudentSums<-colSums(storyteller[,3:8])

sum(StudentSums)

#determining the amount of students in each section

SectionSums<-rowSums(storyteller[,3:8])

data.frame(SectionSums)

#Creating a barplot to show distribution

StudentSums<-colSums(storyteller[,3:8])
barplot(StudentSums, main="Student totals across all categories")

#There is a gap between middling and completed, meaning that the only students who can be described
#as 'ahead' have already finished the program.

#there is no category that describes between 'Middling' and 'Very Ahead'
#This means that survey respondents would have to decide between 'middling' or 'Very Ahead' 
#should they fall in the middle. This may skew the data towards one side or the other, however it
#is reasonable to assume that survey respondents would stick to a more honest 'Middling Answer'
#rather than exaggerating their students' success.

#The data seems to be centered on the 'Behind' category, and by a large margin.

StudentSums/sum(StudentSums)

#14% of students are on track in the middling category.
#Nearly 20% of students have completed the program.
#Nearly 50% of students in this program are in the 'behind' category alone.
#Students in the bottom two categories make up 20% of the sample. Meaning ~70% of students
#are behind in the curicculum. 

plot(storyteller$Completed, storyteller$`Middling +0`)
#If we were to assume that students are generally not doing well because the program is difficult, 
#there is is still a discrepancy with the number of students that have 
#completed the course ahead of schedule.Essentially, A fifth of the students have nothing to
#work on for the remainder of the time.

#Breaking data down into individual schools to see if we can better understand the data

storytellerA<-storyteller[which(storyteller$School == "A"),]
storytellerB<-storyteller[which(storyteller$School == "B"),]
storytellerC<-storyteller[which(storyteller$School == "C"),]
storytellerD<-storyteller[which(storyteller$School == "D"),]
storytellerE<-storyteller[which(storyteller$School == "E"),]

StudentSumsA<-colSums(storytellerA[3:8])
StudentSumsA

barplot(StudentSumsA, main = "School A")

StudentSumsB<-colSums(storytellerB[3:8])
StudentSumsB

barplot(StudentSumsB, main = "School B")

StudentSumsC<-colSums(storytellerC[3:8])
StudentSumsC

barplot(StudentSumsC, main = "School C")

StudentSumsD<-colSums(storytellerD[3:8])
StudentSumsD

barplot(StudentSumsD, main = "School D")

StudentSumsE<-colSums(storytellerE[3:8])
StudentSumsE

barplot(StudentSumsE, main = "School E")

#Schools B and D do not follow the same picture that was shown by the data combined. 
#A, C, and E follow generally the same pattern as shown by figure 1
#B shows many students ahead of the curriculum, completing the program. 
#The majority of students that are behind are behind by 1-5 assignments and very few are in
#the lowest two categories. 
#This is the most positive picture that the data shows out of the schools.
#School D is the opposite with a small percentage of students having completed all assignments
#and a large percentage of students being considered 'Very Behind'. 

#Comparing the two schools as a representation of the population of program takers
sum(StudentSumsB)/sum(StudentSums)

sum(StudentSumsD)/sum(StudentSums)

#School D has a small number of students, meaning that the data from school D has a greater
#probability of not being representative of the situation and may be acconted for by random chance.
#School B is a sample that is approximately one quarter of the overall population, making it 
#more representative of the situation. It also shows that school B is doing something that the 
#rest of the schools are not in order to get students through the course.

#looking at the data to determine quality sections
storytellerB

barplot(storytellerB$Completed, names.arg =c(1:12),ylim=c(0,30), main = "Students Completed by Section")

#Sections 6, 10, and 12 have the most promising results within school B.
rowSums(storytellerB[,3:8])

barplot(storytellerB$`Behind -1-5`+storytellerB$`More Behind -6-10`+storytellerB$`Very Behind -11`, names.arg =c(1:12), ylim=c(0,30), main = "Students Behind by Section")
