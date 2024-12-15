---
title: "Statistical inference with the GSS data"
output: 
  html_document: 
    fig_height: 4
    highlight: pygments
    theme: spacelab
---

## Setup

### Load packages

```{r load-packages, message = FALSE}
library(ggplot2)
library(dplyr)
library(statsr)
```

### Load data

```{r load-data}
load("gss.Rdata")
```

## Part 1: Data

Since 1972, GSS has been tracking and gathering hundreds of trends of data on contemporary American society in order to maintain and explain trends and constants in attitude, behavior and attributes. Altogether the GSS is the single best source for sociological and attitudinal trend data covering the United States.  It allows researchers to examine the structure and functioning of society in general as well as the role played by relevant subgroups and to compare the United States to other nations. The GSS contains a standard core of covered topics like crime and violence, stress, sexual behavior, quality of life, race and religion among many others.<br>
The data contains 57061 observations of 114 variables. The data has been cleaned by removing missing values from the responses and factor variables were created when appropriate to facilitate proper analysis using R.<br>

As a large representative random sampling was drawn, the data for the sample can be generalized to the adult population of the participating states.<br>

This long term survey is a type of observational study. Therefore, it won’t be possible to make causal inferences from the data.<br>

## Part 2: Research question

To find if there's any correlation between the respondent's social class and their opinion on homosexuality. This question piqued my interest as it can help unravel the social stratification of sexuality related issues in the American society, if it exists. Understanding these issues helps in crafting sexual health policies and planning and implementing programs to enhance and protect their integrity in the society. <br>
The variables used in the analysis are : <br> 
1. year : GSS year for the respondent<br>
2. class : Subjective class identification <br>
3. homosex : Homosexual sex relations<br>

Modifications made : <br>
1. Removal of all N/A<br>
2. The duration was divided as before 2006 and after 2006 to evaluate any time bound changes in attitude

## Part 3: Exploratory data analysis

```{r}
#Subset the data
study_data <- select(gss,year,class,homosex) %>% na.omit() %>%
  mutate(positive=grepl("Not Wrong At All",homosex)) %>%
  mutate(recent=as.factor(ifelse(year>=2006,"R","H")))

#summary of the whole data

all_time <- study_data
summary(all_time)

```
```{r}
all_time_table <- table(all_time$class,all_time$positive)
all_time_table
```
```{r}
prop.table(all_time_table)
```

```{r}
#Visualization of data
g <- ggplot(all_time) + aes(x=class,fill=positive) + geom_bar(position = "fill") +
  labs(x="Social class",y="Proportion",title="Opinion of social class towards homosexuality") +
  scale_fill_discrete(name="Opinion",labels=c("Positive view","Negative view"))
g
```
<br>We can see that there is difference in attitude towards homosexuals among the different social classes before 2006(historical time). We do not notice much difference in bias towards homosexuals as the social class progresses.
<br>
Now, for the analysis of year 2006 and later.<br>
```{r}
since_2006 <- filter(all_time,recent=="R")
summary(since_2006)
```
```{r}
since_2006_table <- table(since_2006$class,since_2006$positive)
since_2006_table
```
```{r}
prop.table(since_2006_table)
```
```{r}
#Visualization of data
h <- ggplot(since_2006) + aes(x=class,fill=positive) + geom_bar(position = "fill") + 
  labs(x="Social class",y="Proportion",title="View of social class towards homosexuality") +
  scale_fill_discrete(name="Opinion",labels=c("Positive view","Negative view"))
h
```
<br>We again see difference in attitude towards homosexuals among the different social classes from 2006 and afterwards(recent times). Again not much difference in attitude bias towards homosexuals as social class progresses in recent times.
<br>
A visual comparison between historical and recent times was analyzed.
```{r}
 i<- ggplot(all_time) + aes(x=recent,fill=positive) + geom_bar(position = "fill") + facet_grid(.~class) +
  labs(x="Historical versus Recent",y="Proportion",title="View of social class towards homosexuality") +
  scale_fill_discrete(name="Opinion",labels=c("Positive view","Negative view"))
i
```
<br>As we can see from the above analysis graph, we can see a considerable increase in biased attitude towards homosexuals in the recent times among all classes

## Part 4: Inference

Hypotheses :<br>
H0 (Null hypothesis) : The respondent's opinion towards homosexuality is independent of their social class. <br>
HA (Alternate hypothesis) : There is an association between respondent's opinion towards homosexuality and their social class.<br>

Method of Choice : Since the analysis involves 2 categorical variables (i.e social class and opinion towards homosexuality), chi-squared test for independence is opted which is used when comparing 2 categorical variables where one variable has more than 2 levels.
```{r}
str(all_time)
```
```{r}
str(since_2006)
```
Conditions : <br>
1. Independence between observations. This is unchanged as random sampling was implemented in GSS.<br>
2. As seen below, there are at least 5 counts for each cell except "no class" category.<br>
```{r}
study_table1 <- table(all_time$class,all_time$positive)
study_table1
```
```{r}
sum(study_table1<=5)
```
So, we remove the row beloging to "no class" category.<br>
```{r}
t1 <- study_table1[-5,]
t1
```
```{r}
study_table2 <- table(since_2006$class,since_2006$positive)
study_table2
```
Now 2 cells have count < 5.
```{r}
sum(study_table2<=5)
```
Again, removing the zero cells.
```{r}
t2 <- study_table2[-5,]
t2
```
Now, Chi-Squared test for independence was performed on both the data sets - the entire data set and the recent sub-set.
```{r}
c_all_time <- chisq.test(t1, all_time$class,all_time$positive)
c_all_time
```
```{r}
c_since_2006 <- chisq.test(t2, since_2006$class,since_2006$positive)
c_since_2006
```
As can be seen from the above results obtained, in both the cases, the null hypothesis was rejected and there is a significant association between social class and their opinion on homosexuality of respondents.<br>
Hence, we can infer that "Attitude towards homosexuality varies by social class"
