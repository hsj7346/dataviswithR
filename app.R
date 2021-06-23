# Loading packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggthemes)
library(patchwork)

# Load data
data <- read_csv("data/kaggle_survey_2020_responses.csv") %>% 
    filter(Q5 != "Currently not employed") %>% 
    mutate(Q5 = factor(Q5, 
                       levels = c("Data Engineer", 
                                  "Software Engineer", 
                                  "Data Scientist", 
                                  "Data Analyst",  
                                  "Research Scientist", 
                                  "Other", 
                                  "Statistician", 
                                  "Product/Project Manager", 
                                  "Machine Learning Engineer", 
                                  "Business Analyst", 
                                  "DBA/Database Engineer"),
                       labels = c("Data Engineer", 
                                  "Software Engineer", 
                                  "Data Scientist", 
                                  "Data Analyst",  
                                  "Research Scientist", 
                                  "Other", 
                                  "Statistician", 
                                  "Project Manager", 
                                  "ML Engineer", 
                                  "Business Analyst", 
                                  "DBA/Database Engineer")),
            Q4 = factor(Q4,
                       levels = c("Master’s degree", 
                                  "Bachelor’s degree", 
                                  "Doctoral degree", 
                                  "No formal education past high school", 
                                  "Some college/university study without earning a bachelor’s degree", 
                                  "Professional degree"),
                       labels = c("Master’s degree", 
                                  "Bachelor’s degree", 
                                  "Doctoral degree", 
                                  "High School degree", 
                                  "None Bachelor's degree", 
                                  "Professional degree")),
            Q8 = factor(Q8,
                        levels = c("Python", "R", "C++", 
                                   "SQL", "Java", "MATLAB", 
                                   "C", "Other", "Javascript", 
                                   "Julia", "Switft", "Bash"),
                        labels = c("Python", "R", "C++", 
                                   "SQL", "Java", "MATLAB", 
                                   "C", "Other", "Javascript", 
                                   "Julia", "Switft", "Bash"))) %>% 
    select(Q5, contains("Q7")) %>% 
    pivot_longer(
        cols = contains("Q7"),
        names_to = "variable1",
        values_to = "Q7"
    ) %>% 
    drop_na(Q7)


data2 <- read_csv("data/kaggle_survey_2020_responses.csv") %>% 
    filter(Q5 != "Currently not employed") %>% 
    mutate(Q5 = factor(Q5, 
                       levels = c("Data Engineer", 
                                  "Software Engineer", 
                                  "Data Scientist", 
                                  "Data Analyst",  
                                  "Research Scientist", 
                                  "Other", 
                                  "Statistician", 
                                  "Product/Project Manager", 
                                  "Machine Learning Engineer", 
                                  "Business Analyst", 
                                  "DBA/Database Engineer"),
                       labels = c("Data Engineer", 
                                  "Software Engineer", 
                                  "Data Scientist", 
                                  "Data Analyst",  
                                  "Research Scientist", 
                                  "Other", 
                                  "Statistician", 
                                  "Project Manager", 
                                  "ML Engineer", 
                                  "Business Analyst", 
                                  "DBA/Database Engineer"))) %>% 
    select(Q5, contains("Q14")) %>% 
    pivot_longer(
        cols = contains("Q14"),
        names_to = "variable1",
        values_to = "Q14"
    ) %>% 
    drop_na(Q14)


               
data3 <- read_csv("data/kaggle_survey_2020_responses.csv") %>% 
    filter(Q5 != "Currently not employed") %>% 
    mutate(Q5 = factor(Q5, 
                       levels = c("Data Engineer", 
                                  "Software Engineer", 
                                  "Data Scientist", 
                                  "Data Analyst",  
                                  "Research Scientist", 
                                  "Other", 
                                  "Statistician", 
                                  "Product/Project Manager", 
                                  "Machine Learning Engineer", 
                                  "Business Analyst", 
                                  "DBA/Database Engineer"),
                       labels = c("Data Engineer", 
                                  "Software Engineer", 
                                  "Data Scientist", 
                                  "Data Analyst",  
                                  "Research Scientist", 
                                  "Other", 
                                  "Statistician", 
                                  "Project Manager", 
                                  "ML Engineer", 
                                  "Business Analyst", 
                                  "DBA/Database Engineer"))) %>% 
    select(Q5, contains("Q16")) %>% 
    pivot_longer(
        cols = contains("Q16"),
        names_to = "variable1",
        values_to = "Q16"
    ) %>% 
    drop_na(Q16)

data2020 <- read_csv("data/kaggle_survey_2020_responses.csv")
data2019 <- read_csv("data/kaggle_survey_2019_responses.csv")
data2018 <- read_csv("data/kaggle_survey_2018_responses.csv")
data2017 <- read_csv("data/kaggle_survey_2017_responses.csv") %>% 
    mutate(Age = case_when(
        between(Age, 18,21) ~ "18-21",
        between(Age, 22,24) ~ "22-24",
        between(Age, 25,29) ~ "25-29",
        between(Age, 30,34) ~ "30-34",
        between(Age, 35,39) ~ "35-39",
        between(Age, 40,44) ~ "40-44",
        between(Age, 45,49) ~ "45-49",
        between(Age, 50,54) ~ "50-54",
        between(Age, 55,59) ~ "55-59",
        between(Age, 60,64) ~ "60-64",
        between(Age, 65,69) ~ "65-69",
        between(Age, 70,89) ~ "70+",
        between(Age, 80,89) ~ "80+"
    ))

gender_2017_m <- data2017 %>% 
    filter(GenderSelect == "Male") %>% 
    mutate(Year = 2017) %>% 
    rename(Gender = GenderSelect) %>% 
    select(Gender, Age, Year) %>% 
    drop_na(Age)
gender_2017_f <- data2017 %>% 
    filter(GenderSelect == "Female") %>% 
    mutate(Year = 2017) %>% 
    rename(Gender = GenderSelect) %>% 
    select(Gender, Age, Year) %>% 
    drop_na(Age)
gender_2018_m <- data2018 %>% 
    filter(Q1 == "Male") %>% 
    mutate(Year = 2018) %>% 
    rename(Gender = Q1, Age = Q2) %>% 
    select(Gender, Age, Year)
gender_2018_f <- data2018 %>% 
    filter(Q1 == "Female") %>% 
    mutate(Year = 2018) %>% 
    rename(Gender = Q1, Age = Q2) %>% 
    select(Gender, Age, Year)
gender_2019_m <- data2019 %>% 
    filter(Q2 == "Male") %>% 
    mutate(Year = 2019) %>% 
    rename(Gender = Q2, Age = Q1) %>% 
    select(Gender, Age, Year)
gender_2019_f <- data2019 %>% 
    filter(Q2 == "Female") %>% 
    mutate(Year = 2019) %>% 
    rename(Gender = Q2, Age = Q1) %>% 
    select(Gender, Age, Year)
gender_2020_m <- data2020 %>% 
    filter(Q2 == "Man") %>% 
    mutate(Year = 2020) %>% 
    rename(Gender = Q2, Age = Q1) %>% 
    mutate(Gender = str_replace(Gender, "Man", "Male")) %>% 
    select(Gender, Age, Year)
gender_2020_f <- data2020 %>% 
    filter(Q2 == "Woman") %>% 
    mutate(Year = 2020) %>% 
    rename(Gender = Q2, Age = Q1) %>% 
    mutate(Gender = str_replace(Gender, "Woman", "Female")) %>% 
    select(Gender, Age, Year)

gender1 <- rbind(gender_2017_m,gender_2017_f)
gender2 <- rbind(gender_2018_m,gender_2018_f)
gender3 <- rbind(gender_2019_m,gender_2019_f)
gender4 <- rbind(gender_2020_m,gender_2020_f)
gender5 <- rbind(gender1,gender2)
gender6 <- rbind(gender3,gender4)
gender <- rbind(gender5,gender6)


edc_2017 <- data2017 %>% 
    mutate(Year = 2017) %>% 
    rename(Gender = GenderSelect) %>% 
    rename(Educ = FormalEducation) %>% 
    rename(Job = CurrentJobTitleSelect) %>% 
    mutate(Educ = case_when(
        Educ == "Some college/university study without earning a bachelor's degree" ~ "Other",
        Educ == "I did not complete any formal education past high school" ~ "High school",
        Educ == "Bachelor's degree" ~ "Bachelor's degree",
        Educ == "Master's degree" ~ "Master's degree",
        Educ == "Doctoral degree" ~ "Doctoral degree",
        Educ == "Professional degree" ~ "Professional degree",
        Educ == "I prefer not to answer" ~ "Other"
    )) %>% 
    mutate(Job = case_when(
        Job == "Software Developer/Software Engineer" ~ "Software Engineer",
        Job == "Scientist/Researcher" ~ "Research Scientist",
        Job == "Researcher" ~ "Research Scientist",
        Job == "Data Miner" ~ "Data Engineer",
        Job == "Engineer" ~ "Data Engineer",
        Job == "Operations Research Practitioner" ~ "Other",
        Job == "Computer Scientist" ~ "Other",
        Job == "Predictive Modeler" ~ "Other",
        Job == "Programmer" ~ "Other",
        Job == "DBA/Database Engineer" ~ "DBA/Database Engineer",
        Job == "Data Scientist" ~ "Data Scientist",
        Job == "Business Analyst" ~ "Business Analyst",
        Job == "Other" ~ "Other",
        Job == "Data Analyst" ~ "Data Analyst",
        Job == "Machine Learning Engineer" ~ "Machine Learning Engineer",
        Job == "Statistician" ~ "Statistician"
    )) %>% 
    select(Year,Job,Educ) %>% 
    drop_na(Job,Educ)

edc_2018 <- data2018 %>% 
    mutate(Year = 2018) %>% 
    rename(Educ = Q4) %>% 
    rename(Job = Q6) %>% 
    mutate(Educ = case_when(
        Educ == "Some college/university study without earning a bachelor's degree" ~ "Other",
        Educ == "No formal education past high school" ~ "High school",
        Educ == "Bachelor’s degree" ~ "Bachelor's degree",
        Educ == "Master’s degree" ~ "Master's degree",
        Educ == "Doctoral degree" ~ "Doctoral degree",
        Educ == "Professional degree" ~ "Professional degree",
        Educ == "I prefer not to answer" ~ "Other",
        Educ == "Some college/university study without earning a bachelor’s degree"~"Other"
    )) %>% 
    mutate(Job = case_when(
        Job == "Consultant"~"Other",
        Job == "Other" ~"Other",
        Job =="Data Scientist"~"Data Scientist",
        Job =="Not employed"~"Other",
        Job =="Data Analyst"~"Data Analyst",
        Job =="Software Engineer"~"Software Engineer",
        Job =="Student"~"Other",
        Job =="Research Assistant"~"Other",
        Job =="Chief Officer"~"Other",
        Job =="Manager"~"Other",
        Job =="Research Scientist"~"Research Scientist",
        Job =="Business Analyst"~"Business Analyst",
        Job =="Data Engineer"~"Data Engineer",
        Job =="Developer Advocate"~"Other",
        Job =="Marketing Analyst"~"Other",
        Job =="Product/Project Manager"~"Other",
        Job =="Principal Investigator"~"Other",
        Job =="Salesperson"~"Other",
        Job =="DBA/Database Engineer"~"DBA/Database Engineer",
        Job =="Statistician"~"Statistician",
        Job =="Data Journalist"~"Other"
    )) %>% 
    select(Year,Job,Educ) %>% 
    drop_na(Job,Educ)

edc_2019 <- data2019 %>% 
    mutate(Year = 2019) %>% 
    rename(Educ = Q4) %>% 
    rename(Job = Q5) %>% 
    mutate(Educ = case_when(
        Educ == "Some college/university study without earning a bachelor's degree" ~ "Other",
        Educ == "No formal education past high school" ~ "High school",
        Educ == "Bachelor’s degree" ~ "Bachelor's degree",
        Educ == "Master’s degree" ~ "Master's degree",
        Educ == "Doctoral degree" ~ "Doctoral degree",
        Educ == "Professional degree" ~ "Professional degree",
        Educ == "I prefer not to answer" ~ "Other"
    )) %>% 
    mutate(Job = case_when(
        Job == "Other" ~"Other",
        Job =="Data Scientist"~"Data Scientist",
        Job =="Not employed"~"Other",
        Job =="Data Analyst"~"Data Analyst",
        Job =="Software Engineer"~"Software Engineer",
        Job =="Student"~"Other",
        Job =="Research Scientist"~"Research Scientist",
        Job =="Business Analyst"~"Business Analyst",
        Job =="Data Engineer"~"Data Engineer",
        Job =="Product/Project Manager"~"Other",
        Job =="DBA/Database Engineer"~"DBA/Database Engineer",
        Job =="Statistician"~"Statistician"
    )) %>% 
    select(Year,Job,Educ) %>% 
    drop_na(Job,Educ)

edc_2020 <- data2020 %>% 
    mutate(Year = 2020) %>% 
    rename(Educ = Q4) %>% 
    rename(Job = Q5) %>% 
    mutate(Educ = case_when(
        Educ == "Some college/university study without earning a bachelor’s degree" ~ "Other",
        Educ == "No formal education past high school" ~ "High school",
        Educ == "Bachelor’s degree" ~ "Bachelor's degree",
        Educ == "Master’s degree" ~ "Master's degree",
        Educ == "Doctoral degree" ~ "Doctoral degree",
        Educ == "Professional degree" ~ "Professional degree",
        Educ == "I prefer not to answer" ~ "Other",
        Educ == "Some college/university study without earning a bachelor’s degree" ~ "Other"
    )) %>% 
    mutate(Job = case_when(
        Job == "Other" ~"Other",
        Job == "Currently not employed" ~ "Other",
        Job == "Product/Project Manager" ~ "Other",
        Job == "Machine Learning Engineer" ~ "Other",
        Job =="Data Scientist"~"Data Scientist",
        Job =="Not employed"~"Other",
        Job =="Data Analyst"~"Data Analyst",
        Job =="Software Engineer"~"Software Engineer",
        Job =="Student"~"Other",
        Job =="Research Scientist"~"Research Scientist",
        Job =="Business Analyst"~"Business Analyst",
        Job =="Data Engineer"~"Data Engineer",
        Job =="Product/Project Manager"~"Other",
        Job =="DBA/Database Engineer"~"DBA/Database Engineer",
        Job =="Statistician"~"Statistician",
        Job == "Machine Learning Engineer" ~ "Machine Learning Engineer"
    )) %>% 
    select(Year,Job,Educ) %>% 
    drop_na(Job,Educ)


edc1 <- rbind(edc_2017, edc_2018)
edc2 <- rbind(edc_2019, edc_2020)
edc <- rbind(edc1,edc2)

pay2020m <- data2020 %>% 
    filter(Q2 == "Man") %>% 
    mutate(Year = 2020) %>% 
    mutate(Q2 = str_replace(Q2, "Man", "Male")) %>% 
    mutate(Q24 = case_when(
        Q24 == "$0-999" ~ "0-100k",
        Q24 == "1,000-1,999" ~ "0-100k",
        Q24 == "10,000-14,999" ~ "0-100k",
        Q24 == "100,000-124,999" ~ "100-250k",
        Q24 == "125,000-149,999" ~ "100-250k",
        Q24 == "15,000-19,999" ~ "0-100k",
        Q24 == "150,000-199,999" ~ "100-250k",
        Q24 == "2,000-2,999" ~ "0-100k",
        Q24 == "20,000-24,999" ~ "0-100k",
        Q24 == "200,000-249,999" ~ "100-250k",
        Q24 == "25,000-29,999" ~ "0-100k",
        Q24 == "250,000-299,999" ~ "250k+",
        Q24 == "3,000-3,999" ~ "0-100k",
        Q24 == "30,000-39,999" ~ "0-100k",
        Q24 == "300,000-500,000" ~ "250k+",
        Q24 == "4,000-4,999" ~ "0-100k",
        Q24 == "5,000-7,499" ~ "0-100k",
        Q24 == "50,000-59,999" ~ "0-100k",
        Q24 == "60,000-69,999" ~ "0-100k",
        Q24 == "7,500-9,999" ~ "0-100k",
        Q24 == "70,000-79,999" ~ "0-100k",
        Q24 == "80,000-89,999" ~ "0-100k",
        Q24 == "90,000-99,999" ~ "0-100k",
        Q24 == ">$500,000" ~ "250k+"
    )) %>% 
    rename(Pay = Q24) %>% 
    rename(Gender = Q2) %>% 
    select(Year,Pay,Gender) %>% 
    drop_na(Pay) %>% 
    count(Pay) %>% 
    mutate(perc = 100*(n / sum(n)),
           label = str_c(round(perc),"%",sep = "")) %>% 
    mutate(Year = "2020") %>% 
    mutate(Gender = "Male")

pay2020f <- data2020 %>% 
    filter(Q2 == "Woman") %>% 
    mutate(Year = 2020) %>% 
    mutate(Q2 = str_replace(Q2, "Woman", "Female")) %>% 
    mutate(Q24 = case_when(
        Q24 == "$0-999" ~ "0-100k",
        Q24 == "1,000-1,999" ~ "0-100k",
        Q24 == "10,000-14,999" ~ "0-100k",
        Q24 == "100,000-124,999" ~ "100-250k",
        Q24 == "125,000-149,999" ~ "100-250k",
        Q24 == "15,000-19,999" ~ "0-100k",
        Q24 == "150,000-199,999" ~ "100-250k",
        Q24 == "2,000-2,999" ~ "0-100k",
        Q24 == "20,000-24,999" ~ "0-100k",
        Q24 == "200,000-249,999" ~ "100-250k",
        Q24 == "25,000-29,999" ~ "0-100k",
        Q24 == "250,000-299,999" ~ "250k+",
        Q24 == "3,000-3,999" ~ "0-100k",
        Q24 == "30,000-39,999" ~ "0-100k",
        Q24 == "300,000-500,000" ~ "250k+",
        Q24 == "4,000-4,999" ~ "0-100k",
        Q24 == "5,000-7,499" ~ "0-100k",
        Q24 == "50,000-59,999" ~ "0-100k",
        Q24 == "60,000-69,999" ~ "0-100k",
        Q24 == "7,500-9,999" ~ "0-100k",
        Q24 == "70,000-79,999" ~ "0-100k",
        Q24 == "80,000-89,999" ~ "0-100k",
        Q24 == "90,000-99,999" ~ "0-100k",
        Q24 == ">$500,000" ~ "250k+"
    )) %>% 
    rename(Pay = Q24) %>% 
    rename(Gender = Q2) %>% 
    select(Year,Pay,Gender) %>% 
    drop_na(Pay) %>% 
    count(Pay) %>% 
    mutate(perc = 100*(n / sum(n)),
           label = str_c(round(perc),"%",sep = "")) %>% 
    mutate(Year = "2020") %>% 
    mutate(Gender = "Female")

pay2019m <- data2019 %>% 
    filter(Q2 == "Male") %>% 
    mutate(Q10 = case_when(
        Q10 == "$0-999" ~ "0-100k",
        Q10 == "1,000-1,999" ~ "0-100k",
        Q10 == "10,000-14,999" ~ "0-100k",
        Q10 == "100,000-124,999" ~ "100-250k",
        Q10 == "125,000-149,999" ~ "100-250k",
        Q10 == "15,000-19,999" ~ "0-100k",
        Q10 == "150,000-199,999" ~ "100-250k",
        Q10 == "2,000-2,999" ~ "0-100k",
        Q10 == "20,000-24,999" ~ "0-100k",
        Q10 == "200,000-249,999" ~ "100-250k",
        Q10 == "25,000-29,999" ~ "0-100k",
        Q10 == "250,000-299,999" ~ "250k+",
        Q10 == "3,000-3,999" ~ "0-100k",
        Q10 == "30,000-39,999" ~ "0-100k",
        Q10 == "300,000-500,000" ~ "250k+",
        Q10 == "4,000-4,999" ~ "0-100k",
        Q10 == "5,000-7,499" ~ "0-100k",
        Q10 == "50,000-59,999" ~ "0-100k",
        Q10 == "60,000-69,999" ~ "0-100k",
        Q10 == "7,500-9,999" ~ "0-100k",
        Q10 == "70,000-79,999" ~ "0-100k",
        Q10 == "80,000-89,999" ~ "0-100k",
        Q10 == "90,000-99,999" ~ "0-100k",
        Q10 == ">$500,000" ~ "250k+"
    )) %>% 
    rename(Pay = Q10) %>% 
    rename(Gender = Q2) %>% 
    drop_na(Pay) %>% 
    count(Pay) %>% 
    mutate(perc = 100*(n / sum(n)),
           label = str_c(round(perc),"%",sep = "")) %>% 
    mutate(Year = "2019") %>% 
    mutate(Gender = "Male")

pay2019f <- data2019 %>% 
    filter(Q2 == "Female") %>% 
    mutate(Q10 = case_when(
        Q10 == "$0-999" ~ "0-100k",
        Q10 == "1,000-1,999" ~ "0-100k",
        Q10 == "10,000-14,999" ~ "0-100k",
        Q10 == "100,000-124,999" ~ "100-250k",
        Q10 == "125,000-149,999" ~ "100-250k",
        Q10 == "15,000-19,999" ~ "0-100k",
        Q10 == "150,000-199,999" ~ "100-250k",
        Q10 == "2,000-2,999" ~ "0-100k",
        Q10 == "20,000-24,999" ~ "0-100k",
        Q10 == "200,000-249,999" ~ "100-250k",
        Q10 == "25,000-29,999" ~ "0-100k",
        Q10 == "250,000-299,999" ~ "250k+",
        Q10 == "3,000-3,999" ~ "0-100k",
        Q10 == "30,000-39,999" ~ "0-100k",
        Q10 == "300,000-500,000" ~ "250k+",
        Q10 == "4,000-4,999" ~ "0-100k",
        Q10 == "5,000-7,499" ~ "0-100k",
        Q10 == "50,000-59,999" ~ "0-100k",
        Q10 == "60,000-69,999" ~ "0-100k",
        Q10 == "7,500-9,999" ~ "0-100k",
        Q10 == "70,000-79,999" ~ "0-100k",
        Q10 == "80,000-89,999" ~ "0-100k",
        Q10 == "90,000-99,999" ~ "0-100k",
        Q10 == ">$500,000" ~ "250k+"
    )) %>% 
    rename(Pay = Q10) %>% 
    rename(Gender = Q2) %>% 
    drop_na(Pay) %>% 
    count(Pay) %>% 
    mutate(perc = 100*(n / sum(n)),
           label = str_c(round(perc),"%",sep = "")) %>% 
    mutate(Year = "2019") %>% 
    mutate(Gender = "Female")

pay2018m <- data2018 %>% 
    filter(Q1 == "Male") %>% 
    mutate(Q9 = case_when(
        Q9 == "10-20,000" ~ "0-100k",
        Q9 == "0-10,000" ~ "0-100k",
        Q9 == "20-30,000" ~ "0-100k",
        Q9 == "125-150,000" ~ "100-250k",
        Q9 == "30-40,000" ~ "0-100k",
        Q9 == "50-60,000" ~ "0-100k",
        Q9 == "100-125,000" ~ "100-250k",
        Q9 == "90-100,000" ~ "0-100k",
        Q9 == "70-80,000" ~ "0-100k",
        Q9 == "80-90,000" ~ "0-100k",
        Q9 == "60-70,000" ~ "0-100k",
        Q9 == "400-500,000" ~ "250k+",
        Q9 == "40-50,000" ~ "0-100k",
        Q9 == "150-200,000" ~ "100-250k",
        Q9 == "500,000+" ~ "250k+",
        Q9 == "300-400,000" ~ "250k+",
        Q9 == "200-250,000" ~ "100-250k",
        Q9 == "250-300,000" ~ "250k+",
        Q9 == "I do not wish to disclose my approximate yearly compensation" ~ "0-100k"
    )) %>% 
    drop_na(Q9) %>% 
    rename(Pay = Q9) %>% 
    rename(Gender = Q1) %>% 
    count(Pay) %>% 
    mutate(perc = 100*(n / sum(n)),
           label = str_c(round(perc),"%",sep = "")) %>% 
    mutate(Year = "2018") %>% 
    mutate(Gender = "Male")

pay2018f <- data2018 %>% 
    filter(Q1 == "Female") %>% 
    mutate(Q9 = case_when(
        Q9 == "10-20,000" ~ "0-100k",
        Q9 == "0-10,000" ~ "0-100k",
        Q9 == "20-30,000" ~ "0-100k",
        Q9 == "125-150,000" ~ "100-250k",
        Q9 == "30-40,000" ~ "0-100k",
        Q9 == "50-60,000" ~ "0-100k",
        Q9 == "100-125,000" ~ "100-250k",
        Q9 == "90-100,000" ~ "0-100k",
        Q9 == "70-80,000" ~ "0-100k",
        Q9 == "80-90,000" ~ "0-100k",
        Q9 == "60-70,000" ~ "0-100k",
        Q9 == "400-500,000" ~ "250k+",
        Q9 == "40-50,000" ~ "0-100k",
        Q9 == "150-200,000" ~ "100-250k",
        Q9 == "500,000+" ~ "250k+",
        Q9 == "300-400,000" ~ "250k+",
        Q9 == "200-250,000" ~ "100-250k",
        Q9 == "250-300,000" ~ "250k+",
        Q9 == "I do not wish to disclose my approximate yearly compensation" ~ "0-100k"
    )) %>% 
    drop_na(Q9) %>% 
    rename(Pay = Q9) %>% 
    rename(Gender = Q1) %>% 
    count(Pay) %>% 
    mutate(perc = 100*(n / sum(n)),
           label = str_c(round(perc),"%",sep = "")) %>% 
    mutate(Year = "2018") %>% 
    mutate(Gender = "Female")

pay1 <- rbind(pay2020m,pay2019m)
paym <- rbind(pay1,pay2018m)
pay2 <- rbind(pay2020f,pay2019f)
payf <- rbind(pay2,pay2018f)


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
    
    # Application title
    dashboardHeader(title = "Data Science/ Machine Learning Trends 2020-2021", titleWidth = 550),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Trends", tabName = "charts", icon = icon("chart-line")),
            menuItem("Sources",tabName = "sources", icon = icon("closed-captioning"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "charts",
                box(title = "Job Title", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE, width = 4,
                    radioButtons(
                    inputId = "fill_var",
                    label = "Job Title",
                    choices = list(
                        "Data Engineer", 
                        "Software Engineer", 
                        "Data Scientist", 
                        "Data Analyst",  
                        "Research Scientist", 
                        "Other", 
                        "Statistician", 
                        "Project Manager", 
                        "ML Engineer", 
                        "Business Analyst", 
                        "DBA/Database Engineer"
                        ),
                    selected = "Data Scientist"
                    )),
                box(title = "Programming Languages", status = "info",
                    collapsible = TRUE, plotOutput("distPlot")),
            
                    box(title = "Visualization Tools", status = "info",
                        collapsible = TRUE,plotOutput("plot2")),
                    box(title = "ML Frameworks", status = "info",
                        collapsible = TRUE,plotOutput("plot3"))
            ),
            tabItem(tabName = "sources",
                    h5("https://www.kaggle.com/c/kaggle-survey-2020/data"),
                    h5("https://www.kaggle.com/c/kaggle-survey-2019/data"),
                    h5("https://www.kaggle.com/kaggle/kaggle-survey-2018"),
                    h5("https://www.kaggle.com/kaggle/kaggle-survey-2017")))
            ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        
        fill_hist1 <- switch(input$fill_var,
                            "Data Engineer" = data %>% filter(Q5 == "Data Engineer"), 
                            "Software Engineer" = data %>% filter(Q5 == "Software Engineer"), 
                            "Data Scientist" = data %>% filter(Q5 == "Data Scientist"), 
                            "Data Analyst" = data %>% filter(Q5 == "Data Analyst"),  
                            "Research Scientist" = data %>% filter(Q5 == "Research Scientist"), 
                            "Other" = data %>% filter(Q5 == "Other"), 
                            "Statistician" = data %>% filter(Q5 == "Statistician"), 
                            "Project Manager" = data %>% filter(Q5 == "Project Manager"), 
                            "ML Engineer" = data %>% filter(Q5 == "ML Engineer"), 
                            "Business Analyst" = data %>% filter(Q5 == "Business Analyst"), 
                            "DBA/Database Engineer" = data %>% filter(Q5 == "DBA/Database Engineer")
        )
        
        
        
        # building histogram
        ggplot(fill_hist1, aes(Q7)) +
            geom_bar(fill = "orange") +
            labs(
                x = NULL,
                y = NULL,
                title = "Programming Languages Used"
            ) +
            theme_fivethirtyeight() +
            scale_y_continuous(
                expand = c(0,0),
                limits = c(0,2800),
                breaks = seq(0,3500,500)
            ) +
            theme(
                plot.title = element_text(
                    face = "bold",
                    family = "Comic Sans MS",
                    size = 24,
                    hjust = 0.5,
                    vjust = 1.5),
                axis.text = element_text(family = "Comic Sans MS"),
                legend.title = element_text(family = "Comic Sans MS"),
                legend.text = element_text(family = "Comic Sans MS"),
                legend.position = c(0.95,0.85),
                panel.grid = element_blank()
            )
    })
    output$plot2 <- renderPlot({
        
        
        
        
        fill_hist2 <- switch(input$fill_var,
                             "Data Engineer" = data2 %>% filter(Q5 == "Data Engineer"), 
                             "Software Engineer" = data2 %>% filter(Q5 == "Software Engineer"), 
                             "Data Scientist" = data2 %>% filter(Q5 == "Data Scientist"), 
                             "Data Analyst" = data2 %>% filter(Q5 == "Data Analyst"),  
                             "Research Scientist" = data2 %>% filter(Q5 == "Research Scientist"), 
                             "Other" = data2 %>% filter(Q5 == "Other"), 
                             "Statistician" = data2 %>% filter(Q5 == "Statistician"), 
                             "Project Manager" = data2 %>% filter(Q5 == "Project Manager"), 
                             "ML Engineer" = data2 %>% filter(Q5 == "ML Engineer"), 
                             "Business Analyst" = data2 %>% filter(Q5 == "Business Analyst"), 
                             "DBA/Database Engineer" = data2 %>% filter(Q5 == "DBA/Database Engineer")
        )
        
        
        # building histogram
        ggplot(fill_hist2, aes(Q14)) +
            geom_bar(fill = "orange") +
            labs(
                x = NULL,
                y = NULL,
                title = "Visualization Tools Used"
            ) +
            theme_fivethirtyeight() +
            scale_y_continuous(
                expand = c(0,0),
                limits = c(0,2800),
                breaks = seq(0,3500,500)
            ) +
            theme(
                plot.title = element_text(
                    face = "bold",
                    family = "Comic Sans MS",
                    size = 24,
                    hjust = 0.5,
                    vjust = 1.5),
                axis.text = element_text(family = "Comic Sans MS"),
                axis.text.x = element_text(size = 8, angle = 20),
                legend.title = element_text(family = "Comic Sans MS"),
                legend.text = element_text(family = "Comic Sans MS"),
                legend.position = c(0.95,0.85),
                panel.grid = element_blank()
            )
    })
    output$plot3 <- renderPlot({
        
        
        
        
        fill_hist3 <- switch(input$fill_var,
                             "Data Engineer" = data3 %>% filter(Q5 == "Data Engineer"), 
                             "Software Engineer" = data3 %>% filter(Q5 == "Software Engineer"), 
                             "Data Scientist" = data3 %>% filter(Q5 == "Data Scientist"), 
                             "Data Analyst" = data3 %>% filter(Q5 == "Data Analyst"),  
                             "Research Scientist" = data3 %>% filter(Q5 == "Research Scientist"), 
                             "Other" = data3 %>% filter(Q5 == "Other"), 
                             "Statistician" = data3 %>% filter(Q5 == "Statistician"), 
                             "Project Manager" = data3 %>% filter(Q5 == "Project Manager"), 
                             "ML Engineer" = data3 %>% filter(Q5 == "ML Engineer"), 
                             "Business Analyst" = data3 %>% filter(Q5 == "Business Analyst"), 
                             "DBA/Database Engineer" = data3 %>% filter(Q5 == "DBA/Database Engineer")
        )
        
        
        # building histogram
        ggplot(fill_hist3, aes(Q16)) +
            geom_bar(fill = "orange") +
            labs(
                x = NULL,
                y = NULL,
                title = "Machine Learning Frameworks Used"
            ) +
            theme_fivethirtyeight() +
            scale_y_continuous(
                expand = c(0,0),
                limits = c(0,2800),
                breaks = seq(0,3500,500)
            ) +
            theme(
                plot.title = element_text(
                    face = "bold",
                    family = "Comic Sans MS",
                    size = 24,
                    hjust = 0.5,
                    vjust = 1.5),
                axis.text = element_text(family = "Comic Sans MS"),
                axis.text.x = element_text(size = 8, angle = 20),
                legend.title = element_text(family = "Comic Sans MS"),
                legend.text = element_text(family = "Comic Sans MS"),
                legend.position = c(0.95,0.85),
                panel.grid = element_blank()
            )
    })
    output$plot01 <- renderPlot({
        ggplot(gender, aes(Age)) +
            geom_bar(aes(fill = Gender)) +
            facet_wrap(~Year, ncol = 1) +
            labs(x = "Age",
                 y = "Participants",
                 title = "Gender Distribution in Data Science Community") +
            scale_fill_manual(values = c("orange","grey70"))+
            theme_fivethirtyeight()+
            theme(plot.title = element_text(
                face = "bold",
                family = "Comic Sans MS",
                size = 14,
                hjust = 0.5,
                vjust = 1.5),
                axis.title = element_text(family = "Comic Sans MS"),
                legend.title = element_text(family = "Comic Sans MS"),
                legend.text = element_text(family = "Comic Sans MS", size = 12),
                legend.position = "top",
                strip.text = element_text(family = "Comic Sans MS"),
                panel.grid.major = element_blank(),
                axis.ticks.y = element_line(),
                strip.background =  element_rect(fill = "grey90")
            )
    })
    output$plot02 <- renderPlot({
        ggplot(edc, aes(Year)) +
            geom_bar(fill = "orange") +
            labs(x = NULL,
                 y = "Participants",
                 title = "Education Level in Data Science Community") +
            facet_wrap(~Educ)+
            theme_fivethirtyeight()+
            theme(plot.title = element_text(
                face = "bold",
                family = "Comic Sans MS",
                size = 14,
                hjust = 0.5,
                vjust = 1.5),
                axis.title = element_text(family = "Comic Sans MS"),
                legend.title = element_blank(),
                legend.text = element_text(family = "Comic Sans MS", size = 12),
                legend.position = "top",
                strip.text = element_text(family = "Comic Sans MS"),
                panel.grid.major = element_blank(),
                panel.grid.major.y = element_line(),
                axis.ticks.y = element_line(),
                strip.background =  element_rect(fill = "grey"))
    })
    output$plot03 <- renderPlot({
        mplot <- ggplot(paym, aes(Year, perc, fill = Pay)) +
            geom_col(width = 0.99) + 
            scale_x_discrete(limits=c(" ", "2018", "2019", "2020"))+
            theme_minimal() +
            scale_fill_manual(
                name = "Yearly Compensation",
                values = c("orange", "grey60","grey40"),
                labels = c("$0-100k","$100-250k","$250k+")
            )+
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20),
                  legend.position = "None",
                  panel.grid = element_blank()) +
            annotate(geom = "text", label = "Male", x = 1, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "2018", x = 2, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "2019", x = 3, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "2020", x = 4, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "88.6%", x = 2, y = 80, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "82.7%", x = 3, y = 80, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "85.0%", x = 4, y = 80, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "1.1%", x = 2, y = 1, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "1.3%", x = 3, y = 1, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "1.1%", x = 4, y = 1, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "10.3%", x = 2, y = 10, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "16.0%", x = 3, y = 10, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "13.9%", x = 4, y = 10, size = 2,family = "Comic Sans MS") +
            coord_polar("y")
        
        fplot <- ggplot(payf, aes(Year, perc, fill = Pay)) +
            geom_col(width = 0.99) + 
            scale_x_discrete(limits=c(" ", "2018", "2019", "2020"))+
            theme_minimal() +
            scale_fill_manual(
                name = "Yearly Compensation",
                values = c("orange", "grey60","grey40"),
                labels = c("$0-100k","$100-250k","$250k+")
            )+
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20),
                  legend.position = c(0.88,1),
                  legend.title = element_text(family = "Comic Sans MS", size = 8),
                  legend.text = element_text(size = "8"),
                  strip.text = element_text(family = "Comic Sans MS"),
                  panel.grid = element_blank()) +
            annotate(geom = "text", label = "Female", x = 1, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "2018", x = 2, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "2019", x = 3, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "2020", x = 4, y = 50, size = 4,family = "Comic Sans MS") +
            annotate(geom = "text", label = "91.5%", x = 2, y = 80, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "86.6%", x = 3, y = 80, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "89.6%", x = 4, y = 80, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "0.6%", x = 2, y = 1, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "0.6%", x = 3, y = 1, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "0.6%", x = 4, y = 1, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "7.9%", x = 2, y = 10, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "12.9%", x = 3, y = 10, size = 2,family = "Comic Sans MS") +
            annotate(geom = "text", label = "9.8%", x = 4, y = 10, size = 2,family = "Comic Sans MS") +
            coord_polar("y")
        
        pay_plot <- (mplot + fplot)
        pay_plot + plot_annotation(
            title = "Data Science Industry Salary (2018-2020)",
            caption = "Male vs. Female",  
            theme = theme(plot.title = element_text(
                face = "bold",
                family = "Comic Sans MS",
                size = 14), 
                plot.caption = element_text(family = "Comic Sans MS",
                                            size = 10))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
