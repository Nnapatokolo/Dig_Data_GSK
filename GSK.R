library(tidyverse)
study_df <- read_csv("clinical-study.csv")

head(study_df, 7)

view(study_df)

protein_df <- read_csv("protein-levels.csv")

view(protein_df)

summary(study_df)


# Removing duplicate roles from the dataframe

study_df_no_duplicate <- study_df[!duplicated(study_df),]

view(study_df_no_duplicate)


# Remove age less than 18 (Paediatric Age)

df_filtered <- study_df_no_duplicate[study_df_no_duplicate$age > 18,] # keeps data that age is above 18

View(df_filtered)

# Removing columns with NA from the dataframe 

df_filtered2 <- df_filtered[complete.cases(df_filtered), ] 

view(df_filtered2)

# Removing columns with NA from the dataframe for Protein Level

protein_df1 <- protein_df[complete.cases(protein_df), ]

view(protein_df1)

# Add a new column BMI and insect BMI values

df_filtered2$BMI <- df_filtered2$weight / (df_filtered2$height^2)

view(df_filtered2)

# Renaming the participant_id in "protein-level" data frame to "subject_id"

names(protein_df1)[1] <- "subject_id"

view(protein_df1)


# Merge data frames from df_filtered2 and protein_df

merged_df <- merge(df_filtered2, protein_df1, by = "subject_id")

view(merged_df)


# Aggregate the Mean age in two treatment groups (Drug and Control)
 
mean_age_trt_group <- aggregate(age ~ trt_grp, data = merged_df, FUN = mean)

View(mean_age_trt_group)

# Aggregate the Mean age in responders and Non- responders (N and Y)

mean_age_RESPONDERS <- aggregate(age ~ RESPONSE, data = merged_df, FUN = mean)

view(mean_age_RESPONDERS)

# Aggregate Resonders and Non-responders in two treatment arms
  #subset the data frame for the "DRUG" treatment Group
subset_drug <- merged_df[merged_df$trt_grp == "DRUG",]

view(subset_drug)
 
  #subset the data frame for the "CONTROL" treatment group
 
subset_control <- merged_df[merged_df$trt_grp == "CONTROL",]

view(subset_control)


mean_subset_control <- aggregate(trt_grp ~ RESPONSE, data = subset_control, FUN = count)

  #subset the data frame for "RESPONSE" in the "DRUG" treatment group

subset_drug_responders <- subset_drug[subset_drug$RESPONSE == "Y", ]

  #subset the data frame for "NON-RESPONSE" in the "DRUG" treatment group

subset_drug_non_responders <- subset_drug[subset_drug$RESPONSE == "N", ]

  #subset the data frame for "RESPONSE" in the "CONTROL" treatment group

subset_control_responders <- subset_control[subset_control$RESPONSE == "Y", ]

  #subset the data frame for "NON-RESPONSE" in the "CONTROL" treatment group

subset_control_non_responders <- subset_control[subset_control$RESPONSE == "N", ]


# Aggregate the Mean weight in Responders

mean_weight_responders <- aggregate(weight ~ RESPONSE, data = merged_df, FUN = mean)

View(mean_weight_responders)

# Average Protein concentration in responders and Non Responders 

protein_contration_response <- aggregate(protein_concentration ~ RESPONSE, data = merged_df, FUN = mean)

view(protein_contration_response)

# Creating a Boxplot seperating by treatment Group using GGplot 

ggplot(data = merged_df, aes(y = age, x =  RESPONSE, fill = trt_grp)) + 
  geom_boxplot()+
  xlab("RESPONSE") +
  ylab("AGE") + 
  ggtitle("Age by Response and Treatment Group")

# Creating a Boxplot for BMI and response by Treatment Group using GGplot 

ggplot(data = merged_df, aes(y = BMI, x =  RESPONSE, fill = trt_grp)) + 
  geom_boxplot()+
  xlab("RESPONSE") +
  ylab("BMI") + 
  ggtitle("BMI by Response and Treatment Group")

# Creating a Boxplot for Protein concentration and Response by Treatment Group

ggplot(data = merged_df, aes(y = protein_concentration, x =  RESPONSE, fill = trt_grp)) + 
  geom_boxplot()+
  xlab("RESPONSE") +
  ylab("PROTEIN CONCENTRATION") + 
  ggtitle("Protein Concentration by Response and Treatment Group")

