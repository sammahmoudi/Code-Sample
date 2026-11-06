################################
#Author: 	Sam Mahmoudi 
#Purpose: Sample R Code to calculate point estimates for statistical Table R-1 for the 2023 Survey of doctorate recipients (SDR)
#
# Preamble:
#
# Official survey results are housed at:
# https://ncses.nsf.gov/surveys/doctorate-recipients/2023#data
#
# Specifically, Table R-1 can be accessed at:
# https://ncses.nsf.gov/pubs/nsf25321/table/R-1
# 
# Official results are based on Restricted-use-files (RUF) which require licensing. 
#
# For this sample code, Pubic-use-file (PUF) was used to generate a portion of the table that would not depend on variables that were collapsed(recoded) in the PUF for disclosure avoidance. 
#
#The data were downloaded from:
#https://ncses.nsf.gov/explore-data/microdata/doctorate-recipients
#################################

#loading required packages
library(survey)
library(dplyr)

################################
#Step 1: 	Downloading data, Unzipping epsd23 dataset(public-use-file) and loading it.
#
#
#Consulted documentation at:
# https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/download.file

# initializing a temporary file object to store the downloaded file
temp <- tempfile()

#downloading the ZIP file and storing it in temp
download.file(url = "https://ncses.nsf.gov/824/assets/0/files/doctorate_recipients_2023.zip", 
              destfile = temp, 
              mode = "wb")

#extracting the relevant CSV file and loading the CSV dataset.
epsd23 <- read.csv(unz(temp,"psd23Public/epsd23.csv"))

################################
#Step 2: Inspecting the dataset 
#Checking to make sure that the dataset contains the expected number of records and variables*/
#The official accompanying codebook is used as reference for this check. (codebook is included in the ZIP file)
head(epsd23)  #ok
summary(epsd23) #ok

#frequency review of a key variable (FNINUS here) to ensure that it is in alignment with the codebook.
table(epsd23$FNINUS)  #ok

#This confirms that the dataset was read and loaded as expected, as far as the values of key variables including weight variable (WTSURVY) are concerned.*/
weight_check <- epsd23 %>% summarise(n = n(), sum = sum(WTSURVY), min = min(WTSURVY), max = max(WTSURVY)) #ok

################################
#Step 3: 	Preparing the dataset for the analysis.
#A series of row and column variables, corresponding with the published table layout, are created. 
#For instance, row1 represents "U.S. residing doctorate recipient" of the table and col1 represents "Total" column of the table. 
#For data records contributing to each cell, the corresponding row# and col# variables are set to 1. Otherwise they are set to 0.
#
epsd23_1 <- epsd23 %>% 
            mutate(

              # Rows
              # U.S. residing doctorate recipients
              row1 = if_else(FNINUS == "Y", 1, 0),

              # Male
              row3 = case_when(
                                FNINUS == "Y" & SEX == "M" ~ 1,
                                FNINUS == "Y" & SEX == "F" ~ 0,
                                TRUE ~ 0
                                ),

              # Female
              row4 = case_when(
                                FNINUS == "Y" & SEX == "F" ~ 1,
                                FNINUS == "Y" & SEX == "M" ~ 0,
                                TRUE ~ 0
                                ),

              # Columns
              col1 = 1,

              # Employed, Never retired
              col3 = case_when(
                                LFSTAT == "1" & !(PJRET == "Y" | FTPRET == "Y") ~ 1,
                                LFSTAT == "1" & (PJRET == "Y" | FTPRET == "Y")  ~ 0,
                                TRUE ~ 0
                                ),

              # Employed, Previously retired
              col5 = case_when(
                                LFSTAT == "1" & (PJRET == "Y" | FTPRET == "Y")  ~ 1,
                                LFSTAT == "1" & !(PJRET == "Y" | FTPRET == "Y") ~ 0,
                                TRUE ~ 0
                                ),

              #Note:  Non-employed columns depend on NWRET variable which has been collapsed/recoded into NWOTP in PUF for disclosure avoidance 
              #       and hence there is not enough detail in the PUF to produce the 
              #       Non-employed columns. The same goes for other rows such Race/Ethnicity, AGE, etc.
              )

################################
# Step 4: 	Calculating table statistics

# For variance estimation in officially published tables, replicate weights are utilized along with parameters established by 
# the NSF, but since replicate weight variables are not included in the PUF
# I am going with the Taylor Series linearization method, which would lead to slight discrepancy in standard error estimation (inflated estimates)
# compared to the published table.

# Define survey design
svy_design <- svydesign(
                        ids = ~1,           #no detail on sampling design is included in the PUF.
                        weights = ~WTSURVY, #weight variable
                        data = epsd23_1,
                        na.action = "fail"  # Making sure weight variables does not include missing values (just a safeguard)
                        )

#calculating statistics
row1_col1 <- svytotal(~row1, svy_design) %>% 
  #converting output to data frame
  data.frame()  %>% 
  #renaming raw variables
  rename(col1_se = row1, col1 = total) %>% 
  #creating a row variable for a final merge step
  mutate(row = 1)
                                        
row3_col1 <- svytotal(~row3, svy_design) %>% 
  #converting output to data frame
  data.frame()  %>% 
  #renaming raw variables
  rename(col1_se = row3, col1 = total) %>% 
  #creating a row variable for a final merge step
  mutate(row = 3)

row4_col1 <- svytotal(~row4, svy_design) %>% 
  #converting output to data frame
  data.frame()  %>% 
  #renaming raw variables
  rename(col1_se = row4, col1 = total) %>% 
  #creating a row variable for a final merge step
  mutate(row = 4)

#stacking the results
col1 <- rbind(row1_col1, row3_col1, row4_col1)

row1_col3 <- svyby(~col3, by = ~row1, design = svy_design, FUN = svymean) %>% 
  #converting output to data frame
  data.frame() %>% 
  #filtering to retain values of interest
  filter(row1 == 1) %>% 
  #renaming raw variables
  rename(col3_se = se) %>% 
  #creating a row variable for a final merge step
  mutate(row = 1) %>% 
  #retaining variables of interest
  select(-row1)


row3_col3 <- svyby(~col3, by = ~row3, design = svy_design, FUN = svymean)  %>% 
  #converting output to data frame
  data.frame() %>% 
  #filtering to retain values of interest
  filter(row3 == 1) %>% 
  #renaming raw variables
  rename(col3_se = se) %>% 
  #creating a row variable for a final merge step
  mutate(row = 3) %>% 
  #retaining variables of interest
  select(-row3)

row4_col3 <- svyby(~col3, by = ~row4, design = svy_design, FUN = svymean) %>% 
  #converting output to data frame
  data.frame() %>% 
  #filtering to retain values of interest
  filter(row4 == 1) %>% 
  #renaming raw variables
  rename(col3_se = se) %>% 
  #creating a row variable for a final merge step
  mutate(row = 4) %>% 
  #retaining variables of interest
  select(-row4)

#stacking the results
col3 <- rbind(row1_col3, row3_col3, row4_col3)

row1_col5 <- svyby(~col5, by = ~row1, design = svy_design, FUN = svymean) %>% 
  #converting output to data frame
  data.frame() %>% 
  #filtering to retain values of interest
  filter(row1 == 1) %>% 
  #renaming raw variables
  rename(col5_se = se) %>% 
  #creating a row variable for a final merge step
  mutate(row = 1) %>% 
  #retaining variables of interest
  select(-row1)


row3_col5 <- svyby(~col5, by = ~row3, design = svy_design, FUN = svymean) %>% 
  #converting output to data frame
  data.frame() %>% 
  #filtering to retain values of interest
  filter(row3 == 1) %>% 
  #renaming raw variables
  rename(col5_se = se) %>% 
  #creating a row variable for a final merge step
  mutate(row = 3) %>% 
  #retaining variables of interest
  select(-row3)

row4_col5 <- svyby(~col5, by = ~row4, design = svy_design, FUN = svymean) %>% 
  #converting output to data frame
  data.frame() %>% 
  #filtering to retain values of interest
  filter(row4 == 1) %>% 
  #renaming raw variables
  rename(col5_se = se) %>% 
  #creating a row variable for a final merge step
  mutate(row = 4) %>% 
  #retaining variables of interest
  select(-row4)

#stacking the results
col5 <- rbind(row1_col5, row3_col5, row4_col5)

#creating a row variable to be used as the basis for the table.
binder <- data.frame(row = 1:4)

all_stats <- binder %>% left_join(col1, by = "row") %>% 
  left_join(col3, by = "row") %>% 
  left_join(col5, by = "row") 
  

################################
#Step 5: 	Pulling raw statistics, formatting them (rounding here) and rearranging them 
#to conform to the final table layout.

all_stats_rounded <- all_stats %>% 
  mutate(
          #Roundings are according to NCSES rounding specs included in table footnote
          col1 = round(col1 / 50) * 50,
          col1_se = ceiling(col1_se / 25) * 25,

          col3 = round(col3 * 100 / 0.1) * 0.1,
          col3_se = ceiling(col3_se * 100 / 0.05) * 0.05,

          col5 = round(col5 * 100 / 0.1) * 0.1,
          col5_se = ceiling(col5_se * 100 / 0.05) * 0.05     
          
          #Typically suppression of disclosive cells would be handled as well, but since
          #the PUF has already gone through disclosure review this step is not included in this sample code.
          )


