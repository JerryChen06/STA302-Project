#' Code for reading the csv file, defining the classes and description for each variable, and cleaning data. 
#' <cleaned_data> Dataframe containing all observations for all variables. Skipped values set to NA. Categorical variables are factored. 
#' <dict> : Named vector in the form "Predictor Code" = "Predictor Description". Modified for readability, but full description name is present as comment. 
#' <classes> Named vector in the form "Predictor Code" = "Class + Skip Annotations"
#' <categorical_codes> List of named vectors, where each element corresponds to a predictor code, and the named vector maps a value to its meaning
#' <general_classes> Named vector in the form "Predictor Code" = "Class"
#' <non_NA> Vector of the codes of all meaningful predictors
#' <numeric_classes> Vector of the codes of all ord. cat. and numeric variables
#' <factored_classes> Vector of the codes of all categorical variables that are factored in the data (includes all Y/N variables)

library(tidyverse)

original_data <- read_csv(
    "/Users/dazedinthecity/Desktop/STA302/CCHS-PUMF-2022/Data_Données/pumf_cchs.csv"
)

dict <- c(
  "VERDATE" = "Date of file creation", # page 3
  "REFPER" = "Reference period", # page 3
  "ADM_RNO" = "Sequential record number", # page 3
  "COLMODE" = "Collection mode - 18+", # page 3
  "COLMODEY" = "Collection mode 12-17 (youth respondents)", # page 3
  "COLMODEP" = "Collection mode 12-17 (PMK respondents)", # page 3
  "GEOGPRV" = "Province or territory of residence of respondent - (G)", # page 3
  "GEODGHR4" = "Health region - (D, G)", # page 3
  "DHHDGHSZ" = "Household size - Grouped", # page 3
  "EDDVH3" = "Highest level of education in household", # page 3
  "DHHGAGE" = "Age - (G)", # page 3
  "DHH_SEX" = "Sex at birth", # page 3
  "DOMAC" = "Main activity (1) Inclusion Flag - (F)", # page 3
  "MAC_05" = "Main activity worked at job/business 12 mo", # page 3
  "DOGEN" = "General health - Inclusion Flag - (F)", # page 3
  "GEN_01" = "Perceived health", # page 3
  "GEN_05" = "Perceived mental health", # page 3
  "GEN_10" = "Perceived life stress", # page 3
  "GEN_15" = "Perceived stress at work", # page 3
  "GEN_20" = "Sense of belonging to local community", # page 3
  "DOLSM" = "Life satisfaction measures - Inclusion Flag - (F)", # page 3
  "LSM_01" = "Satisfaction with life in general", # page 3
  "LSMDVSWL" = "Satisfaction with life in general - (D)", # page 3
  "DOHWT" = "Height and weight - Inclusion Flag - (F)", # page 3
  "HWTDGISW" = "BMI classification for adults (self-reported) - intl standard (D, G)", # page 3
  "HWTDGWHO" = "BMI age 12 to 17 (self-reported) - WHO classification - (D, G)", # page 3
  "HWTDGBCC" = "BMI classification", #  for adults (adjusted) - intl standard - (D, G). Page 3
  "DOWTP" = "Weight perception - Inclusion Flag - (F)", # page 3
  "WTP_50" = "Self-perceived weight - overweight / underweight / just about right", # page 3
  "DOCOV2" = "COVID19 -(2) Inclusion Flag - (F)", # page 3
  "COV2_005" = "Self-perceived mental health before / during pandemic", # page 3
  "COV2_10A" = "COVID-19 precautions - washes hands more frequently", # page 3
  "COV2_10B" = "COVID-19 precautions - wears a mask in public places", # page 3
  "COV2_10C" = "COVID-19 precautions - keeps a 2 m / 6 ft distance from others", # page 3
  "COV2_10D" = "COVID-19 precautions - avoids crowds / large gatherings", # page 3
  "COV2_10E" = "COVID-19 precautions - avoids leaving house for non-essential reasons", # page 3
  "COV2_10F" = "COVID-19 precautions - works from home", # page 3
  "COV2_10G" = "COVID-19 precautions - uses delivery / curb side pick-up services", # page 3
  "COV2_10H" = "COVID-19 precautions - self-isolates if symptoms", # page 3
  "COV2_10I" = "COVID-19 precautions other precaution", # page 3
  "COV2_10J" = "COVID-19 precautions no precautions taken", # page 3
  "COV2_038" = "Received COVID-19 vaccine", # page 3
  "COV2_030" = "COVID-19 vaccine requires 1 or 2 doses", # page 3
  "COV2_035" = "COVID-19 vaccine number of doses received so far", # page 3
  "COV2_041" = "COVID-19 vaccine main reason why not vaccinated", # page 3
  "COV2_45A" = "COVID-19 vaccine - reasons don't want not at high risk", # page 3
  "COV2_45B" = "COVID-19 vaccine reasons don't want won't be very sick", # page 3
  "COV2_45C" = "COVID-19 vaccine - reasons don't want - pandemic severity overstated", # page 3
  "COV2_45D" = "COVID-19 vaccine reasons don't want - vaccines are not effective", # page 3
  "COV2_45E" = "COVID-19 vaccine reasons don't want vaccines are not safe", # page 3
  "COV2_45F" = "COVID-19 vaccine reasons don't want don't trust its effectiveness", # page 3
  "COV2_45G" = "COVID-19 vaccine - reasons don't want - don't trust its safety", # page 3
  "COV2_45H" = "COVID-19 vaccine reasons don't want - philosophical / religious", # page 3
  "COV2_45I" = "COVID-19 vaccine reasons don't want other reason", # page 3
  "COV2_040" = "COVID-19 vaccine - likeliness of getting it", # page 3
  "DOCOV3" = "COVID-19 (3) Inclusion Flag - (F)", # page 3
  "COV3_005" = "COVID-19 vaccine - likeliness of getting additional dose if offered", # page 3
  "DOCCC" = "Chronic conditions - Inclusion Flag - (F)", # page 3
  "CCC_05" = "Has diabetes", # page 4
  "CCC_80" = "Has high blood pressure", # page 4
  "CCC_85" = "High blood pressure - took medication - 1 mo", # page 4
  "CCC_90" = "Has had high blood cholesterol - lifetime", # page 4
  "CCC_135" = "Has a mood disorder (depression, bipolar, mania, dysthymia)", # page 4
  "CCCDGSKL" = "Musculoskeletal condition - (G)", # page 4
  "CCCDGCAR" = "Cardiovascular condition (Heart disease or stroke) - (G)", # page 4
  "DOCC1" = "Chronic conditions (1) Inclusion Flag - (F)", # page 4
  "CC1_140" = "Has an anxiety disorder (phobia, panic, generalized anxiety)", # page 4
  "CC1_145" = "Has post-traumatic stress disorder (PTSD)", # page 4
  "CC1_155" = "Has chronic fatigue syndrome", # page 4
  "CC1_170" = "Has multiple chemical sensitivities (MCS)", # page 4
  "DOWDM" = "Abilities Inclusion Flag - (F)", # page 4
  "WDMDGDIF" = "Difficulty Level of difficulty across domains - (G)", # page 4
  "DOOHM" = "Oral health - Inclusion Flag - (F)", # page 4
  "OHM_05" = "Perceived oral health", # page 4
  "OHM_15" = "Persistent/on-going mouth pain frequency 12 mo", # page 4
  "DOOHM3" = "Oral Health 3 Inicusion Flag - (F)", # page 4
  "OHM3_05" = "Visit dental professional - frequency", # page 4
  "OHM3_10" = "Visited dental professional - last time", # page 4
  "OHM3_15" = "Avoided going to a dental professional due to cost - 12 mo", # page 4
  "OHM3_18" = "Avoided having recommended dental treatment due to cost - 12 mo", # page 4
  "OHM3_20A" = "Type dental treatment avoided due to cost check-ups/cleanings", # page 4
  "OHM3_20B" = "Type dental treatment avoided due to cost - fillings", # page 4
  "OHM3_20C" = "Type dental treatment avoided due to cost root canal treatment", # page 4
  "OHM3_20D" = "Type dental treatment avoided due to cost tooth extraction", # page 4
  "OHM3_20E" = "Type dental treatment avoided due to cost tooth replacement", # page 4
  "OHM3_20F" = "Type dental treatment avoided due to cost no response provided", # page 4
  "OHM3_45" = "Insurance - dental expenses all/part cost coverage", # page 4
  "OHM3G50A" = "Type of dental insurance - private plan (G)", # page 4
  "OHM3_50C" = "Type of insurance dental - government paid plan", # page 4
  "OHM3_50E" = "Type of insurance dental - don't know", # page 4
  "DOEAH" = "Eating habits - Inclusion Flag (F)", # page 4
  "EAHG05" = "Ate food from a restaurant number of times 30 d - (G)", # page 4
  "EAHDVCHO" = "Daily consumption - chocolate milk - (D)", # page 4
  "EAHDVFFD" = "Daily consumption - fruit flavoured drinks or sports drinks - (D)", # page 4
  "EAHDVFRU" = "Daily consumption - fruit - (D)", # page 4
  "EAHDVGRN" = "Daily consumption - dark green vegetables - (D)", # page 4
  "EAHDVJUI" = "Daily consumption - pure fruit juice - (D)", # page 4
  "EAHDVORA" = "Daily consumption orange-coloured vegetables - (D)", # page 4
  "EAHDVSCD" = "Daily consumption - sweetened coffee drinks or iced tea - (D)", # page 4
  "EAHDVSOD" = "Daily consumption - regular soft drinks - (D)", # page 4
  "EAHDVSTA" = "Daily consumption starchy vegetables - (D)", # page 4
  "EAHDVVEG" = "Daily consumption other vegetables - (D)", # page 4
  "EAHDVEND" = "Daily consumption energy drinks - (D)", # page 4
  "EAHDVBVT" = "Daily consumption - beverages - (D)", # page 4
  "EAHDVBVG" = "Grouping of total daily consumption - beverages - (D)", # page 4
  "EAHDVFVT" = "Daily consumption - fruits and vegetables (D)", # page 4
  "EAHDVFVG" = "Grouping of total daily consumption - fruits and vegetables - (D)", # page 4
  "DOPAA" = "Physical activities adults Inclusion Flag (F)", # page 4
  "PAA_05" = "Active transportation - 7 d", # page 4
  "PAA_10A" = "Active transportation - Monday", # page 4
  "PAA_10B" = "Active transportation - Tuesday", # page 4
  "PAA_10C" = "Active transportation - Wednesday", # page 4
  "PAA_10D" = "Active transportation - Thursday", # page 4
  "PAA_10E" = "Active transportation - Friday", # page 4
  "PAA_10F" = "Active transportation - Saturday", # page 4
  "PAA_10G" = "Active transportation - Sunday", # page 4
  "PAA_15A" = "Hours spent on active transportation per week", # page 4
  "PAA_15B" = "Active transportation - 7d total - minutes", # page 4
  "PAA_30" = "Sports/fitness / recreational physical act 7 d", # page 4
  "PAA_35" = "Sports/fitness / recreational physical act sweat", # page 4
  "PAA_40A" = "Sports/fitness / recreational physical act - Monday", # page 4
  "PAA_40B" = "Sports/fitness/ recreational physical act - Tuesday", # page 5
  "PAA_40C" = "Sports/fitness / recreational physical act - Wednesday", # page 5
  "PAA_40D" = "Sports/fitness / recreational physical act - Thursday", # page 5
  "PAA_40E" = "Sports/fitness / recreational physical act - Friday", # page 5
  "PAA_40F" = "Sports/fitness / recreational physical act - Saturday", # page 5
  "PAA_40G" = "Sports/fitness / recreational physical act - Sunday", # page 5
  "PAA_45A" = "Sports/fitness / recreational physical act 7 d - total - hours", # page 5
  "PAA_45B" = "Sports/fitness/ recreational physical act 7 dtotal minutes", # page 5
  "PAA_60" = "Other physical activities - 7 d", # page 5
  "PAA_65" = "Other physical activities - sweat", # page 5
  "PAA_70A" = "Other physical activities - Monday", # page 5
  "PAA_70B" = "Other physical activities Tuesday", # page 5
  "PAA_70C" = "Other physical activities - Wednesday", # page 5
  "PAA_70D" = "Other physical activities - Thursday", # page 5
  "PAA_70E" = "Other physical activities - Friday", # page 5
  "PAA_70F" = "Other physical activities - Saturday", # page 5
  "PAA_70G" = "Other physical activities - Sunday", # page 5
  "PAA_75A" = "Other physical activities 7d - total - hours", # page 5
  "PAA_75B" = "Other physical activities 7 dtotal - minutes", # page 5
  "PAA_95" = "Physical activities - vigorous intensity", # page 5
  "PAA_100A" = "Physical activities - vigorous intensity 7 dtotal - hours", # page 5
  "PAA_100B" = "Physical activities - vigorous intensity - 7 d- total - minutes", # page 5
  "PAADVMVA" = "Total minutes moderate to vigorous physical activities - 7 d- (D)", # page 5
  "PAADVACV" = "Physical activity indicator - (D)", # page 5
  "PAADVAC2" = "Alternate physical activity indicator - (D)", # page 5
  "PAADVVIG" = "Total minutes - vigorous physical activities - 7 d - (D)", # page 5
  "PAADVVOL" = "Volume of weekly activity", # page 5 - 7d (METs*mins/week) - (D)
  "PAADVWHO" = "Physically active, based on WHO guidelines - (D)", # page 5
  "DOUPE" = "Use of protective equipment - Inclusion Flag (F)", # page 5
  "UPE_05" = "Bicycling 12 mo", # page 5
  "UPE_10" = "Bicycling - helmet - frequency", # page 5
  "UPE_15" = "Rollerblading 12 mo", # page 5
  "UPE_20" = "Rollerblading - helmet frequency", # page 5
  "UPE_25" = "Rollerblading - wrist protectors - frequency", # page 5
  "UPE_30" = "Rollerblading elbow pads frequency", # page 5
  "UPE_35" = "Rollerblading - knee pads frequency", # page 5
  "UPE_40A" = "Downhill skiing - 12 mo.", # page 5
  "UPE_45" = "Downhill skiing helmet - frequency", # page 5
  "UPE_40B" = "Snowboarding - 12 mo", # page 5
  "UPE_50" = "Snowboarding - helmet - frequency", # page 5
  "UPE_55" = "Snowboarding wrist protectors - frequency", # page 5
  "UPE_60A" = "Skateboarding - 12 mo", # page 5
  "UPE_65" = "Skateboarding helmet frequency", # page 5
  "UPE_70" = "Skateboarding - wrist protectors - frequency", # page 5
  "UPE_75" = "Skateboarding elbow pads frequency", # page 5
  "UPE_80" = "Ice hockey 12 mo", # page 5
  "UPE_85" = "Ice hockey mouth guard - frequency", # page 5
  "DOSBE" = "Sedentary behaviours - Inclusion Flag - (F)", # page 5
  "SBE_005" = "Free time sitting/lying watching screen school/work day - 7d", # page 5
  "SBE_010" = "Free time sitting/lying watching screen not school/work day7d", # page 5
  "DOSLP" = "Sleep Inclusion Flag - (F)", # page 5
  "SLPG005" = "Number of hours spent sleeping per night - (G)", # page 5
  "SLP_010" = "Trouble going to sleep or staying asleep frequency", # page 5
  "SLP_015" = "Refreshing sleep - frequency", # page 5
  "SLP_020" = "Difficulty staying awake frequency", # page 5
  "DOCSS" = "Current smoking status Inclusion Flag - (F)", # page 5
  "CSS_05" = "Smoked a whole cigarette lifetime", # page 5
  "CSS_15" = "Smoked more than 100 cigarettes - lifetime", # page 5
  "CSSG20" = "Smoked - Frequency 30 days - (G)", # page 5
  "CSS_25" = "Num of cigarettes smoked daily (daily smoker)", # page 5
  "CSS_30" = "Num of cigarettes smoked daily (occasional smoker)", # page 5
  "CSS_40" = "Made serious attempt to stop smoking 12 mo", # page 5
  "DOSPU" = "Smoking past use Inclusion Flag - (F)", # page 5
  "SPU_05" = "Smoked daily lifetime (occasional / former smoker)", # page 6
  "SPU_20" = "Num of cigarettes smoked daily (former daily smoker)", # page 6
  "SMKDVSTY" = "Smoking status (type 2) - traditional definition (D)", # page 6
  "SMKDGYCS" = "Num. of years smoked daily (current daily smokers only) - (G)", # page 6
  "SMKDGSTP" = "Number of years since stopping smoking completely - (G)", # page 6
  "DOECV" = "Electronic cigarettes and vaping Inclusion Flag - (F)", # page 6
  "ECV_05" = "Tried e-cigarette / vaping device - life", # page 6
  "ECVG15" = "E-Cigarette - vaping device use - (G)", # page 6
  "ECV_20" = "Used e-cigarette llast time nicotine", # page 6
  "ECVDVSMK" = "E-Cigarette / vaping device and cigarette use (D)", # page 6
  "DOECV2" = "Electronic cigarette and vaping 2- Inicusion Flag - (F)", # page 6
  "ECV2G05A" = "Vape E-liquid with nicotine (G)", # page 6
  "ECV2G05B" = "Vape E-liquid without nicotine - (G)", # page 6
  "ECV2G05C" = "Vape - E-liquid with unknown content - (G)", # page 6
  "ECV2_10" = "Tried to quit smoking with e-cigarette / vaping device - 12 mo", # page 6
  "ECV2_15" = "Tried first - e-cigarette / vaping device or cigarette", # page 6
  "DOALC" = "Alcohol use Inclusion Flag - (F)", # page 6
  "ALC_05" = "Had a drink - lifetime", # page 6
  "ALC_10" = "Drank alcohol - 12 mo", # page 6
  "ALC_15" = "Drank alcohol - frequency 12 mo", # page 6
  "ALC_20" = "Drank 5+/4+ drinks one occasion - frequency - 12 mo", # page 6
  "ALCDVTTM" = "Type of drinker 12 months (D)", # page 6
  "DOCAN" = "Cannabis use Inclusion Flag - (F)", # page 6
  "CAN_05C" = "Used cannabis - 12 mo", # page 6
  "CAN_35A" = "Used cannabis medical purpose pain 12 mo", # page 6
  "CAN_35B" = "Used cannabis medical purpose nausea vomiting - 12 mo", # page 6
  "CAN_35C" = "Used cannabis - medical purpose- lack of appetite - 12 mo", # page 6
  "CAN_35D" = "Used cannabis - medical purpose- headaches / migraines - 12 mo", # page 6
  "CAN_35E" = "Used cannabis medical purpose muscle spasms / seizures - 12 mo", # page 6
  "CAN_35F" = "Used cannabis medical purpose- anxiety/depression 12 mo", # page 6
  "CAN_35G" = "Used cannabis - medical purpose- symptoms of PTSD - 12 mo", # page 6
  "CAN_35H" = "Used cannabis medical purpose- problems sleeping - 12 mo", # page 6
  "CAN_35I" = "Used cannabis - purpose- opioid withdrawal symptoms - 12 mo", # page 6
  "CAN_35J" = "Used cannabis medical purpose other 12 mo", # page 6
  "CAN_40" = "Used cannabis - has a medical document for medical purposes use", # page 6
  "DOFLU" = "Flu shots Inclusion Flag (F)", # page 6
  "FLU_05" = "Had seasonal flu vaccine - 12 mo", # page 6
  "FLU_10" = "Last seasonal flu vaccine - month", # page 6
  "FLU_15" = "Last seasonal flu vaccine current / last year", # page 6
  "FLU_20A" = "No flu vaccine 12 mo didn't think necessary", # page 6
  "FLU_20B" = "No flu vaccine 12 mo concern about side effects", # page 6
  "FLU_20C" = "No flu vaccine 12 moflu is not that severe", # page 6
  "FLU_20D" = "No flu vaccine 12 moflu vaccine doesn't work", # page 6
  "FLU_20E" = "No flu vaccine 12 mo previous bad reaction", # page 6
  "FLU_20F" = "No flu vaccine 12 mo- other", # page 6
  "DORHC" = "Regular health care provider - Inclusion Flag (F)", # page 6
  "RHC_05" = "Regular health care provider", # page 6
  "DOLMAM" = "Labour market activities - Inclusion Flag - (F)", # page 6
  "LBFDGHPW" = "Total usual hours worked per week", # page 6
  "LBFDVPFT" = "Full-time/part-time working status (for total usual hours) - (D)", # page 6
  "LBFDGWSS" = "Working status last week (G)", # page 6
  "SDCDGIMM" = "Immigigrant flag - (D, G)", # page 6
  "SDCDVABT" = "Aboriginal Identity - (D)", # page 6
  "SDCDVFLA" = "Visible minority flag - (D)", # page 6
  "DOLAN" = "Language Inclusion Flag - (F)", # page 6
  "DOINL" = "Insurance coverage (INL) - Inclusion Flag - (F)", # page 6
  "INL_05" = "Insurance - long-term care costs all / part cost coverage", # page 6
  "DOINP" = "Insurance coverage (INP) - Inclusion Flag - (F)", # page 6
  "INP_05" = "Insurance - prescription medications - all / part cost coverage", # page 6
  "DOPCN" = "Presciption cost - Inclusion Flag - (F)", # page 6
  "PCN_05" = "Had prescriptions for medication - 12 mo", # page 6
  "PCNDGCRA" = "Prescription Medication - Cost related non-adherence - (G)", # page 6
  "DOFSC" = "Food security Inclusion Flag - (F)", # page 6
  "FSCDVAF2" = "Food security adult status (including marginally) - (D)", # page 7
  "FSCDVCF2" = "Food security child status (including marginally) - (D)", # page 7
  "FSCDVHF2" = "Household food security status", # page 7
  "INCDGHH" = "Total Household Income", # page 7
  "INCDGRCA" = "Distribution of household income - National level - (D, G)", # page 7
  "INCDGRPR" = "Distribution of household income - Provincial level - (D, G)", # page 7
  "INCDGRRS" = "Distribution of household income Health region level (D, G)", # page 7
  "WTS_M" = "Weights - Master" # page 7
)

# NA: Not useful (only takes 1 value)
# Cat: Categorical in nature, no ordering
# Ord: Categorical but has natural ordering
# YN: Yes/No, where Yes = 1, No = 2
# YNC: Yes/No, where Yes = 1 and No = 0
# YNDK: Yes/No/Don't Know, where Yes = 1, No = 2, Don't know = 3
# Num: Numeric variable
# Numbers after class: Corresponds to skipped option. Typically ending with "6" means valid skip, whereas "9" just means normal skip
# For example, "Num 996 999" would mean the input is numeric (e.g. minutes for commute), and any skipped option is 996 and 999
classes <- c(
  "VERDATE" = "NA",
  "REFPER" = "NA", # page 3
  "ADM_RNO" = "NA", # page 3
  "COLMODE" = "Cat 6", # page 3
  "COLMODEY" = "Cat 6", # page 3
  "COLMODEP" = "Cat 6 9", # page 3
  "GEOGPRV" = "Cat", # page 3
  "GEODGHR4" = "Cat", # page 3
  "DHHDGHSZ" = "YN 9", # page 3, 1 = Yes = Lives alone, 2 = No = Doesn't live alone, Question: "Do you live alone?"
  "EDDVH3" = "Cat 9", # page 3
  "DHHGAGE" = "Ord", # page 3
  "DHH_SEX" = "Cat", # page 3
  "DOMAC" = "NA", # page 3
  "MAC_05" = "YN 6 9", # page 3
  "DOGEN" = "NA", # page 3
  "GEN_01" = "Ord 9", # page 3
  "GEN_05" = "Ord 9", # page 3
  "GEN_10" = "Ord 9", # page 3
  "GEN_15" = "Ord 6 9", # page 3
  "GEN_20" = "Ord 9", # page 3
  "DOLSM" = "NA", # page 3
  "LSM_01" = "Ord 99", # page 3
  "LSMDVSWL" = "Ord 9", # page 3
  "DOHWT" = "NA", # page 3
  "HWTDGISW" = "Cat 6 9", # page 3
  "HWTDGWHO" = "Cat 6 9", # page 3
  "HWTDGBCC" = "Cat 6 9", # page 3
  "DOWTP" = "NA", # page 3
  "WTP_50" = "Cat 9", # page 3
  "DOCOV2" = "NA", # page 3
  "COV2_005" = "Ord 9", # page 3
  "COV2_10A" = "YN 9", # page 3
  "COV2_10B" = "YN 9", # page 3
  "COV2_10C" = "YN 9", # page 3
  "COV2_10D" = "YN 9", # page 3
  "COV2_10E" = "YN 9", # page 3
  "COV2_10F" = "YN 9", # page 3
  "COV2_10G" = "YN 9", # page 3
  "COV2_10H" = "YN 9", # page 3
  "COV2_10I" = "YN 9", # page 3
  "COV2_10J" = "YN 9", # page 3
  "COV2_038" = "YN 9", # page 3
  "COV2_030" = "Cat 6 9", # page 3
  "COV2_035" = "Ord 6 9", # page 3
  "COV2_041" = "Cat 96 99", # page 3
  "COV2_45A" = "YN 6 9", # page 3
  "COV2_45B" = "YN 6 9", # page 3
  "COV2_45C" = "YN 6 9", # page 3
  "COV2_45D" = "YN 6 9", # page 3
  "COV2_45E" = "YN 6 9", # page 3
  "COV2_45F" = "YN 6 9", # page 3
  "COV2_45G" = "YN 6 9", # page 3
  "COV2_45H" = "YN 6 9", # page 3
  "COV2_45I" = "YN 6 9", # page 3
  "COV2_040" = "Ord 6 9", # page 3
  "DOCOV3" = "NA", # page 3
  "COV3_005" = "Ord 6 9", # page 3
  "DOCCC" = "NA", # page 3
  "CCC_05" = "YN 9", # page 4
  "CCC_80" = "YN 9", # page 4
  "CCC_85" = "YN 9", # page 4
  "CCC_90" = "YN 9", # page 4
  "CCC_135" = "YN 9", # page 4
  "CCCDGSKL" = "YN 6 9", # page 4
  "CCCDGCAR" = "YN 6 9", # page 4
  "DOCC1" = "NA", # page 4
  "CC1_140" = "YN 9", # page 4
  "CC1_145" = "YN 9", # page 4
  "CC1_155" = "YN 6 9", # page 4
  "CC1_170" = "YN 6 9", # page 4
  "DOWDM" = "YNC", # page 4
  "WDMDGDIF" = "Cat 6 9", # page 4
  "DOOHM" = "YNC", # page 4
  "OHM_05" = "Ord 6 9", # page 4
  "OHM_15" = "Ord 6 9", # page 4
  "DOOHM3" = "YNC", # page 4
  "OHM3_05" = "Cat 6 9", # page 4
  "OHM3_10" = "Ord 96 99", # page 4
  "OHM3_15" = "YN 6 9", # page 4
  "OHM3_18" = "YN 6 9", # page 4
  "OHM3_20A" = "YN 6 9", # page 4
  "OHM3_20B" = "YN 6 9", # page 4
  "OHM3_20C" = "YN 6 9", # page 4
  "OHM3_20D" = "YN 6 9", # page 4
  "OHM3_20E" = "YN 6 9", # page 4
  "OHM3_20F" = "YN 6 9", # page 4
  "OHM3_45" = "YNDK 6 9", # page 4
  "OHM3G50A" = "YN 6 9", # page 4
  "OHM3_50C" = "YN 6 9", # page 4
  "OHM3_50E" = "YN 6 9", # page 4
  "DOEAH" = "YNC", # page 4
  "EAHG05" = "Ord 6 9", # page 4
  "EAHDVCHO" = "Num 999.6 999.9", # page 4
  "EAHDVFFD" = "Num 999.6 999.9", # page 4
  "EAHDVFRU" = "Num 999.6 999.9", # page 4
  "EAHDVGRN" = "Num 999.6 999.9", # page 4
  "EAHDVJUI" = "Num 999.6 999.9", # page 4
  "EAHDVORA" = "Num 999.6 999.9", # page 4
  "EAHDVSCD" = "Num 999.6 999.9", # page 4
  "EAHDVSOD" = "Num 999.6 999.9", # page 4
  "EAHDVSTA" = "Num 999.6 999.9", # page 4
  "EAHDVVEG" = "Num 999.6 999.9", # page 4
  "EAHDVEND" = "Num 999.6 999.9", # page 4
  "EAHDVBVT" = "Num 9999.6 9999.9", # page 4
  "EAHDVBVG" = "Ord 6 9", # page 4
  "EAHDVFVT" = "Num 9999.6 9999.9", # page 4
  "EAHDVFVG" = "Ord 6 9", # page 4
  "DOPAA" = "YNC", # page 4
  "PAA_05" = "YN 6 9", # page 4
  "PAA_10A" = "YN 6 9", # page 4
  "PAA_10B" = "YN 6 9", # page 4
  "PAA_10C" = "YN 6 9", # page 4
  "PAA_10D" = "YN 6 9", # page 4
  "PAA_10E" = "YN 6 9", # page 4
  "PAA_10F" = "YN 6 9", # page 4
  "PAA_10G" = "YN 6 9", # page 4
  "PAA_15A" = "Num 996 999", # page 4
  "PAA_15B" = "Ord 96 99", # page 4
  "PAA_30" = "YN 6 9", # page 4
  "PAA_35" = "YN 6 9", # page 4
  "PAA_40A" = "YN 6 9", # page 4
  "PAA_40B" = "YN 6 9", # page 5
  "PAA_40C" = "YN 6 9", # page 5
  "PAA_40D" = "YN 6 9", # page 5
  "PAA_40E" = "YN 6 9", # page 5
  "PAA_40F" = "YN 6 9", # page 5
  "PAA_40G" = "YN 6 9", # page 5
  "PAA_45A" = "Num 996 999", # page 5
  "PAA_45B" = "Ord 96 99", # page 5
  "PAA_60" = "YN 6 9", # page 5
  "PAA_65" = "YN 6 9", # page 5
  "PAA_70A" = "YN 6 9", # page 5
  "PAA_70B" = "YN 6 9", # page 5
  "PAA_70C" = "YN 6 9", # page 5
  "PAA_70D" = "YN 6 9", # page 5
  "PAA_70E" = "YN 6 9", # page 5
  "PAA_70F" = "YN 6 9", # page 5
  "PAA_70G" = "YN 6 9", # page 5
  "PAA_75A" = "Num 996 999", # page 5
  "PAA_75B" = "Ord 96 99", # page 5
  "PAA_95" = "YN 6 9", # page 5
  "PAA_100A" = "Num 996 999", # page 5
  "PAA_100B" = "Ord 96 99", # page 5
  "PAADVMVA" = "Num 99996 99999", # page 5
  "PAADVACV" = "Cat 6 9", # page 5
  "PAADVAC2" = "Ord 6 9", # page 5
  "PAADVVIG" = "Num 99996 99999", # page 5
  "PAADVVOL" = "Num 99996 99999", # page 5
  "PAADVWHO" = "Ord 6 9", # page 5
  "DOUPE" = "YNC", # page 5
  "UPE_05" = "YN 6 9", # page 5
  "UPE_10" = "Ord 6 9", # page 5
  "UPE_15" = "YN 6 9", # page 5
  "UPE_20" = "Ord 6 9", # page 5
  "UPE_25" = "Ord 6 9", # page 5
  "UPE_30" = "Ord 6 9", # page 5
  "UPE_35" = "Ord 6 9", # page 5
  "UPE_40A" = "YN 6 9", # page 5
  "UPE_45" = "Ord 6 9", # page 5
  "UPE_40B" = "YN 6 9", # page 5
  "UPE_50" = "Ord 6 9", # page 5
  "UPE_55" = "Ord 6 9", # page 5
  "UPE_60A" = "YN 6 9", # page 5
  "UPE_65" = "Ord 6 9", # page 5
  "UPE_70" = "Ord 6 9", # page 5
  "UPE_75" = "Ord 6 9", # page 5
  "UPE_80" = "YN 6 9", # page 5
  "UPE_85" = "Ord 6 9", # page 5
  "DOSBE" = "YNC", # page 5
  "SBE_005" = "Cat 96 99", # page 5
  "SBE_010" = "Ord 6 9", # page 5
  "DOSLP" = "YNC", # page 5
  "SLPG005" = "Ord 96 99", # page 5
  "SLP_010" = "Ord 6 9", # page 5
  "SLP_015" = "Ord 6 9", # page 5
  "SLP_020" = "Ord 6 9", # page 5
  "DOCSS" = "NA", # page 5
  "CSS_05" = "YN 9", # page 5
  "CSS_15" = "YN 6 9", # page 5
  "CSSG20" = "Cat 6 9", # page 5
  "CSS_25" = "Num 996 999", # page 5
  "CSS_30" = "Num 996 999", # page 5
  "CSS_40" = "YN 6 9", # page 5
  "DOSPU" = "NA", # page 5
  "SPU_05" = "YN 6 9", # page 6
  "SPU_20" = "Num 996 999", # page 6
  "SMKDVSTY" = "Cat 96 99", # page 6
  "SMKDGYCS" = "Ord 6 9", # page 6
  "SMKDGSTP" = "Ord 6 9", # page 6
  "DOECV" = "NA", # page 6
  "ECV_05" = "YN 9", # page 6
  "ECVG15" = "YN 6 9", # page 6, here 1 = At least once in the past month, 2 = Never
  "ECV_20" = "YN 6 9", # page 6
  "ECVDVSMK" = "Cat 9", # page 6
  "DOECV2" = "NA", # page 6
  "ECV2G05A" = "YN 6 9", # page 6, 1 = Has vaped e-liquid with nicotine, 2 = has not
  "ECV2G05B" = "YN 6 9", # page 6 1 = Has vaped e-liquid without nicotine, 2 = has not
  "ECV2G05C" = "YN 6 9", # page 6 1 = Has vaped e-liquid with unknown content, 2 = has not
  "ECV2_10" = "YN 6 9", # page 6
  "ECV2_15" = "Cat 6 9", # page 6
  "DOALC" = "NA", # page 6
  "ALC_05" = "YN 9", # page 6
  "ALC_10" = "YN 6 9", # page 6
  "ALC_15" = "Ord 96 99", # page 6
  "ALC_20" = "Ord 96 99", # page 6
  "ALCDVTTM" = "Cat 9", # page 6
  "DOCAN" = "NA", # page 6
  "CAN_05C" = "YN 9", # page 6
  "CAN_35A" = "YN 6 9", # page 6
  "CAN_35B" = "YN 6 9", # page 6
  "CAN_35C" = "YN 6 9", # page 6
  "CAN_35D" = "YN 6 9", # page 6
  "CAN_35E" = "YN 6 9", # page 6
  "CAN_35F" = "YN 6 9", # page 6
  "CAN_35G" = "YN 6 9", # page 6
  "CAN_35H" = "YN 6 9", # page 6
  "CAN_35I" = "YN 6 9", # page 6
  "CAN_35J" = "YN 6 9", # page 6
  "CAN_40" = "YN 6 9", # page 6
  "DOFLU" = "YNC", # page 6
  "FLU_05" = "YN 6 9", # page 6
  "FLU_10" = "Cat 96 99", # page 6, time of year categories
  "FLU_15" = "Cat 6 9", # page 6
  "FLU_20A" = "YN 6 9", # page 6
  "FLU_20B" = "YN 6 9", # page 6
  "FLU_20C" = "YN 6 9", # page 6
  "FLU_20D" = "YN 6 9", # page 6
  "FLU_20E" = "YN 6 9", # page 6
  "FLU_20F" = "YN 6 9", # page 6
  "DORHC" = "NA", # page 6
  "RHC_05" = "Cat 9", # page 6
  "DOLMAM" = "NA", # page 6
  "LBFDGHPW" = "Num 96 99", # page 6
  "LBFDVPFT" = "Cat 6 9", # page 6
  "LBFDGWSS" = "Cat 6 9", # page 6
  "SDCDGIMM" = "YN 9", # page 6, Yes = 1 = Non-immigrant, No = 2 = Immigrant/Non-PR. YN to the question "Are you not an immigrant?"
  "SDCDVABT" = "YN 9", # page 6 Yes = 1 = indigenous, No = 2 = non-indigenous. YN to the question "Are you indigenous?"
  "SDCDVFLA" = "YN 9", # page 6 Yes = 1 = visible minority, No = 2 = not a visible minotiry. YN to the question "Are you a visible minority?"
  "DOLAN" = "NA", # page 6
  "DOINL" = "YNC", # page 6
  "INL_05" = "YNDK 6 9", # page 6
  "DOINP" = "YNC", # page 6
  "INP_05" = "YNDK 6 9", # page 6
  "DOPCN" = "YNC", # page 6
  "PCN_05" = "YN 6 9", # page 6
  "PCNDGCRA" = "YN 6 9", # page 6 1 = Cost related non-adherence, 2 = no cost...
  "DOFSC" = "NA", # page 6
  "FSCDVAF2" = "Ord 9", # page 7
  "FSCDVCF2" = "Ord 6 9", # page 7
  "FSCDVHF2" = "Ord 9", # page 7
  "INCDGHH" = "Ord 9", # page 7
  "INCDGRCA" = "Ord 6 9", # page 7
  "INCDGRPR" = "Ord 6 9", # page 7
  "INCDGRRS" = "Ord 6 9", # page 7
  "WTS_M" = "NA" # page 7
)

# General classes (just getting the root class)
general_classes <- sub(" .*", "", classes)

# All variables that are not useless
non_NA <- names(classes)[classes != "NA"]

# Named list defining the coding for each categorical predictor
categorical_codes <- list(
  "COLMODE" = NULL,
  "COLMODEY" = NULL, 
  "COLMODEP" = NULL, 
  "GEOGPRV" = NULL,
  "GEODGHR4" = NULL, 
  "EDDVH3" = c("1" = "Less than secondary", "2" = "Secondary, no post-secondary", "3" = "Post-secondary"),   
  "DHH_SEX" = c("1" = "Male", "2" = "Female"),  
  "HWTDGISW" = NULL,
  "HWTDGWHO" = NULL, 
  "HWTDGBCC" = c("1" = "Normal/Underweight", "2" = "Overweight/Obese"), 
  "WTP_50" = NULL,   
  "COV2_030" = NULL,
  "COV2_041" = NULL, 
  "WDMDGDIF" = NULL,
  "OHM3_05" = NULL,
  "PAADVACV" = NULL,
  "SBE_005" = NULL,
  "CSSG20" = NULL,
  "SMKDVSTY" = NULL,
  "ECVDVSMK" = NULL,
  "ECV2_15" = NULL,
  "ALCDVTTM" = NULL, 
  "FLU_10" = NULL,
  "FLU_15" = NULL, 
  "RHC_05" = NULL,
  "LBFDVPFT" = NULL,
  "LBFDGWSS" = NULL
)

# Looping across each non-NA variable
cleaned_data <- lapply(non_NA, FUN = function(column) { #defining a func and column is the param.

  complete_class <- str_split_1(classes[column], " ")

  # Either NA, Cat, Ord, YN, YNC, YNDK, or Num
  root_class <- complete_class[1]

  # If the user skipped, what number would it be? 
  skipped_options <- as.numeric(setdiff(complete_class, root_class))

  # Anything that ends with 6 is a valid skip
  valid_skip <- skipped_options[endsWith(as.character(skipped_options), "6")]
  
  cleaned_vector <- original_data[[column]]

  # Setting all valid skips to NA. Other skips will be set to the mean/mode later in the code
  cleaned_vector[cleaned_vector == valid_skip] <- NA

  # ADJUSTING FOR EACH CLASS

  # If categorical, then just return the same vector but factorized
  if (root_class == "Cat") {

    # Setting other skips as the mode (median imputation)
    cleaned_vector[cleaned_vector %in% skipped_options] <- names(which.max(table(cleaned_vector[!is.na(cleaned_vector)])))

    # Will replace values with categorical keys
    key <- categorical_codes[[column]]

    # Only changing if we added the key
    if (!is.null(key)) {
      cleaned_vector <- key[cleaned_vector]
    }

    return(factor(cleaned_vector))
  }

  # If categorical with ordering, or numeric, then just return the same vector
  if (root_class %in% c("Ord", "Num")) {
    # Setting other skips as the mean (mean imputation)
    cleaned_vector[cleaned_vector %in% skipped_options] <- mean(cleaned_vector[!cleaned_vector %in% skipped_options], na.rm = TRUE)

    return(cleaned_vector)
  }

  # If YN, then return the same vector but replacing all 2's with 0's. This is done to match "No" to 0 and "Yes" to 1 for consistency
  # Will also factor
  if (root_class %in% c("YN", "YNC", "YNDK")) {

    # Setting other skips as the mode (median imputation)
    cleaned_vector[cleaned_vector %in% skipped_options] <- names(which.max(table(cleaned_vector[!is.na(cleaned_vector)])))

    cleaned_vector[cleaned_vector == 2] <- 0
    cleaned_vector[cleaned_vector == 3] <- 2 # Case when root_class == "YNDK", since Don't Know is 3
    
    # Assigning known factors
    cleaned_vector <- as.character(cleaned_vector)

    cleaned_vector[cleaned_vector == 0] <- "No"
    cleaned_vector[cleaned_vector == 1] <- "Yes"
    cleaned_vector[cleaned_vector == 2] <- "Don't Know"
    
    return(factor(cleaned_vector))
  }

}) |>
  setNames(non_NA) |> 
  as_tibble()

# Classes that are purely numeric (not factored)
numeric_classes <- names(general_classes)[general_classes %in% c("Ord", "Num")]

# Classes that are factored
factored_classes <- names(general_classes)[general_classes %in% c("Cat", "YN", "YNC", "YNDK")]


num_responses <- sort(sapply(colnames(cleaned_data), FUN = function(column) {

  num_not_na <- sum(!is.na(cleaned_data[[column]]))
}), decreasing = TRUE)

names(num_responses) <- dict[names(num_responses)]
