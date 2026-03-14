#First, we need to install and load the necessary packages that will be used for this analysis

libraries <- c('rcompanion', 'haven','dplyr') #to store the packages
#install.packages(libraries) #to install the packages (run once)
lapply(libraries, require, character.only = TRUE) #to load the packages

#to read data from British Election Study Stata file (.dta) and store it in data frame df
df <- read_dta('./BSE_2010.dta')

#Let's check the data size
dim(df) #there are 2787 rows and 637 columns (variables)

# --------------------------- 1. Find Relevant Variables ----------------------------- #

#The data set contains many variables, and several of them are irrelevant to the research question.
#We therefore need to select only the relevant variables.

#Dependent variable: Annual Household Income
#Independent variable: Party Identification
#Filter variable: Ethnicity

#loops through every column in df and extracts variable label into a clean, named vector
var_labels <- sapply(df, function(x) attr(x, "label"))
var_labels[!is.na(var_labels)]

#store them into df_var_labels
df_var_labels <- data.frame(variable = names(var_labels[!is.na(var_labels)]),
                            label = var_labels[!is.na(var_labels)],
                            row.names = NULL)

#create a character vector to list the relevant keywords that could be in variable
relevant_key <- c('income','party','identify','ethnicity','race','background','identification')

#find, filter, and store in relevant_var
relevant_var <- subset(df_var_labels, grepl(paste0('\\b(',paste(relevant_key, collapse = '|'),')\\b'),
                            label,
                            ignore.case = TRUE))

# --------------------------- 2. Create New Data Set with Relevant Variables ----------------------------- #

#store selected variable in df_new
df_new <- df[c("Unqid", "zq101", "zqinc", "bq9_1")]
df_new <- as.data.frame(lapply(df_new, zap_labels))


#filter to 5 minority ethnicity groups
#mppi = minority's political party identification
mppi <- df_new[df_new$zq101 %in% c(7,8,9,11,12) & 
              !(df_new$zqinc %in% c(-2,-1,17)) & 
              !(df_new$bq9_1 %in% c(-2,-1,15)),]

# --------------------------- 3. Make Data Set Easier to Understand ----------------------------- #

names(mppi)[names(mppi) == "Unqid"] <- "id"
names(mppi)[names(mppi) == "zq101"] <- "ethnicity"
names(mppi)[names(mppi) == "zqinc"] <- "annual_household_income"
names(mppi)[names(mppi) == "bq9_1"] <- "party_identification"



#---------- ethnicity ----------


#replace value with label to make the data set interpretable 
mppi$ethnicity <- factor(mppi$ethnicity, 
                         levels = c(7, 8, 9, 11, 12),
                         labels = c('Indian', 'Pakistani', 'Bangladeshi', 'Black Caribbean', 'Black African')
                         )


#---------- annual_household_income ----------


#replace value with label to make the data set interpretable 
mppi$annual_household_income <- factor(mppi$annual_household_income,
                                       levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                                       labels = c('0-5000', '5001-10000', '10001-15000', '15001-20000', '20001-25000', '25001-30000', '30001-35000', 
                                                  '35001-40000', '41000-45000', '45001-50000', '50001-60000', '60001-70000', '70001-80000', '80001+')
                                       )

#check unique value of annual household income  
unique(mppi$annual_household_income)

#create income_group column and group annual household income into Low, Medium, and High 
mppi <- mppi %>% mutate(income_group = case_when(annual_household_income %in% c('0-5000', '5001-10000', '10001-15000', '15001-20000') ~ 'Low',
                                                 annual_household_income %in% c('20001-25000', '25001-30000', '30001-35000', '35001-40000', '41000-45000', '45001-50000') ~ 'Medium',
                                                 annual_household_income %in% c('50001-60000', '60001-70000', '70001-80000', '80001+') ~ 'High'))

#place the new income_group after annual_household_income column
mppi <- mppi %>% relocate(income_group, .after = annual_household_income)

mppi$income_group <- factor(mppi$income_group)


#--------- party_identification ----------


#replace value with label to make the data set interpretable 
mppi$party_identification <- factor(mppi$party_identification, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                                     labels = c('None/No', 'Labour', 'Conservatives', 'Liberal Democrats', 'Scottish National Party (SNP)', 
                                                'Plaid Cymru', 'Green Party', 'United Kingdom Independence Party (UKIP)', 'British National Party (BNP)', 
                                                'Coalition party/Conservative-Lib dem party', 'Other', 'Respect')
                                     )

#count each party identification
table(mppi$party_identification)

#create party_group column and group the party group that has small value into Others 
mppi <- mppi %>% mutate(party_group = case_when(party_identification %in% c('None/No', 'Labour', 'Conservatives', 'Liberal Democrats') ~ party_identification,
                                                TRUE ~ 'Others'))
mppi$party_group <- factor(mppi$party_group)

#--------------------

#drop unused factor levels (levels with zero observations)
variables <- c("ethnicity", "annual_household_income", "income_group", "party_identification", "party_group")
mppi[variables] <- lapply(mppi[variables], droplevels)

#check if the data set is correctly cleaned
table(mppi$ethnicity)
table(mppi$annual_household_income)
table(mppi$income_group)
table(mppi$party_identification)
table(mppi$party_group)

#Let's check the data size
dim(mppi) #we can see that the size of the data set is now reduced

#export cleanned data
#write.csv(mppi, 'BSE_2010_clean_updated.csv', row.names = FALSE)
#write_dta(mppi, 'BSE_2010_clean_updated.dta')

# --------------------------- 4. Perform Analysis ----------------------------- #

#create a contingency table 
table_income_party <- table(mppi$income_group, mppi$party_group)
table_income_party <- table_income_party[c("Low", "Medium", "High"), ]
table_income_party

#perform chi-square test
chi_sqr <- chisq.test(table_income_party)
chi_sqr
#p-value = 8.901e-05, which is lesser than 0.05, so the null hypothesis is rejected
#there is an association between annual household income and political party identification among minority ethnicities

#check expected counts
chi_sqr$expected
#Around 93.33% of the expected counts are greater than five
#Meaning that the assumption that more than 80% of expected counts should exceed five is satisfied


#Although Chi-square Test shows an association, we still need to check how strong the association is between annual household income and party identification
cramer_v <- cramerV(table_income_party)
cramer_v #the result is 0.1031 -> weak association

# --------------------------- 5. Visualization ----------------------------- #

#convert raw counts into proportions so that, within each annual household income group, the proportions of party identification sum to 1
prop <- prop.table(table_income_party, margin = 1)

#set color for each annual household income group
cols <- palette()[1:ncol(prop)]

#set plot margins
par(mar = c(5, 4, 4, 14))

#plot bar chart
bp <- barplot(t(prop), beside = FALSE, ylim = c(0, 1), col = cols,
              main = "Minority's Political Party Identification by Annual Household Income",
              xlab = "Income Group",
              cex.lab = 1, cex.names = 1,
              cex.axis = 1,
              )

#set plot boundaries
usr <- par("usr")

#add legend to the right of the plot
legend(x = usr[2] + 0.2, y = usr[4],
       legend = rownames(t(prop)), 
       fill = cols, 
       xjust = 0, yjust = 1, 
       cex = 1, 
       bty = "n",
       xpd = NA           
       )
