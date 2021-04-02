
*Below are SAS Codes used for all the five hypotheses in final project;

*#############################################  BLOCK ONE    #######################################################
######################################   Name = Vandana   #######################################################
        ##########################           Hypothesis 1             ###########################;

* Importing the whole 2018 mortality SAS file;
data mortality_data;
set 'D:\SAS Dataset\mort2018us.sas7bdat'; * Define the location of the objects;
run;

*Avg age of mortality in men aged 65 years or greater at hospice
care is less than females in the same group;

data hypothesis_1; * Give name to the data;
set mortality_data;
keep sex ager52 placdth; *Keeping variables of interest;
where ager52>=39 and ager52<52 and placdth~=9; *Filtering values;
run;

data hypothesis_1_recoded;
set hypothesis_1;
if placdth<=3 then place_death='Hospital'; *Recoding variable to see the results as per hypothesis;
if placdth=4 then place_death='Home';
if placdth=5 then place_death='Hospice';
if placdth=6 then place_death='Nursing Care';
where placdth~=7 and placdth~=9;
run;

*Bar chart for people who died at hospice facility by gender;
proc gchart data=hypothesis_1_recoded; *Bar chart;
title 'Bar chart for place of death for people
at or after 65 years';
vbar place_death/type=PERCENT; *vbar used to plot bar chart;
run;

*Chi-square;
proc freq data=hypothesis_1_recoded;
title 'Association between Place of Death and Gender';
tables place_death*sex/ chisq; *Chi-sqaure test;
run;


*#############################################  BLOCK ONE ENDS     #######################################################


*******************************************************************************************************************************************


*#############################################  BLOCK TWO    #######################################################
######################################     Name = Rudra Patel    #######################################################
        ##########################           Hypothesis 2             ###########################;
* Married person with higher education are associated with death at later age 
(above 50 years) than individuals who are not married and have higher education;

* Importing the whole 2018 mortality SAS file;
data final_proj;
set 'D:\SAS Dataset\mort2018us.sas7bdat'; * Define the location of the objects;
run;

data final_data; * Give name to the data;
set final_proj;
keep marstat educ2003 ager52; *Keeping variables of interest;
run;

*Descriptive Statistics;
proc univariate data=final_data;
title 'Descriptive Statistics'; *Assigning title to the result table;
ods select BasicMeasures;
var ager52;
where educ2003>5 and educ2003<9 and marstat='M';
run;

*Frequency Distribution;
proc freq data=final_data ORDER=freq;
title 'Frequency for Marital Status'; *Assigning title to the result table;
tables marstat*educ2003; *frequency distribution;
run;

*Pie chart;
data graduatedegree;
set final_data;
if educ2003<=3 then degree='high school graduate'; *Recoding variables for the analysis;
if educ2003>3 and educ2003<7 then degree='Not Graduate';
if educ2003>=7 and educ2003<9 then degree='Graduate';
run;

proc gchart data=graduatedegree;
title 'Pie Chart of Education'; *Assigning title to the result table;
pie degree/ type=PERCENT;
where ager52>35 and ager52<52 and marstat='M' ;
run;

*Bar chart;
proc gchart data=final_data;
title 'Bar chart for education of married individuals with age above 50'; *Assigning title to the result table;
vbar educ2003/ midpoints=1 to 8 by 1 type=PERCENT;
where ager52>35 and ager52<52 and educ2003<9 and marstat='M' ;
run;

*Hypothesis testing Chi-square test;
data mod_degree;
set final_data;
if educ2003>3 and educ2003<7 then degree=1; *Recoding variables for the analysis;
if educ2003>=7 and educ2003<9 then degree=2;
run;
proc freq data=mod_degree;
table marstat*ager52 / chisq measures;
where degree=2 and ager52>35 and ager52<52 and marstat~='U';
run;

proc logistic data=final_proj; *logistic regression analysis;
class autopsy;
model mandeath(event='7')= autopsy;
WHERE autopsy = "Y" OR autopsy = "N" AND mandeath = 2 OR mandeath = 7;
run;


*#############################################  BLOCK TWO ENDS     #######################################################


****************************************************************************************************************************************


*#############################################  BLOCK THREE    #######################################################
######################################    Name = Kamana Pokhrel    #######################################################
        ##########################           Hypothesis 3             ###########################

The probability of performing autopsies is less when the manner of death is considered natural versus those that died of suicide.

* Importing the whole 2018 mortality SAS file;
DATA Mortality; * give the name ;
SET 'C:/Users/kaman/Desktop/HCIP 6102/Assignment_5/Mortality_Data/Mort2018USPubUse.sas7bdat' ; * Define the location of the objects;
KEEP educ2003 monthdth Sex ager52 placdth marstat weekday injwork mandeath autopsy racer5 ; * Select variables;
RUN ; *run the code;

* Filter data and variables;
DATA autopsy_death ; * Give name to the data;
SET  ; * Provide the location of data;
KEEP mandeath autopsy ; * Select variable;
WHERE autopsy = "Y" OR autopsy = "N" AND mandeath = 2 OR mandeath = 7 ; * Filter values;
RUN; * Run the code;


* Five values of autopsy had n, so I converted to N;
DATA autopsy_death ; * Give name to the data;
  SET autopsy_death ; * Specify the location of data;
  autopsy=TRANWRD(autopsy,'n','N') ; * Replace 'n' value of autopsy by 'N';
RUN ; * Run the code;


*****************************************************
*Conduct a chi-square test for normal data;
PROC FREQ DATA = autopsy_death; * Provide procedure and data ;
TITLE 'Chi-square test for autopsy and manner of death'; * Give the title ;
TABLE autopsy * mandeath /CHISQ; * Specify the variables and method ;
RUN; * Run the code;

*Conduct a chi-square test for suicide and natural death;
PROC FREQ DATA = autopsy_death; * Provide procedure and data ;
TITLE 'Chi-square test for autopsy and manner of death as suicide or natural deaths'; * Give the title ;
TABLE autopsy * mandeath /CHISQ MEASURES; * Specify the variables and method ;
WHERE mandeath = 2 OR mandeath = 7 AND autopsy = "Y" OR autopsy = "N" ; * Filter values;
FOOTNOTE "mandeath 2 = suicide & mandeath 7 = natural deaths"; * Give a footnote;
RUN; * Run the code;

****************************************************
* Create new variable Group;
DATA autopsy_death_new; * Give name to the data;
SET autopsy_death; * Specify the location of data;
IF autopsy = "Y" AND mandeath = 2 THEN Group = "Group 1"; * Create group 1; 
IF autopsy = "Y" AND mandeath = 7 THEN Group = "Group 2"; * Create group 2; 
IF autopsy = "N" AND mandeath = 2 THEN Group = "Group 3"; * Create group 3; 
IF autopsy = "N" AND mandeath = 7 THEN Group = "Group 4"; * Create group 4; 
RUN; * Run the code;
* Note mandeath 2 = suicide, mandeath 7 = natural death, autopsy="Y" is yes and autopsy="N" is no ;

*****************************************************
* Conduct Chi Square Test for New Group;
PROC FREQ DATA = autopsy_death_new; * Specify the procedure and data;
TITLE "Chi Square Test of the Chance of Autopsy in the Manner of Death as Suicide vs. Natural One" ; * Give the title;
TABLES Group / CHISQ MEASURES ; * Create tables;
FOOTNOTE "Group 1 = Suicide & Autopsy, Group 2 = Natural Death & Autopsy, 
Group 3 = Suicide and no autopsy, and Group 4 = Natural Deaths and No Autopsy" ; * Give a footnote;
RUN ; * Run the code;

****************************************************
* Create a bargraph for groups using percent;
TITLE "Comparision of having autopsy in suicide and natural deaths" ; * Give the title;
PROC GCHART DATA =autopsy_death_new (FIRSTOBS = 1 OBS = 284630) ;
VBAR Group / TYPE = PERCENT ; * Specify variable and type;
FOOTNOTE "Group 1 = Suicide & Autopsy, Group 2 = Natural Death & Autopsy, 
Group 3 = Suicide & no autopsy, and Group 4 = Natural Deaths & No Autopsy" ; * Give a footnote;
RUN ; * Run the code;

* Create a bargraph for groups using frequency;
TITLE "Comparision of having autopsy in suicide and natural deaths" ; * Give the title;
PROC GCHART DATA =autopsy_death_new; * Specify procedure and data;
VBAR Group; * Specify variable;
FOOTNOTE "Group 1 = Suicide & Autopsy, Group 2 = Natural Death & Autopsy, 
Group 3 = Suicide & no autopsy, and Group 4 = Natural Deaths & No Autopsy" ; * Give a footnote;
RUN ; * Run the code;


***************************************************************
* Pie chart;
* Create a pie chart to compare autopsy in natural death;
PROC GCHART DATA = autopsy_death_new ; * Specify procedure and data;
TITLE "Pie Chart of Autopsy in Natural Deaths" ; * Give the title;
WHERE Group = "Group 2" OR Group = "Group 4" ; * Filter values;
PIE Group / TYPE = PERCENT; * Specify variable and type;
FOOTNOTE "Group 2 = Natural Death & Autopsy, and Group 4 = Natural Deaths & No Autopsy" ; * Give a footnote;
RUN; * Run the code;

* Create a pie chart to compare autopsy in suicidal deaths;
PROC GCHART DATA = autopsy_death_new ; * Specify procedure and data;
TITLE "Pie Chart of Autopsy in Suicidal Deaths" ; * Give the title;
WHERE Group = "Group 1" OR Group = "Group 3" ; * Filter values;
PIE Group / TYPE = PERCENT; * Specify variable and type;
FOOTNOTE "Group 1 = Suicide & Autopsy, Group 3 = Suicide & no autopsy" ; * Give a footnote;
RUN; * Run the code;

***********************************************************
* Binomial Test for suicide;
* Create a new variable and data to prepare for binomial test of autopsy for suicide;
DATA autopsyChangeSui; * Give name;
SET autopsy_death_new ; * Define location;
A_suicide = 0; * Set new variable's value to 0;
IF Group = "Group 1" THEN A_suicide = 1; * Put group 1 as 1;
WHERE Group = "Group 3" OR Group = "Group 1"; * Filter values;
FOOTNOTE; * Disable footnote;
RUN; * run codes;

* Create frequency table, and conduct proportion test;
PROC FREQ DATA = autopsyChangeSui; * Select procedure;
TITLE 'Proportion test for Group 1 and Group 3 (autopsy in suicide cases)';  * Give title;
TABLES A_suicide / BINOMIAL(P=0.5 LEVEL ='1'); * Create tables containing results of binomial proportion test;
RUN; * run codes;

* Binomial Test for natural deaths;
* Create a new variable and data to prepare for binomial test of autopsy for suicide;
DATA autopsyChangeNat; * Give name;
SET autopsy_death_new ; * Define location;
A_Nat = 0; * Set new variable's value to 0;
IF Group = "Group 2" THEN A_Nat = 1; * Put group 2 as 1;
WHERE Group = "Group 4" OR Group = "Group 2"; * Filter values;
RUN; * run codes;

* Create frequency table, and conduct proportion test;
PROC FREQ DATA = autopsyChangeNat; * Select procedure;
TITLE 'Proportion test for Group 2 and Group 4 (autopsy in natural deaths)';  * Give title;
TABLES A_Nat / BINOMIAL(P=0.5 LEVEL ='1'); * Create tables containing results of binomial proportion test;
RUN; * run codes;


**********************************************************
* Check the logistic regression for the manner of death and having autopsy ;
proc logistic data=autopsy_death plots;*(only)=effect;
title "Logistic Regression for Autopsy for Suicidal and Natural Deaths" ; * Provide title;
class mandeath; * Specify the class;
model autopsy(event='Y')= mandeath; * Put event as suicide;
WHERE mandeath = 2 or mandeath = 7 AND autopsy = "Y" OR autopsy = "N"; * Filter values;
RUN ; * run codes;

********************************************************************;
* Do Multiple logistic regression for suicidal deaths ; *Done By Rudra;
data mort_1;
set Mortality;
if educ2003 <=4 then degree='High School Eductaion'; *Recoding variables for the analysis;
if educ2003>=5 and educ2003<9 then degree='Having a Degree'; 
if racer5=1 then race_recode='White';
if racer5=2 then race_recode='black';
if placdth<=3 then place_death='Hospital';
if placdth=4 then place_death='Home';
if placdth=5 then place_death='Hospice';
if placdth=6 then place_death='Nursing Care';
where mandeath=2 or mandeath=7;
run;


PROC LOGISTIC DATA = mort_1 ; * Specify data and procedure;
TITLE 'Multiple Logistic Regression for Suicidal Deaths' ; * Provide title;
CLASS autopsy sex degree race_recode place_death; * Specify the classes;
MODEL mandeath(Event='2')= autopsy sex degree race_recode place_death; * Specify the event and model;
WHERE autopsy='Y' or autopsy='N'; * Filter values;
RUN ; * run codes;


*#############################################  BLOCK THREE ENDS     #######################################################


******************************************************************************************************************************************


*#############################################  BLOCK FOUR    #######################################################
######################################   Name =  Timothy Sokphat   #######################################################
        ##########################           Hypothesis 4             ###########################;

data Logistic; *Name Data Subset;
Set "\\apporto.com\dfs\UNCC\Users\tsokphat_uncc\Desktop\mort2017.sas7bdat\mort2017.sas7bdat"; *Provide the location of data ;
Keep ager52 racer5; * Specify variables to keep ;
Run; * Run the code ;

data hypothesis_2; *Recoding race as binary variable, setting up third variable to filter age;
set logistic; *Provide the location of data ;
if racer5 = 1 then Status=0; *Create status = o ;
if racer5 = 2 then Status=1; *Create status = 1 ;
if ager52<29 then age='Early age'; *Filter starting at 29 because that is <14 years of age in data set;
if ager52>=29 and ager52<=30 then age='adult'; *Create age = adult;
where ager52<=30; * Filter values ;
run; * Run the code ;

proc logistic data=hypothesis_2;*Run regression;
title 'Child mortality as a function of race and age'; * Give the title ;
class age; * Put class as age ;
model status(event='1')=age; * Put event = 1 for status;
where Status~=.; * Filter values ;
run; * Run the code ;

*#############################################  BLOCK FOUR ENDS     #######################################################


******************************************************************************************************************************************


*#############################################  BLOCK FIVE    #######################################################
######################################   Name =  Khalil Fields   #######################################################
        ##########################           Hypothesis 5             ###########################;
data sasfinal; * Give name to the data;
set '\\apporto.com\dfs\UNCC\Users\kfield12_uncc\Desktop\mortterr2017'; *Provide the location of data ;
keep weekday mandeath ager52; * Specify variables to keep ;
where ager52>=31 and ager52<=32; *filtering for only ages of interest*;
run; * Run the code ;

data sasfinal_new; * Give name to the data;
set sasfinal; *Provide the location of data ;
if mandeath=1 then death='Accident'; * Create death as Accident ;
if mandeath>1 then death='Other'; * Create death as Others ;
if weekday>=2 and weekday<=6 then day='weekday'; * Create day as weekday for Monday-Friday*;
else day='weekend'; * Create day as weekend ;
where weekday~=9 and mandeath~=.; *Filter values ;
run; * Run the code ;

proc freq data=sasfinal_new; * Specify the procedure and data ;
title 'Chi-Square for Accidental Deaths'; *Provide title ;
table day*death/chisq; * Create a table for chi-square test ;
run;

data sasfinal_new2; * Give name to the data;
set sasfinal; *Provide the location of data ;
if mandeath=1 then death='Accident'; * Create death as Accident ;
if mandeath>1 then death='Other'; * Create death as Others ; 
if weekday>=2 and weekday<=5 then day='weekday'; * Create day as weekday for Monday-Friday*;
else day='weekend'; * Create day as weekend ; 
where weekday~=9 and mandeath~=.; *Filter values ;
run; * Run the code ;

*#############################################  BLOCK FIVE ENDS     #######################################################


*******************************************************************************************************************************************
