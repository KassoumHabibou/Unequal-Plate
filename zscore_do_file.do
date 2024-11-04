* Compute the Zscore dofile

** Set working directory
cd "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2"
capture mkdir ".\input\zanthro"
cd ".\input\zanthro"
net get dm0004_1, from(http://www.stata-journal.com/software/sj13-2/)

********************************************************************************************************************************************************
****************************************** df global ***************************************************************************************************
********************************************************************************************************************************************************

*** Importing the file 'df_global'
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\df_global.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\df_global.xlsx", firstrow(variables) replace

********************************************************************************************************************************************************
****************************************** Round 1 data ************************************************************************************************
********************************************************************************************************************************************************
** For underfive household member
*** Importing the file 'r1' data
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r1_underfive.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r1_underfive.xlsx", firstrow(variables) replace

** For overfive household member
*** Importing the file 'r1' data
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r1_overfive.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r1_overfive.xlsx", firstrow(variables) replace


********************************************************************************************************************************************************
****************************************** Round 2 data ************************************************************************************************
********************************************************************************************************************************************************
** For underfive household member
*** Importing the file 'r2' data
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r2_underfive.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r2_underfive.xlsx", firstrow(variables) replace

** For overfive household member
*** Importing the file 'r1' data
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r2_overfive.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r2_overfive.xlsx", firstrow(variables) replace



********************************************************************************************************************************************************
****************************************** Round 3 data ************************************************************************************************
********************************************************************************************************************************************************
** For underfive household member
*** Importing the file 'r1' data
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r3_underfive.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r3_underfive.xlsx", firstrow(variables) replace

** For overfive household member
*** Importing the file 'r1' data
import excel "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r3_overfive.xlsx", sheet("Sheet1") firstrow clear

*** Created the zscore for the df_global
egen waz_who = zanthro(weight_kg,wa,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen haz_who = zanthro(height_cm,ha,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)
egen bmiz_who = zanthro(bmi,ba,WHO), xvar(hhm_age) gender(hhm_sex) gencode(m=male, f=female) ageunit(year)

*** Exporting results
export excel using "C:\Users\ASUS\Dropbox\PhD thesis\chapitre_2\PhD-Chapter-2\output\data\r3_overfive.xlsx", firstrow(variables) replace

