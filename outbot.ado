**********************************************
***Methods of labeling outliers
***Imputation outliers value by median or mean
***BC is boxcox transformation
**********************************************

*! version 2.1.1   8sept2023

cap program drop outbot
program define outbot, rclass sortpreserve byable( recall) 

syntax varlist (numeric) [if] [in] [using] [,SETs(varlist max=5) NOGENerate BC RULE(integer 999) LABOUTlier ] METhods(string) Imethod(string)

marksample touse
set more off
local MAXObsUse 3
local StataUsedVersion=int(c(version))

******PARAMETRAGE DU PROGRAMME
qui{

		***Macro incluant toutes les methodes
		if "`methods'"=="ALL" | "`methods'"=="all"{
			local methods "standdeviation zscore mzscore tukey tuajust  made medrule"	
		}

		***creation de la variable des groupes de stratification
		if "`sets'"!=""{
			tempvar SetIdentificator
			qui egen `SetIdentificator'=group(`sets') if `touse'
		}

		if "`sets'"==""{
			tempvar SetIdentificator
			qui gen `SetIdentificator'=1 if `touse'
		}
		***creation de la macro du contenue des groupes
		levelsof `SetIdentificator',local(SetsContents)

		***Nombre des elements dans les macros
		local nbrVarsList: word count `varlist'
		local nbrSetsContents: word count `SetsContents'
		local nbrMethode: word count `methods'

		***Compteur
		local TOTALBOUCLES=`nbrSetsContents'*`nbrVarsList'
		local ITERATION=0
		
		if "`sets'"==""{
			local TOTALBOUCLES=`nbrVarsList'
			local ITERATION=0
		}

		***controle du nombre de methodes d'imputation
		local nbrIMethod: word count `imethod'

		if `nbrIMethod'>=2{
			di as err "Only one method of imputation is required for option imethod"
			exit
		}

		if `nbrIMethod'==1{

				if "`imethod'"=="mean"{
				local inputmethod r(mean)
				}

				if "`imethod'"=="p50"{
				local inputmethod r(p50)
				}				
		}

		***controle de la règle d'imputation
		local nbrMethode: word count `methods'
		if "`rule'"!="999" & `rule'>`nbrMethode'{
			di as err "Erreur: Le nombre de methodes qui définit la regle de valeur aberrante est supérieur au nombre de methodes spécifié"
			exit
		}

		if "`rule'"=="999"{
			local rule sd
		}



}

********************************************************************************
**************************************************************FIN DU PARAMETRAGE
********************************************************************************


******Begin var treatment
foreach var of local varlist {

******PARAMETRAGE AVANT  LA RECHERCHE DE VALEURS ABERRANTES

***reinitialisation des méthodes pour chaque variable
local nbrMethode: word count `methods'
if "`methods'"=="ALL" | "`methods'"=="all"{
	local methods "standdeviation zscore mzscore tukey tuajust  made medrule"
}
local Methods "`methods'"
local Methods: list uniq Methods


***Non utilisation de la transformation de Box Cox
 if "`bc'"==""{
 qui cap drop `var'_bc
 
	foreach method of local Methods{
		
		local MethodInUpperCase=strupper("`method'")
		local Method3th=substr("`MethodInUpperCase'",1,3)
		
		cap drop I`var'_`Method3th'
		qui cap drop DBOTI_`var'`Method3th'
		
		qui ge I`var'_`Method3th'=`var'
		qui ge DBOTI_`var'`Method3th'=0
		
		lab def outliersBotiLab 0 "No outlier" 1 "Outlier",replace
		lab val DBOTI_`var'`Method3th' outliersBotiLab
		
		local MethodAbreviation0 `MethodAbreviation0' `Method3th'		
	}	
}

***Utilisation de la transformation de Box Cox
if "`bc'"!=""{
 cap drop `var'_bc

 qui bcskew0 `var'_bc=`var'
 local lambda_bc=r(lambda)
 lab var `var'_bc "Box Cox transformation"
 
		foreach method of local Methods{	
			local MethodInUpperCase=strupper("`method'")
			local Method3th=substr("`MethodInUpperCase'",1,3)
			
			cap drop I`var'_`Method3th'
			cap drop DBOTI_`var'`Method3th'
			
			qui bcskew0 I`var'_`Method3th'=`var'
			qui ge DBOTI_`var'`Method3th'=0
			
			lab def outliersBotiLab 0 "No outlier" 1 "Outlier",replace
			lab val DBOTI_`var'`Method3th' outliersBotiLab
			local MethodAbreviation0 `MethodAbreviation0' `Method3th'		
		}	

}


local MethodAbreviation0 :list uniq MethodAbreviation0

***Variable resultat des imputations	
 cap drop i`var'			
 qui   ge i`var'=`var'		//Variable imputé apres la methode 	
 lab var  i`var' "`var'  imputé "	 

******DEBUT DES RECHERCHES DE VALEURS ABERRANTES PAR GROUPE
foreach SetsContent of local SetsContents{

di _newline(1)
di as input "Execution of the procedure for detecting outliers in the `var' variable for " ///
as text "`SetsContent'"


qui local Methods "`methods'"
qui local MethodAbreviation  "`MethodAbreviation0'" 
 


											*recherche de valeurs aberrantes pour au moins 10 observations
											 qui count if `SetIdentificator'==`SetsContent' &  `touse'

											  if r(N)>`MAXObsUse'{									  	
di as res "Number of treated observation for sets=`SetsContent' is " as text "`r(N)'"						

																				
												gettoken method Methods: Methods												
												while "`method'"!="" {
di as result "Outlier detection for the `var' variable:  " as err "Execution of method `method'"
													local MethodInUpperCase=strupper("`method'")
													local Method3th=substr("`MethodInUpperCase'",1,3)
													
													if "`bc'"==""{
													qui outbotiDavid `var' if `SetIdentificator'==`SetsContent' &  `touse',`method'													
													}
													
													if "`bc'"!=""{
													qui outbotiDavid `var'_bc if `SetIdentificator'==`SetsContent' &  `touse',`method'
													ren `var'_bc_`Method3th' `var'_`Method3th'
													}
													
													gettoken method Methods: Methods

													if _rc==2000{
													di as err "no observation"
													}

													else {													
																																					
																qui count if  `SetIdentificator'==`SetsContent' &  `touse' & `var'_`Method3th'!=0 &  `var'_`Method3th'!=.
																local NBREOUTLIERS=r(N)
																if   `NBREOUTLIERS'!=0{
di as res "Number of outliers detected for this method is " as err " `r(N)'"

																	qui count if  `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'==0
																	local NBRENONOUTLIERS=r(N)
																	if `NBRENONOUTLIERS'!=0{
																	if "`bc'"==""{
																	qui su  `var' if `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'==0,de
																	replace  I`var'_`Method3th'=`inputmethod' if `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'!=0 &  `var'_`Method3th'!=.
																	replace  DBOTI_`var'`Method3th'=1 if `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'!=0 &  `var'_`Method3th'!=.
																	}
																	if "`bc'"!=""{
																	qui su  `var'_bc if `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'==0,de
																	replace  I`var'_`Method3th'=`inputmethod' if `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'!=0 &  `var'_`Method3th'!=.
																	replace  DBOTI_`var'`Method3th'=1 if `SetIdentificator'==`SetsContent' &  `touse' &  `var'_`Method3th'!=0 &  `var'_`Method3th'!=.
																	}
																	
																	}
																	
										
																}		
																
																if "`laboutlier'"==""{
																	qui drop `var'_`Method3th'
																}
																if "`laboutlier'"!=""{
																	qui lab var `var'_`Method3th' "identification of outliers for the variable `var' using the `MethodInUpperCase' method"
																}
												
															}
																																								 
														 }
														 
														if "`rule'"=="sd"{
														di "sd methode"
														***regle de choix basé sur un ecart type min
														foreach x of local MethodAbreviation{
														qui su I`var'_`x' if `SetIdentificator'==`SetsContent' &  `touse' ,de
														local SD_`x'=r(sd)
														}

														gettoken first MethodAbreviation: MethodAbreviation
														local Min= `SD_`first''
														local MinMethod="`first'"
														foreach y of local MethodAbreviation{
														 if `Min'>=`SD_`y''{
															 local Min=`SD_`y''
															 local MinMethod="`y'"
															 *di "Le minimum est la methode `y' de r(sd)=`Min'"
														 }														
														}
														***Choix de la methode avec sd min
														if "`bc'"==""{
														qui replace i`var'=I`var'_`MinMethod' if `SetIdentificator'==`SetsContent' &  `touse' & DBOTI_`var'`MinMethod'==1
														}
														
														if "`bc'"!=""{
														qui replace i`var'=round(exp(ln(`lambda_bc'*I`var'_`MinMethod'+1)/`lambda_bc')) if `SetIdentificator'==`SetsContent' &  `touse' & DBOTI_`var'`MinMethod'==1
														}
														di as res "The best choice of method is the `MinMethod' method with " as text " r(sd)=`Min'" 
														}
														
														
														if "`rule'"!="sd"{
														tempvar outlierSum
														qui egen `outlierSum'=rowtotal(DBOTI_`var'*)
														qui ge botidavidOULIERfix=(`outlierSum'>=`rule')
														qui su `var' if `SetIdentificator'==`SetsContent' &  `touse' & botidavidOULIERfix==0 ,de
																					
														qui replace i`var'=`inputmethod' if `SetIdentificator'==`SetsContent' &  `touse' & botidavidOULIERfix==1 
														preserve
														keep botidavidOULIERfix
														ren botidavidOULIERfix outliers
														tabstat *,s(sum mean sd) 
														restore
														qui drop botidavidOULIERfix	
															
														}
														
														
														
di as input "End of the detection procedure for the variable `var'"


													}

local ITERATION=`ITERATION'+1
local PRCT=round(`ITERATION'*100/`TOTALBOUCLES')
di "Percentage of execution: `PRCT'% "	
}

*********************************************************
******FIN DES RECHERCHES DE VALEURS ABERRANTES PAR GROUPE
*********************************************************


******EXPORTATION DES OUTLIERS
 local meboti "`using'"
 local p__botix: list sizeof meboti
 

if "`p__botix'"!="0"{
	foreach dropme of local MethodAbreviation0{
	local restMethodAbreviation0: list MethodAbreviation0-dropme

	preserve
		foreach botiLocal of local restMethodAbreviation0{
		qui cap drop I`var'_`botiLocal'
		qui cap drop `var'_`botiLocal'
		}
		qui cap drop I`var'_`dropme'
		*keep if `var'!=i`var'_`dropme'
		keep if `var'_`dropme'!=0
		qui cap drop `SetIdentificator'
		qui cap drop DBOTI_`var'*
		cap drop __*
		cap order `var'_* ,after(`var')
		qui cap export excel `using',sheet(I`var'_`dropme') firstrow(var)  sheetreplace  
	restore

	}
}

***statistic of outlier labelling
preserve
	keep DBOTI_`var'*
	ren DBOTI_`var'* *
	tabstat *,s(sum mean sd) 
restore
						 
qui cap drop DBOTI_`var'*

***dropping obs
if "`nogenerate'"!=""{
foreach dropme of local MethodAbreviation0{
qui drop I`var'_`dropme'
}
qui cap drop `var'_bc
}


}
qui cap drop `SetIdentificator'

end

********************************************************************************
***********************************
***Seven Methods of labeling outliers
***********************************

*! version 2.1 .0   13mars2023
cap program drop outbotiDavid
program define outbotiDavid, rclass sortpreserve byable( recall) 

syntax varlist [if] [in]   [,ALL STAnddeviation ZSCore MZScore TUkey TUAjust BOXajust MADe MEDrule]
local StataUsedVersion=int(c(version))

foreach var of local varlist {
  capture confirm numeric variable `var'
   if _rc {
     di as error  " `var' se veut être une variable numérique"
     error 108
    }
marksample touse
**list de retour
qui summ `var' if `touse', de
	local STDEV = r(sd)
	local MEAN = r(mean)
	local P50 = r(p50)
	local P25 = r(p25)
	local P75 = r(p75)
	local IQR = `P75' - `P25'	

set more off

***Control of options
/*
if "`all'"=="" & "`standdeviation'"=="" &  "`zscore'"=="" &  "`mzscore'"=="" & "`turkey'"=="" & "`tuajust'"==""{
di "
}
*/

if "`standdeviation'"=="" &  "`zscore'"=="" &  "`mzscore'"=="" & "`tukey'"=="" & "`tuajust'"=="" & "`made'"=="" & "`medrule'"=="" {
	local all ALL
}					 

*****************************************************SD-METHOD  (Standard Deviation Method)



if "`standdeviation'"!="" | "`all'"!=""{

	qui {	
	local Isd1=`MEAN'-2*`STDEV'
	local Isd2=`MEAN'+2*`STDEV'
	local Osd1=`MEAN'-3*`STDEV'
	local Osd2=`MEAN'+3*`STDEV'
	capture drop `var'_STA

	gen byte `var'_STA = 0 if    `Isd1'<=`var'  & `var'<=`Isd2'  & `touse'
	
	
	replace `var'_STA  = -1 if `var'<`Isd1'  & `var'>`Osd1'  & `touse'
	replace `var'_STA  = -2 if `var'<=`Osd1' & `touse'
	replace `var'_STA  = 1 if `var'>`Isd2'   & `var'<`Osd2'  & `touse'
	replace `var'_STA  = 2 if `var'>=`Osd2'  & !mi(`var')    & `touse'
	
	label define `var'_STA 	-2 "Probable outlier (-3)"   ///
							-1 "Possible  outlier (-2)"     ///
							 0 "Non outlier"                      ///
							 1 "Possible  outlier (+2)"      ///
							 2 "Probable  outlier (+3)", replace
	label values `var'_STA `var'_STA
	}
	
	***output
if `StataUsedVersion'<=16{
	di _newline(2)
	label var `var'_STA "STANDARD DEVIATION (SD) METHOD"
	di in smcl "{bf: ****    STANDARD DEVIATION (SD) METHOD   **** }"
	table `var'_STA, c(n `var' mean `var' sd `var' min `var' max `var') row
	di "Outliers flag variable has been genereted"
}

if `StataUsedVersion'>=17{
}

}


/********************************************************************   Z-SCORE  METHOD *****************************************/

if "`zscore'"!="" |  "`all'"!=""{
	
	qui {
	
	capture drop `var'_ZSC
	tempvar Z
	ge `Z'=abs((`var'-`MEAN')/`STDEV')

	ge byte `var'_ZSC=(`Z'>3) if  `touse'
	lab def `var'_ZSC  0 "Non outlier"  1 "Oulier",replace
	lab val `var'_ZSC `var'_ZSC
	}
	
	***output
	
	if `StataUsedVersion'<=16{
	di _newline(2)
	label var `var'_ZSC  "Z-SCORE METHOD"
	di in smcl "{bf: ****    Z-SCORE METHOD   **** }"
	table `var'_ZSC, c(n `var' mean `var' sd `var' min `var' max `var') row
	di "Outliers flag variable has been genereted"
	}
	if `StataUsedVersion'>=17{
	}
	
}

/********************************************************************   THE MODIFIED Z-SCORE   METHOD *****************************************/
if "`mzscore'"!="" |  "`all'"!=""{
	
	qui {
		capture drop `var'_MZS
		tempvar M  BOTITEMP
		ge `BOTITEMP'=abs(`var'-`STDEV') if  `touse'
		su `BOTITEMP' if  `touse',de 
		local MAD=r(p50)
		ge `M'=abs((0.675 *(`var'-`MEAN'))/`MAD')  if  `touse'

		ge byte `var'_MZS=(`M'>3.5) if  `touse'
		lab def `var'_MZS 0 "Non outlier"  1 "Oulier",replace
		lab val `var'_MZS	`var'_MZS						
	}
	
	***output
	if `StataUsedVersion'<=16{
	di _newline(2)
	label var `var'_MZS  "THE MODIFIED Z-SCORE "
	di in smcl "{bf: ****    THE MODIFIED Z-SCORE METHOD   **** }"
	table `var'_MZS, c(n `var' mean `var' sd `var' min `var' max `var') row
	di "Outliers flag variable has been genereted"
	}
	
	if `StataUsedVersion'>=17{
	}
}

/********************************************************************  TUKEY'S BOXPLOT METHOD  *****************************************/
if "`tukey'"!="" |  "`all'"!=""{

	qui {
		local IF1 = `P25' - 1.5*`IQR' 
		local IF2 = `P75' + 1.5*`IQR' 
		local OF1 = `P25' - 3*`IQR' 
		local OF2 = `P75' + 3*`IQR' 
		capture drop `var'_TUK /*Nom de variable_Tukey Methode traditionnelle*/
		
		gen byte `var'_TUK = 0 if `var'>=`IF1' & `var'<=`IF2' & `touse'
		replace `var'_TUK = -1 if `var'<`IF1' & `var'>`OF1' & `touse'
		replace `var'_TUK = -2 if `var'<=`OF1' & `touse'
		replace `var'_TUK = 1 if `var'>`IF2' & `var'<`OF2' & `touse'
		replace `var'_TUK = 2 if `var'>=`OF2' & !mi(`var') & `touse'
		
		label define `var'_TUK  -2 "Probable outlier (-3)"   ///
								-1 "Possible  outlier (-1,5)"     ///
								 0 "Non outlier"                      ///
								 1 "Possible  outlier (+1,5)"      ///
								 2 "Probable  outlier (+3)", replace
		label values `var'_TUK `var'_TUK 
		
		return scalar L_MT1 = `P25' - 1.5*`IQR'
		return scalar U_MT1 = `P25' + 1.5*`IQR'
		return scalar L_MT2 = `P25' - 3*`IQR'
		return scalar U_MT2 = `P25' + 3*`IQR'
	}
	
	***output
	if `StataUsedVersion'<=16{
	di _newline(2)
	label var `var'_TUK "TUKEY'S BOXPLOT METHOD"
	di in smcl "{bf: ****   TUKEY'S BOXPLOT METHOD   **** }"
	table `var'_TUK, c(n `var' mean `var' sd `var' min `var' max `var') row
	di "Outliers flag variable has been genereted"
	}
	
	if  `StataUsedVersion'>=17{
	}
}

/*******************************************  AJUSTED TUKEY METHOD  **************************************************************/
if "`tuajust'"!="" |  "`all'"!=""{

	qui {
		capture drop `var'_TUA
		gen byte `var'_TUA = -1 if `var' < (`P25' - 6*(`P50'-`P25') ) & `touse'
		replace `var'_TUA = 1 if `var' > (`P75' + 6*(`P75'-`P50') ) & !mi(`var') & `touse'
		replace `var'_TUA = 0 if `var' >= (`P25' - 6*(`P50'-`P25') ) &  `var' <= (`P75' + 6*(`P75'-`P50') ) & `touse'
		
		label define `var'_TUA -1 "outlier (-6)"   ///
								 0 "Non outlier"           ///
								 1 "outlier(+6)", replace
		label values `var'_TUA `var'_TUA
		return scalar L_ADJ_MT= (`P25' - 6*(`P50'-`P25') )
		return scalar U_ADJ_MT = (`P75' + 6*(`P75'-`P50') )
	}

	***output
	if `StataUsedVersion'<=16{
		di _newline(2)
		label var `var'_TUA "AJUSTED TUKEY METHOD"
		di in smcl "{bf: ****   AJUSTED TUKEY METHOD   **** }"
		table `var'_TUA, c(n `var' mean `var' sd `var' min `var' max `var') row
		di "Outliers flag variable has been genereted"
	}
	
	if `StataUsedVersion'>=17{
	}

}

/*******************************************  ADJUSTED BOXPLOT  **************************************************************/
if "`boxajust'"!="" {

di " ADJUSTED BOXPLOT METHOD not avaible"
}

/*******************************************  THE Median Absolute Deviation (MAD) METHOD   **************************************************************/


if "`made'"!="" |  "`all'"!=""{

	qui {	
		tempvar madboti 
		ge `madboti'=abs(`var'-`P50') if  `touse'
		su `madboti' if  `touse',de 
		local madboti2=1.483*r(p50)
			
		local Imad1=`P50'-2*`madboti2'
		local Imad2=`P50'+2*`madboti2'
		local Omad1=`P50'-3*`madboti2'
		local Omad2=`P50'+3*`madboti2'
		capture drop `var'_MAD
		
		gen byte `var'_MAD = 0 if `var'>=`Imad1' & `var'<=`Imad2' & `touse'
		replace `var'_MAD = -1 if `var'<`Imad1' & `var'>`Omad1' & `touse'
		replace `var'_MAD = -2 if `var'<=`Omad1' & `touse'
		replace `var'_MAD = 1 if `var'>`Imad2' & `var'<`Omad2' & `touse'
		replace `var'_MAD = 2 if `var'>=`Omad2' & !mi(`var') & `touse'
		
		label define `var'_MAD  -2 "Probable outlier (-3)"   ///
								-1 "Possible  outlier (-2)"     ///
								 0 "Non outlier"                      ///
								 1 "Possible  outlier (+2)"      ///
								 2 "Probable  outlier (+3)", replace
		label values `var'_MAD `var'_MAD
	}
	
	if `StataUsedVersion'<=16{
		***output
		di _newline(2)
		label var `var'_MAD "THE Median Absolute Deviation (MAD) METHOD"
		di in smcl "{bf: ****    THE Median Absolute Deviation (MAD) METHOD   **** }"
		table `var'_MAD, c(n `var' mean `var' sd `var' min `var' max `var') row
		di "Outliers flag variable has been genereted"
	}
	
	if `StataUsedVersion'>=17{
	}
	
}

/*******************************************  THE  MEDIAN RULE   **************************************************************/
if "`medrule'"!="" |  "`all'"!=""{

	qui {
		local MEDF1 = `P50' - 2.3*`IQR' 
		local MEDF2 = `P50' + 2.3*`IQR' 
	 
		capture drop `var'_MED /*Nom de variable_Tukey Methode traditionnelle*/
		gen byte `var'_MED = 0 if `var'>=`MEDF1' & `var'<=`MEDF2' & `touse'
		replace `var'_MED = -1 if `var'<`MEDF1'  & `touse'
		replace `var'_MED = 1 if  `var'>`MEDF2'& !mi(`var') & `touse'
		
		label define `var'_MED 	 0 "Non outlier"    ///
								-1 "Outlier (-2.3)" ///
								 1 "Outlier (+2.3)", replace
		label values `var'_MED `var'_MED
		return scalar L_MED = `P50' - 2.3*`IQR'
		return scalar U_MED = `P50' + 2.3*`IQR'	
	}
	
	***output
	if `StataUsedVersion'<=16{
	di _newline(2)
	label var `var'_MED "MEDIAN RULE "
	di in smcl "{bf: ****    MEDIAN RULE    **** }"
	table `var'_MED, c(n `var' mean `var' sd `var' min `var' max `var') row
	di "Outliers flag variable has been genereted"	
	}
	if `StataUsedVersion'>=17{
	}
}





}


end


