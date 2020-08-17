*--------------------------------------------------
* Pro-social behavior and climate change
* Code to replicate the results reported in the manuscript and SOM 
* code_prosocial_behavior&CC.do
* Last changes: 2020-07-14
* Ivo Steimanis, Philipps University Marburg
*--------------------------------------------------


*--------------------------------------------------
* Program Setup
*--------------------------------------------------
version 16              // Set Version number for backward compatibility
set more off            
clear all               
set linesize 80         
macro drop _all        
set matsize 2000
set scheme my538w
* -------------------------------------------------

*--------------------------------------------------
* Directory
*--------------------------------------------------
* Set your directories:
global workpath ""
global datapath ""
global output ""
* --------------------------------------------------

*--------------------------------------------------
* Description
*--------------------------------------------------
/*
 (1) Fast-onset hazards: Typhoon Haiyan (Philippines)
	- Figures 1 and 3
	- Analysis for supplementary materials
 (2) Slow-onset hazards: Sea-level rise (SI, BD, VN)
	- Figures 1 to 4
	- Analysis for supplementary materials
 (3) Repeated Trust Game (Solomon Islands)
	- Analysis for supplementary materials
*/
*--------------------------------------------------











*------------------------------------------------------------------
* (1) Fast-onset hazards: Typhoon Haiyan (Philippines)
*------------------------------------------------------------------
* Load dataset with Philippines data:
use "$datapath\Haiyan_data.dta", replace
* Drop all observations of participants who could not remember positive (T1) or negative (T2) events
drop if primed==0



// Affectedness by HAIYAN
sum h6need house_dmg damages_house_ppp damages_haiyan_ppp house_dmg
* 93% of participants were in need of aid because of Haiyan and 83% of houses were damaged
* The estimated costs of the damages to their houses was $865+-1125(SD) PPP adjusted

* TIME TO PREPARE
sum h1hours 
* Median 2 hours, 90% of participants had no more than 10 hours to prepare themselves
tab h1where 
* 76% of participants were at their homes when they were hit by Haiyan and only 11% found shelter in evacuation centers
sum h1prep h1rein h1prop h1food h1medi 
* 90% stated to have taken some kind of preperation measures, such as storing additional food (70%), reinforcing their houses (61%) or stacked up on medical supplies (29%)


*----------------------------------------------------------------------------------
* Fig 1.	Effects on pro-social behavior across samples and potential mechanisms
*----------------------------------------------------------------------------------
global socio female married age only_elementary people_hh income_hh_ppp100

*Panel A: Average experimental treatment effect
reg z_prosocial_primed_anonymous z_prosocial_baseline_anonymous treated $socio, vce(robust)
est store sotp_main
testparm $socio
local F1 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1') adec(2) dec(2) word replace

* Panel B: Interaction treatment and self-reported affectedness by Haiyan
reg transfer_primed_person2 transfer_person2 treated affected treated_affected $socio, vce(robust)

reg z_prosocial_primed_anonymous z_prosocial_baseline_anonymous treated affected treated_affected $socio, vce(robust)
est store sotp_affected
testparm $socio
local F1 = r(p)
test (treated+affected+treated_affected)=0
local F2 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1', "F-test: Interaction", `F2') adec(2) dec(2) word append


*------------------------------------------------------------
* Fig 3.	Differences in transfers between in- & out-group
*------------------------------------------------------------
* Panel A: Philippines
cibar difference_primed , over1(treated) over2(affected) barcolor(538t%70 538m%70) barlabel(on) blfmt(%5.1f) blpos(11) blgap(0.2) graphopts(xsize(3.465) ysize(2)  legend(ring(0) pos(12) rows(1))  yla(-15(5)15, nogrid) xla(, nogrid) title("{bf:a} Philippines: Differences in solidarity transfers", size(7pt)) ytitle("Mean difference (friend-stranger)", size(6pt))) ciopts(lcolor(black) lpattern(dash))
gr save "$output\discrimination_a.gph", replace
*no significant difference between treatments, however respondents discriminate between friends and strangers from their community

*significance tests
ttest difference_primed==0
ttest difference_primed==0 if treated==0
ttest difference_primed==0 if treated==1
ttest difference_primed, by(treated)
ttest difference_primed, by(affected)
ttest difference_primed if affected==0, by(treated)
* p=0.37
ttest difference_primed if affected==1, by(treated)
*p=0.61




*----------------------------
* SUPPLEMENTARY MATERIALS
*----------------------------
// Table S4.	Typhoon Haiyan sample: Summary statistics 
global overview transfer_person2 transfer_primed_person2 female married age only_elementary people_hh income_hh_ppp eyedis h1hours h6need house_dmg damages_haiyan_ppp

estpost tabstat $overview, statistics(count mean sd min max) columns(statistics)
esttab . using TableS5.rtf, cells("count(fmt(0)) mean(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(0)) max(fmt(0))")  not nostar unstack nomtitle nonumber nonote label replace
 

// Table S6.	Activating memories of  Haiyan treatment balance
global balance female married age only_elementary people_hh income_hh_ppp eyedis h1hours h6need house_dmg damages_haiyan_ppp

iebaltab $balance, grpvar(priming) rowvarlabels format(%9.2f) ///
	  stdev ftest fmissok tblnonote save("$output\table_S7.xlsx") replace

	  
// Figure S6.	Activating positive and negative memories of Haiyan
reg z_prosocial_primed_anonymous z_prosocial_baseline_anonymous t2 t3 $socio, vce(robust)
est store detailed_treatment
test t2=t3

coefplot (detailed_treatment),  keep(t2 t3)  coeflabels(t2 = "T1: Positive (n=124)" t3 = "T2: Negative (n=89)") xline(0, lpattern(dash) lcolor(gs3)) title("", size(med))  xtitle("Effect size in standard deviations relative to control group", size(6pt)) msymbol(d) msize(4pt) xla(-0.75(0.25)0.75, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) mlabel(string(@b, "%5.2f")) mlabsize(7pt)  mlabposition(12) mlabgap(0.3)  legend(order(1 "95% CI" 2 "90% CI" ) pos(12) ring(1) rows(1) size(6pt) bmargin(small)) xsize(3.465) ysize(2)
gr save "$output\detailed_treatment_haiyan.gph", replace
gr export "$output\02_supplementary-materials\detailed_treatment_haiyan.tif", replace width(3465)
gr export "$output\02_supplementary-materials\detailed_treatment_haiyan.svg", replace


// Table S11.	Difference between in- and out-group giving
reg difference_primed difference_baseline treated affected treated_affected $socio, vce(robust)
outreg2 using "$output\02_supplementary-materials\tableS10_out_group_analysis", drop() dec(2) word  replace


// Robustness checks with full sample
* Reload dataset with Philippines data
use "$datapath\Haiyan_data.dta", replace

// How many participants did not pay attention to the information videos?
bysort priming: tab primed
* OVERALL 39 participants could not remember either positive (n=2) or negative (n=37) events
* 1.6% in the positive treatment and 30% in the negative treatment

tab primed priming if priming>1, chi2 exact
* participants in the negative treatment found it significantly harder to remember any negative events that happened in the aftermath of Haiyan
* We drop all observations from participants who did not pay attention for the main analysis
* We do robustness checks with the full dataset

* Table S9.	Main treatment effects & robustness checks
global socio female married age only_elementary people_hh income_hh_ppp100


*Average experimental treatment effect (column 3)
reg z_prosocial_primed_anonymous z_prosocial_baseline_anonymous treated $socio, vce(robust)
est store sotp_main
testparm $socio
local F1 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1') adec(2) dec(2) word append


* Interaction: treatment and self-reported affectedness by Haiyan (column 4)
reg z_prosocial_primed_anonymous z_prosocial_baseline_anonymous treated affected treated_affected $socio, vce(robust)
est store sotp_affected
testparm $socio
local F1 = r(p)
test (treated+affected+treated_affected)=0
local F2 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1', "F-test: Interaction", `F2') adec(2) dec(2) word append










*----------------------------------------------------
* (2) Slow-onset hazards: Sea-level rise (SI, BD, VN)
*----------------------------------------------------
// Load dataset with pooled SLR data:
use "$datapath\SLR_pooled_data.dta", replace
* Drop observations of participants who did not pay attention to the treatment videos
drop if not_primed==1

// SLR affectedness descriptives
ttest relocation, by(affected)
tab rel_certainty country_id
ttest pc_livelihood, by(country_id)
ttest pc_relocate, by(country_id)


ttest cc_fp==cc_pp
ttest fp_slr==pp_slr
ttest fp_slr, by(affected)
ttest pp_slr, by(affected)
ranksum cc_fp, by(affected)
ranksum pp_slr, by(affected)
ranksum fp_slr, by(affected)
signrank cc_pp=cc_fp

* Adaptation actions
sum sealevel1 sealevel2 sealevel3 sealevel4 sealevel5 sealevel6 sealevel7
tab adapt1
tab strat1 affected, chi2 exact column
tab strat2 affected, chi2 exact column
tab strat3 affected, chi2 exact column
tab strat4 affected, chi2 exact column
*more exposed participants are less likely to consider only migration as a way to adapt
tab strat3 affected if country_id==1, chi2 exact column
tab strat2 affected if country_id==1, chi2 exact column


*----------------------------------------------------------------------------------
* Fig 1.	Effects on pro-social behavior across samples and potential mechanisms
*----------------------------------------------------------------------------------
// set globals for control variables
global socio female married age edu people_hh income_hh_ppp100
global imbalance z_survey_time z_survey_trust

* Panel A: Main treatment effects
reg z_prosocial treated fp_slr $socio $imbalance i.country_id, vce(robust)
est store sotf_main
testparm $socio
local F1 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1') adec(2) dec(2) word append

*graph main treatment affected
coefplot (sotp_main,) (sotf_main,) , keep(treated)  coeflabels(treated = "Treatment: Awareness of hazard" ) xline(0, lpattern(dash) lcolor(gs3)) title("{bf:a} Treatment effects: Haiyan & SLR", size(8pt)) xtitle("Effect size in standard deviations relative to control group", size(6pt)) msymbol(d)  xla(-0.75(0.25)0.75, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) mlabel(string(@b, "%5.2f")) mlabsize(6pt)  mlabposition(12) mlabsize(6pt) mlabgap(0.3) legend(order(3 "Philippines: Typhoon Haiyan (n=336)" 6 "Pooled: SLR impacts (n=863)") pos(6) ring(1) rows(1) size(6pt) bmargin(small)) xsize(3.465) ysize(2)
gr save "$output\main_treatment_effects_a.gph", replace


* Panel B: Interaction effects with affectedness
reg z_prosocial treated affected treated_affected fp_slr $socio $imbalance i.country_id, vce(robust)
est store sotf_affected
testparm $socio
local F1 = r(p)
test (treated+affected+treated_affected)=0
local F2 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1', "F-test: Interaction", `F2') adec(2) dec(2) word append

*Graph interaction effects
coefplot (sotp_affected,) (sotf_affected,) , keep(treated affected treated_affected)  coeflabels(treated = "Treatment: Awareness of hazard" affected = "Self-reported: More affected" treated_affected = "Interaction: Treatment*Affected") xline(0, lpattern(dash) lcolor(gs3)) title("{bf:b}  Heterogeneous treatment effects by affectedness", size(8pt)) xtitle("Effect size in standard deviations relative to control group", size(6pt)) msymbol(d)  xla(-0.75(0.25)0.75, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) mlabel(string(@b, "%5.2f")) mlabsize(6pt)  mlabposition(12) mlabsize(6pt) mlabgap(0.3) legend(order(3 "Philippines: Typhoon Haiyan (n=336)" 6 "Pooled: SLR impacts (n=863)") pos(6) ring(1) rows(1) size(6pt) bmargin(small)) xsize(3.465) ysize(2)
gr save "$output\main_treatment_effects_b.gph", replace

grc1leg "$output\main_treatment_effects_a.gph" "$output\main_treatment_effects_b.gph", legendfrom("$output\main_treatment_effects_b.gph") rows(2) pos(6) ring(1) xsize(3.465) ysize(4)
gr save "$output\figure1_main_treatment_effects.gph", replace
gr export "$output\01_main-manuscript\figure1_main_treatment_effects.tif", replace width(3465)
gr export "$output\01_main-manuscript\figure1_main_treatment_effects.svg", replace


*Collective action (y/n) in the past year
probit collective_action z_prosocial affected i.country_id, vce(robust)
margins, dydx(*)


*-----------------------------------------------------------------------------------------
* Fig 2.	Bangladesh: Negative emotions  dampen the treatment effect of affected people
*-----------------------------------------------------------------------------------------
*PANEL A: EMOTIONS by treatment & affectedness
global NA p9_afraid p6_nervous p4_ashamed p3_alert p2_hostile p1_upset p13_helpless p12_sad p11_hopeless
bysort treated: sum $NA if country_id==2

preserve
rename NA_bd emo1 
rename PA_bd emo2

reshape long emo, i(id) j(emotions_lab)
lab def emo_lab1 1 "Negative emotions" 2 "Positive emotions", replace 
lab val emotions_lab emo_lab1
cibar emo, over1(treated) over2(affected) over3(emotions_lab) gap(60) barcolor(538t%70 538m%70) barlabel(on) blfmt(%5.1f) blpos(12) blsize(6pt) blgap(0.28) graphopts(xsize(3.465) ysize(2)  legend(ring (0) pos(12) rows(1))  yla(1(1)5, nogrid) xla(, nogrid) title("{bf:a} Negative emotions by treatment & affectedness", size(8pt)) ytitle("mean", )) ciopts(lcolor(black) lpattern(dash))
gr save "$output\figure2_emotions_a.gph", replace
restore

// Non-parametric tests
* Average effects
ttest NA_bd, by(treated)
ranksum NA_bd, by(treated)
ttest PA_bd, by(treated)
ranksum PA_bd, by(treated)
*by affectedness
ranksum NA_bd if affected==0, by(treated)
ranksum NA_bd if affected==1, by(treated)
ranksum PA_bd if affected==0, by(treated)
ranksum PA_bd if affected==1, by(treated)
* Do more and less affected participants react differently?
ranksum NA_bd if treated==0, by(affected)
ttest NA_bd if treated==1, by(affected)
ranksum NA_bd if treated==1, by(affected)
ranksum PA_bd if treated==0, by(affected)
ranksum PA_bd if treated==1, by(affected)


*PANEL B: Causal mediation analysis of negative emotions
global x_1 fp_slr female married age edu people_hh income_hh_ppp100

*pooled
medeff (regress NA_bd treated $x_1) (regress z_prosocial treated NA_bd $x_1), seed(12345) vce(robust) treat(treated) mediate(NA_bd) sims(1000)
mat a1 = r(delta0)
mat b1 = r(zeta0)
mat c1 = r(tau)
mat a2 = r(delta0 lo)
mat b2 = r(zeta0 lo)
mat c2 = r(tau lo)
mat a3 = r(delta0 hi)
mat b3 = r(zeta0 hi)
mat c3 = r(tau hi)
matrix med_pooled = a1,a2,a3\b1,b2,b3\c1,c2,c3
matrix coln med_pooled = mean ll95 ul95
matrix rown med_pooled = ACME ADE ATE

*more affected
medeff (regress NA_bd treated  $x_1) (regress z_prosocial treated NA_bd $x_1) if affected==1 , seed(12345) vce(robust) treat(treated) mediate(NA_bd) sims(1000)
mat a1 = r(delta0)
mat b1 = r(zeta0)
mat c1 = r(tau)
mat a2 = r(delta0 lo)
mat b2 = r(zeta0 lo)
mat c2 = r(tau lo)
mat a3 = r(delta0 hi)
mat b3 = r(zeta0 hi)
mat c3 = r(tau hi)
matrix med_more = a1,a2,a3\b1,b2,b3\c1,c2,c3
matrix coln med_more = mean ll95 ul95
matrix rown med_more = ACME ADE ATE

*less affected
medeff (regress NA_bd treated  $x_1) (regress z_prosocial treated NA_bd $x_1) if affected==0, seed(12345) vce(robust) treat(treated) mediate(NA_bd) sims(1000)
mat a1 = r(delta0)
mat b1 = r(zeta0)
mat c1 = r(tau)
mat a2 = r(delta0 lo)
mat b2 = r(zeta0 lo)
mat c2 = r(tau lo)
mat a3 = r(delta0 hi)
mat b3 = r(zeta0 hi)
mat c3 = r(tau hi)
matrix med_less = a1,a2,a3\b1,b2,b3\c1,c2,c3
matrix coln med_less = mean ll95 ul95
matrix rown med_less = ACME ADE ATE

*Visualizing the effects
coefplot (matrix(med_pooled[.,1]), offset(0.3) ci((med_pooled[.,2] med_pooled[.,3])))  (matrix(med_more[.,1]), offset(0) ci((med_more[.,2] med_more[.,3]))) (matrix(med_less[.,1]),offset(-0.3) ci((med_less[.,2] med_less[.,3]))),xline(0, lpattern(dash) lcolor(gs3)) title("{bf:b} Causal mediation analysis", size(10pt)) xtitle("Effect size in standard deviations of control group", size(7pt)) msymbol(d) msize(5pt) xla(-0.75(0.25)0.75, nogrid) grid(none) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) mlabel(string(@b, "%5.2f")) mlabposition(12) mlabsize(8pt) mlabgap(0.4) legend(order(2 "Pooled (n=202)" 4 "More affected (n=108)" 6 "Less affected (n=94)") pos(6) ring(1) rows(2) rowgap(tiny) size(8pt) bmargin(small)) xsize(3.465) ysize(2)
gr save "$output\figure2_mediation_b.gph", replace

gr combine "$output\figure2_emotions_a.gph" "$output\figure2_mediation_b.gph", rows(2)  xsize(3.465) ysize(4)
gr save "$output\figure2_bd_emotions.gph", replace
gr export "$output\01_main-manuscript\figure2_bd_emotions.tif", replace width(3465)
gr export "$output\01_main-manuscript\figure2_bd_emotions.svg", replace


*------------------------------------------------------------
* Fig 3.	Differences in transfers between in- & out-group
*------------------------------------------------------------
* T-tests
ttest difference_primed=0 if affected==0
ttest difference_primed=0 if affected==1
ttest difference_primed if affected==0, by(treated)
*p=0.48
ttest difference_primed if affected==1, by(treated)
*p=0,72

cibar difference_primed if country_id==1, over1(treated) over2(sample) barcolor(538t%60 538m%60) barlabel(on) blfmt(%5.1f) blpos(11) blgap(0.2) graphopts(xsize(3.465) ysize(2)  legend(ring(0) pos(12) rows(1))  yla(-15(5)15, nogrid) xla(, nogrid) title("{bf:b} Solomon Islands: Differences in SVO angles", size(7pt)) ytitle("Mean difference (in-group - out-group)", size(6pt))) ciopts(lcolor(black) lpattern(dash))
gr save "$output\discrimination_b.gph", 

gr combine "$output\discrimination_a.gph" "$output\discrimination_b.gph", rows(1) xsize(3.465) ysize(2) scale(1.1)
gr save "$output\SLR_discrimination.gph", replace
gr export "$output\01_main-manuscript\figure3_SLR_discrimination.tif", replace width(3465)
gr export "$output\01_main-manuscript\figure3_SLR_discrimination.svg", replace


*----------------------------------------------
* Fig 4.	Repeated interactions & impatience
*----------------------------------------------
// Panel A: Repeated trust game results
clear all
* Load dataset with trust game data from Solomon Islands
use "$datapath\trust_game_data.dta", replace

* Average treatment effect
global socio female married age edu people_hh income_hh
global session f1a_friends f1b_relatives f1c_conflict


*Panel A: Treatment effects: repeated vs one-shot
bysort position repeated: sum tg_1 tg_mean
bysort position : sum tg_1

cibar tg_1 , over1(repeated) over2(position) over3(location)  gap(50) barcolor(538b%70 538r%70) barlabel(on) blfmt(%5.2f) blpos(12) blsize(8pt) graphopts(xsize(3.465) ysize(2) legend(rows(1) ring(0) pos(12))  yla(0(0.2)1, nogrid) xla(, nogrid) title("{bf:a:} Trust game treatment effects", size(10pt))  ytitle("Share of trust*worthiness", size(8pt))) ciopts(lcolor(gs5) lpattern(dash))
graph save "$output\figure4_a", replace
* Tests
tab tg_1 repeated if position==1, chi2 exact
tab tg_1 repeated if position==2, chi2 exact
tab tg_1 repeated if position==1 & location==1, chi2 exact
tab tg_1 repeated if position==2 & location==1, chi2 exact
tab tg_1 repeated if position==1 & location==3, chi2 exact
tab tg_1 repeated if position==2 & location==3, chi2 exact


* Reload dataset with pooled SLR data with full sample for patience measurement
clear all
use "$datapath\SLR_pooled_data.dta", replace

// Panel B: TIME PREFERENCE INDEX
* how many participants are totally impatient in the quantitative measure
tab survey_time if country_id==1
*67%, annual discount rate is left-censored at 114% for these participants

local start = -1.5
local bin = 0.15 
local opts start(`start') width(`bin') 
local normal_begin = -1.5 
local normal_end = 2.5 
local range ra(`normal_begin' `normal_end') 
hist patience, percent kdensity kdenopts(lpattern(dash) lcolor(538r)) normal `opts' bcolor(538b%30) normopts(lcolor(538b) lpattern(longdash)) title("{bf:b:} Time preference index", size(10pt)) xtitle("Patience (z-score)", size(8pt)) ytitle("Percent", size(8pt)) xla(`normal_begin'(0.5)`normal_end', nogrid) xsize(3.465) ysize(2)
gr save "$output\figure4_b.gph", replace

gr combine "$output\figure4_a" "$output\figure4_b", xsize(3.465) ysize(4) rows(2) scale(1.5)
gr save "$output\figure4_future_orientation.gph", replace
gr export "$output\01_main-manuscript\figure4_future_orientation.tif", replace width(3465)
gr export "$output\01_main-manuscript\figure4_future_orientation.svg", replace 




*-------------------------
* SUPPLEMENTARY MATERIALS
*-------------------------
* Reload dataset with pooled SLR data
use "$datapath\SLR_pooled_data.dta", replace
drop if not_primed==1

// SUPPLEMENTARY TABLES
* Table S5.	SLR samples: Solomon Islands, Bangladesh & Vietnam
global overview svo_in_r2 dg_percent strat1 strat2 strat3 strat4 collective_action rel_assistance /// experimental
		female married age edu people_hh income_ppp income_hh_ppp /// socio
		survey_time survey_trust place_attachment NA_bd PA_bd  /// survey outcomes
		fp_slr pp_slr pc_livelihood pc_relocate relocation number_extremes land_lost /*perceived risk exposure*/

estpost tabstat $overview, statistics(count mean sd min max) columns(statistics)
esttab . using TableS4_summary_statistics.rtf, cells("count(fmt(0)) mean(fmt(%9.2fc)) sd(fmt(%9.2fc)) min(fmt(0)) max(fmt(0))")  not nostar unstack nomtitle nonumber nonote label replace


* Table S7.	Video information treatment balance
global balance female married age edu people_hh income_ppp income_hh_ppp survey_time survey_trust place_attachment

iebaltab $balance, grpvar(treated) rowvarlabels format(%9.2f) ///
	  stdev ftest fmissok tblnonote save("$output\tableS&_balancing.xlsx") replace

reg treated $balance
* trust, household size and perceptions of SLR impacts are different between control and treatment
* does this potentially bias our results? correlation between these variables and outcomes of interest
pwcorr z_prosocial svo_in_r2 dg_percent z_time z_trust z_income age, sig
* not the case for pro-social behavior results


* Table S10.	Bangladesh: Mediation results and robustness check
* Save regresion outputs from mediation analysis (IMAI & ACHARYA)
* Acharya et al (2016) method: Step 1: regress Y on the mediator, treatment, a set of pretreatment and post-treatment confounders --> generate the predicted value of the outcome fixing all mediators to zero = "demediated" outcome
global x_1 fp_slr female married age edu people_hh income_hh_ppp100

*Pooled
medeff (regress NA_bd treated $x_1) (regress z_prosocial treated NA_bd $x_1), seed(12345) vce(robust) treat(treated) mediate(NA_bd) sims(1000)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  replace

reg z_prosocial NA_bd treated $x_1, vce(robust)
gen ytilde1 = z_prosocial - _b[NA_bd] * NA_bd
reg ytilde1 treated $x_1, vce(robust)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  append

*more affected
medeff (regress NA_bd treated $x_1) (regress z_prosocial treated NA_bd $x_1) if affected==1 , seed(12345) vce(robust) treat(treated) mediate(NA_bd) sims(1000)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  append

reg z_prosocial NA_bd treated $x_1 if affected==1, vce(robust)
gen ytilde2 = z_prosocial - _b[NA_bd] * NA_bd
reg ytilde2 treated $x_1 if affected==1, vce(robust)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  append

*less affected
medeff (regress NA_bd treated $x_1) (regress z_prosocial treated NA_bd $x_1) if affected==0, seed(12345) vce(robust) treat(treated) mediate(NA_bd) sims(1000)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  append

reg z_prosocial NA_bd treated $x_1 if affected==0, vce(robust)
gen ytilde3 = z_prosocial - _b[NA_bd] * NA_bd
reg ytilde3 treated $x_1 if affected==0, vce(robust)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  append


* Table S11.	Difference between in- and out-group giving
reg difference_primed difference_baseline treated affected treated_affected fp_slr $socio ,  vce(cluster session_ID)
outreg2 using "$output\02_supplementary-materials\tableS9_causal_mediation_analysis", drop() dec(2) word  append


// SUPPLEMENTARY FIGURES
* Figure S4.	Climate change impact appraisal
preserve
rename cc_pp appraisal1 
rename cc_fp appraisal2
rename pp_slr appraisal3 
rename fp_slr appraisal4 

reshape long appraisal, i(id) j(appraisal_lab)
lab def appraisal_lab1 1 "Past: CC" 2 "Future: CC" 3 "Past: SLR" 4 "Future: SLR", replace 
lab val appraisal_lab appraisal_lab1
cibar appraisal , over1(affected) over2(appraisal_lab) gap(60) barcolor(538t%70 538m%70) barlabel(on) blfmt(%5.1f) blpos(12) blsize(6pt) blgap(0.2) graphopts(xsize(3.465) ysize(2) legend(ring (0) pos(12) rows(1))  yla(1(1)5, nogrid) xla(, nogrid) ytitle("mean", )) ciopts(lcolor(black) lpattern(dash))
gr save "$output\figureS4_impact_appraisal.gph", replace
gr export "$output\02_supplementary-materials\figureS4_impact_appraisal.tif", replace width(3465)
gr export "$output\02_supplementary-materials\figureS4_impact_appraisal.svg", replace
restore

* significance tests
ranksum cc_pp, by(affected)
ttest cc_pp, by(affected)
ranksum cc_fp, by(affected)
ttest cc_fp, by(affected)
ranksum pp_slr, by(affected)
ttest pp_slr, by(affected)
ranksum fp_slr, by(affected)
ttest fp_slr, by(affected)


* Figure S5.	Recommended adaptation strategies
*Outside help
tab1 h1 h2 h3 h4 h5 h6 h7

tab adapt1 affected, chi2
mylabels 0(20)100, myscale(@) local(pctlabel) suffix("%") 
catplot adapt1, over(affected, label(labsize(6pt))) over(country_id) asyvar stack percent(country_id affected)  yla(`pctlabel', nogrid) bar(1, bcolor(538r)) bar(2, bcolor(538y))  bar(3, bcolor(538b)) bar(4, bcolor(538g))  blabel(bar, format(%9.0f) size(7pt) pos(center))  ytitle(percent, size(7pt)) l1title("")  b1title("") legend(rows(2) size(7pt)) xsize(3.465) ysize(2)
gr save  "$output\figureS5_adaptation_strategies.gph", replace
gr export "$output\01_main-manuscript\figureS5_adaptation_strategies.tif", replace width(3465)
gr export "$output\01_main-manuscript\figureS5_adaptation_strategies.svg", replace 


* Figure S7.	SLR treatment effects by sample
global socio female married age edu people_hh income_hh_ppp100
global imbalance z_survey_time z_survey_trust

*Solomon Islands
reg z_prosocial treated affected treated_affected fp_slr $socio if country_id==1, vce(cluster session_ID)
est store si
test (treated+affected+treated_affected)=0
*Bangladesh
reg z_prosocial treated affected treated_affected fp_slr $socio if country_id==2, vce(robust)
est store bd
test (treated+affected+treated_affected)=0
*Vietnam
reg z_prosocial treated affected treated_affected fp_slr $socio if country_id==3, vce(robust)
est store vn
test (treated+affected+treated_affected)=0

coefplot (si, offset(0.25)) (bd, offset(0)) (vn, offset(-0.25)), keep(treated affected treated_affected)  coeflabels(treated = "Treatment: SLR information" affected = "Self-reported: More affected" treated_affected = "Interaction: Treatment*Affected") xline(0, lpattern(dash) lcolor(gs3)) xtitle("Effect size in standard deviations relative to control group", size(6pt)) msymbol(d)  xla(-0.75(0.25)0.75, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) mlabsize(6pt)  mlabel(string(@b, "%5.2f")) mlabposition(12) mlabsize(6pt) mlabgap(0.3) legend(order(3 "Solomon Islands (n=350)" 6 "Bangladesh (n=202)" 9 "Vietnam (n=313)") pos(6) ring(1) rows(1) size(6pt)  bmargin(small)) xsize(3.465) ysize(2)
gr save "$output\figureS7_SLR_effects_by_sample.gph", replace
gr export "$output\02_supplementary-materials\figureS7_SLR_effects_by_sample.tif", replace width(3465)
gr export "$output\02_supplementary-materials\figureS7_SLR_effects_by_sample.svg", replace


* Figure S8.	Assistance in case of relocation by treatment
* Relocation asistance beliefs by treatment
lab def vn_t 0 "Control" 1 "T1: Community" 2 "T2: Individual", replace
lab val treatment_vn vn_t
vioplot rel_assistance , over(treatment_vn, nolab)  vertical median(msymbol(D) mcolor(white)) density(color(538t)) yla(1(1)5, nogrid) xla(, nogrid) obs title("{bf:a:} Distribution", size(large)) ytitle("Likelihood", size(medium)) xsize(3.465) ysize(2)
gr save "$output\figureS8_a.gph", replace

* Mean treatment effect
cibar rel_assistance if country_id > 1, over1(treatment_vn) bargap(20) barcolor(538t%70 538m%70 538y%70) barlabel(on) blfmt(%5.1f) blpos(12) blgap(0.3) graphopts(xsize(3.465) ysize(2) legend(rows(1) ring(0) pos(12))  yla(1(1)5, nogrid) xla(, nogrid) title("{bf:b:} Average", size(large))  ytitle("mean", size(medium))) ciopts(lcolor(black) lpattern(dash))
gr save "$output\figureS8_b.gph", replace

grc1leg "$output\figureS8_a" "$output\figureS8_b", legendfrom("$output\figureS8_b") pos(6) rows(1) ring(1) xsize(3.465) ysize(4)
gr save "$output\figureS8_relocation_beliefs.gph", replace
gr export "$output\02_supplementary-materials\figureS8_relocation_beliefs.tif", replace width(3465)
gr export "$output\02_supplementary-materials\figureS8_relocation_beliefs.svg", replace 

ranksum rel_assistance if treatment_vn!=2, by(treatment_vn)
*not significantly different from control group
ranksum rel_assistance if treatment_vn!=1, by(treatment_vn)
* significantly lower than control group, p=0.044
ranksum rel_assistance if treatment_vn!=0, by(treatment_vn)
* significantly lower than community treatment, p=0.0033


* Figure S9.	Community vs individual resettlement treatment effects
tab treatment_vn, gen (t)
gen t2_affected = t2*affected
gen t3_affected = t3*affected

reg z_prosocial t2 t3 affected t2_affected t3_affected fp_slr $socio,  vce(robust)
est sto vn_dg

coefplot (vn_dg,) , keep(t2 t3 affected t2_affected t3_affected)  coeflabels(t2 = "T1: Community" t3 = "T2: Individual" affected = "More affected"  t2_affected = "T1*More affected"  t3_affected = "T2*More affected") xline(0, lpattern(dash) lcolor(gs3)) title("", size(med))  xtitle("Effect size in standard deviations relative to control group", size(7pt)) msymbol(d) msize(6pt) xla(-1(0.25)1.5, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.5 1.2) lcolor(*.8 *.2) recast(rcap)) mlabel(string(@b, "%5.2f")) mlabsize(7pt)  mlabposition(12) mlabgap(0.3) legend(order(1 "95% CI" 2 "90% CI") pos(1) ring(0) rows(1) size(7pt) bmargin(small)) xsize(3.465) ysize(2)
gr save "$output\figureS9_treatment_effects_VN.gph", replace
gr export "$output\02_supplementary-materials\figureS9_treatment_effects_VN.tif", replace width(3465)
gr export "$output\02_supplementary-materials\figureS9_treatment_effects_VN.svg", replace 


* Figure S10.	Sensitivity analysis of ACME
medsens (regress NA_bd treated $x_1) (regress z_prosocial treated NA_bd $x_1), treat(treated) mediate(NA_bd) sims(2000)

graph twoway rarea _med_updelta0 _med_lodelta0 _med_rho, bcolor(gs14) yline(0.19, lpattern(dash)) || line _med_delta0 _med_rho, lcolor(black) yla(,nogrid) ytitle("ACME") xline(0) yline(0) xtitle("Sensitivity parameter: {&rho}") legend(off) scheme(sj)  xscale(r(-1(0.25)1)) 
gr save "$output\figureS10_sensitivity_analysis.gph", replace
gr export "$output\02_supplementary-materials\figureS10_sensitivity_analysis.tif", replace width(3465)
gr export "$output\02_supplementary-materials\figureS10_sensitivity_analysis.svg", replace 
*results sensitive to unobsorved confounders
* for rho < -0.16 the ACME goes away



// ROBUSTNESS CHECK
clear all	  
use "$datapath\SLR_pooled_data.dta", replace

// How many participants did not pay attention to the information videos?
bysort country_id: tab not_primed if treated==1
* OVERALL 72: 21 in SI, 14 in BD, 37 in VN
* Between 11% to 15% of participants in the treatment group were not primed.

tab not_primed country_id if treated==1, chi2 exact
* no significant differences between countries in participants that did not pay attention to
* the information video
* We drop all observations from participants who did not pay attention for the main analysis
* We do robustness checks with the full dataset

*using full-dataset
* Table S9.	Main treatment effects & robustness checks
global socio female married age edu people_hh income_hh_ppp100
global imbalance z_survey_time z_survey_trust

* Main treatment effects (column 7)
reg z_prosocial treated fp_slr $socio $imbalance i.country_id, vce(robust)
est store sotf_main
testparm $socio
local F1 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1') adec(2) dec(2) word append

* Interaction: treatment and self-reported affectedness by Haiyan (column 8)
reg z_prosocial treated affected treated_affected fp_slr $socio $imbalance i.country_id, vce(robust)
est store sotf_affected
testparm $socio
local F1 = r(p)
test (treated+affected+treated_affected)=0
local F2 = r(p)
outreg2 using "$output\02_supplementary-materials\tableS9_main-treatment-effects-with-controls",  drop() addstat("Adjusted R2", e(r2_a), "F-test: Socio-economic", `F1', "F-test: Interaction", `F2') adec(2) dec(2) word append











*------------------------------------------------------------------
* (3) Repeated Trust Game (Solomon Islands)
*------------------------------------------------------------------
clear all	  
use "$datapath\trust_game_data.dta", replace

*-------------------------
* SUPPLEMENTARY MATERIALS
*-------------------------
*Table S8 Trust game: Treatment balance
global balance female married age edu people_hh income_hh f1a_friends f1b_relatives f1c_conflict

iebaltab $balance, grpvar(repeated) rowvarlabels format(%9.2f) ///
	  stdev ftest fmissok tblnonote save("$output\S8_balancing_TG.xlsx") replace
reg repeated $balance

* Figure S11.	Treatment effects by number of relatives in session
bysort location: sum f1a_friends f1b_relatives
egen m_relatives = mean(f1b_relatives), by(session_ID)
gen relatives_session=0
replace relatives_session=1 if m_relatives>=4
lab define in1 0 "less than 4 related" 1 "4 or more related", replace
lab values relatives_session in1

cibar tg_1 if position==1, over1(repeated) over2(relatives_session)  gap(50) barcolor(538b%70 538r%70) barlabel(on) blfmt(%5.2f) blpos(12) blgap(0.15) graphopts(xsize(3.465) ysize(2) legend(rows(1) ring(0) pos(12))  yla(0(0.2)1, nogrid) xla(, nogrid) title("{bf:a:} Trust", size(large))  ytitle("mean", size(medium))) ciopts(lcolor(black) lpattern(dash))
graph save "$output\figureS11_a", replace

tab tg_1 repeated if position==1 & relatives_session==0, chi2 exact column
* Pearson Chi^2(1)=0.57; p=0.45
tab tg_1 repeated if position==1 & relatives_session==1, chi2 exact column
* Pearson Chi^2(1)=0.14; p=0.71
