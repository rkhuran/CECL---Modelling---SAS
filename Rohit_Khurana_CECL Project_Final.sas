/*Import of the mortgage file */
libname BankData '/folders/myfolders/bank_data';
FILENAME REFFILE '/folders/myfolders/bank_data/mortgage.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=BankData.mortgage;
	GETNAMES=YES;
RUN;
/* Sorting and adding two extra variables to the original data as below */
PROC sort DATA=BankData.mortgage;
	by id time;
RUN;

/*Two new calculated variables to the original data
 - Loan Age (current time - origination time) and 
 - HPI change to base of 100  */

data mortgage_mod;
	set bankdata.mortgage;
	loan_age = time-orig_time;
	hpi_change =hpi_time-100;
run;

/*-----------------------Section 1-------------------------------------
Clustering of Mortgage loans based upon FICO score */

data mortgage_superprime mortgage_prime  mortgage_subprime;
	set work.mortgage_mod;
	if FICO_ORIG_TIME >= 740 then output mortgage_superprime;
	else if FICO_ORIG_TIME >= 640 then output mortgage_prime;
	else output mortgage_subprime;
run;

/*-----------------------Section 2---------------------------------
PD Model*/
/*---------------- Section 2.1------------------*/
/* SuperPrime 
Input variables considered are 
1. LTV_Time
2. Loan_Age (this is transformed paramter = current time - origin
3. hpi_change (this has changed to level 100
*/

/* Champion Model - Logistic model, both have same AIC but variables 
had acceptable p-value*/
proc logistic data = work.mortgage_superprime descending;
	model default_time = LTV_time loan_age hpi_change /link=logit;
	output out = logis predprob=(individual);
run;

/* Challenger model - Logistic model with addition REtype_SF_orig_time
Here Single Family Original time coffecint has high value*/
proc logistic data = work.mortgage_superprime descending;
	model default_time = LTV_time loan_age REtype_SF_orig_time hpi_change /link=logit;
run;

/*---------------- Section 2.2------------------*/
/* Prime 
Input variables considered are 
1. LTV_Time
2. interest_rate_time
3. loan_age
3. uer_time (Unemployement has been added to the analysis)
*/

/* Champion Model - Logistic model, both have same AIC but variables 
had acceptable p-value*/
proc logistic data = work.mortgage_prime descending;
	model default_time = LTV_time interest_rate_time loan_age uer_time /link=logit;
	output out = logis predprob=(individual);
run;

/* Challenger model - Logistic model with addition REtype_SF_orig_time
Here Single Family Original time coffecint has high value*/
proc logistic data = work.mortgage_prime descending;
	model default_time = LTV_time interest_rate_time REtype_SF_orig_time uer_time /link=logit;
run;
/*---------------- Section 2.3------------------
SubPrime 
Input variables considered are 
1. LTV_Time
2. interest_rate_time
3. uer_time (Unemployement has been added to the analysis)
*/

/* Champion Model - Logistic model, both have same AIC but variables 
had acceptable p-value*/
proc logistic data = work.mortgage_subprime descending;
	model default_time = LTV_time interest_rate_time uer_time /link=logit;
	output out = logis predprob=(individual);
run;

/* Challenger model - Logistic model with addition REtype_SF_orig_time
Here Single Family Original time coffecint has high value*/
proc logistic data = work.mortgage_subprime descending;
	model default_time = LTV_time interest_rate_time REtype_SF_orig_time uer_time/link=logit;
run;


/*-----------------------Section 3---------------------------------
Build a LGD model using the lgd.csv data file */
/*LGD Data Prep*/
libname bankdata "/folders/myfolders/bank_data";
FILENAME REFFILE '/folders/myfolders/bank_data/lgd.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=bankdata.lgd;
	GETNAMES=YES;
RUN;

data mylgd ;
	set bankdata.lgd;
	y_logistic=log(lgd_time/(1-lgd_time));
	y_probit=probit(lgd_time);
	lnrr=log(1-lgd_time);
run;

/*LGD: Linear Regression with various combinations of the model, this appears 
to be most optimal*/
proc reg data=bankdata.lgd;
	model y_logistic = event purpose1 LTV;
	output out = bankdata.lgd
           p = predicted_value;
run;

/*-----------------------Section 4---------------------------
Build an EAD Model */
/* Data Prep */
proc sort data=work.mortgage_mod;
	by id time;
run;

%let precision=1e-6;
data ead (drop=diff_limit_drawn);
	set work.mortgage_mod;
	by id time;
	array x(*) lag1-lag4;
	/* Define lag variables - time period is quarterly*/
	lag1=LAG1(balance_time);
	lag2=LAG2(balance_time);
	lag3=LAG3(balance_time);
	lag4=LAG4(balance_time);
	limit=balance_orig_time;
	drawn=lag4;
	exposure=balance_time;
	if drawn EQ .
		or limit EQ .
		or exposure EQ .
		or exposure EQ 0 then delete;
		
	/*Cap Exposure bound */
	if exposure > limit then
		exposure=limit;
	if drawn > limit then
		drawn=limit;
	
	/*Credit Conversion Factor - CCF */
	diff_limit_drawn=limit-drawn;
	if (abs(diff_limit_drawn)>&precision) then
		CCF=(exposure-drawn)/diff_limit_drawn;
	else
		CCF=0;

	/*CEQ - credit equivalent */
	if (limit > &precision) then
		CEQ=(exposure-drawn)/limit;
	else
		CEQ=0;
run;

data ead2;
	set ead;
	where drawn NE .
		and limit NE .
		and exposure NE .
		and exposure NE 0;
run;


proc means data=ead2 p1 p99;
	var CCF CEQ;
run;

data ead_default;
	set ead2;
	where default_time=1;
	if CCF <= -18.05 then CCF=-18.05;
	else if CCF >= 0.99 then CCF=0.99;
	if CEQ <= -0.13 then CEQ=-0.13;
	else if CEQ >=0.103 then CEQ=0.103;
run;


/*Check Histogram */
proc univariate data=ead_default;
	var CCF CEQ;
	histogram;
run;

/* CCF Model */
proc reg data=ead_default;
	model CCF=LTV_time;
run;

/* CEQ Model */
proc reg data=ead_default;
	model CEQ=LTV_time;
run;