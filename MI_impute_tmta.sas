/*********************
* File for imputing TMT-A data for Aging
*************************/



/***********set library;**************/
*libname lib1 "E:\";
libname lib1 "D:\";



/***********get data;**************/
*proc import datafile="E:\all_aging_data" out=whas dbms=dta replace;
proc import datafile="D:\all_aging_data" out=whas dbms=dta replace;
run;








/***********check missing;**************/
* only missing for endogenous, death, gds (1); 
proc means data = whas nmiss;
var _ALL_;
run;




/**************************
 MULTIPLE IMPUTATION FOR TRAIL A
*****************************/

%let dsname1 = whas;
%let numdats=1;



%macro impute;

*for each dataset (only 1);
%do j=1 %to &numdats;

* IMPUTE TRAIL A, GDS TO CREATE MONOTONE for each dataset;
proc mi data= &&dsname&j seed=10 minimum=0 maximum=100 nimpute=5 out=whasmono;
var age ed race disease vision gds traila1-traila6;
mcmc impute=monotone;
run;


* Rename monotone imputation variable;
data whasimpute; set whasmono;
rename _Imputation_ =fimpute;
run;


* IMPUTE TRAIL A using regression for each dataset;
proc mi data=whasimpute seed=10 minimum=0 maximum=100 nimpute=5 out=whasimpute&j;
	by fimpute;
	var age ed race disease vision gds traila1-traila6;
	monotone regression; 
	run;



%end;

%mend impute;
run;

%impute;
run;







/**************************
 Change missing to 999 (otherwise cannot save dat)
*****************************/

%let y1 = trailb;
%let y2 = hvlr;
%let y3 = hvldel;
%let y4 = ws;
%let y5 = mmse;


%let numvar = 5;
%let dsname = whasimpute1;




%macro datafix;
ods output new = newdata;
run;

*for each endogenous;
%do j=1 %to &numvar;

*for each time point;
%do k=1 %to 6;
ods listing close;

data &dsname; set &dsname;
*change missing to 999;
if &&y&j..&k = . then &&y&j..&k = 999;
run;
%end;
%end;
data new; set &dsname;
run;

%mend datafix;
run;

%datafix;
run;




*fix 999 for other missing;
%let y1 = sppb1;
%let y2 = sppb2;
%let y3 = sppb3;
%let y4 = sppb5;
%let y5 = sppb6;
%let y6 = death;
%let y7 = gdsc;
%let numvar = 7;
%macro datafix;
ods output new = newdata;
run;

*for each variable;
%do j=1 %to &numvar;

ods listing close;
data &dsname; set &dsname;
*change missing to 999;
if &&y&j = . then &&y&j = 999;
run;
%end;
data new; set &dsname;
run;

%mend datafix;
run;

%datafix;
run;






*check missing;
proc means data = whasimpute1 nmiss;
var _ALL_;
run;








/*********
* Save 25 datasets in dat files
**********/

%let numvar=5;
%let numvar2=5;


*%let pPath1=E:\;
%let pPath1=D:\;


%macro datacreate;

*for each dataset (1);
%do k=1 %to &numdats;


*for each imputed data;
%do j=1 %to &numvar;
%do i=1 %to &numvar2;

*save SAS datafile;
data lib&k..impute&j&i; set whasimpute&k;
if fimpute=&i & _Imputation_=&j	;
run;

*save dat file;
proc export data=lib&k..impute&j&i outfile="&&pPath&k..impute&j&i..dat" dbms=dlm replace;
putnames=no;
run;


%end;
%end;
%end;

%mend datacreate;
run;

%datacreate;
run;






































/**************************
*****************************/
/**************************
*****************************/
/**************************
Add in other predictors for imputation
*****************************/
/**************************
*****************************/





/**************************
 MULTIPLE IMPUTATION FOR TRAIL A
*****************************/

%let dsname1 = whas;
%let numdats=1;



%macro impute;

*for each dataset (only 1);
%do j=1 %to &numdats;

* IMPUTE TRAIL A, GDS TO CREATE MONOTONE for each dataset;
proc mi data= &&dsname&j seed=10 minimum=0 maximum=100 nimpute=5 out=whasmono;
var age ed race disease vision gds sppb1 ws1 mmse1 hvlr1 traila1-traila6;
mcmc impute=monotone;
run;


* Rename monotone imputation variable;
data whasimpute; set whasmono;
rename _Imputation_ =fimpute;
run;


* IMPUTE TRAIL A using regression for each dataset;
proc mi data=whasimpute seed=10 minimum=0 maximum=100 nimpute=5 out=whasimpute&j;
	by fimpute;
	var age ed race disease vision gds sppb1 ws1 mmse1 hvlr1 traila1-traila6;
	monotone regression; 
	run;



%end;

%mend impute;
run;

%impute;
run;







/**************************
 Change missing to 999 (otherwise cannot save dat)
*****************************/

%let y1 = trailb;
%let y2 = hvlr;
%let y3 = hvldel;
%let y4 = ws;
%let y5 = mmse;


%let numvar = 5;
%let dsname = whasimpute1;




%macro datafix;
ods output new = newdata;
run;

*for each endogenous;
%do j=1 %to &numvar;

*for each time point;
%do k=1 %to 6;
ods listing close;

data &dsname; set &dsname;
*change missing to 999;
if &&y&j..&k = . then &&y&j..&k = 999;
run;
%end;
%end;
data new; set &dsname;
run;

%mend datafix;
run;

%datafix;
run;




*fix 999 for other missing;
%let y1 = sppb1;
%let y2 = sppb2;
%let y3 = sppb3;
%let y4 = sppb5;
%let y5 = sppb6;
%let y6 = death;
%let y7 = gdsc;
%let numvar = 7;
%macro datafix;
ods output new = newdata;
run;

*for each variable;
%do j=1 %to &numvar;

ods listing close;
data &dsname; set &dsname;
*change missing to 999;
if &&y&j = . then &&y&j = 999;
run;
%end;
data new; set &dsname;
run;

%mend datafix;
run;

%datafix;
run;






*check missing;
proc means data = whasimpute1 nmiss;
var _ALL_;
run;








/*********
* Save 25 datasets in dat files
**********/

%let numvar=5;
%let numvar2=5;


*%let pPath1=E:\;
%let pPath1=D:\;


%macro datacreate;

*for each dataset (1);
%do k=1 %to &numdats;


*for each imputed data;
%do j=1 %to &numvar;
%do i=1 %to &numvar2;

*save SAS datafile;
data lib&k..imputeendo&j&i; set whasimpute&k;
if fimpute=&i & _Imputation_=&j	;
run;

*save dat file;
proc export data=lib&k..imputeendo&j&i outfile="&&pPath&k..imputeendo&j&i..dat" dbms=dlm replace;
putnames=no;
run;


%end;
%end;
%end;

%mend datacreate;
run;

%datacreate;
run;



