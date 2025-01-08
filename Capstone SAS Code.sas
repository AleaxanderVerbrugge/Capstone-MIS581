/* Generated Code (IMPORT) */
/* Source File: Customer_DF .csv */
/* Source Path: /home/u62440121/CustomerDF */
/* Code generated on: 1/8/25, 10:34 AM */

%web_drop_table(WORK.CustDF);


FILENAME REFFILE '/home/u62440121/CustomerDF/Customer_DF .csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.CustDF;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.CustDF; RUN;


%web_open_table(WORK.CustDF);

/* Generated Code (IMPORT) */
/* Source File: cust_transaction_details (1).csv */
/* Source Path: /home/u62440121/CustomerDF */
/* Code generated on: 1/8/25, 10:35 AM */

%web_drop_table(WORK.TransDF);


FILENAME REFFILE '/home/u62440121/CustomerDF/cust_transaction_details (1).csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.TransDF;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.TransDF; RUN;


%web_open_table(WORK.TransDF);

proc logistic data=WORK.TransDF;
	class paymentMethodType / param=glm;
	model transactionFailed(event='1')=paymentMethodType / link=logit 
		technique=fisher;
run;

proc glm data=WORK.TransDF;
	class paymentMethodType;
	model transactionFailed=paymentMethodType;
	means paymentMethodType / hovtest=levene welch plots=none;
	lsmeans paymentMethodType / adjust=tukey pdiff alpha=.05;
	run;
quit;
