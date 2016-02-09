/*data Train;
set Train_Users_2;
if age = . or age > 115 or age < 10 then age = -1;
run;
data Train_Ctr1 ;
set Train_Users_2;
where country_destination not in ('NDF','US','other');
drop timestamp_first_active date_first_booking;
run;

data Train_Ctr2 ;
set Train_Users_2;
where country_destination in ('NDF','US','other');
drop timestamp_first_active date_first_booking;
run;
 data Train_final;
 set Train_Ctr1 Train_NDFUS;
 run;
*/
proc logistic data= Train_final;
class affiliate_provider first_affiliate_tracked first_browser language affiliate_channel first_device_type
 signup_method;
model country_destination = affiliate_provider first_affiliate_tracked first_browser 
language affiliate_channel first_device_type signup_method / lackfit rsq Technique=newton  
outroc=roc clparm=wald clodds=wald scale=none PPROB=0.5 CTABLE NODUMMYPRINT NOLOGSCALE NOCHECK; 
OUTPUT OUT=pred  RESDEV=resdev RESCHI=reschi H=hat p=phat lower=lcl upper=ucl PRED=pred ; 
RUN;
quit;
