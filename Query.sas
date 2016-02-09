proc sql;
create table final_results as
select id,Probability,Country
from RANKRankedSTRIP_RESULTS
group by id
having rank_Probability > 7
order by id, rank_Probability desc;
quit;

Proc sql;
Create table final_results1 as
SELECT B.id, 
case when A.Country = "NF" then "NDF"
when A.Country = "OT" then "Other" else A.Country end as Country
from final_results A inner join SeqID B on (A.id = B.id);
quit;

