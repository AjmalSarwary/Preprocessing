libname sasba '/home/u61013211/EST142/data/import/'; 

data amesmiss; 
   set sasba.ames300miss; 
run;

data ames70; 
   set sasba.ames70; 
run; 
 


******Part A.1: missing value indicator variable to treat information loss by imputation******;


data amesmi; 
   set amesmiss; 
   array mi{*} MI_Total_Bsmt_SF MI_Gr_Liv_Area MI_High_Kitchen_Quality; 
   array x_impute{*} Total_Bsmt_SF Gr_Liv_Area High_Kitchen_Quality; 
 
   do i=1 to dim(mi); 
      mi{i}=(x_impute{i}=.); 
   end; 
run; 

*******Part A.2: median imputaion********;

proc stdize data=amesmi 
            reponly 
            method=median 
            out=med_impute; 
           var Total_Bsmt_SF Gr_Liv_Area High_Kitchen_Quality; 
run; 
 

********Part B: collapsing levels of categorical variable *******;

********Part B.1: pearson chisquare statistic for bonus paid by neighborhood 
                used later to calculate _pchi_*rsquared for min(logpvalue) ******;  


proc freq data=ames70 noprint; 
   tables bonus*neighborhood/chisq; 
   output out=chi (keep=_pchi_) chisq; 
run; 
 
 
 
********Part B.2: proportions of bonus=yes by neighborhood ******; 

proc means data=ames70 noprint nway; 
  class neighborhood; 
  var bonus; 
  output out=propbonus mean=prop; 
run; 
 


********Part B.3: collapsing neigborhood levels sequentially 
                and recording metrics at each level such as rsquare****;
                
proc cluster data=propbonus method=ward outtree=treeinfo 
        plots=(dendrogram(vertical height=rsq)); 
   freq _freq_; 
   var prop; 
   id neighborhood; 
   ods output clusterhistory=cluster;
run;

proc print data=cluster; 
title 'Contents of the Cluster History'; 
run; 
 
 
 
********Part B.4: extending clusterhistory by additonal metrics
                to choose clustered level with min(logpvalue) ******; 

data cutoff; 
   if _n_=1 then set chi; 
   set cluster; 
   chisquare=_pchi_*rsquared; 
   degfree=numberofclusters-1; 
   logpvalue=logsdf('CHISQ',chisquare,degfree); 
run; 
 
proc print data=cutoff; 
   var numberofclusters Semipartialrsq       rsquared 
chisquare degfree 
       logpvalue; 
   title 'Log P-Value Information and the Cluster History'; 
run; 
 
 
 
********Part B.5: plotting logpvalues against clustered neighborhood levels******; 

proc sgplot data=cutoff;        
   scatter y=logpvalue x=numberofclusters 
           / markerattrs=(color=blue symbol=circlefilled); 
   xaxis label="Number of Clusters"; 
   yaxis label="Log of P-Value" min=-350 max=-250; 
   title "Plot of Log P-Value by Number of Clusters"; 
run; 
 
 
********Part B.6: choosing cluster level with min(logpvalue) and mapping neighborhoods to selected clusters ******; 

proc sql; 
   select numberofclusters into :ncl 
   from cutoff 
   having logpvalue=min(logpvalue); 
quit; 
run; 
 
proc tree data=treeinfo nclusters=&ncl out=clus_solution; 
   id neighborhood; 
run; 
 
proc sort data=clus_solution; 
   by clusname; 
run; 
 
proc print data=clus_solution; 
   by clusname; 
   id clusname; 
   title ‘List of Neighborhoods by Cluster’; 
run;


********Part C: variable selection 
                data encoding for subsequent analysis*******;

*******Part C.1: one-hot-encoding nominal variables******;

   AboveAverage_Quality=0; BelowAverage_Quality=0; 
   
   AboveAverage_Condition=0; BelowAverage_Condition=0; 
   
   if Overall_Quality=3 then AboveAverage_Quality=1; 
   
   if Overall_Quality=1 then BelowAverage_Quality=1; 
   
   if Overall_Condition=3 then AboveAverage_Condition=1; 
   
   if Overall_Condition=1 then BelowAverage_Condition=1;
   
   if Heating_QC="Ex" then Excellent_Heat_QC=1; 
   
   if Heating_QC="Gd" or Heating_QC="Fa" or Heating_QC="TA" 
      
      then Excellent_Heat_QC=0; 
      
   if Lot_Shape="IRR" then Irreq_Lot_Shape=1; 
   
   if Lot_Shape="Reg" then Irreq_Lot_Shape=0; 
      
   if Central_Air="Y" then C_Air=1; 
   
   if Central_Air="N" then C_Air=0; 
      
   nbr_clus1=0; nbr_clus2=0; nbr_clus3=0; 
   
   if neighborhood="NoRidge" or neighborhood="NridgHt" 
or 
      neighborhood="Somerst" or neighborhood="StoneBr" 
or 
      neighborhood="Timber" or neighborhood="Veenker" 
or 
      neighborhood="CollgCr" or neighborhood="Gilbert" 
or 
      neighborhood="ClearCr" then nbr_clus1=1; 
      
   if neighborhood="NAmes" or neighborhood="SWISU" 
or 
      neighborhood="IDOTRR" or neighborhood="OldTown" 
or      
     neighborhood="Edwards" or neighborhood="BrkSide" 
or 
      neighborhood="Sawyer" then nbr_clus2=1;
      
   if neighborhood="Mitchel" then nbr_clus3=1; 
   
   
   
***define full set with 32 inputs*** 4th neighborhood is reference group; 

%let fullset=Gr_Liv_Area Total_Bsmt_SF Bsmt_Fin_SF Bsmt_Unf_SF Lot_Area 
     Age_At_Sale Bedroom_AbvGr High_Kitchen_Quality Fullbath_2Plus 
     Fireplace_1Plus TwoPlusCar_Garage High_Exterior_Cond 
     High_Exterior_Qual One_Floor Vinyl_Siding CuldeSac Has_Fence 
     Land_Level Poured_Concrete Paved_Driveway Total_Functionality 
     Normal_Prox_Cond AboveAverage_Quality BelowAverage_Quality 
     AboveAverage_Condition BelowAverage_Condition Excellent_Heat_QC 
     Irreq_Lot_Shape C_Air nbr_clus1 nbr_clus2 nbr_clus3; 
run; 

OPTIONS NONOTES NOSTIMER NOSOURCE NOSYNTAXCHECK;

********Part C.2: variable selection through divisive clustering 
                  and splitting criterion 2nd eigenvalue > 0.6   ****;
%en;
proc varclus data=ames70 maxeigen=.60 hi short plots(only)=(dendrogram); 
   var &fullset; 
   ods output clusterquality=clusqual;
   ods output rsquare=rsq;
   title1 "Cluster Variation: 32 = # of variables";
   title2 "Total Variation by 1st PC: ...., %age of Total Variation?";
run;

proc print data=rsq;
run;

proc print data=clusqual;
run;













