If[$VersionNumber<6, 
Needs["Statistics`ContinuousDistributions`"];
Needs["Statistics`LinearRegression`"]
]


DemingRegression[dataset_, vr_, type___] := 
  Module[{x, y, SSDy, SSDx, SPDxy, n, beta, beta0, alpha, alpha0, ksi,
     sigmax, sigmay},
   n = Length[dataset];
   {x, y} = Transpose[dataset];
   SSDy = Variance[y]*(n - 1);
   SSDx = Variance[x]*(n - 1);
   SPDxy = Covariance[x, y]*(n - 1);
   beta0 = 
    beta = (SSDy - vr*SSDx + 
        Sqrt[(SSDy - vr*SSDx)^2 + 4*vr*SPDxy^2])/(2*SPDxy); (* slope *)

      alpha0 = alpha = Mean[y] - Mean[x]*beta; (* intercept *)
   
   ksi[i_] := (vr*x[[i]] + beta*(y[[i]] - alpha))/(vr + beta^2);
   sigmax = 
    N[1/((n - 2)*vr) (vr*Sum[(x[[i]] - ksi[i])^2, {i, 1, n}] + 
        Sum[(y[[i]] - alpha - beta*ksi[i])^2, {i, 1, n}])];
   sigmay = vr*sigmax;
   sigmax = Sqrt[sigmax];
   sigmay = Sqrt[sigmay];
   {{alpha + beta*xx}, {sigmax, sigmay}}
   ];

DemingRegressionBootstrap[dataset_, vr_, nboot_, type___] := 
  Module[{x, y, SSDy, SSDx, SPDxy, n, beta, beta0, alpha, alpha0, ksi,
     sigmax, sigmay, new, alphalist, betalist, sigmaxlist, sigmaylist},
   n = Length[dataset];
   alphalist = {};
   betalist = {};
   sigmaylist = {};
   sigmaxlist = {};
   
   Do[
    new = RandomChoice[dataset, n];
    x = new[[All, 1]];
    y = new[[All, 2]];
    SSDy = Variance[y]*(n - 1);
    SSDx = Variance[x]*(n - 1);
    SPDxy = Covariance[x, y]*(n - 1);
    beta = (SSDy - vr*SSDx + 
        Sqrt[(SSDy - vr*SSDx)^2 + 4*vr*SPDxy^2])/(2*SPDxy);
    alpha = Mean[y] - Mean[x]*beta;
    ksi[i_] := (vr*x[[i]] + beta*(y[[i]] - alpha))/(vr + beta^2);
    sigmax = 
     N[1/((n - 2)*vr) (vr*Sum[(x[[i]] - ksi[i])^2, {i, 1, n}] + 
         Sum[(y[[i]] - alpha - beta*ksi[i])^2, {i, 1, n}])];
    sigmay = vr*sigmax;
    sigmax = Sqrt[sigmax];
    sigmay = Sqrt[sigmay];
    alphalist = AppendTo[alphalist, alpha];
    betalist = AppendTo[betalist, beta];
    sigmaylist = AppendTo[sigmaylist, sigmay];
    sigmaxlist = AppendTo[sigmaxlist, sigmax], {count, 1, nboot}
    ];
   {{alphalist, betalist}, {sigmaxlist, sigmaylist}}
   ];




CVtest[{tempdata1_, tempdata2_}, nboot_] := 
  Module[{boot1, boot2, diff, pval},
   boot1 = 
    Table[RandomChoice[tempdata1, Length[tempdata1]], {nboot}];
   boot2 = 
    Table[RandomChoice[tempdata2, Length[tempdata2]], {nboot}];
   diff = Map[CV[#] &, boot1] - Map[CV[#] &, boot2];
   pval = N[Length[Cases[diff, x_?(# < 0 &)]]/nboot];
   {pval, 1 - pval}
   ];

FisherExact[data_]:=Module[{
        drs, (* row sums *)
        dcs, (* column sums *)
        total, (* total sum *)
        x,
        MkVars, (* create matrix vars as x_i,j *)
        MkEquations, (* create equations *)
        ComputeFactor, (* calculate common factor to p-values *)
        ComputeP, (* compute p-value for given data matrix *)
        matrix,
        equations,
        vars,
        sol, (* all solutions from Reduce *)
        factor,
        cutoff, (* p-value of original data *)
        pVal,
        v
    },
    drs = Plus@@#&/@data;
    dcs = Plus@@#&/@Transpose[data];
    total = Plus @@ drs;
    MkVars[nr_,nc_]:=Table[Subscript[x, i,j],{i,1,nr},{j,1,nc}];
    MkEquations[rs_,cs_,m_,v_]:=
        Join[
            MapThread[Equal,{Plus @@ # &/@m,rs}],
            MapThread[Equal,{Plus @@#&/@Transpose[m],cs}],
            MapThread[GreaterEqual,{v,Array[0&,Length[v]]}]
        ];
    ComputeFactor[rs_,cs_,tot_]:=Times@@(Factorial /@ rs) Times@@(Factorial /@ cs) / Factorial[tot];
    ComputeP[f_,m_]:=f/(Times @@ (Factorial /@ Flatten[m]));
    matrix = MkVars[Length[drs],Length[dcs]];
    vars = Flatten[MkVars[Length[drs],Length[dcs]]];
    equations = MkEquations[drs,dcs,matrix,vars];
    sol=Reduce[equations,vars,Integers];
    factor = ComputeFactor[drs,dcs,total];
    cutoff = ComputeP[factor,data];
    Print[{"cutoff",cutoff // N}];
    pVal=0;
    Select[(matrix /. ToRules[#])& /@ sol ,(v=ComputeP[factor,#];If[v<=cutoff,pVal+=v;Print[{#,v //N}]];v<=cutoff)&];
    pVal //N
];

FisherExactB[table_] :=
Module[  {r, s, nidot, ndotj, ntot, ev, prob},

(* Table dimensions r=nr of rows, s=nr of cols *)
{r, s} = Dimensions[table];

(* Margin and Total counts *)
nidot = Total[table, {2}] ;(* sum in r-direction *)
ndotj = Total[table, {1}] ;(* sum in s-direction *)
ntot = Total[table, 2]; (* overall total *)

(* evidence Eq.VI-54 p.200 *)

ev = Log[(r - 1)!/(ntot + r - 1)!] +
Total[Log[(ndotj + r - 1)!/(r - 1)!]] +
(Total[Log[nidot!]] - Log[ntot!]) -
(Total[Log[table!], 2] - Log[ntot!]) ;

(* probability from evidence: III-13 p.84 *)

prob = (1 + Exp[-ev])^-1 ;

{ev // N, prob // N} (* output *)
] (* End of Module *)


(* False discovery rate adjusted p values *)
PvalAdjusted[list_, q_] := Module[{n = Length[list], psig, ll, pmin, k},
   ll = Sort[list];
   k = 1;
   For[i = 1, i <= n, i++,
     If[ll[[i]] <= i/n*q, k = i]
    ];
   pmin = k/n*q;
   psig=Cases[ll, x_?(# <= pmin &)];
   {pmin, Max[psig],Length[psig]}
   ];

(* data must be in form data = {{11, 206}, {32, 1374}} *)
(* {{patients1,patients2}, {patients1,patients2}} *)

ContingencyTest0[data_] := 
  Module[{rc, fit, residual, \[Chi]2array, \[Chi]2, df},
   fit = Outer[Times, Plus @@@ data, Plus @@@ Transpose[data]]/
     Plus @@ Flatten[data];
   residual = data - fit;
   \[Chi]2array = residual^2/fit;
   \[Chi]2 = Plus @@ Flatten[\[Chi]2array];
   df = Length[Flatten[data]] - Length[data] - 
     Length[Transpose[data]] + 1;
   N[{\[Chi]2, df, 1 - CDF[ChiSquareDistribution[df], \[Chi]2]}]
   ];


ContingencyTest[data_] :=
  Module[{rc, fit, residual, \[Chi]2array, \[Chi]2, df},
   rc = {{"Row 1", "Row 2"}, {"Column 1", "Column 2"}};
   Print[TableForm[data, TableHeadings -> rc]];
   fit = Outer[Times, Plus @@@ data, Plus @@@ Transpose[data]]/
     Plus @@ Flatten[data];
   residual = data - fit;
   \[Chi]2array = residual^2/fit;
   \[Chi]2 = Plus @@ Flatten[\[Chi]2array];
   df = Length[Flatten[data]] - Length[data] -
     Length[Transpose[data]] + 1;
   N[{\[Chi]2, df, 1 - CDF[ChiSquareDistribution[df], \[Chi]2]}]
   ];


(* Data must be in the form (x1,y1), (x2,y2), etc *)
MyGoodnessFitTestUnequal[dat_, df__] := 
 Module[{R1 = Total[dat[[All, 1]]], R2 = Total[dat[[All, 2]]], chi, 
   df0 = 0},
  chi = Total[
    Map[(#[[1]]*Sqrt[R2/R1] - #[[2]]*
           Sqrt[R1/R2])^2/(#[[1]] + #[[2]]) &, dat]];
  If[NumericQ[df], df0 = df];
  (* Print["Chi2=", N[chi], " for df=", Length[dat] - df0]; *)
  {N[chi], Length[dat] - df0,N[1 - CDF[ChiSquareDistribution[Length[dat] - df0], chi]]}
  ];

MyGoodnessFitTestUnequal0[dat_, df__] := 
 Module[{R1 = Total[dat[[All, 1]]], R2 = Total[dat[[All, 2]]], chi, 
   df0 = 0},
  chi = Total[
    Map[(#[[1]]*Sqrt[R2/R1] - #[[2]]*
           Sqrt[R1/R2])^2/(#[[1]] + #[[2]]) &, dat]];
  If[NumericQ[df], df0 = df]
  {N[chi], Length[dat] - df0,N[1 - CDF[ChiSquareDistribution[Length[dat] - df0], chi]]}
  ];


MyGoodnessFitTest[dat_,df_] := Module[{chi},
  chi = Total[Map[(#[[1]] - #[[2]])^2/#[[2]] &, dat]];
  {N[chi], Length[dat]-df,N[1 - CDF[ChiSquareDistribution[Length[dat] - df], 
    chi]]}
];

ClearAll[ftest,pars];

ftest::usage="ftest[pars] gives the p value from the F-test for nested models. pars has to be in the form {solution1,solution2,nd} where solution1 and 2 are the solutions provided by Mathematica for the FindMinimum routine, and nd is the number of data points fitted."

Pvalue[sol_]:=sol[[1, 2, 1, 2, 4]];

SlopeValue[sol_]:=sol[[1, 2, 1, 2, 1]];


slopeTtest9[sol_, slope_] := Module[
   {tab, anova},
   tab = sol["ParameterTableEntries"];
   anova = sol["ANOVATableEntries"];
   2*(1 - 
      N[CDF[StudentTDistribution[(anova)[[2, 1]]], 
        Abs[(tab[[2, 1]] - slope)]/tab[[2, 2]]]])
   ];

slopeValue9[sol_] := Module[
    {tab, anova},
    tab = sol["ParameterTableEntries"];
    tab[[2, 1]]
   ];

interceptTtest9[sol_, slope_] := Module[
   {tab, anova},
   tab = sol["ParameterTableEntries"];
   anova = sol["ANOVATableEntries"];
   2*(1 - 
      N[CDF[StudentTDistribution[(anova)[[2, 1]]], 
        Abs[(tab[[1, 1]] - slope)]/tab[[1, 2]]]])
   ];


(* tdata has to be in the form {data1,data2} where          *)
(* data1 and data2 are tested to have the same linear slope *)
(* Another script is in malaria/coinfections notebook *)

MyTwoSlopesTest[tdata_]:=Module[{sol1,sol2},
    sol1 = FindMinimum[
     Sum[Total[Map[((a[i] + b*#[[1]]) - #[[2]])^2 &, tdata[[i]]]], {i, 1, 2}], {{a[1], 6}, {a[2], 6}, {b, -0.02}}];
    sol2 = FindMinimum[
     Sum[Total[Map[((a[i] + b[i]*#[[1]]) - #[[2]])^2 &, tdata[[i]]]], {i, 1, 
        2}], {{a[1], 6}, {a[2], 6}, {b[1], -0.02}, {b[2], -0.02}}];
    ftest[{sol1, sol2, Length[Flatten[tdata, 1]]}]
];

slopeTtest[sol_, slope_] := 
    2*(1 - N[CDF[StudentTDistribution[(ANOVATable /. sol)[[1, 2, 1]]], 
              Abs[(sol[[1, 2, 1, 2, 1]] - slope)]/sol[[1, 2, 1, 2, 2]]]]);

slopeTtest[sol_, slope_] := 
    2*(1 - N[CDF[StudentTDistribution[(ANOVATable /. sol)[[1, 2, 1]]], 
              Abs[(sol[[1, 2, 1, 2, 1]] - slope)]/sol[[1, 2, 1, 2, 2]]]]);

interceptTtest[sol_, slope_] := 
    2*(1 - N[CDF[StudentTDistribution[(ANOVATable /. sol)[[1, 2, 1]]], 
              Abs[(sol[[1, 2, 1, 1, 1]] - slope)]/sol[[1, 2, 1, 1, 2]]]]);

ftest[pars_] := Module[{solTemp1, solTemp2, nd, aa, bb, cc},
      {solTemp1, solTemp2, nd} = pars;
      If[solTemp1[[1]] < 
          solTemp2[[1]], {solTemp2, solTemp1} = {solTemp1, solTemp2}];
      bb = Length[solTemp2[[2]]] - Length[solTemp1[[2]]];
      cc = nd - Length[solTemp2[[2]]];
      aa = cc*(solTemp1[[1]] - solTemp2[[1]])/(bb*solTemp2[[1]]);
      Print["F value for the test is ", aa, " at ", bb, ",", cc];
      Print["Probability that the model with fewer parameters fits no worse is ", 
        1 - N[CDF[FRatioDistribution[bb, cc], aa]]];
      {aa,bb,cc,1 - N[CDF[FRatioDistribution[bb, cc], aa]]}
      ];


ftest2[pars_] := Module[{solTemp1, solTemp2, nd, aa, bb, cc},
      {solTemp1, solTemp2, nd} = pars;
      If[solTemp1[[1]] < 
          solTemp2[[1]], {solTemp2, solTemp1} = {solTemp1, solTemp2}];
      bb = Length[solTemp2[[2]]] - Length[solTemp1[[2]]];
      cc = nd - Length[solTemp2[[2]]];
      aa = cc*(solTemp1[[1]] - solTemp2[[1]])/(bb*solTemp2[[1]]);
      {bb,cc,aa,1 - N[CDF[FRatioDistribution[bb, cc], aa]]}
];

aic::usage="ftest[pars] gives the AIC values and their relative weights out of the presented models. pars has to be in the form {solution1,solution2,...,solutionN,nd} where solutioni are the solutions provided by Mathematica by the FindMinimum routine, and nd is the number of data points fitted."


(* Plain AIC *)

aic0[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta,i,w},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];
      nd=Last[pars];
      For[i=1,i<=nmod, i++,
       npar=Length[solTemp[i][[2]]]+1;
       AIC[[i]]=nd*Log[solTemp[i][[1]]/nd] + 2*npar;
      ];
      Print["Min AICc = ",min=Min[AIC]];
      delta = AIC - Table[min,{nmod}];
      w=Table[0,{Length[delta]}];
      For[i=1,i<=nmod, i++,
      Print["AICc = ", AIC[[i]]," delta = ",delta[[i]], " weight = ",
       w[[i]]=Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]], " for model ", solTemp[i]
       ];
      ];
      {AIC,delta,w}
];


(* Corrected AICc *)

aic[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta,i},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];
      nd=Last[pars];
      For[i=1,i<=nmod, i++, 
       npar=Length[solTemp[i][[2]]]+1;
       AIC[[i]]=nd*Log[solTemp[i][[1]]/nd] + 2*npar*nd/Max[1,nd-npar-1];
      ];
      Print["Min AICc = ",min=Min[AIC]];
      delta = AIC - Table[min,{nmod}];
      w=Table[0,{Length[delta]}];
      For[i=1,i<=nmod, i++,
      Print["AICc = ", AIC[[i]]," delta = ",delta[[i]], " weight = ", 
       w[[i]]=Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]], " for model ", solTemp[i]
       ];
      ];
   {AIC,delta,w}
];

aic2[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];

      nd=Last[pars];
      For[i=1,i<=nmod, i++, 
       npar=Length[solTemp[i][[2]]];
       AIC[[i]]=nd*Log[solTemp[i][[1]]/nd] + 2*npar*nd/Max[1,nd-npar-1];
      ];
      min=Min[AIC];
      delta = AIC - Table[min,{nmod}];
      Table[{AIC[[i]],delta[[i]],Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]]}, 
         {i,1,nmod}]
];  
(* This AIC = -2L+2K *)

aicLik0[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta,weights,aa},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];

      nd=Last[pars];
      For[i=1,i<=nmod, i++,
       npar=Length[solTemp[i][[2]]];
       AIC[[i]]=2*solTemp[i][[1]]+2*npar;
      ];
      Print["Min AICc = ",min=Min[AIC]];
      delta = AIC - Table[min,{nmod}];
      weights={};
      For[i=1,i<=nmod, i++,
      Print["AICc = ", AIC[[i]], " weight = ",
       aa=Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]], " for model ", solTemp[i]
       ];
      AppendTo[weights,aa];
      ];
      {AIC, delta,weights}
];

(* AIC with no printing of results *)
aicLik00[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta,weights,aa},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];

      nd=Last[pars];
      For[i=1,i<=nmod, i++,
       npar=Length[solTemp[i][[2]]];
       AIC[[i]]=2*solTemp[i][[1]]+2*npar;
      ];
      min=Min[AIC];
      delta = AIC - Table[min,{nmod}];
      weights={};
      For[i=1,i<=nmod, i++,
       aa=Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]];
      AppendTo[weights,aa];
      ];
      {AIC, delta,weights}
];


(* AICc *)
aicLik[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta,weights,aa},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];

      nd=Last[pars];
      For[i=1,i<=nmod, i++,
       npar=Length[solTemp[i][[2]]];
       AIC[[i]]=2*solTemp[i][[1]]+2*npar+2*npar*(npar+1)/(nd-npar-1);
      ];
      Print["Min AICc = ",min=Min[AIC]];
      delta = AIC - Table[min,{nmod}];
      weights={};
      For[i=1,i<=nmod, i++,
      Print["AICc = ", AIC[[i]], " weight = ",
       aa=Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]], " for model ", solTemp[i]
       ];
      AppendTo[weights,aa];
      ];
      {AIC, delta,weights}
];


aicLik2[pars_] := Module[{nmod,solTemp, AIC, nd, npar,min, delta,weights,aa},
      nmod=Length[pars]-1;
      For[i=1,i<=nmod, i++, solTemp[i]=pars[[i]]];
      AIC=Table[0,{nmod}];

      nd=Last[pars];
      For[i=1,i<=nmod, i++,
       npar=Length[solTemp[i][[2]]];
       AIC[[i]]=2*solTemp[i][[1]] + 2*(npar +
        1 + (npar+1)*(npar+2)/(nd - npar - 2));
      ];
      min=Min[AIC];
      delta = AIC - Table[min,{nmod}];
      weights={};
      For[i=1,i<=nmod, i++,
       aa=Exp[-delta[[i]]/2]/Apply[Plus,Exp[-delta/2]];
       AppendTo[weights,aa];
      ];
      weights
];



lof0::usage="lof0[dataset]: Provides characteristics of the data to be used in Lack of fit test(lof). The dataset should have format {{x1,y1},{x2,y2}..}, and thus be for 1 response variable. The output is total variance, total degrees of freedom and the total number of data points."

lof0[fdata_]:=Module[{replications, df, timesM, means, nd},

	replications = df = {};
	timesM = Split[Sort[fdata[[All,1]]]][[All,1]];
	means =
	 Table[Mean[Cases[fdata, x_?(#[[1]] == timesM[[jj]] &) :> x[[2]]]], {jj,
          1, Length[timesM]}];
	AppendTo[df, Length[means]];
	AppendTo[replications,
	Apply[Plus,
        Flatten[Table[(Cases[fdata, x_?(#[[1]] == timesM[[jj]] &) :> x[[2]]] -
                   means[[jj]])^2, {jj, 1, Length[timesM]}]]]];
	nd = Length[fdata];
       {Total[replications], Total[df], nd }
];


lof::usage="Lack of fit ftest[{solution,dataset}] provides an F-test of goodness of fit when data have repeated measurements."

lof[{sol1_, fdata_}] := Module[{replications, df, nd},
    {replications,df,nd}=lof0[fdata];
    Print["Residuals = ", sol1[[1]], " and df = ", (nd - Length[sol1[[2]]])];
    Print["Replications = ", replications,
      " and df = ", (nd - df)];
    Print["Lack of fit = ", sol1[[1]] - replications,
      " and df = ", (df - Length[sol1[[2]]])];
    aa = (sol1[[1]] - replications)/(df - Length[sol1[[2]]])/replications*(nd - df);
    bb = (df - Length[sol1[[2]]]);
    cc = (nd - df);
    Print["Test for lack of fit: F-value is ", aa, " for ",bb,",",cc, " with p = ",
      1 - N[CDF[FRatioDistribution[bb, cc], aa]]];
    ];



lofM::usage="Same as function lof but applied to fits of data with multiple datasets."

lofM[{sol1_, fdata_}] := Module[{replications, df, nd,ss},
    ss=Map[lof0[#]&,fdata];
    replications=Total[ss[[All,1]]];
    df=Total[ss[[All,2]]];
    nd=Total[ss[[All,3]]];
    Print["Residuals = ", sol1[[1]], " and df = ", (nd - Length[sol1[[2]]])];
    Print["Replications = ", replications,
      " and df = ", (nd - df)];
    Print["Lack of fit = ", sol1[[1]] - replications,
      " and df = ", (df - Length[sol1[[2]]])];
    aa = (sol1[[1]] - replications)/(df- 
                Length[sol1[[2]]])/replications*(nd - df);
    bb = (df - Length[sol1[[2]]]);
    cc = (nd - df);
    Print["Test for lack of fit: F-value is ", aa, " for ",bb,",",cc, " with p = ",
      1 - N[CDF[FRatioDistribution[bb, cc], aa]]];
    ];





normality::usage="normality[dataset] provides a Shapiro-Wilk normality test done in R. It is done by writing two temp files into a current directory and the removing it."

normality[fdata_] := Module[{},
   Export["residuals.csv", fdata, "TSV"];
   Run["R -q --slave --vanilla < ~/refs/mathematica/my_packages/checking_normality.R > r.out"];
   Print[ReadList[OpenRead["r.out"], String] // TableForm];
   Close["r.out"];
   aa=ReadList[OpenRead["r.out"], String];
   Close["r.out"];
   DeleteFile[{"r.out", "residuals.csv"}];
   ToExpression[StringDrop[Last[aa],3]]
   ];

logLik::usage="logLik[pars] gives the p value from the log-likelihood difference test for nested models. pars has to be in the form {solution1,solution2} where solution1 and 2 are the solutions provided by Mathematica for the FindMinimum routine, and nd is the number of data points fitted."

logLik[pars_] := Module[{solTemp1, solTemp2, aa, bb, cc, pval},
      {solTemp1, solTemp2} = pars;
      If[solTemp1[[1]] <
          solTemp2[[1]], {solTemp2, solTemp1} = {solTemp1, solTemp2}];
      bb = Length[solTemp2[[2]]] - Length[solTemp1[[2]]];
      aa = 2*(solTemp1[[1]] - solTemp2[[1]]);
      Print["Chi-square value for the test is ", aa, " at ", bb, " df"];
      pval= 1 - N[CDF[ChiSquareDistribution[bb], aa]];
      Print["Probability that the model with fewer parameters fits no worse is ",       pval];
	{bb, aa, pval}
      ];


logLik0[pars_] := Module[{solTemp1, solTemp2, aa, bb, cc, pval},
      {solTemp1, solTemp2} = pars;
      If[solTemp1[[1]] <
          solTemp2[[1]], {solTemp2, solTemp1} = {solTemp1, solTemp2}];
      bb = Length[solTemp2[[2]]] - Length[solTemp1[[2]]];
      aa = 2*(solTemp1[[1]] - solTemp2[[1]]);
      pval= 1 - N[CDF[ChiSquareDistribution[bb], aa]];
      {bb, aa, pval}
      ];


