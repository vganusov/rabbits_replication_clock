(* routines for fitting multiple datasets together *)
(* see notebook for the example *)

(* bootstrapM2 - resample the data *)

minimum2M[array_, residual_, allpars_, par0_, keep_, fixed_, posit_, opt___] :=
        Module[
      {nfits, npars, parf, pars, parf0, i, j, temp, temp2, par0o, parf2, 
        pars2},
      nfits = Length[array];
      npars = Length[allpars];
      pars = Table[
          Table[
            If[keep[[j]] == 0, allpars[[j]][i], allpars[[j]]
              ], {j, 1, npars}], {i, 1, nfits}];
      pars = 
        Table[If[fixed[[i, j]] == 0, pars[[i, j]], par0[[i, j]]], {i, 1, 
            nfits}, {j, 1, npars}];
      parf = 
        DeleteCases[Map[#[[1]] &, Split[Sort[Flatten[pars]]]], 
          x_?(NumericQ[#] &)];
      parf0 = 
        Map[#[[2]] &, 
          DeleteCases[
            Map[#[[1]] &, 
              Split[Sort[
                  Flatten[
                    Table[If[fixed[[i, j]] == 0, 
                        pars[[i, j]] -> (par0[[i, j]])^(1/(1 + posit[[j]])), 
                        par0[[i, j]]], {i, 1, nfits}, {j, 1, npars}]]]]
              ], x_?(NumericQ[#] &)]];
      pars2 = pars;
      pars = 
        Table[If[posit[[j]] == 1&& fixed[[i, j]] == 0, (pars[[i, j]])^2, 
         pars[[i, j]]], {i, 1, nfits}, {j, 1, npars}];
      parf2 = 
        DeleteCases[Map[#[[1]] &, Split[Sort[Flatten[pars]]]], 
          x_?(NumericQ[#] &)];
      temp = 
        FindMinimum[Apply[Plus, Flatten[residual[array, pars], 1]^2], 
          Table[{parf[[i]], parf0[[i]], Max[10^-5,1.1*parf0[[i]]]}, {i, 1, 
              Length[parf]}], AccuracyGoal -> eps, PrecisionGoal -> eps, 
          MaxIterations -> 10^4];
      par0o = pars /. temp[[2]];
      Do[temp[[2, i, 2]] = (parf2 /. temp[[2]])[[i]], {i, 1, Length[parf]}];
      parf0 = parf /. temp[[2]];
      temp2 = {temp, par0o, parf0, pars2, parf};
      Clear[temp, parOo, parf0, pars, parf2];
      temp2
      ];

ClearAll[shuffle, data1, normalR, minimum, bootstrap];

shuffle[data1_] := Module[{tlength}, tlength = Length[data1];
      Table[data1[[Random[Integer, {1, tlength}]]], {tlength}]];

normalR[data1_] := Module[{tlength}, tlength = Length[data1];
      Table[
        Random[NormalDistribution[Mean[data1], 
            StandardDeviation[data1]]], {tlength}]];

shuffleM[data1_] := Map[shuffle[#]&,data1];

normalM[data1_] := Module[{tlength, len, len2, tdata, rdata},
      len = Map[Length[#] &, data1];
      tdata = Flatten[data1];
      tlength = Length[tdata];
      rdata = 
        Table[Random[
            NormalDistribution[Mean[tdata], 
              StandardDeviation[tdata]]], {tlength}];
      len2 = 
        Prepend[Table[Sum[len[[j]], {j, 1, i}], {i, 1, Length[len]}], 0];
      Table[
        Take[Flatten[rdata], {len2[[i]] + 1, len2[[i + 1]]}], {i, 1, 
          Length[len]}]
      ];

NoZero[vec_] := Map[Max[0, #] &, vec];

bootstrap2M[array_, residual_, allpars_, par0_, keep_, fixed_, posit_, Nboot_, 
      opt___] :=
    Module[
      {solTemp, pars, npar, bestresiduals, bestsolution, BootstrapData, 
        BootstrapFits, temp, ldata},
      solTemp = minimum2M[array, residual, allpars, par0, keep, fixed, posit];
      bestresiduals = residual[array, solTemp[[2]]];
      bestsolution = bestresiduals + Map[#[[All,2]] &, array];
      bestresiduals = bestresiduals - Mean[Flatten[bestresiduals]];
      besttimes = Map[#[[All, 1]] &, array];
      BootstrapData = Table[
          Table[Transpose[{besttimes[[i]], 
                NoZero[(bestsolution + shuffleM[bestresiduals])[[i]]]}], {i, 
              1, Length[array]}], {Nboot}];
      BootstrapFits = {};
      For[i = 1, i <= Nboot, i++,
        AppendTo[
          BootstrapFits, 
          minimum2M[BootstrapData[[i]], residual, allpars, par0, keep, fixed, 
            posit]
          ]
        ];
      temp = {BootstrapData, BootstrapFits, solTemp};
      Clear[solTemp, bestresiduals, bestsolution, besttimes, BootstrapData, 
        BootstrapFits];
      temp
      ];


bootstrap2Mdata[array_, residual_, allpars_, par0_, keep_, 
fixed_, posit_, Nboot_,      opt___] :=
    Module[
      {solTemp, pars, npar, bestresiduals, bestsolution, BootstrapData,
        BootstrapFits, temp, ldata},
      solTemp = minimum2M[array, residual, allpars, par0, keep, fixed, posit];
      BootstrapData = Table[shuffleM[array],{Nboot}];
      BootstrapFits = {};
      For[i = 1, i <= Nboot, i++,
        AppendTo[
          BootstrapFits,
          minimum2M[BootstrapData[[i]], residual, allpars, par0, keep, fixed,
            posit]
          ]
        ];
      temp = {BootstrapData, BootstrapFits, solTemp};
      Clear[solTemp, bestresiduals, bestsolution, besttimes, BootstrapData,
        BootstrapFits];
      temp
      ];

