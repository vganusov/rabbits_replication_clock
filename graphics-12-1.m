(*Load the package code*)

(* check if the package is not installed then install *)

If[Length[FileNames[
  FileNameJoin[{$UserBaseDirectory, "Applications", 
    "PolygonPlotMarkers.m"}]]]==0, 
	package = 
	Import["http://raw.github.com/AlexeyPopkov/PolygonPlotMarkers/master/PolygonPlotMarkers.m", "Text"];

	(*Install the package (existing file will be overwritten!)*)
	Export[FileNameJoin[{$UserBaseDirectory, "Applications", 
    	"PolygonPlotMarkers.m"}], package, "Text"];
]

Needs["PolygonPlotMarkers`"]

em[name_, size_ : 5.5] := 
 Graphics[{Dynamic@
    EdgeForm@
     Directive[CurrentValue["Color"], JoinForm["Round"], 
      AbsoluteThickness[2], Opacity[1]], FaceForm[White], 
   PolygonMarker[name, Offset[size]]}, AlignmentPoint -> {0, 0}]

em0[name_, size_ : 5.5, color_] := 
 Graphics[{Dynamic@
    EdgeForm@
     Directive[CurrentValue["Color"], JoinForm["Round"], 
      AbsoluteThickness[2], color, Opacity[1]], FaceForm[White], 
   PolygonMarker[name, Offset[size]]}, AlignmentPoint -> {0, 0}]


histPlot[data_, bins_: 10, opt___] := 
 Module[{countBorder = 
    Partition[Riffle[Riffle[#1, #1[[2 ;;]]], Riffle[#2, #2]], 2] & @@ 
     HistogramList[data, bins, "Probability"]}, 
  ListLinePlot[countBorder, opt]
]

histPlot2[data_, bins_ : 10, opt___] := Module[{hist},
  hist = HistogramList[data, bins, "Probability"];
  ListPlot[
   Transpose[{hist[[1]], ArrayPad[hist[[2]], {0, 1}, "Fixed"]}],
   InterpolationOrder -> 0,
   Joined -> True, opt, Options[MyPlot]]
  ]



Get["/Users/vitaly/refs/mathematica/CustomTicks.m"];
Clear[myticks]
DisplayTogether:=Show;
SymbolStyle:=PlotStyle;
SymbolShape:=PlotMarkers;

line:="\!\(\*
StyleBox[\"-\",\nFontSize->36]\)";

(* nice open symbols *)
symbolsOpen=em /@ {"Circle", "Triangle", "Diamond", "Square", "DownTriangle"};

(* closed symbols *)
symbolsClosed={\[FilledCircle],
    \[FilledUpTriangle],
    \[FilledDiamond],
    \[FilledSquare],
    \[FilledDownTriangle]};


symbols0=symbolsText0= Flatten[
	Table[Join[symbolsOpen,symbolsClosed] ,{15}]];
symbols= symbols0;

symbolsText = Flatten[Table[{
      {\[EmptyCircle]},
      {\[EmptyUpTriangle]},
      {\[EmptyDiamond]},
      {\[EmptySquare]},
      {\[EmptyDownTriangle]},
    {\[FilledCircle]},
    {\[FilledUpTriangle]},
    {\[FilledDiamond]},
    {\[FilledSquare]},
    {\[FilledDownTriangle]}
      },{15}]];


colors = Flatten[Table[{
      GrayLevel[0],
      RGBColor[1, 0, 0],
      RGBColor[0, 0, 1],
      RGBColor[0, 0.8, 0.2],
      Hue[8/9],
      Hue[0.1],
      RGBColor[0.6, .3, 0.2]
      },{15}],1];

dashings = Flatten[Table[{
      Dashing[{0, 0}],
      Dashing[{Medium}],
      Dashing[{Small}],
      Dashing[Tiny],
      Dashing[{Medium, Small, Tiny, Small}],     
      Dashing[{Large}],
      Dashing[{Medium, Tiny, Tiny, Tiny}],
      Dashing[{Large, Medium, Small, Medium}],
      Dashing[{Large, Large}]
      },{15}],1];

dashingsText = Flatten[Table[{
      {"\[LongDash]\[LongDash]\[LongDash]\[LongDash]"},
      {"\[Dash] \[Dash]  "},
      {"\[Dash]\[Bullet]\[Dash]"},
      {"\[LongDash] \[LongDash] "},
      {"\[Bullet]\[Bullet]\[Bullet]"},
      {"\[Dash]\[Bullet]\[Bullet]\[Dash]"},
      {"\[LongDash] \[Dash] "},
      {"\[LongDash] \[LongDash] "},
      {"\[LongDash]\[LongDash]\[LongDash]\[LongDash] "},
      {"\[Dash] \[Dash]  "},
      {"\[LongDash] \[LongDash] "}
      },{15}],1];

colorsgray = Flatten[Table[{
  GrayLevel[0],
  GrayLevel[0.3],
  GrayLevel[0.5],
  GrayLevel[0.7],  
  GrayLevel[0],
  GrayLevel[0.3],
  GrayLevel[0.5], 
  GrayLevel[0.7],
  GrayLevel[0],
  GrayLevel[0.3],
  GrayLevel[0.5],
  GrayLevel[0.7],  
  GrayLevel[0]
},{15}],1];


log10[x_]:=Log[10,x];

(* myticks:=Automatic; *)


myticks:=LinTicks[#1, #2, MajorTickLength->{0.02,0},MinorTickLength->{0.0,0}] & 

(* myticks[min_, max_] := 
 Table[If[FractionalPart[i] == 0., {i, i, {.02, 0}}, {i, "", 0}], {i, 
   Floor[min], Ceiling[max], 0.1}];

*)

Clear[MyListPlot];

Options[MyListPlot] = {
      BaseStyle->{FontSize -> 16}, 
      Axes->False,
      Frame -> {True, True, False, False},
      FrameTicks->myticks,
      FrameStyle -> Thickness[0.005],
      FrameTicksStyle->Thickness[0.005],
      PlotRange -> All,
      Joined->False,
      FrameLabel -> {"x", "f"},
      AspectRatio -> 1/GoldenRatio,
      ImageSize -> {420, 280}
      };

MyListPlot::usage="Plots multiple datasets on one plot. SymbolShape available options (None,Plain,Line), SymbolStyle->(Plain,Gray),PlotStyle->(Plain,Gray,NoDashing)."


  MyListPlot[datalist_, opts___] := 
    Module[{plotlist,nplot, opt, legend0, i, myshape,mystyle,mystyleline},
    plotlist=datalist;
    nplot = Length[plotlist];
    opt = Flatten[Join[{opts}, Options[MyListPlot]],1];
    legend0 = 0;
    If[MatrixQ[legend /. opt] || VectorQ[legend/.opt],
      legend0 = legend /. opt;
      opt = DeleteCases[opt, x_?(#[[1]]==legend &)];
     (* opt = Delete[opt, 1] *)
      ];
    myshape=Table[{symbols[[i]],10},{i,1,nplot}];
    mystyle=Table[{Thickness[0.006],dashings[[i]],colors[[i]]},{i,1,nplot}];
    
    If[(PlotMarkers /. opt) == None, myshape = Table["",{nplot}]];
    If[(PlotMarkers /. opt) == Plain, myshape = Table[{symbols[[1]],10},{nplot}]];
    If[(PlotMarkers /. opt) == Line, myshape = Table["\!\(\*
StyleBox[\"-\",\nFontSize->36]\)",{nplot}]];
    If[MatrixQ[PlotMarkers /. opt]||VectorQ[PlotMarkers /. opt], myshape=PlotMarkers /. opt];

    If[(PlotStyle /. opt) == Plain, 
                            mystyle = Table[{{Thickness[0.006], dashings[[1]],colors[[1]]}},{nplot}]];
    If[(PlotStyle /. opt) == Gray, 
                    mystyle = Table[{{Thickness[0.006], dashings[[i]],colorsgray[[i]]}},{i,1,nplot}]];
    If[(PlotStyle /. opt) == NoDashing, 
                        mystyle = Table[{{Thickness[0.006], colors[[i]]}},{i,1,nplot}]];
    If[Length[PlotStyle /. opt]>0 , mystyle=PlotStyle /.opt];

    If[! MatrixQ[legend0] && ! VectorQ[legend0],
        ListPlot[
          plotlist,
          PlotMarkers -> myshape,
          PlotStyle -> mystyle,
          opt
          ],
         ListPlot[
          plotlist,
          PlotMarkers -> myshape,
          PlotStyle -> mystyle,
	  Epilog->legend0,
          opt
          ]
        ]
      ];

mylegend[tlabels_, pars__,opt___] := Module[
{nd0,x,y,spacing,extra, pars0,k,opt1,allsizes,allcolors,allsymbols,alldashings},
      opt1=Join[{opt},Options[mylegend]];
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.06};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.06}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.06}];
      allcolors=Table[colors[[i]],{i,1,nd0}];
      allsizes=Table[12,{nd0}]; 
      alldashings=Table[dashings[[i]],{i,1,nd0}];   
      allsymbols=Table[dashingsText[[i]],{i,1,nd0}];
      If[Length[FontColor/.opt1]>0,allcolors=FontColor/.opt1];
      If[Length[FontSize/.opt1]>0,allsizes=FontSize/.opt1];
      If[Length[SymbolShape/.opt1]>0,allsymbols=SymbolShape/.opt1];
      If[Length[DashingStyle/.opt1]>0,alldashings=DashingStyle/.opt1];
      opt1 =
      DeleteCases[opt1, x_?(#[[1]] == FontColor || #[[1]]==SymbolShape 
                         || #[[1]] == DashingStyle &)];
      {x, y, spacing, extra} = pars0;
  Flatten[{
   Table[{alldashings[[j]], allcolors[[j]], Thickness[0.005], 
     Line[{Scaled[{x, y - spacing*(j - 1)}], 
       Scaled[{x + 2*extra, y - spacing*(j - 1)}]}]}, {j, 1, nd0}],
   Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold, allsizes[[j]]], 
     Scaled[{x + 2.1*extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, 
     nd0}]
   }]
];

Clear[mylegend2,tlabels,pars,nd0];



mylegend2[tlabels_, pars__, opt___] := Module[
   {nd0,x,y,spacing,extra, pars0,opt1,allsizes,allcolors,allsymbols},
      opt1=Join[{opt},Options[mylegend]];
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.05};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.05}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
      allcolors=Table[colors[[i]],{i,1,nd0}];
      allsizes=Table[12,{i,1,nd0}];
      allsymbols=Table[symbolsText[[i]],{i,1,nd0}];
      If[Length[FontColor/.opt1]>0,allcolors=FontColor/.opt1];
      If[Length[SymbolShape/.opt1]>0,allsymbols=SymbolShape/.opt1];
      If[Length[FontSize/.opt1]>0,allsizes=FontSize/.opt1];
      opt1 =
      DeleteCases[opt1, x_?(#[[1]] == FontColor || #[[1]]==SymbolShape &)];
  {x, y, spacing, extra} = pars0;
  Flatten[{
   Table[Text[Style[allsymbols[[j]], allcolors[[j]], Bold,allsizes[[j]]], 
     Scaled[{x, 0.003+y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}], 
   Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold,allsizes[[j]]], 
     Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}]
   }]
 ];

mylegend20[tlabels_, pars__, opt___] := 
  Module[{nd0, x, y, spacing, extra, pars0, opt1, allcolors, 
    allsymbols, shapes, symbolsT, extrashift}, 
   opt1 = Join[{opt}, Options[mylegend]];
   nd0 = Length[tlabels];
   pars0 = {0.03, 0.95, 0.07, 0.05};
   If[Length[pars] == 4, pars0 = pars];
   shapes = {"Circle", "Triangle", "Diamond", "Square", 
     "DownTriangle"};
   If[Length[pars] == 1, pars0 = {0.8, 0.95, 0.07, 0.05}];
   If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
   allcolors = Table[colors[[i]], {i, 1, nd0}];
   symbolsT = 
    Table[If[i <= Length[shapes], em0[shapes[[i]], 5.5, colors[[i]]], 
      symbolsClosed[[i]]], {i, 1, Length[allcolors]}];
   allsymbols = Table[symbolsT[[i]], {i, 1, nd0}];
   If[Length[FontColor /. opt1] > 0, allcolors = FontColor /. opt1];
   If[Length[SymbolShape /. opt1] > 0, 
    allsymbols = SymbolShape /. opt1];
   If[NumericQ[ExtraShift/.opt1],extrashift=ExtraShift/.opt1,extrashift=0];
   opt1 = 
    DeleteCases[opt1, 
     x_?(#[[1]] == FontColor || #[[1]] == SymbolShape &)];
   {x, y, spacing, extra} = pars0;
   Flatten[{Table[
      Text[Style[allsymbols[[j]], allcolors[[j]], Bold, 12], 
       Scaled[{x - 0.37*If[j <= Length[shapes], 1, 0]+extrashift, 
         0.00 + y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}], 
     Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold, 12], 
       Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, 
       nd0}]}]];


mylegend2noS[tlabels_, pars__, opt___] := Module[
   {nd0,x,y,spacing,extra, pars0,opt1,allcolors,allsizes,allsymbols},
      opt1=Join[{opt},Options[mylegend]];
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.05};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.05}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
      allcolors=Table[colors[[i]],{i,1,nd0}];
      allsizes=Table[12,{i,1,nd0}];
      allsymbols=Table[symbolsText[[i]],{i,1,nd0}];
      If[Length[FontColor/.opt1]>0,allcolors=FontColor/.opt1];
      If[Length[FontSize/.opt1]>0,allsizes=FontSize/.opt1];
      If[Length[SymbolShape/.opt1]>0,allsymbols=SymbolShape/.opt1];
      opt1 =
      DeleteCases[opt1, x_?(#[[1]] == FontColor || #[[1]]==SymbolShape &)];
  {x, y, spacing, extra} = pars0;
  Flatten[
   Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold,allsizes[[j]]],
     Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}]
   ]
 ];

mylegend2noS0[tlabels_, pars__, opt___] := Module[
   {nd0,x,y,spacing,extra, pars0,opt1,allcolors,allsymbols},
      opt1=Join[{opt},Options[mylegend]];
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.05};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.05}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
      allcolors=Table[colors[[i]],{i,1,nd0}];
      allsymbols=Table[symbolsText[[i]],{i,1,nd0}];
      If[Length[FontColor/.opt1]>0,allcolors=FontColor/.opt1];
      If[Length[SymbolShape/.opt1]>0,allsymbols=SymbolShape/.opt1];
      opt1 =
      DeleteCases[opt1, x_?(#[[1]] == FontColor || #[[1]]==SymbolShape &)];
  {x, y, spacing, extra} = pars0;
  Flatten[
   Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold,12],
     Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}]
   ]
 ];



mylegend3[tlabels_, pars__,opt___] := Module[
      {allsizes,nd0,x,y,spacing,extra, pars0,k,opt1,allcolors,allsymbols},
  opt1=Join[{opt},Options[mylegend]];
  nd0 = Length[tlabels];
  pars0 = {0.03, 0.95, 0.07, 0.03};
  If[Length[pars] == 4, pars0 = pars];
  If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
  allcolors=Table[colors[[i]],{i,1,nd0}];
  alldashings=Table[dashings[[i]],{i,1,nd0}];
  allsizes=Table[12,{i,1,nd0}];
  allsymbols=Table[symbolsText[[i]],{i,1,nd0}];
  If[Length[SymbolShape/.opt1]>0,allsymbols=SymbolShape/.opt1];
  If[Length[FontColor/.opt1]>0,allcolors=FontColor/.opt1];
  If[Length[FontSize/.opt1]>0,allsizes=FontSize/.opt1];
  If[Length[DashingStyle/.opt1]>0,alldashings=DashingStyle/.opt1];
  opt1 =
      DeleteCases[opt1, x_?(#[[1]] == FontColor || #[[1]]==DashingStyle || 
                            #[[1]] == SymbolShape &)];
  {x, y, spacing, extra} = pars0;
  Flatten[{
   Table[{alldashings[[j]], allcolors[[j]], Thickness[0.005],
     Line[{Scaled[{x, 0.007+y - spacing*(j - 1)}],
       Scaled[{x + 2*extra, 0.007+y - spacing*(j - 1)}]}]}, {j, 1, nd0}],
   Table[Text[Style[allsymbols[[j]], allcolors[[j]], Bold,allsizes[[j]]],
     Scaled[{x+0.75*extra, 0.011+y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}],
   Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold,allsizes[[j]]],
     Scaled[{x + 2.1*extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}]
   }]
 ];


Clear[mylegend30];

mylegend30[tlabels_, pars__, opt___] := 
  Module[{nd0, x, y, spacing, extra, pars0, k, opt1, allcolors, 
    shapes, allsymbols, symbolsT, alldashings}, 
   opt1 = Join[{opt}, Options[mylegend]];
   nd0 = Length[tlabels];
   shapes = {"Circle", "Triangle", "Diamond", "Square", 
     "DownTriangle"};
   pars0 = {0.03, 0.95, 0.07, 0.03};
   If[Length[pars] == 4, pars0 = pars];
   If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
   allcolors = Table[colors[[i]], {i, 1, nd0}];
   alldashings = Table[dashings[[i]], {i, 1, nd0}];
   If[Length[FontColor /. opt1] > 0, allcolors = FontColor /. opt1];
   symbolsT = 
   Table[If[i <= Length[shapes], em0[shapes[[i]], 5.5, allcolors[[i]]], 
    symbolsClosed[[i - Length[shapes]]]], {i, 1, Length[allcolors]}];
   allsymbols = Table[symbolsT[[i]], {i, 1, nd0}];
   If[Length[SymbolShape /. opt1] > 0, 
    allsymbols = SymbolShape /. opt1];
   If[Length[FontColor /. opt1] > 0, allcolors = FontColor /. opt1];
   If[Length[DashingStyle /. opt1] > 0, 
    alldashings = DashingStyle /. opt1];
   opt1 = 
    DeleteCases[opt1, 
     x_?(#[[1]] == FontColor || #[[1]] == DashingStyle || #[[1]] == 
          SymbolShape &)];
   {x, y, spacing, extra} = pars0;
   Flatten[{Table[{alldashings[[j]], allcolors[[j]], Thickness[0.005],
        Line[{Scaled[{x, 0.007 + y - spacing*(j - 1)}], 
         Scaled[{x + 2*extra, 0.007 + y - spacing*(j - 1)}]}]}, {j, 1,
        nd0}], Table[
      Text[Style[allsymbols[[j]], allcolors[[j]], Bold, 12], 
       Scaled[{x + 0.75*extra - 0.24*If[j<=Length[shapes],1,0], 
         0.011 + y - spacing*(j - 1)}], {-1, 0}], {j, 1, nd0}], 
     Table[Text[Style[tlabels[[j]], allcolors[[j]], Bold, 12], 
       Scaled[{x + 2.1*extra, y - spacing*(j - 1)}], {-1, 0}], {j, 1, 
       nd0}]}]];


mylegend2color[tlabels_, pars_,col_] := Module[
      {nd0,x,y,spacing,extra, pars0},
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.05};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.05}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
      {x, y, spacing, extra} = pars0;
      {
        Table[
          Graphics[{col[[j]],
              Text[symbolsText[[j]] <> "",
                Scaled[{x, y - spacing*(j - 1)}], {-1, 0},
                TextStyle -> {FontSize -> 14, FontWeight -> "Bold",
                    FontFamily -> "Courier"}]}], {j, 1, nd0}],
        Table[Graphics[{col[[j]],
              Text[ToString[tlabels[[j]]],
                Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0},
                TextStyle -> {FontSize -> 14, FontWeight -> "Bold",
                    FontFamily -> "Courier"}]}], {j, 1, nd0}]
        }
      ];


mylegend2colorsS[tlabels_, pars_,col_,sym_] := Module[
      {nd0,x,y,spacing,extra, pars0},
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.05};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.05}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
      {x, y, spacing, extra} = pars0;
      {
        Table[
          Graphics[{col[[j]],
              Text[sym[[j]] <> "",
                Scaled[{x, y - spacing*(j - 1)}], {-1, 0},
                TextStyle -> {FontSize -> 14, FontWeight -> "Bold",
                    FontFamily -> "Courier"}]}], {j, 1, nd0}],
        Table[Graphics[{col[[j]],
              Text[ToString[tlabels[[j]]],
                Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0},
                TextStyle -> {FontSize -> 14, FontWeight -> "Bold",
                    FontFamily -> "Courier"}]}], {j, 1, nd0}]
        }
      ];




mylegend2gray[tlabels_, pars__] := Module[
      {nd0,x,y,spacing,extra},
      nd0 = Length[tlabels];
      pars0={0.03,0.95,0.07,0.05};
      If[Length[pars]==4,pars0=pars];
      If[Length[pars]==1,pars0={0.8,0.95,0.07,0.05}];
      If[Length[pars] == 2, pars0 = {pars[[1]], pars[[2]], 0.07, 0.05}];
      {x, y, spacing, extra} = pars0;
      {
        Table[
          Graphics[{colorsgray[[j]], 
              Text[symbolsText[[j]] <> "", 
                Scaled[{x, y - spacing*(j - 1)}], {-1, 0}, 
                TextStyle -> {FontSize -> 14, FontWeight -> "Bold", 
                    FontFamily -> "Courier"}]}], {j, 1, nd0}], 
        Table[Graphics[{colorsgray[[j]], 
              Text[ToString[tlabels[[j]]], 
                Scaled[{x + extra, y - spacing*(j - 1)}], {-1, 0}, 
                TextStyle -> {FontSize -> 14, FontWeight -> "Bold", 
                    FontFamily -> "Courier"}]}], {j, 1, nd0}]
        }
      ];


Options[mylegend]= {TextStyle -> {FontSize -> 14, FontWeight -> "Bold"
                    }};

MyLegend[tlabels_,pars__]:=mylegend[tlabels,pars];


logticks[{x1_,x2_,step_}]:=LogTicks[x1,x2,TickLabelStep->step,MajorTickLength->{0.02,0},MinorTickLength->{0.015,0}];

logticks2[{x1_,x2_,step_}]:=StripTickLabels[LogTicks[x1,x2,TickLabelStep->step,MajorTickLength->{0.02,0},MinorTickLength->{0.015,0}]];

logticks3[{x1_,x2_,step_}]:=LogTicks[x1,x2,TickLabelStep->step,MajorTickLength->{0.02,0},MinorTickLength->{0.0,0}];

MyGraphicsArray00[list0_, opt___] :=
 Module[{xdim,nmax,list,n,m,nf,letters,i,j,ii},
xdim = Map[Length[#] &, list0];
nmax = Max[xdim];
list = Map[
   If[Length[#] < nmax,
     Append[#, Table["NA", {i, 1, nmax - Length[#]}]], #] &,
   list0];
{n, m} = {nmax, Length[xdim]};
nf = Total[xdim];
ii = 0;
panels =
  Table[
   Table[
    ii++;
    Panel[list[[i, j]], Background -> GrayLevel[1]],
    {j, 1, xdim[[i]]}], {i, 1, m}];

Grid[panels, Alignment -> {Left, Top}, Spacings -> {0, 0.02},
 ItemSize -> All, opt]

]


MyGraphicsArray0[list0_, opt___] := 
 Module[{let0,xdim,nmax,list,n,m,nf,letters,i,j,ii},
xdim = Map[Length[#] &, list0];
nmax = Max[xdim];
list = Map[
   If[Length[#] < nmax, 
     Append[#, Table["NA", {i, 1, nmax - Length[#]}]], #] &, 
   list0];
{n, m} = {nmax, Length[xdim]};
nf = Total[xdim];
let0 = CharacterRange["A", "Y"];
letters=Join[let0, Flatten[Table[
   let0[[i]] <> let0[[j]], {i, 1, Length[let0]}, {j, 1, 
    Length[let0]}], 1]];
ii = 0;
panels =
  Table[
   Table[
    ii++;
    Panel[list[[i, j]], 
     Style[letters[[ii]], Bold, 21, 
      FontFamily -> "Helvetica"], {{Left, Top}}, 
     Background -> GrayLevel[1]],
    {j, 1, xdim[[i]]}], {i, 1, m}];

Grid[panels, Alignment -> {Left, Top}, Spacings -> {0, 0.02}, 
 ItemSize -> All]

]

MyGraphicsArray[list_, opt___] := 
  Module[{n, m, i, j, letters, dim, panels, opt1, missing},
   dim = Dimensions[list];
   {n, m} = If[Length[dim] == 1, {1, dim[[1]]}, dim];
   missing = Map[Map[If[StringQ[#] == True, 1, 0] &, #] &, list];
   letters = 
    Take[
let0 = CharacterRange["A", "Y"];
Join[let0, Flatten[Table[
   let0[[i]] <> let0[[j]], {i, 1, Length[let0]}, {j, 1, 
    Length[let0]}], 1]], 
     n*m];
   panels = 
    If[n == 1, {Table[
       Panel[list[[j]], 
        Style[letters[[j]], Bold, 21, 
         FontFamily -> "Arial"], {{Left, Top}}, 
        Background -> GrayLevel[1]], {j, 1, m}]},
     Table[Panel[list[[i, j]],
       If[missing[[i, j]] == 0,
        Style[letters[[j + (i - 1)*m]], Bold, 21, 
         FontFamily -> "Arial"],
        Style[""]
        ], {{Left, Top}}, Background -> GrayLevel[1]], {i, 1, n}, {j, 
       1, m}]];
   Grid[panels, Alignment -> {Left, Top}, Spacings -> {0, 0.03}, 
    ItemSize -> All, opt]];



Options[MyQuantilePlot] = {
    PlotMarkers -> symbols[[1]],
    Joined -> False,
    Frame -> True,
    PlotJoined -> False,
    Frame -> True,
    PlotStyle -> Thickness[0.006],
    ReferenceLineStyle -> {Hue[0], Thickness[0.006], Dashing[{0.02}]},
    Axes -> True,
    ImageSize -> {420, 280},
    TextStyle -> {FontSize -> 16},
    FrameStyle -> Thickness[0.004],
    PlotRange -> All,
    FrameLabel -> {"exp quantile", "theory quantile"}
};


MyQuantilePlot[array_,opt___]:=Module[{opt1},
opt1=Join[{opt},Options[MyQuantilePlot]];
QuantilePlot[
    array, 
    RandomArray[NormalDistribution[Mean[array],StandardDeviation[array]],1000],
    opt1
    ]
];

Clear[MyPlot];

Options[MyPlot] = {
      BaseStyle -> {FontSize -> 16},
      Axes->False,
      Frame -> {True, True, False, False},
      FrameTicks->myticks,
      FrameStyle -> Thickness[0.005],
      FrameTicksStyle->Thickness[0.005],
      (* PlotRange -> All, *)
      FrameLabel -> {"x", "f"},
      AspectRatio -> 1/GoldenRatio,
      ImageSize -> {420, 280}
      };

MyPlot[funclist_, range_, opts___] := 
    Module[{nplot, opt, legend0, newrange, myfunclist,mystyle},
      myfunclist=If[Length[funclist]==0,{funclist},funclist];
      nplot = Length[myfunclist];
      opt = Flatten[Join[{opts}, Options[MyPlot]],1];
      legend0 = 0;
      If[MatrixQ[legend /. opt],
        legend0 = legend /. opt;
        opt = Delete[opt, 1]
        ];
      mystyle=Table[{{Thickness[0.007], dashings[[i]],colors[[i]]}},
              {i,1,nplot}];
      If[(PlotStyle /. opt) == Plain, 
          mystyle = Table[{{Thickness[0.007], dashings[[1]],colors[[1]]}}, 
                      {i,1,nplot}] ];
        If[(PlotStyle /. opt) == Gray, 
           mystyle = Table[{{Thickness[0.007], dashings[[i]],colorsgray[[i]]}}, 
                      {i,1,nplot}]   
           ];
        If[(PlotStyle /. opt) == NoDashing, 
           mystyle = Table[{{Thickness[0.007], dashings[[1]],colors[[i]]}}, 
                      {i,1,nplot}]
          ];
        If[Length[PlotStyle /. opt]>0, mystyle=PlotStyle /.opt];
        newrange=range;        
        opt=DeleteCases[opt,_?(#[[1]]==PlotStyle&)]; 
      
      Show[
       If[! MatrixQ[legend0] && ! VectorQ[legend0],
        Plot[myfunclist,range,
          PlotStyle->mystyle
          ],
        Plot[myfunclist,range,
          PlotStyle->mystyle,
	  Epilog->legend0
          ]
        ],
       opt
      ]
     ];


