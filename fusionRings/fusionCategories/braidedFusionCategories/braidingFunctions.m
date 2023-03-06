(* ::Package:: *)

(* ::Section:: *)
(*Nice tricks*)


Unprotect[Abs,Conjugate,Power,Times,Sqrt,Im,Re];
Abs[\[Phi][a_,b_][c_]]:=1;
Conjugate[\[Phi][a_,b_][c_]]:=1/\[Phi][a,b][c];

Abs[\[CurlyPhi][x_Integer][a_,b_][c_]]:=1
Conjugate[\[CurlyPhi][x_Integer][a_,b_][c_]]:=\[CurlyPhi][x][a,b][c]^-1
\[CurlyPhi][x_Integer][a_,b_][c_]^m_Integer/;(m<0||m>=x):=\[CurlyPhi][x][a,b][c]^Mod[m,x]

Protect[Abs,Conjugate,Power,Times,Sqrt];


(* ::Section:: *)
(*R-Symbols*)


R[a_,b_][c_]:=0

R[u_,a_][a_]/;(u===unit):=1
R[a_,u_][a_]/;(u===unit):=1

R[a_,b_][c_]/;MemberQ[Intersection[fusionproduct[a,b],fusionproduct[b,a]],c]:=R[a,b][c]=X0[a,b][c]//.repB0


Rs:=Rs=Table[R[a,b][c],{a,obs},{b,obs},{c,fusionproduct[a,b]}]//Flatten


(* ::Subsection:: *)
(*Re-gauging*)


newR[a_,b_][c_]/;MemberQ[fusionproduct[a,b],c]:=newR[a,b][c]=R[a,b][c]U[a,b][c]Conjugate[U[b,a][c]]
newRs:=newRs=Table[newR[a,b][c],{a,obs},{b,obs},{c,fusionproduct[a,b]}]//Flatten


regauge[gaugeReplacement_]:=Module[{relabF0,relabR0},
relabF0=Table[X0[a,b,c][d][e,f]->Simplify[newF[a,b,c][d][e,f]//.gaugeReplacement//RootReduce]//Refine,{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[e,c],fusionproduct[a,f]]}]//Flatten;
relabF0=DeleteCases[relabF0,_?(#[[1]]===#[[2]]&)];

relabR0=Table[X0[a,b][c]->Simplify[newR[a,b][c]//.gaugeReplacement//RootReduce]//Refine,{a,obs},{b,obs},{c,fusionproduct[a,b]}]//Flatten;
relabR0=DeleteCases[relabR0,_?(#[[1]]===#[[2]]&)];

Return[<|rgF->relabF0,rgR->relabR0|>];
]


(* ::Subsection::Closed:: *)
(*Hexagons*)


hexagon1[a_,b_,c_]:=hexagon1[a,b,c]=Table[
R[a,c][e]F[a,c,b][d][e,f]R[b,c][f]
==
Sum[F[c,a,b][d][e,g]R[g,c][d]F[a,b,c][d][g,f],{g,fusionproduct[a,b]}]
,{e,fusionproduct[a,c]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,b]]}]

hexagon2[a_,b_,c_]:=hexagon2[a,b,c]=Table[
Conjugate[R[c,a][e]]F[a,c,b][d][e,f]Conjugate[R[c,b][f]]
==
Sum[F[c,a,b][d][e,g]Conjugate[R[c,g][d]]F[a,b,c][d][g,f],{g,fusionproduct[a,b]}]
,{e,fusionproduct[a,c]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,b]]}
]

allhexagons:=allhexagons=Table[{hexagon1[a,b,c],hexagon2[a,b,c]},{a,obs},{b,obs},{c,obs}]//Flatten//DeleteDuplicates;


(* ::Subsection::Closed:: *)
(*Unitarity*)


unitaryR:=unitaryR=
Table[Abs[R[a,b][c]]==1
,{a,obs},{b,obs},{c,fusionproduct[a,b]}]//Flatten//DeleteCases[True]//DeleteDuplicates;


(* ::Subsection::Closed:: *)
(*Premodular*)


\[Theta][x_]:=dim[x]Sum[F[x,x,dual[x]][x][e,unit]Conjugate[F[x,x,dual[x]][x][e,unit]]R[x,x][e],{e,fusionproduct[x,x]}]//RootReduce//FullSimplify

ribbonconditions:=ribbonconditions=Table[R[b,a][c]R[a,b][c]==\[Theta][c]/(\[Theta][a]\[Theta][b]),{a,obs},{b,obs},{c,fusionproduct[a,b]}]//Flatten//DeleteCases[True]//DeleteDuplicates

S:=S=1/dtot[] Table[Sum[\[Theta][x]/(\[Theta][a]\[Theta][dual[b]]) V[a,dual[b]][x]dim[x],{x,fusionproduct[a,dual[b]]}],{a,obs},{b,obs}]//RootReduce//Simplify


(* ::Subsubsection:: *)
(*Symmetric center*)


Z2[]:=Z2[]=Module[{FLAG=False,C={}},
Do[FLAG=False;

Do[
If[FullSimplify[\[Theta][x]/(\[Theta][a]\[Theta][b])]!=1,FLAG=True;Break[]]
,{b,obs},{x,fusionproduct[a,b]}];

If[FLAG==False,AppendTo[C,a]];
,{a,obs}];
Return[C]];
