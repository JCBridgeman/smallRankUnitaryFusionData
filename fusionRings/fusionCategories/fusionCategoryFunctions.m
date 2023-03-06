(* ::Package:: *)

(* ::Section::Closed:: *)
(*Nice tricks*)


Unprotect[Abs,Conjugate,Power,Times,Sqrt,Im,Re];
Abs[\[Phi][a_,b_,c_][d_][e_,f_]]:=1;
Conjugate[\[Phi][a_,b_,c_][d_][e_,f_]]:=1/\[Phi][a,b,c][d][e,f];

Abs[\[Zeta][a_,b_,c_][d_][e_,f_]]:=1;
Conjugate[\[Zeta][a_,b_,c_][d_][e_,f_]]:=\[Zeta][a,b,c][d][e,f];
\[Zeta][a_,b_,c_][d_][e_,f_]^m_Integer/;(m<0||m>1):=\[Zeta][a,b,c][d][e,f]^Mod[m,2]

Abs[\[CurlyPhi][x_Integer][a_,b_,c_][d_][e_,f_]]:=1
Conjugate[\[CurlyPhi][x_Integer][a_,b_,c_][d_][e_,f_]]:=\[CurlyPhi][x][a,b,c][d][e,f]^-1
\[CurlyPhi][x_Integer][a_,b_,c_][d_][e_,f_]^m_Integer/;(m<0||m>=x):=\[CurlyPhi][x][a,b,c][d][e,f]^Mod[m,x]

Conjugate[Re0[a_,b_,c_][d_][e_,f_]]:=Re0[a,b,c][d][e,f];
Re[Re0[a_,b_,c_][d_][e_,f_]]:=Re0[a,b,c][d][e,f];
Im[Re0[a_,b_,c_][d_][e_,f_]]:=0;
Abs[Re0[a_,b_,c_][d_][e_,f_]]:=Sign[Re0[a,b,c][d][e,f]];

Conjugate[P0[a_,b_,c_][d_][e_,f_]]:=P0[a,b,c][d][e,f];
Re[P0[a_,b_,c_][d_][e_,f_]]:=P0[a,b,c][d][e,f];
Im[P0[a_,b_,c_][d_][e_,f_]]:=0;
Abs[P0[a_,b_,c_][d_][e_,f_]]:=P0[a,b,c][d][e,f];
Protect[Abs,Conjugate,Power,Times,Sqrt];


(* ::Section:: *)
(*F-Symbols*)


F[a_,b_,c_][d_][e_,f_]:=0

F[a_,b_,u_][c_][c_,b_]/;(u===unit&&MemberQ[fusionproduct[a,b],c]):=1;
F[a_,u_,b_][c_][a_,b_]/;(u===unit&&MemberQ[fusionproduct[a,b],c]):=1;
F[u_,a_,b_][c_][a_,c_]/;(u===unit&&MemberQ[fusionproduct[a,b],c]):=1;

F[a_,b_,c_][d_][e_,f_]/;MemberQ[fusionproduct[a,b],e]&&MemberQ[fusionproduct[b,c],f]&&MemberQ[Intersection[fusionproduct[e,c],fusionproduct[a,f]],d]:=F[a,b,c][d][e,f]=
X0[a,b,c][d][e,f]//.repF0


Fs:=Fs=Table[F[a,b,c][d][e,f],{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,c]]}]//Flatten


leftBasis[a_,b_,c_][d_]:=Flatten[Table[{\[Alpha]0,e,\[Beta]0},{e,Select[fusionproduct[a,b],V[#,c][d]>0&]},{\[Alpha]0,V[a,b][e]},{\[Beta]0,V[e,c][d]}],2];
rightBasis[a_,b_,c_][d_]:=Flatten[Table[{\[Sigma]0,f,\[Tau]0},{f,Select[fusionproduct[b,c],V[a,#][d]>0&]},{\[Sigma]0,V[b,c][f]},{\[Tau]0,V[a,f][d]}],2];

Fblk[a_,b_,c_][d_]:=Table[F[a,b,c][d][l[[2]],r[[2]]],{l,leftBasis[a,b,c][d]},{r,rightBasis[a,b,c][d]}];


(* ::Text:: *)
(*Line bending*)


\[Kappa][x_]:=F[x,dual[x],x][x][unit,unit]dim[x]//FullSimplify


A[a_,b_][c_]:=Conjugate[F[dual[a],a,b][b][unit,c]]Sqrt[(dim[a]dim[b])/dim[c]]
A[c_][a_,b_]:=F[dual[a],a,b][b][unit,c]Sqrt[(dim[a]dim[b])/dim[c]]

B[a_,b_][c_]:=F[a,b,dual[b]][a][c,unit]Sqrt[(dim[a]dim[b])/dim[c]]
B[c_][a_,b_]:=Conjugate[F[a,b,dual[b]][a][c,unit]]Sqrt[(dim[a]dim[b])/dim[c]]


(* ::Subsection:: *)
(*Re-gauging F*)


newF[a_,b_,c_][d_][e_,f_]/;MemberQ[fusionproduct[a,b],e]&&MemberQ[fusionproduct[b,c],f]&&MemberQ[Intersection[fusionproduct[e,c],fusionproduct[a,f]],d]:=newF[a,b,c][d][e,f]=
F[a,b,c][d][e,f]U[b,c][f]U[a,f][d]Conjugate[U[a,b][e]U[e,c][d]]

U[a_,u_][b_]/;(u===unit):=1;
U[u_,a_][b_]/;(u===unit):=1;

newFs:=newFs=Table[newF[a,b,c][d][e,f],{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,c]]}]//Flatten

Unprotect[Abs,Conjugate,Power,Times,Sqrt,Im,Re];
Conjugate[U[a_,b_][c_]]/;V[a,b][c]==1:=1/U[a,b][c]
Abs[U[a_,b_][c_]]/;V[a,b][c]==1:=1
Protect[Abs,Conjugate,Power,Times,Sqrt];


new\[Kappa][x_]:=newF[x,dual[x],x][x][unit,unit]dim[x]//FullSimplify


regauge[gaugeReplacement_]:=Module[{relabF0},
relabF0=Table[X0[a,b,c][d][e,f]->Simplify[newF[a,b,c][d][e,f]//.gaugeReplacement//RootReduce]//Refine,{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[e,c],fusionproduct[a,f]]}]//Flatten;
Return[<|rgF->DeleteCases[relabF0,_?(#[[1]]===#[[2]]&)]|>]
]


(* ::Subsection::Closed:: *)
(*Pentagons*)


pentagons[a_,b_,c_,d_]:=pentagons[a,b,c,d]=Table[
F[f,c,d][e][g,x]F[a,b,x][e][f,y]==
Sum[F[a,b,c][g][f,z]F[a,z,d][e][g,y]F[b,c,d][y][z,x],{z,fusionproduct[b,c]}]
,{f,fusionproduct[a,b]},{g,fusionproduct[f,c]},{x,fusionproduct[c,d]},{y,fusionproduct[b,x]},{e,Intersection[fusionproduct[g,d],fusionproduct[a,y]]}
]//Flatten//DeleteCases[True]

allpentagons:=allpentagons=Table[pentagons[a,b,c,d],{a,obs},{b,obs},{c,obs},{d,obs}]//Flatten//DeleteCases[True]//DeleteDuplicates

nicepentagons:=nicepentagons=Module[
{a,b,c,d,pents={}},
Do[If[Length[fusionproduct[b,c]]==1,
AppendTo[pents,Table[pentagons[a,b,c,d],{a,obs},{d,obs}]//Flatten]
],{b,obs},{c,Select[obs,fusionproduct[b,#]==1&]}];
Return[pents//Flatten//DeleteDuplicates];
]


(* ::Subsection::Closed:: *)
(*Unitarity*)


unitaryFL:=unitaryFL=Table[
Sum[F[a,b,c][d][e,f]Conjugate[F[a,b,c][d][g,f]],{f,fusionproduct[b,c]}]==If[e===g,1,0]
,{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{g,fusionproduct[a,b]},{d,fusionproduct[e,c]}]//Flatten//DeleteCases[True]//DeleteDuplicates

unitaryFR:=unitaryFR=Table[
Sum[F[a,b,c][d][f,e]Conjugate[F[a,b,c][d][f,g]],{f,fusionproduct[a,b]}]==If[e===g,1,0]
,{a,obs},{b,obs},{c,obs},{e,fusionproduct[b,c]},{g,fusionproduct[b,c]},{d,fusionproduct[a,e]}]//Flatten//DeleteCases[True]//DeleteDuplicates

unitaryF:=unitaryF=Join[unitaryFL,unitaryFR]//DeleteDuplicates;


(* ::Subsection::Closed:: *)
(*Symmetries*)


(* ::Subsubsection::Closed:: *)
(*Tetrahedral action*)


symmetry1:=symmetry1=
Table[
F[a,b,c][d][e,f]==1/Abs[\[Kappa][a]]^2 Sqrt[(dim[e]dim[f])/(dim[b]dim[d])]A[d][a,f]A[a,b][e]Conjugate[F[dual[a],e,c][f][b,d]],
{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,c]]}]//Flatten(*//RootReduce*)//DeleteCases[True]//DeleteDuplicates;

symmetry2:=symmetry2=
Table[
F[a,b,c][d][e,f]==1/Conjugate[\[Kappa][b]] Sqrt[(dim[e]dim[f])/(dim[a]dim[c])]A[f][b,c]B[a,b][e]Conjugate[F[e,dual[b],f][d][a,c]],
{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,c]]}]//Flatten(*//RootReduce*)//DeleteCases[True]//DeleteDuplicates;

symmetry3:=symmetry3=
Table[
F[a,b,c][d][e,f]==1/Abs[\[Kappa][c]]^2 Sqrt[(dim[e]dim[f])/(dim[b]dim[d])]B[f][b,c]B[e,c][d]Conjugate[F[a,f,dual[c]][e][d,b]],
{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,c]]}]//Flatten(*//RootReduce*)//DeleteCases[True]//DeleteDuplicates;


(* ::Subsubsection::Closed:: *)
(*Dagger structure*)


pivotalstar:=Table[
F[a,b,c][d][e,f]==1/(Conjugate[\[Kappa][a]]\[Kappa][c]) A[d][a,f]A[dual[a],e][b]B[f][b,c]B[d,dual[c]][e]F[dual[a],d,dual[c]][b][f,e],{a,obs},{b,obs},{c,obs},{e,fusionproduct[a,b]},{f,fusionproduct[b,c]},{d,Intersection[fusionproduct[a,f],fusionproduct[e,c]]}]//Flatten//DeleteDuplicates//DeleteCases[True];
