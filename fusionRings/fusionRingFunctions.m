(* ::Package:: *)

(* ::Section::Closed:: *)
(*Formatting*)


polarForm=Expand[#/.z_?NumericQ:>Abs[z] Exp[I Arg[z]]]&;


(* ::Section::Closed:: *)
(*Fusion rings*)


fusionproduct[a_,b_]/;MemberQ[obs,a]&&MemberQ[obs,b]:=fusionproduct[a,b]=DeleteCases[Table[If[V[a,b][c]=!=0,c],{c,obs}],Null]
fusionproduct[a_,b_,c__]:=Sort[DeleteDuplicates[Flatten[fusionproduct[#,c]&/@fusionproduct[a,b]]]]

dim[a_]/;MemberQ[obs,a]:=dim[a]=Module[{d0,dimeqs,dimsol,da},
dimeqs=Join[
Outer[d0[#1]d0[#2]==Sum[V[#1,#2][x]d0[x],{x,fusionproduct[#1,#2]}]&,obs,obs]//Flatten
,d0[#]>=1&/@obs];
da:=d0[a]//.Solve[dimeqs][[1]]//RootReduce//FullSimplify//ToRadicals;
Return[da]];

dtot[]:=dtot[]=Sqrt[Total[dim[#]^2&/@obs]]//RootReduce//FullSimplify//ToRadicals
dtot[X_]:=dtot[X]=Sqrt[Total[dim[#]^2&/@X]]//RootReduce//FullSimplify//ToRadicals

dual[a_]/;MemberQ[obs,a]:=dual[a]=Module[{v=Abs[V[a,#][unit]]&/@obs},obs[[Position[v,1][[1,1]]]]]
OverBar[a_]/;MemberQ[obs,a]:=dual[a]

dim[{A__}]:=Sum[dim[a],{a,{A}}]
