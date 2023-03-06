(* ::Package:: *)

commonFCs=<|Fib->{FR[2, 0][2],0},Ising->{FR[3, 0][1],1},H2->{FR[6,2][8],2},H3->{FR[6,2][8],3},Z2->{FR[2,0][1],0},RepS3->{FR[3,0][2],0},Z3->{FR[3,2][1],0},Z2xZ2->{FR[4,0][1],0},RepD5->{FR[4,0][3],0},Z4->{FR[4,2][1],0},RepD4->{FR[5,0][1],3},RepD7->{FR[5,0][4],0},RepS4->{FR[5,0][6],1},Z5->{FR[5,4][1],0},Z6->{FR[6,4][1],0}|>


(* ::Subsubsection:: *)
(*Categorifications*)


categorifications[FR[2,0][1]]:={0,1}
categorifications[FR[2,0][2]]:={0}

categorifications[FR[3,0][1]]:={0,1}
categorifications[FR[3,0][2]]:={0,1,2}
categorifications[FR[3,0][3]]:={0};
categorifications[FR[3,2][1]]:={0,1,2}

categorifications[FR[4,0][1]]:={0,1,2,3}
categorifications[FR[4,0][2]]:={0,1}
categorifications[FR[4,0][3]]:={0,1,2}
categorifications[FR[4,0][4]]:={0}
categorifications[FR[4,0][5]]:={0}
categorifications[FR[4,0][6]]:={0}
categorifications[FR[4,2][1]]:={0,1,2,3}
categorifications[FR[4,2][2]]:={0,1,2,3}
categorifications[FR[4,2][4]]:={0,1}

categorifications[FR[5,0][1]]:={0,1,2,3}
categorifications[FR[5,0][3]]:={0,1}
categorifications[FR[5,0][4]]:={0,1}
categorifications[FR[5,0][6]]:={0,1}
categorifications[FR[5,0][7]]:={0}
categorifications[FR[5,0][10]]:={0}
categorifications[FR[5,2][1]]:={0,1,2,3}
categorifications[FR[5,2][3]]:={0,1}
categorifications[FR[5,2][4]]:={0,1}
categorifications[FR[5,4][1]]:={0,1,2}

categorifications[FR[6,0][1]]:={0,1,2,4,6,7}
categorifications[FR[6,0][2]]:={0,1,2,3,4,5}
categorifications[FR[6,0][4]]:={0,1}
categorifications[FR[6,0][5]]:={0,1,2}
categorifications[FR[6,0][6]]:={0,1}
categorifications[FR[6,0][7]]:={0,1,2,3,6}
categorifications[FR[6,0][8]]:={0,1,5,7,13}
categorifications[FR[6,0][9]]:={0,1,2,3}
categorifications[FR[6,0][14]]:={0}
categorifications[FR[6,0][16]]:={0}
categorifications[FR[6,0][18]]:={0}
categorifications[FR[6,2][1]]:={0,1,2,3,4,5}
categorifications[FR[6,2][2]]:={0,1,2,3,5,7}
categorifications[FR[6,2][3]]:={0,1}
categorifications[FR[6,2][4]]:={0,1,2,3,4,5}
categorifications[FR[6,2][7]]:={0,2}
categorifications[FR[6,2][8]]:={0,1,2,3}
categorifications[FR[6,4][1]]:={0,1,2,3,4,5}
categorifications[FR[6,4][2]]:={0,1,2,3}
categorifications[FR[6,4][3]]:={0,1,4,5}
categorifications[FR[6,4][5]]:={0,1,2}
