(* ::Package:: *)

commonBCs=<|Z2->{FR[2,0][1],0,0},Ising->{FR[3,0][1],1,3},RepS3->{FR[3,0][2],0,0},Z3->{FR[3,2][1],0,0},Z2xZ2->{FR[4,0][1],0,0},RepD5->{FR[4,0][3],0,0},Z4->{FR[4,2][1],0,0},RepD4->{FR[5,0][1],3,0},RepD7->{FR[5,0][4],0,0},RepS4->{FR[5,0][6],1,0},Z5->{FR[5,4][1],0,0},Z6->{FR[6,4][1],0,0}|>;


(* ::Subsubsection:: *)
(*Possible braid*)


braidingPossible[2]:={FR[2,0][1],FR[2,0][2]}
braidingPossible[3]:={FR[3,0][1],FR[3,0][2],FR[3,0][3],FR[3,2][1]}
braidingPossible[4]:={FR[4,0][1],FR[4,0][2],FR[4,0][3],FR[4,0][4],FR[4,0][5],FR[4,0][6],FR[4,2][1]}
braidingPossible[5]:={FR[5,0][1],FR[5,0][3],FR[5,0][4],FR[5,0][6],FR[5,0][7],FR[5,0][10],FR[5,4][1]}
braidingPossible[6]:={FR[6,0][1],FR[6,0][2],FR[6,0][4],FR[6,0][5],FR[6,0][6],FR[6,0][7],FR[6,0][8],FR[6,0][9],FR[6,0][14],FR[6,0][16],FR[6,0][18],FR[6,2][2],FR[6,2][3],FR[6,2][4],FR[6,2][7],FR[6,4][1],FR[6,4][2],FR[6,4][3],FR[6,4][5]}


(* ::Subsubsection:: *)
(*Braidings*)


braidings[FR[2,0][1],0]:={0,1}
braidings[FR[2,0][1],1]:={0,1}

braidings[FR[2,0][2],0]:={0,1}


braidings[FR[3,0][1],0|1]:=Range[0,3]

braidings[FR[3,0][2],0]:=Range[0,2]
braidings[FR[3,0][2],1|2]:={}

braidings[FR[3,0][3],0]:={0,1}

braidings[FR[3,2][1],0]:={0,1,2}
braidings[FR[3,2][1],1|2]:={}


braidings[FR[4,0][1],0|1]:={0,1,2,11}
braidings[FR[4,0][1],2|3]:={}

braidings[FR[4,0][2],0|1]:=Range[0,3]

braidings[FR[4,0][3],0]:=Range[0,2]
braidings[FR[4,0][3],1|2]:={}

braidings[FR[4,0][4],0]:={0}

braidings[FR[4,0][5],0]:={0,1,3}

braidings[FR[4,0][6],0]:={0,1}

braidings[FR[4,2][1],0|2]:=Range[0,3]
braidings[FR[4,2][1],1|3]:={}

braidings[FR[4,2][2],0|1|2|3]:={}

braidings[FR[4,2][4],0|1]:={}


braidings[FR[5,0][1],0|1]:={0,1,2,3,4,7}
braidings[FR[5,0][1],2|3]:={0,3,4,7}

braidings[FR[5,0][3],0|1]:={0,2}

braidings[FR[5,0][4],0]:={0,1,3}
braidings[FR[5,0][4],1]:={}

braidings[FR[5,0][6],0|1]:={0}

braidings[FR[5,0][7],0]:={0,1}

braidings[FR[5,0][10],0]:={0,1}

braidings[FR[5,2][1],0|1|2|3]:={}

braidings[FR[5,2][2],0|1]:={}

braidings[FR[5,2][3],0|1]:={}

braidings[FR[5,2][4],0|1]:={}

braidings[FR[5,4][1],0]:={0,1,2}
braidings[FR[5,4][1],1|2]:={}


(*braidings[FR[6,0][1],0|1|2]:=Range[0,4 2^3-1]*)
braidings[FR[6,0][1],0]:={0,1,3,16,17,18}
braidings[FR[6,0][1],1]:={0,1,9,16,17,25}
braidings[FR[6,0][1],2]:={0,1,8,9,16,17,24,25}

braidings[FR[6,0][1],4|6|7]:={}

braidings[FR[6,0][2],0]:=Range[0,2 3-1]
braidings[FR[6,0][2],1|2|4|5]:={}
braidings[FR[6,0][2],3]:=Range[0,5]

braidings[FR[6,0][4],0|1]:=Range[0,7]

braidings[FR[6,0][5],0]:=Range[0,2 3 -1]
braidings[FR[6,0][5],1|2]:={}

braidings[FR[6,0][6],0|1]:={0,1,2,3}

braidings[FR[6,0][7],0]:={0,1,2,4,7}
braidings[FR[6,0][7],1|2|3|6]:={}

braidings[FR[6,0][8],0]:={0,1,2,3,11}
braidings[FR[6,0][8],1|5|7|13]:={}

braidings[FR[6,0][9],0|1|2|3]:={0}

braidings[FR[6,0][14],0]:=Range[0,3]

braidings[FR[6,0][16],0]:=Range[0,1]

braidings[FR[6,0][18],0]:=Range[0,1]

braidings[FR[6,2][2],0|1|2|3|5|7]:={}

braidings[FR[6,2][3],0]:=Range[0,7]
braidings[FR[6,2][3],1]:={}

braidings[FR[6,2][4],0|3]:=Range[0,5]
braidings[FR[6,2][4],1|2|4|5]:={}

braidings[FR[6,2][7],0|2]:={}

braidings[FR[6,4][1],0|3]:=Range[0,5]
braidings[FR[6,4][1],1|2|4|5]:={}

braidings[FR[6,4][2],0|1|2|3]:={}

braidings[FR[6,4][3],0|1|4|5]:={}

braidings[FR[6,4][5],0]:=Range[0,5]
braidings[FR[6,4][5],1|2]:={}
