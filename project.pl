convertBinToDec(B,D):-
						convertBinToDec(B,0,D).

convertBinToDec(0,_,0).

convertBinToDec(B,X,D):-
						B > 0,	
						B0 is B mod 10,
						B_new is B // 10,
						X1 is X + 1,
						convertBinToDec(B_new,X1,D1),
						D is D1 + B0 * (2 ** X).
	

replaceIthItem(Item,List,I,Result):-
						replaceIthItem(Item,List,0,I,Result).

replaceIthItem(_,[],_,_,[]).

replaceIthItem(Item,[_|T1],I,I,[Item|T2]):-
						Nxt is I + 1,
						replaceIthItem(Item,T1,Nxt,I,T2).

replaceIthItem(Item,[H|T1],Cur,I,[H|T2]):-
						Cur \= I,	
						Nxt is Cur + 1,
						replaceIthItem(Item,T1,Nxt,I,T2).

splitEvery(N,L,[L]):-
						length(L,X),
						X \= 0,
						X =< N.
splitEvery(N,L,[S|R]):-
						splitEvery_helper(N,0,L,Rest,[],S),
						splitEvery(N,Rest,R).
	
	
splitEvery_helper(N,N,L,L,Acc,Acc).
splitEvery_helper(N,X,[H|T],Rest,Acc,S):-
						X < N,
						X1 is X + 1,
						append(Acc,[H],Acc1),
						splitEvery_helper(N,X1,T,Rest,Acc1,S).	
	

logBase2(X,0):- X =< 1.
logBase2(N,R):- 	
				N>1,
				N1 is N/2,
				logBase2(N1,R1),
				R is R1+1.

getNumBits(_,fullyAssoc,_,0).
getNumBits(_,directMap,L,BitsNum):-
						length(L,X),
						logBase2(X,BitsNum).
getNumBits(X,setAssoc,_,S):-
						logBase2(X,S).


fillZeros(String,N,R):- 
						fillZeros_helper(N,S),
						string_concat(S,String,R).
fillZeros_helper(0,"").
fillZeros_helper(R,S):- 
						R>0,
						R1 is R-1,
						fillZeros_helper(R1,S1),
						string_concat("0",S1, S).
			
% DirectMap			

splitTagIndex(StringAddress,TagFinal,Index,BitsNum):-
			number_string(NumberAddress,StringAddress),
			Index is NumberAddress mod (10**BitsNum),
			TagNum  is NumberAddress //(10**BitsNum),
			number_string(TagNum ,TagStr),
			string_length(StringAddress,L1),
			string_length(TagStr , L2),
			 N is L1 - BitsNum - L2,			
			fillZeros(TagStr,N,TagFinal).
							
getDataFromCache(StringAddress,Cache,Data,HopsNum,directMap,BitsNum):-
			splitTagIndex(StringAddress,TagFinal,Index,BitsNum),
			convertBinToDec(Index, IndDec),
			nth0(IndDec,Cache,item(tag(T1),data(D1),ValidBit,_)),
			T1 = TagFinal,
			ValidBit = 1,
			Data = D1,
			HopsNum = 0.

getDataFromCache(StringAddress,Cache,Data,HopsNum,fullyAssoc,_) :-
			fullyAssoc_helper(StringAddress,Cache,Data,0,HopsNum).
			
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	number_string(Address,StringAddress),
	convertAddress(Address,SetsNum,Tag,Idx,setAssoc),
	convertBinToDec(Idx,IndexNum),
	length(Cache,Len),
	Lenn is Len//SetsNum,
	splitEvery(Lenn,Cache,SplitCache),
	nth0(IndexNum,SplitCache,L),
	setHelper(Tag,L,Data,0,HopsNum).

			

setHelper(Address,[H|_],Data,Cur,Cur) :-
			H = item(tag(StringAddress),data(Data),1,_),
			number_string(Address,StringAddress).
			
setHelper(Address,[H|T],Data,Cur,HopsNum) :-
			H = item(tag(StringAddress),_,_,_),
			number_string(Address1,StringAddress),
			Address1 \= StringAddress,
			Nxt is Cur + 1,
			setHelper(Address,T,Data,Nxt,HopsNum).
	
	
convertAddress(Bin,BitsNum,Tag,Idx,directMap):- 
							Idx is Bin mod (10**BitsNum),
							Tag is Bin //(10**BitsNum).	

convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
	logBase2(SetsNum,IdxBits),
	Idx is Bin mod (10**IdxBits),
	Tag is Bin // (10**IdxBits).
	
convertAddress(Bin,_,Bin,_,fullyAssoc).

										

fullyAssoc_helper(StringAddress,[H|_],Data,Cur,Cur) :-
			H = item(tag(Tag),data(Data),1,_),
			atom_number(Tag,N),
			atom_number(StringAddress,N).
			
				
fullyAssoc_helper(StringAddress,[H|T],Data,Cur,HopsNum) :-
			H = item(tag(Tag),_,_,_),
			atom_number(Tag,X1),
			atom_number(StringAddress,X2),
			X1 \= X2,
			Nxt is Cur + 1,
			fullyAssoc_helper(StringAddress,T,Data,Nxt,HopsNum).		
			
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum) :-
			atom_number(Ta,Tag),
			atom_number(Id,Idx),
			len(Idx,L),
			Y is BitsNum - L,
			fillZeros(Id,Y,I),
			len(Tag,Len),
			X is 6 - Len - BitsNum,
			fillZeros(Ta,X,T),
			string_concat(T,I,Z),
			atom_number(Z,N),
			convertBinToDec(N,Dec),
			get(Mem,Dec,0,ItemData),
			convertBinToDec(Idx,D),
			update_direct(T,D,0,ItemData,OldCache,NewCache).
			

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
			atom_number(Ta,Tag),
			atom_number(Id,Idx),
			logBase2(SetsNum,BitsNum),
			len(Idx,Le),
			Y is BitsNum - Le,
			fillZeros(Id,Y,I),
			len(Tag,Len),
			X is 6 - Len - BitsNum,
			fillZeros(Ta,X,T),
			string_concat(T,I,Z),
			atom_number(Z,N),
			convertBinToDec(N,Dec),
			get(Mem,Dec,0,ItemData),
			splitEvery(SetsNum,OldCache,L),
			update_setAssoc(T,Idx,0,ItemData,L,Cache),
			flatten(Cache,NewCache).

replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :-
		convertBinToDec(Tag,Dec),
		get(Mem,Dec,0,ItemData),
		atom_number(StringTag,Tag),
		len(Tag,Len),
		X is 6 - Len,
		fillZeros(StringTag,X,R),
		update(R,ItemData,OldCache,Cache),
		updateOrder(Cache,NewCache).	
	
replaceInCache(Tag,_,Mem,OldCache,NewCache,ItemData,fullyAssoc,_) :-
		convertBinToDec(Tag,Dec),
		get(Mem,Dec,0,ItemData),
		atom_number(StringTag,Tag),
		len(Tag,Len),
		X is 6 - Len,
		fillZeros(StringTag,X,R),
		\+ update(R,ItemData,OldCache,_),
		max(OldCache,0,Max),
		update2(R,ItemData,Max,OldCache,Cache),
		updateOrder(Cache,NewCache).		

update_direct(Tag,Idx,Idx,Data,[_|R],[item(tag(Tag),data(Data),1,0)|R]).

update_direct(Tag,Idx,Cur,Data,[H|L],[H|R]):-
			Cur \= Idx,
			Cur1 is Cur + 1,
			update_direct(Tag,Idx,Cur1,Data,L,R).

update_setAssoc(Tag,Idx,Cur,Data,[H|L],[H|R]) :-			
			Cur \= Idx,
			Cur1 is Cur + 1,
			update_setAssoc(Tag,Idx,Cur1,Data,L,R).
			
update_setAssoc(Tag,Idx,Idx,Data,[H|T],[R|T]) :-
			update(Tag,Data,H,C),
			updateOrder(C,R).
	
update_setAssoc(Tag,Idx,Idx,Data,[H|T],[R|T]) :-
			\+ update(Tag,Data,H,R),
			max(H,0,Max),
			update2(Tag,Data,Max,H,C),
			updateOrder(C,R).

update2(Tag,Data,Max,[item(_,_,_,Max)|T],[item(tag(Tag),data(Data),1,-1)|T]).

update2(Tag,Data,Max,[H|OldCache],[H|NewCache]) :-
		H = item(_,_,_,Order),
		Order \= Max,
		update2(Tag,Data,Max,OldCache,NewCache).

update(Tag,Data,[item(_,_,0,_)|T],[item(tag(Tag),data(Data),1,-1)|T]).

update(Tag,Data,[H|OldCache],[H|NewCache]) :-
		H = item(_,_,1,_),
		update(Tag,Data,OldCache,NewCache).

len(0,1).
len(1,1).
len(N,M):-
	N > 1,
	N1 is N//10,
	len(N1,M1),
	M is M1 + 1.
	
max([],M,M).	
max([item(_,_,_,Order)|T],Cur,Max):-
	Cur > Order,
	max(T,Cur,Max).
max([item(_,_,_,Order)|T],Cur,Max):-
	Cur =< Order,
	max(T,Order,Max).
	
get([H|_],Idx,Idx,H).	
get([_|T],Idx,Cur,Data):-
	Cur \= Idx,
	Cur1 is Cur + 1,
	get(T,Idx,Cur1,Data).

updateOrder([],[]).

updateOrder([item(Tag,D,1,O)|T],[item(Tag,D,1,O1)|Set1]):-
		O1 is O+1,
		updateOrder(T,Set1).

updateOrder([item(Tag,D,0,O)|T],[item(Tag,D,0,O)|Set1]):-
		updateOrder(T,Set1).


getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
	getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	NewCache = OldCache.
	
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
	\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
	atom_number(StringAddress,Address),
	convertAddress(Address,BitsNum,Tag,Idx,Type),
	replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).


runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
	getNumBits(NumOfSets,Type,OldCache,BitsNum),
	(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
	getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
	runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).




	



							