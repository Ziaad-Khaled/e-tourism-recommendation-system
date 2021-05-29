
% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 4), hotel).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel, 100).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
possibleSubset(L,R):-
	possibleSubset1(L,N),
	perm(N,R).

possibleSubset1([], []).

possibleSubset1([E|Tail], [E|NTail]):-
	possibleSubset1(Tail, NTail).
possibleSubset1([E|Tail], NTail):-
	possibleSubset1(Tail, NTail).

perm([H|T],L) :- perm(T,P), insert(H,P,L).
perm([],[]).
insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :- insert(X,T,T1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
choosePreferences(Prefs, ChosenPreferences):-
	\+member(activity(_),Prefs),
	possibleSubset(Prefs,ChosenPreferences).
choosePreferences(Prefs, ChosenPreferences):-
	member(activity(A),Prefs),
	remove(activity(A),Prefs,NPrefs),
	possibleSubset(A,N),
	possibleSubset([activity(N)|NPrefs],ChosenPreferences).
	
remove(A,[],[]).
remove(A,[A|T],T).
remove(A,[B|T],[B|L]):-
	B\=A,
	remove(A,T,L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
preferenceSatisfaction(Offer,Customer,ChosenPrefs,S):-
	relevance_Mean(Offer,Customer,ChosenPrefs,S1),
	relevance_Acc(Offer,Customer,ChosenPrefs,S2),
	relevance_Act(Offer,Customer,ChosenPrefs,S3),
	S is S1+S2+S3.

relevance_Mean(Offer,Customer,[],0).
relevance_Mean(Offer,Customer,[mean(M)|T],S):-
	relevance_Mean(Offer,Customer,T,S1),
	offerMean(Offer,M),
	customerPreferredMean(Customer,M,S2),
	S is S1 + S2.

relevance_Mean(Offer,Customer,[mean(M)|T],0):-
	offerMean(Offer,N),
	M\=N.
relevance_Mean(Offer,Customer,[H|T],S):-
	H\=mean(M),
	relevance_Mean(Offer,Customer,T,S).
	
relevance_Acc(Offer,Customer,[],0).
relevance_Acc(Offer,Customer,[accommodation(A)|T],S):-
	offerAccommodation(Offer,A),
	customerPreferredAccommodation(Customer,M,S).

relevance_Acc(Offer,Customer,[accommodation(A)|T],0):-
	offerAccommodation(Offer,B),
	B\=A.
relevance_Acc(Offer,Customer,[H|T],S):-
	H\=accommodation(A),
	relevance_Acc(Offer,Customer,T,S).
	
relevance_Act(Offer,Customer,[],0).
relevance_Act(Offer,Customer,[activity(L)|T],S):-
	sum_relevance_Activ(Offer,Customer,L,S).
relevance_Act(Offer,Customer,[H|T],S):-
	H\=activity(L),
	relevance_Act(Offer,Customer,T,S).
	
	
sum_relevance_Activ(O,C,[],0).
sum_relevance_Activ(Offer,Customer,[Act1|T],S):-
	getAct(Offer,Act),
	member(Act1,Act),
	customerPreferredActivity(Customer,Act1,S1),
	sum_relevance_Activ(Offer,Customer,T,S2),
	S is S1 + S2.
sum_relevance_Activ(Offer,Customer,[Act1|T],S):-
	getAct(Offer,Act),
	\+member(Act1,Act),
	sum_relevance_Activ(Offer,Customer,T,S).
	
getAct(offer(_, Act, _, _, _, _, _, _),Act).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
later(Y1-M1-D1,Y2-M2-D2):-
	Y1>Y2.
later(Y1-M1-D1,Y2-M2-D2):-
	Y1=Y2,
	M1>M2.
later(Y1-M1-D1,Y2-M2-D2):-
	Y1=Y2,
	M1=M2,
	D1>D2.

overlapPeriod(period(D1,D2), period(D3,D4)):-
	later(D3,D1),
	later(D2,D3).
overlapPeriod(period(D1,D2), period(D3,D4)):-
	later(D1,D3),
	later(D4,D1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
getOffer(ChosenPrefs,Offer):-
	destOffers(ChosenPrefs,Offer),
	periodOffers(ChosenPrefs,Offer),
	meanOffers(ChosenPrefs,Offer),
	activityOffers(ChosenPrefs,Offer),
	budgetOffers(ChosenPrefs,Offer),
	accommodationOffers(ChosenPrefs,Offer).
	
%%%%
destOffers([],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).
	
destOffers([dest(DEST)|T],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).

destOffers([H|T],Offer):-
	H\=dest(M),
	destOffers(T,Offer).
%%%%
periodOffers([],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).
	
periodOffers([period(D1, D2)|T],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2,P, X, Y), _),	
	overlapPeriod(period(D1,D2), P),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).

periodOffers([H|T],Offer):-
	H\=period(D1, D2),
	periodOffers(T,Offer).
%%%%
meanOffers([],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).
	
meanOffers([mean(M)|T],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2,period(D3, D4), X, Y), M),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).

meanOffers([H|T],Offer):-
	H\=mean(M),
	meanOffers(T,Offer).
%%%%
activityOffers([],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).
	
activityOffers([activity(A1)|T],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2,period(D3, D4), X, Y), M),
	possibleSubset(A,A1),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).

activityOffers([H|T],Offer):-
	H\=activity(M),
	activityOffers(T,Offer).
%%%%
budgetOffers([],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).
	
budgetOffers([budget(B1)|T],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2,P, X, Y), M),
	B1>B,
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).

budgetOffers([H|T],Offer):-
	H\=budget(M),
	budgetOffers(T,Offer).
%%%%
accommodationOffers([],Offer):-
	offerMean(offer(DEST, A, B, DET1, DET2, P, X, Y), _),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).
	
accommodationOffers([accommodation(AA)|T],Offer):-
	offerAccommodation(offer(DEST, A, B, DET1, DET2,period(D3, D4), X, Y), AA),
	Offer=offer(DEST, A, B, DET1, DET2, P, X, Y).

accommodationOffers([H|T],Offer):-
	H\=accommodation(M),
	accommodationOffers(T,Offer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
	choosePreferences(Prefs,ChosenPrefs),
	getOffer(ChosenPrefs,O).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
	offerMean(offer(A, B, C,D, E, F, G, H), _),
	Offer=offer(A, B, C,D, E, F, G, H),
	getSatisfactionList(Customers,PreferenceList,Offer,SList),
	mergingLists(Customers,SList,MergedCustomerSatisfactionList),
	sortedSatisfactionCustomers(MergedCustomerSatisfactionList, SortedMergedCustomers),
	chooseCustomers(Offer,SortedMergedCustomers,CustomersChosen).



			
getSatisfactionList([],[],Offer,[]).
getSatisfactionList([C1|C],[P1|P],Offer,PreferenceList):-
	preferenceSatisfaction(Offer, C1, P1, S),
	getSatisfactionList(C,P,Offer,PreferenceList1),
	append([S],PreferenceList1,PreferenceList).
	
	
mergingLists([],[],[]).	
mergingLists([C1|C],[S1|S],MergedCustomerSatisfactionList):-
		mergingLists(C,S,MergedCustomerSatisfactionList1),
		Element=C1+S1,
		append([Element],MergedCustomerSatisfactionList1,MergedCustomerSatisfactionList).




sortedSatisfactionCustomers([A|B], Sorted) :- sortedSatisfactionCustomers(B, SortedTail), insert1(A, SortedTail, Sorted).
sortedSatisfactionCustomers([], []).

insert1(customer(X1,X2,X3,X4,X5,X6)+A, [customer(Y1,Y2,Y3,Y4,Y5,Y6)+B|C], [customer(Y1,Y2,Y3,Y4,Y5,Y6)+B|D]) :- A @< B, !, insert1(customer(X1,X2,X3,X4,X5,X6)+A, C, D).
insert1(customer(X1,X2,X3,X4,X5,X6)+A, C, [customer(X1,X2,X3,X4,X5,X6)+A|C]).  




chooseCustomers(offer(A, B, C, D, E, F, G, Num),Customers,CustomersChosen):-
	chooseCustomersHelper(Num,Customers,CustomersChosenWithPreference),
	deletingThePreference(CustomersChosenWithPreference,CustomersChosen).

chooseCustomersHelper(0,X,[]).
chooseCustomersHelper(Num,[],[]).
chooseCustomersHelper(Num,[C1|C],CustomersChosen):-
	Num>0,
	Num1 is Num-1,
	chooseCustomersHelper(Num1,C,CustomersChosen1),
	append([C1],CustomersChosen1,CustomersChosen).
	
deletingThePreference([],[]).
deletingThePreference([customer(X1,X2,X3,X4,X5,X6)+_|C],EditedList):-
		deletingThePreference(C,EditedList1),
		append([customer(X1,X2,X3,X4,X5,X6)],EditedList1,EditedList).
		

getAllActivities(L):-
setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).
		
mostPreferredActivity(C,A):-
getAllActivities(L),
comparing(L,C,A).


comparing([H],_,H).
comparing([H,X|T],C,A):-
customerPreferredActivity(C, H, HNum),
customerPreferredActivity(C, X, XNum),
HNum>=XNum,
comparing([H|T],C,A).

comparing([H,X|T],C,A):-
customerPreferredActivity(C, H, HNum),
customerPreferredActivity(C, X, XNum),
HNum<XNum,
comparing([X|T],C,A).








	
		


		
	
