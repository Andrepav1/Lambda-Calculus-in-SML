
(* ------------------------------------------------------------------------------------------- *)
(* ap103 foundations1 assignment solutions *)
(* Should be able to run the whole file in the terminal *)
(* After the file is used in the terminal, press CTRL+C to terminate the last operation, which is trying to reduce omega. *)
(* ------------------------------------------------------------------------------------------- *)

(* Question 3: Just like I introduced SML terms vx, vy, vz, t1, t2, ... t9 which implement terms in M.
 *  Implement the corresponding terms each of the other sets M', Λ, Λ', M''. *)

(* Classical notation (M) *)
datatype LEXP  	=  	APP of LEXP * LEXP 	| LAM of string *  LEXP 	|  ID of string;

val vx = (ID "x");
val vy = (ID "y");
val vz = (ID "z");
val t1 = (LAM("x",vx));
val t2 = (LAM("y",vx));
val t3 = (APP(APP(t1,t2),vz));
val t4 = (APP(t1,vz));
val t5 = (APP(t3,t3));
val t6 = (LAM("x",(LAM("y",(LAM("z",(APP(APP(vx,vz),(APP(vy,vz))))))))));
val t7 = (APP(APP(t6,t1),t1));
val t8 = (LAM("z", (APP(vz,(APP(t1,vz))))));
val t9 = (APP(t8,t3));

(* Item notation (M') *)
datatype IEXP  	=  	IAPP of IEXP * IEXP | ILAM of string *  IEXP 	|  IID of string;

val ivx = IID "x";
val ivy = IID "y";
val ivz = IID "z";
val it1 = ILAM ("x",ivx);
val it2 = ILAM ("y",ivx);
val it3 = IAPP (ivz, IAPP(it2, it1));
val it4 = IAPP (ivz,it1);
val it5 = IAPP (it3,it3);
val it6 = ILAM("x",(ILAM("y",(ILAM("z",(IAPP((IAPP(ivz,ivy),IAPP(ivz,ivx)))))))));
val it7 = IAPP(it1,IAPP(it1,it6));
val it8 = ILAM ("z", (IAPP(IAPP(ivz,it1),ivz)));
val it9 = IAPP(it3,it8);


(* De Bruijn indices (Λ), terms list: [x,y,z,x',y',z',..] *)
datatype BEXP  	=  	BAPP of BEXP * BEXP | BLAM of BEXP 	|  BID of int;

val bv1 = (BID 1);
val bv2 = (BID 2);
val bv3 = (BID 3);
val bt1 = (BLAM bv1);
val bt2 = (BLAM bv2);
val bt3 = (BAPP(BAPP(bt1,bt2),bv3));
val bt4 = (BAPP(bt1,bv3));
val bt5 = (BAPP(bt3,bt3));
val bt6 = (BLAM((BLAM((BLAM((BAPP(BAPP(bv3,bv1),(BAPP(bv2,bv1))))))))));
val bt7 = (BAPP(BAPP(bt6,bt1),bt1));
val bt8 = (BLAM(BAPP(bv1,BAPP(bt1,bv1))));
val bt9 = (BAPP(bt8,bt3));

(* Another De Bruijn indices (Λ'), terms list: [x,y,z,x',y',z',..] *)
datatype IBEXP  =  	IBAPP of IBEXP * IBEXP | IBLAM of IBEXP  |  IBID of int;

val ibv1 = (IBID 1);
val ibv2 = (IBID 2);
val ibv3 = (IBID 3);
val ibt1 = (IBLAM ibv1);
val ibt2 = (IBLAM ibv2);
val ibt3 = (IBAPP(ibv3, IBAPP(ibt2, ibt1)));
val ibt4 = (IBAPP(ibv3,ibt1));
val ibt5 = (IBAPP(ibt3,ibt3));
val ibt6 = (IBLAM((IBLAM((IBLAM((IBAPP(IBAPP(ibv1,ibv2),(IBAPP(ibv1,ibv3))))))))));
val ibt7 = (IBAPP(ibt1, IBAPP(ibt1,ibt6)));
val ibt8 = (IBLAM((IBAPP((IBAPP(ibv1,ibt1)),ibv1))));
val ibt9 = (IBAPP(ibt3,ibt8)); 

(* Combinatory Logic (M'')*)
datatype COM 	= 	CI  |  CK  |  CS  |  CID of string  |  CAPP of COM * COM;

val cvx = (CID "x");
val cvy = (CID "y");
val cvz = (CID "z");
val ct1 = (CI);
val ct2 = (CAPP (CK,cvx));
val ct3 = (CAPP (CAPP (ct1,ct2),cvz));
val ct4 = (CAPP (ct1,cvz));
val ct5 = (CAPP(ct3,ct3));
val ct6 = (CS);
val ct7 = (CAPP (CAPP(ct6,ct1), ct1));
val ct8 = (CAPP((CAPP(CS,CI)),CI));
val ct9 = (CAPP (ct8,ct3));

(* ------------------------------------------------------------------------------------------------- *)

(* Question 4:  For each of M', Λ, Λ', M'', implement a printing function that prints its elements nicely.
 * Then, test it on every one of the corresponding terms vx, vy, vz, t1, t2, ... t9. *)

(*Prints a term in M notation*)
fun printLEXP (ID v) =
    print v
  | printLEXP (LAM (v,e)) =
    (print "(λ";
     print v;
     print ".";
     printLEXP e;
     print ")")
  | printLEXP (APP(e1,e2)) =
    (print "(";
     printLEXP e1;
     print " ";
     printLEXP e2;
     print ")");

printLEXP(vx);
printLEXP(vy);
printLEXP(vz);
printLEXP(t1);
printLEXP(t2);
printLEXP(t3);
printLEXP(t4);
printLEXP(t5);
printLEXP(t6);
printLEXP(t7);
printLEXP(t8);
printLEXP(t9);

(* prints a term in M' notation *)
fun printIEXP (IID v) = 
	print v
  | printIEXP (ILAM (v, e)) = 
  ( print "[";
	print v;
	print "]";
	printIEXP e )
  | printIEXP (IAPP(e1,e2)) = 
  ( print "<";
	printIEXP e1;
	print ">";
	printIEXP e2 );

printIEXP(ivx);
printIEXP(ivy);
printIEXP(ivx);
printIEXP(it1);
printIEXP(it2);
printIEXP(it3);
printIEXP(it4);
printIEXP(it5);
printIEXP(it6);
printIEXP(it7);
printIEXP(it8);
printIEXP(it9);

(* prints a term in Λ notation *)
fun printBEXP (BID n) =
	print (Int.toString n)
  | printBEXP (BLAM(e)) = 
  ( print "(";
  	print "λ";
  	printBEXP e;
  	print ")" )
  | printBEXP (BAPP(e1,e2)) = 
  ( print "(";
  	printBEXP e1;
  	print " ";
  	printBEXP e2;
  	print ")" );

printBEXP(bv1);
printBEXP(bv2);
printBEXP(bv3);
printBEXP(bt1);
printBEXP(bt2);
printBEXP(bt3);
printBEXP(bt4);
printBEXP(bt5);
printBEXP(bt6);
printBEXP(bt7);
printBEXP(bt8);
printBEXP(bt9);

(* prints a term in Λ' notation *)
fun printIBEXP (IBID n) =
	print (Int.toString n)
  | printIBEXP (IBLAM(e)) = 
  ( print "[]";
  	printIBEXP e )
  | printIBEXP (IBAPP(e1,e2)) = 
  ( print "<";
  	printIBEXP e1;
  	print ">";
  	printIBEXP e2 );

printIBEXP(ibv1);
printIBEXP(ibv2);
printIBEXP(ibv3);
printIBEXP(ibt1);
printIBEXP(ibt2);
printIBEXP(ibt3);
printIBEXP(ibt4);
printIBEXP(ibt5);
printIBEXP(ibt6);
printIBEXP(ibt7);
printIBEXP(ibt8);
printIBEXP(ibt9);

(* prints a term in M'' notation *)
fun printCEXP (CID v) = 
	print v
  | printCEXP (CK) = 
  	print "K''"
  | printCEXP (CS) = 
	print "S''"
  | printCEXP (CI) = 
	print "I''"
  | printCEXP (CAPP(e1,e2)) = 
  ( print "(";
  	printCEXP e1;
  	print " ";
  	printCEXP e2;
  	print ")" );

printCEXP(cvx);
printCEXP(cvy);
printCEXP(cvz);
printCEXP(ct1);
printCEXP(ct2);
printCEXP(ct3);
printCEXP(ct4);
printCEXP(ct5);
printCEXP(ct6);
printCEXP(ct7);
printCEXP(ct8);
printCEXP(ct9);

(* prints CEXP terms in a prettier way *)
fun printprettyCEXP (CID v) = 
	print v
  | printprettyCEXP (CK) = 
  	print "K"
  | printprettyCEXP (CS) = 
	print "S"
  | printprettyCEXP (CI) = 
	print "I"
  | printprettyCEXP (CAPP(e1,e2)) = 
  ( print "(";
  	printprettyCEXP e1;
  	printprettyCEXP e2;
  	print ")" );

(* ------------------------------------------------------------------------------------------------- *)

(* Question 5: Implement in SML the translation functions T, U and V and give these implemented functions here *)

(* Translation function (V: M  -> M' ) *)
fun itran (ID v) = IID(v)
  | itran (LAM (v, e)) = ILAM(v, itran(e))
  | itran (APP(e1,e2)) = IAPP(itran(e2),itran(e1));

(* Auxiliary functions for T and U *)
fun cfree v1 (CID v2) = (v1 = v2)  
  | cfree v (CAPP(e1,e2)) = (cfree v e1) orelse (cfree v e2) 
  | cfree v (CI) = false 
  | cfree v (CK) = false 
  | cfree v (CS) = false; 

fun comfun v1 (CID v2) = if (v1 = v2) 
	then CI
	else CAPP(CK, (CID v2))
  | comfun v (CAPP(e1,e2)) = if not (cfree v (CAPP(e1,e2)))
	then CAPP(CK, CAPP(e1,e2))
	else if not (cfree v e1) andalso ((CID v) = e2) 
		then e1
		else CAPP(CAPP(CS, (comfun v e1)), (comfun v e2))
  | comfun v e = e;


(* Translation function (U: M  -> M'') *)
fun ltoc (ID v) = (CID v)
  | ltoc (LAM(v,e)) = comfun v (ltoc e)
  | ltoc (APP(e1,e2)) = CAPP(ltoc e1,ltoc e2);


(* Translation function (T: M' -> M'') *)
fun itoc (IID v) = (CID v)
  | itoc (ILAM(v,e)) = (comfun v (itoc e))
  | itoc (IAPP(e1,e2)) = (CAPP((itoc e2),(itoc e1)));


(* ------------------------------------------------------------------------------------------------- *)

(* Question 6: Test these functions on all possible translations between these various sets for all the given terms vx, vy, vz, t1, ... t9 
 * and give your output clearly.*)

printIEXP(itran vx);
printIEXP(itran vy);
printIEXP(itran vx);
printIEXP(itran t1);
printIEXP(itran t2);
printIEXP(itran t3);
printIEXP(itran t4);
printIEXP(itran t5);
printIEXP(itran t6);
printIEXP(itran t7);
printIEXP(itran t8);
printIEXP(itran t9);

printCEXP(ltoc vx);
printCEXP(ltoc vy);
printCEXP(ltoc vx);
printCEXP(ltoc t1);
printCEXP(ltoc t2);
printCEXP(ltoc t3);
printCEXP(ltoc t4);
printCEXP(ltoc t5);
printCEXP(ltoc t6);
printCEXP(ltoc t7);
printCEXP(ltoc t8);
printCEXP(ltoc t9);

printCEXP(itoc ivx);
printCEXP(itoc ivy);
printCEXP(itoc ivx);
printCEXP(itoc it1);
printCEXP(itoc it2);
printCEXP(itoc it3);
printCEXP(itoc it4);
printCEXP(itoc it5);
printCEXP(itoc it6);
printCEXP(itoc it7);
printCEXP(itoc it8);
printCEXP(itoc it9);

(* ------------------------------------------------------------------------------------------------- *)

(* Question 7: Deﬁne the subterms in M'' and implement this function in SML. You should give below the formal deﬁnition of subterm'', 
 * its implementation in SML and you need to test on ﬁnding the subterms for all combinator terms that correspond to vx, vy, vz, t1, ... t9. *)

(* Gives the list of subterms *)
fun csubterms (CID v) = [(CID v)]
  | csubterms (CI) = [(CI)]
  | csubterms (CK) = [(CK)]
  | csubterms (CS) = [(CS)]
  | csubterms (CAPP(e1,e2)) = (csubterms e1) @ (csubterms e2) @ [(CAPP(e1,e2))];

(* Checks if term is present in the list *)
fun cfind (e:COM) [] = false
  | cfind e1 [e2] = (e1 = e2)
  | cfind e l = (e = (hd l)) orelse cfind e (tl l);

(* Removes all duplicates from a COM list *)
fun cremoveduplicates [e] = [e]
  | cremoveduplicates l = if not (cfind (hd l) (tl l)) 
  							then [hd l] @ cremoveduplicates (tl l) 
  							else cremoveduplicates (tl l);

(* prints the elements of a COM list in a nicely formatted way *)
fun printprettylist [] = print ""
  | printprettylist [e] = (printCEXP e; print "\n" )
  | printprettylist l = (printCEXP (hd l); print ", \n"; printprettylist (tl l));

(* Removes duplicates and prints them out nicely *)
fun printlistcomb l = printprettylist(cremoveduplicates l);

(* Finds the subterms of a COM term, removes duplicates and prints them out nicely *)
fun printcsubterms t = printlistcomb(csubterms t);

csubterms cvx;
csubterms cvy;
csubterms cvz;
csubterms ct1;
csubterms ct2;
csubterms ct3;
csubterms ct4;
csubterms ct5;
csubterms ct6;
csubterms ct7;
csubterms ct8;
csubterms ct9;

printcsubterms cvx;
printcsubterms cvy;
printcsubterms cvz;
printcsubterms ct1;
printcsubterms ct2;
printcsubterms ct3;
printcsubterms ct4;
printcsubterms ct5;
printcsubterms ct6;
printcsubterms ct7;
printcsubterms ct8;
printcsubterms ct9;

(* ------------------------------------------------------------------------------------------------- *)

(* Question 8: Implement the combinatory reduction rules =c given in the data sheets and use your implementation 
 * to reduce all combinator terms that correspond to vx, vy, vz, t1, ... t9, showing all reduction steps. *)

(* Checks if term is combinatory redex. *)
fun is_credex (CAPP(CI,_)) = true
  | is_credex (CAPP(CAPP(CK,_),_)) = true
  | is_credex (CAPP(CAPP(CAPP(CS,_),_),_)) = true
  | is_credex _ = false;

(* Checks if a term has combinatory redexes in it. *)
fun has_credex (CID v) = false
  | has_credex (CI) = false
  | has_credex (CK) = false
  | has_credex (CS) = false
  | has_credex (CAPP(e1,e2)) = (is_credex (CAPP(e1,e2))) 
  								orelse has_credex e1 
  								orelse has_credex e2;

(* This is the function that implement the actual combinatory reduction. *)
fun cred (CAPP(CI,e1)) = e1 
|     cred (CAPP(CAPP(CK,e1),e2)) = e1 
|     cred (CAPP(CAPP(CAPP(CS,e1),e2),e3)) = (CAPP(CAPP(e1,e3),CAPP(e2,e3)))
|     cred e = e;

(* This function does one combinatory reduction step*)
fun one_creduce (CAPP(e1,e2)) = if is_credex (CAPP(e1,e2)) 
									then cred (CAPP(e1,e2))
									else if (has_credex e1)
										then (CAPP(one_creduce e1,e2))
									else if (has_credex e2)
										then (CAPP(e1,one_creduce e2))
									else (CAPP(e1,e2))
  | one_creduce e = e;

(* Returns a list with all combinatory reduction of one term *)
fun creduce e = if(has_credex e)
					then [e] @ creduce(one_creduce e)
					else [e];

(* Prints a list in a nicely formatted way *)
fun printcredexlist [] = print "\n"
  | printcredexlist [e] = 
  ( printCEXP e;
  	print "\n" )
  | printcredexlist l = 
  ( printCEXP (hd l);
  	print " → \n";
  	printcredexlist (tl l) );

(* This function creates a list of combinatory redex from a term and then prints it out nicely. *)
fun printcreduce e = printcredexlist (creduce e);

printcreduce cvx;
printcreduce cvy;
printcreduce cvz;
printcreduce ct1;
printcreduce ct2;
printcreduce ct3;
printcreduce ct4;
printcreduce ct5;
printcreduce ct6;
printcreduce ct7;
printcreduce ct8;
printcreduce ct9;

(* ------------------------------------------------------------------------------------------------- *)

(* Question 9: For creduce in the above question, implement a counter that counts the number of −->’s used to reach a normal form. *)

fun countprintcreduce e = ( let val templist = (creduce e) in 
  ( printcredexlist templist;
  	print (Int.toString((length templist) - 1));
  	print " steps\n" ) end);

countprintcreduce cvx;
countprintcreduce cvy;
countprintcreduce cvz;
countprintcreduce ct1;
countprintcreduce ct2;
countprintcreduce ct3;
countprintcreduce ct4;
countprintcreduce ct5;
countprintcreduce ct6;
countprintcreduce ct7;
countprintcreduce ct8;
countprintcreduce ct9;

(* ------------------------------------------------------------------------------------------------- *)

(* Question 10: Implement η-reduction on M and test it on many examples of your own. 
 * Give the implementation as well as the test showing all the reduction steps one by one until you reach a η-normal form. *)
(* λv.Av →η A for v not FV(A) *)

(* Return true if a variable is free in another term, otherwise return false *)
fun free v1 (ID v2) = (v1 = v2) |
    free v1 (APP(e1,e2))= (free v1 e1) orelse (free v1 e2) | 
    free v1 (LAM(v2, e1)) = (free v1 e1) andalso not (v1 = v2);

(* Checks if term is eta redex *)
fun is_eredex (LAM(v1,(APP(e,v2)))) = ((ID v1) = v2) andalso (free v1 (APP(e,v2)))
  | is_eredex _ = false;

(* Checks if term has eta redexes *)
fun has_eredex (ID v) = false
  | has_eredex (LAM(v,e)) = is_eredex (LAM(v,e)) orelse has_eredex e
  | has_eredex (APP(e1,e2)) = (has_eredex e1) orelse (has_eredex e2);

(* This function does one eta reduction step *)
fun one_ereduce (ID v) = (ID v)
  | one_ereduce (LAM(v,(APP(e1,e2)))) = if (is_eredex (LAM(v,(APP(e1,e2))))) 
  											then (e1)
  											else if (has_eredex (APP(e1,e2))) 
  												then (one_ereduce (APP(e1,e2)))
  												else (LAM(v,(APP(e1,e2))))
  | one_ereduce (LAM(v,e)) = if(has_eredex e) 
  								then (LAM(v,(one_ereduce e))) 
  								else (LAM(v,e))
  | one_ereduce (APP(e1,e2)) = if (has_eredex e1) 
  								then (APP(one_ereduce e1,e2))
  								else if (has_eredex e2) 
  									then (APP(e1, one_ereduce e2))
  									else  (APP(e1,e2));

(* Creates a list of the sequence of eta redexes for one term *)
fun ereduce e = if (has_eredex e) 
					then [e] @ ereduce (one_ereduce e)
					else [e];

(* Prints out the list of eta redexes in a pretty way *)
fun printeredexlist [] = print "\n"
  | printeredexlist [e] = 
  ( printLEXP e;
  	print "\n" )
  | printeredexlist l = 
  ( printLEXP (hd l);
  	print " → 𝜂\n";
  	printeredexlist (tl l) );

(* This function does everything above, printing a nicely formatted eta reduction sequence. *)
fun printereduce e = printeredexlist (ereduce e);

val t10 = (LAM("x",APP(LAM("y",APP(vz,vy)),vx)));
val t11 = (LAM("z",APP(t10,vz)));
val t12 = (APP(APP(t2,t10),t1));
val t13 = (LAM("x",APP(t12,vx)));

printLEXP(t10);
printLEXP(t11);
printLEXP(t12);
printLEXP(t13);

printereduce t10;
printereduce t11;
printereduce t12;
printereduce t13;


(* ------------------------------------------------------------------------------------------------- *)

(* Question 11: Give an implementation of leftmost reduction in M and test them on a number of rich examples 
 * that show which terminates more and which is more efficient. *)

(* Auxiliary functions for leftmost reduction *)

(*the function below adds lambda id to a list of terms *)
fun addlam id [] = [] |
    addlam id (e::l) = (LAM(id,e))::(addlam id l);

(*Finds a beta redex*)
fun is_redex (APP(LAM(_,_),_)) =
      true
  | is_redex _ =
      false;

(* checks whether a variable is free in a term *)
fun free id1 (ID id2) = if (id1 = id2) then true else false |
    free id1 (APP(e1,e2))= (free id1 e1) orelse (free id1 e2) | 
    free id1 (LAM(id2, e1)) = if (id1 = id2) then false else (free id1 e1);

(* finds new variables which are fresh  in l and different from id*)
fun findme id l = (let val id1 = id^"1"  in if not (List.exists (fn x => id1 = x) l) then id1 else (findme id1 l) end);

(* finds the list of free variables in a term *)
fun freeVars (ID id2)       = [id2]
  | freeVars (APP(e1,e2))   = freeVars e1 @ freeVars e2
  | freeVars (LAM(id2, e1)) = List.filter (fn x => not (x = id2)) (freeVars e1); (* Filter out of the list *)

(*does substitution avoiding the capture of free variables*)
fun subs e id (ID id1) =  if id = id1 then e else (ID id1) |
    subs e id (APP(e1,e2)) = APP(subs e id e1, subs e id e2)|
    subs e id (LAM(id1,e1)) = (if id = id1 then LAM(id1,e1) else
                                   if (not (free id e1) orelse not (free id1 e))
				       then LAM(id1,subs e id e1)
                                   else (let val id2 = (findme id ([id1]@ (freeVars e) @ (freeVars e1)))
					 in LAM(id2, subs e id (subs (ID id2) id1 e1)) end));

(*beta-reduces a redex*)
fun red (APP(LAM(id,e1),e2)) = subs e2 id e1;

(* Checks that the term has at least one beta redex in it *)
fun has_redex (ID id) = false |
    has_redex (LAM(id,e)) = has_redex e|
    has_redex (APP(e1,e2)) = if (is_redex  (APP(e1,e2))) then true else
                              ((has_redex e1) orelse (has_redex e2));

(* Does one leftmost beta reduction step *)
fun one_loreduce (ID id) = (ID id)|
    one_loreduce (LAM(id,e)) = LAM(id, (one_loreduce e))|
    one_loreduce (APP(e1,e2)) = if (is_redex (APP(e1,e2))) then (red (APP(e1,e2))) else
                                 if (has_redex e1) then APP(one_loreduce e1, e2) else
                                 if (has_redex e2) then APP(e1, (one_loreduce e2)) else (APP(e1,e2));


(* Leftmost outermost reduction function *)
fun loreduce (ID id) =  [(ID id)] |
    loreduce (LAM(id,e)) = (addlam id (loreduce e)) |
    loreduce (APP(e1,e2)) = (let val l1 = if (is_redex (APP(e1,e2))) then  (loreduce (red (APP(e1,e2)))) else 
				 if (has_redex e1) then (loreduce (APP(one_loreduce e1, e2))) else 
				 if (has_redex e2) then  (loreduce (APP(e1, (one_loreduce e2)))) else []
				 in [APP(e1,e2)]@l1
			      end);

(*prints elements from a list putting an arrow in between*)
fun printlistreduce [] = ()|
    printlistreduce (e::[]) = (printLEXP e) |
    printlistreduce (e::l) = (printLEXP e; print "-->\n" ; (printlistreduce l));

(* Prints out the leftmost beta reduction sequence *)
fun printloreduce e = (let val tmp =  (loreduce e)
		      in (printlistreduce tmp; print "\n") end);


(* lefmost beta reduction testing*)

val half_omega = (LAM("x",APP(vx,vx)));
val omega = (APP(half_omega,half_omega));
val t14 = (APP(t2,omega)); 
val t15 = (APP(omega,t2)); 
val t16 = (LAM("x", APP(t2, APP(t3,omega))));
val t17 = (LAM("x", APP(t3,omega)));

printloreduce t14; (* This term should terminate *)
printloreduce t15; (* This term should never terminate *)
printloreduce t16; (* This term should terminate *)
printloreduce t17; (* This term should never terminate *)


(* ---------------------- FINITO ---------------------- *)
