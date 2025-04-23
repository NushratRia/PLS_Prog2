(* Map function *)
fun mymap f nil = nil
| mymap f (h::t) = (f h) :: (mymap f t)


(*myfoldr function: The right fold operation on lists is performed by this function. *)

fun myfoldr f b nil = b
| myfoldr f b (h::t) = f (h, myfoldr f b t)



type Subst = string * int -> Term

(* null subst *)
fun null x = Var x

fun valu S (Var v) = S v
| valu S (Fun (f, args)) = Fun (f, mymap (valu S) args)

fun compose (S, R) v = valu S (R v)
fun compose (S, R) = fn v => valu S (R v)

fun updt (v, t) S = compose (fn w => if w = v then t else Var w, S)

(* The function accepts two lists as inputs while it forms pairs between their elements.*)
exception nomatch and occursin_check and lngth;
fun
	pairs (nil, nil)	= nil		        |
	pairs (a::b, c::d)	= (a,c)::(pairs (b,d))  |
	pairs (_)		= raise lngth;


fun
	occursin v (Var w)	        = (v=w)	        |
	occursin v (Fun (O, args))	= List.exists (occursin v) args;

(* unify' function*)

fun unify' ((t1,t2),S)=
let
	val t1' = valu S t1 and t2' = valu S t2;    
in
	case (t1', t2') of 
	(Var v,Var w)=>if (v=w) then S else updt(v,t2')S |
       (Var v, _)=>if occursin v t2 then raise occursin_check else updt(v,t2')S |
       (_, Var w)=>if occursin w t1 then raise occursin_check else updt(w,t1')S |
       (Fun (o1, tlist1), Fun (o2, tlist2))=> 
       if (o1=o2) then 
       myfoldr unify' S (pairs (tlist1, tlist2)) 
       else raise nomatch
end;

(* Wrapper around unify' *)
fun dounify (t1,t2, S) =
	(unify' ((t1,t2),S))
	handle occursin_check => raise nomatch
	| lngth => raise nomatch

(* The rnm function applies recursive renaming procedures to all arguments it receives. *)
fun rnm l(Var (x,_))= Var (x,l)|
	rnm l (Fun(f,args))=Fun(f, mymap(rnm l) args)

(* The recursive behavior of gathers function allows it to retrieve variables from all its input arguments. *)
fun gathers (v as Var _, vars)      = v::vars
  |  gathers (Fun (_, args), vars) = myfoldr gathers vars args

(* rmv function removes duplicate elements from a list while preserving the order of elements *)
fun rmv [] = []
 | rmv (x::xs) = x::rmv(List.filter (fn y => y <> x) xs)

fun pairlst (nil,nil)  = nil
 | pairlst ((x::xs),(y::ys))= (x,y)::pairlst (xs,ys)

exception no_sol;


(*This function takes a list of goals and a database as input and attempts to solve the goals using the given database.
 It iterates through the list of goals and attempts to match each goal with the rules in the database. 
 If a solution is found, it returns the resulting substitution. If no solution is found, 
 it raises the no_sol exception.*)

fun slvr(goals, db)=
	let
		fun slv(nil,_,_,S)= S 
		| slv(_,nil,_,_)= raise no_sol
                | slv(_,Headless _::_,_,_) = null
		| slv(goal::goals, Headed(head,tail)::rules, l, S)=
               let
                        val h = rnm l(head)
                        val SN = dounify(goal,h,S)
                        val t = mymap (rnm l) tail
                        val t' = mymap (valu SN) t
                        val g' = mymap (valu SN) goals
                        val ap= t' @ g';                      
               in
                       slv(ap, db, l+1, compose(SN,S))

                       handle no_sol => slv(goal::goals,rules,l,S)
               end
                       
                       handle nomatch => slv(goal::goals,rules,l,S)


       in
                 slv(goals, db, 1, null)

       end

exception PrintFail;

(*This function takes a list of goals and a database as input and attempts to find a solution. 
If a solution is found, it returns the pairs of variables and their corresponding values. 
If no solution is found, it raises the PrintFail exception.*)

fun OutQry (goals, db)=
	let
      val S = slvr(goals, db) 
      handle no_sol => raise PrintFail
      val var = myfoldr  gathers [] goals
      val d = rmv var
      val w = mymap (valu S) d
      val p = pairs(d, w)
      handle lngth => []
	in 
      OutSol(p)
	end


fun Prolog (x as (Headed (Var _, _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (x as (Headless (Var _ :: _))) =
    OutLine ("Illegal clause:  " ^ PrintClause x)
  | Prolog (Headed (Fun ("init", nil),nil)) = InitDB ()
  | Prolog (x as (Headed _)) = Assert x
  | Prolog (x as Headless y) =
    (OutLine ("query:  " ^ PrintClause x);
     (*OutLine ("query not yet implemented")*)
      OutQry (y, !db) 
      handle PrintFail => OutLine("No")
    )
