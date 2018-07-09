exception NOT_UNIFIABLE;;
type term = V of string | Node of (string*(term list)) | Cut | Fail;;
type goal = (term list);;
type clause = (term * (term list));;


let rec foldl f e l = match l with
           [] -> e |
           x::xs -> foldl f (f e x) xs;;

let rec vars1 ll t = match t with  
           V v -> if ((List.mem (V v) ll)) then ll else (V v)::ll|
           Node(s,l) ->  (foldl vars1 ll l)|
           _ -> [];;

let rec vars t = match t with 
           Cut -> [] |
           Fail -> [] |
           _ -> vars1 [] t;; 

let rec varl l = match l with
           [] -> [] |
           x::xs -> List.append (vars x) (varl xs);;

let compose f g x = g(f(x));;   

let rec map g f l = match l with
           [] -> [] |
           x::xs ->  (g f x) :: (map g f xs);;

let rec subst s x = match x with   
           V v -> s(x) |
           Node(sy,[]) -> Node(sy,[]) |
           Node(sy,l) -> Node(sy,(map subst s l)) |
           _ -> x ;;

let id x = x;;  

let mat1(a1,b) x = match x with    
            V a -> if(a<>a1) then x else V b |
            _ -> x;;

let mat2(a1,b) x = match x with   
            V a -> if(a<>a1) then x else Node(b,[]) |
            _ -> x;;

let mat3(a1,s,l) x = match x with   
            V a -> if(a<>a1) then x else Node(s,l) |
            _ -> x;;

let rec map1 f (l1,l2) = match (l1,l2) with 
           ([],[]) -> [] |
           (x1::xs1,x2::xs2) -> (f(x1,x2)::(map1 f ((map subst (f(x1,x2)) xs1),(map subst (f(x1,x2)) xs2))))|
            _ -> raise NOT_UNIFIABLE;;

let rec supercom l = match l with   
           [] -> id |
           x::xs -> compose x (supercom xs);;

let rec mgu (t1,t2) = match (t1,t2) with    
           (V a,V b) -> if(a<>b) then mat1(a,b) else id |
           (V a,Node(s,[])) -> mat2(a,s) |
           (Node(a,[]),Node(b,[])) -> if(a<>b) then raise NOT_UNIFIABLE else id |
           (Node(a,[]),Node(b,l)) -> raise NOT_UNIFIABLE |
           (V a,Node(s,l)) -> if (List.mem (V a) (vars (Node(s,l)))) then raise NOT_UNIFIABLE else mat3(a,s,l) |
           (Node(a,l1),Node(b,l2)) -> if (a<>b) then raise NOT_UNIFIABLE else (supercom(map1 mgu (l1,l2))) |
           (Cut, _ )  -> raise NOT_UNIFIABLE |
           (Fail,_) -> raise NOT_UNIFIABLE |
           _ -> mgu(t2,t1);;

let rec exe(go,st,pr,prf,curt,(ans,tbl),ovars,fa) = match go with
        ([]) ->
          (match st with
            [] ->
                let mt = compose curt tbl in
                let ax x = mt x in
                let l2 = List.map ax ovars in
                (true,mt,fa@[l2])
            |(go',pr',(ans',tbl'))::st' ->
                let mt = compose curt tbl in
                let ax x = mt x in
                let l2 = List.map ax ovars in
                exe(go',st',pr',prf,tbl',(true,mt),ovars,(fa@[l2])))
        |Cut::qs-> 
          (match st with
            []->  exe(qs, [], pr, prf, curt, (ans,tbl), ovars, fa)
            |(go',pr',(ans',tbl'))::st'-> 
              if(List.mem Cut go') then 
              	exe(Cut::qs, st', pr, prf, curt, (ans,tbl), ovars, fa)
              else
              	exe(qs, st', pr, prf, curt, (ans,tbl), ovars, fa)
          )
        |Fail::qs-> (match st with
            []-> (ans,tbl,fa)
            |(go',pr',(ans',tbl'))::st'-> 
            exe(go', st', pr', prf, tbl', (ans',tbl), ovars, fa)
          )
        | (V(a))::qs -> (true,id,[])
        | (Node(sx,lx)::xs) ->
          (match pr with
            ([]) -> (match st with
              [] -> (ans,tbl,fa)
              | (go',pr', (ans',tbl'))::st' ->
                exe(go',st',pr',prf,tbl',((ans'||ans),tbl),ovars,fa)
              )
            |(((xp,(bod)))::xs1) ->
              try 
                let b = mgu(xp,Node(sx,lx)) in
                let temp x = subst b x in
                let bod2 = List.map temp bod in
                let xs2 = List.map temp xs in
                let tbl2 = compose b curt in
                let nst = (go,(xs1),(ans,curt))::st in
                let nq = bod2@xs2 in
                exe(nq,nst,prf,prf,tbl2,(ans,tbl),ovars,fa)
              with NOT_UNIFIABLE -> exe(go,st,((xs1)),prf,curt,(ans,tbl),ovars,fa)
              );;  

let execute g1 p1 = match g1 with
      [] -> exe(g1,[],p1,p1,id,(false,id),[],[]) |
      _ -> exe(g1,[],p1,p1,id,(false,id),(List.rev (varl (g1))),[]);;

(* let f1 = ((Node("edge",[Node("a",[]);Node("b",[])])),([]));;
let f2 = ((Node("edge",[Node("b",[]);Node("c",[])])),([]));;
let f3 = ((Node("edge",[Node("c",[]);Node("d",[])])),([]));;
let f4 = ((Node("edge",[Node("a",[]);Node("d",[])])),([]));;
let f5 = ((Node("path",[V("x");V("y")])),([Node("edge",[V "x";V "y"])]));;
let r1 = ((Node("path",[V("x");V("y")])),([Node("edge",[V("x");V("z")]);Node("path",[V("z");V("y")])]));;
let p1 = ([f1;f2;f3;f4;f5;r1]);;

let g12 = ([Node("path",[V "p";V "q"]); Cut]);;
execute g12 p1;; *)
(* let f1 = ((Node("edge",[Node("1",[]);Node("2",[])])),([]));;
let f2 = ((Node("edge",[Node("2",[]);Node("3",[])])),([]));;
let f3 = ((Node("edge",[Node("3",[]);Node("4",[])])),([]));;
let f4 = ((Node("edge",[Node("3",[]);Node("5",[])])),([]));;
let f5 = ((Node("path",[V("x");V("x")])),([]));;
let r1 = ((Node("path",[V("x");V("y")])),([Node("edge",[V("x");V("z")]);Node("path",[V("z");V("y")]);Cut]));;

let p1 = ([f1;f2;f3;f4;f5;r1]);;

let g1 = ([Node("path",[V "a";V "b"])]);;

let a1 = ((Node("g",[Node("1",[])])),([]));;
let a2 = ((Node("g",[Node("2",[])])),([]));;
let a3 = ((Node("f",[V "x"])),([Node("g",[V "x"]);Cut]));;

let p2 = ([a1;a2;a3]);;

let g2 = ([Node("f",[V "a"]);Node("g",[V "a"])]);;

execute g1 p1;;
execute g2 p2;;

let g2 = ([Node("g",[V "a"]);Node("f",[V "a"])]);;
execute g2 p2;; *)