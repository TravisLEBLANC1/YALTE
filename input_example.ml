(* generic *)
let I = λz.z in

(* Church integers *)
let zero = λf.λx.x in 
let one = λf.λx.f x in 
let two = λf.λx.f (f x) in
let three = λf.λx.f (f (f x)) in
let foor = λf.λx.f (f (f (f x))) in
let five = λf.λx.f (f (f (f (f x)))) in
let six = λf.λx.f (f (f (f (f (f x))))) in
let seven = λf.λx.f (f (f (f (f (f (f x)))))) in
let eight = λf.λx.f (f (f (f (f (f (f (f x))))))) in
let nine = λf.λx.f (f (f (f (f (f (f (f (f x)))))))) in
let dix = λf.λx.f (f (f (f (f (f (f (f (f (f x))))))))) in
let suc = λn.λf.λx. n f (f x) in
let double = λn.λf.λx.n f (n f x) in
let pred = λn.λf.λx.n (λr.λi.i (r f)) (λf. x) (λx.x) in

(* booleans*)
let true = λt.λf.t in
let false = λt.λf.f in 
let iszero = λn.n (λx.false) true in
let ifelse = λb.λt.λf. b t f in

(* example from "(in)efficiency of interaction"*)
let ex1 = (λy.λx.x y) I I in 
(* example from "The abstract machinery of interaction"*)
let ex2 = ((λz.λx.x) w) (λy.y) in
let ex3 = (λx.x x) (λy.y) in 

(* fix points*)
let theta = (λx.λn.λy.n y (x x (double n) y)) in
let IL = (theta theta dix) in

let theta1 = (λx.λy.y (x x y)) in
let turing = (theta1 theta1) in
let f = \f.\n.ifelse (iszero n) zero (suc f) in

turing f two | turing f zero