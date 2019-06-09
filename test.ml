fun x -> fun y -> fun z -> x z (y z) 
fun x -> let y = x + 1 in y 
fun b -> fun x -> if x b then x else (fun x -> b) 
fun x -> fun y -> if x then x else y 
fun n -> (fun x -> x (fun y -> y)) (fun f -> f n) 
fun x -> fun y -> x (y x) 
fun x -> fun y -> x (y x) (y x) 
