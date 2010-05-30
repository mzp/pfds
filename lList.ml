open Base
type 'a cell = Nil | Cons of 'a * 'a t
and  'a t = 'a cell Lazy.t

let cons x y =
  Cons (x,y)

let nil =
  lazy Nil

let hd = function
    lazy (Cons (x,_)) ->
      x
  | lazy Nil ->
      failwith "hd"

let tl = function
    lazy (Cons (_, x)) ->
      x
  | lazy Nil ->
      failwith "tl"

let rec append xs ys =
  lazy begin match xs,ys with
      lazy Nil, lazy ys -> ys
    | lazy (Cons (x,xs)), ys ->
	cons x @@ append xs ys
  end

let (++)  = append

let rec rev xs r =
  lazy begin match xs with
      lazy Nil ->
	!$ r
    | lazy (Cons (y, ys)) ->
	!$ (rev ys @@ lazy (cons y r))
  end

let rev xs = rev xs nil

let rec take n xs =
  if n <= 0 then
    nil
  else
    lazy begin match xs with
	lazy Nil -> Nil
      | lazy (Cons (x,xs)) ->
	  cons x (take (n-1) xs)
    end

let rec drop n xs =
  if n = 0 then
    xs
  else
    lazy begin match xs with
	lazy Nil ->
	  Nil
      | lazy (Cons (_, xs)) ->
	  !$ (drop (n-1) xs)
    end

let drop n xs =
  drop n xs

let rec of_list = function
    [] ->
      nil
  | x :: xs ->
      lazy (cons x @@ of_list xs)

let rec to_list = function
    lazy Nil ->
      []
  | lazy (Cons (x, xs)) ->
      x ::to_list xs
