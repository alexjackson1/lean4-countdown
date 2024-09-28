/-!
# Countdown Numbers Game
-/

inductive Op
| add : Op
| sub : Op
| mul : Op
| div : Op

def applyOp (op : Op) (a b : Int) : Option Int :=
  match op with
  | Op.add => some (a + b)
  | Op.mul => if a = 1 ∨ b = 1 then none else some (a * b)
  | Op.sub => if a > b then some (a - b) else none
  | Op.div => if b ≠ 0 ∧ a % b = 0 then some (a / b) else none

def opToString (op : Op) : String :=
  match op with
  | Op.add => "+"
  | Op.sub => "-"
  | Op.mul => "*"
  | Op.div => "/"

def eqToString (op : Op) (n1 n2 r : Int): String :=
  s!"{n1} {opToString op} {n2} = {r}"

def removeAt {α : Type} : List α → Nat → List α
| [], _ => []
| (_ :: xs), 0 => xs
| (x :: xs), n => x :: removeAt xs (n - 1)

def combine (n1 n2 : Int) (ops : List Op) : List (Int × String) :=
  ops.foldl (λ acc op =>
    match applyOp op n1 n2 with
    | some result => (result, eqToString op n1 n2 result) :: acc
    | none => acc) []

def listProduct {α β : Type} (l1 : List α) (l2 : List β) : List (α × β) :=
  List.bind l1 (λ x => List.map (λ y => (x, y)) l2)

def solve (N : List Int) (T : Int) : List (Int × String) :=
  let rec _solve (numbers : List Int) (l : Nat) : List (Int × String) :=
      if l < 2 then []
      else
        List.bind (listProduct (List.range l) (List.range l)) (λ (i, j) =>
          if i < j then
            let n1 := numbers.get! i
            let n2 := numbers.get! j
            let remaining := removeAt (removeAt numbers j) i -- remove larger first
            let nextSteps := combine n1 n2 [Op.add, Op.sub, Op.mul, Op.div]

            -- Recursively solve with the new numbers
            List.bind nextSteps (λ (result, expr) =>
              let nextNumbers := result :: remaining
              if result = T then [(result, expr)]
              else List.map (λ (r, ex) =>
                (r, s!"{expr}; {ex}")) (_solve nextNumbers (l - 1))
            )
          else []
        )

  match N with
  | [] => []
  | [n] => if n = T then [(n, s!"{n}")] else []
  | _ => _solve N N.length

#eval solve [1, 2, 3, 4] 24

def countdownGame (numbers : List Int) (T : Int) : IO Unit :=
  let solutions := solve numbers T
  if solutions.isEmpty then
    IO.println s!"No solution found for {numbers} to make {T}"
  else
    solutions.forM (λ (_, explanation) => IO.println explanation)

#eval countdownGame [25, 50, 75, 100, 3, 6] 946
