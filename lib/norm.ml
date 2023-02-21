let norm t = Interpret.interpret t |> Quote.quote
let norm_eq t t' = Types.eq (norm t) (norm t')
