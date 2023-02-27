let norm t = Interpret.interpret t |> Quote.quote
let norm_equal t t' = Types.equal_term (norm t) (norm t')
