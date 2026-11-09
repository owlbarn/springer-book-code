let levels documents cluster =
  if documents < 0 || not(Float.is_finite cluster) || cluster < 1. then
    invalid_arg "Nonnegative document count and cluster size >= 1 required";
  if documents = 0 || float_of_int documents <= cluster then 0
  else Float.log2 (float_of_int documents /. cluster) |> ceil |> int_of_float
