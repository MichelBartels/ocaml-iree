let dummy_handler f =
  let open Effect.Deep in
  try_with f ()
    { effc=
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Random.Counter _ ->
              Some
                (fun (k : (a, _) continuation) ->
                  continue k @@ Dsl.scalar_u64 "0" )
          | Distribution.Sample (_, d, batch_size) ->
              Some
                (fun (k : (a, _) continuation) ->
                  continue k @@ Distribution.sample d batch_size )
          | _ ->
              None ) }
