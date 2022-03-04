## Create Headers.t and add items to it

```ocaml
open Cohttp_parser
```

```ocaml
# let hdrs = Headers.create 1;;
val hdrs : Headers.t =
  {Cohttp_parser.Headers.headers = [|("", "")|]; len = 0}

# Headers.add hdrs ("nm1", "val1");;
- : unit = ()

# Headers.add hdrs ("nm2", "val2");;
- : unit = ()

# Headers.length hdrs;;
- : int = 2

# Headers.add hdrs ("nm3", "val3");;
- : unit = ()

# Headers.add hdrs ("nm4", "val4");;
- : unit = ()

# Headers.length hdrs;;
- : int = 4

# hdrs;;
- : Headers.t =
{Cohttp_parser.Headers.headers =
  [|("nm1", "val1"); ("nm2", "val2"); ("nm3", "val3"); ("nm4", "val4")|];
 len = 4}
```

## Clear Headers.t

```ocaml
# Headers.clear hdrs;;
- : unit = ()

# Headers.length hdrs;;
- : int = 0

# Headers.add hdrs ("nm1", "val1");;
- : unit = ()

# Headers.length hdrs;;
- : int = 1
```
