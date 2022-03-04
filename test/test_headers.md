## Create Header.t and add items to it

```ocaml
open Cohttp_parser
```

```ocaml
# let hdrs = Header.create 1;;
val hdrs : Header.t = {Cohttp_parser.Header.headers = [|("", "")|]; len = 0}

# Header.add hdrs ("nm1", "val1");;
- : unit = ()

# Header.add hdrs ("nm2", "val2");;
- : unit = ()

# Header.length hdrs;;
- : int = 2

# Header.add hdrs ("nm3", "val3");;
- : unit = ()

# Header.add hdrs ("nm4", "val4");;
- : unit = ()

# Header.length hdrs;;
- : int = 4

# hdrs;;
- : Header.t =
{Cohttp_parser.Header.headers =
  [|("nm1", "val1"); ("nm2", "val2"); ("nm3", "val3"); ("nm4", "val4")|];
 len = 4}
```

## Clear Header.t

```ocaml
# Header.clear hdrs;;
- : unit = ()

# Header.length hdrs;;
- : int = 0

# Header.add hdrs ("nm1", "val1");;
- : unit = ()

# Header.length hdrs;;
- : int = 1
```
