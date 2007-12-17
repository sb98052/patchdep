type linespec = int * int
type changespec = ChangeSpec of linespec * linespec
type filespec = FileSpec of string * changespec list
type filespeclist = filespec list
