%{
        open Types
        open Printf
        let log s = printf "%s\n" s;flush Pervasives.stdout 
%}

%token <int*int> CHANGESPEC
%token <string> FILEDEF
%token CHANGEDEF
%token EOF

%start file
%start filespeclist
%type <Types.filespeclist> filespeclist file
%%

changespec: 
        | CHANGESPEC CHANGESPEC {ChangeSpec($1 ,$2)}
        ;

changespeclist:
        | changespeclist changespec  {$1 @ [$2]}
        |                              { [] } 
        ;

filespec:
        | FILEDEF changespeclist {FileSpec($1, $2)}
        ;
        
filespeclist:
        | filespeclist filespec {$1 @ [$2]}
        |                       { [] }
        ;

file: filespeclist EOF                       { $1 }
        ;
