%{

open Pbparams

let retrieve = Pbparams.param_string
let assign   = Pbparams.assign_string

%}

%token <string> RAWTEXT
%token <string> IDENT
%token SIGIL SOPEN SCLOSE
%token SUBDEF SUBSET SUBERR
%token EOF

%start pbexp
%type <string> pbexp

%%

pbexp:
| param pbexp   { $1 ^ $2 }
| text  pbexp   { $1 ^ $2 }
|               { "" }
| EOF           { "" }

param:
| SIGIL pname        { try retrieve $2 with Not_found -> "" }
| SOPEN pname SCLOSE { try retrieve $2 with Not_found -> "" }
| SOPEN pname SUBDEF pbexp SCLOSE
    { try retrieve $2 with Not_found -> $4 }
| SOPEN pname SUBSET pbexp SCLOSE
    { try retrieve $2
      with Not_found ->
        assign $2 $4 ;
        retrieve $2
    }
| SOPEN pname SUBERR pbexp SCLOSE
    { try retrieve $2 with Not_found -> failwith $4 }

text:
| RAWTEXT { $1   }
| SUBDEF  { ":-" (* Convert meta-chars in wrong place back to text *) }
| SUBSET  { ":=" (* All ways around this problem seem hackish. *)     }
| SUBERR  { ":?" (* Owell... *) }

pname:
| IDENT { $1 }
| param { $1 }

%%
