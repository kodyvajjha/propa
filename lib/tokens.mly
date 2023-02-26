%token <string> PROP
%token IMPLIESTOK 
%token FALSETOK
%token TRUETOK
%token LPAREN
%token RPAREN
%token NOTTOK
%token ANDTOK 
%token ORTOK 
%token IFFTOK 
%token EOL 

// %right ANDTOK ORTOK IFFTOK IMPLIESTOK NOTTOK 
%right IFFTOK IMPLIESTOK
%right ORTOK ANDTOK 
%right NOTTOK

%%