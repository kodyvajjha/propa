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

%right IFFTOK IMPLIESTOK
%right ORTOK  
%right ANDTOK
%right NOTTOK

%%