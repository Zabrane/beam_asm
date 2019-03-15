Nonterminals
  Arg
  Args
  Elements
  Eols
  ExportFuns
  Exports
  FunBody
  Funs
  Integers
  KV
  KVs
  LocalFun
  MaybeArgs
  MaybeElements
  MaybeEols
  MaybeIntegers
  MaybeKVs
  Module
  Statement
.

Terminals
  '#{'
  '('
  ')'
  ','
  '.'
  '/'
  ':'
  '<<'
  '=>'
  '>>'
  '?'
  '['
  ']'
  'fun'
  '{'
  '}'
  arrow
  atom
  eol
  exports
  float
  integer
  label
  register
.

Rootsymbol Module.

% Top level file structure
Module -> Exports Funs : #{exports => '$1', funs => '$2'}.

% Exports
Exports -> MaybeEols exports '(' '[' ExportFuns ']' ')' '.' Eols : '$5'.

ExportFuns -> MaybeEols LocalFun ExportFuns : ['$2' | '$3'].
ExportFuns -> MaybeEols : [].

% Funs
Funs -> MaybeEols LocalFun arrow Eols FunBody '.' Funs : [{'$2', '$5'} | '$7'].
Funs -> MaybeEols : [].

FunBody -> Statement Eols FunBody : ['$1' | '$3'].
FunBody -> '$empty' : [].

Statement -> label : {label, v('$1')}.
Statement -> '?' atom label Args : {test, v('$2'), {label, v('$3')}, '$4'}.
Statement -> atom MaybeArgs : {op, v('$1'), '$2'}.

% Erlang terms, labels and registers in arguments

MaybeArgs -> Args : '$1'.
MaybeArgs -> '$empty' : [].

Args -> Arg Args : ['$1' | '$2'].
Args -> Arg : ['$1'].

MaybeElements -> Arg Elements : ['$1' | '$2'].
MaybeElements -> '$empty' : [].

Elements -> ',' Arg Elements : ['$2' | '$3'].
Elements -> '$empty' : [].

MaybeKVs -> KV KVs : ['$1' | '$2'].
MaybeKVs -> '$empty' : [].

KVs -> ',' KV KVs : ['$2' | '$3'].
KVs -> '$empty' : [].

KV -> Arg '=>' Arg : {'$1', '$3'}.

MaybeIntegers -> integer Integers : [v('$1') | '$2'].
MaybeIntegers -> '$empty' : [].

Integers -> ',' integer Integers : [v('$2') | '$3'].
Integers -> '$empty' : [].

Arg -> register : {register, v('$1')}.
Arg -> label : {label, v('$1')}.
Arg -> integer : v('$1').
Arg -> float : v('$1').
Arg -> atom : v('$1').
Arg -> 'fun' atom ':' atom '/' integer : M = v('$2'), F = v('$4'), A = v('$6'), fun M:F/A.
Arg -> '#{' MaybeKVs '}' : maps:from_list('$2').
Arg -> '{' MaybeElements '}' : {tuple, list_to_tuple('$2')}.
Arg -> '[' MaybeElements ']' : '$2'.
Arg -> '<<' MaybeIntegers '>>' : list_to_binary('$2').

LocalFun ->  atom '/' integer : {v('$1'), v('$3')}.

% Helper rules
MaybeEols -> Eols.
MaybeEols -> '$empty'.

Eols -> eol Eols.
Eols -> eol.

Expect 1.

Erlang code.

v({Symbol, _Line})        -> Symbol;
v({_Type, _Line, Symbol}) -> Symbol.
