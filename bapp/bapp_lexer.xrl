Definitions.

EscSeq = \\([0-7][0-7][0-7]|x([a-zA-Z0-9][a-zA-Z0-9]|\{[a-zA-Z0-9]+\})|\^[a-zA-Z]|.)

Rules.

% Whitespace, comments
\n : {token, {eol, TokenLine}}.
\045[^\n]* : skip_token.
\s|\t : skip_token.

% Special symbols & keywords
-exports : {token, {exports, TokenLine}}.
-> : {token, {arrow, TokenLine}}.
fun\s : {token, {'fun', TokenLine}}.
\[|\]|[(){}/,.:?]|#\{|<<|>>|=> : {token, {list_to_atom(TokenChars), TokenLine}}.

% Bapp-specific data types: labels and registers
@@ : {token, {label, TokenLine, TokenChars}}.
@[a-zA-Z0-9_]+ : {token, {label, TokenLine, TokenChars}}.
[xy][0-9]+ : {token, {register, TokenLine, {list_to_atom([hd(TokenChars)]), list_to_integer(tl(TokenChars))}}}.

% Erlang terms.
[a-z][a-zA-Z0-9_@]* : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
'({EscSeq}|[^\'])*' : {token, {atom, TokenLine, list_to_atom(lists:droplast(tl(TokenChars)))}}.
[0-9]+\.[0-9]+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
[0-9]+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.

Erlang code.
