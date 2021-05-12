# cmsc430-final
add regex support to the Hustle+ compiler for CMSC430

Reg-exp will support empty strings, characters, unions, concatenations, and kleene stars.
Reg-exp may also support wildcard, negation, etc.

TODOS:
- 1) Create a new Prim2 struct (Prim2 'regex-match? DFA string) which stores a DFA struct and a string
- 2) ~~ During parsing, create (Prim2 'regex-match? DFA string) struct. 'regex-match? calls will come in as ('regex-match? <regex string> <input string>).~~
   -  ~~ a) Check that <regex string> is a valid regular expression ~~
   -  ~~ b) Convert the valid <regex string> into an NFA struct ~~
   -  c) Convert the NFA struct into DFA struct
   -  d) Store reduction as (Prim2 'regex-match? DFA string)
- 3) In the compiler:
   -  a) Create a mapping of states to gensym labels
   -  b) For each state/transition in the DFA struct, create Jumps to other states. State+transitions not in the DFA struct should default to the starting state
   -  c) If we reach the end of the string, and we're at a final state, then return True. Else return False
