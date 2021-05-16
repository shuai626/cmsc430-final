# cmsc430-final
add regex support to the Hustle+ compiler for CMSC430

Reg-exp will support empty strings, characters, unions, concatenations, and kleene stars.
Reg-exp may also support wildcard, negation, etc.

TODOS:
- 1) <del> Create a new Prim2 struct (Prim2 'regexp-match? DFA string) which stores a DFA struct and a string <del>
- 2) <del> During parsing, create (Prim2 'regexp-match? DFA string) struct. 'regexp-match? calls will come in as ('regexp-match? <regex_string> <input_string>). <del>
   -  <del> a) Check that <regex string> is a valid regular expression <del>
   -  <del> b) Convert the valid <regex string> into an NFA struct <del>
   -  <del> c) Convert the NFA struct into DFA struct <del>
   -  <del> d) Store reduction as (Prim2 'regexp-match? DFA string) <del>
- 3) In the compiler:
   - <del>a) At the start of compilation, find all 'regex-match clauses in the program <del>
   - b) Create a regex tree for each 'regex-match clause (using global labels for return-true and return-false)
      -  <del> a) Create a mapping of states to gensym labels. Create a function that returns
      all transitions from a given state. Create a function that returns a label given a state <del>
      -  <del> b) For each state/transition in the DFA struct, create Jumps to other states. Create special case <del>
      for (Wild) char
      -  <del> c) If we reach the end of the string, and we're at a final state, then return True. Else return False <del>
   - <del> c) compile-dfa loads address of dfa's start-state into rax <del>
   - <del> d) compiling prim2 clause will store current memory location into memory, push string
   pointer to stratch register, then jump to dfa's start-state <del>


STRETCH
- <del> Add substring matching: \.\*<reg-exp>\.\* <del>
- Add support for quantifiers
- Add support for anchors: ^ and $
- Add support for quantifiers: + and {}
- Add support for character classes: [abc]
- Add support to return first substring matched