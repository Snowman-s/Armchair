_This markdown is proto-type of accurate docs._

_The Armchair language is an extension of the Janus language(by Kahn, and Saraswat)._

# BNF

\<PROGRAM> ::= (\<WS> \<behavior> \<WS>)\*  
\<behavior> ::= \<behavior_name> \<lbracket> [\<ask_term> (\<sep> \<ask_term>)\*] \<rbracket> \<double_colon> \<agent>  
\<agent> ::= \<tell_contraint> | \<behavior_name> \<lbracket> [\<tell_term> (\<sep> \<tell_term>)\*] \<rbracket> | \<ask_constraint> \<arrow> \<agent> | \<agent> (\<sep> \<agent>)+

\<WS> ::= (" " | "\n" | "\t")\*  
\<sep> ::= \<WS> "," \<WS>  
\<lbracket> ::= \<WS> "(" \<WS>  
\<rbracket> ::= \<WS> ")" \<WS>  
\<double_colon> ::= \<WS> ":" ":" \<WS>  
\<arrow> ::= \<WS> "-" ">" \<WS>

# How Armchair work?

There are a global object, **"variable store"**.  
**variable store** memories **"constraints"** for each of _variables_.  
If you _tell_ a new **constraint**, **Variable store** memories that constraint in addtion to what the store already memories.

Agents
