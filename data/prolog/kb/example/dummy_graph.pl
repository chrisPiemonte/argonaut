%            
%                                   +-----+     L2
%                                   |  C  +---------+
%                                   +-----+         |
%           +-----+                              +--v--+
%           |  A  +----------------------------> |  B  +--+
%           +-+---+            L1                +-+---+  |
%             ^                                    |      |
%          L6 |                                    |      | L4
%             |                                    |      |
%         +---+-+              L3                  |  +---v-+
%         |  D  <----------------------------------+  |  E  |
%         +--+--+                                     +--+--+
%            |                                           |
%         L5 |               L7          +-----+         |
%            |  +------------------------+  F  | <-------+
%            v  |                        +--^--+       L8
%          +-+--v+                          |
%          |  G  +--------------------------+
%          +-----+           L9
% 


% NODES
argument(a).
argument(b).
argument(c).
argument(d).
argument(e).
argument(f).
argument(g).


% EDGES

attack(a,b).

attack(b,d).
attack(b,e).

attack(c,b).

attack(d,a).
attack(d,g).

attack(e,f).

attack(f,g).

attack(g,f).



edge(U, V) :-
    attack(U, V) ; attack(V, U).