# Semantics of While

This repository shows the denotational semantics for the **While language** and the **Proc language** as given in the book 'Semantics with Applications' <sup>1</sup>. 
The semantics for **While** are given in both **direct-style** (*Directstyle.hs*) and **continuation-style** (*Contstyle.hs*).
You can run test programs in the While language using either style. A few test programs are given at the end of the files.

There is also a showcase of using PL semantics to analyse programs.

*Liveness.hs* is a showcase of **live-variables analysis**.\
*Secinfflow.hs* is a showcase of **security analysis**.\
*Constprop.hs* is a showcase of **constant propagation analysis**.

The **Proc** language is an extension of the While language with support for procedures.
It makes use of environments to store variables and procedures, which complicates the semantics.
For **Proc**, the semantics are given in direct-style denotational semantics.

This was done outside regular courses as an exercise to practice with PL semantics and Haskell.



<sup>1</sup> Hanne Riis Nielson and Flemming Nielson. 2007. Semantics with applications: an appetizer. John Wiley & Sons, Inc., USA.