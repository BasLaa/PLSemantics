# Semantics of While

This repository implements denotational semantics for the **While language** and the **Proc language** as described in the book 'Semantics with Applications' <sup>1</sup>. 
The semantics for **While** are given in both **direct-style** (*Directstyle.hs*) and **continuation-style** (*Contstyle.hs*).
You can run test programs in the While language using either style. A few test programs are given at the end of the files.

There is also a showcase of using PL semantics to analyse programs.

*Liveness.hs* is a showcase of **live-variables analysis**.\
*Secinfflow.hs* is a showcase of **security analysis**.\
*Constprop.hs* is a showcase of **constant propagation analysis**.

<sup>1</sup> Hanne Riis Nielson and Flemming Nielson. 2007. Semantics with applications: an appetizer. John Wiley & Sons, Inc., USA.
