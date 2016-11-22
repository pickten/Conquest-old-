typedef struct Node {
  char Type; /* This will either be L (land), S (sea), P(port), or C (capitol). P and C are
	        treated as land for the purpose of moving, but there are other checks that
	        might be useful */
  piece * currentPiece; /* Whatever piece is on the node */
  struct Node * adj1;
  struct Node * adj2;
  struct Node * adj3;
  struct Node * adj4;
  /* * * *
  IMPORTANT: For parsing files and generating the map, each node's adj1 is defined as the node
  from which they were generated; I've defined four adj nodes just in case there is a board that
  needs that many.
  * * * */
}Node;
