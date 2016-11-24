typedef struct Node {
  struct Node * adj0;
  struct Node * adj1;
  struct Node * adj2;
  struct Node * adj3;
  /* I've defined four adj nodes just in case there is a board that
   * needs that many. Each node has pointers to its adjacent nodes purely for the
   * purpose of checking validity of moves without too much mucking.
   */
  // piece * currentPiece; /* Whatever piece is on the node */
  char *type; /* This will either be L (land) or S (sea)
	      * Pl treated as land and Ps as water for the purpose of moving, but there are other 
	      * checks that might be useful regarding port/land distinctions.
	      */
}Node;

extern Node * nodes;

Node* mapinit(char *input);

Node* testpmapinit(Node *head);

void printNodes(nodes);

void freeallnodes();
