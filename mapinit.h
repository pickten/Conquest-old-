typedef struct Node {
<<<<<<< HEAD
  struct Node * adj0 = NULL;
  struct Node * adj1 = NULL;
  struct Node * adj2 = NULL;
  struct Node * adj3 = NULL;
  /* I've defined four adj nodes just in case there is a board that
   * needs that many. Each node has pointers to its adjacent nodes purely for the
   * purpose of checking validity of moves without too much mucking.
   */
  piece * currentPiece = NULL; /* Whatever piece is on the node */
  char type; /* This will either be L (land) or S (sea)
	      * Pl treated as land and Ps as water for the purpose of moving, but there are other 
	      * checks that might be useful regarding port/land distinctions.
	      */
}Node;

int mapinit();

int testpmapinit();

void freeallnodes();

