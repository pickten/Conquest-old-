typedef struct Node {
  struct Node * adj0;
  struct Node * adj1;
  struct Node * adj2;
  struct Node * adj3;
  int id;
  /* I've defined four adj nodes just in case there is a board that
   * needs that many. Each node has pointers to its adjacent nodes purely for the
   * purpose of checking validity of moves without too much mucking.
   */
  // piece * currentPiece; /* Whatever piece is on the node */
  char *type; /* This will either be L (land) or S (sea)
	      * Pl treated as land and Ps as water for the purpose of moving, but there are other 
	      * checks that might be useful regarding port/land distinctions.
	      */
} Node;

// extern Node * nodes[15];

typedef struct NodeList {
  expNode * nodePointer;
  struct NodeList * next;
  int index;
} NodeList;

extern NodeList * allNodes;

void mapinit(char *input);

void testpmapinit();

void printNodes(Node *node0);

/* EXPERIMENTAL SHIT */

void newNode(int id, char type);

typedef struct Piece {
  /* placeholder piece struct */
  int type;
} Piece;

typedef struct ExpNode {
  /* experimental malloc'd node type */
  int id;
  int adj1id;
  int adj2id;
  int adj3id;
  int adj4id;
  char type;
  Piece *piece;
} ExpNode;

void destructNode(int id);

void freeallnodes();

void attachPieceToNode(int id, Piece *toAdd);

Piece getPieceFromNode(int id);
