/*
typedef struct Node {
  struct Node * adj0;
  struct Node * adj1;
  struct Node * adj2;
  struct Node * adj3;
  int id;
  char *type;
} Node;
*/

// extern Node * nodes[15];

typedef struct NodeList {
  Node * nodePointer;
  struct NodeList * next;
  int index;
} NodeList;

extern NodeList * allNodes;

void mapinit(char *input);

void testpmapinit();

// void printNodes(Node *node0);

/* EXPERIMENTAL SHIT */

void newNode(int id, char type);

typedef struct Piece {
  /* placeholder piece struct */
  int type;
} Piece;

typedef struct Node {
  /* experimental malloc'd node type */
  int id;
  int adjIDs[4];
  char type;
  Piece * piece;
} Node;

void destructNode(int id);

void freeallnodes();
void attachPieceToNode(int id, Piece *toAdd);

Piece getPieceFromNode(int id);

Node * findNode(int id);
