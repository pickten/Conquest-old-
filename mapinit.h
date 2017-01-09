typedef struct NodeList {
    Node * nodePointer;
    struct NodeList * next;
    int index;
} NodeList;

extern NodeList * allNodes;

void mapinit(char *input);

void testpmapinit();

void newNode(int id, char type);

typedef struct Piece {
  /* placeholder piece struct */
  int type;
} Piece;

typedef struct AdjList {
    // linked list of adjacent nodes because arrays suck shit
    int id;
    Struct Adjlist * next;
} AdjList;

typedef struct Node {
    /* experimental malloc'd node type */
    int id;
    adjList * adjIDs;
    Piece * piece;
    Node * trebLink;
    char type;
} Node;

void destructNode(int id);

void freeallnodes();
void attachPieceToNode(int id, Piece *toAdd);
Piece getPieceFromNode(int id);
Node * findNode(int id);
