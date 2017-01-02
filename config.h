#ifndef CONFIG_H
#define CONFIG_H
#include <stdio.h>
#include "mapinit.h"
#include "pieces.h"

#define Node expnode

#define RBTREE_TYPE Node
#include "rbtrees.h"
#define RBTREES_H_Node 1
#undef RBTREE_TYPE

struct intint {
  int a;
  int b;
  char c;  /* shush */
  int d;   /* shush */
};

#define STACK_TYPE intint
#include "stack.h"
#define STACK_H_intint 1
#undef STACK_TYPE

struct config {
  struct RBTree_pieceType *types;
  struct Node *map;
  struct RBTree_Node *allNodes;
};

struct config *newConfig(void);
void deleteConfig(struct config *cfg);

struct environment {
  int typeID;
  int nodeID;
  int pieceID;
  int playerID;
  int preambleLines;
  int workingLine;
  bool currentLine;
  char actionType;
  
  struct intPieceAction *ip;
  struct pieceAction *p;
  struct nodeAction *n;

  struct stack_intint *ipExceptions;
  struct stack_intint *pExceptions;
  struct stack_intint *oExceptions;
  
  
  
  struct pieceType *type;
  struct Node *node;
};



struct environment *newEnvironment(void);
struct environment *newPieceTypeEnv(struct environment *e);
struct environment *newNodeEnv(struct environment *e);
struct environment *insertException(struct environment *e, int id, int val);
// adds an exception to e's working action sometimes, or delays it otherwise, or sometimes it just fills in the preamble. It's weird


void deleteEnvironment(struct environment *e);

char* fileIsIncluded(char *l);  // check if the line includes a file; if so, allocate a line to work with and return the file name.

  
struct environment *parseFileEnv(struct config *c, struct environment *e, char* line, FILE *f);
// parse file with env; returns the env for implementation reasons.

struct config *parseFileRaw(FILE *f);
// parse file given a handler

struct config *parseFile(char *f);
// parse file given a name




#endif  /* CONFIG_H */
