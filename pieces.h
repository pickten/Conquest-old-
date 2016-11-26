#ifndef PIECES_H
#define PIECES_H
#include <stdbool.h>
#include "genericmacros.h"


struct pieceType;
struct piece;
struct pieceAction;
struct nodeAction;

// time to run the rbtree nonsense.
// I know what I'm doing, I swear.
#define RBTREE_TYPE intPiece
#include "rbtrees.h"
#define RBTREES_H_intPiece 1
#undef RBTREE_TYPE

#define RBTREE_TYPE pieceType
#include "rbtrees.h"
#define RBTREES_H_pieceType 1
#undef RBTREE_TYPE

#define RBTREE_TYPE charWrapper
#include "rbtrees.h"
#define RBTREES_H_charWrapper 1
#undef RBTREE_TYPE

#define RBTREE_TYPE piece
#include "rbtrees.h"
#define RBTREES_H_piece 1
#undef RBTREE_TYPE


struct intPiece {
  int num;
  struct pieceType* piece;
};

struct charWrapper {
  char c;
};

  

struct intPieceAction {
  bool defaultState;
  struct RBTree_intPiece* exceptions;
};

  

struct pieceAction {
  bool defaultState;
  struct RBTree_pieceType* exceptions;
};


struct nodeAction {
  bool defaultState;
  struct RBTree_charWrapper* exceptions;  /* since node types are chars */
};




struct pieceType {
  int maxMoves;  /* how many moves the piece can take in one turn */
  int id;       /* used to get the piece type when parsing */
  char *name;
  struct pieceAction* Kill; // they're uppercased for reason (i.e. I'm lazy)
  
  struct intPieceAction* Carry;
  int carryCap;
  

  struct pieceAction* Combo;
  struct intPieceAction* Recapture;
  struct nodeAction* Travel;
  struct pieceAction* MountAttack;
  struct pieceAction* DismountAttack;
  
};

struct pieceType* parsePieceType(char* fileName);
/* Parse a file and grab all the pieceTypes in order by id */


struct piece {
  int id;  /* we'll sort by this */
  

  int movesLeft;
  struct pieceType* pieceType;
  struct RBTree_piece* piecesCarried;
};



void deletePiece(struct piece *p);
void deletePieceType(struct pieceType *p);
void deleteIntPiece(struct intPiece *p);
void deletePieceAction(struct pieceAction *p);
void deleteIntPieceAction(struct intPieceAction *p);
void deleteNodeAction(struct nodeAction *p);
void deleteCharWrapper(struct charWrapper *p);


struct piece* newPiece(int id, struct pieceType *p);
struct pieceType* newPieceType(int id);
struct intPiece* newIntPiece(int i, struct pieceType *p);
struct pieceAction* newPieceAction(bool d, struct RBTree_pieceType *p);
struct intPieceAction* newIntPieceAction(bool d, struct RBTree_intPiece *p);
struct nodeAction* newNodeAction(bool d, struct RBTree_charWrapper *p);
struct charWrapper* newCharWrapper(char c);


struct intPiece* emptyIntPiece(struct pieceType *p);
struct pieceAction* emptyPieceAction(bool d);
struct intPieceAction* emptyIntPieceAction(bool d);
struct nodeAction* emptyNodeAction(bool d);
struct charWrapper* emptyCharWrapper(void);



#define CANDOTHIS(name, type) bool can ## name ## Raw(struct pieceType *x, int id)


CANDOTHIS(Kill, pieceType);
CANDOTHIS(Carry, intPiece);
CANDOTHIS(Combo, pieceType);
CANDOTHIS(Recapture, intPiece);
CANDOTHIS(Travel, charWrapper);
CANDOTHIS(MountAttack, pieceType);
CANDOTHIS(DismountAttack, pieceType);

#undef CANDOTHIS

void startCarrying(struct piece *p, struct piece *o);
void stopCarrying(struct piece *p, struct piece *o);


#endif  /* PIECES_H */
