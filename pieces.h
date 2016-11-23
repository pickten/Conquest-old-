#include <stdbool.h>
#ifndef PIECES_H
#define PIECES_H



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
  

  struct pieceAction* kill;
  
  struct intPieceAction* carry;
  int carryCap;
  

  struct pieceAction* combo;

  struct intPieceAction* recapture;

  struct nodeAction* travel;

  struct pieceAction* mountAttack;

  struct pieceAction* dismountAttack;
  
};

struct pieceType* parsePieceType(char* fileName);  /* Parse a file and grab all the pieceTypes in order by id */


struct piece {
  int id;  /* we'll sort by this */
  

  int movesLeft;

  struct pieceType* pieceType;

  struct RBTree_piece* piecesCarried;
  
};




#endif  /* PIECES_H */
