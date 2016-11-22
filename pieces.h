#include <stdbool.h>
#ifndef PIECES_H
#define PIECES_H

struct pieceType;
struct piece;
struct pieceAction;
struct nodeAction;

struct intPiece {
  int num;
  struct piece* piece;
};

  

struct intPieceAction {
  bool defaultState;
  int exceptionCount;
  struct intPiece* exceptions;
};

  

struct pieceAction {
  int exceptionCount;
  bool defaultState;
  struct pieceType* exceptions;
};


struct nodeAction {
  int exceptionCount;
  bool defaultState;
  struct nodeType* exceptions;
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

  struct piece* piecesCarried;

  int numberPiecesCarried;
  
};

#endif  /* PIECES_H */
