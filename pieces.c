#include "pieces.h"


void deletePiece(struct piece *p){
  free(p->piecesCarried);
  free(p);
}

void deletePieceType(struct pieceType *p){
  deleteIntPieceAction(p->Carry);
  deletePieceAction(p->Combo);
  deletePieceAction(p->DismountAttack);
  deletePieceAction(p->Kill);
  deletePieceAction(p->MountAttack);
  deleteIntPieceAction(p->Recapture);
  deleteNodeAction(p->Travel);
  free(p->name);
  free(p);
}

void deleteIntPiece(struct intPiece *p){
  free(p->piece);
  free(p);
  
}

void deletePieceAction(struct pieceAction *p){
  free(p->exceptions);
  free(p);
}

void deleteIntPieceAction(struct intPieceAction *p){
  free(p->exceptions);
  free(p);
}

void deleteNodeAction(struct nodeAction *p){
  free(p->exceptions);
  free(p);
}

void deleteCharWrapper(struct charWrapper *p){
  free(p->c);
  free(p);
}


struct piece* newPiece(int id, struct pieceType *p) {
  struct piece *a = malloc(sizeof(struct piece));
  if(a == NULL)
    return NULL;
  a->id = id;
  a->pieceType = p;
  a->piecesCarried = emptyRBTree_piece();
  return a;
  
}

struct pieceType* newPieceType(int id){
  struct pieceType *a = malloc(sizeof(struct pieceType));
  if(a == NULL)
    return NULL;
  a->id = id;
  a->Carry = emptyIntPieceAction(false);
  a->Combo = emptyPieceAction(true);
  a->DismountAttack = emptyPieceAction(true);
  a->Kill = emptyPieceAction(true);
  a->MountAttack = emptyPieceAction(false);
  a->Recapture = emptyIntPieceAction(true);
  a->Travel = emptyNodeAction(true);
  return a;
}

struct intPiece* newIntPiece(int i, struct pieceType *p){
  struct intPiece *a = malloc(sizeof(struct intPiece));
  if(a == NULL)
    return NULL;
  a->num = i;
  a->piece = p;
  return a;
}

struct pieceAction* newPieceAction(bool d, struct RBTree_pieceType *p){
  struct pieceAction *a = malloc(sizeof(struct pieceAction));
  if(a == NULL)
    return NULL;
  a->defaultState = d;
  a->exceptions = p;
  return a;
}

struct intPieceAction* newIntPieceAction(bool d, struct RBTree_intPiece *p){
  struct intPieceAction *a = malloc(sizeof(struct intPieceAction));
  if(a == NULL)
    return NULL;
  a->defaultState = d;
  a->exceptions = p;
  return a;
}

struct nodeAction* newNodeAction(bool d, struct RBTree_charWrapper *p){
  struct nodeAction  *a = malloc(sizeof(struct nodeAction));
  if(a == NULL)
    return NULL;
  a->defaultState = d;
  a->exceptions = p;
  return a;
}


struct charWrapper* newCharWrapper(char *c){
  struct charWrapper  *a = malloc(sizeof(struct charWrapper));
  if(a == NULL)
    return NULL;
  a->c = c;
  return a;
}

struct intPiece* emptyIntPiece(struct pieceType *p){
  return newIntPiece(1, p);
}

struct pieceAction* emptyPieceAction(bool d){
  return newPieceAction(d, emptyRBTree_pieceType());
}

struct intPieceAction* emptyIntPieceAction(bool d){
  return newIntPieceAction(d, emptyRBTree_intPiece());
}

struct nodeAction* emptyNodeAction(bool d){
  return newNodeAction(d, emptyRBTree_charWrapper());
}

struct charWrapper* emptyCharWrapper(void){
  return newCharWrapper(NULL);
}





#define DOESTHIS(name, type) bool can ## name ## Raw(struct pieceType *x, int id){ \
    return (AFFIX(findRB, type)(x->name->exceptions, id) != NULL) ^ (x->name->defaultState);}


DOESTHIS(Kill, pieceType); // surprised the extra ; is ok, but that's good for me since it looks more normal
DOESTHIS(Carry, intPiece);
DOESTHIS(Combo, pieceType);
DOESTHIS(Recapture, intPiece);
DOESTHIS(Travel, charWrapper);
DOESTHIS(MountAttack, pieceType);
DOESTHIS(DismountAttack, pieceType);

#undef DOESTHIS

void startCarrying(struct piece *p, struct piece *o){
  insertRB_piece(p->piecesCarried, o);
}

void stopCarrying(struct piece *p, struct piece *o){
  removeRB_piece(p->piecesCarried, o->id);
}

