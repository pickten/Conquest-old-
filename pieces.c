#include "pieces.h"
#define DOESTHIS(name, type) bool can ## name ## Raw(struct pieceType *x, int id){                     \
    return (AFFIX(findRB, type)(x->name->exceptions, id) != NULL) ^ (x->name->defaultState);}


DOESTHIS(Kill, pieceType); // surprised the extra ; is ok, but that's good for me since it looks more normal
DOESTHIS(Carry, intPiece);
DOESTHIS(Combo, pieceType);
DOESTHIS(Recapture, intPiece);
DOESTHIS(Travel, charWrapper);
DOESTHIS(MountAttack, pieceType);
DOESTHIS(DismountAttack, pieceType);

#undef DOESTHIS
