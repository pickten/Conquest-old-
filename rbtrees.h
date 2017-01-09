#include "genericmacros.h"
#include <stdbool.h>
#include <stdlib.h>


#ifndef RBTREE_TYPE
struct RBTree_placeholder {void* item; int id;};
#define RBTREE_TYPE RBTree_placeholder
#endif

#if NDEF(AFFIX(RBTREES_H, RBTREE_TYPE))  /* you'll have to define this manually. Sorry! But it's just not possible to autodefine when doing this */



struct AFFIX(RBTree, RBTREE_TYPE) {
  bool color;
  struct RBTREE_TYPE *item;  /* Well, not like I wasn't planning on using it for structs anyways */
  struct AFFIX(RBTree, RBTREE_TYPE) *left;
  struct AFFIX(RBTree, RBTREE_TYPE) *right;
};


struct AFFIX(RBTree, RBTREE_TYPE)* AFFIX(newRBTree, RBTREE_TYPE)(bool color, struct RBTREE_TYPE *item,
                                                                 struct AFFIX(RBTree, RBTREE_TYPE) *left,
                                                                 struct AFFIX(RBTree, RBTREE_TYPE) *right);

struct AFFIX(RBTree, RBTREE_TYPE)* AFFIX(emptyRBTree, RBTREE_TYPE)(void);  /* gets an empty RBTree to play with */


bool AFFIX(verifyRBTree, RBTREE_TYPE) (struct AFFIX(RBTree, RBTREE_TYPE) *tree);  /* Verify that a tree satisfies the RB properties. Unusued. */

struct AFFIX(RBTree, RBTREE_TYPE)* AFFIX(insertRB, RBTREE_TYPE) (struct AFFIX(RBTree, RBTREE_TYPE) *tree, struct RBTREE_TYPE *newval); /* Insert a value into the tree */

struct RBTREE_TYPE * AFFIX(findRB, RBTREE_TYPE) (struct AFFIX(RBTree, RBTREE_TYPE) *tree, int id); /* Locate a specific ID */

struct RBTREE_TYPE * AFFIX(removeRB, RBTREE_TYPE) (struct AFFIX(RBTree, RBTREE_TYPE) *tree, int id); /* Remove a specific ID */

void AFFIX(deleteRB, RBTREE_TYPE) (struct AFFIX(RBTree, RBTREE_TYPE) *tree);


#endif /* RBTREES_H */
