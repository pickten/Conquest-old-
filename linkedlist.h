#include "genericmacros.h"


#ifndef LINKED_TYPE
struct linked_placeholder {
  void *item;
};
#define LINKED_TYPE linked_placeholder
#endif

#if NDEF(AFFIX(LINKED_H, LINKED_TYPE)) && NDEF(AFFIX(STACK_H, LINKED_TYPE)) && NDEF(AFFIX(RBTREES_H, LINKED_TYPE))
/* Can't automatically DEF it, sorry. C is weird.  */

/* A linked list of void pointers; will be used in implementation, and possibly elsewhere */
struct AFFIX(linked, LINKED_TYPE) {
  struct LINKED_TYPE * item;
  struct AFFIX(linked, LINKED_TYPE) *next;
};

/* make a new void_ll; pretty trivial */
struct AFFIX(linked, LINKED_TYPE)* AFFIX(newLinked, LINKED_TYPE)(struct LINKED_TYPE *item, struct AFFIX(linked, LINKED_TYPE) *data);

void AFFIX(deleteLinked, LINKED_TYPE)(struct AFFIX(linked, LINKED_TYPE) *data);

#endif
