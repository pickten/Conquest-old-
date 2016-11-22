#include "genericmacros.h"


#ifndef STACK_TYPE
struct stack_placehoder {
  void *item;
};
#define STACK_TYPE stack_placeholder
#endif


#if NDEF(AFFIX(STACK_H, STACK_TYPE)) && NDEF(AFFIX(RBTREES_H, STACK_TYPE))

#define LINKED_TYPE STACK_TYPE
#include "linkedlist.h"  /* again, can't generically do the defs, so please, o caller in the sky, do this yourself */
#undef LINKED_TYPE

struct AFFIX(stack, STACK_TYPE) {
  int count;
  struct AFFIX(linked, STACK_TYPE) *data;
};

/* make a new stack; not much of interest in here */
struct AFFIX(stack, STACK_TYPE)* AFFIX(newStack, STACK_TYPE)(void);

/* pop the stack */
struct STACK_TYPE * AFFIX(popStack, STACK_TYPE) (struct AFFIX(stack, STACK_TYPE) *s);

/* peek in the stack */
struct STACK_TYPE * AFFIX(peekStack, STACK_TYPE) (struct AFFIX(stack, STACK_TYPE) *s);


/* push to the stack */
struct AFFIX(stack, STACK_TYPE)* pushStack(struct AFFIX(stack, STACK_TYPE) *s, struct STACK_TYPE* item);


#endif
