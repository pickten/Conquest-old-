#include "stack.h"
#include <stdlib.h>

struct AFFIX(stack, STACK_TYPE)* AFFIX(newStack, STACK_TYPE)(void){
  struct AFFIX(stack, STACK_TYPE)* a = malloc(sizeof(struct AFFIX(stack, STACK_TYPE)));
  if(a == NULL)
    return NULL;
  
  a->count = 0;
  a->data = NULL;
  return a;
  
}

struct STACK_TYPE * AFFIX(popStack, STACK_TYPE)(struct AFFIX(stack, STACK_TYPE) *s){
  if(s->data == NULL)
    return NULL;
  
  struct STACK_TYPE * ans = s->data->item;
  s->data = s->data->next;
  s->count--;
  return ans;
  
}


struct STACK_TYPE * AFFIX(peekStack, STACK_TYPE)(struct AFFIX(stack, STACK_TYPE) *s){
  if(s->data == NULL)
    return NULL;
  
  return s->data->item;
}

struct AFFIX(stack, STACK_TYPE)* AFFIX(pushStack, STACK_TYPE)(struct AFFIX(stack, STACK_TYPE) *s, struct STACK_TYPE *item){
  s->count++;
  s->data = AFFIX(newLinked, STACK_TYPE)(item, s->data);
  return s;
  
}

void AFFIX(deleteStack, STACK_TYPE)(struct AFFIX(stack, STACK_TYPE) *data){
  AFFIX(deleteLinked, STACK_TYPE)(data->data);
  free(data);
}

#undef STACK_TYPE
