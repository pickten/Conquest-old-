#include <stdlib.h>
#include "linkedlist.h"


struct AFFIX(linked, LINKED_TYPE)* AFFIX(newLinked, LINKED_TYPE)(struct LINKED_TYPE *item, struct AFFIX(linked, LINKED_TYPE) *data){
  struct AFFIX(linked, LINKED_TYPE) *ans = malloc(sizeof(struct AFFIX(linked, LINKED_TYPE)));
  if(ans == NULL)
    return NULL;
  
  ans->item = item;
  ans->next = data;
  return ans;
}


