#include "rbtrees.h"
#include <stdlib.h>

#ifndef RBTREE
#define RBTREE AFFIX(RBTree, RBTREE_TYPE)
#endif

#define STACK_TYPE RBTREE
#include "stack.h"
#undef STACK_TYPE


struct RBTREE * AFFIX(newRBTree, RBTREE_TYPE)(bool color, struct RBTREE_TYPE *item,
                                                   struct RBTREE *left,
                                                   struct RBTREE *right){
  
  struct RBTREE *ans = malloc(sizeof(struct RBTREE));
  if(ans == NULL)
    return NULL;  /* we should probably use xmalloc or something else to fail better */
  
  ans->color = color;
  ans->item = item;
  ans->left = left;
  ans->right = right;
  return ans;
  
}


struct RBTREE * AFFIX(emptyRBTree, RBTREE_TYPE)(void){
  return AFFIX(newRBTree, RBTREE_TYPE)(false, NULL, NULL, NULL);
  
}


bool AFFIX(verifyRBTree, RBTREE_TYPE) (struct RBTREE *tree){
  return true;  /* not implemented yet */
  
}


struct RBTREE * AFFIX(insertRB, RBTREE_TYPE) (struct RBTREE *tree, struct RBTREE_TYPE *newval){
  if(tree == NULL)
    return AFFIX(newRBTree, RBTREE_TYPE)(false, newval, AFFIX(emptyRBTree, RBTREE_TYPE)(), AFFIX(emptyRBTree, RBTREE_TYPE)());
  
  struct RBTREE *node = tree;
  struct AFFIX(stack, RBTREE) *ancestorStack = AFFIX(newStack, RBTREE)(); /* flycheck was yelling about this line but I think it's actualy ok */
  
  
  while(node != NULL){
    if(node->item == NULL)
      break;
    
    int x = RBTREE_COMPARISON(node->item);
    if(x == RBTREE_COMPARISON(newval)){  /* we assume we aren't being given NULL; that's a really rude thing to input, after all */
      AFFIX(deleteStack, RBTREE)(ancestorStack);  /* all done here, time to exit */
      node->item = newval;
      return tree;
    }
    
    AFFIX(pushStack, RBTREE)(ancestorStack, node);
    
    if(x > RBTREE_COMPARISON(newval)) {
      node = node->left;
    } else {
      node = node->right;
    }
    
  }
  // Recap: either we found it (instabreak) or node is leaf, ancestorStack is a stack of all previous ancestorStack

  struct RBTREE *parent = AFFIX(peekStack, RBTREE)(ancestorStack);
  struct RBTREE *grandparent;
  struct RBTREE *uncle;
  bool lastLeft = parent->left == node;  /* initialization stuff */

  /* insert our node */
  if(lastLeft){  
    parent->left = AFFIX(newRBTree, RBTREE_TYPE)(true, newval,
                                                 AFFIX(emptyRBTree, RBTREE_TYPE)(),
                                                 AFFIX(emptyRBTree, RBTREE_TYPE)());
    node = parent->left;
    
  } else {
    parent->right = AFFIX(newRBTree, RBTREE_TYPE)(true, newval,
                                                  AFFIX(emptyRBTree, RBTREE_TYPE)(),
                                                  AFFIX(emptyRBTree, RBTREE_TYPE)());
    node = parent->right;
    
  }
  
  /* now we need to rebalance */
  
  while(true){
    
    if(ancestorStack->count == 0) {  /* At the head of the tree */
      node->color = false;   /* just to be sure */
      
      AFFIX(deleteStack, RBTREE)(ancestorStack);  /* be a good person */
      return node;  /* finish */
    }

    parent = AFFIX(popStack, RBTREE)(ancestorStack);
    lastLeft = parent->left == node;
  
    /* done if parent is black */
    /* since we can't have a new RR, path lengths are still good, so we're done */
    if(!parent->color){  
      AFFIX(deleteStack, RBTREE)(ancestorStack);
      return tree;
    }
  
    /* now we know parent->color == true and have inserted. Here's where things get messy. */

    /* first, we eliminate the case of a red uncle */

    grandparent = AFFIX(popStack, RBTREE)(ancestorStack);   // must exist because root was black

    lastLeft = grandparent->left == parent;  /* knew I should've taken that left at Albuquerque */

    uncle = lastLeft ? grandparent->right : grandparent->left;

    if(uncle != NULL && uncle->color){
      grandparent->color = true; // for... reasons (i.e. the granduncle & black path lengths)
      parent->color = false; // to compensate and fix the RR 
      uncle->color = false; // to compensate
      node = grandparent; // up we go!
      continue;
    }

  }

  /* so we have a red parent and black uncle/grandparent, and we're red */

  struct RBTREE *greatgp = AFFIX(popStack, RBTREE)(ancestorStack);  /* used for the final rotation */
  
  AFFIX(deleteStack, RBTREE)(ancestorStack);  /* don't need this anymore */

  bool parentdir = grandparent->left == parent;
  bool dir = parent->left == node;

  /* rotate parent and node if needbe so node and parent are aligned */
  if(parentdir && !dir){
    struct RBTREE *temp = node;
    
    grandparent->left = node;
    parent->right = node->left;
    node->left = parent;
    
    node = parent;  /* swapping to keep grand->parent->node*/
    parent = temp;
    
  } else if(dir && !parentdir){
    struct RBTREE *temp = node;
    
    grandparent->right = node;
    parent->left = node->right;
    node->right = parent;
    node = parent;
    parent = temp;
    
  }

  parent->color = false;
  grandparent->color = true;
  if(parentdir){
    grandparent->left = parent->right;
    parent->right = grandparent;
  } else {
    grandparent->right = parent->left;
    parent->left = grandparent;
  }

  if(greatgp == NULL)
    return parent;

  if(greatgp->left == grandparent){
    greatgp->left = parent;
  } else {
    greatgp->right = parent;
  }
  
  
  return tree;  /* Just to make it compile */
  
}

struct RBTREE_TYPE * AFFIX(findRB, RBTREE_TYPE) (struct RBTREE *tree, int id){
  struct RBTREE *node = tree;
  while(node != NULL){
    int x = RBTREE_COMPARISON(node->item);
    /* This is probably a bad idea but I think it's reasonable to assume an id field.
       Regardless, an easy change */
    
    if(x > id)
      node = node->left;

    if(x < id)
      node = node->right;
    return node->item;
  }

  return NULL;
  
}

struct RBTREE_TYPE * AFFIX(removeRB, RBTREE_TYPE) (struct RBTREE *tree, int id){
  if(tree == NULL)
    return NULL;

  struct AFFIX(stack, RBTREE) *ancestorStack = AFFIX(newStack, RBTREE)(); /* flycheck was yelling about this line but I think it's actualy ok */
  
  struct RBTREE *node = tree;
  
  
  while(node != NULL){
    if(node->item == NULL){
      AFFIX(deleteStack, RBTREE)(ancestorStack);  
      return NULL;
    }
    
    int x = RBTREE_COMPARISON(node->item);
    
    if(x == id)
      break;
    
    AFFIX(pushStack, RBTREE)(ancestorStack, node);
    
    
    if(x > id)
      node = node->left;
    else
      node = node->right;
  }

  struct RBTREE_TYPE *ans = node->item;  /* we'll return this in a minute */


  bool righthalf = node->right->item != NULL;  /* what side are we working on? */
  
  struct RBTREE *min = righthalf ? node->right : node->left;
  AFFIX(pushStack, RBTREE)(ancestorStack, node);
  
  
  while(min != NULL){
    if(min->item == NULL || (righthalf ? min->left : min->right) == NULL){
      AFFIX(deleteStack, RBTREE)(ancestorStack);
      return NULL;
    }

    if((righthalf ? min->left : min->right)->item == NULL) {
      node->item = min->item;  /* removed the item successfully */
      node = min; // we want to cut a node with a leaf, either node if min was NULL, or min. This simplifies things
      break;  
    }

    AFFIX(pushStack, RBTREE)(ancestorStack, min);
    min = righthalf ? min->left : min->right;  /* down down dooby dooby down */
  }

  struct RBTREE *parent = AFFIX(popStack, RBTREE)(ancestorStack);

  if(parent == NULL){

    node->left = NULL;
    node->item = NULL;
    node->color = false;
    
    AFFIX(deleteStack, RBTREE)(ancestorStack);
    return ans;
    
  }
  
  bool dir = parent->left == node;
  
  
  if(node->left == NULL || node->right == NULL) {   // somehow got a leaf/bad node; kill it with fire
    if(dir)
      parent->left = NULL;
    else
      parent->right = NULL;
    AFFIX(deleteStack, RBTREE)(ancestorStack);
    return ans;
  }

  /* to recap:
     - node is a node with at least one leaf child that needs to be removed, and parent is its parent
     - ancestorStack holds the ancestry of the parent
   */
  struct RBTREE *child = (node->right->item == NULL) ? node->left : node->right;
  // we guarantee that child's sibling will always be a leaf
  
  if(dir)
    parent->left = child;
  else
    parent->right = child;

  free(node);  /* not sure this is needed but it's safer */
  
  
  if(node->color){
    // we were OK if the original was red.
    
    AFFIX(deleteStack, RBTREE)(ancestorStack);
    return ans;
  }

  // thus, node is black.
  // if we have a red child we can still do this, but need to repaint.
  if(child->color){
    child->color = false;
    AFFIX(deleteStack, RBTREE)(ancestorStack);
    return ans;
  }
  
  node = child;
  struct RBTREE *sibling = dir ? parent->right : parent->left; // node's sibling
  struct RBTREE *grandparent = AFFIX(popStack, RBTREE)(ancestorStack);
  bool parentdir = grandparent->left == parent;
  
  
  while(true){ // time to climb the tree
    
    if(ancestorStack->count == 0){
      // at the root
      AFFIX(deleteStack, RBTREE)(ancestorStack);
      return ans;
    }

    if(sibling->color){ // sibling cannot be leaf because that side has more black ancestorStack than our side
      // rotate the parent and make it red,
      // this removes a red from its side and adds a black to our side
      sibling->color = false; 
      parent->color = true;
      
      if(dir) {
        parent->right = sibling->left;
        sibling->left = parent;
      } else {
        parent->left = sibling->right;
        sibling->right = parent;
      }
      
      if(parentdir)
        grandparent->left = sibling;
      else
        grandparent->right = sibling;
      AFFIX(deleteStack, RBTREE)(ancestorStack);
      return ans;
    }

    if(sibling->left->color || sibling->right->color || parent->color)
      break;
    
    // both black children & black parent
    sibling->color = true;
    // just remove a node by brute force (i.e. recolor)
    // now we've unbalanced the next level up!
    // we're now going to operate on the parent
    node = parent; 
    parent = grandparent;
    dir = parentdir;
    sibling = dir ? parent->right : parent->left;
    grandparent = AFFIX(popStack, RBTREE)(ancestorStack);
    parentdir = grandparent->left == parent;
      
  } // finally done with the loop

  AFFIX(deleteStack, RBTREE)(ancestorStack);

  if(parent->color && (!sibling->left->color) && (!sibling->right->color)){
    // we can just swap the colors, like our rotation trick before but safer and easier
    parent->color = false;
    sibling->color = true;
    AFFIX(deleteStack, RBTREE)(ancestorStack);
    return ans;
  }

  // now, we want to align things

  if(dir && sibling->left->color && (!sibling->right->color)){
    parent->right = sibling->left;

    // i.e. if sibling has a colored node in an annoying spot on the right
    sibling->color = true;
    sibling->left->color = false;
    sibling->left = sibling->left->right;
    sibling->left->right = sibling;
    
  }

  if((!dir) && (!sibling->left->color) && sibling->right->color){
    parent->left = sibling->right;
    
    // i.e. if sibling has a colored node in an annoying spot on the left
    sibling->color = true;
    sibling->right->color = false;
    sibling->right = sibling->right->left;
    sibling->right->left = sibling;
    
  }

  if(parentdir) // final step: rotate the parent and substuff. We'll do the grandparent stuff now.
    grandparent->left = sibling;
  else
    grandparent->right = sibling;
  
  sibling->right->color = false;
  sibling->color = parent->color;
  parent->color = false;
  parent->right = sibling->left;
  sibling->left = parent;
  
  
  return ans;  /* Just to make it compile */
  
}

void AFFIX(deleteRB, RBTREE_TYPE)(struct RBTREE *data) {
  if(data == NULL)
    return;
  
  AFFIX(deleteRB, RBTREE_TYPE)(data->left);
  AFFIX(deleteRB, RBTREE_TYPE)(data->right);
  free(data);
  
}



#undef RBTREE  /* just to be on the safe side */
#undef RBTREE_COMPARISON
#undef RBTREE_TYPE
