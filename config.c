#include "config.h"
#include <stdio.h>
#include <string.h>
#include "genericmacros.h"
#include "mapinit.h"
#include "pieces.h"

#define NEWNODE(a,b) (newNode(a,b), expnodes[a])

struct intint *newIntint(int a, int b, char c, int d){
  struct intint  *x = malloc(sizeof(struct intint));
  if(x == NULL)
    return NULL;
  x->a = a;
  x->b = b;
  x->c = c;
  x->d = d;
  
  
  return x;
}


struct config *newConfig(void){
  struct config  *a = malloc(sizeof(struct config));
  if(a == NULL)
    return NULL;
  a->map = NEWNODE(-1,NULL);
  a->allNodes = emptyRBTree_Node();
  
  a->types = emptyRBTree_pieceType();
  return a;
}

void deleteConfig(struct config *cfg){
  deleteRB_pieceType(cfg->types);
  deleteRB_Node(cfg->allNodes);
  
  free(cfg);
}




struct environment *newEnvironment(void){
  struct environment  *a = malloc(sizeof(struct environment));
  if(a == NULL)
    return NULL;
  a->nodeID = 0;
  a->typeID = 0;
  a->node = NULL;
  a->type = NULL;
  a->p = NULL;
  a->ip = NULL;
  a->n = NULL;
  a->pExceptions = newStack_intint();
  a->ipExceptions = newStack_intint();
  a->oExceptions = newStack_intint();
  
  
  return a;
}

struct environment *newPieceTypeEnv(struct environment *e){
  if(e->type != NULL) {
    return e;
  }
  
  e->type = newPieceType(e->typeID++);
  return e;
}

  
struct environment *NEWNODEEnv(struct environment *e, char * type){
  if(e->node != NULL) {
    return e;
  }
  
  e->node = NEWNODE(e->nodeID++, type);
  return e;
}


void deleteEnvironment(struct environment *e){
  deletePieceType(e->type);
  deleteNode(e->node);
  deleteNodeAction(e->n);
  deleteIntPieceAction(e->ip);
  deletePieceAction(e->p);
  deleteStack_intint(e->pExceptions);  /* we'll free the intints before calling this */
  deleteStack_intint(e->ipExceptions);
  deleteStack_intint(e->oExceptions);
  
  free(e);
}

struct environment *insertException(struct environment *e, int id, int val){
  if(e->p != NULL){ // working on a pieceAction
    pushStack_intint(e->pExceptions, newIntint(e->typeID, id, e->actionType, 0));

    if(val >=0)
      pushStack_intint(e->pExceptions, newIntint(e->typeID, val, e->actionType, 0));
  } else if(e->ip != NULL){ //working on an intPieceAction
    pushStack_intint(e->ipExceptions, newIntint(e->typeID, id, e->actionType, val));
  } else if(e->n != NULL){ //working on a nodeAction
    e->n->exceptions = insertRB_charWrapper(e->n->exceptions, newCharWrapper(id));
    if(val >= '\1') // \0 is terrible and shouldn't be used
      e->n->exceptions = insertRB_charWrapper(e->n->exceptions, newCharWrapper(val));

  } else { //not working on anything, fill in the preamble
    e->preambleLines++;
    if(e->preambleLines == 1)
      e->type->maxMoves = id;
    if(e->preambleLines == 2)
      e->type->carryCap = id;
    if(val >= 0) {
      e->preambleLines++;
      if(e->preambleLines == 2)
        e->type->carryCap = val;
    }
  }
  
    
  return e;
  
}

char* fileIsIncluded(char *l){
  if(l[0] == '#')  /* probably could use better syntax but meh. */
    return strcpy(malloc(sizeof(char[100])), l+1);
  return NULL;
}


struct environment *parseFileEnv(struct config *c, struct environment *e, char * l, FILE *f){
  if(l == NULL) // if we're out of space, we're out of luck!
    return NULL;
  
  char* inclusion;
  char* line;
  int linum = 0;
  bool fatal = false;
  
  while(!feof(f)){
    fgets(l, 100, f);
    linum++;
    for(line = l; line[0] == ' ' || line[0] == '\t'; line++);
    
    if(line[0] == ';' || line[0] == '\0') // we use ; comments because we fookin can; also allow extra newlines.
      continue;
    
    inclusion = fileIsIncluded(line);
    if(inclusion != NULL) {
      e = parseFileEnv(c, e, inclusion, fopen(inclusion, "r")); // can't really avoid the recursion, unfortunately
      continue;
    }

    if(e->node != NULL) {
      if(line[0] == ')') { // time to end this node!
        c->allNodes = insertRB_Node(c->allNodes, e->node);
        e->node = NULL;
        
      } else if(e->preambleLines == 0) { // getting the current type for our node
        e->preambleLines++;
        
        int lineis = atoi(line);         // I know atoi sucks. But it's way simpler for this use case
        
        
        e->playerID = lineis;
        

      } else if(e->preambleLines == 1) {
        e->preambleLines++;

        int lineis = atoi(line);
        
        if(e->typeID < lineis) {
          printf("Fatal error in config: you were a terrible person on line %d and used a piece type in a node before it was defined (\"%s\")", linum, line);
          printf("You can fix this by putting your piece type declarations first. I\'m not doing it for you.");
          fatal = true;
          break;
        }

        attachPieceToNode(e->node->id, newPiece(e->pieceID++, findRB_pieceType(c->types, lineis)));        
        
        givePlayerPiece(e->playerID, getPieceFromNode(e->node->id));
        
      } else if(e->preambleLines == 2) {
        e->node->type = strcpy(malloc(sizeof(char[100])), line);
        e->preambleLines++;
        
      } else {
        pushStack_intint(e->oExceptions, newIntint(e->node->id, atoi(line),' ',0)); 
      }
      
    } else if (e->type != NULL) {
      if(line[0] == '}') {
        c->types = insertRB_pieceType(c->types, e->type);
        e->type = NULL;
        
      }
      
      if(line[0] >= '0' && line[0] <= '9'){
        if(e->currentLine) {
          e->currentLine = false;
          insertException(e, atoi(line), e->workingLine);
        } else {
          e->currentLine = true;
          e->workingLine = atoi(line);
        }
        continue;
      }
      
      if (e->currentLine) {
        e->currentLine = false;
        insertException(e, e->workingLine, -1);
      }
      
#define AUTO_TYPE(c,n,r,t,u) if(e->actionType == c){    \
        e->type->n = e->r;                              \
        e->r = NULL;                                    \
      }                                                 \
      if(line[0] == c){                                 \
        e->r = empty ## t(line[1] == c );               \
      }                                                         
#define RUNTYPE(x)                                              \
      x('k', Kill, p, PieceAction,pieceType);                   \
      x('c', Carry, ip, IntPieceAction, intPiece);              \
      x('C', Combo, p, PieceAction, pieceType);                 \
      x('r', Recapture, ip, IntPieceAction, intPiece);          \
      x('m', MountAttack, p, PieceAction, pieceType);           \
      x('d', DismountAttack, p, PieceAction, pieceType);       
      
      RUNTYPE(AUTO_TYPE);
      AUTO_TYPE('t', Travel, n, NodeAction, charWrapper);
#undef AUTO_TYPE      
      
      if(e->p != NULL || e->n != NULL || e->ip != NULL){
        printf("Fatal: unknown action on line %d (\"%s\") of the working file.", linum, line);
        fatal = true;
        e->p = NULL;
        e->n = NULL;
        e->ip = NULL;
      }
      
      e->actionType = line[0];

    } else if(line[0] == '('){
      e->node = NEWNODE(e->nodeID++, NULL);
      e->preambleLines = 0;
      
    } else if (line[0] == '{') {
      e->actionType = '}';
      e->type = newPieceType(e->typeID++);
      e->type->name = strcpy(malloc(sizeof(char[100])), line + 1);
      e->preambleLines = 0;
      
    } else {
      printf("Warning: syntax error on line %d (\"%s\") of the working file. I'll pretend it was a comment and move on", linum, line);
    }
    
  }

  while(e->pExceptions->count){
    struct intint *x = popStack_intint(e->pExceptions);
    struct pieceType* y = findRB_pieceType(c->types, x->a);
    
#define AUTO_TYPE(ch,n,r,t,u)                                           \
    if(x->c == ch){                                                     \
      y->n->exceptions = AFFIX(insertRB, u)(y->n->exceptions,           \
                                            AFFIX(u,)(x->d,findRB_pieceType(c->types, x->b))); \
    }
#define intPiece_(a,b) newIntPiece(a,b) // terrible workaround
#define pieceType_(a,b) b // this is a terrible workaround to do a conditional within the AUTO_TYPE macro. Sorry.

    
    RUNTYPE(AUTO_TYPE);
    free(x);

  }

  while(e->ipExceptions->count){
    struct intint *x = popStack_intint(e->pExceptions);
    struct pieceType* y = findRB_pieceType(c->types, x->a);
    
#define AUTO_TYPE(ch,n,r,t,u)                                           \
    if(x->c == ch){                                                     \
      y->n->exceptions = AFFIX(insertRB, u)(y->n->exceptions,           \
                                            AFFIX(u,)(x->d,findRB_pieceType(c->types, x->b))); \
    }
#define intPiece_(a,b) newIntPiece(a,b) // terrible workaround
#define pieceType_(a,b) b // this is a terrible workaround to do a conditional within the AUTO_TYPE macro. Sorry.

    
    RUNTYPE(AUTO_TYPE);
    free(x);

#undef AUTO_TYPE
#undef intPiece_
#undef pieceType_
  }

  
  while(e->oExceptions->count){
    struct intint *x = popStack_intint(e->oExceptions);
    struct Node *y = findRB_Node(c->allNodes, x->a);
    addLinkToNode(y, findRB_Node(c->allNodes, x->b));
    free(x);
  }
  
  
  #undef RUNTYPE

  free(line);
  fclose(f);
  return fatal ? NULL : e;
}


struct config *parseFileRaw(FILE *f){
  if(f == NULL)
    return NULL;

  struct config *c = newConfig();
  struct environment *e = newEnvironment();
  char* cc = malloc(sizeof(char[100]));
  
  
  deleteEnvironment(parseFileEnv(c, e, cc, f));  /* we just want the fx at this point */
  return c;
}



struct config *parseFile(char *f) {
  return parseFileRaw(fopen(f, "r"));
}

#undef NEWNODE
#undef Node
