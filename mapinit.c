#include "mapinit.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void testMapInit();

NodeList *  mapInit(char *input) {
  /* Returns a NodeList of pointers to nodes; each node contains pointers to its adjacent nodes.
   * Reads from string of characters passed to it somewhere else
   */
  // do stuff
  return headNode;
}

void addNodeToList(Node * toAdd, int id) {
  int i;
  NodeList * currentNode = allNodes;
  for (i = 0; i < id; i++) {
    currentNode = currentNode->next;
  }
  currentNode->next = malloc(sizeof(NodeList));
  currentNode->nodePointer->nodePointer = toAdd;
  currentNode->next = NULL;
  currentNode->index = id;
}

void newNode(int id, char type) {
  // mallocs a node, adds its pointer to NodeList
  Node newNode = malloc(sizeof(Node)); /* for optimization, store sizeof(expNode) in a 
						variable and use that when loading from a config */
  newNode.id = id;
  int adjIDs[4] = {NULL, NULL, NULL, NULL}
  newnode.type = type;
  newnode.piece = NULL;
  addNodeToList(newNode, id);
}

void attachPieceToNode(int id, Piece *toAdd) {
  currentNode = findNode(id);
  currentNode->piece = toAttach;
  return;
}

Piece getPieceFromNode(int id) {
  currentNode = findNode(id);
  Piece toGet = currentNode->piece;
  return toGet;
}

Node * findNode(int id) {
  NodeList * currentNodeList = allNodes;
  while (currentNodeList->index != id) {
    currentNodeList = currentNodeList->next;
  }
  Node * currentNode = currentNodeList->nodePointer;
  return currentNode;
}

void attachLinkToNode(int id, int toLink) {
  // attaches a single node to Node id
  int i;
  Node * base = findNode(id);
  for(i = 0; i < 4; i++) {
    if (base->adjIDs[i] == NULL) {
	base.adjIDs[i] = toLink;
	return;
      }
    if (base->adjIDs[i] == toLink) {
      return;
    }
  }
  return;
}

void destructNode(int id) {
  // frees a single node, but keeps allNodes entry with a null pointer
  numNodes = sizeof(expnodes);
  int i = 0;
  Node * toDestruct = findNode(id);
  free(toDestruct);
  return;
}

void freeAllNodes(Node allNodes) {
  /* THIS NEEDS TO BE RUN AT THE END OF EVERY GAME SO WE DON'T HAVE A MEMORY LEAK 
   * (if we use a malloc implementation)
   */
  NodeList * currentNode = allNodes;
  while (currentNode->next != NULL) {
    NodeList * nextNode = currentNode->next;
    free(currentNode);
    currentNode = nextNode;
  }
  return;
}
