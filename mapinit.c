#include "mapinit.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

Node *nodes[15];

void testmapinit();

void mapinit(char *input) {
  /* Returns an array of pointers to nodes; each node contains pointers to its adjacent nodes.
   * If there is no file matching the input, it returns 0.
   */
  // char mapChoice = *input;
  int isInit = 0;
  while (isInit == 0) {
    if (strcmp(input, "test") == 0) {
      testmapinit();
      isInit = 1;
    }
  }
  return;
}

void printNodes(Node *node0) {
  int i = 0;
  for (; i < 14; i++) {
    // int nodeSize = sizeof(Node);
    char *currentNodeType = nodes[i]->type;
    printf("node%d type: %s", i, currentNodeType);
    }
}

/* EXPERIMENTAL SHIT */

void addNodeToList(expNode * toAdd, int id) {
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
  /* mallocs a node, adds its pointer to expnodes */
  ExpNode newNode = malloc(sizeof(expNode)); /* for optimization, store sizeof(expNode) in a variable
					     and use that when loading from a config */
  newNode.id = id;
  int adjIDs[4] = {NULL, NULL, NULL, NULL}
  newnode.type = type;
  newnode.piece = NULL;
  addNodeToList(newNode, id);
}

void attachPieceToNode(int id, Piece *toAdd) {
  expnodes[id]->piece = toAttach;
  return;
}

Piece getPieceFromNode(int id) {
  Piece toGet = nodes[id]->piece;
  return toGet;
}

void attachLinkToNode(int id, int toLink) {
  /* attaches a single node to Node id */
  int i = 0;
  expNode base = *nodes[id];
  for(; i < 4; i++) {
    if (base.ajdIDs[i] == NULL) {
	base.adjIDs[i] = toLink;
	return;
      }
    if (base.adjIDs[i] == toLink) {
      return;
    }
  }
  return;
}

void destructNode(int id) {
  numNodes = sizeof(expnodes);
  int i = 0;
  for (; i < numNodes; i++) {
    if (nodes[i]->id == id) {
      free(nodes[i]);
      return;
    }
  }
  printf("node not found");
  return;
}

void freeAllNodes(Node expNode) {
  /* THIS NEEDS TO BE RUN AT THE END OF EVERY GAME SO WE DON'T HAVE A MEMORY LEAK 
   * (if we use a malloc implementation)
   */
  numNodes = sizeof(expnodes);
  int i = 0;
  for (; i < numNodes; i++) {
    free(nodes[i]);
  }
  return;
}



void testmapinit() {
  /* initializing the nodes */
  extern Node node0;
  extern Node node1;
  extern Node node2;
  extern Node node3;
  extern Node node4;
  extern Node node5;
  extern Node node6;
  extern Node node7;
  extern Node node8;
  extern Node node9;
  extern Node node10;
  extern Node node11;
  extern Node node12;
  extern Node node13;
  extern Node node14;
  // Node * nodes[14]();
  nodes[0] = &node0;
  nodes[1] = &node1;
  nodes[2] = &node2;
  nodes[3] = &node3;
  nodes[4] = &node4;
  nodes[5] = &node5;
  nodes[6] = &node6;
  nodes[7] = &node7;
  nodes[8] = &node8;
  nodes[9] = &node9;
  nodes[10] = &node10;
  nodes[11] = &node11;
  nodes[12] = &node12;
  nodes[13] = &node13;
  nodes[14] = &node14;
  /* head node - CAPITOL*/
  node0.adj0 = &node1;
  node0.adj1 = &node2;
  node0.adj2 = &node9;
  node0.id = 0;
  node0.type = "L";
  /* node1 */
  node1.adj0 = &node0;
  node1.adj1 = &node2;
  node1.adj2 = &node3;
  node1.id = 1;
  node1.type = "L";
  /* node2 */
  node2.adj0 = &node0;
  node2.adj1 = &node1;
  node2.adj2 = &node4;
  node2.id = 2;
  node2.type = "L";
  /* node3 */
  node3.adj0 = &node1;
  node3.adj1 = &node5;
  node3.id = 3;
  node3.type = "L";
  /* node4 */
  node4.adj0 = &node2;
  node4.adj1 = &node6;
  node4.id = 4;
  node4.type = "L";
  /* node5 */
  node5.adj0 = &node3;
  node5.adj1 = &node6;
  node5.adj2 = &node7;
  node5.id = 5;
  node5.type = "L";
  /* node6 */
  node6.adj0 = &node4;
  node6.adj1 = &node5;
  node6.adj2 = &node7;
  node6.adj3 = &node11;
  node6.id = 6;
  node6.type = "L";
  /* node7 - CAPITOL */
  node7.adj0 = &node5;
  node7.adj1 = &node6;
  node7.adj2 = &node12;
  node7.id = 7;
  node7.type = "P";
  /* node8 */
  node8.adj0 = &node1;
  node8.adj1 = &node13;
  node8.id = 8;
  node8.type = "S";
  /* node9 */
  node9.adj0 = &node0;
  node9.adj1 = &node10;
  node9.id = 9;
  node8.type = "S";
  /* node10 */
  node10.adj0 = &node9;
  node10.adj1 = &node11;
  node10.id = 10;
  node10.type = "S";
  /* node11 */
  node11.adj0 = &node6;
  node11.adj1 = &node10;
  node11.adj2 = &node14;
  node11.id = 11;
  node11.type = "S";
  /* node12 */
  node12.adj0 = &node7;
  node12.adj1 = &node13;
  node12.adj2 = &node14;
  node12.id = 12;
  node12.type = "S";
  /* node13 */
  node13.adj0 = &node8;
  node13.adj1 = &node12;
  node13.id = 13;
  node13.type = "S";
  /* node14 */
  node14.adj0 = &node11;
  node14.adj1 = &node12;
  node14.id = 14;
  node14.type = "S";
  /* all done with nodes */
  return;
}
