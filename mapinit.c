#include "mapinit.h"
#include <stdlib.h>
#include <string.h>

int mapinit(input) {
  /* Returns an array of pointers to nodes; each node contains pointers to its adjacent nodes.
   * If there is no file matching the input, it returns 0.
   */
  Node *head = malloc(sizeof(Node));
  if (strcmp(*input, "test") == 0) {
    testmapinit(head);
  }
  else {
    return 0;
  }
  return nodes;
}


int testmapinit(head) {
  /* initializing the nodes */
  Node * node1;
  Node * node2;
  Node * node3;
  Node * node4;
  Node * node5;
  Node * node6;
  Node * node7;
  Node * node8;
  Node * node9;
  Node * node10;
  Node * node11;
  Node * node12;
  Node * node13;
  Node * node14;
  nodes[14] = {node1, node2, node3, node4, node5, node6, node7, node8, node9, node10,
		   node11, node12, node13, node14};
  /* head node - CAPITOL*/
  head->adj0 = node1;
  head->adj1 = node2;
  head->adj2 = node9;
  head->type = "L";
  /* node1 */
  node1->adj0 = head;
  node1->adj1 = node2;
  node1->adj2 = node3;
  node1->type = "L";
  /* node2 */
  node2->adj0 = head;
  node2->adj1 = node1;
  node2->adj2 = node4;
  node2->type = "L";
  /* node3 */
  node3->adj0 = node1;
  node3->adj1 = node5;
  node3->type = "L";
  /* node4 */
  node4->adj0 = node2;
  node4->adj1 = node6;
  node4->type = "L";
  /* node5 */
  node5->adj0 = node3;
  node5->adj1 = node6;
  node5->adj2 = node7;
  node5->type = "L";
  /* node6 */
  node6->adj0 = node4;
  node6->adj1 = node5;
  node6->adj2 = node7;
  node6->adj3 = node11;
  node6->type = "L";
  /* node7 - CAPITOL */
  node7->adj0 = node5;
  node7->adj1 = node6;
  node7->adj2 = node12;
  node7->type = "P";
  /* node8 */
  node8->adj0 = node1;
  node8->adj1 = node13;
  node8->type = "S";
  /* node9 */
  node9->adj0 = head;
  node9->adj1 = node10;
  node8->type = "S";
  /* node10 */
  node10->adj0 = node9;
  node10->adj1 = node11;
  node10->type = "S";
  /* node11 */
  node11->adj0 = node6;
  node11->adj1 = node10;
  node11->adj2 = node14;
  node11->type = "S";
  /* node12 */
  node12->adj0 = node7;
  node12->adj1 = node13;
  node12->adj2 = node14;
  node12->type = "S";
  /* node13 */
  node13->adj0 = node8;
  node13->adj1 = node12;
  node13->type = "S";
  /* node14 */
  node14->adj0 = node11;
  node14->adj1 = node12;
  node14->type = "S";
}

void freeallnodes(node) {
  /* THIS NEEDS TO BE RUN AT THE END OF EVERY GAME SO WE DON'T HAVE A MEMORY LEAK 
   * (if we use a malloc implementation, i.e. nodes defining nodes)
   */
  int len = (sizeof(nodes) / sizeof(nodes[0]));
  int i = 0;
  for (i; i < len; i++) {
    free(nodes[i]);
  }
  return;
}
