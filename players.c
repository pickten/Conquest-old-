/* In this file:
 * The amazing adventures of Count von Moves!
 * Choose your own adventure at the Tent of Input Parsing!
 * Watch as the world-renowed Un Do turns back time before your eyes!
 * Moving functions! Cute, cuddly, and great with kids! Take one home today!
 * And much, much more!
 */

#include "players.h"
#include <stdlib.h>
#include <stdio.h>

extern Player * players[2]; // Will need a way to parse this to be different

void makePlayer(int color) {
  Player newPlayer = malloc(sizeof(Player));
  if (color == 0) {
    newPlayer.movesLeft = 10;
  }
  else {
    newPlayer.movesLeft = 20;
  }
  newPlayer.color = color;
  // newPlayer->prevMoves = NULL;
  players[color] =  &newPlayer;
  return;
}

void destructPlayer(int color) {
  free(players[color]);
  return;
}

void destructAllPlayers() {
  int i = 0;
  for(; i < 2; i++) {
    free(players[i]);
  }
  return;
}

/* Move format:
 * Type Source Dest 
 * Types: (0 (soldier), 1 (knight), 2 (chariot), 3 (elephant), 4 (boat/ship), 5 (galleon), 6/7
 *        (embarked soldier 1/2)) (janky and should be changed)
 * Dest is just the id of the node to move to
 * Example: e 12
 */

void parseMove(Player * currentPlayer) {
  sscanf("%d %d %d", int * type, int * source, int * dest);
  /* you have the piece actions all squared away, so this bit is all yours; take what you like from
   * what I have here
   */
  int isAdj = checkAdj(source, dest)
  Node sourceNode = *nodes[source];
  if ((sourceNode.currentPiece->color == currentPlayer->color) and
      (sourceNode.currentPiece->type == *type) and
      (isAdj == 1))
    { // thanks to ^^ i'm using this bracketing style for readability (just this once)
    printf("NYI\n"); // do the move
    }
  currentPlayer->movesLeft -= 1;
  return;
}

int checkAdj(int * source, int * dest) {
  Node sourceNode = *nodes[source];
  if ((sourceNode.adj1 == dest) or
      (sourceNode.adj2 == dest) or
      (sourceNode.adj3 == dest) or
      (sourcenode.adj4 == dest)) {
    return 1;
  }
  else {
    return 0;
  }
}
