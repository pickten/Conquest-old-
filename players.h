typedef struct Player {
  int movesLeft;
  int color; // 0 is white, 1 is black; also possibility of 2, 3
  // char * prevMoves; // keeps track of previous moves; will get to later
}Player;

extern Player * players[];

void makePlayer(int color); // God giveth

void destructPlayer(int color); // God taketh away

void destructAllPlayers(); // Rapture

void parseMove(Player * currentPlayer);

int checkAdj(source, dest);
