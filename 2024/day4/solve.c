#include <stdio.h>

// #define DIMENSIONS 140
#define DIM 140
#define LENGTH DIM *DIM

typedef struct vector {
  int x;
  int y;
} vector;

static char mas[] = {'M', 'A', 'S'};
static vector offsets[] = {{0, 1},  {1, 1},   {1, 0},  {1, -1},
                           {0, -1}, {-1, -1}, {-1, 0}, {-1, 1}};

typedef struct xvector {
  char letter;
  vector vec;
} xvector;
static xvector crossPattern[4][5] = {
    {{'M', {0, 0}}, {'M', {0, 2}}, {'A', {1, 1}}, {'S', {2, 0}}, {'S', {2, 2}}},
    {{'S', {0, 0}}, {'M', {0, 2}}, {'A', {1, 1}}, {'S', {2, 0}}, {'M', {2, 2}}},
    {{'S', {0, 0}}, {'S', {0, 2}}, {'A', {1, 1}}, {'M', {2, 0}}, {'M', {2, 2}}},
    {{'M', {0, 0}},
     {'S', {0, 2}},
     {'A', {1, 1}},
     {'M', {2, 0}},
     {'S', {2, 2}}}};

static inline char at(const char (*puzzle)[DIM], const vector *v) {
  return puzzle[v->y][v->x];
}

vector move(const vector *v, const vector *i) {
  vector next = {i->x + v->x, i->y + v->y};
  return next;
}

static inline unsigned char withinBounds(const vector v) {
  return v.x >= 0 && v.x < DIM && v.y >= 0 && v.y < DIM;
}

unsigned char matchExists(const char (*puzzle)[DIM], const vector *initial,
                          const vector *v) {
  vector position = *initial;
  for (size_t k = 0; k < sizeof(mas); k++) {
    position = move(v, &position);
    if (!withinBounds(position) || mas[k] != at(puzzle, &position)) {
      return 0;
    }
  }
  return 1;
}

unsigned char singleCrossMatchExists(const char (*puzzle)[DIM],
                                     const vector *initial,
                                     const xvector patterns[5]) {
  for (size_t p = 0; p < 5; p++) {
    xvector pattern = patterns[p];
    vector position = move(initial, &pattern.vec);
    if (!withinBounds(position)) {
      return 0;
    }
    char piece = at(puzzle, &position);
    if (piece != pattern.letter) {
      return 0;
    }
  }
  return 1;
}

unsigned char crossMatchExists(const char (*puzzle)[DIM],
                               const vector *initial) {
  int found = 0;
  for (size_t rotation = 0; rotation < 4; rotation++) {
    found += singleCrossMatchExists(puzzle, initial, crossPattern[rotation]);
  }
  return found;
}

int completions(const char (*puzzle)[DIM], const vector *pos) {
  if (at(puzzle, pos) != 'X') {
    return 0;
  }
  int seen = 0;
  for (size_t o = 0; o < sizeof(offsets); o++) {
    const vector v = offsets[o];
    seen += matchExists(puzzle, pos, &v);
  }
  return seen;
}

int solve(const char (*puzzle)[DIM]) {
  int count = 0;
  for (size_t j = 0; j < DIM; j++) {
    for (size_t i = 0; i < DIM; i++) {
      const vector pos = {i, j};
      count += completions(puzzle, &pos);
    }
  }
  return count;
}

int solve_part2(const char (*puzzle)[DIM]) {
  int count = 0;
  for (size_t j = 0; j < DIM; j++) {
    for (size_t i = 0; i < DIM; i++) {
      vector position = {i, j};
      count += crossMatchExists(puzzle, &position);
    }
  }
  return count;
}

void read(FILE *f, char puzzle[DIM][DIM]) {
  for (size_t i = 0; i < DIM; i++) {
    fread(puzzle[i], sizeof(puzzle[0][0]), DIM, f);
    fseek(f, 1, SEEK_CUR);
  }
  fclose(f);
}

int main() {
  char puzzle[DIM][DIM] = {0};
  FILE *f = fopen("input.txt", "r");
  if (f == NULL) {
    perror("Couldn't open file");
    return 1;
  }
  read(f, puzzle);
  int count = solve(puzzle);
  int count_part2 = solve_part2(puzzle);

  return 0;
}
