
#include<stdio.h>
#include<stdlib.h>

#include "draw_utils.c"

#define BLOCK_WIDTH 1
#define BLOCK_CHAR "_"

#define BOARD_SIZE 23
int (*make_array())[BOARD_SIZE];
void init_array(int (*array_will_be_init)[BOARD_SIZE], int *init_value_array);
void print_array(int (*board)[BOARD_SIZE]);
void usleep(int utime);
void process(int (*array1)[BOARD_SIZE], int (*array2)[BOARD_SIZE]);

const int N_STEPS = 50;
const int SLEEP_TIME = 500000;
int main() {
//  int init_value_array[BOARD_SIZE] = {
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000111111111100000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//      0b00111100000000000000,
//      0b01000100000000000000,
//      0b00000100000000000000,
//      0b01001000000000000000,
//      0b00000000000000000000,
//      0b00000000000000000000,
//  };
  int init_value_array[BOARD_SIZE] = {
      0b00000000000000000000000,
      0b00000000000000110000000,
      0b00000000000111011000000,
      0b00000000000111110000000,
      0b00000000000011100000000,
      0b00000000000000000000000,
      0b00000000000000000000000,
      0b00000000000111111110000,
      0b00001000100000000010000,
      0b00001000100000000010000,
      0b00001000100000000100000,
      0b00000000000000000000000,
      0b00001000100000000100000,
      0b00001000100000000010000,
      0b00001000100000000010000,
      0b00000000000111111110000,
      0b00000000000000000000000,
      0b00000000000000000000000,
      0b00000000000011100000000,
      0b00000000000111110000000,
      0b00000000000111011000000,
      0b00000000000000110000000,
      0b00000000000000000000000,
  };
  int array1[BOARD_SIZE][BOARD_SIZE];
  init_array(array1, init_value_array);

  int array2[BOARD_SIZE][BOARD_SIZE];
  for (int i = 0; i < N_STEPS / 2; i++) {
    print_array(array1);
    process(array1, array2);
    usleep(SLEEP_TIME);
    print_array(array2);
    process(array2, array1);
    usleep(SLEEP_TIME);
  }

  return 0;
}

int get_with_boundary_check(int (*array)[BOARD_SIZE], int i, int j);
int get_n_neighbourhood(int (*array)[BOARD_SIZE], int i, int j);

void process(int (*array1)[BOARD_SIZE], int (*array2)[BOARD_SIZE]) {
  for (int i = 0; i < BOARD_SIZE; i++) {
    for (int j = 0; j < BOARD_SIZE; j++) {
      int count = get_n_neighbourhood(array1, i, j);
      if (count <= 1) {
        array2[i][j] = 0;
      } else if (count == 2) {
        array2[i][j] = array1[i][j];
      } else if (count == 3) {
        array2[i][j] = 1;
      } else {
        array2[i][j] = 0;
      }
    }
  } 
}


int get_n_neighbourhood(int (*array)[BOARD_SIZE], int i, int j) {
  int count = 0;
  count = count + get_with_boundary_check(array, i - 1, j - 1);
  count = count + get_with_boundary_check(array, i, j - 1);
  count = count + get_with_boundary_check(array, i + 1, j - 1);
  count = count + get_with_boundary_check(array, i - 1, j);
  count = count + get_with_boundary_check(array, i + 1, j);
  count = count + get_with_boundary_check(array, i - 1, j + 1);
  count = count + get_with_boundary_check(array, i, j + 1);
  count = count + get_with_boundary_check(array, i + 1, j + 1);
  return count;
}

int get_with_boundary_check(int (*array)[BOARD_SIZE], int i, int j) {
  if (i < 0 || i >= BOARD_SIZE || j < 0 || j >= BOARD_SIZE) {
    return 0;
  } else {
    return array[i][j];
  }
}

void print_array(int (*board)[BOARD_SIZE]){
  clear();
  reset_cursor();
  for (int i = 0; i < BOARD_SIZE; i++) {
    for (int k1 = 0; k1 < BLOCK_WIDTH; k1++) {
      for (int j = 0; j < BOARD_SIZE; j++) {
          printf("\e[4%dm", board[i][j] + 3);
          for (int k2 = 0; k2 < BLOCK_WIDTH; k2++) {
            printf(BLOCK_CHAR);
          }
          printf("\e[0m");
      }
      printf("\n");
    }
  }
}


void init_array(int (*array_will_be_init)[BOARD_SIZE], int *init_value_array) {
  for (int i = 0; i < BOARD_SIZE; i++) {
    for (int j = 0; j < BOARD_SIZE; j++) {
      array_will_be_init[i][j] = (init_value_array[i] >> (BOARD_SIZE - j - 1)) & 1;
    }
  }
}
