void printf(char *msg, ...);
void *malloc(int size);
void exit(int status);

void clear() {
  printf("\e[2J");
}

void reset_cursor() {
  printf("\e[0;0H");
  printf("\e[0;0f");
}

const int block_width = 1;

const int board_size = 23;
int (*make_array())[board_size];
void init_array(int (*array_will_be_init)[board_size], int *init_value_array);
void print_array(int (*board)[board_size]);
void usleep(int utime);
void process(int (*array1)[board_size], int (*array2)[board_size]);

const int N_STEPS = 50;
const int SLEEP_TIME = 500000;
int main() {
  // clear();
  // reset_cursor();
//  int init_value_array[board_size] = {
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
  int init_value_array[board_size] = {
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
  int array1[board_size][board_size];
  init_array(array1, init_value_array);

  int array2[board_size][board_size];
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

int get_with_boundary_check(int (*array)[board_size], int i, int j);
int get_n_neighbourhood(int (*array)[board_size], int i, int j);

void process(int (*array1)[board_size], int (*array2)[board_size]) {
  for (int i = 0; i < board_size; i++) {
    for (int j = 0; j < board_size; j++) {
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


int get_n_neighbourhood(int (*array)[board_size], int i, int j) {
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

int get_with_boundary_check(int (*array)[board_size], int i, int j) {
  if (i < 0 || i >= board_size || j < 0 || j >= board_size) {
    return 0;
  } else {
    return array[i][j];
  }
}

void print_array(int (*board)[board_size]){
  clear();
  reset_cursor();
  for (int i = 0; i < board_size; i++) {
    for (int k1 = 0; k1 < block_width; k1++) {
      for (int j = 0; j < board_size; j++) {
          printf("\e[4%dm", board[i][j] + 3);
          for (int k2 = 0; k2 < block_width; k2++) {
            printf("#");
          }
          printf("\e[0m");
      }
      printf("\n");
    }
  }
}


void init_array(int (*array_will_be_init)[board_size], int *init_value_array) {
  for (int i = 0; i < board_size; i++) {
    for (int j = 0; j < board_size; j++) {
      array_will_be_init[i][j] = (init_value_array[i] >> (board_size - j - 1)) & 1;
    }
  }
}
