// Edited by uint256_t

// #include <stdio.h>
// #include <stdlib.h>
// #include <unistd.h>

int printf(char *msg);
int usleep(int time);

#define SIZE 20

int count_nbr(int (*grid)[SIZE], int i, int j, int size);

int main()
{
  int neighbour_count[SIZE][SIZE];
  int grid[SIZE][SIZE];
  for (int i = 0; i < SIZE; i = i + 1)
  {
    for (int j = 0; j < SIZE; j = j + 1)
    {
      grid[i][j] = 0;
    }
  }
  for (int j = 5; j <= 14; j = j + 1)
  {
    grid[7][j] = 1;
  }
  for (int j = 3; j <= 6; j = j + 1)
  {
    grid[8][j] = 1;
  }
  grid[15][1] = 1;
  grid[15][5] = 1;
  grid[16][5] = 1;
  grid[17][5] = 1;
  grid[18][1] = 1;
  grid[18][4] = 1;
  int i;
  int j;
  int steps;

  for (steps = 0; steps < 50; steps = steps + 1)
  {
    printf("\e[0;0H");
    for (i = 0; i < SIZE; i = i + 1)
    {
      printf("\n");
      for (j = 0; j < SIZE; j = j + 1)
      {
        if (grid[i][j] == 1)
          printf("\e[42m  \e[m");
        else
          printf("\e[47m  \e[m");
        neighbour_count[i][j] = count_nbr(grid, i, j, SIZE);
      }
    }

    for (i = 0; i < SIZE; i = i + 1)
    {
      for (j = 0; j < SIZE; j = j + 1)
      {
        if (grid[i][j] >= 1)
        {
          if (neighbour_count[i][j] <= 1)
          {
            grid[i][j] = 0;
          }
          else if (neighbour_count[i][j] >= 4)
            grid[i][j] = 0;
        }
        else if (neighbour_count[i][j] == 3)
          grid[i][j] = 1;
      }
    }

    usleep(100000);
  }

  return 0;
}

int count_nbr(int (*grid)[SIZE], int i, int j, int size)
{
  int n_count = 0;
  if (i - 1 >= 0)
  {
    if (j - 1 >= 0)
    {
      if (grid[i - 1][j - 1] >= 1)
        n_count = n_count + 1;
    }
  }

  if (i - 1 >= 0)
  {
    if (grid[i - 1][j] >= 1)
      n_count = n_count + 1;
  }

  if (i - 1 >= 0)
  {
    if (j + 1 < size)
    {
      if (grid[i - 1][j + 1] >= 1)
        n_count = n_count + 1;
    }
  }

  if (j - 1 >= 0)
  {
    if (grid[i][j - 1] >= 1)
      n_count = n_count + 1;
  }

  if (j + 1 < size)
  {
    if (grid[i][j + 1] >= 1)
      n_count = n_count + 1;
  }

  if (i + 1 < size)
  {
    if (j - 1 >= 0)
    {
      if (grid[i + 1][j - 1] >= 1)
        n_count = n_count + 1;
    }
  }

  if (i + 1 < size)
  {
    if (grid[i + 1][j] >= 1)
      n_count = n_count + 1;
  }

  if (i + 1 < size)
  {
    if (j + 1 < size)
    {
      if (grid[i + 1][j + 1] >= 1)
        n_count = n_count + 1;
    }
  }

  return n_count;
}