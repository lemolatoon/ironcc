#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "test_utils.h"
int test0(void);
int test0_2(void);
int test0_3(void);
int test1(void);
int test2(int arg0);
int test3(int arg0);
int test4(int arg0);
int test5(int arg0);
int test6(int arg0);
int test7(int arg0);
int test8(void);
int test9(void);
int test9_0(void);
int test9_1(void);
int test10(void);
int test11(void);
int test12(void);
int test13(void);
int test14(void);
int test15(void);
int test16(void);
int test17(void);
int test18(void);
int test19(void);
int test20(void);
int test21(void);
int test22(void);
int test23(void);
int test24(void);
int test25(void);
int test26(void);
int test27(void);
int test28(void);
int test29(void);
int test30(void);
int test31(void);
int test32(void);
int test33(void);
int test34(void);
int test35(void);
int test36(void);
int test37(void);
int test38(void);
int test39(void);
int test40(void);
int test41(void);
int test42(void);
int test43(void);
int test44(void);
int test45(void);
int test46(void);
int test47(void);
int test48(void);
int test49(void);
int test50(void);
int test51(void);
int test52(void);
int test53(void);
int test54(void);
int test55(void);
int test56(void);
int test57(void);
int test58(void);
int test59(void);
int test60(void);
int test61(void);
int test62(void);
int test63(void);
int test64(void);
int test65(void);
int test66(void);
int test67(void);
int test68(void);
int test69(void);
int test70(void);
int test71(void);
int test72(void);
int test73(void);
int test74(void);
int test75(void);
int test76(void);
int test77(void);
int test78(void);
int test79(void);
int test80(void);
int test81(void);
int test(void);
void my_assert(int index, int expected, int got);
// this is comment for the test of function of comment

int f(int arg0);
int g(int arg0);
int h(int arg0);

int sub(int arg0, int arg1);
int add(int arg0, int arg1);

int **alloc4_ptr(int *a, int *b, int *c, int *d);
int *alloc4(int a, int b, int c, int d);

void print_ok(void);

int main(void) {
  my_assert(0, 3, test0());
  my_assert(0, 2, test0_2());
  my_assert(0, 10, test0_3());
  my_assert(1, 9, test1());
  my_assert(2, 5, test2(5));
  my_assert(3, 5, test3(5));
  my_assert(4, 2, test4(3));
  my_assert(4, 3, test4(4));
  my_assert(5, 10, test5(5));
  my_assert(6, 2, test6(2));
  my_assert(7, 0, test7(1));
  my_assert(7, 0, test7(91));
  my_assert(7, 1, test7(109));
  my_assert(8, 3, test8());
  test9_0();
  test9_1();
  my_assert(9, 10, test9());
  my_assert(10, 3, test10());
  my_assert(11, 0, test11());
  my_assert(12, 0, test12());
  my_assert(13, 0, test13());
  my_assert(14, 0, test14());
  my_assert(15, 0, test15());
  my_assert(16, 0, test16());
  my_assert(17, 0, test17());
  my_assert(18, 0, test18());
  my_assert(19, 0, test19());
  my_assert(20, 0, test20());
  my_assert(21, 0, test21());
  my_assert(22, 0, test22());
  my_assert(23, 0, test23());
  my_assert(24, 0, test24());
  my_assert(25, 0, test25());
  my_assert(26, 0, test26());
  my_assert(27, 0, test27());
  my_assert(28, 0, test28());
  my_assert(29, 0, test29());
  my_assert(30, 0, test30());
  my_assert(31, 0, test31());
  my_assert(32, 0, test32());
  my_assert(33, 0, test33());
  my_assert(34, 0, test34());
  my_assert(35, 0, test35());
  my_assert(36, 0, test36());
  my_assert(37, 0, test37());
  my_assert(38, 0, test38());
  my_assert(39, 0, test39());
  my_assert(40, 0, test40());
  my_assert(41, 0, test41());
  my_assert(42, 0, test42());
  my_assert(43, 0, test43());
  my_assert(44, 0, test44());
  my_assert(45, 0, test45());
  my_assert(46, 0, test46());
  my_assert(47, 0, test47());
  my_assert(48, 0, test48());
  my_assert(49, 0, test49());
  my_assert(50, 0, test50());
  my_assert(51, 0, test51());
  my_assert(52, 0, test52());
  my_assert(53, 0, test53());
  my_assert(54, 0, test54());
  my_assert(55, 0, test55());
  my_assert(56, 0, test56());
  my_assert(57, 0, test57());
  my_assert(58, 58, test58());
  my_assert(59, 59, test59());
  my_assert(60, 60, test60());
  my_assert(61, 0, test61());
  my_assert(62, 0, test62());
  my_assert(63, 0, test63());
  my_assert(64, 0, test64());
  my_assert(65, 0, test65());
  my_assert(66, 0, test66());
  my_assert(67, 0, test67());
  my_assert(68, 0, test68());
  my_assert(69, 0, test69());
  my_assert(70, 70, test70());
  my_assert(71, 0, test71());
  my_assert(72, 0, test72());
  my_assert(73, 0, test73());
  my_assert(74, 0, test74());
  my_assert(75, 0, test75());
  my_assert(76, 0, test76());
  my_assert(77, 0, test77());
  my_assert(78, 0, test78());
  my_assert(79, 1, test79());
  my_assert(80, 0, test80());
  my_assert(81, 0, test81());

  print_ok();
  return 0;
}

int assertion_failed(int index, int expected, int got) {
  printf("Assertion_failed At test%d\n", index);
  printf("Expected %d, but got %d\n", expected, got);
  exit(1);
}

void passed(int index) { printf("Test %d passed\n", index); }

void print_ok(void) { printf("\e[32mALL TESTS PASSED\e[0m\n"); }

void my_assert(int index, int expected, int got) {
  if (expected != got) {
    assertion_failed(index, expected, got);
  } else {
    passed(index);
  }
}

void fail(char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

int test0(void) { return 3; }

int test0_2(void) { return g(2); }

int test0_3(void) { return h(5); }

int g(int n) { return n; }

int h(int n) {
  if (n > 5) {
    return 5;
  } else {
    return 10;
  }
}

int test1(void) { return f(5); }

int f(int n) {
  int i;
  i = 3;
  if (n > 4) {
    return f(n - 1) + 2;
  }

  return i + n;
}

int test2(int i) {
  if (i == 0) {
    return 0;
  }
  if (i == 1) {
    return 1;
  }

  return i;
}

int test3(int i) {
  if (i == 0) {
    return 0;
  }
  if (i == 1) {
    return 1;
  }

  return test3(i - 1) + 1;
}

int test4(int i) {
  if (i == 0) {
    return 0;
  }
  if (i == 1) {
    return 1;
  }

  return test4(i - 1) + test4(i - 2);
}

int test5(int i) { return add(i, 1000000) + sub(i, 1000000); }

int add(int x, int y) { return x + y; }

int sub(int x, int y) { return x - y; }

int test6(int i) {
  if (i == 0) {
    return 1;
  }
  return i * test6(i - 1);
}

int test7(int n) {
  if (n == 1) {
    return 0;
  }

  int result;
  result = 1;
  int i;
  for (i = 2; i < n; i = i + 1) {
    if (n % i == 0) {
      result = 0;
    }
  }
  return result;
}

int test8(void) {
  int x;
  x = 3;
  int *y;
  y = &x;
  return *y;
}

int test9_0(void) {
  int y;
  y = 10;
  int *py;
  py = &y;
  int x;
  int *px;
  px = &x;
  int **z;
  z = &px;
  *z = py;
  my_assert(90, 10, **z);
  return 0;
}

int test9_1(void) {
  int y;
  y = 10;
  int *py;
  py = &y;
  int x;
  x = 5;
  int *px;
  px = &x;
  int **z;
  z = &px;
  my_assert(91, 5, **z);
  *z = py;
  my_assert(91, 10, **z);
  return 0;
}

int test9(void) {
  int x;
  x = 5;
  my_assert(9, 5, x);
  int y;
  y = 10;
  my_assert(9, 10, y);
  int *px;
  int *py;
  px = &x;
  py = &y;
  my_assert(9, 5, *px);
  my_assert(9, 10, *py);
  int **z;
  z = &px;
  my_assert(9, 5, **z);
  if (**z == 5) {
    *z = py;
    my_assert(9, 10, **z);
  }
  return **z;
}

int test10(void) {
  int x;
  int *y;
  y = &x;
  *y = 3;
  return x;
}

int test11(void) {
  int *p;
  p = alloc4(2, 3, 5, 8);
  int *q;
  q = p + 2;
  my_assert(11, 5, *q);
  q = p + 3;
  my_assert(11, 8, *q);

  int *p2;
  p2 = alloc4(1, 2, 3, 4);
  my_assert(11, 4, *(3 + p2));
  return 0;
}

int test12(void) {
  int a;
  int b;
  int c;
  int d;
  a = 1;
  b = 2;
  c = 3;
  d = 4;
  int **p;
  p = alloc4_ptr(&a, &b, &c, &d);
  my_assert(12, 1, **p);
  p = p + 2;
  my_assert(12, 3, **p);
  p = p - 2;
  int *d2;
  d2 = *(p + 3);
  my_assert(12, 4, *d2);
  my_assert(12, 4, **(p + 3));
  **(p + 3) = 9;
  my_assert(12, 9, **(p + 3));
  int *q;
  int e;
  e = 1;
  q = &e;
  *q = **p + **(p + 1) + **(p + 2) - **(p + 3);
  my_assert(12, -3, *q);
  return 0;
}

int test13(void) {
  my_assert(13, 4, sizeof(int));
  my_assert(13, 8, sizeof(int *));
  my_assert(13, 8, sizeof(int ***));
  int a;
  my_assert(13, 4, sizeof(a));
  my_assert(13, 4, sizeof a);
  my_assert(13, 8, sizeof &a);
  return 0;
}

int test14(void) {
  int a = 5;
  my_assert(14, 5, a);
  return 0;
}

int test15(void) {
  int *first_ptr = alloc4(1, 2, 3, 4);
  int *last_ptr = first_ptr + 3;
  my_assert(15, 4, *last_ptr);
  my_assert(15, 3, last_ptr - first_ptr);
  return 0;
}

int test16(void) {
  int array[5];
  *array = 1;
  my_assert(16, 1, *array);
  my_assert(16, 1, array[0]);
  return 0;
}

int test17(void) {
  int a[2];
  a[0] = 3;
  a[1] = 5;
  my_assert(16, 3, *a);
  my_assert(16, 3, a[0]);
  my_assert(16, 3, 0 [a]);
  my_assert(16, 5, 1 [a]);
  my_assert(16, 5, *(a + 1));
  return 0;
}

int init_2d_mat(int (*mat)[2], int a, int b, int c, int d) {
  mat[0][0] = a;
  mat[0][1] = b;
  mat[1][0] = c;
  mat[1][1] = d;
  return 0;
}

int (*mat_mul_2d(int (*new_mat)[2], int (*lhs)[2], int (*rhs)[2]))[2] {
  for (int i = 0; i < 2; i = i + 1) {
    for (int j = 0; j < 2; j = j + 1) {
      new_mat[i][j] = 0;
      for (int k = 0; k < 2; k = k + 1) {
        new_mat[i][j] = new_mat[i][j] + lhs[i][k] * rhs[k][j];
      }
    }
  }
  return new_mat;
}

int test18(void) {
  my_assert(18, 0, 0);
  int mat1[2][2];
  init_2d_mat(mat1, 1, 2, 1, 3);
  my_assert(18, 2, mat1[0][1]);
  int mat2[2][2];
  init_2d_mat(mat2, 3, -2, -1, 1);
  my_assert(18, 1, mat2[1][1]);
  int multiplied[2][2];
  mat_mul_2d(multiplied, mat1, mat2);
  my_assert(18, 1, multiplied[0][0]);
  my_assert(18, 0, multiplied[0][1]);
  my_assert(18, 0, multiplied[1][0]);
  my_assert(18, 1, multiplied[1][1]);

  return 0;
}

int test19(void) {
  int dummy;
  int array1[3];
  my_assert(19, 4 * 3, sizeof(array1));
  int *array2[3];
  my_assert(19, 8 * 3, sizeof(array2));
  int(*array3)[3];
  my_assert(19, 8, sizeof(array3));
  int **array4[3][5][7];
  my_assert(19, 8 * 3 * 5 * 7, sizeof(array4));
  return 0;
}

int test20_g_var;
int test20_index;

int inc_test20_index(void);

int test20(void) {
  test20_g_var = 9;
  my_assert(20, 9, test20_g_var);
  test20_index = 0;
  inc_test20_index();
  my_assert(20, 20, test20_index);
  return 0;
}

int inc_test20_index(void) {
  for (int i = 0; i < 20; i = i + 1) {
    test20_index = test20_index + 1;
  }
  return 0;
}

int test21_global_var = 21;
int test21(void) {
  my_assert(21, 21, test21_global_var);
  return 0;
}

int test22_global_with_init[3] = {1, 2, 3};
int test22(void) {
  my_assert(22, 1, test22_global_with_init[0]);
  my_assert(22, 2, test22_global_with_init[1]);
  my_assert(22, 3, test22_global_with_init[2]);
  test22_global_with_init[0] = 9;
  my_assert(22, 9, *test22_global_with_init);
  return 0;
}

int test23_global = 3;
int test23(void) {
  my_assert(23, 3, test23_global);
  int *p = &test23_global;
  *p = 4;
  my_assert(23, 4, test23_global);
  return 0;
}

char test24_char_global = 9;
int test24_int_global = -1;
int test24(void) {
  my_assert(24, 9, test24_char_global);
  test24_char_global = 98;
  my_assert(24, 98, test24_char_global);
  my_assert(24, -1, test24_int_global);
  test24_char_global = test24_int_global;
  my_assert(24, -1, test24_char_global);
  test24_int_global = 999;
  my_assert(24, 999, test24_int_global);
  test24_int_global = test24_char_global;
  my_assert(24, -1, test24_int_global);
  return 0;
}

int test25(void) {
  int int_var = -1;
  char char_var;
  char_var = int_var;
  my_assert(25, -1, char_var);
  char char_var2 = int_var;
  my_assert(25, -1, char_var2);
  int int_var2 = char_var2;
  my_assert(25, -1, int_var2);
  return 0;
}

int test26(void) {
  int array[1 * 0 + 8 * 2];
  my_assert(26, 16, sizeof(array) / sizeof(array[0]));
  return 0;
}

int test27(void) {
  int array_with_init[1 + 2 + 1] = {1, 2, 3};
  my_assert(27, 1, array_with_init[0]);
  my_assert(27, 2, array_with_init[1]);
  my_assert(27, 3, array_with_init[2]);
  array_with_init[3] = 4;
  my_assert(27, 4, array_with_init[3]);
  return 0;
}

int test28_var = 3;
int *test28_var_ptr = &test28_var;
int test28(void) {
  my_assert(28, 3, test28_var);
  *test28_var_ptr = 89;
  my_assert(28, 89, test28_var);
  return 0;
}

char add_chars(char a, char b, char c);

int test29(void) {
  my_assert(29, 12, add_chars(9, 6, 3));
  return 0;
}

int ret1(int this) { return this; }

int ret2(int dummy1, int this) { return this; }

int ret3(int dummy1, int dummy2, int this) { return this; }

int test30(void) {
  char char_var = -1;
  int int_var = 4;
  my_assert(30, 3, ret1(int_var + char_var));
  my_assert(30, 3, ret2(0, int_var + char_var));
  my_assert(30, 3, ret3(0, 0, int_var + char_var));
  my_assert(30, -1, char_var);
  my_assert(30, 4, int_var);
  my_assert(30, 3, int_var + char_var);
  my_assert(30, 3, char_var + int_var);
  return 0;
}

int test31(void) {
  // char str_lit[] = "123456789";
  my_assert(31, 10, sizeof("123456789") / sizeof(char));
  // assert(30, 10, sizeof(str_lit) / sizeof(str_lit[0]));
  return 0;
}

int test32(void) {
  int a = 4;
  my_assert(32, -5, ~a);
  my_assert(32, -10, ~9);
  my_assert(32, 2, ~(-3));
  char c = 3;
  my_assert(32, -4, ~c);
  char *hello_world = "Hello World";
  my_assert(32, 108, hello_world[3]);
  return 0;
}

int test33(void) {
  my_assert(33, 4, 1 << 2);
  my_assert(33, 1, 4 >> 2);
  return 0;
}

int test34(void) {
  my_assert(34, 0b101, 0b101 & 0b111);
  my_assert(34, 0b10100, 0b10110 & 0b11100);
  my_assert(34, 2, 2 & 3);
  my_assert(34, 1026, 1098 & 1335);
  return 0;
}

int test35(void) {
  struct A {
    char a;
    int b;
    char c;
  } struct_a;
  struct A struct_a2;
  my_assert(35, 12, sizeof(struct_a));
  my_assert(35, 12, sizeof(struct_a2));
  return 0;
}

int test36(void) {
  int arr[2] = {1, 2};
  struct A {
    char a;
    int b;
    char c;
  } struct_a;
  int arr2[2] = {1, 2};
  struct_a.a = 3;
  my_assert(36, 3, struct_a.a);
  struct_a.c = struct_a.a;
  my_assert(36, 3, struct_a.c);
  my_assert(36, 2, arr[1]);
  my_assert(36, 1, arr2[0]);
  return 0;
}

int test37(void) {
  struct A {
    int a[2];
    char b;
    int c;
  };

  int arr[2] = {1, 2};
  struct A a;
  int arr2[2] = {1, 2};
  a.b = 2;
  my_assert(37, 2, a.b);
  a.a[1] = 22;
  my_assert(37, 22, a.a[1]);
  a.c = 9;
  my_assert(37, 9, a.c);
  a.a[1] = 22;
  my_assert(37, 9, a.c);
  my_assert(37, 1, arr[0]);
  my_assert(37, 2, arr[1]);
  my_assert(37, 1, arr2[0]);
  my_assert(37, 2, arr2[1]);
  return 0;
}

char add_chars(char a, char b, char c) { return a + b - c; }

void test38_void_func(int *n) { *n = *n + 1; }

char test38_take_void(void *char_array) {
  char *char_ptr = char_array;
  return *char_ptr;
}

int test38(void) {
  int a = 2;
  test38_void_func(&a);
  my_assert(38, 3, a);
  char char_array[2] = {22, 33};
  my_assert(38, 22, test38_take_void(char_array));
  return 0;
}

int test39(void) {
  char *char_array = "0123456789";
  my_assert(39, 48, *char_array);
  my_assert(39, 49, char_array[1]);
  my_assert(39, 50, char_array[2]);
  my_assert(39, 51, char_array[3]);
  my_assert(39, 52, char_array[4]);
  my_assert(39, 53, char_array[5]);
  my_assert(39, 54, char_array[6]);
  my_assert(39, 48, "0123456789"[0]);
  int n = 9;
  my_assert(39, 48 + 9, "0123456789"[9]);
  my_assert(39, 48 + 9, "0123456789"[n]);
  return 0;
}

int test40(void) {
  // int n = -1600;
  int n = -200;
  my_assert(40, -200, n);
  my_assert(40, -199, n + 1);
  my_assert(40, -400, n * 2);
  my_assert(40, -100, n / 2);
  int calced = -1600 / 10000;
  int calced2 = n / 10000;
  my_assert(40, 0, -1600 / 10000);
  my_assert(40, 0, -1 >= 1);
  int N = -1;
  my_assert(40, 0, N >= 1);
  return 0;
}

int test41(void) {
  struct List {
    int a;
    struct List *next;
  };
  struct List head;
  head.next = malloc(sizeof(head));
  struct List *watching = head.next;
  (*head.next).a = 0;
  for (int i = 1; i < 10; i = i + 1) {
    (*watching).next = malloc(sizeof(head));
    (*(*watching).next).a = i;
    watching = (*watching).next;
  }
  int i = 0;
  for (watching = head.next; watching; watching = (*watching).next) {
    my_assert(41, i, (*watching).a);
    i = i + 1;
  }
  watching = head.next;
  for (int i2 = 0; i2 < 10; i2 = i2 + 1) {
    my_assert(41, i2, (*watching).a);
    watching = (*watching).next;
  }
  return 0;
}

int test42(void) {
  struct A {
    struct A *before;
    struct A *after;
    int value;
  };
  // TODO: support struct type name
  my_assert(42, 4, sizeof(int));
  my_assert(42, 8, sizeof(int *));
  my_assert(42, 24, sizeof(struct A));
  return 0;
}

// struct ListNode {
// 	void *data;
//     int sz;

// 	struct ListNode *next;
// } ;

// struct List {
// 	struct ListNode *head;
// 	struct ListNode *tail;
// };

// struct ListNode *AddNode(struct List * list, void * data, int sz);
// void DeleteNode(struct List * list, struct ListNode * node);
// struct ListNode *FindNodeByRef(struct List * list, void * data);
// struct ListNode *FindNodeByValue(struct List * list, void * data, int sz);
// void FreeNodes(struct List * list, int free_parameter_as_well);
// void FreeList(struct List * list);

int test43(void) {
  struct Doubly {
    struct Doubly *before;
    struct Doubly *after;
    int value;
  };
  struct Doubly head;
  struct Doubly tail;
  struct Doubly *node = malloc(sizeof(struct Doubly));
  node->value = 0;
  node->before = &head;
  head.after = node;
  for (int i = 1; i < 10; i = i + 1) {
    node->after = malloc(sizeof(struct Doubly));
    node->after->value = i;
    struct Doubly *next = node->after;
    next->before = node;
    node = next;
  }
  node->after = &tail;
  tail.before = node;

  node = head.after;
  for (int i = 0; i < 10; i = i + 1) {
    my_assert(43, i, node->value);
    node = node->after;
  }
  // assert(43, 1, node == &tail);
  node = tail.before;
  for (int i = 9; i >= 0; i = i - 1) {
    my_assert(43, i, node->value);
    node = node->before;
  }
  // assert(43, 1, node == &head);
  return 0;
}

int test44_global_var;
int inc_44_global_var_and_return_true(void) {
  test44_global_var = test44_global_var + 1;
  return 1;
}

int inc_44_global_var_and_return_false(void) {
  test44_global_var = test44_global_var + 1;
  return 0;
}

int test44(void) {
  my_assert(44, 0, test44_global_var);
  my_assert(44, 1,
            inc_44_global_var_and_return_true()
                ? inc_44_global_var_and_return_true()
                : inc_44_global_var_and_return_false());
  my_assert(44, 2, test44_global_var);
  my_assert(44, 0,
            inc_44_global_var_and_return_false()
                ? inc_44_global_var_and_return_true()
                : 0);
  my_assert(44, 3, test44_global_var);
  my_assert(44, 0,
            inc_44_global_var_and_return_false()
                ? 1
                : inc_44_global_var_and_return_false());
  my_assert(44, 5, test44_global_var);
  return 0;
}

int test45_global_var;
int inc_45_global_var_and_return_true(void) {
  test45_global_var = test45_global_var + 1;
  return 1;
}

int inc_45_global_var_and_return_false(void) {
  test45_global_var = test45_global_var + 1;
  return 0;
}

int test45(void) {
  my_assert(45, 1,
            inc_45_global_var_and_return_true() &&
                inc_45_global_var_and_return_true());
  my_assert(45, 2, test45_global_var);
  my_assert(45, 1,
            inc_45_global_var_and_return_true() ||
                inc_45_global_var_and_return_true());
  my_assert(45, 3, test45_global_var);
  my_assert(45, 1,
            inc_45_global_var_and_return_false() ||
                inc_45_global_var_and_return_true());
  my_assert(45, 5, test45_global_var);
  my_assert(45, 0,
            inc_45_global_var_and_return_false() &&
                inc_45_global_var_and_return_true());
  my_assert(45, 6, test45_global_var);
  my_assert(45, 0,
            inc_45_global_var_and_return_true() &&
                inc_45_global_var_and_return_false());
  my_assert(45, 8, test45_global_var);
  return 0;
}

int test46(void) {
  int a = 1;
  my_assert(46, 1, a++);
  my_assert(46, 2, a++);
  my_assert(46, 3, a--);
  my_assert(46, 2, a--);
  int *p = &a;
  my_assert(46, 1, (*p)++);
  my_assert(46, 2, *p);
  int arr[4] = {1, 2, 3, 4};
  int *p2 = arr;
  my_assert(46, 1, *p2++);
  my_assert(46, 2, *p2++);
  my_assert(46, 3, *p2++);
  my_assert(46, 4, *p2--);
  my_assert(46, 3, *p2--);
  my_assert(46, 2, *p2--);
  my_assert(46, 1, *p2);
  return 0;
}

int test47_global_var = 1;
// called only once
int *ret_addr_of_test47_global_bar(void) {
  my_assert(47, 1, test47_global_var);
  return &test47_global_var;
}
int test47(void) {
  my_assert(47, 1, (*(ret_addr_of_test47_global_bar()))++);
  my_assert(47, 2, test47_global_var);
  return 0;
}

int test48(void) {
  int a = 1;
  my_assert(48, 2, ++a);
  my_assert(48, 3, ++a);
  my_assert(48, 2, --a);
  my_assert(48, 1, --a);
  int *p = &a;
  my_assert(48, 2, ++(*p));
  my_assert(48, 2, *p);
  int arr[4] = {1, 2, 3, 4};
  int *p2 = arr;
  my_assert(48, 2, ++(*p2));
  my_assert(48, 3, ++(*p2));
  my_assert(48, 4, ++(*p2));
  my_assert(48, 3, --(*p2));
  my_assert(48, 2, --(*p2));
  my_assert(48, 1, --(*p2));
  my_assert(48, 1, *p2);
  return 0;
}

int test49_global_var = 1;
// called only once
int *ret_addr_of_test49_global_bar(void) {
  my_assert(49, 1, test49_global_var);
  return &test49_global_var;
}
int test49(void) {
  my_assert(49, 2, ++(*(ret_addr_of_test49_global_bar())));
  my_assert(49, 2, test49_global_var);
  return 0;
}

#define CONST_FOR_TEST50 -5111

int test50(void) {
  my_assert(50, -5111, CONST_FOR_TEST50);
  return 0;
}

#define TEST51_RENAMED_FUNC test51_renaming_func

int test51_renaming_func(void) { return 234; }

int test51(void) {
  my_assert(51, 234, TEST51_RENAMED_FUNC());
  return 0;
}

#define TEST52_DEFINED_CONST

#ifdef TEST52_DEFINED_CONST
int test52_just_ret(void) { return 2; }
#else
int test52_just_ret(void) { return 3; }
this is てきとう words
#endif

#undef TEST52_DEFINED_CONST
#ifdef TEST52_DEFINED_CONST
int test52_just_ret2(void) { return 2; }
#else
    int
    test52_just_ret2(void) {
  return 3;
}
#endif

int test52(void) {
  my_assert(52, 2, test52_just_ret());
  my_assert(52, 3, test52_just_ret2());
  return 0;
}

enum test53_enum {
  TEST53_ENUM_A,
  TEST53_ENUM_B,
  TEST53_ENUM_C,
};
int test53(void) {
  enum A { A1, A2, A3 };
  my_assert(53, A1, 0);
  my_assert(53, A3, 2);
  my_assert(53, 0, TEST53_ENUM_A);
  my_assert(53, 1, TEST53_ENUM_B);
  my_assert(53, 2, TEST53_ENUM_C);
  return 0;
}

int test54(void) {
  int two = 2;
  char fifteen = 11;
  int arr[5][3] = {
      {1, two, 3}, {4, 5, 6}, {7, 8, 9}, {10, fifteen, 12}, {13, 14, 15},
  };
  my_assert(54, 4 * 5 * 3, sizeof(arr));
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 3; j++) {
      my_assert(54, i * 3 + j + 1, arr[i][j]);
    }
  }
  return 0;
}

int test55_global_array[5][2] = {
    {1, 2}, {3, 4}, {5}, {7, 8}, {9, 10},
};

int test55(void) {
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 2; j++) {
      if (i == 2 && j == 1) {
        my_assert(55, 0, test55_global_array[i][j]);
      } else {
        my_assert(55, i * 2 + j + 1, test55_global_array[i][j]);
      }
    }
  }
  return 0;
}

char *test56_strings[4] = {
    "red",
    "green",
    "blue",
    "really_long_string_more_than_eight",
};
int test56(void) {
  my_assert(56, 0,
            strncmp(test56_strings[0], "red", sizeof("red") / sizeof(char)));
  my_assert(
      56, 0,
      strncmp(test56_strings[1], "green", sizeof("green") / sizeof(char)));
  my_assert(56, 0,
            strncmp(test56_strings[2], "blue", sizeof("blue") / sizeof(char)));
  my_assert(
      56, 0,
      strncmp(test56_strings[3], "really_long_string_more_than_eight",
              sizeof("really_long_string_more_than_eight") / sizeof(char)));
  char *local_strings[4] = {
      "local_red",
      "local_green",
      "local_blue",
      "local_really_long_string_more_than_eight",
  };
  my_assert(56, 0,
            strncmp(local_strings[0], "local_red",
                    sizeof("local_red") / sizeof(char)));
  my_assert(56, 0,
            strncmp(local_strings[1], "local_green",
                    sizeof("local_green") / sizeof(char)));
  my_assert(56, 0,
            strncmp(local_strings[2], "local_blue",
                    sizeof("local_blue") / sizeof(char)));
  my_assert(
      56, 0,
      strncmp(
          local_strings[3], "local_really_long_string_more_than_eight",
          sizeof("local_really_long_string_more_than_eight") / sizeof(char)));

  return 0;
}

int test57(void) {
  char *string =
      "this"
      "is"
      "a"
      "string.";
  my_assert(57, 0,
            strncmp(string, "thisisastring.",
                    sizeof("thisisastring.") / sizeof(char)));
  return 0;
}

__asm__(
    ".intel_syntax noprefix\n"
    "test58:\n"
    "  mov rax, 58\n"
    "  ret\n");

int test59(void) {
  __asm__(
      ".intel_syntax noprefix\n"
      "  mov rax, 59\n"
      "  mov rsp, rbp\n"
      "  pop rbp\n"
      "  ret\n");
#ifdef __STDC__
  // to remove annoying warning
  return 1;
#endif
}

int test60(void) {
#ifndef __STDC__
  int a = __asm__("push 60\n");
#else
  int a = 60;
#endif

  return a;
}

typedef int test61_typedefed;
int test61(void) {
  my_assert(61, 4, sizeof(test61_typedefed));
  typedef int test61_typedefed[3];
  my_assert(61, 12, sizeof(test61_typedefed));
  {
    typedef char *test61_typedefed;
    my_assert(61, 8, sizeof(test61_typedefed));
  }
  my_assert(61, 12, sizeof(test61_typedefed));
  return 0;
}

int test62(void) {
  typedef struct {
    int a;
    int b;
  } A;
  A a;
  a.a = 1;
  a.b = 2;
  typedef struct B B;
  struct B {
    A *a;
  };
  B struct_b;
  struct_b.a = &a;
  my_assert(62, 1, a.a);
  my_assert(62, 2, a.b);
  my_assert(62, 1, struct_b.a->a);
  my_assert(62, 2, struct_b.a->b);
  return 0;
}

int test63(void) {
  typedef enum {
    A,
    B,
    C,
  } CertainKind;
  my_assert(63, 0, A);
  my_assert(63, 1, B);
  my_assert(63, 2, C);

  typedef enum enum_tag CertainKind2;
  enum enum_tag {
    D,
    E,
    F,
  };
  my_assert(63, 0, D);
  my_assert(63, 1, E);
  my_assert(63, 2, F);
  return 0;

  my_assert(63, 4, sizeof(CertainKind));
  my_assert(63, 4, sizeof(CertainKind2));
}

int test64(void) {
  my_assert(64, 0, false);
  my_assert(64, 1, true);
  typedef enum {
    A0,
    A1,
    A2,
    A3,
  } EnumConstants;
  for (int i = 0; i < 6; ++i) {
    switch (i) {
      case A0:
        my_assert(64, A0, i);
      case A1:
        my_assert(64, true, i == A0 || i == A1);
        break;
      case A2:
        my_assert(64, A2, i);
        break;
      case A3: {
        my_assert(64, A3, i);
        break;
      }
      default:
        my_assert(64, true, i > A3);
        break;
    }
  }
  return 0;
}

void exit_as_error_with_msg(char *msg) {
  printf("\e[31m%s\e[0m\n", msg);
  exit(1);
}

int test65(void) {
  int sum = 0;
  for (int i = 0;; ++i) {
    sum = sum + i;
    if (i == 10) {
      break;
    }
  }
  my_assert(65, 55, sum);
  sum = 0;
  int i = 0;
  while (true) {
    sum = sum + i;
    if (i == 10) {
      break;
    } else {
      i++;
      continue;
    }
    exit_as_error_with_msg("TEST65: should not reach here");
  }
  my_assert(65, 55, sum);
  return 0;
}

int test66(void) {
  char *string1 = "abcd";
  char string2[5];
  string2[0] = 'a';
  string2[1] = 'b';
  string2[2] = 'c';
  string2[3] = 'd';
  string2[4] = '\0';
  my_assert(66, 0, strncmp(string1, string2, 5));
  return 0;
}

int test67(void) {
  my_assert(67, 67, test67_external_global_var);
  return 0;
}

int test68(void) {
  int a = 1;
  a += 3;
  my_assert(68, 4, a);
  int arr[3] = {1, 2, 3};
  int *ptr = arr;
  my_assert(68, 1, *ptr);
  ptr += 2;
  my_assert(68, 3, *ptr);
  return 0;
}

void test69_set_ptr(int *p) {
  *p = 1;
  return;
  *p = 2;
}

int test69(void) {
  int a;
  int *p = &a;
  test69_set_ptr(p);
  my_assert(69, 1, a);
  return 0;
}

int test70(void) {
  switch (1) {
    case 1:
      switch (2) {
        case 1:
          exit_as_error_with_msg("TEST70: should not reach here1");
        case 2:
          return 70;
        default:
          exit_as_error_with_msg("TEST70: should not reach here2");
      }
      break;
    case 2:
      break;
    default:
      break;
  }
  exit_as_error_with_msg("TEST70: should not reach here3");
}

int test71(void) {
  int tmp_int = 1001;
  typedef struct {
    int a;
    char b;
    int *p;
  } A;
  A a1;
  a1.a = 1;
  a1.b = 2;
  a1.p = &tmp_int;
  A a2;
  a2 = a1;
  my_assert(71, 1, a2.a);
  my_assert(71, 2, a2.b);
  my_assert(71, 1001, *a1.p);
  return 0;
}

int just_ret_double(int n) { return n * 2; }

int just_ret_triple(int n) { return n * 3; }

int test72(void) {
  int (*f)(int arg1) = just_ret_double;
  my_assert(72, 10, f(5));
  my_assert(72, 4, f(2));
  f = just_ret_triple;
  my_assert(72, 15, f(5));
  my_assert(72, 6, f(2));
  return 0;
}

int test73_test_func(int a, int b, int c) { return a * b + c; }
int test73(void) {
  int a = (3 + 3, 5);
  int t;
  int c = (1, (3, 4));
  my_assert(73, a, 5);
  my_assert(73, 3, (1, 2, 3, (4, 5), 3));
  my_assert(73, 29, test73_test_func(a, (t = 3, t + 2), c));
  return 0;
}

typedef struct test74_tag Test74;

struct test74_tag {
  int value;
};

struct test74_tag *test74_f(int n) {
  Test74 *ptr = malloc(sizeof(Test74));
  ptr->value = n;
  return ptr;
}

int test74(void) {
  Test74 *ptr = test74_f(9);
  my_assert(74, 9, ptr->value);
  return 0;
}

#define TEST75_ARRAY_LEN 11
int test75(void) {
  int array[5][TEST75_ARRAY_LEN];
  my_assert(75, /*  4 * 5 * 11  */ 220, sizeof(array));
  return 0;
}

int test76(void) {
  struct fibArray {
    int idx;
    int fib;
  } numerical_array[10];
  numerical_array[0].idx = 0;
  numerical_array[0].fib = 1;
  numerical_array[1].idx = 1;
  numerical_array[1].fib = 1;
  for (int i = 2; i < 10; i++) {
    numerical_array[i].idx = i;
    numerical_array[i].fib =
        numerical_array[i - 1].fib + numerical_array[i - 2].fib;
  }
  int prev = 1;
  int current = 1;
  for (int i = 2; i < 10; i++) {
    int new = prev + current;
    prev = current;
    current = new;
    my_assert(76, new, numerical_array[i].fib);
  }
  return 0;
}

int test77(void) {
  char *array[5] = {
      "1", "12", "123", "1234", "12345",
  };
  for (int i = 0; i < 5; i++) {
    my_assert(77, i + 1, strlen(array[i]));
  }
  return 0;
}

int test78_increment_function(int n) {
  static int count = 1;  // should be initialized once
  count += n;
  return count;
}

int test78(void) {
  my_assert(78, 1, test78_increment_function(0));
  my_assert(78, 4, test78_increment_function(3));
  my_assert(78, -96, test78_increment_function(-100));
  return 0;
}

int test79(void) {
  // ifdef watching
  int ret;
#define A
  // 0 0

#ifdef A
  // 1 0
  // 1 1

#ifdef A
  // 2 1
  // 2 2

#else
  // 2 2

#endif
  // 1 2
  // 1 1

  ret = 1;
#else
  // 1 1

#ifdef A
  // 2 1

#ifdef A
  // 3 1

#else
  // 3 1

#endif
  // 2 1

#else
  // 2 1

#endif
  // 1 1
  ret = 2;
#endif
#undef A
#ifdef A
  ret = 999;
#endif
  return ret;
}
int test80(void) {
  // clang-format off
#define A \
80
  // clang-format on
#ifndef A
  fail("unreachable");
#endif
  my_assert(80, 80, A);
#undef A
  // clang-format off
#define \
A \
2
#ifndef \
A
  fail("unreachable");
#endif
#ifdef \
A
  my_assert(80, 2, A);
#else
  fail("unreachable");
#endif
#undef \
A
  // clang-format on
  return 0;
}

int test81(void) {
#define TEST81_SUM(A, B, C) A + B + C
#define ARGS3 1 + 1, 2, 3
  my_assert(81, 6, TEST81_SUM(1, 2, 3));
  char num[4];
  snprintf(num, sizeof(num), "%d%d%d", ARGS3);
  my_assert(81, 0, strncmp("223", num, sizeof(num)));
  // clang-format off
#define TEST81_MAX(A, B) \
  ((A) > (B) ? (A) : (B))
  // clang-format on
  my_assert(81, 32, TEST81_MAX(32, -5));
  return 0;
}