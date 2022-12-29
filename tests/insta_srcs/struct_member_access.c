struct A {
  int a[2];
  char b;
  int c;
};

int main() {
  struct A a;
  a.b = 2;
  a.a[1] = 1;
  return a.c;
}