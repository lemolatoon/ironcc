int f();

int main() {
  switch (f()) {
    case 0:
      return 0;
    case 1:
      return 1;
    default:
      return 666;
  }
}