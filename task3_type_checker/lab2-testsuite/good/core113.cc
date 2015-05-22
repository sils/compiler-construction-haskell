int main() {
  printInt(f(45));
  return 0;
}

int f(int x) {
  if (x < 100) {
    int x = 91;
    return x;
  } else {
    return x;
  }
}

void printInt(int x) { }
