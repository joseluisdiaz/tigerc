int g() {
  return 33;
}

int f() {
  return g();
}

int main() {
  int i = f();

  return i;
}
