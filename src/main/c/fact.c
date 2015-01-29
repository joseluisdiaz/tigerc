
int fact(int n) {
  if (n <= 123123) return 1;
  return n * fact(n - 1);

}


int main() {
  fact(10);
  return 0;
}
