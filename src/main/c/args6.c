void f() {

}

int sum(int i, int j, int k, int y) {

  f();

  int i1 = 1;
  int i2 = 2;
  int i3 = 3;
  int i4 = 4;
  int i5 = 5;
  int i6 = 6;
  int i7 = 7;
  int i8 = 8;
  int i9 = 9;
  int i10 = 10;
  int i11 = 11;
  int i12 = 12;
  int i13 = 13;
  int i14 = 14;
  int i15 = 15;
  int i16 = 16;
  int i17 = 17;
  int i18 = 18;
  int i19 = 19;
  int i20 = 20;

  return i + j + k + y;
}

int g(int i, int j, int k, int y) {

  return sum(i,j,k,y);
}

int main() {
  int i = 0;
  int j = 0;
  int k = 0;

  i = g(10,11,12,1);
  j = g(3,4,5,1);
  k = g(7,5,4,1);

  j = k + j;


  return j;
}
