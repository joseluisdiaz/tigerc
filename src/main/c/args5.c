int  f(int i) {
  return i;
}

int sum(int i, int j, int k, int y, int c, int x) {

  f(3);

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

  return i + j + k + y + c + x;
}

int w(int i1, int i2, int i3, int i4, int i5, int i6, int i7) {

  int a = f(i5);

  return i1 + i2 + i3 + i4 + a + i6 + i7;
}

int g(int i, int j, int k, int y) {

  //  int a = 1;
  // int a = sum(1,2,3,4,5,6);
  int a = w(1,2,3,4,5,6,7);

  return a + sum(i,j,k,y, 123, 324);
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