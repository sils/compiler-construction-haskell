/**
 * Test Suite for CodeGenerator.
 * Program should return (#passedArguments + 1)
 */

/**
 * Test declarations and initializations, also inside blocks.
 * @returns {Integer} 3 on success
 */
int testDecls() {
  int a, b, c;
  int d = 3;
  bool e = false;
  e = true;
  double f, g;
  double h = 3.452;
  {
    int f = 1;
    double d = 3.4;
  }
  return d;
}

/**
 * Test Plus operator.
 * @returns {Integer} 4 on success
 */
int testPlus() {
  int a = 2;
  int b = a + 3;
  b = a + a;
  a = 6 + 0;
  b = a - 2;
  double c = 1.0;
  double d = 2.2;
  d = d - c + d;
  return b;
}

/**
 * Test Minus operator.
 * @returns {Integer} 5 on success
 */
int testMinus() {
  int a = 0-1;
  int b = 4 - a;
  a = 5 - 8;
  b = 2 - a;
  double c = 3.2;
  double d = 0.0-1.2;
  c = d - c - c;
  return b;
}

/**
 * Test Times operator.
 * @returns {Integer} 6 on success
 */
int testTimes() {
  int a = 2;
  int b = a * 4;
  b = a * a;
  a = a * 3;
  double c = 1.2;
  double d = 2.4 * c * 3.84;
  return a;
}

/**
 * Test Division operator.
 * @returns {Integer} 7 on success
 */
int testDiv() {
  int a = 42;
  int b = a / 3;
  double c = 3.5;
  double d = c / 7.0 / 0.5;
  // 23 / (42 / 14)
  return 23 / (0-(a / (0-b)));
}

/**
 * Test comparisions.
 * @returns {Integer} 8 on success
 */
int testCmp() {
  int a = 3;
  int b = 2;
  int c = 0-1;
  if (a < b || a == b || a <= b)
    return c;
  else
    c = 8;
  if (a > b && a != b && a >= b)
    return c;
  else
    return 0-1;
  return 0-1;
}

/**
 * Test while statement. Requires all other test to complete successfully.
 * @returns {Integer} 9 on success
 */
int testWhile() {
  int a = 81;
  while (a > 9) {
    a = a - 9;
  }
  return a;
}

/**
 * Run test, specified by number of passed arguments.
 * @param {Integer} argCount Number of arguments passed to program
 * @returns {Integer} result of excecuted test
 */
int main(int argCount) {
  if (argCount == 1)
    return 2;
  else {
  if (argCount == 2)
    return testDecls();
  else {
  if (argCount == 3)
    return testPlus();
  else {
  if (argCount == 4)
    return testMinus();
  else {
  if (argCount == 5)
    return testTimes();
  else {
  if (argCount == 6)
    return testDiv();
  else {
  if (argCount == 7)
    return testCmp();
  else {
  if (argCount == 8)
    return testWhile();
  else
    return 0-1;
  }
  }
  }
  }
  }
  }
  }
  return 0-1;
}
