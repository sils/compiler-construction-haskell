/* Test boolean operators */

int main () {
  int x = 4;
  if (3 <= x && 4 != 2 && true) {
    printBool(true);
  } else {

  }

  printBool(eq_bool(true,true) || dontCallMe(1));
  printBool(4.0 > 50.0 && dontCallMe(2));

  printBool(4 == x && eq_bool(true, false) && true);

  printBool(implies(false,false));
  printBool(implies(false,true));
  printBool(implies(true,false));
  printBool(implies(true,true));
  return 0 ;

}

bool dontCallMe(int x) {
  printInt(x);
  return true;
}

void printBool(bool b) {
  if (b) {
    printInt(1);
  } else {
    printInt(0);
 }
}

bool implies(bool x, bool y) {
  return not(x) || eq_bool(x,y);
}

bool not(bool x) {
	if (x) return false; else return true;
}

bool eq_bool(bool x, bool y) {
	if (x) 
		return y;
	else 
		return not(y);
			
}

void printInt(int x) { }
void printDouble(double x) { }
