#include <stdio.h>
#define fixnum_mask   3
#define fixnum_tag    0
#define fixnum_shift  2
/* TODO
 *
 * add/define empty_list value
 *
 * */

int main(int argc, char** argv){
  int val = scheme_entry();
  if (( val & fixnum_mask) == fixnum_tag){
    printf("%d\n", scheme_entry());
  } else if (val == empty_list){
    printf("()\n");
  }
  return 0;
}
