#ifndef GENERIC_MACROS_H
#define GENERIC_MACROS_H

#define AFFIX(x, y) AFFIX_(x, y)
#define AFFIX_(x, y) x ## _ ## y
/* Reason for this macro is that we will be using the pseudo-generics trick from
   http://stackoverflow.com/questions/16522341/pseudo-generics-in-c
   because this is a pain, albeit valuable.
   Yeah, this is still pretty damn messy. :(
*/

#define ISDEF(i) (i == 1)
#define NDEF(i) (i == 0)


#endif
