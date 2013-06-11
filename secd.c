/*
 * A bytecode interpreter for a SECD-like machine 
 * Pedro Vasconcelos <pbv@dcc.fc.up.pt>, 2013
 */
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "secd.h"

int     code[CODE_MAX];       /* code segment */
value_t stack[STACK_MAX];     /* stack segment */
dump_t  dump[DUMP_MAX];       /* dump segment */


/* extend an environment (prepend a value to a list) 
*/
env_t extend(value_t elm, env_t env) {
  env_t nenv = (env_t) malloc(sizeof(env_node_t));
  assert(nenv!=NULL);
  nenv->elm = elm;
  nenv->next= env;
  return nenv;
}


/* lookup a value in an environment by index 
*/
value_t lookup(int n, env_t env) {
  while(n>0) {
    assert(env!=NULL);
    env = env->next;
    n--;
  }
  return env->elm;
}

/* allocate a new closure 
*/
closure_t *mkclosure(int pc, env_t env) {
  closure_t *ptr = (closure_t*) malloc(sizeof(closure_t));
  assert(ptr!=NULL);
  ptr->pc = pc;
  ptr->env = env;
  return ptr;
}


/* byte code interpretation loop
 */
value_t interp(void) {
  int pc = 0;  // program counter
  int sp = 0;  // stack pointer 
  int dp = 0;  // dump pointer
  env_t env = NULL;    // environment pointer

  for (;;) {            // loop
    value_t opa, opb;   // temporary operands
    int t;              // temporary register
    closure_t *cptr;    // closure pointer
    env_t nenv;         // temporary environment

    int opcode = code[pc++];    // fetch next opcode

    switch(opcode) {
    case LDC: 
      opa = (value_t)code[pc++];  // fetch operand
      stack[sp++] = opa;  // push it
      break;

    case LD: 
      t = code[pc++];       // fetch index
      stack[sp++] = lookup(t, env); // lookup in environment and push it
      break;

    case ADD: 
      opa = stack[--sp]; 
      opb = stack[--sp];  
      stack[sp++] = opa + opb;
      break;

    case SUB: 
      opa = stack[--sp]; 
      opb = stack[--sp];  
      stack[sp++] = opb - opa;  // order maters here!
      break;

    case MUL: 
      opa = stack[--sp];
      opb = stack[--sp];  
      stack[sp++] = opa * opb;
      break;

    case SEL:
      opa = stack[--sp];  // integer on top of stack
      dump[dp].pc = pc+2; // save PC on dump
      dp++;
      if (opa == 0)
	pc = code[pc];
      else
	pc = code[pc+1];
      break;

    case LDF: 
      t = code[pc++];              // fetch code address
      cptr = mkclosure(t, env);    // make a new closure
      stack[sp++] = (value_t)cptr; // push it
      break;

    case LDRF: 
      t = code[pc++]; // fetch code address
      nenv = extend((value_t)NULL, env);
      cptr = mkclosure(t, nenv);
      nenv->elm = (value_t)cptr;   // tie the knot in the environment
      stack[sp++] = (value_t)cptr; // push closure
      break;

    case AP:
      opa = stack[--sp];                // function argument
      cptr = (closure_t*) stack[--sp];  // function closure
      dump[dp].pc = pc;                 // save registers on dump 
      dump[dp].env = env;
      dp++;
      env = extend(opa, cptr->env);   // augment environment
      pc = cptr->pc;                  // jump to code address in closure
      break;

    case RTN:
      dp--;                 // restore registers
      pc = dump[dp].pc;
      env = dump[dp].env;
      break;

    case JOIN:
      dp--;
      pc = dump[dp].pc; // restore PC
      break;

    case HALT:
      return stack[--sp];  // return top of stack 
    
    case PAIR:
      //push both to stack

    default:
      fprintf(stderr, "unimplemented opcode: %d\n", opcode);
      exit(-1);
    }

    /* check register bounds */
    assert(sp>=0 && sp<STACK_MAX);
    assert(dp>=0 && dp<DUMP_MAX);
    assert(pc>=0 && pc<CODE_MAX);
  }
}



/* read bytecode into memory; returns number of entries read
 */
int read_code(FILE *f) {
  int i = 0, data;

  while(!feof(f) && i<CODE_MAX) {
    fscanf(f, "%d\n", &data);
    code[i++] = data;
  }
  return i;
}


int main(void) {
  value_t v;  /* top of stack */

  read_code(stdin);
  v = interp();
  printf("%d\n", (int)v);
  return 0;
}
