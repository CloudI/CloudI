#include "ruby.h"
#include <string.h>

#define ERL_VERSION       131
#define ERL_SMALL_INT     97
#define ERL_INT           98
#define ERL_SMALL_BIGNUM  110
#define ERL_LARGE_BIGNUM  111
#define ERL_FLOAT         99
#define ERL_ATOM          100
#define ERL_REF           101
#define ERL_NEW_REF       114
#define ERL_PORT          102
#define ERL_PID           103
#define ERL_SMALL_TUPLE   104
#define ERL_LARGE_TUPLE   105
#define ERL_NIL           106
#define ERL_STRING        107
#define ERL_LIST          108
#define ERL_BIN           109
#define ERL_FUN           117
#define ERL_NEW_FUN       112

static VALUE mErlectricity;
static VALUE cDecoder;
void Init_decoder();

VALUE method_decode(VALUE klass, VALUE rString);

VALUE read_any_raw(unsigned char **pData);

// checkers

void check_int(int num) {
  char buf[17];
  sprintf(buf, "%u", num);
  rb_raise(rb_eStandardError, buf);
}

void check_str(char *str) {
  rb_raise(rb_eStandardError, str);
}

// string peekers/readers

unsigned int peek_1(unsigned char **pData) {
  return (unsigned int) **pData;
}

unsigned int peek_2(unsigned char **pData) {
  return (unsigned int) ((**pData << 8) + *(*pData + 1));
}

unsigned int peek_4(unsigned char **pData) {
  return (unsigned int) ((**pData << 24) + (*(*pData + 1) << 16) + (*(*pData + 2) << 8) + *(*pData + 3));
}

unsigned int read_1(unsigned char **pData) {
  unsigned int val = peek_1(pData);
  *pData += 1;
  return val;
}

unsigned int read_2(unsigned char **pData) {
  unsigned int val = peek_2(pData);
  *pData += 2;
  return val;
}

unsigned int read_4(unsigned char **pData) {
  unsigned int val = peek_4(pData);
  *pData += 4;
  return val;
}

// tuples, lists

VALUE read_small_tuple(unsigned char **pData) {
  if(read_1(pData) != ERL_SMALL_TUPLE) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small tuple");
  }

  int arity = read_1(pData);

  VALUE array = rb_ary_new2(arity);

  int i;
  for(i = 0; i < arity; ++i) {
    rb_ary_store(array, i, read_any_raw(pData));
  }

  return array;
}

VALUE read_large_tuple(unsigned char **pData) {
  if(read_1(pData) != ERL_LARGE_TUPLE) {
    rb_raise(rb_eStandardError, "Invalid Type, not a large tuple");
  }

  unsigned int arity = read_4(pData);

  VALUE array = rb_ary_new2(arity);

  int i;
  for(i = 0; i < arity; ++i) {
    rb_ary_store(array, i, read_any_raw(pData));
  }

  return array;
}

VALUE read_list(unsigned char **pData) {
  if(read_1(pData) != ERL_LIST) {
    rb_raise(rb_eStandardError, "Invalid Type, not an erlang list");
  }

  unsigned int size = read_4(pData);

  VALUE newref_class = rb_const_get(mErlectricity, rb_intern("List"));
  VALUE array = rb_funcall(newref_class, rb_intern("new"), 1, INT2NUM(size));

  int i;
  for(i = 0; i < size; ++i) {
    rb_ary_store(array, i, read_any_raw(pData));
  }

  read_1(pData);

  return array;
}

// primitives

void read_string_raw(unsigned char *dest, unsigned char **pData, unsigned int length) {
  memcpy((char *) dest, (char *) *pData, length);
  *(dest + length) = (unsigned char) 0;
  *pData += length;
}

VALUE read_bin(unsigned char **pData) {
  if(read_1(pData) != ERL_BIN) {
    rb_raise(rb_eStandardError, "Invalid Type, not an erlang binary");
  }

  unsigned int length = read_4(pData);

  VALUE rStr = rb_str_new((char *) *pData, length);
  *pData += length;

  return rStr;
}

VALUE read_string(unsigned char **pData) {
  if(read_1(pData) != ERL_STRING) {
    rb_raise(rb_eStandardError, "Invalid Type, not an erlang string");
  }

  int length = read_2(pData);
  VALUE newref_class = rb_const_get(mErlectricity, rb_intern("List"));
  VALUE array = rb_funcall(newref_class, rb_intern("new"), 1, INT2NUM(length));

  int i = 0;
  for(i; i < length; ++i) {
    rb_ary_store(array, i, INT2NUM(**pData));
    *pData += 1;
  }

  return array;
}

VALUE read_atom(unsigned char **pData) {
  if(read_1(pData) != ERL_ATOM) {
    rb_raise(rb_eStandardError, "Invalid Type, not an atom");
  }

  int length = read_2(pData);

  unsigned char buf[length + 1];
  read_string_raw(buf, pData, length);

  // Erlang true and false are actually atoms
  if(length == 4 && strncmp((char *) buf, "true", length) == 0) {
    return Qtrue;
  } else if(length == 5 && strncmp((char *) buf, "false", length) == 0) {
    return Qfalse;
  } else {
    return ID2SYM(rb_intern((char *) buf));
  }
}

VALUE read_small_int(unsigned char **pData) {
  if(read_1(pData) != ERL_SMALL_INT) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small int");
  }

  int value = read_1(pData);

  return INT2FIX(value);
}

VALUE read_int(unsigned char **pData) {
  if(read_1(pData) != ERL_INT) {
    rb_raise(rb_eStandardError, "Invalid Type, not an int");
  }

  long long value = read_4(pData);

  long long negative = ((value >> 31) & 0x1 == 1);

  if(negative) {
    value = (value - ((long long) 1 << 32));
  }

  return INT2FIX(value);
}

VALUE read_small_bignum(unsigned char **pData) {
  if(read_1(pData) != ERL_SMALL_BIGNUM) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small bignum");
  }

  unsigned int size = read_1(pData);
  unsigned int sign = read_1(pData);

  VALUE num = INT2NUM(0);
  VALUE tmp;

  unsigned char buf[size + 1];
  read_string_raw(buf, pData, size);

  int i;
  for(i = 0; i < size; ++i) {
    tmp = INT2FIX(*(buf + i));
    tmp = rb_funcall(tmp, rb_intern("<<"), 1, INT2NUM(i * 8));
    num = rb_funcall(num, rb_intern("+"), 1, tmp);
  }

  if(sign) {
    num = rb_funcall(num, rb_intern("*"), 1, INT2NUM(-1));
  }

  return num;
}

VALUE read_large_bignum(unsigned char **pData) {
  if(read_1(pData) != ERL_LARGE_BIGNUM) {
    rb_raise(rb_eStandardError, "Invalid Type, not a small bignum");
  }

  unsigned int size = read_4(pData);
  unsigned int sign = read_1(pData);

  VALUE num = INT2NUM(0);
  VALUE tmp;

  unsigned char buf[size + 1];
  read_string_raw(buf, pData, size);

  int i;
  for(i = 0; i < size; ++i) {
    tmp = INT2FIX(*(buf + i));
    tmp = rb_funcall(tmp, rb_intern("<<"), 1, INT2NUM(i * 8));

    num = rb_funcall(num, rb_intern("+"), 1, tmp);
  }

  if(sign) {
    num = rb_funcall(num, rb_intern("*"), 1, INT2NUM(-1));
  }

  return num;
}

VALUE read_float(unsigned char **pData) {
  if(read_1(pData) != ERL_FLOAT) {
    rb_raise(rb_eStandardError, "Invalid Type, not a float");
  }

  unsigned char buf[32];
  read_string_raw(buf, pData, 31);

  VALUE rString = rb_str_new2((char *) buf);

  return rb_funcall(rString, rb_intern("to_f"), 0);
}

VALUE read_nil(unsigned char **pData) {
  if(read_1(pData) != ERL_NIL) {
    rb_raise(rb_eStandardError, "Invalid Type, not a nil list");
  }

  VALUE newref_class = rb_const_get(mErlectricity, rb_intern("List"));
  return rb_funcall(newref_class, rb_intern("new"), 0);
}

// specials

VALUE read_pid(unsigned char **pData) {
  if(read_1(pData) != ERL_PID) {
    rb_raise(rb_eStandardError, "Invalid Type, not a pid");
  }

  VALUE node = read_atom(pData);
  VALUE id = INT2NUM(read_4(pData));
  VALUE serial = INT2NUM(read_4(pData));
  VALUE creation = INT2FIX(read_1(pData));

  VALUE pid_class = rb_const_get(mErlectricity, rb_intern("Pid"));
  return rb_funcall(pid_class, rb_intern("new"), 4, node, id, serial, creation);
}

VALUE read_new_reference(unsigned char **pData) {
  if(read_1(pData) != ERL_NEW_REF) {
    rb_raise(rb_eStandardError, "Invalid Type, not a new-style reference");
  }

  int size = read_2(pData);
  VALUE node = read_atom(pData);
  VALUE creation = INT2FIX(read_1(pData));

  VALUE id = rb_ary_new2(size);
  int i;
  for(i = 0; i < size; ++i) {
    rb_ary_store(id, i, INT2NUM(read_4(pData)));
  }

  VALUE newref_class = rb_const_get(mErlectricity, rb_intern("NewReference"));
  return rb_funcall(newref_class, rb_intern("new"), 3, node, creation, id);
}

// read_any_raw

VALUE read_any_raw(unsigned char **pData) {
  switch(peek_1(pData)) {
    case ERL_SMALL_INT:
      return read_small_int(pData);
      break;
    case ERL_INT:
      return read_int(pData);
      break;
    case ERL_FLOAT:
      return read_float(pData);
      break;
    case ERL_ATOM:
      return read_atom(pData);
      break;
    case ERL_PID:
      return read_pid(pData);
      break;
    case ERL_SMALL_TUPLE:
      return read_small_tuple(pData);
      break;
    case ERL_LARGE_TUPLE:
      return read_large_tuple(pData);
      break;
    case ERL_NIL:
      return read_nil(pData);
      break;
    case ERL_STRING:
      return read_string(pData);
      break;
    case ERL_LIST:
      return read_list(pData);
      break;
    case ERL_BIN:
      return read_bin(pData);
      break;
    case ERL_SMALL_BIGNUM:
      return read_small_bignum(pData);
      break;
    case ERL_LARGE_BIGNUM:
      return read_large_bignum(pData);
      break;
    case ERL_NEW_REF:
      return read_new_reference(pData);
      break;
  }
  return Qnil;
}

VALUE method_decode(VALUE klass, VALUE rString) {
  unsigned char *data = (unsigned char *) StringValuePtr(rString);

  unsigned char **pData = &data;

  // check protocol version
  if(read_1(pData) != ERL_VERSION) {
    rb_raise(rb_eStandardError, "Bad Magic");
  }

  return read_any_raw(pData);
}

void Init_decoder() {
  mErlectricity = rb_const_get(rb_cObject, rb_intern("Erlectricity"));
  cDecoder = rb_define_class_under(mErlectricity, "Decoder", rb_cObject);
  rb_define_singleton_method(cDecoder, "decode", method_decode, 1);
}
