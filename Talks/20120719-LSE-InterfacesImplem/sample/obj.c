struct s_obj {
  int (*get)(struct s_obj*, void);
  void (*set)(struct s_obj*, int);
  int content;
};

/*

class obj {
  get() : cint;
  set(cint x) : void;
  cint content;
}


struct s_obj_vtbl {
  int (*get)(struct s_obj*, void);
  void (*set)(struct s_obj*, int);
};

struct s_obj {
  struct s_obj_vtbl *vtbl;
  int content;
};

struct s_obj {
  void *(interface_table)(int);
  struct s_obj_vtbl *vtbl;
  // object content
};



interface A {
  get() : cint;
}

class B support A {
  get() : cint { return 42; }
}

f1(o : A) : cint { return o.get(); }

f2(o : B) : cint { return f1(o); }

f2(o : B) : cint { return f1(CAST(o,A)); }

 */

struct s_A_vtbl { int (*get)(void); };

struct s_obj_interface_table {
  struct {
    s_A_vtbl *vtbl;
    void     *real_this;
  } A;
};

struct s_obj {
  struct s_A_vtbl *interfaces;
  struct s_obj_vtbl *vtbl;
  // object state
  struct s_A_vtbl _interfaces;
};
#define CAST(_OBJ, _INTER_) (&((_OBJ)->interfaces->_INTER_))

int f(s_obj *o) {
  CAST(o,A);
}

interface A { /* Some defs */ }
interface B : A { /* More defs */ }
class C support B { /* implem */ }

f1(o : A) { /* some code */ }
f2(o : B) { f1(o); }
f3(o : C) { f2(o); }
