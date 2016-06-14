#include "queryparser.h"

Node* cellData(ListCell* l) {
  return (Node*) (l -> data).ptr_value;
}

NodeTag my_nodeTag(Node* nodeptr) {
  return (nodeptr)->type;
}

NodeTag const_type(Node* v) {
  return ((A_Const *)(v))->val.type;
}

long const_to_integer(Node* v) {
  return ((A_Const *)(v))->val.val.ival;
}
char* const_to_str(Node* v) {
  return ((A_Const *)(v))->val.val.str;
}

char* my_strVal(Node* v) {
  return ((Value *)(v))->val.str;
}

long my_intVal(Node* v) {
  return ((Value *)(v))->val.ival;
}
