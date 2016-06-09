#include "queryparser.h"

Node* cellData(ListCell* l) {
  return (Node*) (l -> data).ptr_value;
}

NodeTag my_nodeTag(Node* nodeptr) {
  return (nodeptr)->type;
}

char* my_strVal(Node* v) {
  return ((Value *)(v))->val.str;
}
