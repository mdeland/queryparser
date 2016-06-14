#include "postgres.h"

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include "utils/memutils.h"

#include "parser/parser.h"
#include "nodes/print.h"
#include "nodes/pg_list.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "./postgres/src/include/nodes/nodes.h"
#include "./postgres/src/include/nodes/pg_list.h"

// const char* progname = "queryparser";
extern NodeTag my_nodeTag(Node* nodeptr);
//strVal(v)               (((Value *)(v))->val.str)
char* my_strVal(Node* v);
long my_intVal(Node* v);
Node* cellData(ListCell* l);

// extract data from A_Const
extern NodeTag const_type(Node* val);
extern long const_to_integer(Node* val);
extern char* const_to_str(Node* val);
