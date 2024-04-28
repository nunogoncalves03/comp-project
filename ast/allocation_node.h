#ifndef __TIL_AST_ALLOCATION_NODE_H__
#define __TIL_AST_ALLOCATION_NODE_H__

#include <cdk/ast/unary_operation_node.h>

namespace til {

  /**
   * Class for describing stack allocation nodes.
   */
  class allocation_node : public cdk::unary_operation_node {

  public:
    allocation_node(int lineno, cdk::expression_node *argument) :
        cdk::unary_operation_node(lineno, argument) {}

    void accept(basic_ast_visitor *sp, int level) { sp->do_allocation_node(this, level); }

  };

} // til

#endif
