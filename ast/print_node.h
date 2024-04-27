#ifndef __TIL_AST_PRINT_NODE_H__
#define __TIL_AST_PRINT_NODE_H__

#include <cdk/ast/sequence_node.h>

namespace til {

  /**
   * Class for describing print nodes.
   */
  class print_node : public cdk::basic_node {
    cdk::sequence_node *_arguments;
    bool _add_newline;

  public:
    print_node(int lineno, cdk::sequence_node *arguments, bool add_newline = false) :
        cdk::basic_node(lineno), _arguments(arguments), _add_newline(add_newline) {}

    cdk::sequence_node *arguments() { return _arguments; }
    bool add_newline() { return _add_newline; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_print_node(this, level); }

  };

} // til

#endif
