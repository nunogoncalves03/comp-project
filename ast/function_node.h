#ifndef __TIL_AST_FUNCTION_NODE_H__
#define __TIL_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/typed_node.h>
#include <cdk/types/basic_type.h>
#include <cdk/types/functional_type.h>
#include <cdk/types/primitive_type.h>

namespace til {

  /**
   * Class for describing function nodes.
   */
  class function_node : public cdk::expression_node {
    cdk::sequence_node *_arguments;
    cdk::sequence_node *_declarations;
    cdk::sequence_node *_instructions;
    bool _is_program;

  public:
    function_node(int lineno, std::shared_ptr<cdk::basic_type> return_type, cdk::sequence_node *arguments,
        cdk::sequence_node *declarations, cdk::sequence_node *instructions, bool is_program = false) :
        cdk::expression_node(lineno), _arguments(arguments), _declarations(declarations),
        _instructions(instructions), _is_program(is_program) {
          std::vector<std::shared_ptr<cdk::basic_type>> argument_types;

          for (size_t i = 0; i < arguments->size(); i++) {
            argument_types.push_back(dynamic_cast<cdk::typed_node*>(arguments->node(i))->type());
          }

          this->type(cdk::functional_type::create(argument_types, return_type));
        }
    
    function_node(int lineno, cdk::sequence_node *declarations, cdk::sequence_node *instructions) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _declarations(declarations),
        _instructions(instructions), _is_program(true) {
          this->type(cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_INT)));
        }

    cdk::sequence_node *arguments() { return _arguments; }

    cdk::sequence_node *declarations() { return _declarations; }

    cdk::sequence_node *instructions() { return _instructions; }

    bool is_program() { return _is_program; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_function_node(this, level); }

  };

} // til

#endif
