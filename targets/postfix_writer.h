#ifndef __TIL_TARGETS_POSTFIX_WRITER_H__
#define __TIL_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <optional>
#include <set>
#include <sstream>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;

    std::stack<std::string> _function_labels; // the history of function labels visited
    std::optional<std::string> _external_function_name; // name of external function to call
    std::set<std::string> _external_functions_to_declare; // set of external functions to declare

    // the history of loop labels visited during the current function's visit
    // format: pair (condition_label, end_label)
    std::vector<std::pair<std::string, std::string>> *_current_function_loop_labels;
    
    // to know where a return_node should jump to for performing the actual return
    std::string _current_function_ret_label;

    // semantic analysis
    bool _in_function_body = false, _in_function_args = false, _visited_final_instruction = false;

    int _offset;

    cdk::basic_postfix_emitter &_pf;
    int _lbl;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }
  
  protected:
    void prepareIDBinaryExpression(cdk::binary_operation_node * const node, int lvl);
    void prepareIDPredicateComparison(cdk::binary_operation_node * const node, int lvl);
    void cast_compatible_types(cdk::expression_node * const node, int lvl, std::shared_ptr<cdk::basic_type> const type);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    inline bool inFunction() {
      return !_function_labels.empty();
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // til

#endif
