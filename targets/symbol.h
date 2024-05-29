#ifndef __TIL_TARGETS_SYMBOL_H__
#define __TIL_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace til {

  class symbol {
    std::string _name;
    std::shared_ptr<cdk::basic_type> _type;
    bool _is_program = false;
    int _qualifier = -1;
    int _offset = 0; // 0 (zero) means global variable/function

  public:
    symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type) :
        _name(name), _type(type) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_program() const {
      return _is_program;
    }
    bool is_program(bool b) {
      return _is_program = b;
    }
    int qualifier() const {
      return _qualifier;
    }
    int qualifier(int v) {
      return _qualifier = v;
    }
    int offset() const {
      return _offset;
    }
    int offset(int v) {
      return _offset = v;
    }
    bool global() const {
      return _offset == 0;
    }
  };

} // til

#endif
