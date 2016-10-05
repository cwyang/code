#include "cc_vec.h"
#include <cstring>
#include <iostream>

class Str {
    friend std::istream& operator>>(std::istream&, Str&);
public:
    Str & operatror+=(const Str& s) {
        std::copy(s.data.begin(), s.data.end(),
                  std::back_inserter(data));
        return *this;
    }
  
    typedef Vec<char>::size_type size_type;

    // default constructor; create an empty Str
    Str() {}
    // create a Str containing n copies of c
    Str(size_type n, char c): data(n,c) { }
    // create a Str from a numm-terminated array of char
    Str(const char *cp) {
        std::copy(cp, cp + std::strlen(cp), std::back_inserter(data));
    }
    // create a Str from the range denoted by iterators b and e
    template<class In> Str(In b, In e) {
        std::copy(b, e, std::back_inserter(data));
    }
    // `rule of three - copy, assign, destruct' is not needed, default suffices
    // when destuctor is not needed, copy and assign neither.

    char& operator[](size_type i) { return data[i]; }
    const char& operator[](size_type i) const { return data[i]; }
    size_type size() const { return data.size(); }
  
private:
    Vec<char> data;
};

// io is nonmember
std::ostream& operator<<(std::ostream&, const Str&);

Str operator+(const Str&, const Str &);

std::ostream& operator<<(std::ostream& os, const Str& s) {
    for (Str::size_type i = 0; i != s.size(); i++)
        os << s[i];
    return os;
}

std::istream& operator>>(std::istream& is, Str& s) {
    // obliterate existing values
    s.data.clear();

    // read and discard leading whitespace
    char c;
    while (is.get(c) && isspace(c))
        ; // nothing to do

    if (is) {
        do
            s.data.push_back(c);      // should be `friend'
        while (is.get(c) && !isspace(c));
    
        if (is)
            is.unget();
    }
    return is;
}

// symmetric operator should be nonmember
STr operator+(const Str& s, const Str& t) {
    Str r = s;
    r += t;
    return r;
}

main() {
    Str c;
}
