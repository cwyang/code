/* set ts=4 sw=4 enc=utf-8: -*- Mode: c++; tab-width: 4; c-basic-offset:4; coding: utf-8 -*- */

template <class T> class Ref_handle 
{
public:
    // manage reference count as well as pointer
    Ref_handle() : refptr(new size_t (1)), p(0) { }
    Ref_handle(T* t): refptr(new size_t(1)), p(t) { }
    Ref_handle(const Ref_handle& h): refptr(h.refptr), p(h.p) {
        ++*refptr;
    }
    Ref_handle& operator=(const Ref_handle&);
    ~Ref_handle() { }

    operator bool() const { return p; }
    T& operator *() const {
        if (p)
            return *p;
        throw std::runtime_error("unbound Ref_handle");
    }
    T* operator->() const {
        if (p)
            return p;
        throw std::runtime_error("unbound Ref_handle");
    }

private:
    T* p;
    size_t *refptr;
};

template<class T>
Ref_handle<T>& Ref_handle<T>::operator=(const Ref_handle& rhs)
{
    ++*rhs.refptr;
    // free the left-hands side, destroying pointers if needed
    if (--*refptr == 0) {
        delete refptr;
        delete p;
    }
    // copy in values from the rhs
    refptr = rhs.refptr;
    p = rhs.p;

    return *this;
}

template<class T>
Ref_handle<T>::~Ref_handle()
{
    if (--*refptr == 0) {
        delete refptr;
        delete p;
    }
}
