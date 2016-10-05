/* set ts=4 sw=4 enc=utf-8: -*- Mode: c++; tab-width: 4; c-basic-offset:4; coding: utf-8 -*- */

template <class T> class Handle 
{
public:
    Handle() : p(0) { }
    Handle(const Handle& s):p(0) { if (s.p) p = s.p->clone(); }
    Handle& operator=(const Handle&);
    ~Handle() { delete p; }

    Handle(T* t): p(t) { }

    operator bool() const { return p; }
    T& operator *() const;
    T* operator->() const;

private:
    T* p;
};

template<class T>
Handle<T>& Handle<T>::operator=(const Handle& rhs)
{
    if (&rhs != this) {
        delete p;
        p = rhs.p ? rhs.p->clone() : 0;
    }
    return *this;
}

template <class T>
T& Handle<T>::operator*() const
{
    if (p)
        return *p;
    throw runtime_error("unbound Handle");
}

template <class T>
T* Handle<T>::operator->() const
{
    if (p)
        return p;
    throw runtime_error("unbound Handle");
}
    

