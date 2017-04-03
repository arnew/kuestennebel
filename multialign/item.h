#pragma once
#include <cmath>
#include <map>
#include <string>

class item {
	public:
	double data;
};
template <typename it>
struct diff  {
 inline double operator() (it lhs, it rhs) const {
	return fabs(lhs.data-rhs.data);
}
};
template <typename it>
struct sim1 {
 inline double operator() (it l, it r)  const{
	double s = 1/(1+fabs(l.data-r.data));
	return s;
}
};
template <typename it>
struct simpow {
 inline double operator() (it l, it r) const {
	double s = pow(2,-fabs(l.data-r.data));
	return s;
}
};

double M;
template <typename it>
struct simdiff {
 inline double operator() (it l, it r) const {
	double s = (M-fabs(l.data-r.data))/M;
	return s;
}
};

template <typename it>
struct simrdiff {
 inline double operator() (it l, it r) const {
	double S = fmax(fabs(l.data),fabs(r.data));
	if(S == 0)
		return 1;
	double s = 1.0 - (fabs(l.data-r.data)/S);
	return s;
}
};

template <typename it>
struct pen_foo{
 inline double operator() (it i) const {
	return -(fabs(i.data))/M;
}
};

template <typename it>
struct pen_abs{
 inline double operator() (it i) const {
	return fabs(i.data)+std::numeric_limits<double>::epsilon();
}
};
template <typename it>
struct pen_eps{
 inline double operator() (it i) const {
	return std::numeric_limits<double>::epsilon();
}
};
template <typename it>
struct pen0 { 
 inline double operator() (it) const {
	return 0;
}
};


