/*
  C++ implementation of 
  "A linear space algorithm for computing maximal common subsequences"
  D. S. Hirschberg
  http://portal.acm.org/citation.cfm?id=360861
  
  See also: http://wordaligned.org/articles/longest-common-subsquence

  Modifyied to different comparison functions by Arne Wichmann, 2015-2017
*/
#include <algorithm>
#include <iterator>
#include <vector>
#include<cmath>
#include <iostream>
#include <functional>

typedef std::vector<double> lengths;

/*
  The "members" type is used as a sparse set for LCS calculations.
  Given a sequence, xs, and members, m, then
  if m[i] is true, xs[i] is in the LCS.
*/
typedef std::vector<bool> members;

/*
  Fill the LCS sequence from the members of a sequence, xs
  x - an iterator into the sequence xs
  xs_in_lcs - members of xs
  lcs - an output results iterator
*/
template <typename it, typename ot>
void set_lcs(it x, members const & xs_in_lcs, ot lcs)
{
    for (members::const_iterator xs_in = xs_in_lcs.begin();
         xs_in != xs_in_lcs.end(); ++xs_in, ++x)
    {
        if (*xs_in)
        {
            *lcs++ = *x;
        }
    }
}

template<typename t_cmp, typename T> struct mselect {
	t_cmp cmp;
	mselect(t_cmp cmp) : cmp(cmp) {}
	T operator()(T a, T b) {
		return cmp(a,b)?a:b;
	}
};

/*
  Calculate LCS row lengths given iterator ranges into two sequences.
  On completion, `lens` holds LCS lengths in the final row.
*/
template <typename it, typename t_cmp, typename t_penalty, typename t_compare>
void lcs_lens(it xlo, it xhi, it ylo, it yhi, lengths & lens, const t_cmp cmp, const t_penalty penalty, const t_compare compare)//double(*const select )(double,double), double (* const penalty)(ref), double (* const compare)(ref,ref))
{
    // Two rows of workspace.
    // Careful! We need the 1 for the leftmost column.
    lengths curr(1 + distance(ylo, yhi), 0);
    lengths prev(curr);
    curr[0] = 0;
    int j = 0;
    for (it y = ylo; y != yhi; ++y, ++j)
	curr[j+1] = curr[j] + penalty(*y);

#ifdef DEBUG
	std::cout << std::endl << "calculated lens ";
	for(auto i = curr.begin(); i!=curr.end(); i++) {
		std::cout << *i << " ";
	}
#endif
    
    for (it x = xlo; x != xhi; ++x)
    {
	prev[0] = curr[0] + penalty(*x);
        swap(prev, curr);
        int i = 0;
        for (it y = ylo; y != yhi; ++y, ++i)
        {
            curr[i + 1] = mselect<t_cmp,double>(cmp)(	curr[i] + penalty(*y)		,
			  mselect<t_cmp,double>(cmp)(	prev[i + 1] + penalty(*x)	,
				  	prev[i] + compare(*x,*y)	));
        }
    
#ifdef DEBUG
	std::cout << std::endl << "calculated lens ";
	for(auto i = curr.begin(); i!=curr.end(); i++) {
		std::cout << *i << " ";
	}
#endif
	
    }
#ifdef DEBUG
	std::cout<< std::endl;
#endif
    swap(lens, curr);
}

/*
  Recursive LCS calculation.
  See Hirschberg for the theory!
  This is a divide and conquer algorithm.
  In the recursive case, we split the xrange in two.
  Then, by calculating lengths of LCSes from the start and end
  corners of the [xlo, xhi] x [ylo, yhi] grid, we determine where
  the yrange should be split.
  
  xo is the origin (element 0) of the xs sequence
  xlo, xhi is the range of xs being processed
  ylo, yhi is the range of ys being processed
  Parameter xs_in_lcs holds the members of xs in the LCS.
*/
//struct sim_compare : public std::binary_function<const item&, const item&, bool> {
//	const item & master;
//	public:
//	sim_compare(const item& master) : master(master) {}
//	bool operator() (const item &l , const item& r) {
//		return sim(master, l) < sim(master, r);
//	}
//};
#include <iostream>
	
template <typename it, typename t_cmp, typename t_penalty, typename t_compare>
void
calculate_lcs(it xo, it xlo, it xhi, it ylo, it yhi, members & xs_in_lcs, it yo, members & ys_in_lcs, const t_cmp cmp, const t_penalty penalty, const t_compare compare)//, double(* const select)(double,double), bool (* const cmp)(double,double), double (* const penalty)(ref), double (* const compare)(ref,ref))
{
    unsigned const nx = distance(xlo, xhi);
    unsigned const ny = distance(ylo, yhi);
    
    if (nx == 0)
    {
        // empty range. all done
    }
    else if (nx == 1)
    {
        // single item in x range.
        // If it's in the yrange, mark its position in the LCS
	double sel= -1;
	auto j = yhi;
	
	for(auto i= ylo; i < yhi; i++) {
		double c = compare(*i, *xlo);
		if( cmp(c,sel) || sel == -1) {
			sel = c;
			j = i;
		}
#ifdef DEBUG
		std::cerr << sel << " " << c << std::endl;
#endif
	}
        xs_in_lcs[distance(xo, xlo)] = sel != -1;
	ys_in_lcs[distance(yo, j)] = sel!=-1;
	
#ifdef DEBUG
	if(sel!=-1)
	std::cerr << "nx " << xlo->data << " " << j->data<< std::endl;
#endif
    }
    else if (ny == 1)
    {
        // single item in x range.
        // If it's in the yrange, mark its position in the LCS
	double sel = -1;
	auto j = xhi;
	
	for(auto i= xlo; i < xhi; i++) {
		double c = compare(*i, *ylo);
		if( cmp(c,sel) || sel == -1) {
			sel = c;
			j = i;
		}
#ifdef DEBUG
		std::cerr << sel << " " << c << std::endl;
#endif
	}
        ys_in_lcs[distance(yo, ylo)] = sel != -1;
	xs_in_lcs[distance(xo, j)] = sel != -1;

#ifdef DEBUG
	if(sel!=-1)
	std::cerr << "ny " << j->data << " " << ylo->data<< std::endl;
#endif
    }
    else
    {
        // split the xrange
        it xmid = xlo + nx / 2;
        
        // Find LCS lengths at xmid, working from both ends of the range
        lengths ll_b, ll_e;
        std::reverse_iterator<it> hix(xhi), midx(xmid), hiy(yhi), loy(ylo);
        
        lcs_lens(xlo, xmid, ylo, yhi, ll_b, cmp, penalty, compare);
        lcs_lens(hix, midx, hiy, loy, ll_e, cmp, penalty, compare);
        
        // Find the optimal place to split the y range
        lengths::const_reverse_iterator e = ll_e.rbegin();
        double lsel = -1;
        it y = ylo, ymid = ylo;
        
        for (lengths::const_iterator b = ll_b.begin();
             b != ll_b.end(); ++b, ++e)
        {
            if (cmp(*b + *e,lsel) || lsel == -1)
            {
#ifdef DEBUG
	std::cout << "lsel " << std::endl;
#endif
                lsel = *b + *e;
                ymid = y;
            }
#ifdef DEBUG
	std::cout << "lsel " << lsel << " " << *b+*e << std::endl;
#endif
            // Care needed here!
            // ll_b and ll_e contain one more value than the range [ylo, yhi) 
            // As b and e range over dereferenceable values of ll_b and ll_e,
            // y ranges over iterator positions [ylo, yhi] _including_ yhi.
            // That's fine, y is used to split [ylo, yhi), we do not
            // dereference it. However, y cannot go beyond ++yhi.
            if (y != yhi)
            {
                ++y;
            }
        }
        // Split the range and recurse
#ifdef DEBUG
std::cout << "split " 
	<< xlo->data<< " .. " << xmid->data<< " .. " << xhi->data
	<< " :: " 
	<< ylo->data << " .. " << ymid->data << " .. " << yhi->data
	<< std::endl;
#endif
        calculate_lcs(xo, xlo, xmid, ylo, ymid, xs_in_lcs, yo, ys_in_lcs,cmp,penalty,compare);
        calculate_lcs(xo, xmid, xhi, ymid, yhi, xs_in_lcs, yo, ys_in_lcs,cmp,penalty,compare);
#ifdef DEBUG
	std::cerr << "join" << std::endl;
#endif
    }
}

// Calculate an LCS of (xs, ys), returning the result in an_lcs. 
template <typename seq, typename t_cmp, typename t_penalty, typename t_compare>
void lcs(seq const & xs, seq const & ys, seq & x_lcs, seq & y_lcs,const t_cmp cmp, const t_penalty penalty, const t_compare compare)//, double (* const penalty)(it), double (* const compare)(it, it))
{
    members xs_in_lcs(xs.size(), false);
    members ys_in_lcs(ys.size(), false);
    calculate_lcs//<it,typename seq::const_reference, fmin, std::islessequal, penalty, compare>
	    (xs.begin(), xs.begin(), xs.end(),
                  ys.begin(), ys.end(), xs_in_lcs, ys.begin(), ys_in_lcs,
		  cmp,penalty, compare);
    set_lcs(xs.begin(), xs_in_lcs, back_inserter(x_lcs));
    set_lcs(ys.begin(), ys_in_lcs, back_inserter(y_lcs));
}
