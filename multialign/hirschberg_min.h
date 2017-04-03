/*
  C++ implementation of 
  "A linear space algorithm for computing maximal common subsequences"
  D. S. Hirschberg
  http://portal.acm.org/citation.cfm?id=360861
  
  See also: http://wordaligned.org/articles/longest-common-subsquence.html
*/
#include <algorithm>
#include <iterator>
#include <vector>
#include<iostream>
#include<limits>

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

/*
  Calculate LCS row lengths given iterator ranges into two sequences.
  On completion, `lens` holds LCS lengths in the final row.
*/
template <typename it>
void lcs_lens(it xlo, it xhi, it ylo, it yhi, lengths & lens)
{
    // Two rows of workspace.
    // Careful! We need the 1 for the leftmost column.
    //double penalty = 1000; //std::numeric_limits<double>::infinity();
    lengths curr(1 + distance(ylo, yhi), 0);
    lengths prev(1 + distance(ylo, yhi), 0);
    //prev[0] = penalty;
    curr[0] = 0;
    //lengths prev(curr);
    int j = 0;
        for (it y = ylo; y != yhi; ++y, ++j)
		curr[j+1] = fabs(y->data);
    
//prev[0] = 1000;
//curr[0] = 0000;
    for (it x = xlo; x != xhi; ++x)
    {
	prev[0] = fabs(x->data);
        swap(prev, curr);
        int i = 0;
        for (it y = ylo; y != yhi; ++y, ++i)
        {
            curr[i + 1] = 
		    //prev[i] + diff(*x,*y);
std::min(curr[i] + fabs(y->data), std::min(prev[i + 1] + fabs(x->data),prev[i] + diff(*x,*y)));
        }
    }
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
	
template <typename it>
void
calculate_lcs(it xo, it xlo, it xhi, it ylo, it yhi, members & xs_in_lcs, it yo, members & ys_in_lcs, double asdf)
{
    unsigned const nx = distance(xlo, xhi);
    unsigned const ny = distance(ylo, yhi);
    std::cout << nx << " " << ny << std::endl;
    
    if (nx == 0)
    {
        // empty range. all done
    }
    else if (nx == 1)
    {
        // single item in x range.
        // If it's in the yrange, mark its position in the LCS
	double min = -1;
	it j = yhi;
	
	for(auto i= ylo; i < yhi; i++) {
		double d = diff(*i, *xlo);
		if( d <= min || (min == -1)) {
			min = d;
			j = i;
		}
	//std::cout << min << " " << d << std::endl;
	}
        xs_in_lcs[distance(xo, xlo)] = min!=-1;
	ys_in_lcs[distance(yo, j)] = min != -1;
	
	if(min!=-1)
	{
	std::cout << "nx " << xlo->name 
        << " " << xs_in_lcs[distance(xo, xlo)]
		<< " " << j->name 
	<< " " << ys_in_lcs[distance(yo, j)]
		<< " " << min << std::endl;
	}
//find(ylo, yhi, *xlo) != yhi;
    }
    else if (ny == 1)
    {
        // single item in x range.
        // If it's in the yrange, mark its position in the LCS
	double min = -1;
	it j = xhi;
	
	for(auto i= xlo; i < xhi; i++) {
		double d = diff(*i, *ylo);
		if( d <= min || (min == -1)) {
			min = d;
			j = i;
		}
	}
	xs_in_lcs[distance(xo, j)] = min!=-1;
        ys_in_lcs[distance(yo, ylo)] = min!=-1;

	if(min!=-1)
	std::cout << "ny " << j->name 
        << " " << xs_in_lcs[distance(xo, j)]
		<< " " << ylo->name 
	<< " " << ys_in_lcs[distance(yo, ylo)]
		<< " " << min<< std::endl;
	
//find(ylo, yhi, *xlo) != yhi;
    }
    else
    {
        // split the xrange
        it xmid = xlo + nx / 2;
        
        // Find LCS lengths at xmid, working from both ends of the range
        lengths ll_b, ll_e;
        std::reverse_iterator<it> hix(xhi), midx(xmid), hiy(yhi), loy(ylo);
        
        lcs_lens(xlo, xmid, ylo, yhi, ll_b);
        lcs_lens(hix, midx, hiy, loy, ll_e);
        
        // Find the optimal place to split the y range
        lengths::const_reverse_iterator e = ll_e.rbegin();
        double lmin = -1;
        it y = ylo, ymid = yhi;
        
        for (lengths::const_iterator b = ll_b.begin();
             b != ll_b.end(); ++b, ++e)
        {
            if (*b + *e < lmin || lmin == -1)
            {
                lmin = *b + *e;
                ymid = y;
            }
	std::cout << "lmin " << lmin << " " << *b+*e << std::endl;
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
std::cout << "split " 
	<< xlo->name << " .. " << xmid->name << " .. " << xhi->name
	<< " :: " 
	<< ylo->name << " .. " << ymid->name << " .. " << yhi->name
	<< std::endl;
        calculate_lcs(xo, xlo, xmid, ylo, ymid, xs_in_lcs, yo, ys_in_lcs,lmin);
        calculate_lcs(xo, xmid, xhi, ymid, yhi, xs_in_lcs, yo, ys_in_lcs,lmin);
	std::cout << "join" << std::endl;
    }
}

// Calculate an LCS of (xs, ys), returning the result in an_lcs. 
template <typename seq>
void lcs(seq const & xs, seq const & ys, seq & x_lcs, seq & y_lcs)
{
    members xs_in_lcs(xs.size(), false);
    members ys_in_lcs(ys.size(), false);
    calculate_lcs(xs.begin(), xs.begin(), xs.end(),
                  ys.begin(), ys.end(), xs_in_lcs, ys.begin(), ys_in_lcs,0);
    set_lcs(xs.begin(), xs_in_lcs, back_inserter(x_lcs));
    set_lcs(ys.begin(), ys_in_lcs, back_inserter(y_lcs));
}
