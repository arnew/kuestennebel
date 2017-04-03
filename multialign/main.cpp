#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include "rapidjson/filereadstream.h"

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <map>
#include <numeric>

#include"hirschberg.h"
#include"item.h"


using namespace rapidjson;


class function: public item {
	public:
	uint64_t addr;
	std::string name;
};
unsigned int edit_distance(const std::string& s1, const std::string& s2)
{
	const std::size_t len1 = s1.size(), len2 = s2.size();
	std::vector<std::vector<unsigned int>> d(len1 + 1, std::vector<unsigned int>(len2 + 1));

	d[0][0] = 0;
	for(unsigned int i = 1; i <= len1; ++i) d[i][0] = i;
	for(unsigned int i = 1; i <= len2; ++i) d[0][i] = i;

	for(unsigned int i = 1; i <= len1; ++i)
		for(unsigned int j = 1; j <= len2; ++j)
                      // note that std::min({arg1, arg2, arg3}) works only in C++11,
                      // for C++98 use std::min(std::min(arg1, arg2), arg3)
                      d[i][j] = std::min({ d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + (s1[i - 1] == s2[j - 1] ? 0 : 1) });
	return d[len1][len2];
}

template <typename it>
struct simname {
 inline double operator() (it l, it r) const {
	auto s1 = l.name;
	auto s2 = r.name;
	double S = fmax(s1.size(),s2.size());
	if(S == 0)
		return 1;
	double s = 1.0 - (edit_distance(s1,s2)/S);
	return s;
}
};

template <typename it>
struct idname {
 inline double operator() (it l, it r) const {
	auto s1 = l.name;
	auto s2 = r.name;
	return s1 == s2?1:0;
}
};

typedef std::vector<function> seq;
typedef std::map<std::string,int> pos;

seq readfile(char* filename) {
	FILE* filea = fopen(filename,"rb");
	Document da;
	char readBuffer[65536];
	FileReadStream is(filea, readBuffer, sizeof(readBuffer));
	da.ParseStream(is);
	fclose(filea); 

	seq a;
	for (Value::ConstValueIterator itr = da.Begin(); itr != da.End(); ++itr)
	{
		function i;
		i.addr = (*itr)[0].GetUint64();
		i.name = (*itr)[1].GetString();
		i.data = (*itr)[2].GetDouble();
		a.push_back(i);
	}
	return a;
}

struct sine : public std::unary_function<std::string, bool> {
    double operator()(std::string x) { return x.substr(0,4) == "sub_"; }
};

int main (int argc, char ** argv)
{
	auto mode = std::string(argv[1]);
	seq a = readfile(argv[2]);
	seq b = readfile(argv[3]);

	int identical = ({
		std::vector<std::string> as; for(auto &i: a) as.push_back(i.name);
		std::vector<std::string> bs; for(auto &i: b) bs.push_back(i.name);
		std::vector<std::string> cs;
		std::sort(as.begin(), as.end());
		std::sort(bs.begin(), bs.end());
		std::set_intersection(as.begin(), as.end(), bs.begin(), bs.end(), back_inserter(cs));
		std::vector<std::string> ds;
		std::remove_copy_if(cs.begin(), cs.end(), back_inserter(ds), sine());
		ds.size();
	});

	int unique = ({
		std::vector<std::string> as; for(auto &i: a) as.push_back(i.name);
		std::vector<std::string> bs; for(auto &i: b) bs.push_back(i.name);
		std::vector<std::string> cs;
		std::sort(as.begin(), as.end());
		std::sort(bs.begin(), bs.end());
		std::set_union(as.begin(), as.end(), bs.begin(), bs.end(), back_inserter(cs));
		std::vector<std::string> ds;
		std::remove_copy_if(cs.begin(), cs.end(), back_inserter(ds), sine());
		ds.size();
	});
	

		double min = std::min(
				std::accumulate(a.begin(), a.end(), std::numeric_limits<double>::infinity(), [](double a, function x) {return std::min(a, x.data);}),
				std::accumulate(b.begin(), b.end(), std::numeric_limits<double>::infinity(), [](double a, function x) {return std::min(a, x.data);})
				) ;
		double max = std::max(
				std::accumulate(a.begin(), a.end(), 0.0, [](double a, function x) {return std::max(a, x.data);}),
				std::accumulate(b.begin(), b.end(), 0.0, [](double a, function x) {return std::max(a, x.data);})
				) ;
		M = fabs(max-min);

	//do{ 
	seq outl, outr;

	std::function<double(seq::const_reference,seq::const_reference)> sim;
	std::function<double(seq::const_reference)> pen ;
	if(mode == "mindiff") {
	
		/*
		std::vector<double> lens;
		lcs_lens<std::vector<ding>::iterator,std::vector<ding>::const_reference>(a.begin(), a.end(), b.begin(), b.end(), lens, fmin, pen, diff);
		for(auto i = lens.begin(); i!=lens.end(); i++) {
			std::cout << "len " << *i << std::endl;
		}
		*/
		auto lsim = diff<seq::const_reference>() ;
		auto lpen = pen_abs<seq::const_reference>();
		lcs(a,b,outl,outr,std::less_equal<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else if(mode == "maxsim1") {                                        
				auto lpen = pen0<seq::const_reference>();
			       auto lsim = 	sim1<seq::const_reference>();
		lcs(a,b,outl,outr,std::greater<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else if(mode == "maxsim2") {                                        
			auto lpen =	pen0<seq::const_reference>();
				auto lsim = simpow<seq::const_reference>();
		lcs(a,b,outl,outr,std::greater<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else if(mode == "maxsim3") {                                        
				auto lpen = pen0<seq::const_reference>();
				auto lsim = simdiff<seq::const_reference>();
		lcs(a,b,outl,outr,std::greater<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else if(mode == "maxsim4") {                                        
				auto lpen = pen0<seq::const_reference>();
				auto lsim = simrdiff<seq::const_reference>();
		lcs(a,b,outl,outr,std::greater<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else if(mode == "mindiff2") {                                        
				auto lpen = pen_eps<seq::const_reference>();
					auto lsim = diff<seq::const_reference>();
		lcs(a,b,outl,outr,std::less_equal<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else if(mode == "truth") {                                        
		auto lpen = pen0<seq::const_reference>();
		auto lsim = idname<seq::const_reference>();
		lcs(a,b,outl,outr,std::greater<double>(), lpen, lsim);
		pen = lpen; sim = lsim;
	} else {
		exit(1);
	}
	std::cout << a.size() << " " << b.size() << " " << outl.size() << " " << outr.size() << std::endl;
	//for(auto i = outl.begin(),j= outr.begin(); i!=outl.end() && j!=outr.end(); i++,j++) {
		//std::cout << i->name << " " << j->name << std::endl;
	//}
	assert(outl.size() == outr.size());
	int matches = 0;

	auto i = outl.begin(), j = outr.begin(), m = a.begin(), n = b.begin();
	bool done = false, start = true;
	std::ofstream outfile;
	outfile.open(argv[4]);
	outfile << "{ \"alignment\": [" << std::endl;
	pos X,Y;
	double o=0;

	int num = 1, lnum = 1, rnum = 1;
	while(!done) {
		std::string sep = "";
		if(!start) sep = ", ";
		// check cases: left starts, right starts, match starts
		//std::cout << (bool)(i==std::end(outl)) << (bool)(j == std::end(outr)) << (bool)(m ==std::end(a)) << (bool)(n == std::end(b)) << std::endl;
		//std::cout << (bool)(i!=std::end(outl)) << (bool)(j != std::end(outr)) << (bool)(m !=std::end(a)) << (bool)(n != std::end(b)) << std::endl;
	
//std::cout << (i!=outl.end()? i->name : "" ) << ": " << (j!=outr.end()? j->name : "" ) << ": " << (m!=a.end()? m->name : "" ) << ": " << (n!=b.end()? n->name : "" ) << std::endl;
		if((i!=outl.end() && m !=a.end() && i->name != m->name )||((i==outl.end()) && m!=a.end())) { // left ist not part of match and gets printed
			X[m->name]=num;
			outfile << sep 
				<< "{\"ldata\":"<<m->data
				<<",\"o\":" << pen(*m)
				<<",\"lname\":\"" << m->name 
				<< "\", \"laddr\":" << m->addr 
				<< ",\"lnum\":"<<lnum
				<< ",\"num\":"<<num<<"}"  
				<< std::endl;
			o += (pen)(*m);
			m++;
			lnum++;
		} else if((j!=outr.end() && n !=b.end() &&  j->name!=n->name) || ((j==outr.end()) && n!=b.end())) { // right is not part of match and gets printed
			Y[n->name]=num;
			outfile << sep 
				<< "{\"num\":"<<num
				<<",\"o\":" << pen(*n)
				<<",\"rdata\":"<<n->data
				<<",\"rname\":\"" << n->name 
				<< "\", \"raddr\":" << n->addr 
				<< ",\"rnum\":"<<rnum<< "}"  
				<< std::endl;
			o += (pen)(*n);
			n++;
			rnum++;
		} else if( i!=outl.end() && m !=a.end() && j!=outr.end() && n !=b.end() ){
			X[m->name]=num;
			Y[n->name]=num;
			// count true matches for F1 score
			if(i->name == j->name && i->name.substr(0,4) != "sub_") matches++;
			o += (sim)(*m,*n);
			outfile << sep 
				<< "{\"o\":"<< sim(*m,*n)
				<< ",\"ldata\":"<<m->data
				<<",\"rdata\":"<<n->data
				<<",\"lname\":\"" << m->name 
				<< "\", \"laddr\":" << m->addr 
				<< ",\"lnum\":"<<lnum
				<< ",\"num\":"<<num
				<<",\"rname\":\"" << n->name 
				<< "\",\"raddr\": " << n->addr 
				<< ",\"rnum\":"<<rnum
				<< "}"  << std::endl;
			m++; n++; i++; j++;
			lnum++;
			rnum++;
		} else {
			done = true;
		} 
		num ++;
		start = false;
	}
std::cout<< num << std::endl;

	//for(auto i = outl.begin(), j = outr.begin(); i!= outl.end() && j != outr.end(); i++, j++) {
	//	if(i->name == j->name && i->name.substr(0,4) != "sub_") matches++;
	//}
	//std::ofstream outfile;
	//outfile.open(argv[4]);
	//outfile << "{ \"alignment\": [" << std::endl;
	////std::cout << outl.size() << outr.size() << a.size() << b.size();
	//auto i = outl.begin(), j = outr.begin(), m = a.begin(), n = b.begin();
	//bool done = false, start = true;
	//std::ofstream outfile;
	//outfile.open(argv[4]);
	//outfile << "{ \"alignment\": [" << std::endl;

	//while(!done) {
	//	std::string sep = "";
	//	if(!start) sep = ", ";
	//	// check cases: left starts, right starts, match starts
	//	//std::cout << (bool)(i==std::end(outl)) << (bool)(j == std::end(outr)) << (bool)(m ==std::end(a)) << (bool)(n == std::end(b)) << std::endl;
	//	//std::cout << (bool)(i!=std::end(outl)) << (bool)(j != std::end(outr)) << (bool)(m !=std::end(a)) << (bool)(n != std::end(b)) << std::endl;
	//
//std::c//out << (i!=outl.end()? i->name : "" ) << ": " << (j!=outr.end()? j->name : "" ) << ": " << (m!=a.end()? m->name : "" ) << ": " << (n!=b.end()? n->name : "" ) << std::endl;
	//	if((i!=outl.end() && m !=a.end() && i->name != m->name )||((i==outl.end()) && m!=a.end())) { // left ist not part of match and gets printed
	//		outfile << sep << "[[\"" << m->name << "\", " << m->addr << "], []]"  << std::endl;
	//		m++;
	//	} else if((j!=outr.end() && n !=b.end() &&  j->name!=n->name) || ((j==outr.end()) && n!=b.end())) { // right is not part of match and gets printed
	//		outfile << sep << "[[],[\"" << n->name << "\", " << n->addr << "]]"  << std::endl;
	//		n++;
	//	} else if( i!=outl.end() && m !=a.end() && j!=outr.end() && n !=b.end() ){
	//		// count true matches for F1 score
	//		if(i->name == j->name && i->name.substr(0,4) != "sub_") matches++;
	//		outfile << sep << "[[\"" << m->name << "\", " << m->addr << "], [\"" << n->name << "\", " << n->addr << "]]"  << std::endl;
	//		m++; n++; i++; j++;
	//	} else {
	//		done = true;
	//	} 
	//	start = false;
	//}
	//std::cout << (bool)(i==std::end(outl)) << (bool)(j == std::end(outr)) << (bool)(m ==std::end(a)) << (bool)(n == std::end(b)) << std::endl;
	int match0=0 ,match1 =0,match2 =0,match3 =0,match4 =0,match5 =0,match6 =0,match7 =0,match8 =0,match9 =0,match10 =0,match11 =0,match12 =0,match13 =0,match14 =0,match15 =0,match16 =0,match17 =0,match18 =0,match19=0;
	for(auto x = X.begin(); x != X.end(); ++x) {
		if(sine()(x->first)) continue;
		if(Y[x->first] == 0) continue;
		switch(abs(x->second-Y[x->first])) {
			case 0: match0++;
			case 1: match1++;
			case 2: match2++;
			case 3: match3++;
			case 4: match4++;
			case 5: match5++;
			case 6: match6++;
			case 7: match7++;
			case 8: match8++;
			case 9: match9++;
			case 10: match10++;
			case 11: match11++;
			case 12: match12++;
			case 13: match13++;
			case 14: match14++;
			case 15: match15++;
			case 16: match16++;
			case 17: match17++;
			case 18: match18++;
			case 19: match19++;
			default: break;
		}
	}

	double precision = ((double)matches) / outl.size();
	if(outl.size() == 0) precision = 0;
	double recall = ((double)matches) / identical;
	if(identical == 0) recall = 0;
	double F1 = (2*precision * recall) / (precision+recall);
	if(precision+recall == 0) F1 = 0;
	outfile << "], \"fileA\": [\"" <<argv[2] <<"\"," <<a.size() <<"],\"fileB\": [\"" <<argv[3] <<"\","
		<<b.size() <<"], \"identical\":"<<identical <<",\"unique\":" <<unique<<",\"length\":" <<outl.size() <<",\"matches\":" <<matches 
		<<",\"precision\":" <<precision 
		<<",\"recall\":" <<recall 
		<<",\"opt\":" <<o
		<<",\"F1\":" <<F1 
		<< ", \"match\":"
		<< "["
<< match0 << "," << match1 << "," << match2 << "," << match3 << "," << match4 << ","
<< match5 << "," << match6 << "," << match7 << "," << match8 << "," << match9 << ","
<< match10 << "," << match11 << "," << match12 << "," << match13 << "," << match14 << ","
<< match15 << "," << match16 << "," << match17 << "," << match18 << "," << match19
		<< "]"
		<<"} " << std::endl;
	std::cout 
		<< " " 
		<< argv[2]
		<< " " 
		<< argv[3]
		<< " " 
		<< F1 << std::endl;
	outfile.close();
	if(match19>identical) return 1;
	//} while (size > 0);
	return 0;
}
