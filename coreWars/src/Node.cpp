class Node {
public:
	INT64 volatile key; // multi-threaded algorithm
	Node* volatile left; // needs members to be
	Node* volatile right; // declared volatile
	Node() {key = 0; right = left = NULL;} // default constructor
};