#ifndef __TREE_OB_H__
#define __TREE_OB_H__
/*************************************************************************************************************************
	Published version of binary tree structure 1.0
	Copyright Shengquan Yan
	The source code is not allowed to modification without the permission of the author.
	email: smyan@uiuc.edu sq_yan@hotmail.com

	See the comments in treedef.h for the usage of the tree structures
**************************************************************************************************************************/
#include "treedef.h"

/////////////////////////////////////////////////////////////////////////////

class CObBinTree : public CObject
{
	DECLARE_SERIAL(CObBinTree)
public:
	// local typedefs for class templates
	typedef BOOL (*TV_FUNC)(CObject*& ppData, LPVOID lpParam );
	typedef CObject* BASE_TYPE;
	typedef CObject* BASE_ARG_TYPE;

protected:
	struct CNode
	{
		CNode* pLChild;
		CNode* pRChild;
		CNode* pParent;
		CObject* data;
	};
public:

// Construction
	CObBinTree(int nBlockSize = 10);

// Attributes (head and tail)
	// count of elements
	int GetCount() const;
	BOOL IsEmpty() const;

	// peek at head or tail or root
	CObject*& GetRoot();
	CObject* GetRoot() const;
	CObject*& GetHead();
	CObject* GetHead() const;
	CObject*& GetTail();
	CObject* GetTail() const;

// Operations
	// get head or tail or root (and remove it) - don't call on empty list!
	CObject* RemoveHead();
	CObject* RemoveTail();
	CObject* RemoveRoot();

	// insert or add(the same as insert) in the tree
	POSITION Add(CObject* newElement);
	POSITION Insert(CObject* newElement);

	// add another tree of elements
//	void AddTree(CObBinTree* pNewBinTree);

	void RemoveAt(POSITION position);
	// remove all elements
	void RemoveAll();

	//depth interate the tree, pfnVisit is the function used for visiting every node,
	void WalkTree( TV_FUNC pfnVisit, LPVOID lpParam=NULL, int nTvOrder=TV_INORDER );

	// iteration
	POSITION GetHeadPosition() const;
	POSITION GetTailPosition() const;
	CObject*& GetNext(POSITION& rPosition); // return *Position++
	CObject* GetNext(POSITION& rPosition) const; // return *Position++
	CObject*& GetPrev(POSITION& rPosition); // return *Position--
	CObject* GetPrev(POSITION& rPosition) const; // return *Position--

	POSITION GetRootPosition() const;
	CObject*& GetLeft( POSITION& rPosition );
	CObject* GetLeft( POSITION& rPosition ) const;
	CObject*& GetRight( POSITION& rPosition );
	CObject* GetRight( POSITION& rPosition ) const;
	CObject*& GetParent( POSITION& rPosition );
	CObject* GetParent( POSITION& rPosition ) const;

	// getting an element at a given position
	CObject*& GetAt(POSITION position);
	CObject* GetAt(POSITION position) const;

	//Get an ordered list or array, the list should be empty before calling.
	void GetList( CObList& list );
	void GetArray( CObArray& array );

	//SearchTree is used searching from root;
	//Find is used searching after startAfter, note we only search the left
	//child tree of searchAfter, before <= elements are all in left child tree
	//POSITION Find(CObject* searchValue, POSITION startAfter = NULL) const;
	POSITION Search( CObject* searchValue ) const;

// Implementation
private:
	static void PreOrderVisit(CObBinTree::CNode* pNode);
	static void InOrderVisit(CObBinTree::CNode* pNode);
	static void PostOrderVisit(CObBinTree::CNode* pNode);

	static BOOL SerialStore(CObject*& pData, LPVOID lpParam);
	static BOOL StoreInList(CObject*& pData, LPVOID lpParam);
	static BOOL StoreInArray(CObject*& pData, LPVOID lpParam);

	//Attention! these global variables make DepthVisitAvlTree faster, but if it want to 
	//be used in multi-thread programming, they must be stored in TLS.
	static LPVOID m_lpTvParam;
	static TV_FUNC m_pfnVisit;
	static BOOL m_bTvStop;

protected:
	CNode* m_pNodeRoot;
	int m_nCount;
	CNode* m_pNodeFree;
	struct CPlex* m_pBlocks;
	int m_nBlockSize;

	CNode* NewNode(CNode*, CNode*, CNode*);
	void FreeNode(CNode*);
	virtual int CompareNode( LPVOID pData1, LPVOID pData2 ) const;

public:
	~CObBinTree();

	void Serialize(CArchive&);

#ifdef _DEBUG
	void Dump(CDumpContext&) const;
	void AssertValid() const;
#endif
};


/////////////////////////////////////////////////////////////////////////////
class CObAvlTree : public CObject
{
	DECLARE_SERIAL(CObAvlTree)
public:
	// local typedefs for class templates
	typedef BOOL (*TV_FUNC)(CObject*& ppData, LPVOID lpParam );
	typedef CObject* BASE_TYPE;
	typedef CObject* BASE_ARG_TYPE;

protected:
	struct CNode
	{
		CNode* pLChild;
		CNode* pRChild;
		CNode* pParent;
		WORD nHeight;
		CObject* data;
	};
public:

// Construction
	CObAvlTree(int nBlockSize = 10);

// Attributes (head and tail)
	// count of elements
	int GetCount() const;
	BOOL IsEmpty() const;

	// peek at head or tail or root
	CObject*& GetRoot();
	CObject* GetRoot() const;
	CObject*& GetHead();
	CObject* GetHead() const;
	CObject*& GetTail();
	CObject* GetTail() const;

// Operations
	// get head or tail or root (and remove it) - don't call on empty list!
	CObject* RemoveHead();
	CObject* RemoveTail();
	CObject* RemoveRoot();

	// insert or add(the same as insert) in the tree
	POSITION Add(CObject* newElement);
	POSITION Insert(CObject* newElement);

	// add another tree of elements
//	void AddTree(CObAvlTree* pNewAvlTree);

	void RemoveAt(POSITION position);
	// remove all elements
	void RemoveAll();

	//depth interate the tree, pfnVisit is the function used for visiting every node,
	void WalkTree( TV_FUNC pfnVisit, LPVOID lpParam=NULL, int nTvOrder=TV_INORDER );

	// iteration
	POSITION GetHeadPosition() const;
	POSITION GetTailPosition() const;
	CObject*& GetNext(POSITION& rPosition); // return *Position++
	CObject* GetNext(POSITION& rPosition) const; // return *Position++
	CObject*& GetPrev(POSITION& rPosition); // return *Position--
	CObject* GetPrev(POSITION& rPosition) const; // return *Position--

	POSITION GetRootPosition() const;
	CObject*& GetLeft( POSITION& rPosition );
	CObject* GetLeft( POSITION& rPosition ) const;
	CObject*& GetRight( POSITION& rPosition );
	CObject* GetRight( POSITION& rPosition ) const;
	CObject*& GetParent( POSITION& rPosition );
	CObject* GetParent( POSITION& rPosition ) const;

	// getting an element at a given position
	CObject*& GetAt(POSITION position);
	CObject* GetAt(POSITION position) const;

	//Get an ordered list or array, the list should be empty before calling.
	void GetList( CObList& list );
	void GetArray( CObArray& array );

	//SearchTree is used searching from root;
	//Find is used searching after startAfter, note we only search the left
	//child tree of searchAfter, before <= elements are all in left child tree
	//POSITION Find(CObject* searchValue, POSITION startAfter = NULL) const;
	POSITION Search( CObject* searchValue ) const;

	void CheckTree( CNode* pNode );
	void CheckTree( );

// Implementation
protected:
	void SingleRightRotation( CNode* pNode );
	void DoubleRightRotation( CNode* pNode );
	void SingleLeftRotation( CNode* pNode );
	void DoubleLeftRotation( CNode* pNode );
	void CalcHeight( CNode* pNode );

private:
	static void PreOrderVisit(CObAvlTree::CNode* pNode);
	static void InOrderVisit(CObAvlTree::CNode* pNode);
	static void PostOrderVisit(CObAvlTree::CNode* pNode);

	static BOOL SerialStore(CObject*& pData, LPVOID lpParam);
	static BOOL StoreInList(CObject*& pData, LPVOID lpParam);
	static BOOL StoreInArray(CObject*& pData, LPVOID lpParam);

	//Attention! these global variables make DepthVisitAvlTree faster, but if it want to 
	//be used in multi-thread programming, they must be stored in TLS.
	static LPVOID m_lpTvParam;
	static TV_FUNC m_pfnVisit;
	static BOOL m_bTvStop;

protected:
	CNode* m_pNodeRoot;
	int m_nCount;
	CNode* m_pNodeFree;
	struct CPlex* m_pBlocks;
	int m_nBlockSize;

	CNode* NewNode(CNode*, CNode*, CNode*);
	void FreeNode(CNode*);
	virtual int CompareNode( LPVOID pData1, LPVOID pData2 ) const;

public:
	~CObAvlTree();

	void Serialize(CArchive&);

#ifdef _DEBUG
	void Dump(CDumpContext&) const;
	void AssertValid() const;
#endif
};

#ifdef _DEBUG
#ifndef _TREE_ENABLE_INLINES
#define _TREE_ENABLE_INLINES
#endif
#endif

#ifdef _TREE_ENABLE_INLINES
#ifndef _TREE_INLINE
#define _TREE_INLINE AFX_INLINE
#endif
#include "tree_ob.inl"
#endif


#endif //__TREE_OB_H__