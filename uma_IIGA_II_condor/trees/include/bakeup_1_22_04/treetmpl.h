#ifndef _TREETMPL_H_
#define _TREETMPL_H_
/*************************************************************************************************************************
	Published version of binary tree structure 1.0
	Copyright Shengquan Yan
	The source code is not allowed to modification without the permission of the author.
	email: smyan@uiuc.edu sq_yan@hotmail.com

	See the comments in treedef.h for the usage of the tree structures
**************************************************************************************************************************/
#include "treedef.h"

/*--------------------------------------------------------------------------
					standard binary tree declaration
----------------------------------------------------------------------------*/
/////////////////////////////////////////////////////////////////////////////
// CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>
template< typename TYPE, typename ARG_TYPE = const TYPE&, typename ARG_KEY = ARG_TYPE, class ETraits=CElementTraits< TYPE, ARG_KEY > >
class CBinTree : public CObject
{
public:
	// local typedefs for class templates
	typedef BOOL (*TV_FUNC)(TYPE& data, LPVOID lpParam );

protected:
	struct CNode
	{
		CNode* pLChild;
		CNode* pRChild;
		CNode* pParent;
		TYPE data;
	};
public:

// Construction
	CBinTree(int nBlockSize = 10);

// Attributes (head and tail)
	// count of elements
	int GetCount() const;
	BOOL IsEmpty() const;

	// peek at head or tail or root
	TYPE& GetRoot();
	TYPE GetRoot() const;
	TYPE& GetHead();
	TYPE GetHead() const;
	TYPE& GetTail();
	TYPE GetTail() const;

// Operations
	// get head or tail or root (and remove it) - don't call on empty list!
	TYPE RemoveHead();
	TYPE RemoveTail();
	TYPE RemoveRoot();

	// insert or add(the same as insert) in the tree
	POSITION Add(ARG_TYPE newElement);
	POSITION Insert(ARG_TYPE newElement);

	// add another tree of elements
//	void AddTree(CObBintree* pNewBinTree);

	void RemoveAt(POSITION position);
	// remove all elements
	void RemoveAll();

	//depth interate the tree, pfnVisit is the function used for visiting every node,
	void WalkTree( TV_FUNC pfnVisit, LPVOID lpParam=NULL, int nTvOrder=TV_INORDER );

	// iteration
	POSITION GetHeadPosition() const;
	POSITION GetTailPosition() const;
	TYPE& GetNext(POSITION& rPosition); // return *Position++
	TYPE GetNext(POSITION& rPosition) const; // return *Position++
	TYPE& GetPrev(POSITION& rPosition); // return *Position--
	TYPE GetPrev(POSITION& rPosition) const; // return *Position--

	POSITION GetRootPosition() const;
	TYPE& GetLeft( POSITION& rPosition );
	TYPE GetLeft( POSITION& rPosition ) const;
	TYPE& GetRight( POSITION& rPosition );
	TYPE GetRight( POSITION& rPosition ) const;
	TYPE& GetParent( POSITION& rPosition );
	TYPE GetParent( POSITION& rPosition ) const;

	// getting an element at a given position
	TYPE& GetAt(POSITION position);
	TYPE GetAt(POSITION position) const;

	//Search is searching from root;
	//SearchByKey can accept a key value and find the corresponding node
	POSITION Search( ARG_TYPE searchValue ) const;
	POSITION SearchByKey( ARG_KEY keyValue ) const;

#ifdef _MFC_VER
	//Get an ordered list or array, the list should be empty before calling.
	void GetList( CList<TYPE, ARG_TYPE>& list );
	void GetArray( CArray<TYPE, ARG_TYPE>& array );
#endif //_MFC_VER

// Implementation
	//Depth visit helper function and variables
private:
	static void PreOrderVisit(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode);
	static void InOrderVisit(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode);
	static void PostOrderVisit(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode);

#ifdef _MFC_VER
	static BOOL SerialStore(TYPE& data, LPVOID lpParam);
	static BOOL StoreInList(TYPE& data, LPVOID lpParam);
	static BOOL StoreInArray(TYPE& data, LPVOID lpParam);
#endif //_MFC_VER

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
	~CBinTree();

#ifdef _MFC_VER	//for MFC only
		void Serialize(CArchive&);

	#ifdef _DEBUG
		void Dump(CDumpContext&) const;
		void AssertValid() const;
	#endif
#endif	//_MFC_VER
};

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::m_bTvStop = FALSE;
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::TV_FUNC CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::m_pfnVisit = NULL;
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
LPVOID CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::m_lpTvParam = NULL;

////////////////////////////////////////////////////////////////////////////
//					inline functions

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE int CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetCount() const
	{ return m_nCount; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE BOOL CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::IsEmpty() const
	{ return m_nCount == 0; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetHead()
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetHead() const
	{ return GetHead(); }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetTail()
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetTail() const
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRoot()
{
	ASSERT(NULL!=m_pNodeRoot);
	return m_pNodeRoot->data;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRoot() const
{
	return GetRoot();
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRootPosition() const
	{ return (POSITION) m_pNodeRoot; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetLeft( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetLeft( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRight( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRight( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetParent( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetParent( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetAt(POSITION position)
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetAt(POSITION position) const
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetNext(POSITION& rPosition) const // return *Position++
	{ return ((CBinTree*)this)->GetNext( rPosition ); }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetPrev(POSITION& rPosition) const // return *Position--
	{ return ((CBinTree*)this)->GetPrev( rPosition ); }

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
_TREE_INLINE POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Add(ARG_TYPE newElement)
{
	return Insert(newElement);
}

/////////////////////////////////////////////////////////////////////////////
// CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits> construction, destruction functions

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CBinTree(int nBlockSize)
{
	ASSERT(nBlockSize > 0);

	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks = NULL;
	m_nBlockSize = nBlockSize;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveAll()
{
	ASSERT_VALID(this);

	// destroy elements
	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks->FreeDataChain();
	m_pBlocks = NULL;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::~CBinTree()
{
	RemoveAll();
	ASSERT(m_nCount == 0);
}

/////////////////////////////////////////////////////////////////////////////
template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetHeadPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pLChild )pNode = pNode->pLChild;

	return (POSITION) pNode;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetTailPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pRChild )pNode = pNode->pRChild;

	return (POSITION) pNode;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetNext(POSITION& rPosition) // return *Position++
{ 
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pRChild ){
		pNodeNext = pNode->pRChild;
		while( pNodeNext->pLChild )pNodeNext = pNodeNext->pLChild;
	}else{
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext ){
			if( pNodeNext->pLChild == pLast )break;
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
TYPE& CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetPrev(POSITION& rPosition) // return *Position--
{
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pLChild ){
		pNodeNext = pNode->pLChild;
		while( pNodeNext->pRChild )pNodeNext = pNodeNext->pRChild;
	}else{
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext ){
			if( pNodeNext->pRChild == pLast )break;
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

/////////////////////////////////////////////////////////////////////////////
// Node helpers
/*
 * Implementation note: CNode's are stored in CPlex blocks and
 *  chained together. Free blocks are maintained in a singly linked list
 *  using the 'pNext' member of CNode with 'm_pNodeFree' as the head.
 *  Used blocks are maintained in a doubly linked list using both 'pNext'
 *  and 'pPrev' as links and 'm_pNodeHead' and 'm_pNodeTail'
 *   as the head/tail.
 *
 * We never free a CPlex block unless the List is destroyed or RemoveAll()
 *  is used - so the total number of CPlex blocks may grow large depending
 *  on the maximum past size of the list.
 */

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode*
CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::NewNode(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pParent, CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pLChild, CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pRChild)
{
	if (m_pNodeFree == NULL)
	{
		// add another block
		CPlex* pNewBlock = CPlex::Create(m_pBlocks, m_nBlockSize,
				 sizeof(CNode));

		// chain them into free list
		CNode* pNode = (CNode*) pNewBlock->data();
		// free in reverse order to make it easier to debug
		pNode += m_nBlockSize - 1;
		for (int i = m_nBlockSize-1; i >= 0; i--, pNode--)
		{
			pNode->pRChild = m_pNodeFree;
			m_pNodeFree = pNode;
		}
	}
	ASSERT(m_pNodeFree != NULL);  // we must have something

	CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode = m_pNodeFree;
	m_pNodeFree = m_pNodeFree->pRChild;

	pNode->pParent = pParent;
	pNode->pLChild = pLChild;
	pNode->pRChild = pRChild;
	m_nCount++;
	ASSERT(m_nCount > 0);  // make sure we don't overflow

#if _MSC_VER>1000
	#pragma push_macro("new")
	#undef new
		::new( (void*)( &pNode->data ) ) TYPE;
	#pragma pop_macro("new")
#else
		::new( (void*)( &pNode->data ) ) TYPE;
#endif

	return pNode;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::FreeNode(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	pNode->data.~TYPE();
	pNode->pRChild = m_pNodeFree;
	m_pNodeFree = pNode;
	m_nCount--;
	ASSERT(m_nCount >= 0);  // make sure we don't underflow

	// if no more elements, cleanup completely
	if (m_nCount == 0)
		RemoveAll();
}

/////////////////////////////////////////////////////////////////////////////

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Insert(ARG_TYPE newElement)
{

	ASSERT_VALID(this);

	CNode* pParent = m_pNodeRoot;
	CNode** ppNode = &m_pNodeRoot;

	while( *ppNode ){
		pParent = *ppNode;
		int nRet = CompareNode( (LPVOID)&newElement, (LPVOID)&pParent->data );
		if( nRet<=0 ){
			ppNode = &pParent->pLChild;
		}else if( nRet>0 ){
			ppNode = &pParent->pRChild;
		}
	}

	CNode* pNewNode = NewNode(pParent, NULL, NULL);
	pNewNode->data = (TYPE)newElement;
	*ppNode = pNewNode;
	return (POSITION) pNewNode;

}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveAt(POSITION position)
{
	ASSERT_VALID(this);

	CNode* pOldNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pOldNode, sizeof(CNode)));

	//case 1. single child, just bubble the child the the parent place
	//case 2. double children, bubble the in order successor to the parent place.
	CNode* pBubbleNode = NULL;
	if( pOldNode->pLChild==NULL )pBubbleNode = pOldNode->pRChild;
	else if( pOldNode->pRChild==NULL )pBubbleNode = pOldNode->pLChild;
	else{
		//case 2
		pBubbleNode = pOldNode->pRChild;
		while( pBubbleNode->pLChild )pBubbleNode = pBubbleNode->pLChild;
		//now pBubbleNode is the IOS, but if bubble node is the right child of the removing node,
		//do not change the right child of the bubble node
		if( pBubbleNode->pParent!=pOldNode ){ //the bubble node is more than one height below the removing node
			pBubbleNode->pParent->pLChild = pBubbleNode->pRChild;
			if( pBubbleNode->pRChild )pBubbleNode->pRChild->pParent=pBubbleNode->pParent;
			//link the right child of the removing node to bubble node
			pBubbleNode->pRChild = pOldNode->pRChild;
			pOldNode->pRChild->pParent = pBubbleNode;
		}
		//link the left child of the removing node to the bubble node
		pBubbleNode->pLChild = pOldNode->pLChild;
		pOldNode->pLChild->pParent = pBubbleNode;
	}
	CNode** ppNode=NULL;
	if( pOldNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pOldNode->pParent->pLChild==pOldNode )ppNode=&pOldNode->pParent->pLChild;
	else ppNode=&pOldNode->pParent->pRChild;

	*ppNode = pBubbleNode;
	if( pBubbleNode )pBubbleNode->pParent = pOldNode->pParent;

	FreeNode(pOldNode);
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveRoot()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	CNode* pOldNode = m_pNodeRoot;
	TYPE returnValue = pOldNode->data;

	RemoveAt( (POSITION)m_pNodeRoot );
	return returnValue;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveHead()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetHeadPosition();
	CNode* pOldNode = (CNode*)pos;
	TYPE returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

template< typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits >
TYPE CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveTail()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetTailPosition();
	CNode* pOldNode = (CNode*)pos;
	TYPE returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

#ifdef _MFC_VER
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::StoreInList(TYPE& data, LPVOID lpParam)
{
	CList<TYPE, ARG_TYPE>& list = *((CList<TYPE, ARG_TYPE>*))lpParam);
	list.AddTail( data );
	return TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::StoreInArray(TYPE& data, LPVOID lpParam)
{
	CArray<TYPE, ARG_TYPE>& array = *((CArray<TYPE, ARG_TYPE>*))lpParam);
	array.Add( data );
	return TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetList( CList<TYPE, ARG_TYPE>& list )
{
	ASSERT(list.IsEmpty());
	if( !list.IsEmpty() )list.RemoveAll();

	WalkTree( StoreInList, (LPVOID)&list, TV_INORDER );

	ASSERT( list.GetCount()==GetCount() );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetArray( CArray<TYPE, ARG_TYPE>& array )
{
	array.SetSize( 0 );
	WalkTree( StoreInArray, (LPVOID)&array, TV_INORDER );

	ASSERT( array.GetSize()==GetCount() );
}

#endif //_MFC_VER

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::PreOrderVisit(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pLChild )PreOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PreOrderVisit( pNode->pRChild );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::InOrderVisit(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )InOrderVisit( pNode->pLChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pRChild )InOrderVisit( pNode->pRChild );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::PostOrderVisit(CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )PostOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PostOrderVisit( pNode->pRChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::WalkTree( CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::TV_FUNC pfnVisit, LPVOID lpParam, int nTvOrder )
{
	ASSERT( pfnVisit!=NULL );

	if( IsEmpty() )return;

	m_lpTvParam = lpParam;
	m_pfnVisit = pfnVisit;
	m_bTvStop = FALSE;

	ASSERT( m_pNodeRoot!=NULL );
	switch(nTvOrder){
	case TV_PREORDER:
		PreOrderVisit(m_pNodeRoot);
		break;
	case TV_INORDER:
		InOrderVisit(m_pNodeRoot);
		break;
	case TV_POSTORDER:
		PostOrderVisit(m_pNodeRoot);
		break;
	default:
		ASSERT(FALSE);
	}
	m_lpTvParam = NULL;
	m_pfnVisit = NULL;
	m_bTvStop = FALSE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Search( ARG_TYPE searchValue ) const
{
	ASSERT_VALID(this);
	CNode* pNode = m_pNodeRoot;

	while( pNode!=NULL ){
		int nRet = CompareNode( (LPVOID)&pNode->data, (LPVOID)&searchValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::SearchByKey( ARG_KEY keyValue ) const
{
	ASSERT_VALID(this);
	CNode* pNode = m_pNodeRoot;

	while( pNode!=NULL ){
		int nRet = ETraits::CompareToKey( pNode->data, keyValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
int CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CompareNode( LPVOID pData1, LPVOID pData2 ) const
{
	//this function must be override;
	return ETraits::CompareElements( (TYPE&)(*(TYPE*)pData1), (TYPE&)(*(TYPE*)pData2) );
}

#ifdef _MFC_VER
/////////////////////////////////////////////////////////////////////////////
// Serialization
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::SerialStore(TYPE& data, LPVOID lpParam)
{
	CArchive& ar = *((CArchive*)lpParam);

	ASSERT( ar.IsStoring() );
	ETraits::SerializeElement( ar, data );
	return TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);

	CObject::Serialize(ar);

	if (ar.IsStoring())
	{
		ar.WriteCount(m_nCount);
		WalkTree( SerialStore, (LPVOID)&ar, TV_PREORDER );
	}
	else
	{
		DWORD nNewCount = (DWORD)ar.ReadCount();
		while (nNewCount--)
		{
			TYPE newData[1];
			ETraits::SerializeElement( ar, newData[0] );
			Insert(newData[0]);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "with " << m_nCount << " elements";
	if (dc.GetDepth() > 0)
	{
		POSITION pos = GetHeadPosition();
		while (pos != NULL){
			TYPE temp[1];
			temp[0] = ((CBinTree*)this)->GetNext(pos);
			dc << "\n";
			ETraits::DumpElement( dc, temp[0] );
		}
	}

	dc << "\n";
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CBinTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::AssertValid() const
{
	CObject::AssertValid();

	if (m_nCount == 0)
	{
		// empty Bintree
		ASSERT(m_pNodeRoot == NULL);
	}
	else
	{
		// non-empty list
		ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));
	}
}
#endif //_DEBUG

#endif //_MFC_VER

/*--------------------------------------------------------------------------
					avl tree declaration
----------------------------------------------------------------------------*/
/////////////////////////////////////////////////////////////////////////////
// CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>
template< typename TYPE, typename ARG_TYPE = const TYPE&, typename ARG_KEY = ARG_TYPE, class ETraits=CElementTraits< TYPE, ARG_KEY > >
class CAvlTree : public CObject
{
public:
	// local typedefs for class templates
	typedef BOOL (*TV_FUNC)(TYPE& data, LPVOID lpParam );

protected:
	struct CNode
	{
		CNode* pLChild;
		CNode* pRChild;
		CNode* pParent;
		WORD nHeight;
		TYPE data;
	};
public:

// Construction
	CAvlTree(int nBlockSize = 10);

// Attributes (head and tail)
	// count of elements
	int GetCount() const;
	BOOL IsEmpty() const;

	// peek at head or tail or root
	TYPE& GetRoot();
	TYPE GetRoot() const;
	TYPE& GetHead();
	TYPE GetHead() const;
	TYPE& GetTail();
	TYPE GetTail() const;

// Operations
	// get head or tail or root (and remove it) - don't call on empty list!
	TYPE RemoveHead();
	TYPE RemoveTail();
	TYPE RemoveRoot();

	// insert or add(the same as insert) in the tree
	POSITION Add(ARG_TYPE newElement);
	POSITION Insert(ARG_TYPE newElement);

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
	TYPE& GetNext(POSITION& rPosition); // return *Position++
	TYPE GetNext(POSITION& rPosition) const; // return *Position++
	TYPE& GetPrev(POSITION& rPosition); // return *Position--
	TYPE GetPrev(POSITION& rPosition) const; // return *Position--

	POSITION GetRootPosition() const;
	TYPE& GetLeft( POSITION& rPosition );
	TYPE GetLeft( POSITION& rPosition ) const;
	TYPE& GetRight( POSITION& rPosition );
	TYPE GetRight( POSITION& rPosition ) const;
	TYPE& GetParent( POSITION& rPosition );
	TYPE GetParent( POSITION& rPosition ) const;

	// getting an element at a given position
	TYPE& GetAt(POSITION position);
	TYPE GetAt(POSITION position) const;

	//Search is searching from root;
	//SearchByKey can accept a key value and find the corresponding node
	POSITION Search( ARG_TYPE searchValue ) const;
	POSITION SearchByKey( ARG_KEY keyValue ) const;

	//debug only
	void CheckTree( CNode* pNode );
	void CheckTree( );

#ifdef _MFC_VER
	//Get an ordered list or array, the list should be empty before calling.
	void GetList( CList<TYPE, ARG_TYPE>& list );
	void GetArray( CArray<TYPE, ARG_TYPE>& array );
#endif //_MFC_VER

// Implementation
protected:
	void SingleRightRotation( CNode* pNode );
	void DoubleRightRotation( CNode* pNode );
	void SingleLeftRotation( CNode* pNode );
	void DoubleLeftRotation( CNode* pNode );
	void CalcHeight( CNode* pNode );

private:
	static void PreOrderVisit(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode);
	static void InOrderVisit(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode);
	static void PostOrderVisit(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode);

#ifdef _MFC_VER
	static BOOL SerialStore(TYPE& data, LPVOID lpParam);
	static BOOL StoreInList(TYPE& data, LPVOID lpParam);
	static BOOL StoreInArray(TYPE& data, LPVOID lpParam);
#endif //_MFC_VER

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
	~CAvlTree();

#ifdef _MFC_VER	//for MFC only
		void Serialize(CArchive&);

	#ifdef _DEBUG
		void Dump(CDumpContext&) const;
		void AssertValid() const;
	#endif
#endif	//_MFC_VER
};

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::m_bTvStop = FALSE;
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::TV_FUNC CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::m_pfnVisit = NULL;
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
LPVOID CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::m_lpTvParam = NULL;

////////////////////////////////////////////////////////////////////////////
// CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits> inline functions

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE int CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetCount() const
	{ return m_nCount; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE BOOL CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::IsEmpty() const
	{ return m_nCount == 0; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRootPosition() const
	{ return (POSITION) m_pNodeRoot; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetHead()
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetHead() const
{ 
	POSITION pos = GetHeadPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetTail()
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetTail() const
{ 
	POSITION pos = GetTailPosition();
	ASSERT( NULL!=pos );
	return ((CNode*)pos)->data;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRoot()
{
	ASSERT(NULL!=m_pNodeRoot);
	return m_pNodeRoot->data;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRoot() const
{
	ASSERT(NULL!=m_pNodeRoot);
	return m_pNodeRoot->data;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetLeft( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetLeft( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pLChild;
		return pNode->data; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRight( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetRight( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pRChild;
		return pNode->data; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetParent( POSITION& rPosition )
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetParent( POSITION& rPosition ) const
	{ CNode* pNode = (CNode*) rPosition;
		ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
		rPosition = (POSITION) pNode->pParent;
		return pNode->data; }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetAt(POSITION position)
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetAt(POSITION position) const
{ 
	CNode* pNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	return pNode->data; 
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetNext(POSITION& rPosition) const // return *Position++
	{ return ((CAvlTree*)this)->GetNext( rPosition ); }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetPrev(POSITION& rPosition) const // return *Position--
	{ return ((CAvlTree*)this)->GetPrev( rPosition ); }

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
_TREE_INLINE POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Add(ARG_TYPE newElement)
{
	return Insert(newElement);
}

/////////////////////////////////////////////////////////////////////////////
// CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits> construction, destruction functions

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CAvlTree(int nBlockSize)
{
	ASSERT(nBlockSize > 0);

	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks = NULL;
	m_nBlockSize = nBlockSize;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveAll()
{
	ASSERT_VALID(this);

	// destroy elements
	m_nCount = 0;
	m_pNodeRoot = m_pNodeFree = NULL;
	m_pBlocks->FreeDataChain();
	m_pBlocks = NULL;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::~CAvlTree()
{
	RemoveAll();
	ASSERT(m_nCount == 0);
}

/////////////////////////////////////////////////////////////////////////////
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetHeadPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pLChild )pNode = pNode->pLChild;

	return (POSITION) pNode;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetTailPosition() const
{
	CNode* pNode = m_pNodeRoot;
	while( pNode && pNode->pRChild )pNode = pNode->pRChild;

	return (POSITION) pNode;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetNext(POSITION& rPosition) // return *Position++
{ 
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pRChild ){
		//if the node has a right subtree, then the inorder successor is below
		pNodeNext = pNode->pRChild;
		while( pNodeNext->pLChild )pNodeNext = pNodeNext->pLChild;
	}else{
		//the inorder successor is an ancestor, loop upward until pNodeNext is null (root node reached, no IOS)
		//or the left child of its pNodeNext pointing to the node we just come up, in this case, we find IOS.
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext && pNodeNext->pRChild==pLast ){
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
TYPE& CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetPrev(POSITION& rPosition) // return *Position--
{
	CNode* pNode = (CNode*) rPosition;
	CNode* pNodeNext = NULL;
	if( pNode->pLChild ){
		//if the node has a right subtree, then the inorder predecessor is below
		pNodeNext = pNode->pLChild;
		while( pNodeNext->pRChild )pNodeNext = pNodeNext->pRChild;
	}else{
		//the inorder predecessor is an ancestor, loop upward until pNodeNext is null (root node reached, no IOP)
		//or the right child of its pNodeNext pointing to the node we just come up, in this case, we find IOP.
		CNode* pLast = pNode;
		pNodeNext = pNode->pParent;
		while( pNodeNext && pNodeNext->pLChild==pLast ){
			pLast = pNodeNext;
			pNodeNext = pNodeNext->pParent;
		}
	}
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	rPosition = (POSITION) pNodeNext;
	return pNode->data; 
}

/////////////////////////////////////////////////////////////////////////////
// Node helpers
/*
 * Implementation note: CNode's are stored in CPlex blocks and
 *  chained together. Free blocks are maintained in a singly linked list
 *  using the 'pNext' member of CNode with 'm_pNodeFree' as the head.
 *  Used blocks are maintained in a doubly linked list using both 'pNext'
 *  and 'pPrev' as links and 'm_pNodeHead' and 'm_pNodeTail'
 *   as the head/tail.
 *
 * We never free a CPlex block unless the List is destroyed or RemoveAll()
 *  is used - so the total number of CPlex blocks may grow large depending
 *  on the maximum past size of the list.
 */

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode*
CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::NewNode(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pParent, CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pLChild, CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pRChild)
{
	if (m_pNodeFree == NULL)
	{
		// add another block
		CPlex* pNewBlock = CPlex::Create(m_pBlocks, m_nBlockSize,
				 sizeof(CNode));

		// chain them into free list
		CNode* pNode = (CNode*) pNewBlock->data();
		// free in reverse order to make it easier to debug
		pNode += m_nBlockSize - 1;
		for (int i = m_nBlockSize-1; i >= 0; i--, pNode--)
		{
			pNode->pRChild = m_pNodeFree;
			m_pNodeFree = pNode;
		}
	}
	ASSERT(m_pNodeFree != NULL);  // we must have something

	CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode = m_pNodeFree;
	m_pNodeFree = m_pNodeFree->pRChild;

	pNode->pParent = pParent;
	pNode->pLChild = pLChild;
	pNode->pRChild = pRChild;
	pNode->nHeight = 0;
	m_nCount++;
	ASSERT(m_nCount > 0);  // make sure we don't overflow

#if _MSC_VER>1000
	#pragma push_macro("new")
	#undef new
		::new( (void*)( &pNode->data ) ) TYPE;
	#pragma pop_macro("new")
#else
		::new( (void*)( &pNode->data ) ) TYPE;
#endif
	return pNode;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::FreeNode(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	pNode->data.~TYPE();
	pNode->pRChild = m_pNodeFree;
	m_pNodeFree = pNode;
	m_nCount--;
	ASSERT(m_nCount >= 0);  // make sure we don't underflow

	// if no more elements, cleanup completely
	if (m_nCount == 0)
		RemoveAll();
}

/////////////////////////////////////////////////////////////////////////////

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Insert(ARG_TYPE newElement)
{
	ASSERT_VALID(this);

	//block 1, regular binary insertion
	CNode* pParent = m_pNodeRoot;
	CNode** ppNode = &m_pNodeRoot;

	while( *ppNode ){
		pParent = *ppNode;
		int nRet = CompareNode( (LPVOID)&newElement, (LPVOID)&pParent->data );
		if( nRet<=0 ){
			ppNode = &pParent->pLChild;
		}else if( nRet>0 ){
			ppNode = &pParent->pRChild;
		}
	}

	CNode* pNewNode = NewNode(pParent, NULL, NULL);
	pNewNode->data = (TYPE)newElement;
	*ppNode = pNewNode;
	
	//block 2, avl tree balance rotation
	//the loop goes upward to find the rotation node, 
	//whenever a rotation is done, STOP LOOP beause the ancestors' height are the same after rotation.
	//if the rotation is not yet found, then adjust the height of the current node.
	CNode* pRotateNode = pParent;
	while( pRotateNode ){
		if( NODEHEIGHT(pRotateNode->pLChild)-NODEHEIGHT(pRotateNode->pRChild)==2 ){
			//left subtree is too high, rotate to right
			CNode* pChildNode = pRotateNode->pLChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)>NODEHEIGHT(pChildNode->pRChild) ){
				SingleRightRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)<NODEHEIGHT(pChildNode->pRChild) );
				DoubleRightRotation( pRotateNode );
			}
			break;
		}else if( NODEHEIGHT(pRotateNode->pRChild)-NODEHEIGHT(pRotateNode->pLChild)==2 ){
			//right subtree is too high, rotate to left
			CNode* pChildNode = pRotateNode->pRChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)<NODEHEIGHT(pChildNode->pRChild) ){
				SingleLeftRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)>NODEHEIGHT(pChildNode->pRChild) );
				DoubleLeftRotation( pRotateNode );
			}
			break;
		}
		//no rotation happend here, adjust the height
		CalcHeight( pRotateNode );
		pRotateNode = pRotateNode->pParent;
	}
		
	return (POSITION) pNewNode;

}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveAt(POSITION position)
{
	//avl tree removing. 
	//For a node having children, the concept model is to find the inorder successor of the removing node,
	//copy the data of IOS to the removing node, then remove the IOS rather than the removing node.
	//the rotation balance begins from the parent of the IOS and traces back to its ancestors.
	//however, the implementation removes the true removing node so that the iterator won't point to a garbage node.

	ASSERT_VALID(this);

	CNode* pOldNode = (CNode*) position;
	ASSERT(AfxIsValidAddress(pOldNode, sizeof(CNode)));

	//block 1. regular binary tree remove, except makeing pRotateNode point to the start tracing node for rotation,
	//and the bubble node's height is replaced the height of the removed node
	
	//case 1. single child, just bubble the child the the parent place
	//case 2. double children, bubble the in order successor to the parent place.
	CNode* pBubbleNode = NULL;
	CNode* pRotateNode = NULL;
	if( pOldNode->pLChild==NULL ){
		pBubbleNode = pOldNode->pRChild;
		pRotateNode = pOldNode->pParent;
	}else if( pOldNode->pRChild==NULL ){
		pBubbleNode = pOldNode->pLChild;
		pRotateNode = pOldNode->pParent;
	}else{
		//case 2
		pBubbleNode = pOldNode->pRChild;
		while( pBubbleNode->pLChild )pBubbleNode = pBubbleNode->pLChild;
		//now pBubbleNode is the IOS, but if bubble node is the right child of the removing node,
		//do not change the right child of the bubble node
		if( pBubbleNode->pParent!=pOldNode ){ //the bubble node is more than one height below the removing node
			pBubbleNode->pParent->pLChild = pBubbleNode->pRChild;
			if( pBubbleNode->pRChild )pBubbleNode->pRChild->pParent=pBubbleNode->pParent;
			//save the rotation place.
			pRotateNode = pBubbleNode->pParent;
			//link the right child of the removing node to bubble node
			pBubbleNode->pRChild = pOldNode->pRChild;
			pOldNode->pRChild->pParent = pBubbleNode;
		}else{
			pRotateNode = pBubbleNode;
		}
		//link the left child of the removing node to the bubble node
		pBubbleNode->pLChild = pOldNode->pLChild;
		pOldNode->pLChild->pParent = pBubbleNode;

		//replace the height of the bubble node by the removed node
		ASSERT(AfxIsValidAddress(pBubbleNode, sizeof(CNode)));
		pBubbleNode->nHeight = pOldNode->nHeight;
	}
	CNode** ppNode=NULL;
	if( pOldNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pOldNode->pParent->pLChild==pOldNode )ppNode=&pOldNode->pParent->pLChild;
	else ppNode=&pOldNode->pParent->pRChild;

	*ppNode = pBubbleNode;
	if( pBubbleNode )pBubbleNode->pParent = pOldNode->pParent;

	FreeNode(pOldNode);
	
	//block 2, avl tree balance rotation
	//the loop goes upward to find the rotation node until the root node is reached or the current subtree's height is not changed.
	//if a rotation is done, LOOP CONTINUE beause the height of the subtree at the rotation node may decrease,
	//so the ancestors may need rotation.
	//if the rotation is not done for the node, then adjust the height of the current node.
	while( pRotateNode ){
		int nOldHeight = NODEHEIGHT(pRotateNode);
		if( NODEHEIGHT(pRotateNode->pLChild)-NODEHEIGHT(pRotateNode->pRChild)==2 ){
			//left subtree is too high, rotate to right
			CNode* pChildNode = pRotateNode->pLChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)>=NODEHEIGHT(pChildNode->pRChild) ){
				SingleRightRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)<NODEHEIGHT(pChildNode->pRChild) );
				DoubleRightRotation( pRotateNode );
			}
			//after the rotation, the pRotateNode becomes a child of the node at the rotation point
			pRotateNode = pRotateNode->pParent;
		}else if( NODEHEIGHT(pRotateNode->pRChild)-NODEHEIGHT(pRotateNode->pLChild)==2 ){
			//right subtree is too high, rotate to left
			CNode* pChildNode = pRotateNode->pRChild;
			ASSERT( pChildNode!=NULL );
			if( NODEHEIGHT(pChildNode->pLChild)<=NODEHEIGHT(pChildNode->pRChild) ){
				SingleLeftRotation( pRotateNode );
			}else{
				ASSERT( NODEHEIGHT(pChildNode->pLChild)>NODEHEIGHT(pChildNode->pRChild) );
				DoubleLeftRotation( pRotateNode );
			}
			//after the rotation, the pRotateNode becomes a child of the node at the rotation point
			pRotateNode = pRotateNode->pParent;
		}else{
			//no rotation happend here, adjust the height
			CalcHeight( pRotateNode );
		}

		if( nOldHeight==NODEHEIGHT(pRotateNode) ){
			//since the subtree's height is not changed, STOP LOOP since the ancestors' height is maintained.
			break;
		}
		pRotateNode = pRotateNode->pParent;
	}
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveRoot()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	CNode* pOldNode = m_pNodeRoot;
	TYPE returnValue = pOldNode->data;

	RemoveAt( (POSITION)m_pNodeRoot );
	return returnValue;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveHead()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetHeadPosition();
	CNode* pOldNode = (CNode*)pos;
	TYPE returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
TYPE CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::RemoveTail()
{
	ASSERT_VALID(this);
	ASSERT(m_pNodeRoot != NULL);  // don't call on empty tree !!!
	ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));

	POSITION pos = GetTailPosition();
	CNode* pOldNode = (CNode*)pos;
	TYPE returnValue = pOldNode->data;

	RemoveAt(pos);
	return returnValue;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::SingleRightRotation( CNode* pNode )
{
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));

	CNode* pLChild = pNode->pLChild;
	ASSERT(AfxIsValidAddress(pLChild, sizeof(CNode)));

	CNode** ppNode = NULL;
	if( pNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pNode->pParent->pLChild==pNode )ppNode=&pNode->pParent->pLChild;
	else ppNode=&pNode->pParent->pRChild;

	//the right subtree of the left child becomes of the left subtree of the rotation node
	pNode->pLChild = pLChild->pRChild;
	if( pNode->pLChild )pNode->pLChild->pParent = pNode;

	//the left child becomes the root node of the subtree
	*ppNode = pLChild;
	pLChild->pParent = pNode->pParent;

	//the rotation node becomes the right child
	pLChild->pRChild = pNode;
	pNode->pParent = pLChild;

	CalcHeight(pNode);
	CalcHeight(pLChild);
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::DoubleRightRotation( CNode* pNode )
{
	SingleLeftRotation(pNode->pLChild);
	SingleRightRotation(pNode);
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::SingleLeftRotation( CNode* pNode )
{
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));

	CNode* pRChild = pNode->pRChild;
	ASSERT(AfxIsValidAddress(pRChild, sizeof(CNode)));

	CNode** ppNode = NULL;
	if( pNode==m_pNodeRoot )ppNode = &m_pNodeRoot;
	else if( pNode->pParent->pLChild==pNode )ppNode=&pNode->pParent->pLChild;
	else ppNode=&pNode->pParent->pRChild;

	//the left subtree of the right child becomes of the right subtree of the rotation node
	pNode->pRChild = pRChild->pLChild;
	if( pNode->pRChild )pNode->pRChild->pParent = pNode;

	//the right child becomes the root node of the subtree
	*ppNode = pRChild;
	pRChild->pParent = pNode->pParent;

	//the rotation node becomes the left child
	pRChild->pLChild = pNode;
	pNode->pParent = pRChild;

	CalcHeight(pNode);
	CalcHeight(pRChild);
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::DoubleLeftRotation( CNode* pNode )
{
	SingleRightRotation(pNode->pRChild);
	SingleLeftRotation(pNode);
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CalcHeight( CNode* pNode )
{
	ASSERT(AfxIsValidAddress(pNode, sizeof(CNode)));
	if( NODEHEIGHT(pNode->pLChild) > NODEHEIGHT(pNode->pRChild) ){
		pNode->nHeight = NODEHEIGHT(pNode->pLChild) + 1;
	}else{
		pNode->nHeight = NODEHEIGHT(pNode->pRChild) + 1;
	}
}

#ifdef _MFC_VER
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::StoreInList(TYPE& data, LPVOID lpParam)
{
	CList<TYPE, ARG_TYPE>& list = *((CList<TYPE, ARG_TYPE>*))lpParam);
	list.AddTail( data );
	return TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::StoreInArray(TYPE& data, LPVOID lpParam)
{
	CArray<TYPE, ARG_TYPE>& array = *((CArray<TYPE, ARG_TYPE>*))lpParam);
	array.Add( data );
	return TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetList( CList<TYPE, ARG_TYPE>& list )
{
	ASSERT(list.IsEmpty());
	if( !list.IsEmpty() )list.RemoveAll();

	WalkTree( StoreInList, (LPVOID)&list, TV_INORDER );

	ASSERT( list.GetCount()==GetCount() );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::GetArray( CArray<TYPE, ARG_TYPE>& array )
{
	array.SetSize( 0 );
	WalkTree( StoreInArray, (LPVOID)&array, TV_INORDER );

	ASSERT( array.GetSize()==GetCount() );
}

#endif //_MFC_VER

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::PreOrderVisit(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pLChild )PreOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PreOrderVisit( pNode->pRChild );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::InOrderVisit(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )InOrderVisit( pNode->pLChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
	if( pNode->pRChild )InOrderVisit( pNode->pRChild );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::PostOrderVisit(CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CNode* pNode)
{
	ASSERT( m_pfnVisit!=NULL );

	if( m_bTvStop )return;
	if( pNode->pLChild )PostOrderVisit( pNode->pLChild );
	if( pNode->pRChild )PostOrderVisit( pNode->pRChild );
	if( !(*m_pfnVisit)(pNode->data, m_lpTvParam) )m_bTvStop=TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::WalkTree( CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::TV_FUNC pfnVisit, LPVOID lpParam, int nTvOrder )
{
	ASSERT( pfnVisit!=NULL );

	if( IsEmpty() )return;

	m_lpTvParam = lpParam;
	m_pfnVisit = pfnVisit;
	m_bTvStop = FALSE;

	ASSERT( m_pNodeRoot!=NULL );
	switch(nTvOrder){
	case TV_PREORDER:
		PreOrderVisit(m_pNodeRoot);
		break;
	case TV_INORDER:
		InOrderVisit(m_pNodeRoot);
		break;
	case TV_POSTORDER:
		PostOrderVisit(m_pNodeRoot);
		break;
	default:
		ASSERT(FALSE);
	}
	m_lpTvParam = NULL;
	m_pfnVisit = NULL;
	m_bTvStop = FALSE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Search( ARG_TYPE searchValue ) const
{
	ASSERT_VALID(this);
	CNode* pNode = m_pNodeRoot;

	while( pNode!=NULL ){
		int nRet = CompareNode( (LPVOID)&pNode->data, (LPVOID)&searchValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
POSITION CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::SearchByKey( ARG_KEY keyValue ) const
{
	ASSERT_VALID(this);
	CNode* pNode = m_pNodeRoot;

	while( pNode!=NULL ){
		int nRet = ETraits::CompareToKey( pNode->data, keyValue );
		if( nRet==0 )return (POSITION)pNode;
		if( nRet>0 )pNode = pNode->pLChild;
		else pNode = pNode->pRChild;
	}
	return NULL;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
int CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CompareNode( LPVOID pData1, LPVOID pData2 ) const
{
	//this function must be override;
	return ETraits::CompareElements( (TYPE&)(*(TYPE*)pData1), (TYPE&)(*(TYPE*)pData2) );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CheckTree()
{
	CheckTree( m_pNodeRoot );
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::CheckTree( CNode* pNode )
{
	if( pNode==NULL )return;

	if( pNode->pLChild )CheckTree( pNode->pLChild );
	if( pNode->pRChild )CheckTree( pNode->pRChild );
	if( abs( NODEHEIGHT(pNode->pLChild)-NODEHEIGHT(pNode->pRChild) )>=2 ){
		TRACE( "Failed balance\n" );
	}
}


#ifdef _MFC_VER
/////////////////////////////////////////////////////////////////////////////
// Serialization
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
BOOL CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::SerialStore(TYPE& data, LPVOID lpParam)
{
	CArchive& ar = *((CArchive*)lpParam);

	ASSERT( ar.IsStoring() );
	ETraits::SerializeElement( ar, data );
	return TRUE;
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);

	CObject::Serialize(ar);

	if (ar.IsStoring())
	{
		ar.WriteCount(m_nCount);
		WalkTree( SerialStore, (LPVOID)&ar, TV_PREORDER );
	}
	else
	{
		DWORD nNewCount = (DWORD)ar.ReadCount();
		while (nNewCount--)
		{
			TYPE newData[1];
			ETraits::SerializeElement( ar, newData[0] );
			Insert(newData[0]);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "with " << m_nCount << " elements";
	if (dc.GetDepth() > 0)
	{
		POSITION pos = GetHeadPosition();
		while (pos != NULL){
			TYPE temp[1];
			temp[0] = ((CAvlTree*)this)->GetNext(pos);
			dc << "\n";
			ETraits::DumpElement( dc, temp[0] );
		}
	}

	dc << "\n";
}

template<typename TYPE, typename ARG_TYPE, typename ARG_KEY, class ETraits>
void CAvlTree<TYPE, ARG_TYPE, ARG_KEY, ETraits>::AssertValid() const
{
	CObject::AssertValid();

	if (m_nCount == 0)
	{
		// empty Bintree
		ASSERT(m_pNodeRoot == NULL);
	}
	else
	{
		// non-empty list
		ASSERT(AfxIsValidAddress(m_pNodeRoot, sizeof(CNode)));
	}
}
#endif //_DEBUG

#endif //_MFC_VER

#endif //_TREETMPL_H_